# =============================================================================
# lib_report.R
# Collation, the fit ledger, the cross-layer comparison, the release gates and
# atomic promotion.
#
# Every gate calls stop(). A breach therefore returns a non-zero process status
# and nothing is promoted. The previous pipeline's verifiers printed their
# failures and returned success, which is how a mismatch reached a results
# document.
# =============================================================================

#' Bind data frames that do not share a column set.
#'
#' The two model layers legitimately record different fields: the primary model
#' has mediation quantities and a study count, the reference model does not.
#' Binding on the union, with NA where a layer has nothing to say, keeps both
#' without inventing a value for either. A plain rbind() would simply fail.
rbind_fill <- function(l) {
  l <- l[!vapply(l, is.null, logical(1))]
  l <- l[vapply(l, NROW, integer(1)) > 0]
  if (!length(l)) return(NULL)
  cols <- unique(unlist(lapply(l, names)))
  out <- do.call(rbind, lapply(l, function(d) {
    miss <- setdiff(cols, names(d))
    for (m in miss) d[[m]] <- NA
    d[, cols, drop = FALSE]
  }))
  rownames(out) <- NULL
  out
}

collate_results <- function(fit_results) {
  flat <- unlist(fit_results, recursive = FALSE)
  grab <- function(nm) rbind_fill(lapply(flat, function(x) x[[nm]]))
  ppc <- lapply(flat, function(x) x$ppc)
  ppc <- ppc[!vapply(ppc, is.null, logical(1))]
  bind_ppc <- function(part) rbind_fill(lapply(ppc, function(p) p[[part]]))

  list(
    diagnostics = grab("diagnostics"),
    slopes      = grab("slopes"),
    pooled      = grab("pooled"),
    predict     = grab("predict"),
    contrasts   = grab("contrasts"),
    heterogeneity = grab("heterogeneity"),
    pmed        = grab("pmed"),
    pmed_diag   = grab("pmed_diag"),
    pmed_draws  = grab("pmed_draws"),
    ## Prior predictive and power scaling, previously computed and discarded.
    power_scaling = rbind_fill(lapply(flat, function(x) x$power_scaling)),
    prior_predictive = rbind_fill(lapply(flat, function(x) x$prior_predictive$summary)),
    prior_predictive_verdict = rbind_fill(lapply(flat, function(x) x$prior_predictive$verdict)),
    prior_predictive_draws = rbind_fill(lapply(flat, function(x) x$prior_predictive$draws)),
    ppc_study      = if (length(ppc)) bind_ppc("study") else NULL,
    ppc_coordinate = if (length(ppc)) bind_ppc("coordinate") else NULL,
    ppc_overall    = if (length(ppc)) bind_ppc("overall") else NULL,
    timings = data.frame(fit_id = vapply(flat, function(x) x$fit_id, character(1)),
                         fit_class = vapply(flat, function(x) x$fit_class, character(1)),
                         seconds = vapply(flat, function(x) x$seconds %||% NA_real_, numeric(1)),
                         status = vapply(flat, function(x) x$status, character(1)),
                         error = vapply(flat, function(x) x$error %||% NA_character_, character(1)),
                         stringsAsFactors = FALSE),
    draws_d_z = setNames(lapply(flat, function(x) x$draws_d_z),
                         vapply(flat, function(x) x$fit_id, character(1)))
  )
}

build_fit_ledger <- function(plan, results, analysis_id, data_sha, cfg) {
  src <- tryCatch(system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
                  error = function(e) NA_character_)
  d <- merge(plan, results$timings[, c("fit_id", "seconds", "status", "error")],
             by = "fit_id", all.x = TRUE)
  dg <- results$diagnostics
  if (!is.null(dg) && "fit_id" %in% names(dg))
    d <- merge(d, dg[, c("fit_id", "max_rhat", "min_ess_bulk", "min_ess_tail",
                         "num_divergent", "num_max_treedepth", "min_ebfmi",
                         "escalations")],
               by = "fit_id", all.x = TRUE)
  d$analysis_id <- analysis_id
  d$source_commit <- src[1]
  d$data_sha256 <- data_sha
  d$config_sha256 <- cfg$.sha
  d$seed <- vapply(d$fit_id, function(f) fit_seed(cfg, f), numeric(1))
  d$chains <- cfg$sampling$chains
  d$iter_warmup <- cfg$sampling$iter_warmup
  d$iter_sampling <- cfg$sampling$iter_sampling
  d$status[is.na(d$status)] <- "missing"
  d
}

## ---- cross-layer comparison -------------------------------------------------

#' Compare the two layers on the SAME estimand: the full-contrast change in each
#' zero-order Fisher-z coordinate.
#'
#' The two fits are independent, so the comparison is between marginals. Nothing
#' here pairs draw i of one model with draw i of the other.
cross_layer_table <- function(cfg, results, lock) {
  dz <- results$draws_d_z
  ids <- names(dz)[!vapply(dz, is.null, logical(1))]
  stan_ids <- grep("^stan\\|", ids, value = TRUE)
  out <- list()

  for (sid in stan_ids) {
    parts <- strsplit(sid, "|", fixed = TRUE)[[1]]
    key <- parts[2]; mod <- parts[3]
    bid <- sprintf("brms|%s|%s", key, mod)
    if (!bid %in% ids) next
    sp <- lock$support[lock$support$pathway == key & lock$support$moderator == mod, ]
    S <- dz[[sid]]; B <- dz[[bid]]
    pairs <- PAIRS(cfg)
    for (j in seq_along(pairs)) {
      cmp <- compare_posteriors(S[, j], B[, pairs[j]], labels = c("stan", "brms"))
      out[[paste(sid, j)]] <- cbind(
        data.frame(pathway = key, moderator = mod, coordinate = pairs[j],
                   estimand = "d_z (full contrast)",
                   contrast = contrast_label(sp),
                   ## The first two coordinates are linear in the model's own
                   ## parameters in both layers. The third is reached
                   ## nonlinearly in the Stan layer through the
                   ## valid-correlation map, so agreement there is an
                   ## architecture comparison, not a preprocessing check.
                   comparison_kind = if (j < 3) "matched linear coordinate"
                                     else "architecture sensitivity",
                   stringsAsFactors = FALSE), cmp)
    }
  }
  d <- do.call(rbind, out); rownames(d) <- NULL; d
}

## ---- the percentage carried by the indirect path ----------------------------

#' Apply the post hoc reportability rule and split the pathways.
#'
#' POST HOC. This rule was written on 3 August 2026, after run_c9cb1a366dfb, at
#' the co-authors' request, and before the refit that tests it. It is not
#' prespecified and is never described as such.
#'
#' Returned in three parts, and every pathway appears in the audit whatever
#' happens to it:
#'
#'   released  passing pathways, with the percentage
#'   withheld  failing pathways, WITHOUT a percentage, naming the condition
#'   audit     all pathways, with every retained quantity, released or not
#'
#' A median outside 0 to 100 is not treated as a computational failure. It is the
#' inconsistent-mediation or suppression regime, it is retained in the audit, and
#' it fails the unit-interval condition on its own merits rather than by being
#' discarded.
#'
#' @param pmed the collated per-pathway ratio summaries.
#' @param diagnostics the fit diagnostics table, so the sampler condition is read
#'   from the parent fit rather than assumed.
proportion_mediated_tables <- function(cfg, pmed, diagnostics) {
  if (is.null(pmed) || !nrow(pmed)) return(NULL)
  pm <- cfg$proportion_mediated
  d  <- pmed[pmed$fit_class == "stan_primary" & pmed$moderator == "null", ]
  if (!nrow(d)) return(NULL)

  ## Condition one. The parent fit passed every sampler diagnostic already
  ## applied to the release, read from the diagnostics table rather than
  ## re-derived, so the two cannot drift apart.
  dg <- diagnostics[match(d$fit_id, diagnostics$fit_id), ]
  d$fit_diagnostics_pass <- diagnostics_pass(dg, cfg)

  ## Condition two. Separation of the DENOMINATOR from zero, at the primary
  ## displacement. The grid is carried alongside as a post hoc sensitivity range,
  ## not as a set of competing standards.
  primary_col <- sprintf("p_total_gt_%g", pm$delta_primary)
  if (!primary_col %in% names(d))
    stop("The gate needs ", primary_col, ", which the extraction did not produce. ",
         "delta_primary and delta_grid have drifted apart.")
  d$p_total_gt_delta <- d[[primary_col]]
  d$delta_primary    <- pm$delta_primary
  d$cond_separation  <- d$p_total_gt_delta >= pm$min_p_total_gt_delta

  ## Condition three. Mass of the ratio inside the unit interval. Outside it the
  ## word "percentage" stops conveying what a reader will take from it.
  d$cond_unit_interval <- d$p_in_range >= pm$min_p_in_unit_interval

  d$gate_pass <- d$fit_diagnostics_pass & d$cond_separation & d$cond_unit_interval
  d$failed_conditions <- vapply(seq_len(nrow(d)), function(i) {
    f <- c(if (!d$fit_diagnostics_pass[i]) "sampler diagnostics",
           if (!d$cond_separation[i])
             sprintf("P(total > %g) = %.3f, below %.2f", pm$delta_primary,
                     d$p_total_gt_delta[i], pm$min_p_total_gt_delta),
           if (!d$cond_unit_interval[i])
             sprintf("P(0 < percentage < 100) = %.3f, below %.2f",
                     d$p_in_range[i], pm$min_p_in_unit_interval))
    if (length(f)) paste(f, collapse = "; ") else NA_character_
  }, character(1))

  ## The heuristic, reported and never decisive. A signal-to-noise ratio on the
  ## denominator is not a Fieller-derived rule and is not presented as one; it is
  ## published so a reader can see how the two views of separation compare.
  for (h in unlist(pm$heuristic_median_sd_grid))
    d[[sprintf("heuristic_median_sd_ge_%g", h)]] <- d$total_median_sd_ratio >= h

  keep_released <- c("pathway", "label", "k_samples", "median", "q_lo", "q_hi",
                     "p_in_range", "p_total_gt_delta", "delta_primary",
                     "mcse_median", "mcse_q_lo", "mcse_q_hi", "n_draws")
  released <- d[d$gate_pass, intersect(keep_released, names(d)), drop = FALSE]

  ## The withheld table carries NO percentage. Naming the condition is the point;
  ## printing the number beside the warning is what the earlier release did.
  withheld <- d[!d$gate_pass,
                intersect(c("pathway", "label", "k_samples", "failed_conditions",
                            "p_in_range", "p_total_gt_delta",
                            "delta_primary"), names(d)), drop = FALSE]

  rownames(released) <- NULL; rownames(withheld) <- NULL
  audit <- d[, setdiff(names(d), "fit_class"), drop = FALSE]
  rownames(audit) <- NULL

  if (nrow(released) + nrow(withheld) != nrow(d))
    stop("The released and withheld percentage tables do not partition the ",
         nrow(d), " pathways.")

  list(released = released, withheld = withheld, audit = audit)
}

#' How the percentage and its released-or-withheld status move across every
#' sensitivity class the pathway was refitted under.
#'
#' The classification matters as much as the value. A percentage that shifts two
#' points but flips from released to withheld under a cohort deletion is more
#' fragile than one that shifts eight points and stays released throughout, and a
#' fixed movement threshold cannot see the difference.
proportion_mediated_stability <- function(cfg, pmed, diagnostics) {
  if (is.null(pmed) || !nrow(pmed)) return(NULL)
  ## The prior predictive fits are excluded. They are fitted with the likelihood
  ## switched off, so their ratio is a draw from the PRIOR, centred near zero
  ## because the prior deliberately does not presuppose mediation. Comparing a
  ## posterior median of 55 against a prior median of -0.003 and calling the
  ## difference instability would report the prior's neutrality as a defect, and
  ## it dominated every other class in the first run.
  d <- pmed[pmed$moderator == "null" & pmed$fit_class != "prior_predictive", ]
  if (!nrow(d)) return(NULL)
  pm <- cfg$proportion_mediated
  primary_col <- sprintf("p_total_gt_%g", pm$delta_primary)

  dg <- diagnostics[match(d$fit_id, diagnostics$fit_id), ]
  d$gate_pass <- diagnostics_pass(dg, cfg) &
    d[[primary_col]] >= pm$min_p_total_gt_delta &
    d$p_in_range >= pm$min_p_in_unit_interval

  do.call(rbind, lapply(split(d, d$pathway), function(g) {
    ref <- g[g$fit_class == "stan_primary", ]
    if (!nrow(ref)) return(NULL)
    do.call(rbind, lapply(split(g, g$fit_class), function(s) {
      data.frame(pathway = g$pathway[1], label = g$label[1],
                 fit_class = s$fit_class[1], n_refits = nrow(s),
                 primary_median = ref$median[1],
                 median_min = min(s$median), median_max = max(s$median),
                 max_abs_shift_from_primary = max(abs(s$median - ref$median[1])),
                 n_released = sum(s$gate_pass),
                 n_withheld = sum(!s$gate_pass),
                 classification_stable = all(s$gate_pass == ref$gate_pass[1]),
                 stringsAsFactors = FALSE)
    }))
  }))
}

## ---- tables -----------------------------------------------------------------

build_tables <- function(cfg, lock, results, cross_layer, freq, sbc) {
  prim_slopes <- results$slopes[results$slopes$fit_class %in%
                                  c("stan_primary", "brms_primary"), ]
  dz <- prim_slopes[prim_slopes$estimand == "d_z", ]

  ## Headline counts, computed from the tables they describe.
  fc <- frequentist_counts(freq)
  decided <- dz[!is.na(dz$decision) & dz$decision != "estimation only" &
                  dz$decision != "undecided" & dz$reportable != "not reported", ]

  headline <- data.frame(
    frequentist_slopes = fc$n_slopes,
    frequentist_significant = fc$n_significant,
    frequentist_models = fc$n_models,
    frequentist_models_with_significant = fc$n_models_with_significant,
    bayes_slopes_total = nrow(dz),
    bayes_slopes_reportable = sum(dz$reportable != "not reported"),
    bayes_slopes_not_reportable = sum(dz$reportable == "not reported"),
    bayes_decisions = nrow(decided),
    bayes_decisions_stan = sum(decided$fit_class == "stan_primary"),
    bayes_decisions_brms = sum(decided$fit_class == "brms_primary"),
    stringsAsFactors = FALSE)

  pooled <- results$pooled[results$pooled$fit_class == "stan_primary", ]
  predict <- results$predict[results$predict$fit_class == "stan_primary", ]

  ## Whether the total effect is separated from zero is retained as a reported
  ## fact about each pathway. The percentage carried by the indirect path uses a
  ## stronger condition on the same denominator, stated in config/analysis.yml
  ## and applied in proportion_mediated_tables() above. See
  ## bayesian/docs/proportion_mediated.md.
  tot <- pooled[pooled$quantity == "total", ]
  tot$total_excludes_zero <- tot$q_lo > 0 | tot$q_hi < 0
  pooled <- merge(pooled, tot[, c("pathway", "moderator", "contrast_index",
                                  "total_excludes_zero")],
                  by = c("pathway", "moderator", "contrast_index"), all.x = TRUE)

  ## Prediction-interval summary: how many indirect-effect intervals include
  ## zero, counted rather than asserted.
  ## Split by whether the interval is conditional on a moderator value. The
  ## moderator-endpoint intervals are NOT independent of the moderator analysis,
  ## so pooling all of them under that heading overstated the independent
  ## evidence.
  ind_pi <- predict[predict$quantity == "ind", ]
  unmod <- ind_pi$moderator == "null"
  zero_in <- ind_pi$q_lo < 0 & ind_pi$q_hi > 0
  pi_summary <- data.frame(
    n_intervals = nrow(ind_pi),
    n_including_zero = sum(zero_in),
    n_unmoderated = sum(unmod),
    n_unmoderated_including_zero = sum(unmod & zero_in),
    n_endpoint = sum(!unmod),
    n_endpoint_including_zero = sum(!unmod & zero_in),
    stringsAsFactors = FALSE)

  ## Leave-one-study-out stability for every slope that reached a decision.
  loso <- results$slopes[results$slopes$fit_class %in%
                           c("sens_loso_stan", "sens_loso_brms") &
                           results$slopes$estimand == "d_z", ]
  ## A deletion that collapses a binary arm below the minimum is NOT evidence
  ## about the decision's robustness: it is a model that should not have been
  ## reported at all. Such refits are counted separately and excluded from the
  ## quoted median range, because including them contaminates the range with
  ## estimates the project's own rule forbids reporting. Both counts are
  ## published so a reader can see the exclusion rather than infer it.
  stability <- NULL; loso_detail <- NULL
  if (nrow(decided) && nrow(loso)) {
    stability <- do.call(rbind, lapply(seq_len(nrow(decided)), function(i) {
      d <- decided[i, ]
      layer <- if (d$fit_class == "stan_primary") "sens_loso_stan" else "sens_loso_brms"
      g <- loso[loso$fit_class == layer & loso$pathway == d$pathway &
                  loso$moderator == d$moderator & loso$coordinate == d$coordinate, ]
      if (!nrow(g)) return(NULL)
      rep_g <- g[g$reportable != "not reported", ]
      data.frame(pathway = d$pathway, moderator = d$moderator,
                 coordinate = d$coordinate, layer = d$fit_class,
                 primary_decision = d$decision, primary_median = d$median,
                 n_deletions = nrow(g),
                 n_reportable_deletions = nrow(rep_g),
                 n_not_reportable_deletions = nrow(g) - nrow(rep_g),
                 n_retaining_decision = sum(rep_g$decision == d$decision),
                 median_min = if (nrow(rep_g)) min(rep_g$median) else NA_real_,
                 median_max = if (nrow(rep_g)) max(rep_g$median) else NA_real_,
                 stringsAsFactors = FALSE)
    }))

    ## The per-deletion rows themselves, so the exclusions are auditable by name
    ## rather than only by count.
    keep_cols <- intersect(
      c("fit_id", "fit_class", "pathway", "moderator", "coordinate", "drop_sample",
        "reportable", "k_samples", "median", "hdi_lower", "hdi_upper",
        "sesoi", "decision"), names(loso))
    loso_detail <- loso[, keep_cols, drop = FALSE]
    rownames(loso_detail) <- NULL
  }

  ## Prior-grid stability for every slope that reached a decision.
  grid <- results$slopes[results$slopes$fit_class == "sens_prior_grid" &
                           results$slopes$estimand == "d_z", ]
  prior_stability <- NULL
  if (nrow(decided) && nrow(grid)) {
    prior_stability <- do.call(rbind, lapply(seq_len(nrow(decided)), function(i) {
      d <- decided[i, ]
      g <- grid[grid$pathway == d$pathway & grid$moderator == d$moderator &
                  grid$coordinate == d$coordinate, ]
      if (!nrow(g)) return(NULL)
      data.frame(pathway = d$pathway, moderator = d$moderator,
                 coordinate = d$coordinate, primary_decision = d$decision,
                 n_variants = nrow(g),
                 n_retaining_decision = sum(g$decision == d$decision),
                 median_min = min(g$median), median_max = max(g$median),
                 stringsAsFactors = FALSE)
    }))
  }

  ## Every fit of every contrast that reached a decision, in one place.
  ##
  ## A decision whose interval clears the threshold by less than 0.001 cannot be
  ## honestly summarised by a count of variants that "retained" it: the
  ## categorical label flips on a boundary the estimates barely move across. The
  ## reader needs the estimates themselves, from every layer, prior variant,
  ## covariance construction, likelihood and deletion, with the distance from the
  ## threshold made explicit and the reportability of each refit shown.
  targeted <- NULL
  if (nrow(decided)) {
    all_dz <- results$slopes[results$slopes$estimand == "d_z", ]
    targeted <- do.call(rbind, lapply(seq_len(nrow(decided)), function(i) {
      d <- decided[i, ]
      g <- all_dz[all_dz$pathway == d$pathway & all_dz$moderator == d$moderator &
                    all_dz$coordinate == d$coordinate, ]
      if (!nrow(g)) return(NULL)
      rp <- lock$reportability
      arms <- rp$note[rp$pathway == d$pathway & rp$moderator == d$moderator]
      tau <- results$heterogeneity
      tau_max <- if (!is.null(tau)) {
        t2 <- tau[tau$pathway == d$pathway & tau$moderator == d$moderator &
                    tau$fit_class == "stan_primary", ]
        if (nrow(t2)) max(t2$median) else NA_real_
      } else NA_real_
      cbind(
        data.frame(target_pathway = d$pathway, target_moderator = d$moderator,
                   target_coordinate = d$coordinate,
                   parent_group_counts = if (length(arms)) arms[1] else NA_character_,
                   parent_tau_max = tau_max, stringsAsFactors = FALSE),
        g[, intersect(c("fit_id", "fit_class", "variant", "drop_sample", "reportable",
                        "k_samples", "median", "hdi_lower", "hdi_upper", "sesoi",
                        "decision", "p_gt0"), names(g)), drop = FALSE],
        ## How far the interval sits from the threshold that decides the label.
        ## A margin of 0.0006 is not a disagreement about the effect.
        data.frame(hdi_margin_from_sesoi =
                     pmin(abs(g$hdi_upper) - g$sesoi, abs(g$hdi_lower) - g$sesoi),
                   stringsAsFactors = FALSE))
    }))
    rownames(targeted) <- NULL
  }

  pmt <- proportion_mediated_tables(cfg, results$pmed, results$diagnostics)

  list(headline = headline, slopes = dz, slopes_all = prim_slopes,
       targeted_sensitivity = targeted,
       pmed_released = pmt$released, pmed_withheld = pmt$withheld,
       pmed_audit = pmt$audit, pmed_diagnostics = results$pmed_diag,
       pmed_draws = results$pmed_draws,
       pmed_stability = proportion_mediated_stability(cfg, results$pmed,
                                                      results$diagnostics),
       pooled = pooled, predict = predict, pi_summary = pi_summary,
       contrasts = results$contrasts[results$contrasts$fit_class == "stan_primary", ],
       cross_layer = cross_layer, frequentist = freq,
       reportability = lock$reportability, contrast_support = lock$support,
       stability = stability, loso_detail = loso_detail,
       prior_stability = prior_stability,
       sbc = sbc$per_parameter, sbc_audit = sbc$audit,
       sbc_by_scenario = sbc$by_scenario, sbc_ranks_raw = sbc$ranks_raw,
       heterogeneity = results$heterogeneity[
         results$heterogeneity$fit_class == "stan_primary", ],
       power_scaling = results$power_scaling,
       prior_predictive = results$prior_predictive,
       prior_predictive_verdict = results$prior_predictive_verdict,
       prior_predictive_draws = results$prior_predictive_draws,
       ppc_overall = results$ppc_overall, ppc_study = results$ppc_study,
       ppc_coordinate = results$ppc_coordinate,
       diagnostics = results$diagnostics, vz_comparison = lock$vz_comparison,
       decided = decided)
}

## ---- gates ------------------------------------------------------------------

gate_fit_completeness <- function(plan, ledger) {
  expected <- sort(plan$fit_id)
  completed <- sort(ledger$fit_id[ledger$status == "ok"])
  missing <- setdiff(expected, completed)
  extra   <- setdiff(completed, expected)
  if (length(missing) || length(extra)) {
    ## Name the failures and quote their errors, so one run identifies the
    ## culprit rather than requiring the whole analysis to be repeated blind.
    failed <- ledger[ledger$fit_id %in% missing & !is.na(ledger$error), ]
    detail <- if (nrow(failed))
      paste0("\n  ", failed$fit_id, ": ", substr(failed$error, 1, 160), collapse = "")
    else ""
    stop("Fit set mismatch. Missing ", length(missing), ": ",
         paste(utils::head(missing, 5), collapse = ", "),
         if (length(missing) > 5) " ..." else "",
         ". Unexpected ", length(extra), ": ",
         paste(utils::head(extra, 5), collapse = ", "), ".", detail)
  }
  if (anyDuplicated(completed))
    stop("Duplicate fit_id in the completed set.")
  data.frame(expected = length(expected), completed = length(completed), ok = TRUE)
}

PRIMARY_CLASSES <- c("stan_primary", "brms_primary", "prior_predictive")

gate_diagnostic_thresholds <- function(cfg, results) {
  d <- results$diagnostics
  if (is.null(d) || !nrow(d)) stop("No diagnostics were produced.")
  g <- cfg$diagnostics

  ## Everything except divergences applies to every fit without exception.
  ##
  ## These columns cover the sampled parameters and the deterministic estimands
  ## only. The `predictive_*` columns are reported alongside but deliberately
  ## not judged here: a *_new quantity redraws a random effect every iteration,
  ## so its R-hat measures intended noise. See GATED_PARS in lib_summaries.R.
  other_ok <- d$max_rhat < g$max_rhat & d$min_ess_bulk > g$min_ess_bulk &
    d$min_ess_tail > g$min_ess_tail & d$num_max_treedepth <= g$max_treedepth_hits &
    d$min_ebfmi > g$min_ebfmi
  if (any(!other_ok)) {
    bad <- d[!other_ok, ]
    stop(nrow(bad), " fits breached a convergence threshold, including: ",
         paste(utils::head(sprintf("%s (worst R-hat %.4f on %s, min bulk ESS %.0f on %s)",
                                   bad$fit_id, bad$max_rhat, bad$worst_rhat_variable,
                                   bad$min_ess_bulk, bad$worst_ess_bulk_variable), 5),
               collapse = "; "),
         ". Across all fits: max R-hat ", signif(max(d$max_rhat), 5),
         ", min tail ESS ", signif(min(d$min_ess_tail), 5),
         ", tree-depth hits ", sum(d$num_max_treedepth),
         ", min E-BFMI ", signif(min(d$min_ebfmi), 3), ".")
  }

  ## A reported result must rest on a pristine fit.
  is_primary <- d$fit_class %in% PRIMARY_CLASSES
  prim_div <- d[is_primary & d$num_divergent > g$max_divergent, ]
  if (nrow(prim_div))
    stop(nrow(prim_div), " PRIMARY fits retained divergent transitions: ",
         paste(sprintf("%s (%d)", prim_div$fit_id, prim_div$num_divergent), collapse = ", "),
         ". No tolerance applies to a fit whose result is reported.")

  ## A sensitivity refit may retain divergences only under the declared policy.
  sens <- d[!is_primary, ]
  sens_div <- sens[sens$num_divergent > 0, ]
  cap <- max(1, floor(g$sensitivity_divergence_tolerance_fraction * nrow(sens)))

  if (nrow(sens_div) > cap)
    stop(nrow(sens_div), " sensitivity refits retained divergent transitions, above the declared cap of ",
         cap, " (", 100 * g$sensitivity_divergence_tolerance_fraction, " per cent of ",
         nrow(sens), " sensitivity fits): ",
         paste(utils::head(sens_div$fit_id, 5), collapse = ", "), ".")
  if (any(sens_div$num_divergent > g$sensitivity_max_divergent_per_fit))
    stop("A sensitivity refit exceeded the per-fit divergence limit of ",
         g$sensitivity_max_divergent_per_fit, ": ",
         paste(sprintf("%s (%d)", sens_div$fit_id, sens_div$num_divergent)[
           sens_div$num_divergent > g$sensitivity_max_divergent_per_fit], collapse = ", "), ".")
  ## Tolerated only after the sampler has already been pushed as far as the
  ## configured ladder goes.
  max_steps <- length(unlist(cfg$sampling$adapt_delta_ladder)) - 1L
  lazy <- sens_div[sens_div$escalations < max_steps, ]
  if (nrow(lazy))
    stop(nrow(lazy), " sensitivity refits retained divergences without exhausting the adapt_delta ladder: ",
         paste(sprintf("%s (%d of %d escalations)", lazy$fit_id, lazy$escalations, max_steps),
               collapse = ", "), ".")

  d$passes <- diagnostics_pass(d, cfg)
  attr_out <- data.frame(fits = nrow(d), max_rhat = max(d$max_rhat),
             min_ess_bulk = min(d$min_ess_bulk), min_ess_tail = min(d$min_ess_tail),
             divergences = sum(d$num_divergent),
             treedepth_hits = sum(d$num_max_treedepth),
             min_ebfmi = min(d$min_ebfmi),
             models_escalated = sum(d$escalations > 0),
             escalation_steps = sum(d$escalations),
             primary_fits = sum(is_primary),
             sensitivity_fits = nrow(sens),
             sensitivity_divergence_cap = cap,
             sensitivity_fits_with_divergence = nrow(sens_div),
             tolerated = if (nrow(sens_div))
               paste(sprintf("%s (%d)", sens_div$fit_id, sens_div$num_divergent),
                     collapse = "; ") else "",
             ok = TRUE)
  attr_out
}

#' The expected reportable set is stated explicitly, so a missing contrast fails
#' rather than being read as an absent result. The deliberately non-reportable
#' models are an EXCLUDED expectation, not a gap.
gate_expected_reportable <- function(cfg, lock, tables) {
  rp <- lock$reportability
  expected <- rp[rp$status != "not reported", c("pathway", "moderator")]
  excluded <- rp[rp$status == "not reported", c("pathway", "moderator")]

  for (layer in c("stan_primary", "brms_primary")) {
    got <- unique(tables$slopes[tables$slopes$fit_class == layer,
                                c("pathway", "moderator")])
    e <- paste(expected$pathway, expected$moderator)
    g <- paste(got$pathway, got$moderator)
    miss <- setdiff(e, g)
    if (length(miss))
      stop(layer, " is missing expected reportable contrasts: ",
           paste(miss, collapse = ", "))
    ## Each expected contrast appears exactly once per coordinate.
    tab <- table(paste(tables$slopes$pathway, tables$slopes$moderator)[
      tables$slopes$fit_class == layer])
    if (any(tab[e] != length(PAIRS(cfg))))
      stop(layer, ": expected exactly ", length(PAIRS(cfg)),
           " coordinate rows per contrast; found ",
           paste(names(tab)[tab[e] != length(PAIRS(cfg))], collapse = ", "))
  }
  data.frame(expected_contrasts = nrow(expected),
             excluded_by_design = nrow(excluded), ok = TRUE)
}

## ---- promotion --------------------------------------------------------------

promote_run <- function(cfg, analysis_id, data_sha, lock, results, tables,
                        cross_layer, ledger, sbc, freq, gate_diag = NULL, root = ".") {
  staging <- file.path(root, "bayesian", "results", "_staging")
  final   <- file.path(root, "bayesian", "results", paste0("run_", analysis_id))
  unlink(staging, recursive = TRUE)
  for (d in c("tables", "diagnostics", "figures"))
    dir.create(file.path(staging, d), recursive = TRUE, showWarnings = FALSE)

  w <- function(x, sub, name) {
    if (is.null(x) || !NROW(x)) return(invisible(NULL))
    write.csv(x, file.path(staging, sub, name), row.names = FALSE, na = "")
  }

  ## Machine-readable record of the environment the numbers came off, so a
  ## reproduction attempt can diff package versions rather than guess.
  w(tryCatch({
      lk <- renv::lockfile_read(file.path(root, "renv.lock"))$Packages
      data.frame(package = names(lk),
                 version = vapply(lk, function(p) p$Version %||% NA_character_, character(1)),
                 source  = vapply(lk, function(p) p$Source %||% NA_character_, character(1)),
                 stringsAsFactors = FALSE)
    }, error = function(e) NULL), "diagnostics", "package_inventory.csv")

  w(tables$headline,        "tables", "headline_counts.csv")
  w(tables$slopes,          "tables", "moderator_contrasts.csv")
  w(tables$slopes_all,      "tables", "moderator_contrasts_all_estimands.csv")
  w(tables$pooled,          "tables", "pooled_quantities.csv")
  w(tables$predict,         "tables", "prediction_intervals.csv")
  w(tables$pi_summary,      "tables", "prediction_interval_summary.csv")
  w(tables$contrasts,       "tables", "mediation_contrasts.csv")
  w(tables$cross_layer,     "tables", "cross_layer_agreement.csv")
  w(tables$frequentist,     "tables", "frequentist_targets.csv")
  w(tables$reportability,   "tables", "reportability.csv")
  w(tables$contrast_support,"tables", "contrast_support.csv")
  w(tables$stability,       "tables", "leave_one_study_out_stability.csv")
  w(tables$loso_detail,     "tables", "leave_one_study_out_detail.csv")
  w(tables$targeted_sensitivity, "tables", "targeted_sensitivity.csv")
  w(tables$prior_stability, "tables", "prior_grid_stability.csv")
  w(tables$decided,         "tables", "decisions.csv")
  w(tables$vz_comparison,   "tables", "vz_comparison.csv")

  ## The percentage carried by the indirect path. Released and withheld are
  ## separate files so a withheld pathway cannot be read as a reported one, and
  ## the audit carries every pathway whatever its status.
  w(tables$pmed_released,   "tables", "proportion_mediated.csv")
  ## Written even when empty, headers and no rows. w() skips an empty frame, and
  ## an absent file cannot be distinguished from a forgotten one: a reader would
  ## have to infer "nothing was withheld" from a gap, which is the exact failure
  ## this design exists to avoid. An empty table says it explicitly.
  if (!is.null(tables$pmed_audit))
    write.csv(tables$pmed_withheld %||% tables$pmed_audit[0, , drop = FALSE],
              file.path(staging, "tables", "proportion_mediated_withheld.csv"),
              row.names = FALSE, na = "")
  w(tables$pmed_audit,      "tables", "proportion_mediated_audit.csv")
  w(tables$pmed_stability,  "tables", "proportion_mediated_stability.csv")
  w(tables$pmed_diagnostics, "diagnostics", "proportion_mediated_diagnostics.csv")
  ## The ratio's own draws, so it can be recomputed or replotted without a
  ## refit. Compressed: 12,000 draws per pathway is not small, and a summary
  ## cannot answer a question about the shape of a possibly bimodal posterior.
  if (!is.null(tables$pmed_draws) && NROW(tables$pmed_draws)) {
    gz <- gzfile(file.path(staging, "diagnostics", "pmed_draws.csv.gz"), "w")
    utils::write.csv(tables$pmed_draws, gz, row.names = FALSE, na = "")
    close(gz)
  }

  w(tables$diagnostics,     "diagnostics", "fit_diagnostics.csv")
  w(ledger,                 "diagnostics", "fit_ledger.csv")
  w(tables$heterogeneity,            "tables", "heterogeneity.csv")
  w(tables$power_scaling,            "diagnostics", "power_scaling.csv")
  w(tables$prior_predictive,         "tables", "prior_predictive.csv")
  w(tables$prior_predictive_verdict, "tables", "prior_predictive_verdict.csv")
  w(tables$sbc,             "diagnostics", "sbc_rank_uniformity.csv")
  w(tables$sbc_audit,       "diagnostics", "sbc_audit.csv")
  w(tables$sbc_by_scenario, "diagnostics", "sbc_by_scenario.csv")
  ## Raw truths, ranks and per-replicate diagnostics. Small enough to version,
  ## so exact recomputation of the calibration audit needs no refit.
  w(tables$sbc_ranks_raw,   "diagnostics", "sbc_ranks_raw.csv")
  w(tables$ppc_overall,     "diagnostics", "ppc_overall.csv")
  w(tables$ppc_study,       "diagnostics", "ppc_study.csv")
  w(tables$ppc_coordinate,  "diagnostics", "ppc_coordinate.csv")
  w(results$timings,        "diagnostics", "fit_timings.csv")
  ## The divergence policy and any fit tolerated under it, so a reader can see
  ## exactly what was allowed and on what basis rather than inferring it.
  w(gate_diag,              "diagnostics", "divergence_policy.csv")

  make_figures(cfg, tables, file.path(staging, "figures"))

  ## Checksums of everything, then the manifest LAST. An interrupted promotion
  ## therefore leaves no manifest and cannot be mistaken for a finished run.
  files <- list.files(staging, recursive = TRUE, full.names = TRUE)
  manifest <- data.frame(
    analysis_id = analysis_id,
    source_commit = ledger$source_commit[1],
    data_sha256 = data_sha,
    config_sha256 = cfg$.sha,
    stan_sha256 = sha256_file(file.path(root, "bayesian", "stan", "masem_valid.stan")),
    renv_sha256 = sha256_file(file.path(root, "renv.lock")),
    r_version = R.version.string,
    cmdstan_version = tryCatch(cmdstanr::cmdstan_version(), error = function(e) NA),
    ## The machine the numbers came off. Without this a reader attempting to
    ## reproduce the run cannot tell whether a small numerical difference is
    ## their platform or a real disagreement. No absolute paths are recorded.
    os = paste(Sys.info()[["sysname"]], Sys.info()[["release"]]),
    arch = R.version$arch,
    compiler = tryCatch(
      sub("\\s+$", "", system2(Sys.getenv("CXX", "c++"), "--version", stdout = TRUE)[1]),
      error = function(e) NA_character_, warning = function(w) NA_character_),
    generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    file = sub(paste0("^", staging, "/"), "", files),
    sha256 = vapply(files, sha256_file, character(1)),
    bytes = file.size(files),
    stringsAsFactors = FALSE)

  unlink(final, recursive = TRUE)
  dir.create(dirname(final), recursive = TRUE, showWarnings = FALSE)
  file.rename(staging, final)
  mpath <- file.path(final, "run_manifest.csv")
  write.csv(manifest, mpath, row.names = FALSE, na = "")
  mpath
}
