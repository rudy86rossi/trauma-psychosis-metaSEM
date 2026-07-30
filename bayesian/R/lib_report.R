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
    ppc_study      = if (length(ppc)) bind_ppc("study") else NULL,
    ppc_coordinate = if (length(ppc)) bind_ppc("coordinate") else NULL,
    ppc_overall    = if (length(ppc)) bind_ppc("overall") else NULL,
    timings = data.frame(fit_id = vapply(flat, function(x) x$fit_id, character(1)),
                         fit_class = vapply(flat, function(x) x$fit_class, character(1)),
                         seconds = vapply(flat, function(x) x$seconds, numeric(1)),
                         status = vapply(flat, function(x) x$status, character(1)),
                         stringsAsFactors = FALSE),
    draws_d_z = setNames(lapply(flat, function(x) x$draws_d_z),
                         vapply(flat, function(x) x$fit_id, character(1)))
  )
}

build_fit_ledger <- function(plan, results, analysis_id, data_sha, cfg) {
  src <- tryCatch(system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
                  error = function(e) NA_character_)
  d <- merge(plan, results$timings[, c("fit_id", "seconds", "status")],
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

  ## Proportion mediated is interpretable only where the total effect is
  ## separated from zero. The ratio remains mathematically unbounded and can be
  ## heavy-tailed when the total effect approaches zero, so the flag records
  ## interpretability rather than implying the interval is well behaved.
  tot <- pooled[pooled$quantity == "total", ]
  tot$total_excludes_zero <- tot$q_lo > 0 | tot$q_hi < 0
  pooled <- merge(pooled, tot[, c("pathway", "moderator", "contrast_index",
                                  "total_excludes_zero")],
                  by = c("pathway", "moderator", "contrast_index"), all.x = TRUE)
  pooled$interpretable <- ifelse(pooled$quantity == "pmed",
                                 pooled$total_excludes_zero, TRUE)

  ## Prediction-interval summary: how many indirect-effect intervals include
  ## zero, counted rather than asserted.
  ind_pi <- predict[predict$quantity == "ind", ]
  pi_summary <- data.frame(
    n_intervals = nrow(ind_pi),
    n_including_zero = sum(ind_pi$q_lo < 0 & ind_pi$q_hi > 0),
    stringsAsFactors = FALSE)

  ## Leave-one-study-out stability for every slope that reached a decision.
  loso <- results$slopes[results$slopes$fit_class %in%
                           c("sens_loso_stan", "sens_loso_brms") &
                           results$slopes$estimand == "d_z", ]
  stability <- NULL
  if (nrow(decided) && nrow(loso)) {
    stability <- do.call(rbind, lapply(seq_len(nrow(decided)), function(i) {
      d <- decided[i, ]
      layer <- if (d$fit_class == "stan_primary") "sens_loso_stan" else "sens_loso_brms"
      g <- loso[loso$fit_class == layer & loso$pathway == d$pathway &
                  loso$moderator == d$moderator & loso$coordinate == d$coordinate, ]
      if (!nrow(g)) return(NULL)
      data.frame(pathway = d$pathway, moderator = d$moderator,
                 coordinate = d$coordinate, layer = d$fit_class,
                 primary_decision = d$decision, primary_median = d$median,
                 n_deletions = nrow(g),
                 n_retaining_decision = sum(g$decision == d$decision),
                 median_min = min(g$median), median_max = max(g$median),
                 stringsAsFactors = FALSE)
    }))
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

  list(headline = headline, slopes = dz, slopes_all = prim_slopes,
       pooled = pooled, predict = predict, pi_summary = pi_summary,
       contrasts = results$contrasts[results$contrasts$fit_class == "stan_primary", ],
       cross_layer = cross_layer, frequentist = freq,
       reportability = lock$reportability, contrast_support = lock$support,
       stability = stability, prior_stability = prior_stability,
       sbc = sbc$per_parameter, sbc_audit = sbc$audit,
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
  if (length(missing) || length(extra))
    stop("Fit set mismatch. Missing ", length(missing), ": ",
         paste(utils::head(missing, 5), collapse = ", "),
         if (length(missing) > 5) " ..." else "",
         ". Unexpected ", length(extra), ": ",
         paste(utils::head(extra, 5), collapse = ", "), ".")
  if (anyDuplicated(completed))
    stop("Duplicate fit_id in the completed set.")
  data.frame(expected = length(expected), completed = length(completed), ok = TRUE)
}

gate_diagnostic_thresholds <- function(cfg, results) {
  d <- results$diagnostics
  if (is.null(d) || !nrow(d)) stop("No diagnostics were produced.")
  d$passes <- diagnostics_pass(d, cfg)
  bad <- d[!d$passes, ]
  if (nrow(bad))
    stop(nrow(bad), " fits breached a diagnostic threshold, including: ",
         paste(utils::head(bad$fit_id, 5), collapse = ", "),
         ". Max R-hat ", signif(max(d$max_rhat), 5),
         ", divergences ", sum(d$num_divergent),
         ", tree-depth hits ", sum(d$num_max_treedepth), ".")
  data.frame(fits = nrow(d), max_rhat = max(d$max_rhat),
             min_ess_bulk = min(d$min_ess_bulk), min_ess_tail = min(d$min_ess_tail),
             divergences = sum(d$num_divergent),
             treedepth_hits = sum(d$num_max_treedepth),
             min_ebfmi = min(d$min_ebfmi),
             models_escalated = sum(d$escalations > 0),
             escalation_steps = sum(d$escalations), ok = TRUE)
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
                        cross_layer, ledger, sbc, freq, root = ".") {
  staging <- file.path(root, "bayesian", "results", "_staging")
  final   <- file.path(root, "bayesian", "results", paste0("run_", analysis_id))
  unlink(staging, recursive = TRUE)
  for (d in c("tables", "diagnostics", "figures"))
    dir.create(file.path(staging, d), recursive = TRUE, showWarnings = FALSE)

  w <- function(x, sub, name) {
    if (is.null(x) || !NROW(x)) return(invisible(NULL))
    write.csv(x, file.path(staging, sub, name), row.names = FALSE, na = "")
  }

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
  w(tables$prior_stability, "tables", "prior_grid_stability.csv")
  w(tables$decided,         "tables", "decisions.csv")
  w(tables$vz_comparison,   "tables", "vz_comparison.csv")

  w(tables$diagnostics,     "diagnostics", "fit_diagnostics.csv")
  w(ledger,                 "diagnostics", "fit_ledger.csv")
  w(tables$sbc,             "diagnostics", "sbc_rank_uniformity.csv")
  w(tables$sbc_audit,       "diagnostics", "sbc_audit.csv")
  w(tables$ppc_overall,     "diagnostics", "ppc_overall.csv")
  w(tables$ppc_study,       "diagnostics", "ppc_study.csv")
  w(tables$ppc_coordinate,  "diagnostics", "ppc_coordinate.csv")
  w(results$timings,        "diagnostics", "fit_timings.csv")

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
