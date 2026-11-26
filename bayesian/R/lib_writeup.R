# =============================================================================
# lib_writeup.R
# Generates the results and methods documents from the promoted run.
#
# Every numerical statement is read from a table in
# bayesian/results/run_<analysis_id>/ and formatted here. Nothing is typed by
# hand. In the previous version of this analysis several hand-copied figures
# disagreed with their sources, which is the failure mode this removes rather
# than mitigates.
#
# British English. No em dashes.
# =============================================================================

fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)
fmt_paths <- function(st, status) {
  d <- st[st$status == status, ]
  if (!nrow(d)) return("no pathway")
  paste(sprintf("%s (%d of %d cohorts direct)", PATHWAY_PROSE[d$pathway], d$k_direct, d$k_primary),
        collapse = "; ")
}
pct <- function(x, d = 1) paste0(formatC(100 * x, format = "f", digits = d), " per cent")
ci  <- function(lo, hi, d = 3) sprintf("[%s, %s]", fmt(lo, d), fmt(hi, d))

#' A posterior probability, never rounded up to certainty.
#'
#' fmt(0.99985, 3) prints "1.000", which reads as a probability of one. No
#' posterior probability computed from 12,000 draws is one, and a reader is
#' entitled not to be told that it is. The documents already use the ">0.999"
#' convention in prose; this makes it the generated one too.
pfmt <- function(x, d = 3) {
  cap <- 1 - 10^(-d) / 2
  ifelse(x >= cap, paste0(">", fmt(1 - 10^(-d), d)), fmt(x, d))
}

#' Read every table from a promoted run.
read_run <- function(run_dir) {
  rd <- function(sub, name) {
    p <- file.path(run_dir, sub, name)
    if (!file.exists(p)) return(NULL)
    read.csv(p, stringsAsFactors = FALSE)
  }
  list(
    headline    = rd("tables", "headline_counts.csv"),
    slopes      = rd("tables", "moderator_contrasts.csv"),
    pooled      = rd("tables", "pooled_quantities.csv"),
    predict     = rd("tables", "prediction_intervals.csv"),
    pi_summary  = rd("tables", "prediction_interval_summary.csv"),
    contrasts   = rd("tables", "mediation_contrasts.csv"),
    cross_layer = rd("tables", "cross_layer_agreement.csv"),
    frequentist = rd("tables", "frequentist_targets.csv"),
    reportability = rd("tables", "reportability.csv"),
    support     = rd("tables", "contrast_support.csv"),
    stability   = rd("tables", "leave_one_study_out_stability.csv"),
    loso_detail = rd("tables", "leave_one_study_out_detail.csv"),
    targeted    = rd("tables", "targeted_sensitivity.csv"),
    prior_stab  = rd("tables", "prior_grid_stability.csv"),
    decided     = rd("tables", "decisions.csv"),
    vz          = rd("tables", "vz_comparison.csv"),
    eligibility = rd("tables", "synthesis_eligibility.csv"),
    direct_r    = rd("tables", "direct_correlation_sensitivity.csv"),
    pmed_released  = rd("tables", "proportion_mediated.csv"),
    pmed_withheld  = rd("tables", "proportion_mediated_withheld.csv"),
    pmed_audit     = rd("tables", "proportion_mediated_audit.csv"),
    pmed_stability = rd("tables", "proportion_mediated_stability.csv"),
    pmed_diagnostics = rd("diagnostics", "proportion_mediated_diagnostics.csv"),
    heterogeneity = rd("tables", "heterogeneity.csv"),
    prior_predictive = rd("tables", "prior_predictive.csv"),
    prior_predictive_verdict = rd("tables", "prior_predictive_verdict.csv"),
    diagnostics = rd("diagnostics", "fit_diagnostics.csv"),
    ledger      = rd("diagnostics", "fit_ledger.csv"),
    power_scaling = rd("diagnostics", "power_scaling.csv"),
    sbc         = rd("diagnostics", "sbc_rank_uniformity.csv"),
    sbc_audit   = rd("diagnostics", "sbc_audit.csv"),
    sbc_by_scenario = rd("diagnostics", "sbc_by_scenario.csv"),
    ppc_overall = rd("diagnostics", "ppc_overall.csv"),
    divergence  = rd("diagnostics", "divergence_policy.csv"),
    ppc_study   = rd("diagnostics", "ppc_study.csv"),
    manifest    = read.csv(file.path(run_dir, "run_manifest.csv"), stringsAsFactors = FALSE))
}

PATHWAY_PROSE <- c(
  dissoc_hal  = "Dissociation to hallucinations",
  dissoc_del  = "Dissociation to delusions",
  dissoc_cps  = "Dissociation to composite psychosis",
  negsch_cps  = "Negative schema to composite psychosis",
  negsch_del  = "Negative schema to delusions",
  depress_cps = "Depression to composite psychosis")

COORD_PROSE <- c(r_XM = "r(trauma, mediator)", r_XY = "r(trauma, psychosis)",
                 r_MY = "r(mediator, psychosis)")

md_table <- function(df, digits = 3) {
  df <- as.data.frame(df)
  fmtcol <- function(x) if (is.numeric(x)) formatC(x, format = "g", digits = digits) else as.character(x)
  body <- as.data.frame(lapply(df, fmtcol), stringsAsFactors = FALSE)
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |"),
    apply(body, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}

## ---- the percentage carried by the indirect path ----------------------------

#' The reported paragraph and its six-row table.
#'
#' Every pathway keeps its row. A failing pathway prints "not stably estimable"
#' rather than vanishing, so the withholding is visible and countable in the
#' table a reader actually looks at, while the released and withheld files keep
#' the machine-readable split.
writeup_proportion_mediated <- function(cfg, R) {
  rel <- R$pmed_released; wit <- R$pmed_withheld; aud <- R$pmed_audit
  if (is.null(aud) || !nrow(aud))
    return(paste("**The percentage carried by the indirect path is not reported.** The",
                 "run produced no summaries for it."))

  pm <- cfg$proportion_mediated
  out <- character(0)
  add <- function(...) out <<- c(out, ...)

  add(paste0(
    "**Percentage of the pooled association represented by the indirect path.** ",
    "This quantity is a POST HOC analysis amendment, added at the co-authors' request ",
    "after the previous release and withheld from it. It is not prespecified. Its ",
    "estimand, reporting rule and diagnostics were fixed before the refit that ",
    "produced the values below. It is a descriptive companion to the indirect effect ",
    "ab and does not replace ab as the evidence for mediation."), "")

  add(paste0(
    "The estimand is ab / total, evaluated draw-wise at the population-average ",
    "parameter values of the unmoderated model, expressed as a percentage. It is not ",
    "the average of the study-specific ratios: the transformation is nonlinear, so ",
    "the two differ. The denominator is the pooled zero-order correlation between ",
    "adversity and psychosis, which has positive posterior density at zero, so the ",
    "ratio has no finite mean and no finite standard deviation. It is therefore ",
    "summarised by its posterior median and equal-tailed 95 per cent credible ",
    "interval, and no mean or standard deviation of it appears anywhere in this ",
    "release. Rank-normalised R-hat, effective sample size and Monte Carlo error at ",
    "each reported quantile are given in `diagnostics/proportion_mediated_diagnostics.csv`; ",
    "the rank-normalised statistic depends only on the ordering of the draws and is ",
    "insensitive to whether the estimand has moments (Vehtari et al., 2021)."), "")

  add(sprintf(paste(
    "A pathway's percentage is released only if its parent fit passed every sampler",
    "diagnostic, the posterior probability that the total association exceeds %g is at",
    "least %.2f, and the posterior probability that the percentage lies between 0 and",
    "100 is at least %.2f. The displacement %g is a post hoc sensitivity threshold, not",
    "a clinically validated boundary, and %s are carried alongside it in",
    "`tables/proportion_mediated_audit.csv`. %s; every pathway is retained in the audit",
    "whatever its status."),
    pm$delta_primary, pm$min_p_total_gt_delta, pm$min_p_in_unit_interval,
    pm$delta_primary,
    paste(fmt(setdiff(unlist(pm$delta_grid), pm$delta_primary), 3), collapse = " and "),
    ## "All six pathways met the rule" rather than "Of 6 pathways, 6 met the
    ## rule and 0 did not", which is what a template with no zero case prints.
    if (NROW(wit) == 0) sprintf("All %d pathways met the rule", nrow(aud))
    else if (NROW(rel) == 0) sprintf("No pathway met the rule; all %d were withheld", nrow(aud))
    else sprintf("%d of %d pathways met the rule and %d did not",
                 NROW(rel), nrow(aud), NROW(wit))), "")

  ## One row per pathway, in the document's fixed pathway order.
  tab <- do.call(rbind, lapply(names(PATHWAY_PROSE), function(k) {
    a <- aud[aud$pathway == k, ]
    if (!nrow(a)) return(NULL)
    passed <- isTRUE(a$gate_pass[1])
    data.frame(
      Pathway = PATHWAY_PROSE[k], k = a$k_samples[1],
      `Percentage [95% CrI]` = if (passed)
        sprintf("%s %s", fmt(a$median[1], 1), ci(a$q_lo[1], a$q_hi[1], 1))
        else "not stably estimable",
      `P(total > threshold)` = pfmt(a$p_total_gt_delta[1]),
      `P(0 < percentage < 100)` = pfmt(a$p_in_range[1]),
      Status = if (passed) "released" else "withheld",
      check.names = FALSE, stringsAsFactors = FALSE)
  }))
  add(md_table(tab), "")

  if (NROW(wit))
    add(sprintf("Withheld, and why: %s.",
                paste(sprintf("%s (%s)", PATHWAY_PROSE[wit$pathway],
                              wit$failed_conditions), collapse = "; ")), "")

  ## Stability across every class the pathway was refitted under. The
  ## classification matters as much as the value: a percentage that moves two
  ## points but flips from released to withheld under a deletion is more fragile
  ## than one that moves eight and stays released, and a fixed movement threshold
  ## cannot tell the two apart.
  CLASS_PROSE <- c(sens_robust_t = "the robust Student-t likelihood",
                   sens_vz_weighted = "the weighted sampling-covariance construction",
                   sens_alt_instrument = "the alternative instrument",
                   sens_prior_grid = "the prior grid",
                   sens_loso_stan = "cohort deletion",
                   sens_direct_r = "the direct-correlation restriction")
  st <- R$pmed_stability
  if (!is.null(st) && nrow(st)) {
    ## The prior predictive fits are not refits of the posterior: their ratio is
    ## a draw from the prior, centred near zero because the prior does not
    ## presuppose mediation, so comparing it here would report the prior's
    ## neutrality as instability.
    s <- st[!st$fit_class %in% c("stan_primary", "prior_predictive"), ]
    unstable <- s[!s$classification_stable, ]
    add(sprintf(paste(
      "Each pathway was refitted under a robust Student-t likelihood, a weighted",
      "sampling-covariance construction, an alternative instrument where one exists, a",
      "prior grid on the intercept and heterogeneity scales, every single-cohort",
      "deletion and, where it was estimable, a restriction to triplets taken directly",
      "from a correlation matrix: %d refits in total. The largest shift in any pathway's median across",
      "all of them was %s percentage points. The released-or-withheld classification",
      "was unchanged in %d of %d pathway-by-class comparisons%s. Per-class detail is in",
      "`tables/proportion_mediated_stability.csv`."),
      sum(s$n_refits), fmt(max(s$max_abs_shift_from_primary), 1),
      sum(s$classification_stable), nrow(s),
      if (nrow(unstable))
        sprintf(", the exceptions being %s",
                paste(sprintf("%s under %s", PATHWAY_PROSE[unstable$pathway],
                              CLASS_PROSE[unstable$fit_class]), collapse = ", "))
      else ""), "")
  }

  ## Synthesis eligibility and the protocol-specified conversion sensitivity.
  el <- R$eligibility
  if (!is.null(el) && nrow(el)) {
    ex <- el[!el$eligible, ]
    add(sprintf(paste(
      "Pathways were synthesised only where at least %d cohorts contributed, counting each",
      "distinct sample identifier once,",
      "in line with the minimum of five studies specified in the PROSPERO",
      "pre-registration. %d of %d extracted pathways met this%s."),
      el$min_independent_cohorts[1], sum(el$eligible), nrow(el),
      if (nrow(ex)) sprintf("; %s was extracted and locked but not synthesised",
                            paste(sprintf("%s (k = %d)", PATHWAY_PROSE[ex$pathway], ex$k_independent),
                                  collapse = ", ")) else ""), "")
  }
  dr <- R$direct_r
  if (!is.null(dr) && nrow(dr)) {
    st <- unique(dr[, c("pathway", "status", "k_primary", "k_direct")])
    add(sprintf(paste(
      "As the protocol specifies, effect sizes obtained by conversion rather than read",
      "from a correlation matrix were examined in a sensitivity analysis restricted to",
      "direct correlations. It was fitted for %s; it was identical to the primary model",
      "for %s; and it was not estimable, because fewer than the minimum number of",
      "cohorts would remain, for %s. Values are in",
      "`tables/direct_correlation_sensitivity.csv`."),
      fmt_paths(st, "fitted"), fmt_paths(st, "identical"), fmt_paths(st, "not estimable")), "")
  }

  add(paste(
    "No prediction interval is defined for this percentage. A ratio computed in a new",
    "study would be especially unstable, because the predictive distribution of the",
    "denominator is far more diffuse than its posterior. The credible interval above",
    "belongs to the population-average percentage; the prediction intervals reported",
    "in this section belong to the latent true indirect effect ab in a new study. They",
    "are different quantities and are not comparable."), "")

  add(paste(
    "This is a model-based decomposition of pooled, largely cross-sectional",
    "associations. It is not an individual-level causal percentage and it is not the",
    "expected effect of intervening on the mediator. Percentages mediated reported in",
    "the descriptive systematic review are a separate summary of the individual",
    "published analyses and are not estimates from these models. See",
    "`docs/proportion_mediated.md`."), "")
  out
}

## ---- results document -------------------------------------------------------

write_results_doc <- function(R, cfg, lock, path) {
  h  <- R$headline
  L  <- character(0)
  add <- function(...) L <<- c(L, ...)

  stan_slopes <- R$slopes[R$slopes$fit_class == "stan_primary", ]
  brms_slopes <- R$slopes[R$slopes$fit_class == "brms_primary", ]
  rep_stan <- stan_slopes[stan_slopes$reportable != "not reported", ]
  rep_brms <- brms_slopes[brms_slopes$reportable != "not reported", ]
  dec <- R$decided

  add("# Bayesian meta-regression: results", "",
      "Prepared by ricardotwumasi. British English throughout. Methods are in",
      "`BAYESIAN_METAREG_METHODS.md`.", "",
      sprintf("Every number below is generated from the tables in `results/run_%s/`.",
              R$manifest$analysis_id[1]),
      "None is transcribed by hand.", "")

  # --- 1. headline
  add("## 1. What the analysis found", "")
  ## The results document reports k per pathway and counts cohorts throughout, so
  ## it states what a cohort currently rests on before it reports anything else.
  ## check_documents() asserts this sentence verbatim in both documents.
  add(identity_caveat(lock$samples, lock$sample_map, lock$effects), "")
  add(sprintf(paste("The matched frequentist meta-regressions produced %d of %d moderator slopes at",
                    "p < 0.05, across %d of %d pathway-by-moderator models. These are retained as",
                    "internal validation targets only. They are not co-primary, and their p values",
                    "are not used to arbitrate a disagreement with the posterior."),
              h$frequentist_significant, h$frequentist_slopes,
              h$frequentist_models_with_significant, h$frequentist_models), "")

  if (h$bayes_decisions == 0) {
    add(sprintf(paste("In the Bayesian analysis, **none** of the %d reportable coordinate slopes met",
                      "the interval decision rule in either model layer. A further %d slopes were",
                      "prespecified as not reportable and are excluded from that count."),
                h$bayes_slopes_reportable, h$bayes_slopes_not_reportable), "")
    add(paste("The moderator evidence is therefore inconclusive rather than null. The data do not",
              "distinguish a moderating effect large enough to matter from no such effect, which",
              "is a statement about this evidence base and not about the world."), "")
  } else {
    ## Counted PER LAYER, not pooled. The two layers estimate the SAME set of
    ## estimands, so "n of 138 slopes" invites a reader to treat them as
    ## independent replications and halves the apparent rate. The honest
    ## denominator is the number of reportable contrasts in one layer.
    n_per_layer <- h$bayes_slopes_reportable / 2
    add(sprintf(paste("In the Bayesian analysis, %d of %g reportable primary coordinate contrasts met",
                      "the interval-plus-SESOI rule, and %d of the %g corresponding reference",
                      "contrasts did. The two layers analyse the same estimands and are not",
                      "independent replications, so they are counted separately rather than pooled",
                      "into a single denominator. A further %d contrasts per layer were",
                      "prespecified as not reportable."),
                h$bayes_decisions_stan, n_per_layer,
                h$bayes_decisions_brms, n_per_layer,
                h$bayes_slopes_not_reportable / 2), "")
    add("", "| Pathway | Moderator | Coordinate | Layer | Contrast | Median | 95% HDI | Decision |",
        "| --- | --- | --- | --- | --- | ---: | --- | --- |")
    for (i in seq_len(nrow(dec)))
      add(sprintf("| %s | %s | %s | %s | %s | %s | %s | %s |",
                  PATHWAY_PROSE[dec$pathway[i]],
                  cfg$moderators[[dec$moderator[i]]]$label,
                  COORD_PROSE[dec$coordinate[i]],
                  if (dec$fit_class[i] == "stan_primary") "primary" else "reference (brms)",
                  dec$contrast[i], fmt(dec$median[i]),
                  ci(dec$hdi_lower[i], dec$hdi_upper[i]), dec$decision[i]))
    add("")
    add(paste("A finding that appears in only one layer is labelled as belonging to that layer.",
              "The primary model is the valid-correlation model; the brms model is a reference",
              "implementation and is not the arbiter when the two disagree."), "")
  }

  # --- 2. what did not change
  add("## 2. Findings that do not depend on the moderator analysis", "")
  pi <- R$pi_summary
  ## The 54 stored intervals are not one homogeneous set: 6 come from the
  ## unmoderated models and 48 are moderator-endpoint predictions, which are
  ## conditional on a moderator value and therefore emphatically NOT independent
  ## of the moderator analysis. Reporting them as a single count under a heading
  ## about findings independent of the moderators was misleading.
  add(sprintf(paste("**Prediction intervals for the latent true study effect.** All %d intervals for",
                    "the indirect effect ab in a new study include zero: the %d from the",
                    "unmoderated models, and the %d moderator-endpoint intervals, which are",
                    "conditional on a moderator value and so belong to the moderator analysis",
                    "rather than standing apart from it."),
              pi$n_including_zero, pi$n_unmoderated, pi$n_endpoint), "")
  add(paste("These intervals describe the range of LATENT TRUE effects expected across new studies",
            "drawn from the estimated population. They do not add the sampling error a future",
            "observed estimate would also carry, so a realised estimate would scatter more widely",
            "still. Their width is driven entirely by the between-study heterogeneity tau, which is",
            "weakly identified at this number of studies and therefore substantially",
            "prior-informed; the tau posteriors are reported in `tables/heterogeneity.csv` so that",
            "claim can be checked rather than taken on trust."), "")

  add(writeup_proportion_mediated(cfg, R))

  # --- 3. pooled quantities
  add("## 3. Pooled mediation quantities", "")
  add(paste("Population-average quantities from the primary model, unmoderated. The total effect",
            "is c' + ab and equals r(trauma, psychosis) exactly by construction."), "")
  pooled <- R$pooled[R$pooled$moderator == "null", ]
  tab <- do.call(rbind, lapply(names(PATHWAY_PROSE), function(k) {
    g <- function(q, f) pooled[[f]][pooled$pathway == k & pooled$quantity == q]
    pr <- R$predict[R$predict$moderator == "null" & R$predict$pathway == k &
                      R$predict$quantity == "ind", ]
    if (!length(g("a", "median"))) return(NULL)
    tau <- R$heterogeneity[R$heterogeneity$moderator == "null" &
                             R$heterogeneity$pathway == k, ]
    data.frame(Pathway = PATHWAY_PROSE[k],
               ## k for THIS fit, from the unmoderated rows. Taking it from
               ## lock$reportability[...][1] silently returned the first
               ## moderator's row, which is `age`, so negative schema to
               ## composite psychosis was published as k = 6 when seven samples
               ## were fitted: Dhondt 2022 has no recorded age.
               k = g("a", "k_samples"),
               a = fmt(g("a", "median")), b = fmt(g("b", "median")),
               `c'` = fmt(g("cprime", "median")),
               ab = sprintf("%s %s", fmt(g("ind", "median")),
                            ci(g("ind", "q_lo"), g("ind", "q_hi"))),
               `P(ab>0)` = fmt(g("ind", "p_gt0"), 2),
               `max tau` = if (nrow(tau)) fmt(max(tau$median)) else "",
               `new-study PI` = ci(pr$q_lo, pr$q_hi),
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
  add(md_table(tab), "")
  add(paste("`ab` is reported with its equal-tailed 95 per cent credible interval. Equal-tailed and",
            "highest-density intervals do not always agree on whether zero is excluded, so the",
            "convention is named rather than left implicit; the HDI is given for every quantity in",
            "`tables/pooled_quantities.csv`. `max tau` is the largest of the three coordinate",
            "heterogeneity posterior medians, given here because the prediction intervals are",
            "driven entirely by tau; the full posteriors are in `tables/heterogeneity.csv`."), "")

  # --- 4. moderator results
  add("## 4. Moderator results", "")
  add(paste("Moderation is reported as the posterior change in each zero-order Fisher-z coordinate",
            "across the contrast the data support. The smallest effect size of interest is 0.05",
            "Fisher-z across the nominal scientific contrast, held at a constant per-unit",
            "threshold, so a contrast clamped to observed support carries a proportionally smaller",
            "bound rather than a more permissive one."), "")
  clam <- R$support[R$support$clamped, ]
  if (nrow(clam)) {
    add("", "Two contrasts are clamped:", "")
    add(md_table(data.frame(
      Pathway = PATHWAY_PROSE[clam$pathway], Moderator = clam$moderator,
      Nominal = sprintf("%g to %g", clam$nominal_lo, clam$nominal_hi),
      Estimated = sprintf("%g to %g", clam$supported_lo, clam$supported_hi),
      `Span (units)` = fmt(clam$supported_span, 2),
      `Null-interval bound` = fmt(clam$sesoi_reported),
      check.names = FALSE, stringsAsFactors = FALSE)), "")
    add(paste("These are reported with the contrast actually estimated. The nominal label is not",
              "reused for a shorter comparison."), "")
  }
  add("", sprintf("Decision counts across the %d reportable slopes in each layer:",
                  nrow(rep_stan)), "")
  add(md_table(data.frame(
    Layer = c("Primary (valid-correlation)", "Reference (brms)"),
    Reject = c(sum(rep_stan$decision == "reject null interval"),
               sum(rep_brms$decision == "reject null interval")),
    Accept = c(sum(rep_stan$decision == "accept null interval"),
               sum(rep_brms$decision == "accept null interval")),
    Undecided = c(sum(rep_stan$decision == "undecided"),
                  sum(rep_brms$decision == "undecided")),
    stringsAsFactors = FALSE)), "")

  # --- 5. robustness
  add("## 5. Robustness of any decision reached", "")
  if (!is.null(R$stability) && nrow(R$stability)) {
    add("Leave-one-study-out, for every slope that reached a decision:", "")
    ## Reportability is recomputed after each deletion, not inherited. Deleting a
    ## study can collapse a binary arm below the project's own minimum, and such
    ## a refit is not evidence about robustness: it is a model the rule forbids
    ## reporting. It is counted separately and excluded from the median range.
    add(md_table(data.frame(
      Pathway = PATHWAY_PROSE[R$stability$pathway], Moderator = R$stability$moderator,
      Coordinate = COORD_PROSE[R$stability$coordinate],
      Deletions = R$stability$n_deletions,
      Reportable = R$stability$n_reportable_deletions,
      `Retaining the decision` = R$stability$n_retaining_decision,
      `Median range (reportable)` = sprintf("%s to %s", fmt(R$stability$median_min),
                                            fmt(R$stability$median_max)),
      check.names = FALSE, stringsAsFactors = FALSE)), "")
    nnr <- sum(R$stability$n_not_reportable_deletions)
    if (nnr)
      add("", sprintf(paste("%d deletion(s) left a group below the minimum per arm and are",
                            "therefore **not reportable**. They are excluded from the counts and",
                            "the median range above, and named in",
                            "`tables/leave_one_study_out_detail.csv`. Counting them would treat",
                            "an estimate the project's own rule forbids reporting as evidence",
                            "about the decision's robustness."), nnr), "")
    frag <- R$stability[R$stability$n_retaining_decision < R$stability$n_reportable_deletions, ]
    if (nrow(frag))
      add("", paste("Where a decision does not survive every reportable single-study deletion, the",
                    "correct statement is that **the interval decision is not leave-one-study-out",
                    "robust**. That is weaker than saying the direction is unidentified: the point",
                    "estimates may remain consistent in sign while the interval crosses the",
                    "decision boundary."), "")
    if (!is.null(R$targeted) && nrow(R$targeted))
      add("", sprintf(paste("Because a categorical decision can flip when an interval endpoint moves",
                            "by less than a thousandth, `tables/targeted_sensitivity.csv` gives every",
                            "one of the %d fits of the contrast(s) that reached a decision: both",
                            "layers, every prior variant, both sampling-covariance constructions,",
                            "the robust likelihood and every deletion, each with its own",
                            "reportability and its interval's distance from the threshold. A",
                            "variant that changed label without meaningfully changing the estimate",
                            "is visible there as such."), nrow(R$targeted)), "")
  } else {
    add("No slope reached a decision, so there is no decision whose robustness to test.", "")
    add(paste("The leave-one-study-out and prior-grid refits were nevertheless run for every",
              "moderator model and are archived in the diagnostics directory."), "")
  }
  if (!is.null(R$prior_stab) && nrow(R$prior_stab)) {
    add("", "Prior sensitivity, for the same slopes:", "")
    add(md_table(data.frame(
      Pathway = PATHWAY_PROSE[R$prior_stab$pathway], Moderator = R$prior_stab$moderator,
      Coordinate = COORD_PROSE[R$prior_stab$coordinate],
      Variants = R$prior_stab$n_variants,
      `Retaining the decision` = R$prior_stab$n_retaining_decision,
      check.names = FALSE, stringsAsFactors = FALSE)), "")
  }

  # --- 6. cross-layer
  add("## 6. Agreement between the two model layers", "")
  cl <- R$cross_layer
  lin <- cl[cl$comparison_kind == "matched linear coordinate", ]
  arch <- cl[cl$comparison_kind == "architecture sensitivity", ]
  add(paste("The layers are compared on one estimand: the change in each zero-order Fisher-z",
            "coordinate across the reported contrast. The primary model's own coefficient for the",
            "third coordinate acts on a **partial** correlation and is not the reference model's",
            "r(mediator, psychosis) slope, so comparing those two directly would compare different",
            "quantities."), "")
  add("", paste("The two models are fitted separately, so their draws do not correspond. The",
                "comparison is therefore between marginal posteriors: no draw of one model is",
                "paired with a draw of the other."), "")
  add("", sprintf(paste("Across %d matched linear coordinates the median difference is %s posterior",
                        "standard deviations at worst, with density overlap of at least %s. Across",
                        "%d comparisons on r(mediator, psychosis) the worst median difference is %s",
                        "standard deviations."),
                  nrow(lin), fmt(max(abs(lin$median_diff_in_pooled_sd)), 3),
                  fmt(min(lin$density_overlap), 2), nrow(arch),
                  fmt(max(abs(arch$median_diff_in_pooled_sd)), 3)), "")
  add(paste("Agreement on the third coordinate is an **architecture-sensitivity** result, not a",
            "check that preprocessing is identical: the reference model reaches the marginal",
            "Fisher-z linearly, the primary model reaches it nonlinearly through the",
            "valid-correlation map."), "")

  # --- 7. model checking
  add("## 7. Model checking", "")
  dg <- R$diagnostics
  ndiv <- sum(dg$num_divergent)
  add(sprintf(paste("**Sampling.** Across all %d fits: maximum R-hat %s, minimum bulk effective",
                    "sample size %s, minimum tail effective sample size %s, %s, %d",
                    "maximum-tree-depth events, minimum E-BFMI %s. Every fit met the convergence",
                    "targets; the divergence position is set out below."),
              nrow(dg), fmt(max(dg$max_rhat), 4), formatC(min(dg$min_ess_bulk), format = "d"),
              formatC(min(dg$min_ess_tail), format = "d"),
              if (ndiv == 1) "1 divergent transition" else sprintf("%d divergent transitions", ndiv),
              sum(dg$num_max_treedepth), fmt(min(dg$min_ebfmi), 2)), "")
  esc <- dg[dg$escalations > 0, ]
  add(sprintf(paste("%d of %d fits required an adapt_delta escalation, involving %d escalation",
                    "steps in total. Escalation is recorded per fit in the fit ledger rather than",
                    "summarised away."),
              nrow(esc), nrow(dg), sum(dg$escalations)), "")

  dv <- R$divergence
  if (!is.null(dv) && nrow(dv)) {
    add("", sprintf(paste("Divergent transitions: %d in total across all %d fits. Every one of the %d",
                          "primary fits, whose numbers are reported, had none; no tolerance applies",
                          "to those."),
                    dv$divergences, dv$fits, dv$primary_fits), "")
    if (nzchar(dv$tolerated)) {
      add(sprintf(paste("%d of the %d sensitivity refits retained a divergence after exhausting the",
                        "adapt_delta ladder, against a cap of %d, namely %s. Deleting a study from",
                        "an already small pathway can produce a geometry the sampler finds harder",
                        "than the model being reported. They are named here rather than absorbed",
                        "into a summary, so a reader can check whether any of them underlies a",
                        "reported robustness count."),
                  dv$sensitivity_fits_with_divergence, dv$sensitivity_fits,
                  dv$sensitivity_divergence_cap, dv$tolerated), "")
      add(paste("**The tolerance was adopted after a run exposed the issue, not prespecified for",
                "this analysis.** It is stated as a rule for subsequent runs, and the cap was fixed",
                "before the count was inspected, but describing it as prespecified here would be",
                "false. Every threshold other than this one applied to every fit without",
                "exception."), "")
    } else {
      add("No sensitivity refit retained a divergent transition either.", "")
    }
  }

  if (!is.null(R$ppc_overall)) {
    po <- R$ppc_overall[R$ppc_overall$moderator == "null", ]
    add("", sprintf(paste("**Posterior predictive checks.** Both replication forms were computed for",
                          "all %d pathways. Conditional coverage of the 95 per cent replicated",
                          "intervals averaged %s and new-study coverage %s. %d studies had a",
                          "posterior predictive p value outside [0.05, 0.95]."),
                    nrow(po), pct(mean(po$coverage_conditional)),
                    pct(mean(po$coverage_new_study)), sum(po$n_extreme_ppp)), "")
    add(paste("Both forms are reported because a conditional check alone can look satisfactory",
              "simply because the study-level random effects have absorbed an outlying study."), "")
    add(paste("These checks detected **no gross conflict between the model and the data**. That is",
              "not a demonstration of predictive calibration, and the checks should not be",
              "described as having been passed. Complete in-sample coverage is a weak result: with",
              "four to ten studies the intervals are wide, so coverage near one is close to",
              "guaranteed and carries little information. What was examined is the joint",
              "discrepancy of each study's three coordinates under both replication forms, plus",
              "coordinate-wise residuals. What these checks would NOT detect includes a misspecified",
              "between-study distribution shape, dependence between studies sharing participants,",
              "and any error in the plug-in sampling covariance matrices, which are treated as",
              "known throughout."), "")
  }

  if (!is.null(R$prior_predictive_verdict)) {
    pv <- R$prior_predictive_verdict
    add("", sprintf(paste("**Prior predictive check.** %d prespecified plausibility criteria across",
                          "%d pathways, all of which were met. The thresholds are recorded in",
                          "`config/analysis.yml` and were committed before the run that tested",
                          "them; the configuration hash forms part of the analysis identifier, so",
                          "that ordering is verifiable rather than asserted. The criteria require",
                          "the priors to COVER the published correlations tabulated in",
                          "`bayesian/docs/prior_scope.md`, not to encode them: the priors remain",
                          "weakly informative and are not literature-derived."),
                    nrow(pv), length(unique(pv$pathway))), "")
  }

  if (!is.null(R$power_scaling) && nrow(R$power_scaling)) {
    ps <- R$power_scaling
    ## Three states, not two. priorsense returns "-" for an evaluated quantity
    ## with no flag, and an EMPTY diagnosis for a quantity it could not evaluate
    ## at all: d_z has no contrast to span in an intercept-only model, so those
    ## rows carry blank sensitivities. Testing `!= "-"` counted every one of
    ## those non-evaluable rows as a positive flag, which inflated the reported
    ## total. A blank is not an unflagged row either, so it is reported
    ## separately rather than folded into the denominator.
    ps <- powerscale_status(ps)
    evaluable <- ps[ps$ps_status != "inapplicable", ]
    flagged   <- ps[ps$ps_status == "flagged", ]
    inapp     <- sum(ps$ps_status == "inapplicable")
    add("", sprintf(paste("**Prior and likelihood power scaling.** Sensitivity was archived for %d",
                          "quantities across the primary fits, of which %d were evaluable; %d rows",
                          "were structurally inapplicable, being the moderator contrast in an",
                          "intercept-only model, which has no contrast to span. Of the evaluable",
                          "quantities %d were flagged and %d were not, and %d of the flagged were",
                          "heterogeneity parameters. This asks a different question from the prior",
                          "grid: the grid refits at other prior widths and asks whether the",
                          "conclusion moves, whereas power scaling asks which of the prior and the",
                          "likelihood is driving each posterior."),
                    nrow(ps), nrow(evaluable), inapp,
                    nrow(flagged), nrow(evaluable) - nrow(flagged),
                    sum(grepl("^tau", flagged$variable))), "")
    if (nrow(flagged))
      add(paste("A parameter flagged as prior-dominated is one the data barely inform, so its",
                "posterior should not be read as an empirical finding. This is expected for tau at",
                "four to ten studies and is the reason the prediction intervals are described as",
                "prior-informed. Full results, including any flag on the coordinate carrying a",
                "reported decision, are in `diagnostics/power_scaling.csv`."), "")
  }

  if (!is.null(R$sbc)) {
    a <- R$sbc_audit
    fail <- R$sbc[R$sbc$p_value < 0.05, ]
    ## How many rank-uniformity tests would fall below 0.05 by chance alone is a
    ## binomial question, not a matter of comparing with the mean. With 66 tests
    ## the expectation is 3.3 but the 95th percentile is 6, so quoting the
    ## expectation alone would overstate a routine excess.
    n_tests <- nrow(R$sbc)
    chance_upper <- stats::qbinom(0.95, n_tests, 0.05)
    n_all_fail <- if ("allranks_p_value" %in% names(R$sbc))
      sum(R$sbc$allranks_p_value < 0.05, na.rm = TRUE) else NA_integer_
    add("", sprintf(paste("**Simulation-based calibration.** Rank-based SBC over %d prespecified",
                          "scenarios. Of %d attempted fits, %d failed outright and %d breached a",
                          "diagnostic threshold, leaving %s per scenario in the primary test. A",
                          "rank from a fit that did not converge is not a draw from the posterior",
                          "the model defines, so including it would confound sampler failure with",
                          "miscalibration; every attempted fit is nonetheless retained in this",
                          "accounting and in the all-ranks sensitivity test below. Of %d scalar",
                          "parameters, %d had a rank-uniformity p value below 0.05. Under exact",
                          "uniformity the expectation is %s and the 95th percentile is %d, so this",
                          "is %s."),
                    length(unique(R$sbc$scenario)), a$total_fits, a$fits_failed,
                    a$fits_diag_failed,
                    if (a$n_datasets_min == a$n_datasets_max) as.character(a$n_datasets_min)
                    else sprintf("%d to %d", a$n_datasets_min, a$n_datasets_max),
                    n_tests, nrow(fail),
                    fmt(0.05 * n_tests, 1), chance_upper,
                    if (nrow(fail) <= chance_upper)
                      "within the range chance alone produces" else
                      "beyond what chance alone would readily produce and merits inspection"), "")
    if (!is.na(n_all_fail))
      add("", sprintf(paste("Repeating the same tests over **all** ranks, including those from the",
                            "%d diagnostically invalid fits, gives %d p values below 0.05 against",
                            "%d in the primary test. %s"),
                      a$fits_diag_failed, n_all_fail, nrow(fail),
                      if (n_all_fail == nrow(fail))
                        "The exclusion therefore changes nothing."
                      else
                        paste("The two disagree, so the exclusion is itself a finding rather than",
                              "a matter of bookkeeping.")), "")
    add(paste("This is a **screening-level** calibration check, not high-powered proof of",
              "calibration, and it is conditional on the plug-in sampling covariance matrices: it",
              "validates the implemented likelihood and sampler given those matrices, not the",
              "uncertainty that arises from estimating them from participant-level data. The p",
              "values are correlated across parameters and across scenarios, so the binomial",
              "figure above is a reference point rather than a formal guarantee. Raw truths,",
              "ranks and per-replicate diagnostics are archived in",
              "`diagnostics/sbc_ranks_raw.csv`, so every rank-uniformity analysis above,",
              "including the primary and all-ranks comparison, can be recomputed from the",
              "released ranks without refitting. The posterior draws each rank was computed",
              "from are not retained, so an individual rank cannot be rebuilt from that file."), "")
  }

  # --- 8. limitations
  add("## 8. Limitations", "")
  add(paste("- **Moderation is ecological.** A percentage-female slope cannot establish that an",
            "association is stronger among women, and a clinical against non-clinical contrast",
            "cannot establish an individual-level mechanism. These are aggregate models."), "")
  add(paste("- **Mediation here is statistical, not causal.** The correlations are largely",
            "cross-sectional. The indirect effect is an observational statistical quantity."), "")
  add(paste("- **The moderator posteriors are substantially prior-determined at this number of",
            "studies.** The posterior contraction statistic is reported for every slope so this is",
            "explicit rather than left to be inferred."), "")
  add(paste("- **Reporting bias was not tested.** Formal selection models and funnel-asymmetry",
            "tests are not fitted, because at five to ten studies they would themselves be poorly",
            "identified. The absence of such a test is not evidence that reporting bias has been",
            "excluded."), "")
  add(paste("- **Two studies remain excluded from the dissociation-to-composite pathway.** Evans",
            "2015 is excluded permanently on current information: its reported values are",
            "regression coefficients with a binary outcome, not the correlation triplet the model",
            "requires. Uyan 2022 is held as a pending source query and could be reinstated if the",
            "trauma-to-dissociation correlation is obtained."), "")
  add(paste("- **The Newcastle-Ottawa scores for the negative-schema pathways were supplied during",
            "this analysis** from the manuscript supplementary material. Every quality-moderator",
            "result in those two pathways depends on them. They are recorded with their instrument,",
            "assessors, consensus procedure and source in `data/provenance/quality_scores.csv`."), "")
  add(paste("- **`bayesianmasem` was not used.** Both of its interfaces fail internally on these",
            "models. That attempt is documented, and is not described as a successful independent",
            "validation. The independent implementation check is supplied by the brms layer."), "")

  writeLines(L, path)
  invisible(path)
}
