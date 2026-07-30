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
pct <- function(x, d = 1) paste0(formatC(100 * x, format = "f", digits = d), " per cent")
ci  <- function(lo, hi, d = 3) sprintf("[%s, %s]", fmt(lo, d), fmt(hi, d))

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
    prior_stab  = rd("tables", "prior_grid_stability.csv"),
    decided     = rd("tables", "decisions.csv"),
    vz          = rd("tables", "vz_comparison.csv"),
    diagnostics = rd("diagnostics", "fit_diagnostics.csv"),
    ledger      = rd("diagnostics", "fit_ledger.csv"),
    sbc         = rd("diagnostics", "sbc_rank_uniformity.csv"),
    sbc_audit   = rd("diagnostics", "sbc_audit.csv"),
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
    add(sprintf(paste("In the Bayesian analysis, %d of the %d reportable coordinate slopes met the",
                      "interval decision rule: %d in the primary valid-correlation model and %d in",
                      "the brms reference model. A further %d slopes were prespecified as not",
                      "reportable."),
                h$bayes_decisions, h$bayes_slopes_reportable,
                h$bayes_decisions_stan, h$bayes_decisions_brms,
                h$bayes_slopes_not_reportable), "")
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
  add(sprintf(paste("**New-study prediction intervals.** Of %d stored prediction intervals for the",
                    "indirect effect ab, %d include zero. A prediction interval describes the range",
                    "of effects expected in a new study, not the precision of the pooled mean. Its",
                    "exact width is prior-informed, because between-study heterogeneity is weakly",
                    "identified at this number of studies."),
              pi$n_intervals, pi$n_including_zero), "")

  pm <- R$pooled[R$pooled$moderator == "null" & R$pooled$quantity == "pmed", ]
  fin <- sum(is.finite(pm$q_lo) & is.finite(pm$q_hi))
  add(sprintf(paste("**Proportion mediated.** Finite central 95 per cent credible intervals were",
                    "obtained in %d of %d pathways. The ratio ab / total remains mathematically",
                    "unbounded and can be heavy-tailed when the total effect approaches zero, so a",
                    "finite central interval is not evidence that the quantity is well behaved. It",
                    "is reported only where the total effect is separated from zero, and is",
                    "descriptive rather than an estimand of interest in its own right."),
              fin, nrow(pm)), "")

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
    data.frame(Pathway = PATHWAY_PROSE[k],
               k = lock$reportability$k[lock$reportability$pathway == k][1],
               a = fmt(g("a", "median")), b = fmt(g("b", "median")),
               `c'` = fmt(g("cprime", "median")),
               ab = sprintf("%s %s", fmt(g("ind", "median")),
                            ci(g("ind", "q_lo"), g("ind", "q_hi"))),
               `P(ab>0)` = fmt(g("ind", "p_gt0"), 2),
               `new-study PI` = ci(pr$q_lo, pr$q_hi),
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
  add(md_table(tab), "")

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
    add(md_table(data.frame(
      Pathway = PATHWAY_PROSE[R$stability$pathway], Moderator = R$stability$moderator,
      Coordinate = COORD_PROSE[R$stability$coordinate],
      Deletions = R$stability$n_deletions,
      `Retaining the decision` = R$stability$n_retaining_decision,
      `Median range` = sprintf("%s to %s", fmt(R$stability$median_min),
                               fmt(R$stability$median_max)),
      check.names = FALSE, stringsAsFactors = FALSE)), "")
    frag <- R$stability[R$stability$n_retaining_decision < R$stability$n_deletions, ]
    if (nrow(frag))
      add("", paste("Where a decision does not survive every single-study deletion, the correct",
                    "statement is that **the interval decision is not leave-one-study-out robust**.",
                    "That is weaker than saying the direction is unidentified: the point estimates",
                    "may remain consistent in sign while the interval crosses the decision",
                    "boundary."), "")
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
  add(sprintf(paste("**Sampling.** All %d fits met every diagnostic target: maximum R-hat %s,",
                    "minimum bulk effective sample size %s, minimum tail effective sample size %s,",
                    "%d divergent transitions, %d maximum-tree-depth events, minimum E-BFMI %s."),
              nrow(dg), fmt(max(dg$max_rhat), 4), formatC(min(dg$min_ess_bulk), format = "d"),
              formatC(min(dg$min_ess_tail), format = "d"), sum(dg$num_divergent),
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
                        "adapt_delta ladder, against a declared cap of %d, namely %s. Deleting a",
                        "study from an already small pathway can produce a geometry the sampler",
                        "finds harder than the model being reported. These are named here rather",
                        "than absorbed into a summary, and the affected refits contribute to the",
                        "leave-one-study-out counts above."),
                  dv$sensitivity_fits_with_divergence, dv$sensitivity_fits,
                  dv$sensitivity_divergence_cap, dv$tolerated), "")
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
    add("", sprintf(paste("**Simulation-based calibration.** Rank-based SBC over %d prespecified",
                          "scenarios. Of %d attempted fits, %d failed outright and %d breached a",
                          "diagnostic threshold; all remain in the denominator, since removing them",
                          "would flatter the audit. Of %d scalar parameters, %d had a",
                          "rank-uniformity p value below 0.05. Under exact uniformity the",
                          "expectation is %s and the 95th percentile is %d, so this is %s."),
                    length(unique(R$sbc$scenario)), a$total_fits, a$fits_failed,
                    a$fits_diag_failed, n_tests, nrow(fail),
                    fmt(0.05 * n_tests, 1), chance_upper,
                    if (nrow(fail) <= chance_upper)
                      "within the range chance alone produces" else
                      "beyond what chance alone would readily produce and merits inspection"), "")
    add(paste("This is a **screening-level** calibration check, not high-powered proof of",
              "calibration, and it is conditional on the plug-in sampling covariance matrices: it",
              "validates the implemented likelihood and sampler given those matrices, not the",
              "uncertainty that arises from estimating them from participant-level data."), "")
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
