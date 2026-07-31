# =============================================================================
# lib_writeup_methods.R
# Generates the methods document from the promoted run and the configuration.
#
# The prior table, the SESOI translation table and the software versions are
# read from config/analysis.yml and run_manifest.csv rather than typed, so the
# methods cannot drift from what was actually fitted.
# =============================================================================

write_methods_doc <- function(R, cfg, lock, path) {
  L <- character(0)
  add <- function(...) L <<- c(L, ...)
  m <- R$manifest[1, ]

  add("# Bayesian meta-regression: methods", "",
      "Prepared by ricardotwumasi. British English throughout. Results are in",
      "`BAYESIAN_METAREG_RESULTS.md`.", "",
      sprintf("Generated from `results/run_%s/`. Version numbers and scale constants are read from the run manifest and `config/analysis.yml`.",
              m$analysis_id), "")

  # --- narrative methods
  add("## Methods text for the manuscript", "")
  add(paste("We replaced the frequentist two-stage meta-analytic structural equation model with a",
            "Bayesian analysis. The corrected frequentist results were retained only as internal",
            "validation targets; they are not reported as co-primary findings, and their p values",
            "were not used to arbitrate disagreements with the posterior."), "")

  ## The caveat sentence is generated, not written, and check_documents() asserts
  ## it verbatim. Editing it here without changing the registry fails the build,
  ## and a change in the evidence rewrites the claim and the check together.
  add(paste("The intended analytical unit was one participant cohort, not one publication,",
            "spreadsheet row or instrument. A cohort contributed at most one triplet of",
            "correlations to any pathway. Multiple eligible instruments administered to the same",
            "participants did not increase the number of studies and were not averaged, because an",
            "average describes no defined construct and conceals the dependence induced by shared",
            "participants. Where a cohort supplied more than one eligible instrument, one was",
            "selected by a prespecified hierarchy favouring commensurability with the other",
            "included cohorts, and the alternative was retained as a replacement sensitivity",
            "analysis."), "")
  add(identity_caveat(lock$samples, lock$sample_map), "")

  add(paste("Each included sample contributed three correlations among childhood trauma (X), the",
            "proposed mediator (M) and the psychosis outcome (Y), in the fixed order r(X,M),",
            "r(X,Y), r(M,Y). Correlations were Fisher-z transformed. Because the three",
            "correlations are calculated in the same participants they are not independent, so",
            "each study's full 3 by 3 within-study sampling covariance matrix on the z scale was",
            "supplied to the model as known, constructed with `metafor::rcalc(rtoz = TRUE)`."), "")

  add(paste("Two model layers were fitted from one data-preparation pipeline, one correlation",
            "ordering and one set of sampling covariance matrices, so that any difference between",
            "them reflects the model rather than the preprocessing."), "")

  add(paste("The **primary layer** was a purpose-written Stan model in which the between-study",
            "hierarchy is placed on the coordinates atanh r(X,M), atanh r(X,Y) and",
            "atanh r(M,Y | X), the last a partial correlation. Recovering"), "")
  add("        r(M,Y) = r(X,M) r(X,Y) + r(M,Y|X) sqrt[(1 - r(X,M)^2)(1 - r(X,Y)^2)]", "")
  add(paste("guarantees that every posterior draw defines a positive-definite correlation matrix,",
            "which a hierarchy placed on the three zero-order correlations does not. From each",
            "draw the standardised mediation model M = aX + e, Y = c'X + bM + e was recovered",
            "exactly, giving a = r(X,M), b = [r(M,Y) - r(X,M) r(X,Y)] / [1 - r(X,M)^2],",
            "c' = [r(X,Y) - r(X,M) r(M,Y)] / [1 - r(X,M)^2], the indirect effect ab and the total",
            "effect c' + ab = r(X,Y). These were computed for the population-average correlation",
            "matrix, for each study, and for a new-study predictive distribution. The proportion",
            "mediated ab / (c' + ab) was not computed: it is unbounded and heavy-tailed as the",
            "total effect approaches zero, so it is not a usable estimand at these study counts."), "")

  add(paste("The **reference layer** was fitted in `brms` and models the three Fisher-z",
            "correlations directly, with the known sampling covariance supplied through `fcor()`",
            "and the residual standard deviation fixed to 1 by a constant prior, and with diagonal",
            "between-study random effects by correlation type. It is the like-for-like comparison",
            "with the frequentist zero-order analysis. It was not treated as the arbiter where the",
            "two layers disagreed."), "")

  add(paste("**Comparing the layers.** The two were compared on a single estimand: the change in",
            "each zero-order Fisher-z coordinate across the reported contrast. The primary model's",
            "own coefficient for the third coordinate acts on a partial correlation and is not the",
            "reference model's r(M,Y) slope, so the two are not interchangeable. The reference",
            "model's per-unit coefficient was rescaled to the contrast within its own posterior,",
            "where draws correspond; the comparison between the two fitted models was then made",
            "between their marginal posteriors, since draws from separate fits have no",
            "correspondence. For the first two coordinates both layers are linear and the",
            "comparison is a check that the models and preprocessing agree; for r(M,Y) the primary",
            "model reaches the marginal value nonlinearly, so agreement there is an",
            "architecture-sensitivity result."), "")

  add(paste("Moderators were fitted one at a time. Continuous moderators were centred at the",
            "unweighted mean of the studies contributing to that model and expressed per unit of",
            "interest. Sample type was coded 0 for non-clinical or general-population and 1 for",
            "clinical. Moderation was reported as the posterior change in the mediation quantities",
            "and in the zero-order coordinates between prespecified moderator values, rather than",
            "by relabelling a correlation slope as a path slope."), "")

  add(paste("Models were fitted in Stan through `cmdstanr` with",
            sprintf("%d chains, %d warm-up and %d sampling iterations per chain, giving %s",
                    cfg$sampling$chains, cfg$sampling$iter_warmup, cfg$sampling$iter_sampling,
                    format(cfg$sampling$chains * cfg$sampling$iter_sampling, big.mark = ",")),
            "post-warm-up draws so that tail summaries of the indirect effects and prediction",
            "intervals were stable. Convergence was assessed with rank-normalised R-hat, bulk and",
            "tail effective sample size, divergent transitions, tree-depth saturation and the",
            "energy diagnostic."), "")

  add(paste("Results are reported as posterior medians and standard deviations, 95 per cent",
            "credible intervals and highest-density intervals, posterior probabilities, and",
            "new-study prediction intervals. No p values are used for inference. Following Dienes",
            "(2020), each moderator conclusion is additionally expressed as the relation between",
            "the posterior and a null interval, with a robustness region giving the range of",
            "minimally interesting effect sizes over which the conclusion is unchanged."), "")

  # --- SESOI table
  add("## Smallest effect size of interest, and its translation", "")
  add(paste(sprintf("The smallest effect size of interest is %s Fisher-z **across the nominal scientific contrast**.",
                    fmt(cfg$decision$sesoi_full, 2)),
            "It anchors the prior as well as the decision rule. Because the fitted coefficient is",
            "per unit, both are divided by the nominal span to reach the coefficient scale, and",
            "reporting scales back out to the contrast the data support:"), "")
  add("```",
      "sesoi_per_unit     = sesoi_full / nominal_span",
      "sesoi_reported     = sesoi_per_unit * supported_span",
      "prior_beta_unit_sd = prior_beta_full_sd / nominal_span",
      "```", "")
  add(paste("A contrast clamped to observed support therefore keeps the same per-unit threshold",
            "and carries a proportionally smaller bound. Re-anchoring the full value onto each",
            "shortened contrast would silently redefine the scientific threshold and make short",
            "contrasts easier to declare interesting."), "")
  sp <- R$support
  sp <- sp[order(sp$moderator, sp$pathway), ]
  add("", md_table(data.frame(
    Pathway = PATHWAY_PROSE[sp$pathway],
    Moderator = sp$moderator,
    Nominal = sprintf("%g to %g", sp$nominal_lo, sp$nominal_hi),
    Reported = sprintf("%g to %g", sp$supported_lo, sp$supported_hi),
    `Span (units)` = fmt(sp$supported_span, 2),
    `Prior SD (per unit)` = fmt(sp$prior_beta_unit_sd, 3),
    `SESOI (per unit)` = fmt(sp$sesoi_per_unit, 3),
    `Bound applied` = fmt(sp$sesoi_reported, 3),
    Decision = ifelse(sp$decision_permitted, "permitted", "not reported"),
    check.names = FALSE, stringsAsFactors = FALSE)), "")

  # --- prior table
  add("## Prior table", "")
  add("All priors are on the Fisher-z scale.", "")
  p <- cfg$priors$primary; g <- cfg$priors$grid
  add(md_table(data.frame(
    Parameter = c("Intercepts, alpha", "Moderator slopes (full contrast)",
                  "Between-study heterogeneity, tau"),
    `Primary prior` = c(sprintf("N(0, %g^2)", p$alpha_sd),
                        sprintf("N(0, %g^2)", p$beta_full_sd),
                        sprintf("half-N(0, %g^2)", p$tau_sd)),
    Sensitivity = c(paste(setdiff(unlist(g$alpha_sd), p$alpha_sd), collapse = ", "),
                    paste(setdiff(unlist(g$beta_full_sd), p$beta_full_sd), collapse = ", "),
                    paste(setdiff(unlist(g$tau_sd), p$tau_sd), collapse = ", ")),
    Justification = c(
      "Weakly informative on the correlation scale: the 95 per cent prior interval spans roughly r = -0.59 to 0.59, covering the published external anchors without asserting them",
      "Regularising, on the same scale as the SESOI, which it places at a quarter of a prior standard deviation. Specifying it on the full-contrast scale prevents a moderator being more or less regularised purely because of the unit its coefficient is expressed in",
      "Follows the scale guidance and worked correlation example in Röver (2021), supported by the empirical compilation of van Erp et al. (2017)"),
    check.names = FALSE, stringsAsFactors = FALSE)), "")
  add(paste("These are scale-informed shrinkage priors, not claims that the effects are null.",
            "Prior predictive simulation was run before any posterior was inspected, and priors",
            "were not selected after comparing which gave the preferred substantive conclusion."), "")
  add("", paste("**Informative location priors were not used.** This is an explicit departure from",
                "the original brief and is documented in `docs/prior_scope.md`: both candidate",
                "source syntheses probably contain studies that also appear in the present",
                "extraction, and the study-by-study overlap audit has not been completed. Using",
                "them without that audit would count the same participants twice. The published",
                "values were used only as prior predictive plausibility anchors."), "")

  # --- sampling covariance
  add("## Sampling covariance construction", "")
  v <- R$vz
  add(paste("`metafor::rcalc(rtoz = TRUE)` was used as primary. On the z scale its diagonal is",
            "1/(n-3), which does not depend on the observed correlation, removing the coupling",
            "between an estimate and its own sampling variance that `asyCov()`'s default",
            "`acov = \"weighted\"` exists to mitigate on the r scale. Because the Bayesian",
            "hierarchy already pools across studies, the weighted construction would pool twice.",
            "It was refitted as a sensitivity analysis so that a like-for-like comparison with the",
            "frequentist scripts remains available."), "")
  add("", md_table(data.frame(
    Pathway = PATHWAY_PROSE[v$pathway],
    `vs asyCov(individual)` = formatC(v$max_abs_diff_individual, format = "e", digits = 2),
    `vs asyCov(weighted)` = formatC(v$max_abs_diff_weighted, format = "e", digits = 2),
    check.names = FALSE, stringsAsFactors = FALSE)), "")
  add(paste("The first difference has a single identifiable cause: applying the delta method to",
            "the r-scale covariance places 1/(n-1) on the z-scale diagonal, whereas the standard",
            "Fisher-z sampling variance is 1/(n-3). It is largest in the smallest studies. The",
            "second is an order of magnitude larger and reflects the weighted option borrowing a",
            "pooled correlation matrix across studies."), "")

  # --- validation
  add("## Validation", "")
  a <- R$sbc_audit
  ## The REQUESTED number of datasets is cfg$sbc$n_datasets; the number reaching
  ## the primary test is smaller wherever a replicate breached a diagnostic
  ## threshold. Printing the request as though it were the outcome is how this
  ## section came to contradict its own results document.
  add(sprintf(paste("**Simulation-based calibration.** Formal rank-based SBC (Talts et al. 2018) over",
                    "%d prespecified scenarios, %d datasets simulated per scenario, exercising the",
                    "intercept-only code path, continuous and binary moderation, the smallest and",
                    "largest number of studies, contrasting sampling-covariance structures and a",
                    "clamped reporting contrast. Truth was drawn from the prior and data from the",
                    "joint model at the real design; the rank of each truth among %d retained",
                    "posterior draws was recorded. Thinning was fixed before any rank distribution",
                    "was inspected and ties were broken at random."),
              length(unique(R$sbc$scenario)), cfg$sbc$n_datasets, cfg$sbc$retained_draws), "")
  add(sprintf(paste("The primary rank-uniformity test uses the %s datasets per scenario whose fit",
                    "met every diagnostic threshold. A rank taken from a fit that did not converge",
                    "is not a draw from the posterior the model defines, so it cannot separate a",
                    "miscalibrated model from a failed sampler. Excluding such fits silently would",
                    "be the opposite error, because the hardest replicates are the likeliest to",
                    "fail, so all %d attempted fits are retained in the audit table with their",
                    "status, and the same tests are repeated over all ranks and reported as",
                    "sensitivity evidence."),
              if (a$n_datasets_min == a$n_datasets_max) as.character(a$n_datasets_min)
              else sprintf("%d to %d", a$n_datasets_min, a$n_datasets_max),
              a$total_fits), "")
  add(paste("This is a screening-level check, not high-powered proof of calibration, and it is",
            "conditional on the plug-in sampling covariance matrices: it validates the implemented",
            "likelihood, the generated quantities and the sampler given those matrices, not the",
            "uncertainty arising from estimating them from participant-level data."), "")
  add("", paste("**Posterior predictive checks** were computed in both forms for every pathway:",
                "conditional on the fitted study effects, and with fresh random effects. Both are",
                "archived because a conditional check alone can look satisfactory simply because",
                "the study-level random effects have absorbed an outlying study. Coordinate-wise",
                "residuals, study-level Mahalanobis discrepancies and per-study posterior",
                "predictive p values are archived alongside."), "")
  add("", paste("**Generated quantities** were verified against an independent R implementation of",
                "the same transforms, and the path coefficients additionally against a route that",
                "solves the standardised regression directly from the correlation matrix and",
                "shares no algebra with them."), "")

  # --- reproducibility
  add("## Software and reproducibility", "")
  add(md_table(data.frame(
    Component = c("R", "CmdStan", "Analysis identifier", "Source commit",
                  "Locked analysis table", "Configuration", "Stan source", "renv lockfile"),
    Value = c(m$r_version, m$cmdstan_version, m$analysis_id,
              substr(m$source_commit, 1, 12), substr(m$data_sha256, 1, 16),
              substr(m$config_sha256, 1, 16), substr(m$stan_sha256, 1, 16),
              substr(m$renv_sha256, 1, 16)),
    stringsAsFactors = FALSE)), "")
  add(paste("The analysis identifier is derived deterministically from the frozen source commit,",
            "the canonical data hash, the validated configuration hash, the Stan source hash and",
            "the `renv.lock` hash, so a run cannot be confused with one produced from different",
            "inputs."), "")
  add("", paste("The whole analysis is one `targets` graph. Outputs are written to a staging area",
                "and promoted atomically only after every gate passes, with the run manifest",
                "written last, so an interrupted attempt cannot resemble a finished release. There",
                "is no second, sequential driver."), "")
  add("", sprintf(paste("The fit plan contained %s model specifications with stable identifiers.",
                        "The release gate requires exact set equality between the expected and",
                        "completed identifiers; the expected count is derived from the plan and is",
                        "not recorded as a constant anywhere."),
                  format(nrow(R$ledger), big.mark = ",")), "")
  add("", paste("No modified source workbook is committed. The only binary inputs are the upstream",
                "extraction workbooks, and every departure from them is a row in",
                "`data/provenance/corrections.csv` or `data/provenance/quality_scores.csv`. The",
                "build verifies the upstream hash, requires each correction to address exactly one",
                "cell holding exactly the value it claims to replace, and verifies the row",
                "fingerprint before writing."), "")

  add("## References", "")
  add("Dienes, Z. (2020). How to use and report Bayesian hypothesis tests. *Psychology of Consciousness: Theory, Research, and Practice*, 8(1), 9-26.", "")
  add("Funder, D. C., and Ozer, D. J. (2019). Evaluating effect size in psychological research. *Advances in Methods and Practices in Psychological Science*, 2(2), 156-168.", "")
  add("Kruschke, J. K. (2018). Rejecting or accepting parameter values in Bayesian estimation. *Advances in Methods and Practices in Psychological Science*, 1(2), 270-280.", "")
  add("Röver, C. (2021). On weakly informative prior distributions for the heterogeneity parameter in Bayesian random-effects meta-analysis. *Research Synthesis Methods*, 12(4), 448-474.", "")
  add("Talts, S., Betancourt, M., Simpson, D., Vehtari, A., and Gelman, A. (2018). Validating Bayesian inference algorithms with simulation-based calibration. arXiv:1804.06788.", "")
  add("van Erp, S., Verhagen, J., Grasman, R. P. P. P., and Wagenmakers, E.-J. (2017). Estimates of between-study heterogeneity for 705 meta-analyses. *Journal of Open Psychology Data*, 5(1), 4.", "")
  add("Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., and Bürkner, P.-C. (2021). Rank-normalisation, folding, and localisation: an improved R-hat. *Bayesian Analysis*, 16(2), 667-718.", "")
  add("", "ricardotwumasi")

  writeLines(L, path)
  invisible(path)
}
