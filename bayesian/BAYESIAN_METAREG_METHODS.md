# Bayesian meta-regression: methods

Prepared by ricardotwumasi. British English throughout. Results are in
`BAYESIAN_METAREG_RESULTS.md`.

Generated from `results/run_c9cb1a366dfb/`. Version numbers and scale constants are read from the run manifest and `config/analysis.yml`.

## Methods text for the manuscript

We replaced the frequentist two-stage meta-analytic structural equation model with a Bayesian analysis. The corrected frequentist results were retained only as internal validation targets; they are not reported as co-primary findings, and their p values were not used to arbitrate disagreements with the posterior.

The intended analytical unit was one participant cohort, not one publication, spreadsheet row or instrument, and for most included reports that identity is unevidenced against the source papers. A cohort contributed at most one triplet of correlations to any pathway. Multiple eligible instruments administered to the same participants did not increase the number of studies and were not averaged, because an average describes no defined construct and conceals the dependence induced by shared participants. Where a cohort supplied more than one eligible instrument, one was selected by a prespecified hierarchy favouring commensurability with the other included cohorts, and the alternative was retained as a replacement sensitivity analysis.

Sample identity is curated in `data/provenance/sample_map.csv`. 9 of 41 report-to-cohort assertions have been evidenced against a named source and 1 is disputed; the remaining 31 rest on the author and year printed in the extraction workbook and have not been checked against the papers. The analytical unit is therefore derived and has not been externally verified.

Each included sample contributed three correlations among childhood trauma (X), the proposed mediator (M) and the psychosis outcome (Y), in the fixed order r(X,M), r(X,Y), r(M,Y). Correlations were Fisher-z transformed. Because the three correlations are calculated in the same participants they are not independent, so each study's full 3 by 3 within-study sampling covariance matrix on the z scale was supplied to the model as known, constructed with `metafor::rcalc(rtoz = TRUE)`.

Two model layers were fitted from one data-preparation pipeline, one correlation ordering and one set of sampling covariance matrices, so that any difference between them reflects the model rather than the preprocessing.

The **primary layer** was a purpose-written Stan model in which the between-study hierarchy is placed on the coordinates atanh r(X,M), atanh r(X,Y) and atanh r(M,Y | X), the last a partial correlation. Recovering

        r(M,Y) = r(X,M) r(X,Y) + r(M,Y|X) sqrt[(1 - r(X,M)^2)(1 - r(X,Y)^2)]

guarantees that every posterior draw defines a positive-definite correlation matrix, which a hierarchy placed on the three zero-order correlations does not. From each draw the standardised mediation model M = aX + e, Y = c'X + bM + e was recovered exactly, giving a = r(X,M), b = [r(M,Y) - r(X,M) r(X,Y)] / [1 - r(X,M)^2], c' = [r(X,Y) - r(X,M) r(M,Y)] / [1 - r(X,M)^2], the indirect effect ab and the total effect c' + ab = r(X,Y). These were computed for the population-average correlation matrix, for each study, and for a new-study predictive distribution. The proportion mediated ab / (c' + ab) was not computed: it is unbounded and heavy-tailed as the total effect approaches zero, so it is not a usable estimand at these study counts.

The **reference layer** was fitted in `brms` and models the three Fisher-z correlations directly, with the known sampling covariance supplied through `fcor()` and the residual standard deviation fixed to 1 by a constant prior, and with diagonal between-study random effects by correlation type. It is the like-for-like comparison with the frequentist zero-order analysis. It was not treated as the arbiter where the two layers disagreed.

**Comparing the layers.** The two were compared on a single estimand: the change in each zero-order Fisher-z coordinate across the reported contrast. The primary model's own coefficient for the third coordinate acts on a partial correlation and is not the reference model's r(M,Y) slope, so the two are not interchangeable. The reference model's per-unit coefficient was rescaled to the contrast within its own posterior, where draws correspond; the comparison between the two fitted models was then made between their marginal posteriors, since draws from separate fits have no correspondence. For the first two coordinates both layers are linear and the comparison is a check that the models and preprocessing agree; for r(M,Y) the primary model reaches the marginal value nonlinearly, so agreement there is an architecture-sensitivity result.

Moderators were fitted one at a time. Continuous moderators were centred at the unweighted mean of the studies contributing to that model and expressed per unit of interest. Sample type was coded 0 for non-clinical or general-population and 1 for clinical. Moderation was reported as the posterior change in the mediation quantities and in the zero-order coordinates between prespecified moderator values, rather than by relabelling a correlation slope as a path slope.

Models were fitted in Stan through `cmdstanr` with 4 chains, 2000 warm-up and 3000 sampling iterations per chain, giving 12,000 post-warm-up draws so that tail summaries of the indirect effects and prediction intervals were stable. Convergence was assessed with rank-normalised R-hat, bulk and tail effective sample size, divergent transitions, tree-depth saturation and the energy diagnostic.

Results are reported as posterior medians and standard deviations, 95 per cent credible intervals and highest-density intervals, posterior probabilities, and new-study prediction intervals. No p values are used for inference. Following Dienes (2020), each moderator conclusion is additionally expressed as the relation between the posterior and a null interval, with a robustness region giving the range of minimally interesting effect sizes over which the conclusion is unchanged.

## Smallest effect size of interest, and its translation

The smallest effect size of interest is 0.05 Fisher-z **across the nominal scientific contrast**. It anchors the prior as well as the decision rule. Because the fitted coefficient is per unit, both are divided by the nominal span to reach the coefficient scale, and reporting scales back out to the contrast the data support:

```
sesoi_per_unit     = sesoi_full / nominal_span
sesoi_reported     = sesoi_per_unit * supported_span
prior_beta_unit_sd = prior_beta_full_sd / nominal_span
```

A contrast clamped to observed support therefore keeps the same per-unit threshold and carries a proportionally smaller bound. Re-anchoring the full value onto each shortened contrast would silently redefine the scientific threshold and make short contrasts easier to declare interesting.


| Pathway | Moderator | Nominal | Reported | Span (units) | Prior SD (per unit) | SESOI (per unit) | Bound applied | Decision |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Depression to composite psychosis | age | 20 to 30 | 20 to 30 | 1.00 | 0.200 | 0.050 | 0.050 | permitted |
| Dissociation to composite psychosis | age | 20 to 30 | 20 to 30 | 1.00 | 0.200 | 0.050 | 0.050 | permitted |
| Dissociation to delusions | age | 20 to 30 | 20 to 30 | 1.00 | 0.200 | 0.050 | 0.050 | permitted |
| Dissociation to hallucinations | age | 20 to 30 | 20 to 30 | 1.00 | 0.200 | 0.050 | 0.050 | permitted |
| Negative schema to composite psychosis | age | 20 to 30 | 20 to 30 | 1.00 | 0.200 | 0.050 | 0.050 | permitted |
| Negative schema to delusions | age | 20 to 30 | 20 to 30 | 1.00 | 0.200 | 0.050 | 0.050 | permitted |
| Depression to composite psychosis | clinical | 0 to 1 | 0 to 1 | 1.00 | 0.200 | 0.050 | 0.050 | permitted |
| Dissociation to composite psychosis | clinical | 0 to 1 | 0 to 1 | 1.00 | 0.200 | 0.050 | 0.050 | not reported |
| Dissociation to delusions | clinical | 0 to 1 | 0 to 1 | 1.00 | 0.200 | 0.050 | 0.050 | permitted |
| Dissociation to hallucinations | clinical | 0 to 1 | 0 to 1 | 1.00 | 0.200 | 0.050 | 0.050 | permitted |
| Negative schema to composite psychosis | clinical | 0 to 1 | 0 to 1 | 1.00 | 0.200 | 0.050 | 0.050 | permitted |
| Negative schema to delusions | clinical | 0 to 1 | 0 to 1 | 1.00 | 0.200 | 0.050 | 0.050 | permitted |
| Depression to composite psychosis | female | 40 to 60 | 42.4 to 60 | 1.76 | 0.100 | 0.025 | 0.044 | permitted |
| Dissociation to composite psychosis | female | 40 to 60 | 52 to 60 | 0.80 | 0.100 | 0.025 | 0.020 | permitted |
| Dissociation to delusions | female | 40 to 60 | 40 to 60 | 2.00 | 0.100 | 0.025 | 0.050 | permitted |
| Dissociation to hallucinations | female | 40 to 60 | 40 to 60 | 2.00 | 0.100 | 0.025 | 0.050 | permitted |
| Negative schema to composite psychosis | female | 40 to 60 | 40 to 60 | 2.00 | 0.100 | 0.025 | 0.050 | permitted |
| Negative schema to delusions | female | 40 to 60 | 40 to 60 | 2.00 | 0.100 | 0.025 | 0.050 | permitted |
| Depression to composite psychosis | nos | 5 to 6 | 5 to 6 | 1.00 | 0.200 | 0.050 | 0.050 | permitted |
| Dissociation to composite psychosis | nos | 5 to 6 | 5 to 6 | 1.00 | 0.200 | 0.050 | 0.050 | permitted |
| Dissociation to delusions | nos | 5 to 6 | 5 to 6 | 1.00 | 0.200 | 0.050 | 0.050 | permitted |
| Dissociation to hallucinations | nos | 5 to 6 | 5 to 6 | 1.00 | 0.200 | 0.050 | 0.050 | permitted |
| Negative schema to composite psychosis | nos | 5 to 6 | 5 to 6 | 1.00 | 0.200 | 0.050 | 0.050 | permitted |
| Negative schema to delusions | nos | 5 to 6 | 5 to 6 | 1.00 | 0.200 | 0.050 | 0.050 | permitted |

## Prior table

All priors are on the Fisher-z scale.

| Parameter | Primary prior | Sensitivity | Justification |
| --- | --- | --- | --- |
| Intercepts, alpha | N(0, 0.35^2) | 0.2, 0.5 | Weakly informative on the correlation scale: the 95 per cent prior interval spans roughly r = -0.59 to 0.59, covering the published external anchors without asserting them |
| Moderator slopes (full contrast) | N(0, 0.2^2) | 0.1, 0.35 | Regularising, on the same scale as the SESOI, which it places at a quarter of a prior standard deviation. Specifying it on the full-contrast scale prevents a moderator being more or less regularised purely because of the unit its coefficient is expressed in |
| Between-study heterogeneity, tau | half-N(0, 0.2^2) | 0.1, 0.3 | Follows the scale guidance and worked correlation example in Röver (2021), supported by the empirical compilation of van Erp et al. (2017) |

These are scale-informed shrinkage priors, not claims that the effects are null. Prior predictive simulation was run before any posterior was inspected, and priors were not selected after comparing which gave the preferred substantive conclusion.


**Informative location priors were not used.** This is an explicit departure from the original brief and is documented in `docs/prior_scope.md`: both candidate source syntheses probably contain studies that also appear in the present extraction, and the study-by-study overlap audit has not been completed. Using them without that audit would count the same participants twice. The published values were used only as prior predictive plausibility anchors.

## Sampling covariance construction

`metafor::rcalc(rtoz = TRUE)` was used as primary. On the z scale its diagonal is 1/(n-3), which does not depend on the observed correlation, removing the coupling between an estimate and its own sampling variance that `asyCov()`'s default `acov = "weighted"` exists to mitigate on the r scale. Because the Bayesian hierarchy already pools across studies, the weighted construction would pool twice. It was refitted as a sensitivity analysis so that a like-for-like comparison with the frequentist scripts remains available.


| Pathway | vs asyCov(individual) | vs asyCov(weighted) |
| --- | --- | --- |
| Dissociation to hallucinations | 1.74e-03 | 6.98e-02 |
| Dissociation to delusions | 1.74e-03 | 4.82e-02 |
| Dissociation to composite psychosis | 1.74e-03 | 9.97e-03 |
| Negative schema to composite psychosis | 5.13e-04 | 6.36e-03 |
| Negative schema to delusions | 6.79e-04 | 3.54e-03 |
| Depression to composite psychosis | 9.08e-04 | 2.28e-02 |

The first difference has a single identifiable cause: applying the delta method to the r-scale covariance places 1/(n-1) on the z-scale diagonal, whereas the standard Fisher-z sampling variance is 1/(n-3). It is largest in the smallest studies. The second is an order of magnitude larger and reflects the weighted option borrowing a pooled correlation matrix across studies.

## Validation

**Simulation-based calibration.** Formal rank-based SBC (Talts et al. 2018) over 6 prespecified scenarios, 100 datasets simulated per scenario, exercising the intercept-only code path, continuous and binary moderation, the smallest and largest number of studies, contrasting sampling-covariance structures and a clamped reporting contrast. Truth was drawn from the prior and data from the joint model at the real design; the rank of each truth among 500 retained posterior draws was recorded. Thinning was fixed before any rank distribution was inspected and ties were broken at random.

The primary rank-uniformity test uses the 97 to 100 datasets per scenario whose fit met every diagnostic threshold. A rank taken from a fit that did not converge is not a draw from the posterior the model defines, so it cannot separate a miscalibrated model from a failed sampler. Excluding such fits silently would be the opposite error, because the hardest replicates are the likeliest to fail, so all 600 attempted fits are retained in the audit table with their status, and the same tests are repeated over all ranks and reported as sensitivity evidence.

This is a screening-level check, not high-powered proof of calibration, and it is conditional on the plug-in sampling covariance matrices: it validates the implemented likelihood, the generated quantities and the sampler given those matrices, not the uncertainty arising from estimating them from participant-level data.


**Posterior predictive checks** were computed in both forms for every pathway: conditional on the fitted study effects, and with fresh random effects. Both are archived because a conditional check alone can look satisfactory simply because the study-level random effects have absorbed an outlying study. Coordinate-wise residuals, study-level Mahalanobis discrepancies and per-study posterior predictive p values are archived alongside.


**Generated quantities** were verified against an independent R implementation of the same transforms, and the path coefficients additionally against a route that solves the standardised regression directly from the correlation matrix and shares no algebra with them.

## Software and reproducibility

| Component | Value |
| --- | --- |
| R | R version 4.4.2 (2024-10-31) |
| CmdStan | 2.36.0 |
| Analysis identifier | c9cb1a366dfb |
| Source commit | 29f78bb9d8d2 |
| Locked analysis table | 6aa4705709eb0db8 |
| Configuration | 443ca6cbd2f7e8cf |
| Stan source | 748a11efafb07854 |
| renv lockfile | 4e0bec03ba9bef9e |

The analysis identifier is derived deterministically from the frozen source commit, the canonical data hash, the validated configuration hash, the Stan source hash and the `renv.lock` hash, so a run cannot be confused with one produced from different inputs.


The whole analysis is one `targets` graph. Outputs are written to a staging area and promoted atomically only after every gate passes, with the run manifest written last, so an interrupted attempt cannot resemble a finished release. There is no second, sequential driver.


The fit plan contained 575 model specifications with stable identifiers. The release gate requires exact set equality between the expected and completed identifiers; the expected count is derived from the plan and is not recorded as a constant anywhere.


No modified source workbook is committed. The only binary inputs are the upstream extraction workbooks, and every departure from them is a row in `data/provenance/corrections.csv` or `data/provenance/quality_scores.csv`. The build verifies the upstream hash, requires each correction to address exactly one cell holding exactly the value it claims to replace, and verifies the row fingerprint before writing.

## References

Dienes, Z. (2020). How to use and report Bayesian hypothesis tests. *Psychology of Consciousness: Theory, Research, and Practice*, 8(1), 9-26.

Funder, D. C., and Ozer, D. J. (2019). Evaluating effect size in psychological research. *Advances in Methods and Practices in Psychological Science*, 2(2), 156-168.

Kruschke, J. K. (2018). Rejecting or accepting parameter values in Bayesian estimation. *Advances in Methods and Practices in Psychological Science*, 1(2), 270-280.

Röver, C. (2021). On weakly informative prior distributions for the heterogeneity parameter in Bayesian random-effects meta-analysis. *Research Synthesis Methods*, 12(4), 448-474.

Talts, S., Betancourt, M., Simpson, D., Vehtari, A., and Gelman, A. (2018). Validating Bayesian inference algorithms with simulation-based calibration. arXiv:1804.06788.

van Erp, S., Verhagen, J., Grasman, R. P. P. P., and Wagenmakers, E.-J. (2017). Estimates of between-study heterogeneity for 705 meta-analyses. *Journal of Open Psychology Data*, 5(1), 4.

Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., and Bürkner, P.-C. (2021). Rank-normalisation, folding, and localisation: an improved R-hat. *Bayesian Analysis*, 16(2), 667-718.


ricardotwumasi
