# Bayesian meta-regression: results

Prepared by ricardotwumasi. British English throughout. Methods are in
`BAYESIAN_METAREG_METHODS.md`.

Every number below is generated from the tables in `results/run_0c04f897e2d1/`.
None is transcribed by hand.

## 1. What the analysis found

Sample identity is curated in `data/provenance/sample_map.csv`. All 41 report-to-cohort assertions have been checked against named sources: 40 are evidenced and 1 is disputed. The disputed assertion is excluded from the analysed set. Consequently, no assertion remains unevidenced, and all report-to-cohort mappings used in the analysis are evidenced against named sources.

The matched frequentist meta-regressions produced 12 of 60 moderator slopes at p < 0.05, across 8 of 20 pathway-by-moderator models. These are retained as internal validation targets only. They are not co-primary, and their p values are not used to arbitrate a disagreement with the posterior.

In the Bayesian analysis, 1 of 60 reportable primary coordinate contrasts met the interval-plus-SESOI rule, and 0 of the 60 corresponding reference contrasts did. The two layers analyse the same estimands and are not independent replications, so they are counted separately rather than pooled into a single denominator. A further 0 contrasts per layer were prespecified as not reportable.


| Pathway | Moderator | Coordinate | Layer | Contrast | Median | 95% HDI | Decision |
| --- | --- | --- | --- | --- | ---: | --- | --- |
| Negative schema to delusions | Clinical vs non-clinical sample | r(mediator, psychosis) | primary | clinical minus non-clinical | -0.283 | [-0.447, -0.084] | reject null interval |

A finding that appears in only one layer is labelled as belonging to that layer. The primary model is the valid-correlation model; the brms model is a reference implementation and is not the arbiter when the two disagree.

## 2. Findings that do not depend on the moderator analysis

**Prediction intervals for the latent true study effect.** All 45 intervals for the indirect effect ab in a new study include zero: the 5 from the unmoderated models, and the 40 moderator-endpoint intervals, which are conditional on a moderator value and so belong to the moderator analysis rather than standing apart from it.

These intervals describe the range of LATENT TRUE effects expected across new studies drawn from the estimated population. They do not add the sampling error a future observed estimate would also carry, so a realised estimate would scatter more widely still. Their width is driven entirely by the between-study heterogeneity tau, which is weakly identified at this number of studies and therefore substantially prior-informed; the tau posteriors are reported in `tables/heterogeneity.csv` so that claim can be checked rather than taken on trust.

**Percentage of the pooled association represented by the indirect path.** This quantity is a POST HOC analysis amendment, added at the co-authors' request after the previous release and withheld from it. It is not prespecified. Its estimand, reporting rule and diagnostics were fixed before the refit that produced the values below. It is a descriptive companion to the indirect effect ab and does not replace ab as the evidence for mediation.

The estimand is ab / total, evaluated draw-wise at the population-average parameter values of the unmoderated model, expressed as a percentage. It is not the average of the study-specific ratios: the transformation is nonlinear, so the two differ. The denominator is the pooled zero-order correlation between adversity and psychosis, which has positive posterior density at zero, so the ratio has no finite mean and no finite standard deviation. It is therefore summarised by its posterior median and equal-tailed 95 per cent credible interval, and no mean or standard deviation of it appears anywhere in this release. Rank-normalised R-hat, effective sample size and Monte Carlo error at each reported quantile are given in `diagnostics/proportion_mediated_diagnostics.csv`; the rank-normalised statistic depends only on the ordering of the draws and is insensitive to whether the estimand has moments (Vehtari et al., 2021).

A pathway's percentage is released only if its parent fit passed every sampler diagnostic, the posterior probability that the total association exceeds 0.05 is at least 0.95, and the posterior probability that the percentage lies between 0 and 100 is at least 0.95. The displacement 0.05 is a post hoc sensitivity threshold, not a clinically validated boundary, and 0.025 and 0.100 are carried alongside it in `tables/proportion_mediated_audit.csv`. All 5 pathways met the rule; every pathway is retained in the audit whatever its status.

| Pathway | k | Percentage [95% CrI] | P(total > threshold) | P(0 < percentage < 100) | Status |
| --- | --- | --- | --- | --- | --- |
| Dissociation to hallucinations |   10 | 61.5 [34.6, 106.0] | >0.999 | 0.961 | released |
| Dissociation to delusions |    7 | 50.0 [25.7, 83.1] | >0.999 | 0.995 | released |
| Negative schema to composite psychosis |    7 | 20.8 [0.2, 62.6] | 0.997 | 0.970 | released |
| Negative schema to delusions |    5 | 25.7 [2.2, 97.5] | 0.980 | 0.959 | released |
| Depression to composite psychosis |    5 | 33.4 [14.5, 56.0] | 0.999 | 0.996 | released |

Each pathway was refitted under a robust Student-t likelihood, a weighted sampling-covariance construction, an alternative instrument where one exists, a prior grid on the intercept and heterogeneity scales, every single-cohort deletion and, where it was estimable, a restriction to triplets taken directly from a correlation matrix: 66 refits in total. The largest shift in any pathway's median across all of them was 7.8 percentage points. The released-or-withheld classification was unchanged in 17 of 22 pathway-by-class comparisons, the exceptions being Dissociation to hallucinations under cohort deletion, Negative schema to composite psychosis under cohort deletion, Negative schema to delusions under cohort deletion, Negative schema to delusions under the prior grid, Negative schema to delusions under the weighted sampling-covariance construction. Per-class detail is in `tables/proportion_mediated_stability.csv`.

Pathways were synthesised only where at least 5 cohorts contributed, counting each distinct sample identifier once, in line with the minimum of five studies specified in the PROSPERO pre-registration. 5 of 6 extracted pathways met this; Dissociation to composite psychosis (k = 4) was extracted and locked but not synthesised.

As the protocol specifies, effect sizes obtained by conversion rather than read from a correlation matrix were examined in a sensitivity analysis restricted to direct correlations. It was fitted for Dissociation to delusions (6 of 7 cohorts direct); Dissociation to hallucinations (7 of 10 cohorts direct); it was identical to the primary model for Depression to composite psychosis (5 of 5 cohorts direct); and it was not estimable, because fewer than the minimum number of cohorts would remain, for Negative schema to composite psychosis (2 of 7 cohorts direct); Negative schema to delusions (2 of 5 cohorts direct). Values are in `tables/direct_correlation_sensitivity.csv`.

No prediction interval is defined for this percentage. A ratio computed in a new study would be especially unstable, because the predictive distribution of the denominator is far more diffuse than its posterior. The credible interval above belongs to the population-average percentage; the prediction intervals reported in this section belong to the latent true indirect effect ab in a new study. They are different quantities and are not comparable.

This is a model-based decomposition of pooled, largely cross-sectional associations. It is not an individual-level causal percentage and it is not the expected effect of intervening on the mediator. Percentages mediated reported in the descriptive systematic review are a separate summary of the individual published analyses and are not estimates from these models. See `docs/proportion_mediated.md`.

## 3. Pooled mediation quantities

Population-average quantities from the primary model, unmoderated. The total effect is c' + ab and equals r(trauma, psychosis) exactly by construction.

| Pathway | k | a | b | c' | ab | P(ab>0) | max tau | new-study PI |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Dissociation to hallucinations |   10 | 0.357 | 0.498 | 0.110 | 0.175 [0.105, 0.254] | 1.00 | 0.289 | [-0.043, 0.427] |
| Dissociation to delusions |    7 | 0.415 | 0.414 | 0.170 | 0.170 [0.089, 0.255] | 1.00 | 0.196 | [-0.035, 0.392] |
| Negative schema to composite psychosis |    7 | 0.338 | 0.169 | 0.204 | 0.053 [0.001, 0.130] | 0.98 | 0.292 | [-0.139, 0.339] |
| Negative schema to delusions |    5 | 0.263 | 0.256 | 0.185 | 0.065 [0.011, 0.130] | 0.99 | 0.179 | [-0.056, 0.230] |
| Depression to composite psychosis |    5 | 0.297 | 0.355 | 0.210 | 0.105 [0.046, 0.161] | 1.00 | 0.198 | [-0.042, 0.223] |

`ab` is reported with its equal-tailed 95 per cent credible interval. Equal-tailed and highest-density intervals do not always agree on whether zero is excluded, so the convention is named rather than left implicit; the HDI is given for every quantity in `tables/pooled_quantities.csv`. `max tau` is the largest of the three coordinate heterogeneity posterior medians, given here because the prediction intervals are driven entirely by tau; the full posteriors are in `tables/heterogeneity.csv`.

## 4. Moderator results

Moderation is reported as the posterior change in each zero-order Fisher-z coordinate across the contrast the data support. The smallest effect size of interest is 0.05 Fisher-z across the nominal scientific contrast, held at a constant per-unit threshold, so a contrast clamped to observed support carries a proportionally smaller bound rather than a more permissive one.


Two contrasts are clamped:

| Pathway | Moderator | Nominal | Estimated | Span (units) | Null-interval bound |
| --- | --- | --- | --- | --- | --- |
| Depression to composite psychosis | female | 40 to 60 | 42.4 to 60 | 1.76 | 0.044 |

These are reported with the contrast actually estimated. The nominal label is not reused for a shorter comparison.


Decision counts across the 60 reportable slopes in each layer:

| Layer | Reject | Accept | Undecided |
| --- | --- | --- | --- |
| Primary (valid-correlation) |    1 |    0 |   59 |
| Reference (brms) |    0 |    0 |   60 |

## 5. Robustness of any decision reached

Leave-one-study-out, for every slope that reached a decision:

| Pathway | Moderator | Coordinate | Deletions | Reportable | Retaining the decision | Median range (reportable) |
| --- | --- | --- | --- | --- | --- | --- |
| Negative schema to delusions | clinical | r(mediator, psychosis) |    5 |    3 |    0 | -0.277 to -0.254 |


2 deletion(s) left a group below the minimum per arm and are therefore **not reportable**. They are excluded from the counts and the median range above, and named in `tables/leave_one_study_out_detail.csv`. Counting them would treat an estimate the project's own rule forbids reporting as evidence about the decision's robustness.


Where a decision does not survive every reportable single-study deletion, the correct statement is that **the interval decision is not leave-one-study-out robust**. That is weaker than saying the direction is unidentified: the point estimates may remain consistent in sign while the interval crosses the decision boundary.


Because a categorical decision can flip when an interval endpoint moves by less than a thousandth, `tables/targeted_sensitivity.csv` gives every one of the 20 fits of the contrast(s) that reached a decision: both layers, every prior variant, both sampling-covariance constructions, the robust likelihood and every deletion, each with its own reportability and its interval's distance from the threshold. A variant that changed label without meaningfully changing the estimate is visible there as such.


Prior sensitivity, for the same slopes:

| Pathway | Moderator | Coordinate | Variants | Retaining the decision |
| --- | --- | --- | --- | --- |
| Negative schema to delusions | clinical | r(mediator, psychosis) |    6 |    4 |

## 6. Agreement between the two model layers

The layers are compared on one estimand: the change in each zero-order Fisher-z coordinate across the reported contrast. The primary model's own coefficient for the third coordinate acts on a **partial** correlation and is not the reference model's r(mediator, psychosis) slope, so comparing those two directly would compare different quantities.


The two models are fitted separately, so their draws do not correspond. The comparison is therefore between marginal posteriors: no draw of one model is paired with a draw of the other.


Across 40 matched linear coordinates the median difference is 0.110 posterior standard deviations at worst, with density overlap of at least 0.94. Across 20 comparisons on r(mediator, psychosis) the worst median difference is 0.477 standard deviations.

Agreement on the third coordinate is an **architecture-sensitivity** result, not a check that preprocessing is identical: the reference model reaches the marginal Fisher-z linearly, the primary model reaches it nonlinearly through the valid-correlation map.

## 7. Model checking

**Sampling.** Across all 551 fits: maximum R-hat 1.0044, minimum bulk effective sample size 1605, minimum tail effective sample size 2033, 1 divergent transition, 0 maximum-tree-depth events, minimum E-BFMI 0.50. Every fit met the convergence targets; the divergence position is set out below.

160 of 551 fits required an adapt_delta escalation, involving 176 escalation steps in total. Escalation is recorded per fit in the fit ledger rather than summarised away.


Divergent transitions: 1 in total across all 551 fits. Every one of the 55 primary fits, whose numbers are reported, had none; no tolerance applies to those.

1 of the 496 sensitivity refits retained a divergence after exhausting the adapt_delta ladder, against a cap of 4, namely loso_brms|depress_cps|female|S029 (1). Deleting a study from an already small pathway can produce a geometry the sampler finds harder than the model being reported. They are named here rather than absorbed into a summary, so a reader can check whether any of them underlies a reported robustness count.

**The tolerance was adopted after a run exposed the issue, not prespecified for this analysis.** It is stated as a rule for subsequent runs, and the cap was fixed before the count was inspected, but describing it as prespecified here would be false. Every threshold other than this one applied to every fit without exception.


**Posterior predictive checks.** Both replication forms were computed for all 5 pathways. Conditional coverage of the 95 per cent replicated intervals averaged 100.0 per cent and new-study coverage 100.0 per cent. 0 studies had a posterior predictive p value outside [0.05, 0.95].

Both forms are reported because a conditional check alone can look satisfactory simply because the study-level random effects have absorbed an outlying study.

These checks detected **no gross conflict between the model and the data**. That is not a demonstration of predictive calibration, and the checks should not be described as having been passed. Complete in-sample coverage is a weak result: with four to ten studies the intervals are wide, so coverage near one is close to guaranteed and carries little information. What was examined is the joint discrepancy of each study's three coordinates under both replication forms, plus coordinate-wise residuals. What these checks would NOT detect includes a misspecified between-study distribution shape, dependence between studies sharing participants, and any error in the plug-in sampling covariance matrices, which are treated as known throughout.


**Prior predictive check.** 55 prespecified plausibility criteria across 5 pathways, all of which were met. The thresholds are recorded in `config/analysis.yml` and were committed before the run that tested them; the configuration hash forms part of the analysis identifier, so that ordering is verifiable rather than asserted. The criteria require the priors to COVER the published correlations tabulated in `bayesian/docs/prior_scope.md`, not to encode them: the priors remain weakly informative and are not literature-derived.


**Prior and likelihood power scaling.** Sensitivity was archived for 290 quantities across the primary fits, of which 275 were evaluable; 15 rows were structurally inapplicable, being the moderator contrast in an intercept-only model, which has no contrast to span. Of the evaluable quantities 113 were flagged and 162 were not, and 71 of the flagged were heterogeneity parameters. This asks a different question from the prior grid: the grid refits at other prior widths and asks whether the conclusion moves, whereas power scaling asks which of the prior and the likelihood is driving each posterior.

A parameter flagged as prior-dominated is one the data barely inform, so its posterior should not be read as an empirical finding. This is expected for tau at four to ten studies and is the reason the prediction intervals are described as prior-informed. Full results, including any flag on the coordinate carrying a reported decision, are in `diagnostics/power_scaling.csv`.


**Simulation-based calibration.** Rank-based SBC over 6 prespecified scenarios. Of 600 attempted fits, 0 failed outright and 6 breached a diagnostic threshold, leaving 96 to 100 per scenario in the primary test. A rank from a fit that did not converge is not a draw from the posterior the model defines, so including it would confound sampler failure with miscalibration; every attempted fit is nonetheless retained in this accounting and in the all-ranks sensitivity test below. Of 66 scalar parameters, 2 had a rank-uniformity p value below 0.05. Under exact uniformity the expectation is 3.3 and the 95th percentile is 6, so this is within the range chance alone produces.


Repeating the same tests over **all** ranks, including those from the 6 diagnostically invalid fits, gives 2 p values below 0.05 against 2 in the primary test. The exclusion therefore changes nothing.

This is a **screening-level** calibration check, not high-powered proof of calibration, and it is conditional on the plug-in sampling covariance matrices: it validates the implemented likelihood and sampler given those matrices, not the uncertainty that arises from estimating them from participant-level data. The p values are correlated across parameters and across scenarios, so the binomial figure above is a reference point rather than a formal guarantee. Raw truths, ranks and per-replicate diagnostics are archived in `diagnostics/sbc_ranks_raw.csv`, so every rank-uniformity analysis above, including the primary and all-ranks comparison, can be recomputed from the released ranks without refitting. The posterior draws each rank was computed from are not retained, so an individual rank cannot be rebuilt from that file.

## 8. Limitations

- **Moderation is ecological.** A percentage-female slope cannot establish that an association is stronger among women, and a clinical against non-clinical contrast cannot establish an individual-level mechanism. These are aggregate models.

- **Mediation here is statistical, not causal.** The correlations are largely cross-sectional. The indirect effect is an observational statistical quantity.

- **The moderator posteriors are substantially prior-determined at this number of studies.** The posterior contraction statistic is reported for every slope so this is explicit rather than left to be inferred.

- **Reporting bias was not tested.** Formal selection models and funnel-asymmetry tests are not fitted, because at five to ten studies they would themselves be poorly identified. The absence of such a test is not evidence that reporting bias has been excluded.

- **Two studies remain excluded from the dissociation-to-composite pathway.** Evans 2015 is excluded permanently on current information: its reported values are regression coefficients with a binary outcome, not the correlation triplet the model requires. Uyan 2022 is held as a pending source query and could be reinstated if the trauma-to-dissociation correlation is obtained.

- **The Newcastle-Ottawa scores for the negative-schema pathways were supplied during this analysis** from the manuscript supplementary material. Every quality-moderator result in those two pathways depends on them. They are recorded with their instrument, assessors, consensus procedure and source in `data/provenance/quality_scores.csv`.

- **`bayesianmasem` was not used.** Both of its interfaces fail internally on these models. That attempt is documented, and is not described as a successful independent validation. The independent implementation check is supplied by the brms layer.

