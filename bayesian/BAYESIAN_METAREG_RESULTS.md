# Bayesian meta-regression: results

Prepared by ricardotwumasi. British English throughout. Methods are in
`BAYESIAN_METAREG_METHODS.md`.

Every number below is generated from the tables in `results/run_aed067c558c9/`.
None is transcribed by hand.

## 1. What the analysis found

The matched frequentist meta-regressions produced 16 of 72 moderator slopes at p < 0.05, across 12 of 24 pathway-by-moderator models. These are retained as internal validation targets only. They are not co-primary, and their p values are not used to arbitrate a disagreement with the posterior.

In the Bayesian analysis, 1 of 69 reportable primary coordinate contrasts met the interval-plus-SESOI rule, and 0 of the 69 corresponding reference contrasts did. The two layers analyse the same estimands and are not independent replications, so they are counted separately rather than pooled into a single denominator. A further 3 contrasts per layer were prespecified as not reportable.


| Pathway | Moderator | Coordinate | Layer | Contrast | Median | 95% HDI | Decision |
| --- | --- | --- | --- | --- | ---: | --- | --- |
| Negative schema to delusions | Clinical vs non-clinical sample | r(mediator, psychosis) | primary | clinical minus non-clinical | -0.283 | [-0.447, -0.084] | reject null interval |

A finding that appears in only one layer is labelled as belonging to that layer. The primary model is the valid-correlation model; the brms model is a reference implementation and is not the arbiter when the two disagree.

## 2. Findings that do not depend on the moderator analysis

**Prediction intervals for the latent true study effect.** All 54 intervals for the indirect effect ab in a new study include zero: the 6 from the unmoderated models, and the 48 moderator-endpoint intervals, which are conditional on a moderator value and so belong to the moderator analysis rather than standing apart from it.

These intervals describe the range of LATENT TRUE effects expected across new studies drawn from the estimated population. They do not add the sampling error a future observed estimate would also carry, so a realised estimate would scatter more widely still. Their width is driven entirely by the between-study heterogeneity tau, which is weakly identified at this number of studies and therefore substantially prior-informed; the tau posteriors are reported in `tables/heterogeneity.csv` so that claim can be checked rather than taken on trust.

**Proportion mediated.** Finite central 95 per cent credible intervals were obtained in 6 of 6 pathways. The ratio ab / total remains mathematically unbounded and can be heavy-tailed when the total effect approaches zero, so a finite central interval is not evidence that the quantity is well behaved. It is reported only where the total effect is separated from zero, and is descriptive rather than an estimand of interest in its own right.

## 3. Pooled mediation quantities

Population-average quantities from the primary model, unmoderated. The total effect is c' + ab and equals r(trauma, psychosis) exactly by construction.

| Pathway | k | a | b | c' | ab | P(ab>0) | max tau | new-study PI |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Dissociation to hallucinations |   10 | 0.360 | 0.451 | 0.127 | 0.160 [0.083, 0.239] | 1.00 | 0.313 | [-0.085, 0.418] |
| Dissociation to delusions |    7 | 0.413 | 0.398 | 0.203 | 0.162 [0.077, 0.248] | 1.00 | 0.230 | [-0.062, 0.402] |
| Dissociation to composite psychosis |    4 | 0.273 | 0.348 | 0.200 | 0.089 [0.006, 0.179] | 0.98 | 0.216 | [-0.100, 0.302] |
| Negative schema to composite psychosis |    7 | 0.338 | 0.169 | 0.204 | 0.053 [0.001, 0.130] | 0.98 | 0.292 | [-0.139, 0.339] |
| Negative schema to delusions |    5 | 0.263 | 0.256 | 0.185 | 0.065 [0.011, 0.130] | 0.99 | 0.179 | [-0.056, 0.230] |
| Depression to composite psychosis |    5 | 0.297 | 0.355 | 0.210 | 0.105 [0.046, 0.161] | 1.00 | 0.198 | [-0.042, 0.223] |

`ab` is reported with its equal-tailed 95 per cent credible interval. Equal-tailed and highest-density intervals do not always agree on whether zero is excluded, so the convention is named rather than left implicit; the HDI is given for every quantity in `tables/pooled_quantities.csv`. `max tau` is the largest of the three coordinate heterogeneity posterior medians, given here because the prediction intervals are driven entirely by tau; the full posteriors are in `tables/heterogeneity.csv`.

## 4. Moderator results

Moderation is reported as the posterior change in each zero-order Fisher-z coordinate across the contrast the data support. The smallest effect size of interest is 0.05 Fisher-z across the nominal scientific contrast, held at a constant per-unit threshold, so a contrast clamped to observed support carries a proportionally smaller bound rather than a more permissive one.


Two contrasts are clamped:

| Pathway | Moderator | Nominal | Estimated | Span (units) | Null-interval bound |
| --- | --- | --- | --- | --- | --- |
| Dissociation to composite psychosis | female | 40 to 60 | 52 to 60 | 0.80 | 0.020 |
| Depression to composite psychosis | female | 40 to 60 | 42.4 to 60 | 1.76 | 0.044 |

These are reported with the contrast actually estimated. The nominal label is not reused for a shorter comparison.


Decision counts across the 69 reportable slopes in each layer:

| Layer | Reject | Accept | Undecided |
| --- | --- | --- | --- |
| Primary (valid-correlation) |    1 |    0 |   68 |
| Reference (brms) |    0 |    0 |   69 |

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


Across 48 matched linear coordinates the median difference is 0.110 posterior standard deviations at worst, with density overlap of at least 0.94. Across 24 comparisons on r(mediator, psychosis) the worst median difference is 0.477 standard deviations.

Agreement on the third coordinate is an **architecture-sensitivity** result, not a check that preprocessing is identical: the reference model reaches the marginal Fisher-z linearly, the primary model reaches it nonlinearly through the valid-correlation map.

## 7. Model checking

**Sampling.** Across all 575 fits: maximum R-hat 1.0041, minimum bulk effective sample size 1605, minimum tail effective sample size 1469, 1 divergent transition, 0 maximum-tree-depth events, minimum E-BFMI 0.50. Every fit met the convergence targets; the divergence position is set out below.

182 of 575 fits required an adapt_delta escalation, involving 196 escalation steps in total. Escalation is recorded per fit in the fit ledger rather than summarised away.


Divergent transitions: 1 in total across all 575 fits. Every one of the 66 primary fits, whose numbers are reported, had none; no tolerance applies to those.

1 of the 509 sensitivity refits retained a divergence after exhausting the adapt_delta ladder, against a cap of 5, namely loso_brms|depress_cps|female|S029 (1). Deleting a study from an already small pathway can produce a geometry the sampler finds harder than the model being reported. They are named here rather than absorbed into a summary, so a reader can check whether any of them underlies a reported robustness count.

**The tolerance was adopted after a run exposed the issue, not prespecified for this analysis.** It is stated as a rule for subsequent runs, and the cap was fixed before the count was inspected, but describing it as prespecified here would be false. Every threshold other than this one applied to every fit without exception.


**Posterior predictive checks.** Both replication forms were computed for all 6 pathways. Conditional coverage of the 95 per cent replicated intervals averaged 100.0 per cent and new-study coverage 100.0 per cent. 0 studies had a posterior predictive p value outside [0.05, 0.95].

Both forms are reported because a conditional check alone can look satisfactory simply because the study-level random effects have absorbed an outlying study.

These checks detected **no gross conflict between the model and the data**. That is not a demonstration of predictive calibration, and the checks should not be described as having been passed. Complete in-sample coverage is a weak result: with four to ten studies the intervals are wide, so coverage near one is close to guaranteed and carries little information. What was examined is the joint discrepancy of each study's three coordinates under both replication forms, plus coordinate-wise residuals. What these checks would NOT detect includes a misspecified between-study distribution shape, dependence between studies sharing participants, and any error in the plug-in sampling covariance matrices, which are treated as known throughout.


**Prior predictive check.** 66 prespecified plausibility criteria across 6 pathways, all of which were met. The thresholds are recorded in `config/analysis.yml` and were committed before the run that tested them; the configuration hash forms part of the analysis identifier, so that ordering is verifiable rather than asserted. The criteria require the priors to COVER the published correlations tabulated in `bayesian/docs/prior_scope.md`, not to encode them: the priors remain weakly informative and are not literature-derived.


**Prior and likelihood power scaling.** Sensitivity was computed for 342 parameters across the primary fits. 164 were flagged, of which 87 were heterogeneity parameters. This asks a different question from the prior grid: the grid refits at other prior widths and asks whether the conclusion moves, whereas power scaling asks which of the prior and the likelihood is driving each posterior.

A parameter flagged as prior-dominated is one the data barely inform, so its posterior should not be read as an empirical finding. This is expected for tau at four to ten studies and is the reason the prediction intervals are described as prior-informed. Full results, including any flag on the coordinate carrying a reported decision, are in `diagnostics/power_scaling.csv`.


**Simulation-based calibration.** Rank-based SBC over 6 prespecified scenarios. Of 600 attempted fits, 0 failed outright and 7 breached a diagnostic threshold, leaving 97 to 100 per scenario in the primary test. A rank from a fit that did not converge is not a draw from the posterior the model defines, so including it would confound sampler failure with miscalibration; every attempted fit is nonetheless retained in this accounting and in the all-ranks sensitivity test below. Of 66 scalar parameters, 3 had a rank-uniformity p value below 0.05. Under exact uniformity the expectation is 3.3 and the 95th percentile is 6, so this is within the range chance alone produces.


Repeating the same tests over **all** ranks, including those from the 7 diagnostically invalid fits, gives 3 p values below 0.05 against 3 in the primary test. The exclusion therefore changes nothing.

This is a **screening-level** calibration check, not high-powered proof of calibration, and it is conditional on the plug-in sampling covariance matrices: it validates the implemented likelihood and sampler given those matrices, not the uncertainty that arises from estimating them from participant-level data. The p values are correlated across parameters and across scenarios, so the binomial figure above is a reference point rather than a formal guarantee. Raw truths, ranks and per-replicate diagnostics are archived in `diagnostics/sbc_ranks_raw.csv`, so every rank can be recomputed without refitting.

## 8. Limitations

- **Moderation is ecological.** A percentage-female slope cannot establish that an association is stronger among women, and a clinical against non-clinical contrast cannot establish an individual-level mechanism. These are aggregate models.

- **Mediation here is statistical, not causal.** The correlations are largely cross-sectional. The indirect effect is an observational statistical quantity.

- **The moderator posteriors are substantially prior-determined at this number of studies.** The posterior contraction statistic is reported for every slope so this is explicit rather than left to be inferred.

- **Reporting bias was not tested.** Formal selection models and funnel-asymmetry tests are not fitted, because at five to ten studies they would themselves be poorly identified. The absence of such a test is not evidence that reporting bias has been excluded.

- **Two studies remain excluded from the dissociation-to-composite pathway.** Evans 2015 is excluded permanently on current information: its reported values are regression coefficients with a binary outcome, not the correlation triplet the model requires. Uyan 2022 is held as a pending source query and could be reinstated if the trauma-to-dissociation correlation is obtained.

- **The Newcastle-Ottawa scores for the negative-schema pathways were supplied during this analysis** from the manuscript supplementary material. Every quality-moderator result in those two pathways depends on them. They are recorded with their instrument, assessors, consensus procedure and source in `data/provenance/quality_scores.csv`.

- **`bayesianmasem` was not used.** Both of its interfaces fail internally on these models. That attempt is documented, and is not described as a successful independent validation. The independent implementation check is supplied by the brms layer.

