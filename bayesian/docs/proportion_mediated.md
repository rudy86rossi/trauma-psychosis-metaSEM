# The percentage carried by the indirect path: removed, then reinstated under a rule

British English throughout. This file has two halves. The first records the July 2026
decision to remove the quantity and is left exactly as it was written. The second records
the August 2026 decision to reinstate it, which of the original arguments survived and
which did not. Neither half is deleted. A reader at review is entitled to see that the
position changed and on what grounds, and the honest history is itself part of the
argument that the reinstated version has been thought about.

---

## Part one: why it was removed, July 2026

An earlier version of this analysis computed `pmed_pop = ab / (c' + ab)` in the
Stan generated quantities, exported it as a pooled quantity, and reported it in
the results document with an `interpretable` flag. It is **not** part of this
release. The Stan model no longer computes it, no table contains it, and no
generated sentence mentions it except to say it was not used.

## The reason

The quantity is a ratio whose denominator is the total effect, and the total
effect is not bounded away from zero in any of these six syntheses.

1. **It is unbounded, not merely wide.** As the posterior for the total effect
   puts mass near zero, the posterior for the ratio has no finite moments. A
   central 95 per cent interval can still be computed and can still look modest,
   which is precisely the problem: a finite interval is not evidence that the
   quantity is well behaved. Released rows showed exactly this pattern, with
   unremarkable central intervals beside enormous posterior standard deviations.
2. **It cannot be gated on convergence.** R-hat and effective sample size for a
   heavy-tailed ratio are dominated by the tail behaviour of the estimand rather
   than by sampler performance, so an honest convergence gate cannot judge it.
   The previous release acknowledged this by exempting `pmed_pop` from
   `GATED_PARS` in `lib_summaries.R` while continuing to report it. That is the
   wrong way round: a quantity whose diagnostics cannot be interpreted is a
   quantity that should not be reported, and an exemption is not a remedy.
3. **The flag did not fix it.** Marking a row uninterpretable where the total
   effect includes zero tells a reader when not to read the number, but the
   number is still printed next to the warning, in a table of reported results,
   in the same units as the quantities that are interpretable.
4. **Nothing in the manuscript depends on it.** The claims the paper makes rest
   on `a`, `b`, `c'`, `ab`, the total effect, posterior probabilities,
   between-study heterogeneity and new-study prediction intervals. The
   proportion mediated added no argument that those do not already carry.

## What replaces it

Nothing, in the Bayesian synthesis. The pooled tables report `a`, `b`, `c'`,
`ab` and the total effect, with equal-tailed and highest-density intervals,
posterior probabilities, heterogeneity and prediction intervals.

## What is not affected

The descriptive systematic review may continue to summarise percentages mediated
as reported by the individual included studies. Those are a narrative summary of
what the published analyses stated, covering mediator families that could not be
meta-analysed at all. They are not estimates from these models, and the
manuscript must keep the two visibly separate.

## Where it was removed

| File | What went |
| --- | --- |
| `bayesian/stan/masem_valid.stan` | the `pmed_pop` declaration and its assignment in generated quantities |
| `bayesian/R/lib_stan.R` | `"pmed"` from the pooled-quantity extraction loop |
| `bayesian/R/lib_report.R` | the `interpretable` flag |
| `bayesian/R/lib_writeup.R` | the reported paragraph, replaced by a statement that the quantity is not reported |
| `bayesian/R/lib_writeup_methods.R` | the estimand from the recovered-quantities sentence |
| `bayesian/R/lib_summaries.R` | the gate exemption note |

---

## Part two: reinstated under a post hoc rule, 3 August 2026

The co-authors asked for a single figure for the share of the adversity-psychosis
association each mediator represents, on the reasonable ground that a clinician finds
that more useful than a table of standardised path coefficients. The request was
examined rather than answered by repeating the July decision.

**This is a post hoc analysis amendment, not a prespecification.** The word
"prespecified" is not used of it anywhere in the code, the configuration, the generated
documents or the manuscript. What is fixed before the refit that tests it is the
estimand, the reporting rule, the reportability thresholds and the diagnostics. That is
the honest description and it is the one a statistical reviewer will accept.

### Which of the four original arguments stand

**Argument one stands, and is now enforced rather than argued.** The ratio has no finite
moments. This is not a matter of degree: `total` is `tanh` of a coordinate with unbounded
support, so its density at zero is strictly positive and the expectation does not exist.
The earlier release printed enormous standard deviations beside modest central intervals,
which was the visible symptom. What the July note inferred from it was too broad.
Quantiles need no moments. The median and the equal-tailed percentiles are finite with
probability one and converge under the usual ergodic conditions, and the median is
invariant under monotone reparameterisation, which the mean is not. The remedy for a
quantity with no first moment is to stop reporting its first and second moments. So
`ratio_summary()` in `lib_summaries.R` is structurally incapable of returning a mean or a
standard deviation, and raises if a moment-based column ever appears in its output.

**Argument two was superseded by the literature.** The July note held that R-hat and
effective sample size for a heavy-tailed ratio are dominated by the estimand's tail
rather than by sampler performance, so no honest convergence gate could judge it. That
was a sound criticism of the classical Gelman-Rubin statistic and it is why the earlier
release exempted the quantity. It does not apply to the rank-normalised statistic.
Vehtari, Gelman, Simpson, Carpenter and Bürkner (2021) introduced rank normalisation
precisely because the classical form "will fail to correctly diagnose convergence
failures when the chain has a heavy tail"; each draw is replaced by its normal score
across all chains before the statistic is computed, so the diagnostic depends only on the
ordering of the draws and is insensitive to whether moments exist. The same paper
supplies the effective sample size and Monte Carlo standard error of a quantile.
`ratio_diagnostics()` reports rank-normalised R-hat, bulk and tail ESS, ESS at the median
and at both interval endpoints, and MCSE at all three. That is a stricter regime than any
other quantity in this release receives.

**Argument three stands and is the design constraint.** Marking a row uninterpretable
while printing the number next to the warning does not work. So the released and withheld
pathways are written to separate files, and the withheld file carries no percentage at
all, only the condition that failed. The generated six-row table prints "not stably
estimable" in place of a value, which is a statement rather than a number a reader can
lift out of context.

**Argument four no longer applies, because the request changed what is being asked.** The
conclusions still rest on `a`, `b`, `c'`, `ab`, the total effect, the posterior
probabilities, the heterogeneity and the prediction intervals, and the percentage adds no
argument those do not carry. It is reported as a descriptive companion to `ab` at the
co-authors' request, and it is labelled as one. It does not become the evidence for
mediation.

### The supporting premise that did not match the released numbers

The July note asserted that the total effect "is not bounded away from zero in any of
these six syntheses". Read as a statement about the posterior, that is not what
`run_c9cb1a366dfb` showed: the unmoderated total effect excluded zero in all six
pathways. What is true, and materially weaker, is that the posterior for the total effect
has *support* near zero. Positive density at the singularity is enough to destroy the
moments of the ratio. It is not enough to destroy the ratio. Recording the difference
matters, because the stronger claim would have justified never reporting the quantity and
the weaker one does not.

### The rule

Set out in `config/analysis.yml` under `proportion_mediated`, so a reviewer can see and
change it in one place. A pathway's percentage is released only if all three hold:

1. the parent Stan fit passed every sampler diagnostic already applied to the release;
2. `P(total > delta | y) >= 0.95`, with `delta = 0.05`;
3. `P(0 < percentage < 100 | y) >= 0.95`.

`delta` is examined at 0.025 and 0.10 as post hoc sensitivity values, described as such
and never as clinically validated boundaries. A median-to-standard-deviation rule on the
denominator is retained as an additional heuristic, reported and never decisive; it is
a signal-to-noise summary, not a Fieller-derived reportability criterion, and it is not
presented as one.

Condition three carries most of the weight. Outside the unit interval the ratio is in the
inconsistent-mediation or suppression regime, where the word "percentage" stops conveying
what a reader will take from it. A median outside 0 to 100 is not treated as a
computational failure to be discarded: it is retained in the audit and fails condition
three on its own merits.

### What is retained regardless of the outcome

Every pathway appears in `tables/proportion_mediated_audit.csv` whether or not it was
released, with `P(|total| < delta)`, `P(percentage < 0)`, `P(percentage > 100)`,
`P(|ratio| > 2)`, the 1st, 2.5th, 50th, 97.5th and 99th percentiles, and the per-chain
medians and interval endpoints. No draw is truncated, Winsorised, clipped to 0 to 100 or
silently discarded; a non-finite draw raises rather than being removed. The raw draws of
`ind_pop`, `cprime_pop`, `total_pop` and the ratio, with chain and draw identifiers, are
archived in `diagnostics/pmed_draws.csv.gz` so the quantity can be recomputed or
replotted without a refit.

There is no minimum number of passing pathways, and no acceptance criterion refers to how
many pass or to whether the values resemble the earlier frequentist percentages. Those
came from a different model and a different evidence set. A material difference is worth
investigating and is not by itself evidence of error in either.

### What sensitivity it inherits, and what it does not

The unmoderated models previously had no prior-grid and no leave-one-cohort-out refits;
both existed only inside the moderator branch of the fit plan. Since the percentage is
read off the unmoderated models, both were added, giving it a prior sensitivity and a
deletion sensitivity as well as the robust-likelihood, weighted-covariance and
alternative-instrument variants it already inherited. Stability is judged by whether the
percentage **and its released-or-withheld classification** move, not by an arbitrary
movement threshold.

Neither power scaling nor simulation-based calibration is claimed to validate this
derived ratio. Power scaling measures how a posterior moves when the prior and the
likelihood are tempered; simulation-based calibration tests the sampler's recovery of the
model's own parameters. Both are reported. Neither is evidence that a ratio is a stable
or interpretable estimand, and the documents say so.

### What is not reported

No prediction interval for the percentage, and none is computed. A new-study ratio would
be especially unstable, because the predictive distribution of the denominator is far
more diffuse than its posterior. Note the arithmetic, since an earlier draft of the plan
got it wrong: in `run_c9cb1a366dfb` the equal-tailed new-study interval for the *total*
association included zero in four of six pathways, not five. The claim that belongs in
the manuscript is the one that is true of the indirect effect, where all six prediction
intervals include zero.

No moderator-endpoint percentages. A ratio conditional on a moderator value, in an
analysis that has concluded it cannot establish moderation, invites exactly the
over-reading this release has worked to prevent.

No causal reading. The synthesis pools cross-sectional correlations; `a`, `b` and `c'`
are regression coefficients in a path model fitted to a pooled correlation matrix. The
phrase "percentage of the effect explained" imports no unmeasured confounding of the
mediator-outcome relation, no exposure-mediator interaction and correct temporal
ordering, none of which these data establish. The documents write "percentage of the
pooled adversity-psychosis association represented by the estimated indirect statistical
pathway", and add at every principal mention that this is a model-based decomposition of
pooled, largely cross-sectional associations, not an individual-level causal percentage
and not the expected effect of intervening on the mediator.

### Where it was reinstated

| File | What changed |
| --- | --- |
| `bayesian/stan/masem_valid.stan` | `pmed_prop` and `pmed_pct` in generated quantities, division unguarded |
| `config/analysis.yml` | the `proportion_mediated` block, validated at load |
| `bayesian/R/lib_summaries.R` | `ratio_summary()` and `ratio_diagnostics()`; the quantity is deliberately not in `GATED_PARS` |
| `bayesian/R/lib_stan.R` | extraction into its own frame, with the joint-draw gate inputs and the independent recomputation check |
| `bayesian/R/lib_fitplan.R` | unmoderated prior-grid and leave-one-cohort-out branches |
| `bayesian/R/lib_prior_checks.R` | `pmed_prop` added to power scaling for the unmoderated fits |
| `bayesian/R/lib_report.R` | the rule, the four tables and the stability table |
| `bayesian/R/lib_writeup.R`, `lib_writeup_methods.R` | the reported paragraphs and the six-row table |
| `_targets.R` | `stan_file` made a real dependency of `fit_results` and `sbc_ranks` |

### References

Ananth, C. V., & VanderWeele, T. J. (2019). Proportion mediated in a causal mediation
analysis: how useful is this measure? *BJOG*, 126(8), 983.

MacKinnon, D. P., Warsi, G., & Dwyer, J. H. (1995). A simulation study of mediated effect
measures. *Multivariate Behavioral Research*, 30(1), 41-62.

Preacher, K. J., & Kelley, K. (2011). Effect size measures for mediation models:
quantitative strategies for communicating indirect effects. *Psychological Methods*,
16(2), 93-115.

VanderWeele, T. J. (2013). Policy-relevant proportions for direct effects.
*Epidemiology*, 24(1), 175-176.

Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P.-C. (2021).
Rank-normalization, folding, and localization: an improved R-hat for assessing
convergence of MCMC (with discussion). *Bayesian Analysis*, 16(2), 667-718.
