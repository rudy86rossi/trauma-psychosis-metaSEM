# Reinstating a gated proportion mediated in the Bayesian reanalysis

> **Superseded pre-run planning record.** This is a historical document, kept for the audit
> trail and not rewritten. It uses the word "prespecified" in several places; that is wrong
> and must not be repeated. The proportion-mediated analysis was a **post hoc** amendment,
> and its estimand, thresholds and reporting rule were fixed before the refit that evaluated
> them. The plan also expected five of six pathways to carry a percentage, with negative
> schema to delusions withheld. **In `run_abc9305d0d01` all six pathways met the rule**;
> negative schema to delusions fails the superseded median-to-standard-deviation heuristic
> at 2.92 but meets both posterior probability conditions. For three pathways the
> released-or-withheld classification was not stable to every single-cohort deletion. This
> plan was reviewed before implementation, and the gate it proposes was replaced by the
> posterior probability rule now in `config/analysis.yml`. What was actually done, and why,
> is in
> [`bayesian/docs/proportion_mediated.md`](bayesian/docs/proportion_mediated.md); the results
> are in [`bayesian/BAYESIAN_METAREG_RESULTS.md`](bayesian/BAYESIAN_METAREG_RESULTS.md) and
> [`bayesian/results/run_abc9305d0d01/`](bayesian/results/run_abc9305d0d01/).

Plan prepared 3 August 2026 for Ricardo Twumasi. British English throughout.
Concerns the `bayesian-reanalysis` branch of `trauma-psychosis-metaSEM`, release run
`c9cb1a366dfb`. This document is a proposal. Nothing in the release has been changed.

---

## 1. The question, and the short answer

Co-authors have asked for a point estimate of the percentage of the total trauma-to-psychosis
association that is explained by each mediator, on the reasonable grounds that a single median
figure is more useful to a clinician than a set of standardised path coefficients. The question is
whether that quantity can be recovered from the Bayesian reanalysis, and whether reporting it
alongside credible and prediction intervals is defensible.

The answer has three parts, and they are not the same answer.

The **point estimate and credible interval are feasible and defensible in five of the six
pathways**, provided the quantity is defined precisely, summarised by the posterior median rather
than the mean, and released only where a prespecified separation condition on the total effect is
met. This is a stronger and more honest position than either of the two positions the project has
previously held.

The **prediction interval is not feasible and should not be reported in five of the six pathways**.
The new-study predictive distribution of the total effect straddles zero badly enough that the
predictive proportion mediated is genuinely pathological. This is not a presentational difficulty
that a wider interval would solve. It is the quantity refusing to exist.

The **causal reading co-authors will attach to the phrase "percentage explained" is not supported by
these data at all**, and the labelling in the manuscript must carry that weight rather than leaving
it to a limitation paragraph. This is the objection most likely to be raised by a statistical
reviewer at the *BJPsych*, and it is the one the current removal note does not make.

---

## 2. Where the project currently stands, and one premise that does not hold

The release removed the proportion mediated deliberately. The reasoning is set out in
`bayesian/docs/proportion_mediated_removed.md`, was prompted by section 3 of
`PR9_FINAL_FIX_AUDIT_2026-07-31.md` in the sibling working folder, and is enacted in code: the
`pmed_pop` declaration is gone from `bayesian/stan/masem_valid.stan`, `"pmed"` is gone from the
pooled-quantity loop at `bayesian/R/lib_stan.R:221`, the `interpretable` flag is gone from
`bayesian/R/lib_report.R`, and `bayesian/R/lib_writeup.R:171` now emits a paragraph stating that the
quantity is not reported. The manuscript's limitations paragraph, reproduced at line 191 of
`BAYESIAN_MANUSCRIPT_REPLACEMENT_CHANGE_LIST_2026-07-31.md`, commits the same position in print.
Any reinstatement therefore has to be argued, not merely implemented, and the limitations sentence
has to be amended rather than quietly dropped.

Three of the four arguments in the removal note survive scrutiny. The fourth does not, and one
supporting premise is factually wrong about the released numbers.

The removal note asserts that the ratio's denominator, the total effect, "is not bounded away from
zero in any of these six syntheses". Taken as a statement about the posterior, this is not what the
release shows. In `results/run_c9cb1a366dfb/tables/pooled_quantities.csv` the unmoderated total
effect excludes zero in all six pathways, with posterior mass below zero ranging from below 0.0001
to 0.0086 and median-to-standard-deviation ratios of 8.85, 5.99, 5.89, 4.47, 4.27 and 2.92. What is
true, and materially weaker, is that the posterior for the total effect has *support* near zero,
because the denominator is `tanh` of a coordinate with an unbounded normal-like posterior. Positive
density at the singularity is sufficient to destroy the moments of the ratio. It is not sufficient
to destroy the ratio.

The distinction matters because the release's own machinery already computes the relevant fact. The
column `total_excludes_zero`, retained at `bayesian/R/lib_report.R:163` as a vestige of the removed
`interpretable` flag, is precisely the gate the note dismisses. The note's objection to it, that
"a ratio whose denominator can approach zero is not made reportable by a flag saying when it is
not", is a fair criticism of the *implementation*, which printed the number next to the warning in
the same table as interpretable quantities. It is not an argument that no gate could work. A gate
that suppresses the row rather than annotating it is a different object.

---

## 3. Defining the estimand precisely

The Stan model computes the population-average path decomposition in `rho_to_path` at
`bayesian/stan/masem_valid.stan:41`, returning `[a, b, c', ab, rho_XY]`, and assigns these to
`a_pop`, `b_pop`, `cprime_pop`, `ind_pop` and `total_pop` at line 187. Because
`ab + c' = rho_XY(1 - rho_XM^2)/(1 - rho_XM^2) = rho_XY` identically, the total effect is not an
estimated sum but the pooled trauma-to-psychosis correlation itself. The proposed estimand is
therefore

> **PM = ab / rho_XY**, the share of the pooled zero-order trauma-psychosis correlation that is
> statistically accounted for by the trauma-to-mediator-to-psychosis chain, evaluated at the
> population-average parameter values of the valid-correlation MASEM.

Three consequences follow immediately and must appear in the methods.

The denominator is a correlation coefficient, so it is bounded in the open interval from minus one
to one. This bounds its magnitude but not its distance from zero, which is what governs the tail
behaviour of the ratio. PM itself is unbounded above and can be negative, so it is not a proportion
in the measure-theoretic sense and should never be described as one without qualification.

PM exceeds one whenever the direct path `c'` is negative, and is negative whenever `ab` and
`rho_XY` differ in sign. Both are the classical inconsistent-mediation cases. A reportability
condition must therefore include sign consistency, not merely separation from zero.

PM is a function of population-average parameters. It is not the average of the study-specific
proportions mediated, because the ratio is a non-linear function and Jensen's inequality bites. The
model already recognises this distinction for the indirect effect, computing `ind_study` and
`ind_study_mean` separately at lines 217 and 219 of the Stan file. The methods must say which of the
two objects PM is, and it is the former.

---

## 4. Why the posterior median is admissible where the posterior mean is not

The removal note's first objection is that as the posterior for the denominator puts mass near zero,
the posterior for the ratio has no finite moments. This is correct and is not in dispute. Because
`rho_XY = tanh(eta_2)` and the posterior for `eta_2` has strictly positive density everywhere on the
real line, the density of the denominator at zero is strictly positive, and the expectation of the
ratio does not exist. No amount of separation between the median and zero repairs this, and the
existing summary function `estimate_summary` at `bayesian/R/lib_summaries.R:21` returns `sd(draws)`,
which is why the earlier release printed enormous standard deviations beside unremarkable central
intervals. That was a real defect and the audit was right to flag it.

The inference drawn from it was too broad. Quantiles do not require moments. The posterior median
and the equal-tailed 2.5th and 97.5th percentiles of PM exist for any proper posterior on the ratio,
are finite with probability one, and converge under the usual ergodic conditions. The median is also
invariant under monotone transformation, so it does not depend on whether one summarises `ab/rho_XY`
or its logit-like reparameterisation, which is not true of the mean. The correct remedy for a
quantity with no first moment is to stop reporting its first and second moments, not to stop
reporting the quantity.

The practical rule that follows is unambiguous, and I would write it into the code rather than into
a comment. PM must be summarised by a function that returns the median, the equal-tailed interval,
the highest-density interval and the tail probabilities, and that **structurally cannot** return a
mean or a standard deviation. It must not be passed to `estimate_summary`, and it must not be passed
to `dienes_summary` at `bayesian/R/lib_summaries.R:43`, whose posterior-contraction statistic
`1 - (sd/prior_sd)^2` is undefined for it.

---

## 5. Why the diagnostic objection is answerable

The removal note's second objection is that "R-hat and effective sample size for a heavy-tailed
ratio are dominated by the tail behaviour of the estimand rather than by sampler performance, so an
honest convergence gate cannot judge it". This was a sound criticism of the classical
Gelman-Rubin statistic. It has been superseded.

Vehtari, Gelman, Simpson, Carpenter and Bürkner (2021) introduced rank-normalised split-R-hat
precisely because the classical statistic "will fail to correctly diagnose convergence failures when
the chain has a heavy tail or when the variance varies across the chains". Rank-normalisation
replaces each draw by its normal score across all chains before computing the statistic, so the
diagnostic depends only on the ordering of the draws and is entirely insensitive to whether the
estimand has finite moments. The same paper supplies bulk and tail effective sample sizes and, most
relevantly here, a practical method for the Monte Carlo standard error of a *quantile*. CmdStanR's
`$summary()` returns rank-normalised R-hat and split bulk and tail ESS by default, and
`posterior::mcse_quantile()` supplies the quantile MCSE.

The consequence is that PM can be admitted to `GATED_PARS` at `bayesian/R/lib_summaries.R:140`
rather than exempted from it, which was the audit's stated requirement, and that the gate applied to
it is a valid one. What must change alongside is the reported diagnostic: for PM the release should
publish rank-normalised R-hat, bulk and tail ESS, and the MCSE of the median and of the two interval
endpoints, and should not publish `mcse_mean` or `mcse_sd`, which are meaningless for it. This is a
strictly more stringent diagnostic regime than any other quantity in the release currently receives.

---

## 6. The reportability gate, prespecified

I propose the following gate, to be written into `config/analysis.yml` as a named block so that it
is visible as an analysis-defining choice rather than buried in R, in keeping with the existing
convention stated at the head of that file. A contrast's PM is released only if all four conditions
hold.

**Condition one, interval separation.** The 95 per cent equal-tailed credible interval for
`total_pop` excludes zero. This is the existing `total_excludes_zero` computation at
`bayesian/R/lib_report.R:163`.

**Condition two, singularity mass.** The posterior probability that `total_pop` is less than or
equal to zero is below 0.005. This is the direct measure of how much posterior mass sits on the
far side of the singularity, and it is already available as `1 - p_gt0` on the `total` row. The
threshold is a judgement; 0.005 is roughly an order of magnitude above the Monte Carlo resolution at
12,000 draws and roughly an order of magnitude below the point at which the ratio's central interval
starts to distort.

**Condition three, Fieller-type separation.** The ratio of the posterior median of `total_pop` to
its posterior standard deviation is at least 4. Fieller's theorem gives the underlying result: a
confidence set for a ratio is bounded if and only if the denominator is significantly different from
zero at the chosen level, which for a 95 per cent interval is a signal-to-noise ratio of 1.96. A
threshold of 4 is deliberately conservative relative to that minimum, and its sensitivity should be
reported across the grid 3, 4 and 5 in the same style as the existing robustness regions for the
smallest effect size of interest. Note that the standard deviation used here is that of the
*denominator*, which is well behaved, not that of PM, which is not.

**Condition four, sign consistency.** The posterior probability that `ind_pop` and `cprime_pop` are
both positive is at least 0.90. This excludes the inconsistent-mediation regimes in which PM leaves
the unit interval and ceases to have the meaning the word "percentage" conveys. Computed exactly
from the joint draws; a conservative lower bound from the marginals is
`1 - (1 - P(ab>0)) - (1 - P(c'>0))`, which is what the screen in section 7 uses.

Where the gate fails, the row is **absent**, not annotated. The release should state in prose which
contrasts were withheld and on which condition, so that the suppression is visible and countable
rather than inferred from a gap in a table. This is the specific defect the removal note identified
in the earlier implementation, and it is the one this design corrects.

---

## 7. What the gate returns on the numbers already released

I applied conditions one to four to every one of the 54 primary Stan contrasts in
`results/run_c9cb1a366dfb/tables/pooled_quantities.csv`, using the conservative marginal bound for
condition four. Thirty-two of the 54 pass. The six unmoderated pathways, which are what the main
results table would carry, behave as follows.

| Pathway | k | Total effect, median [95% CrI] | SD | Median / SD | P(total ≤ 0) | Sign lower bound | Gate |
| --- | ---: | --- | ---: | ---: | ---: | ---: | --- |
| Depression to composite psychosis | 5 | 0.317 [0.229, 0.367] | 0.036 | 8.85 | 0.0008 | 0.996 | **pass** |
| Dissociation to delusions | 7 | 0.365 [0.228, 0.472] | 0.061 | 5.99 | <0.0001 | 0.989 | **pass** |
| Dissociation to hallucinations | 10 | 0.287 [0.189, 0.385] | 0.049 | 5.89 | <0.0001 | 0.976 | **pass** |
| Negative schema to composite psychosis | 7 | 0.259 [0.132, 0.368] | 0.058 | 4.47 | 0.0009 | 0.970 | **pass** |
| Dissociation to composite psychosis | 4 | 0.290 [0.135, 0.415] | 0.068 | 4.27 | 0.0023 | 0.965 | **pass** |
| Negative schema to delusions | 5 | 0.251 [0.062, 0.408] | 0.086 | 2.92 | 0.0086 | 0.959 | **fail (conditions two and three)** |

This is the result that makes the proposal worth putting to co-authors. A prespecified gate,
justified on grounds that predate the data, releases the proportion mediated in five of the six
pathways and withholds it in exactly the pathway the project has independently and repeatedly
identified as its least secure. Negative schema to delusions is the k = 5 model whose four
frequentist findings the earlier re-run assessment flagged as boundary artefacts with tau-squared
pinned at zero, and whose mediation contrasts are labelled fragile throughout section 4.2 of the
older results document. The gate is not being tuned to produce that outcome; it produces it from a
condition on the denominator alone.

I would add one deliberate restriction. **PM should be reported only for the unmoderated models.**
Twenty-seven of the 32 passing contrasts are moderator endpoints, and a proportion mediated evaluated
at, say, 60 per cent female is a ratio conditional on a moderator value in an analysis that has
already concluded it cannot establish moderation. Reporting it invites exactly the over-reading the
release has worked to prevent. The six unmoderated rows answer the co-authors' question; the
forty-eight endpoint rows answer a question the data cannot support.

Two caveats on the screen itself. The medians and standard deviations above are the released
marginals, so condition three is exact, but condition four is a conservative bound and the true
joint probability will be slightly higher; a few contrasts near the boundary may move. More
importantly, none of the PM values themselves can be computed from this table, because the ratio
needs the joint draws. Section 9 addresses that.

---

## 8. Prediction intervals: the honest answer is no

Co-authors asked for prediction intervals alongside the credible intervals, which is the right
instinct given how much weight the release places on them elsewhere. On this quantity the answer has
to be no, and the reason is worth stating because it is the most clinically important thing in this
document.

The new-study predictive quantities are generated at `bayesian/stan/masem_valid.stan:191` by adding
a fresh `normal_rng(0, tau)` to each Fisher-z coordinate before mapping back through
`rho_to_path`. The between-study heterogeneity is large, so the predictive distribution of the total
effect is far more diffuse than its posterior. From
`results/run_c9cb1a366dfb/tables/prediction_intervals.csv`, unmoderated rows:

| Pathway | Predictive total, median | SD | Median / SD | P(total > 0) | Gate |
| --- | ---: | ---: | ---: | ---: | --- |
| Depression to composite psychosis | 0.320 | 0.071 | 4.54 | 0.995 | pass |
| Dissociation to delusions | 0.367 | 0.148 | 2.48 | 0.977 | fail |
| Dissociation to composite psychosis | 0.290 | 0.137 | 2.12 | 0.964 | fail |
| Dissociation to hallucinations | 0.291 | 0.138 | 2.10 | 0.971 | fail |
| Negative schema to composite psychosis | 0.257 | 0.160 | 1.61 | 0.937 | fail |
| Negative schema to delusions | 0.252 | 0.201 | 1.26 | 0.890 | fail |

Only depression to composite psychosis clears the gate, and it does so marginally. In the other five
pathways between 2.3 and 11.0 per cent of the predictive mass for the denominator lies at or below
zero. A predictive proportion mediated computed there is not a wide interval; it is a bimodal object
with mass running off to plus and minus infinity, for which a central interval is an artefact of
where the draws happened to fall.

This asymmetry between the posterior and the predictive is the substantive finding, and it should be
stated in the manuscript in words rather than suppressed as a technical failure. The population-
average share of the trauma-psychosis association routed through dissociation can be estimated. The
share that will be observed in the next cohort cannot, because the between-study heterogeneity in
this literature is large enough that a new sample could plausibly show no total association at all,
in which case the question "what percentage of it is mediated" has no answer. That is the same fact
as the existing headline that all 54 prediction intervals for `ab` include zero, expressed in the
units co-authors are asking for. It is arguably the single most useful thing the reanalysis has to
say to a clinician, and it argues for caution rather than for a headline percentage.

My recommendation is therefore that the release computes the predictive PM, applies the same gate,
finds that five of six fail, and reports that finding as a numbered result. Reporting the failure is
more informative than reporting nothing, and considerably more informative than reporting a number.

---

## 9. Feasibility and cost

The proportion mediated cannot be reconstructed from what is on disk. `extract_stan` at
`bayesian/R/lib_stan.R` summarises each pooled quantity marginally in the loop at line 221 and the
`CmdStanMCMC` object is not retained; the 49 `_targets/objects/fit_results_*` files hold summaries
only, totalling about 15 MB. A ratio needs the joint draws of `ind_pop` and `total_pop`,
which no longer exist. Approximating from the marginals by a delta-method or Fieller argument would
discard exactly the dependence between numerator and denominator that makes the ratio behave, and I
would not defend it at review.

A full pipeline rerun is therefore required, and is cheap. `diagnostics/fit_timings.csv` records 575
fits totalling 5,674 seconds of sampling, with the slowest single fit at 34.7 seconds. Adding a
generated quantity does not change the sampler's work; it adds one division per draw per contrast.
The dominant cost is the surrounding `targets` orchestration, figure rendering and document
generation. Budget half a working day including verification, on the machine that produced
`c9cb1a366dfb`, with CmdStan 2.36.0 and the `renv.lock` environment restored.

The rerun buys something the cheaper options cannot. Because PM enters the pipeline as an ordinary
generated quantity, it inherits the leave-one-study-out refits, the prior grid, the `priorsense`
power-scaling and the simulation-based calibration machinery that every other reported quantity
already passes through. Given that the quantity is being reinstated after a documented removal, it
needs to arrive with more sensitivity evidence than its neighbours, not less.

---

## 10. Implementation

The changes are small and localised. In order:

1. **`bayesian/stan/masem_valid.stan`.** Declare `vector[NC] pmed_pop;` beside `ind_pop` and
   `total_pop` at line 117, and assign `pmed_pop[c] = ind_pop[c] / total_pop[c];` immediately after
   line 187. Declare and assign `pmed_new[c] = ind_new[c] / total_new[c];` alongside the predictive
   block at line 195. Do not guard the division; a division by a near-zero denominator is the
   information the gate consumes, and clamping it would hide the thing being tested.

2. **`bayesian/R/lib_summaries.R`.** Add a new `ratio_summary()` beside `estimate_summary()` at line
   21. It returns `median`, `q_lo`, `q_hi`, `hdi_lower`, `hdi_upper`, `p_in_unit_interval`,
   `p_gt0`, `n_draws`, `mcse_median`, `mcse_q_lo`, `mcse_q_hi` and a logical `interval_contiguous`.
   It must not compute a mean or a standard deviation, and should not silently accept a request for
   one. Add `"pmed_pop"` to `GATED_PARS` at line 140 and `"pmed_new"` to `STOCHASTIC_PARS` at line
   144. Replace the comment at lines 148 to 153, which currently explains why the quantity was
   removed, with one explaining why rank-normalised R-hat makes gating it valid, citing Vehtari et
   al. (2021).

3. **`bayesian/R/lib_stan.R`.** Extract `pmed_pop` and `pmed_new` in `extract_stan` through
   `ratio_summary()` rather than through the existing loops at lines 221 and 232, so the column sets
   differ by construction and a mean cannot be produced by accident. Emit them into a separate
   `pmed` data frame, not into `pooled` and `predict`, so that a ratio and a path coefficient never
   share a table.

4. **`bayesian/R/lib_report.R`.** Apply the four gate conditions in `build_tables`, reusing the
   `total_excludes_zero` computation already at line 163 and adding the other three. Write
   `tables/proportion_mediated.csv` containing only passing rows, and
   `tables/proportion_mediated_withheld.csv` containing the failing rows with the condition each
   failed and no PM value. Add the gate thresholds to the run manifest.

5. **`bayesian/R/lib_writeup.R`.** Replace the paragraph at line 171 with a statement of the gate,
   the count of contrasts passing and failing, the PM table for the passing unmoderated pathways,
   and the predictive result from section 8. The existing generated-document architecture means no
   number is transcribed by hand, which should be preserved.

6. **`config/analysis.yml`.** Add a `proportion_mediated` block giving the four thresholds and the
   sensitivity grid for condition three, so a reviewer can see and change them in one place.

7. **`bayesian/docs/proportion_mediated_removed.md`.** Do not delete it. Rename it
   `proportion_mediated.md` and append a dated section recording that the quantity has been
   reinstated under a gate, which of the original four arguments stand, which was superseded by
   rank-normalised diagnostics, and which supporting premise about the denominator did not match the
   released numbers. Deleting the record would be the wrong move at review, and the honest history
   is itself an argument that the reinstated version has been thought about.

8. **Manuscript.** Amend the limitations sentence at line 191 of the change list, which currently
   states that the quantity "was therefore not used as a Bayesian estimand of substantive interest".
   It should now state that the quantity is reported under a prespecified separation condition on
   the total effect, is withheld where that condition fails, and that no predictive interval for it
   is available in five of six pathways.

9. **Tests.** Add to `bayesian/tests/testthat` a test that `ratio_summary()` returns no `sd` column;
   a test that a synthetic posterior whose denominator straddles zero fails the gate; and a test
   that the PM table and the withheld table together account for every contrast, so that a silently
   dropped row fails the suite rather than passing unnoticed.

---

## 11. Acceptance criteria

The rerun is accepted if, and only if, the following all hold. These are deliberately phrased so
that a partial success is a failure.

Rank-normalised R-hat for `pmed_pop` is below 1.01 and bulk and tail ESS both exceed 400 in every
fit that passes the gate. The MCSE of the median is below 5 per cent of the width of the 95 per cent
interval. Every contrast appears in exactly one of `proportion_mediated.csv` and
`proportion_mediated_withheld.csv`. No released table contains a mean or standard deviation of PM.
The five unmoderated pathways that pass the gate return PM medians within a few percentage points of
the frequentist likelihood-based values reported in the older results document, which were 57.6 for
dissociation to hallucinations, 46.4 for dissociation to delusions, 50.5 for dissociation to
composite psychosis, 26.4 for negative schema to composite psychosis, 27.4 for negative schema to
delusions and 31.1 per cent for depression to composite psychosis, with the dissociation-to-composite
pathway expected to differ because of the Evans and Uyan exclusions. The leave-one-study-out refits do not move any
reported PM median by more than about 10 percentage points, and where they do this is reported
rather than smoothed. The generated documents rebuild without hand transcription.

If the gate passes fewer than four unmoderated pathways, or if any PM median falls outside the
interval zero to one, the reinstatement should be abandoned and the co-authors told why.

---

## 12. What remains true against the proposal, and how the manuscript must handle it

Three objections survive everything above. None is fatal, but the manuscript has to carry them
explicitly rather than leaving them to a reviewer.

**The causal reading is not available.** The synthesis pools cross-sectional correlations. The
paths `a`, `b` and `c'` are regression coefficients in a path model fitted to a pooled correlation
matrix, not causal effects, and the phrase "percentage of the effect explained" imports an
assumption set that these data cannot meet: no unmeasured confounding of the mediator-outcome
relation, no exposure-mediator interaction, and correct temporal ordering, none of which a
cross-sectional correlation can establish. VanderWeele's treatment of the proportion mediated in the
causal-inference literature makes the conditions explicit, and Ananth and VanderWeele's *BJOG*
commentary makes the clinical case for caution in almost exactly the terms co-authors are using
here. My recommendation is that the manuscript never writes "percentage of the effect explained" and
instead writes "**percentage of the pooled trauma-psychosis association statistically accounted for
by the indirect path**", both in the table header and at every mention in the text. That is longer
and less satisfying, and it is what the data support.

**The estimator is known to be unstable in exactly this regime.** MacKinnon, Warsi and Dwyer's
simulation study of mediated effect measures examined the proportion of the total effect mediated
across sample sizes from 10 to 5,000 and found it poorly behaved at small samples. Preacher and
Kelley's review of mediation effect sizes catalogues the same problems and declines to recommend it.
The relevant sample size here is ambiguous, because the total participant Ns are large, from 1,053
to 10,791, but the between-study variance that drives the posterior width is identified by four to
ten samples. The honest framing is that the within-study information is ample and the between-study
information is thin, and that PM's interval width is dominated by the latter. Note that the earlier
frequentist analysis reported a depression interval of 27.5 to 35.1 per cent against the Bayesian
14.4 to 57.1, because the frequentist Stage 1 for that pathway fixed the between-study variance at
zero. The wider Bayesian interval is the more honest one, and this comparison is a good argument to
have in print.

**A single pooled percentage will be over-read.** This is the co-authors' stated motivation and also
the main risk. A clinician told that 55 per cent of the trauma-hallucination association runs
through dissociation will reasonably infer that treating dissociation should remove about half the
excess risk in the patient in front of them. Nothing in this analysis supports that inference: not
the cross-sectional design, not the between-study heterogeneity, and not the prediction intervals,
which say the next study could find no association at all. If a percentage is going to appear in the
main results table, the sentence that follows it in the abstract and the discussion matters more
than the number.

---

## 13. Recommendation

Reinstate, under the gate, for the six unmoderated pathways only, with the median and equal-tailed
credible interval, with no mean or standard deviation anywhere, with the sixth pathway withheld and
the withholding stated, with no predictive interval in five of six pathways and that failure
reported as a finding, and with the causal language removed from the table header and the text.

This gives co-authors the clinically legible number they have asked for, gives a statistical
reviewer a prespecified rule and a documented change of position rather than a silent reversal, and
does not overturn any conclusion the release currently rests on. It costs about half a day.

---

## References

Ananth, C. V., & VanderWeele, T. J. (2019). Proportion mediated in a causal mediation analysis: how
useful is this measure? *BJOG*, 126(8), 983.

Cheung, M. W.-L. (2015). *Meta-analysis: A structural equation modeling approach*. Wiley.

Dienes, Z. (2020). How do I know what my theory predicts? *Advances in Methods and Practices in
Psychological Science*.

Fieller, E. C. (1954). Some problems in interval estimation. *Journal of the Royal Statistical
Society, Series B*, 16(2), 175-185.

Kruschke, J. K. (2018). Rejecting or accepting parameter values in Bayesian estimation. *Advances in
Methods and Practices in Psychological Science*, 1(2), 270-280.

MacKinnon, D. P., Warsi, G., & Dwyer, J. H. (1995). A simulation study of mediated effect measures.
*Multivariate Behavioral Research*, 30(1), 41-62.

Preacher, K. J., & Kelley, K. (2011). Effect size measures for mediation models: quantitative
strategies for communicating indirect effects. *Psychological Methods*, 16(2), 93-115.

VanderWeele, T. J. (2013). Policy-relevant proportions for direct effects. *Epidemiology*, 24(1),
175-176.

Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P.-C. (2021). Rank-normalization,
folding, and localization: an improved R-hat for assessing convergence of MCMC (with discussion).
*Bayesian Analysis*, 16(2), 667-718.
