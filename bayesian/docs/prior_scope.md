# Prior scope: an explicit departure from the original brief

British English throughout.

The original scientific brief asked for informative priors drawn from the
published literature. **This release does not use them.** That is a deliberate
scope change, recorded here rather than left implicit.

## What was proposed

Informative intercept priors derived from Bailey et al. (2018) and Longden et
al. (2020), robustified as

    p(mu) = 0.20 N(m_external, s_prior^2) + 0.80 N(0, 0.35^2)

with external weights of 0, 0.20 and 0.50 in sensitivity.

## Why they are not used

Both source syntheses probably contain studies that also appear in the present
extraction. A study-by-study overlap audit, matching on author, year, sample and
cohort, has not been completed. Using an estimate derived from overlapping
primary studies as external prior information counts the same participants
twice, and the resulting posterior would be narrower than the evidence supports.

This is a caution, not a claim that the external estimates are wrong. The
published values are used, but only as **prior predictive plausibility anchors**:
the check is that the priors do not exclude values the external literature
reports, not that they assert them.

| Correlation | Published r | Fisher z | Role here |
| --- | ---: | ---: | --- |
| Trauma with hallucination severity | 0.199 | 0.202 | plausibility anchor only |
| Trauma with delusion severity | 0.172 | 0.174 | plausibility anchor only |
| Dissociation with hallucinations | 0.461 | 0.499 | plausibility anchor only |
| Dissociation with delusions | 0.418 | 0.445 | plausibility anchor only |
| Dissociation with positive symptoms | 0.437 | 0.468 | plausibility anchor only |

Bloomfield et al. (2021) report indirect effects through dissociation and
negative beliefs, but their Cohen's d summaries are not commensurate with
standardised path coefficients here and were not converted.

The current project preprint was not used as a prior: it is the result being
replaced, and would count the present data twice.

## What is used instead

Honestly labelled weakly informative priors, scale-calibrated and checked by
prior predictive simulation before any posterior was inspected.

| Parameter | Prior | Basis |
| --- | --- | --- |
| Intercepts, alpha | N(0, 0.35^2) on Fisher-z | 95 per cent prior interval spans about r = -0.59 to 0.59, covering the external anchors without asserting them |
| Moderator slopes | N(0, 0.20^2) on the **full nominal contrast** | Places the smallest effect size of interest at a quarter of a prior standard deviation, so the prior neither sits inside the null interval nor presupposes moderation |
| Heterogeneity, tau | half-N(0, 0.20^2) | Röver (2021) scale guidance and the empirical compilation of van Erp et al. (2017) |

Specifying the moderator prior on the full-contrast scale, then dividing by the
nominal span to reach the fitted per-unit coefficient, removes an inconsistency
in the earlier analysis: a moderator was otherwise more or less regularised
purely because of the unit its coefficient happened to be expressed in.

## To reinstate informative priors

Complete the overlap audit first. It is a prerequisite, not a refinement. If
non-overlapping commensurate evidence is established, a robust mixture combining
an informative and a weak component should be prespecified before the run, with
the external weight varied in sensitivity.
