# Why the Bayes factors were removed

British English throughout.

An earlier version of this analysis reported restricted Bayes factors alongside
the interval decisions. They are **not** part of this release, and
`config/analysis.yml` refuses to load if `bayes_factors` is set to true.

## The reason

The computation was sound; the inference was not.

1. **The models were chosen after seeing the answer.** The two moderator models
   carried forward were selected because their posterior probabilities for a
   change in the indirect effect had already fallen beyond 0.95 or 0.05. A Bayes
   factor computed on a model selected that way does not have its advertised
   interpretation.
2. **The direction came from the data.** The half-normal alternative was
   oriented by the sign of the observed posterior median. A directional
   hypothesis read off the posterior it is then tested against is circular.
3. **The count was wrong.** The phrase "two slopes" conflated models with
   coordinates. Two moderator models were selected, giving six coordinate
   calculations, of which five directional and two two-sided Bayes factors
   exceeded 3 at the chosen scales.
4. **It answered a different question.** Evidence against a point null is not
   evidence that an effect exceeds a scientifically meaningful size. The
   interval decisions in this release address the second question, which is the
   one the manuscript asks.

Dienes (2020) is explicit that a Bayes factor must be reported with a robustness
region over the prior scale. That requirement was met. It does not repair
post-selection or data-chosen direction.

## What would be required to reinstate them

A separately prespecified sensitivity analysis in which:

- the direction is fixed from external theory before these data are seen, or the
  alternative is two-sided;
- the models are named in advance rather than selected on the posterior;
- all nuisance priors are matched between the models being compared;
- the density or marginal-likelihood computation is validated on simulated data
  where the answer is known;
- prior-scale sensitivity is reported.

Until then, no Bayes-factor claim appears in any headline, table, figure or
abstract of this analysis.
