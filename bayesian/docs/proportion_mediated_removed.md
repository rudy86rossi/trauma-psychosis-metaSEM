# Why the proportion mediated was removed

British English throughout.

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
