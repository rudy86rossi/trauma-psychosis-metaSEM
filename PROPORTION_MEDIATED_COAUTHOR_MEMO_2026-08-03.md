# Memo to co-authors: reporting a percentage mediated

> **Superseded pre-run planning record.** This is a historical document, kept for the
> audit trail and not rewritten. The proportion-mediated analysis was a **post hoc**
> amendment. Its estimand, thresholds and reporting rule were fixed before the refit that
> evaluated them, but they were not prespecified, and this memo should not be cited as
> though they were. This memo anticipated that the rule would release five pathways and
> withhold negative schema to delusions. **In `run_abc9305d0d01` all six pathways met the
> rule**, although for three of them the released-or-withheld classification was not stable
> to every single-cohort deletion. The results that stand are in
> [`bayesian/BAYESIAN_METAREG_RESULTS.md`](bayesian/BAYESIAN_METAREG_RESULTS.md), the method
> in [`bayesian/BAYESIAN_METAREG_METHODS.md`](bayesian/BAYESIAN_METAREG_METHODS.md), the
> reasoning in [`bayesian/docs/proportion_mediated.md`](bayesian/docs/proportion_mediated.md),
> and the tables in
> [`bayesian/results/run_abc9305d0d01/`](bayesian/results/run_abc9305d0d01/).

3 August 2026. From Ricardo Twumasi. Two pages. The technical plan is in
`PROPORTION_MEDIATED_REINSTATEMENT_PLAN_2026-08-03.md` for anyone who wants the working.

## What was asked

Several of you have asked whether we can give readers a single figure for the percentage of the
trauma-to-psychosis association that each mediator accounts for, on the grounds that a clinician
will find "about half of it runs through dissociation" far more useful than a table of standardised
path coefficients. That is a fair point and I have looked into it properly rather than repeating our
earlier decision to drop the quantity.

## The short version

We can report it, for five of our six pathways, as a median with a credible interval. We cannot
report a prediction interval for it in five of those six. And we should not call it "the percentage
of the effect explained", because that phrase means something specific in the causal literature and
our cross-sectional correlational data cannot support it.

## Why we removed it in the first place, and what has changed

The quantity is a ratio: the indirect path divided by the total association. Ratios misbehave when
the thing on the bottom can be close to zero, and our internal audit was right that the earlier
version of this analysis was reporting a quantity whose average and standard deviation formally do
not exist. It was printing enormous standard deviations next to modest-looking intervals, which is
the classic warning sign.

Two things have changed on inspection. First, the note recording that decision says the total effect
is not clearly separated from zero in any of our six pathways, and that is not what our own results
show. The total association excludes zero in all six, and in five of six it sits between four and
nine standard deviations away from zero, which is comfortably inside the range where a ratio is well
behaved. Second, the technical objection about convergence diagnostics has been overtaken by the
literature. The modern rank-based version of the standard convergence statistic was designed
specifically for quantities with heavy tails and works here, which the version we were relying on
did not.

What remains true is that we must report the **median** and never the average, and never a standard
deviation. Medians of ratios are perfectly well defined; averages of them are not. That is a
reporting rule, not a reason to abandon the number.

## What we would actually publish

A prespecified rule, written down before the numbers are looked at, that releases the percentage
only where the total association is clearly separated from zero and where the direct and indirect
paths point the same way. Applied to our results, that rule releases five pathways and withholds
one. The one it withholds is negative schema to delusions, which is our five-study pathway and the
one we have independently flagged as the least secure at every previous stage. I take that as
reassurance that the rule is doing real work rather than being tuned to a convenient answer.

For orientation, our earlier frequentist analysis put these percentages at roughly 58 per cent for
dissociation to hallucinations, 46 per cent for dissociation to delusions, 26 per cent for negative
schema to composite psychosis and 31 per cent for depression to composite psychosis. The Bayesian
figures were close to these, and the intervals were wider, which is the point of doing it this way.
The depression pathway is the clearest illustration: the frequentist interval was 27.5 to 35.1 per
cent, the Bayesian was 14.4 to 57.1 per cent, and the difference is that the frequentist model
assumed the studies did not differ from one another. They do.

## The part I want everyone to read

We cannot give a prediction interval for the percentage, and the reason is the most clinically
important thing in our paper.

A credible interval says how precisely we have estimated an average across the studies we have. A
prediction interval says what the next study is likely to find. In this literature the studies
differ from one another so much that in five of our six pathways the predicted total association in
a new cohort includes zero. If the next cohort might show no trauma-psychosis association at all,
then the question "what percentage of it was mediated" has no answer in that cohort. This is the
same fact as our existing headline finding, that every prediction interval for the indirect effect
includes zero, but expressed in the units you have asked for.

So the percentage describes the average of the published literature. It does not describe the
patient in front of a clinician, and it does not describe the next study. If we put a percentage in
the main table, the sentence after it has to say so, in the abstract as well as the discussion.

## The wording problem

"Percentage of the effect explained" implies causation and implies that removing the mediator would
remove that share of the risk. Our data are pooled cross-sectional correlations. We have no temporal
ordering, no control for confounding between mediator and outcome, and no ability to rule out that
the mediator and the outcome are two facets of the same thing measured twice. I would like us to
write, in the table header and at every mention, **percentage of the pooled trauma-psychosis
association statistically accounted for by the indirect path**. It is clumsy. It is also what we
have. A statistical reviewer at the *BJPsych* will make this point if we do not, and it is much
better made by us.

## What I propose

Report the median percentage and its credible interval for the five pathways that pass the rule,
state which pathway is withheld and why, state that no prediction interval is available and what
that means, and use the longer wording throughout. This costs about half a day of computing and
gives us a number that is genuinely useful without being misleading.

I am happy to be argued out of any part of this. The one part I would resist is putting a bare
percentage in the abstract without the heterogeneity caveat attached to it, because that is the
sentence that will be quoted back at us.
