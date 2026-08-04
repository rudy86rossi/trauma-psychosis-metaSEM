# Release provenance: which commit produced `run_abc9305d0d01`

A release bundle names a commit. This note says exactly which commit produced what, because
for this release the answer is not a single commit, and because the preceding release
carries a defect in the same machinery that a reader should be able to see rather than
discover.

## Three commits

| Commit | Time | What it contributes |
|---|---|---|
| `d12bf4b` | 4 Aug 2026, 01:31:04 BST | The source tree the pipeline ran against. Every one of the 65 files in `bayesian/results/run_abc9305d0d01/` was produced with `HEAD` here, and the manifest was written at 01:32:09 BST. |
| `bb0e4aa` | 4 Aug 2026, 01:39:17 BST | The release commit. Adds the bundle, and changes `bayesian/R/lib_writeup.R` plus the two generated documents it writes. |
| `76ac097` | 4 Aug 2026 | Adds `diagnostics/pmed_draws.csv.gz`, which the manifest already listed but `.gitignore` had excluded. Its bytes are the ones the manifest already recorded. |

The identifier is verifiable rather than asserted. `_targets.R` computes it as the first
twelve hexadecimal digits of

```
sha256( paste(git rev-parse HEAD, data_sha256, config_sha256, stan_sha256, renv_sha256) )
```

and recomputing that from the manifest's own recorded fields returns `abc9305d0d01`. The
identifier, the recorded `source_commit` and the directory name therefore agree: this bundle
is the bundle of the tree at `d12bf4b`.

## What `bb0e4aa` changed after the bundle was promoted, and why it does not touch a number

Outside the bundle, `bb0e4aa` changed three files: `bayesian/R/lib_writeup.R`,
`bayesian/BAYESIAN_METAREG_RESULTS.md` and `bayesian/BAYESIAN_METAREG_METHODS.md`. The
`lib_writeup.R` change is prose templating only. It stops the results document printing
"6 of 6 pathways met the rule and 0 did not" when a template with no zero case is given a
zero, and it prints a readable phrase such as "cohort deletion" in place of the internal
`fit_class` string. Both defects are visible only once a document has been generated from
the promoted bundle, which is why the fix necessarily post-dates the promotion.

Neither change can alter a released quantity: the documents are written *from* the bundle,
never the other way round, and the bundle was not rewritten. That is checked rather than
argued. `check_documents()` reads the committed documents against the committed bundle and
asserts that no figure printed in either document is absent from a source table; it passes
on this pair.

## The accepted exception

A clean checkout of `d12bf4b` reproduces the bundle but regenerates the two documents with
the older prose templates, so it does not reproduce the committed documents. A clean
checkout of `bb0e4aa` has the correct document generator, but a fresh pipeline run from it
would be assigned a different identifier, because the identifier hashes `git rev-parse HEAD`.

So there is no single commit from which the complete public release, bundle and documents
together, regenerates byte for byte. That is accepted for this release rather than repaired,
because repairing it means a full refit under a new identifier, and the supplementary
material already cites `run_abc9305d0d01`. It is recorded here as an exception, not
presented as reproducibility.

What is demonstrated is narrower and worth stating on its own terms: the bundle is
internally consistent and hashes to its manifest; the identifier is the digest of the commit
the manifest names; the only source change between promotion and release is confined to
document prose; and the documents agree with the bundle under the checker that exists for
exactly that purpose.

## The structural cause, and what the guard does and does not fix

Two properties of `_targets.R` combine here.

First, the identifier hashes `HEAD` **at run time**, while the bundle is committed
afterwards. A promotion therefore always names its parent state, and no commit can contain a
bundle whose identifier was computed from its own tree. This is a property of the design.

Second, `analysis_id` captures `git rev-parse HEAD` *inside* the target, where `targets`
cannot see it. `HEAD` is not a tracked dependency, so moving `HEAD` does not invalidate the
identifier, while `build_fit_ledger()` calls `git rev-parse HEAD` afresh every time it runs.
The two can therefore drift apart, and in the preceding release they did:
`run_c9cb1a366dfb` was re-promoted in `d16a1a2` from an updated sample registry, after
`HEAD` had moved to the release commit `3d3bef2`. Its ledger recorded the new `HEAD`; its
identifier stayed cached at the old one. Recomputing the identifier from that manifest's own
fields returns `5f6d468352fe`, not `c9cb1a366dfb`. The bundle's contents are current and it
is in any case superseded by this release, but its manifest is not self-consistent and
should not be cited as evidence of provenance. **`run_abc9305d0d01` does not have this
defect**; the check above is the reason we can say so.

`promote_run()` now refuses to promote from a dirty worktree. That closes the more dangerous
failure, in which released numbers come from source that was never committed. It does **not**
close either property above: it would not have prevented this release's split, because the
worktree was clean when the bundle was promoted and the reporting change came afterwards.
Making `analysis_id` invalidate when `HEAD` moves is the remaining repair and is an open
item.
