# Data, provenance and the correction ledger

British English throughout.

## The rule

**No modified source workbook is committed to this repository.** The only binary
inputs are the upstream extraction workbooks already under `metaSEM/`. Every
departure from them is expressed as a readable row in
`provenance/corrections.csv` or `provenance/quality_scores.csv`, and the build
verifies the upstream hash before applying anything.

This is a deliberate provenance choice, not a convenience. A replacement binary
records that a file changed; a ledger records *which cell*, *from what*, *to
what*, *why*, *on whose authority* and *on what date*.

It reduces but does not remove the governance question. Release of the
corrections, the quality scores and the derived analysis table remains subject
to author and data-governance approval.

## Files

| File | What it holds |
| --- | --- |
| `inputs/analysis_table.csv` | The locked analysis table, canonically serialised. Everything downstream reads this, never a workbook. |
| `provenance/corrections.csv` | One row per corrected cell. Twelve rows. |
| `provenance/quality_scores.csv` | The complete Newcastle-Ottawa record for the negative-schema workbook. Twenty rows. |
| `provenance/sample_map.csv` | **Source table, read by the pipeline.** One row per eligible effect row, asserting which cohort supplied it and what evidence that assertion rests on. |
| `provenance/samples.csv` | **Source table, read by the pipeline.** One row per cohort: the identifier, its status, its lineage and a note. It owns the identifiers and nothing else. |
| `provenance/samples_derived.csv` | Generated. The registry as the pipeline computes it, including the derived verification status. Published for readers; never read back. |
| `provenance/sample_review_queue.csv` | Generated. Cases the map cannot settle, for a human with the papers. |
| `provenance/effects.csv` | One row per pathway, instrument and effect estimate, linked by `sample_id` and `map_id`. |
| `provenance/exclusions.csv` | Studies excluded by explicit decision, with the reason and status. |
| `provenance/instrument_choices.csv` | Samples that supplied more than one eligible instrument, and which was chosen. |
| `provenance/reportability.csv` | Per pathway and moderator: `ok`, `fragile` or `not reported`. |
| `provenance/contrast_support.csv` | Nominal and supported contrasts, spans, priors and SESOIs. |
| `provenance/vz_comparison.csv` | Differences between the three sampling-covariance constructions. |
| `provenance/literature_manifest.csv` | Citations and DOIs in place of journal PDFs. |

## The analytical unit

The analytical unit is one participant cohort, not one paper and not one row.
Identity is **asserted** in `sample_map.csv` and is never derived from text.

`paste(studyID, year)` survives only as a report label. It used to be the
identifier, minted in `lib_data.R`, used immediately to group rows and demote
duplicates by instrument priority, and only then joined to `samples.csv`. Three
things followed from that ordering. One report could never map to two cohorts,
because the join was many-to-one on a string. Two reports could never map to one
cohort, because the registry held one row per string. And both structural
collision gates were unreachable, because the demotion had already guaranteed one
primary row per pathway and key before either could run.

`sample_map.csv` has one row per eligible effect row, keyed on
`source_effect_id`: the workbook, sheet, row and pathway the estimate came from.
Position, not content, for the same reason a correction is addressed by its
cell. A semantic key cannot separate two cohorts that agree on pathway, author,
year and instrument, which is precisely the case this exists to handle. The row
fingerprint travels with the assertion and is re-verified on every build, so a
workbook revision that moves the ground under a curated decision stops the build
instead of silently reassigning identity.

`multiplicity` declares what a repeated cohort means: `single`,
`alternative_instrument` (one cohort measured with two instruments, which is what
licenses instrument-priority selection), or `superseded_report`. Two rows that
merely collide are left alone, so the collision gate can see them.

`samples.csv` owns the identifiers. They are never deleted and never reused: a
cohort that leaves the analysis is marked `retired` and its `predecessor_ids` and
`successor_ids` record the lineage, because a `fit_id` such as
`loso_stan|negsch_del|clinical|S026` must keep meaning one cohort across runs. If
curation ever splits or merges a cohort, the affected identifier is retired
rather than quietly repointed.

**Verification lives on the mapping row and nowhere else.** `samples.csv` has no
verification column to edit; the cohort-level status is derived, and a cohort
counts as verified only when every report mapped to it has been evidenced. Two
files therefore cannot disagree about whether the same thing was checked.

Evidence must be a full-text page, table or recruitment passage, a registry
record, or correspondence, and it must carry a locator another reader can turn
to. A DOI, an abstract and a matching participant count are **not** admissible:
none of them establishes that two reports describe the same people. Who prepared
the research and who accepted it are recorded separately, and research prepared
by a machine is labelled as such. `author_confirmation` is reserved for
confirmation from the original study's authors.

**Structural contradictions stop the build.** These are cases where the data or
the assertions disagree with themselves and no judgement can rescue them:

- an effect row with no curated mapping, or a mapping row matching no effect row;
- a source row whose fingerprint has changed under its assertion;
- two live rows in one pathway on one `sample_id` with nothing declaring why;
- one cohort carrying two different participant counts;
- a declared cohort nothing maps to, or a retired identifier still in use;
- an instrument tie that the configured priority does not separate;
- a Newcastle-Ottawa value that is not an integer in range, or has no authority;
- a duplicated correction;
- a row marked `evidenced` without an admissible type, a checkable locator, an
  acceptor and a date at or before today.

**Judgement calls are surfaced, not silently resolved.**
`sample_review_queue.csv` is regenerated every run and names each case a human
needs to settle: cohorts spanning two source workbooks, labels whose punctuation
will not round-trip, same-author pairs in adjacent years, merges and splits
asserted but not yet evidenced, and superseded reports that disagree about how
many people there were. An item clears once the evidence answers it, so the queue
does not keep asking a settled question.

Most rows remain `unevidenced`. Until they are checked, nothing in this release
may describe the analytical unit as independently verified, and
`check_documents()` enforces that: it requires the generated caveat verbatim, and
it fails any unqualified assertion about sample independence in the surrounding
prose. See `bayesian/docs/sample_identity_verification.md` for what has been
checked and what has not.

## What the corrections are

Thirty data-value changes in total, plus one provenance note.

| Workbook | Change | Count |
| --- | --- | ---: |
| dissociation | `sample` reclassified "Mixed" to "Clinical" (Evans 2015, Khosravi 2021, Levi 2025) | 10 |
| negative schema | `NOS` populated from the supplementary material | 19 |
| negative schema | LoPilato 2021 `%_female` 0.395 to 39.5 | 1 |
| depression | none; byte-identical to upstream | 0 |

The ten reclassifications change provenance only. None of the three studies
enters an analysed pathway under the current inclusion rules: Evans is excluded
as incommensurate, Levi 2025 reports no complete correlation triplet, and
Khosravi 2021 is `trauma_type = abuse` with non-composite dissociation domains.
A regression test in `bayesian/tests/testthat/test-data-lock.R` asserts that
they leave every k and every reportability guard untouched, so that if an
inclusion rule later changes, the consequence appears as a test failure rather
than as a silent shift.

## Newcastle-Ottawa scores

The nineteen supplied scores come from the manuscript supplementary material,
section "Quality assessment procedures": the ten-point Newcastle-Ottawa Scale
for cohort studies, scored by two independent reviewers (GK and IP), with
disagreements resolved at a project group meeting. `quality_scores.csv` records
that instrument, those assessors, the consensus procedure and the source
document for every score.

The table is the **complete** record for that workbook, not only the changed
cells, so a score already present upstream (Morgan 2014) is included and the
build's conflict check runs against a real value. A disagreement between the
workbook and the quality table is an error, never a silent overwrite.

These scores matter disproportionately: every Newcastle-Ottawa value used in
both negative-schema pathways comes from this table.

## Regenerating the ledger

`bayesian/tools/derive_provenance.R` regenerates both CSVs by comparing a
corrected workbook with the upstream one. It is a **maintainer tool** and needs
the corrected workbooks, which are not in this repository. The release pipeline
never runs it.

```sh
Rscript bayesian/tools/derive_provenance.R /path/to/corrected/workbooks
```

Rerun it only when the authors supply a revised workbook, then review the diff
before committing. The build will refuse to proceed if a ledger row no longer
addresses exactly one cell holding exactly the value it claims to replace.

## The comparison that must not be used

Deriving a ledger by cell comparison requires an NA-aware difference. The
obvious formulation

```r
which(!(x == y | (is.na(x) & is.na(y))))            # WRONG
```

evaluates to `NA` when a blank becomes a value, and `which()` drops `NA`
silently. Every populated blank disappears. That defect concealed nineteen
Newcastle-Ottawa scores during the audit of this analysis. `cells_differ()` in
`bayesian/R/lib_provenance.R` is the correct form and is covered by a test.
