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
| `provenance/samples.csv` | **Source table, read by the pipeline.** One row per independent participant sample, with its verification status. |
| `provenance/sample_review_queue.csv` | Generated. Cases the derivation cannot settle, for a human with the papers. |
| `provenance/effects.csv` | One row per pathway, instrument and effect estimate, linked by `sample_id`. |
| `provenance/exclusions.csv` | Studies excluded by explicit decision, with the reason and status. |
| `provenance/instrument_choices.csv` | Samples that supplied more than one eligible instrument, and which was chosen. |
| `provenance/reportability.csv` | Per pathway and moderator: `ok`, `fragile` or `not reported`. |
| `provenance/contrast_support.csv` | Nominal and supported contrasts, spans, priors and SESOIs. |
| `provenance/vz_comparison.csv` | Differences between the three sampling-covariance constructions. |
| `provenance/literature_manifest.csv` | Citations and DOIs in place of journal PDFs. |

## The analytical unit

The analytical unit is one independent participant sample, not one paper and not
one row. `samples.csv` is the registry that records it, and the pipeline **reads**
it: it owns the identifiers, so they stay stable when a workbook is revised, and
a `fit_id` such as `loso_stan|negsch_del|clinical|S026` keeps meaning the same
study across runs.

Sample identity is still **derived**, from `paste(studyID, year)`. That is a
heuristic. It cannot separate two cohorts published by one author in one year,
and it cannot recognise one cohort appearing under two labels. The registry does
not pretend otherwise. It does two things instead.

**Structural contradictions stop the build.** These are cases where the data
disagree with themselves and no judgement can rescue them:

- an effect row that maps to no declared `sample_id`;
- one `sample_id` carrying two different participant counts, which means it is
  either two samples or a transcription error;
- two primary rows in one pathway sharing an author-year key;
- a Newcastle-Ottawa value that is not an integer in range, or has no authority;
- a duplicated correction;
- a row marked `verified` with no `verified_by` and `verified_date`.

**Judgement calls are surfaced, not silently resolved.** `sample_review_queue.csv`
is regenerated every run and names each case an author needs to settle: keys
spanning two source workbooks, labels whose punctuation will not round-trip, and
same-author pairs in adjacent years.

Every row is currently `derived_unverified`. Until an author signs them off,
nothing in this release may describe the analytical unit as verified, and
`check_documents()` enforces that: the stronger wording fails the build while any
row remains unverified.

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
