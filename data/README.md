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
| `provenance/samples.csv` | One row per independent participant sample. |
| `provenance/effects.csv` | One row per pathway, instrument and effect estimate, linked by `sample_id`. |
| `provenance/exclusions.csv` | Studies excluded by explicit decision, with the reason and status. |
| `provenance/instrument_choices.csv` | Samples that supplied more than one eligible instrument, and which was chosen. |
| `provenance/reportability.csv` | Per pathway and moderator: `ok`, `fragile` or `not reported`. |
| `provenance/contrast_support.csv` | Nominal and supported contrasts, spans, priors and SESOIs. |
| `provenance/vz_comparison.csv` | Differences between the three sampling-covariance constructions. |
| `provenance/literature_manifest.csv` | Citations and DOIs in place of journal PDFs. |

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
Rscript --vanilla bayesian/tools/derive_provenance.R /path/to/corrected/workbooks
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
