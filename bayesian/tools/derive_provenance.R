# =============================================================================
# tools/derive_provenance.R
# Maintainer tool. Regenerates data/provenance/corrections.csv and
# data/provenance/quality_scores.csv by comparing the corrected workbooks with
# the upstream workbooks committed under metaSEM/.
#
#   Rscript --vanilla bayesian/tools/derive_provenance.R /path/to/corrected/dir
#
# It requires the corrected workbooks, which are NOT part of this repository.
# The release pipeline never runs this: it consumes the committed CSVs. Rerun it
# only when the authors supply a revised workbook, then review the diff.
#
# Newcastle-Ottawa totals are routed to quality_scores.csv rather than
# corrections.csv, because they are measurements supplied from the supplementary
# material rather than transcription fixes, and they carry their own assessors,
# procedure and source.
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("Usage: derive_provenance.R <directory holding the corrected workbooks>")
CORR_DIR <- normalizePath(args[1], mustWork = TRUE)

ROOT <- normalizePath(".")
if (!dir.exists(file.path(ROOT, "metaSEM")))
  stop("Run from the repository root, the directory containing metaSEM/.")
source(file.path(ROOT, "bayesian", "R", "lib_provenance.R"))

WORKBOOKS <- list(
  dissociation = list(
    upstream = file.path(ROOT, "metaSEM", "dissociation",
                         "metanalisis mediation dissociation workcopy.xlsx"),
    sheet    = "Extraction",
    ## Not unique (Evans 2015 occupies six rows) but identical row for row, so
    ## alignment is positional and verified as such.
    align_cols = c("studyID", "year", "psychosis_construct", "dissociation_domain",
                   "dissociation_instrument", "n", "r_XM", "r_MY", "r_XY")),
  negative_schema = list(
    upstream = file.path(ROOT, "metaSEM", "negative_schemata",
                         "Negative schemas mediation_workcopy.xlsx"),
    sheet    = "data",
    ## Re-sorted relative to upstream, but author and year are unique here.
    align_cols = c("study_author", "year")),
  depression = list(
    upstream = file.path(ROOT, "metaSEM", "depression", "depression_workcopy.xlsx"),
    sheet    = 1,
    align_cols = c("Authors, et al.", "year"))
)

## Why each class of change was made, and on whose authority. Anything not
## matched here raises, so a new kind of correction cannot slip through
## undocumented.
REASONS <- list(
  sample = list(
    reason = "Sample type reclassified from Mixed to Clinical on review of the source report.",
    evidence_source = "Primary report re-read; recorded in bayesian/docs/source_audit_evans_uyan_blose.md",
    authority = "manuscript authors"),
  `%_female` = list(
    reason = "Female composition recorded as a proportion rather than a percentage.",
    evidence_source = "LoPilato 2021 reports 210 women among n = 531, that is 39.5 per cent.",
    authority = "manuscript authors"),
  note = list(
    reason = "Provenance note added alongside a corrected value. Documentation only, no analysed value changes.",
    evidence_source = "Correction ledger",
    authority = "manuscript authors")
)

DECISION_DATE <- "2026-07-30"

NOS_META <- list(
  instrument = "Newcastle-Ottawa Scale (cohort, 10-point)",
  assessors = "GK, IP",
  consensus_procedure = "Two independent reviewers; disagreements resolved at a project group meeting.",
  evidence_source = "Drft SM FINAL.docx, section 'Quality assessment procedures'",
  authority = "manuscript authors")

corrections <- list(); quality <- list()

for (nm in names(WORKBOOKS)) {
  w <- WORKBOOKS[[nm]]
  corrected <- file.path(CORR_DIR, basename(w$upstream))
  if (!file.exists(corrected)) stop("Corrected workbook not found: ", corrected)

  up_sha <- sha256_file(w$upstream)
  cat(sprintf("%-16s upstream %s\n%-16s corrected %s\n", nm, substr(up_sha, 1, 16),
              "", substr(sha256_file(corrected), 1, 16)))

  d <- derive_corrections(w$upstream, corrected, w$sheet, w$align_cols)
  if (!length(d) || !nrow(d)) { cat("  no differences\n"); next }
  d$source_blob_sha <- up_sha

  ## Route NOS to the quality-score table, everything else to the ledger.
  ## The quality table records EVERY non-missing score in the workbook, not only
  ## the changed cells, so that it is the complete Newcastle-Ottawa record for
  ## that workbook and the conflict check runs against values already upstream.
  is_nos <- d$column == "NOS"
  if (any(is_nos)) {
    ## Work in upstream row indexing throughout: d$row_index refers to upstream,
    ## while the corrected workbook may be sorted differently.
    up  <- read_sheet(w$upstream, w$sheet)
    idc <- if (nm == "negative_schema") "study_author" else "studyID"
    nos <- as.numeric(up$NOS)
    nos[d$row_index[is_nos]] <- as.numeric(d$new_value[is_nos])
    keep <- which(!is.na(nos))
    q <- data.frame(
      workbook    = basename(w$upstream),
      study_label = trimws(as.character(up[[idc]][keep])),
      year        = as.integer(up$year[keep]),
      nos_total   = nos[keep],
      supplied    = keep %in% d$row_index[is_nos],
      stringsAsFactors = FALSE)
    q <- cbind(q, as.data.frame(NOS_META, stringsAsFactors = FALSE),
               decision_date = DECISION_DATE)
    quality[[nm]] <- q
    cat("  ", nrow(q), "quality scores (", sum(q$supplied), "newly supplied )\n")
  }

  d <- d[!is_nos, , drop = FALSE]
  if (nrow(d)) {
    unknown <- setdiff(unique(d$column), names(REASONS))
    if (length(unknown))
      stop("No documented reason for corrections to column(s): ",
           paste(unknown, collapse = ", "), ". Add them to REASONS.")
    meta <- do.call(rbind, lapply(d$column, function(cn)
      as.data.frame(REASONS[[cn]], stringsAsFactors = FALSE)))
    d <- cbind(d, meta, decision_date = DECISION_DATE)
    corrections[[nm]] <- d
    cat("  ", nrow(d), "corrections routed to corrections.csv\n")
  }
}

CORR <- do.call(rbind, corrections)
CORR$correction_id <- sprintf("C%03d", seq_len(nrow(CORR)))
CORR <- CORR[, c("correction_id", "source_blob_sha", "workbook", "sheet", "cell_address",
                 "row_index", "column", "row_fingerprint", "expected_old", "new_value",
                 "reason", "evidence_source", "authority", "decision_date")]

QUAL <- do.call(rbind, quality)
QUAL$quality_id <- sprintf("Q%03d", seq_len(nrow(QUAL)))
QUAL <- QUAL[, c("quality_id", "workbook", "study_label", "year", "nos_total", "supplied",
                 "instrument", "assessors", "consensus_procedure", "evidence_source",
                 "authority", "decision_date")]

PROV <- file.path(ROOT, "data", "provenance")
dir.create(PROV, showWarnings = FALSE, recursive = TRUE)
write.csv(CORR, file.path(PROV, "corrections.csv"), row.names = FALSE, na = "")
write.csv(QUAL, file.path(PROV, "quality_scores.csv"), row.names = FALSE, na = "")

cat("\nWrote", nrow(CORR), "corrections and", nrow(QUAL), "quality scores.\n")
print(table(CORR$workbook, CORR$column))
