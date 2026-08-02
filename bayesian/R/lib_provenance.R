# =============================================================================
# lib_provenance.R
# Source hashing, the correction ledger and the quality-score table.
#
# The upstream workbooks under metaSEM/ are the only binary inputs committed to
# this repository. Every departure from them is expressed as a row in
# data/provenance/corrections.csv or data/provenance/quality_scores.csv, so the
# locked analysis table is reconstructed from public inputs plus a readable
# ledger rather than from a replacement binary.
#
# A correction is addressed by its exact cell. Author and year are not enough:
# Evans 2015 occupies six rows of the dissociation workbook that agree on
# author, year, pathway, dissociation domain and instrument. Application
# therefore locates the cell, then verifies both the row fingerprint and the
# declared old value before writing. Any mismatch is fatal.
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(digest)
})

## ---- hashing ---------------------------------------------------------------

sha256_file <- function(path) digest::digest(file = path, algo = "sha256")

sha256_string <- function(x) digest::digest(x, algo = "sha256", serialize = FALSE)

#' Excel column letter from a 1-based column index.
excel_col_letter <- function(i) {
  out <- character(length(i))
  for (k in seq_along(i)) {
    n <- i[k]; s <- ""
    while (n > 0) {
      r <- (n - 1) %% 26
      s <- paste0(LETTERS[r + 1], s)
      n <- (n - 1) %/% 26
    }
    out[k] <- s
  }
  out
}

#' Canonical string for one value, used by fingerprints and by the ledger's
#' expected_old / new_value fields. NA is the empty string, numerics are written
#' at 17 significant digits so that a round trip through text is lossless.
canon_value <- function(x) {
  if (length(x) != 1) x <- x[[1]]
  if (is.na(x)) return("")
  ## %.17g round-trips an IEEE double exactly and, unlike formatC, does not pad.
  if (is.numeric(x)) return(sprintf("%.17g", x))
  trimws(as.character(x))
}

#' SHA-256 of a whole data row, over every column in file order.
row_fingerprint <- function(df, i) {
  sha256_string(paste(vapply(df[i, , drop = FALSE], canon_value, character(1)),
                      collapse = ""))
}

## ---- reading ---------------------------------------------------------------

#' Read one sheet with types left to readxl but column names preserved exactly.
read_sheet <- function(path, sheet = 1) {
  as.data.frame(readxl::read_excel(path, sheet = sheet), stringsAsFactors = FALSE)
}

## ---- deriving the ledger ---------------------------------------------------
## Maintainer-side only. Requires the corrected workbook, which is not committed.
## The release pipeline consumes the resulting CSV and never calls this.

#' NA-aware cell-level difference between two versions of one sheet.
#'
#' The naive comparison `which(!(x == y | (is.na(x) & is.na(y))))` returns NA for
#' a blank-to-value change and `which()` silently drops NA, so every populated
#' blank disappears. That defect hid nineteen NOS scores during the audit of this
#' analysis. The comparison below evaluates to TRUE in that case.
cells_differ <- function(x, y) {
  x <- vapply(seq_along(x), function(i) canon_value(x[i]), character(1))
  y <- vapply(seq_along(y), function(i) canon_value(y[i]), character(1))
  x != y
}

#' Derive ledger rows by comparing an upstream sheet with a corrected one.
#'
#' Row alignment is established before any cell is compared, because a corrected
#' workbook is sometimes re-sorted and sometimes not. `align_cols` must be
#' columns the correction does not touch. If they already agree row for row the
#' order is positional; otherwise they must form a unique key and rows are
#' matched on it. Neither holding is an error, not a silent guess.
derive_corrections <- function(upstream_path, corrected_path, sheet, align_cols) {
  up <- read_sheet(upstream_path, sheet)
  co <- read_sheet(corrected_path, sheet)

  if (!identical(dim(up), dim(co)))
    stop("Upstream and corrected sheets differ in shape: ",
         paste(dim(up), collapse = "x"), " against ", paste(dim(co), collapse = "x"))
  if (!identical(names(up), names(co)))
    stop("Upstream and corrected sheets differ in columns.")

  mk <- function(d) apply(d[, align_cols, drop = FALSE], 1,
                          function(r) paste(vapply(r, canon_value, character(1)), collapse = "|"))
  ku <- mk(up); kc <- mk(co)

  if (identical(ku, kc)) {
    alignment <- "positional"
  } else if (!anyDuplicated(ku) && setequal(ku, kc)) {
    co <- co[match(ku, kc), , drop = FALSE]
    alignment <- "keyed"
  } else {
    stop("align_cols neither agree row for row nor form a unique key; ",
         "cannot align ", basename(upstream_path), " sheet ", sheet, ".")
  }
  message("  row alignment: ", alignment)

  out <- list()
  for (j in seq_along(up)) {
    d <- which(cells_differ(up[[j]], co[[j]]))
    for (i in d) {
      out[[length(out) + 1]] <- data.frame(
        workbook      = basename(upstream_path),
        sheet         = as.character(sheet),
        cell_address  = paste0(excel_col_letter(j), i + 1L),   # +1 for the header row
        row_index     = i,
        column        = names(up)[j],
        row_fingerprint = row_fingerprint(up, i),
        expected_old  = canon_value(up[[j]][i]),
        new_value     = canon_value(co[[j]][i]),
        stringsAsFactors = FALSE)
    }
  }
  if (!length(out)) return(out[0])
  do.call(rbind, out)
}

## ---- applying the ledger ---------------------------------------------------

#' Apply committed corrections to an upstream sheet.
#'
#' Fails, rather than warns, if the source hash is wrong, if a correction does
#' not address exactly one cell, if the row fingerprint has moved, or if the
#' value found is not the value the ledger says it is replacing.
apply_corrections <- function(df, ledger, workbook, sheet, source_sha) {
  rows <- ledger[ledger$workbook == workbook & ledger$sheet == as.character(sheet), , drop = FALSE]
  if (!nrow(rows)) return(df)

  bad_sha <- unique(rows$source_blob_sha[rows$source_blob_sha != source_sha])
  if (length(bad_sha))
    stop("Correction ledger was derived against a different ", workbook,
         ". Ledger expects ", paste(bad_sha, collapse = ", "), ", found ", source_sha, ".")

  ## Fingerprints and expected values are checked against the PRISTINE sheet.
  ## Two corrections can target the same row (a value and its provenance note),
  ## and applying the first would otherwise invalidate the second's fingerprint.
  df0 <- df

  for (i in seq_len(nrow(rows))) {
    r  <- rows[i, ]
    ri <- r$row_index
    if (is.na(ri) || ri < 1 || ri > nrow(df))
      stop("Correction ", r$correction_id, " addresses row ", ri,
           " which is outside ", workbook, " (", nrow(df), " rows).")
    if (!r$column %in% names(df))
      stop("Correction ", r$correction_id, " addresses column '", r$column,
           "' which is absent from ", workbook, ".")

    fp <- row_fingerprint(df0, ri)
    if (!identical(fp, r$row_fingerprint))
      stop("Correction ", r$correction_id, " targets ", workbook, " cell ",
           r$cell_address, " but that row's fingerprint has changed. ",
           "Expected ", substr(r$row_fingerprint, 1, 12), ", found ", substr(fp, 1, 12),
           ". Re-derive the ledger.")

    found <- canon_value(df0[[r$column]][ri])
    if (!identical(found, r$expected_old))
      stop("Correction ", r$correction_id, " expected ", workbook, " cell ",
           r$cell_address, " to hold '", r$expected_old, "' but found '", found, "'.")

    df[[r$column]][ri] <- if (is.numeric(df[[r$column]]))
      as.numeric(r$new_value) else r$new_value
  }
  df
}

#' Apply the quality-score table, which supplies Newcastle-Ottawa totals that the
#' upstream workbook leaves blank.
#'
#' Quality scores take precedence over the workbook, but a disagreement between
#' the two is an error rather than a silent overwrite: it means one of the two
#' records is wrong and a human must decide which.
apply_quality_scores <- function(df, qs, workbook, id_col, year_col, nos_col = "NOS") {
  rows <- qs[qs$workbook == workbook, , drop = FALSE]
  if (!nrow(rows)) return(df)
  if (!nos_col %in% names(df)) stop("No '", nos_col, "' column in ", workbook, ".")

  key_df <- paste(trimws(as.character(df[[id_col]])), as.integer(df[[year_col]]))
  for (i in seq_len(nrow(rows))) {
    r  <- rows[i, ]
    hit <- which(key_df == paste(trimws(r$study_label), as.integer(r$year)))
    if (length(hit) != 1L)
      stop("Quality score for ", r$study_label, " ", r$year, " matches ",
           length(hit), " rows of ", workbook, "; expected exactly one.")
    existing <- df[[nos_col]][hit]
    if (!is.na(existing) && !isTRUE(all.equal(as.numeric(existing), as.numeric(r$nos_total))))
      stop("Quality score conflict for ", r$study_label, " ", r$year, ": ",
           workbook, " holds ", existing, ", quality_scores.csv holds ", r$nos_total,
           ". Resolve before proceeding.")
    df[[nos_col]][hit] <- as.numeric(r$nos_total)
  }
  df
}

## ---- curated sample identity -----------------------------------------------
#
# The analytical unit is one independent participant sample. Identity is
# ASSERTED, in data/provenance/sample_map.csv, and is never derived from text.
#
# The previous design minted `paste(studyID, year)` in lib_data.R, grouped rows
# by it, demoted duplicates by instrument priority, and only then joined
# samples.csv with match(). Three consequences followed. One report could never
# map to two cohorts, because the join was many-to-one. Two reports could never
# map to one cohort, because the registry held one row per author-year string.
# And both structural collision gates were unreachable, because the demotion had
# already guaranteed one primary row per (pathway, key) before either ran.
#
# The map is keyed on `source_effect_id`, the position of a row in its source
# workbook, for the same reason a correction is addressed by its cell: a
# semantic key cannot separate two cohorts agreeing on pathway, author, year and
# instrument, which is precisely the case this exists to handle. The row
# fingerprint travels with the assertion and is re-verified on every build, so a
# workbook revision that moves the ground under an assertion stops the build
# rather than silently reassigning identity.
#
# Verification lives on the mapping row and nowhere else. samples.csv DERIVES a
# cohort-level status from those rows and is never read back for it, so two
# fields cannot disagree about whether the same thing was checked.
#
# Two kinds of problem stay separate. STRUCTURAL failures stop the build: the
# data or the assertions contradict themselves and no judgement rescues them.
# REVIEW items do not stop the build: they are questions about the world that
# only a human with the papers can settle, so they are surfaced by name.

SAMPLE_REGISTRY_COLS <- c("sample_id", "canonical_label", "sample_status",
                          "predecessor_ids", "successor_ids", "study_label",
                          "year", "n_reported", "n_reports", "report_keys",
                          "pathways", "verification_status", "note")

## Authoritative in the file, read back and obeyed.
SAMPLE_REGISTRY_OWNED <- c("sample_id", "canonical_label", "sample_status",
                           "predecessor_ids", "successor_ids", "note")

SAMPLE_MAP_COLS <- c("map_id", "source_effect_id", "workbook", "sheet", "source_row",
                     "pathway", "studyID", "year", "instrument", "n", "row_fingerprint",
                     "sample_id", "multiplicity", "mapping_basis", "evidence_status",
                     "evidence_type", "evidence_locator", "evidence_prepared_by",
                     "accepted_by", "accepted_date", "note")

VERIFICATION_STATUSES <- c("derived_unverified", "verified", "disputed")
SAMPLE_STATUSES       <- c("in_analysis", "retired")
MULTIPLICITIES        <- c("single", "alternative_instrument", "superseded_report")
MAPPING_BASES         <- c("author_year_default", "curated_confirm",
                           "curated_merge", "curated_split")

## What may stand as evidence that a report describes a given cohort.
##
## A DOI, an abstract and a matching participant count are deliberately absent.
## None of them establishes that two reports describe the same people or that
## one report describes two cohorts, which is the only question the map exists
## to answer. A matching N is corroboration and belongs in `note`.
EVIDENCE_STATUSES <- c("unevidenced", "evidenced", "disputed")
EVIDENCE_TYPES    <- c("fulltext", "correspondence", "author_confirmation", "registry")

## A locator has to be checkable by someone else. "fulltext: yes" and "checked
## the paper" are not locators. Two forms qualify. A NUMBERED locator names a
## page, table, figure, numbered section, appendix or supplement. A NAMED
## locator names the structural section of a report in which a sample is
## described, which is what a recruitment passage actually is: journal articles
## do not number their sections, so requiring a number would have excluded the
## commonest admissible evidence and pushed curators towards vaguer wording.
LOCATOR_DOCUMENT <- paste0("(\\bpp?\\.?\\s*[0-9]+|\\bpages?\\s*[0-9]+|\\btable\\s*[0-9A-Za-z]+",
                           "|\\bfigure\\s*[0-9A-Za-z]+|\\bfig\\.?\\s*[0-9A-Za-z]+",
                           "|\\bsection\\s*[0-9A-Za-z.]+|\\bappendix\\b|\\bsupplement",
                           "|\\bmethods?\\b|\\bparticipants?\\b|\\bsubjects?\\b|\\bsample\\b",
                           "|\\brecruit(ment|ed)?\\b|\\bprocedure\\b|\\bmeasures?\\b",
                           "|\\bmaterials?\\b|\\bresults?\\b)")
LOCATOR_CONTACT  <- "(e-?mail|letter|personal communication|correspondence)"

#' The identifier of one row of one source workbook, in one pathway.
#'
#' Position, not content. Two cohorts can agree on pathway, author, year and
#' instrument; they cannot occupy the same spreadsheet row. `pathway` is carried
#' even though every row currently feeds exactly one pathway, so that a future
#' filter which routes one row to two pathways cannot make the key ambiguous.
source_effect_id <- function(workbook, sheet, source_row, pathway)
  sprintf("%s|%s|%d|%s", workbook, as.character(sheet), as.integer(source_row), pathway)

#' Read the curated map. Its absence is fatal, not a licence to derive identity.
read_sample_map <- function(path = "data/provenance/sample_map.csv") {
  if (!file.exists(path))
    stop("The curated sample map is missing: ", path,
         ". Sample identity is asserted, never inferred from author and year. ",
         "Seed it with Rscript bayesian/tools/seed_sample_map.R, then curate it.")
  m <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "",
                       colClasses = "character")
  m$source_row <- as.integer(m$source_row)
  m$year       <- as.integer(m$year)
  m$n          <- as.numeric(m$n)
  m
}

#' Read the cohort registry. Absence is fatal for the same reason.
read_sample_registry <- function(path = "data/provenance/samples.csv") {
  if (!file.exists(path))
    stop("The cohort registry is missing: ", path,
         ". Seed it with Rscript bayesian/tools/seed_sample_map.R.")
  ## Only the owned columns are read back; the rest are derived every run and
  ## republished to samples_derived.csv, so nothing here is coerced on trust.
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "")
}

blank <- function(x) is.na(x) | !nzchar(trimws(x))

#' File-level validation of the curated map and the registry it points into.
#'
#' Everything here can be checked without a workbook, so it runs before a single
#' row is read, and a curator gets one error naming the row rather than a
#' downstream failure naming a matrix.
validate_sample_map <- function(smap, reg,
                                smap_path = "data/provenance/sample_map.csv",
                                reg_path  = "data/provenance/samples.csv") {
  miss <- setdiff(SAMPLE_MAP_COLS, names(smap))
  if (length(miss))
    stop(smap_path, " is missing required column(s): ", paste(miss, collapse = ", "), ".")
  miss <- setdiff(SAMPLE_REGISTRY_OWNED, names(reg))
  if (length(miss))
    stop(reg_path, " is missing required column(s): ", paste(miss, collapse = ", "), ".")

  if (anyDuplicated(smap$map_id))
    stop("Duplicate map_id in ", smap_path, ": ",
         paste(unique(smap$map_id[duplicated(smap$map_id)]), collapse = ", "), ".")
  if (anyDuplicated(smap$source_effect_id))
    stop("Two mapping rows address the same source row in ", smap_path, ": ",
         paste(unique(smap$source_effect_id[duplicated(smap$source_effect_id)]),
               collapse = ", "), ".")
  if (anyDuplicated(reg$sample_id))
    stop("Duplicate sample_id in ", reg_path, ": ",
         paste(unique(reg$sample_id[duplicated(reg$sample_id)]), collapse = ", "), ".")

  chk <- function(x, allowed, field, path) {
    bad <- setdiff(unique(x), allowed)
    if (length(bad))
      stop("Unknown ", field, " in ", path, ": ", paste(bad, collapse = ", "),
           ". Allowed: ", paste(allowed, collapse = ", "), ".")
  }
  chk(smap$multiplicity,    MULTIPLICITIES,    "multiplicity",    smap_path)
  chk(smap$mapping_basis,   MAPPING_BASES,     "mapping_basis",   smap_path)
  chk(smap$evidence_status, EVIDENCE_STATUSES, "evidence_status", smap_path)
  chk(stats::na.omit(smap$evidence_type), EVIDENCE_TYPES, "evidence_type", smap_path)
  chk(reg$sample_status,    SAMPLE_STATUSES,   "sample_status",   reg_path)

  undeclared <- setdiff(smap$sample_id, reg$sample_id)
  if (length(undeclared))
    stop(length(undeclared), " mapping row(s) point at a sample_id that ", reg_path,
         " does not declare: ", paste(utils::head(undeclared, 5), collapse = ", "),
         ". Declare the cohort before a report can be mapped to it.")

  ## An assertion of evidence has to name what was read and who accepted it.
  ## Otherwise "evidenced" is a word rather than a claim someone is answerable
  ## for, which is the failure mode this whole file exists to prevent.
  ev <- smap[smap$evidence_status == "evidenced", , drop = FALSE]
  if (nrow(ev)) {
    bad <- ev$map_id[blank(ev$evidence_type) | blank(ev$evidence_locator) |
                       blank(ev$accepted_by) | blank(ev$accepted_date)]
    if (length(bad))
      stop("Mapping row(s) marked evidenced without evidence_type, ",
           "evidence_locator, accepted_by and accepted_date: ",
           paste(bad, collapse = ", "), ".")

    d <- suppressWarnings(as.Date(ev$accepted_date, format = "%Y-%m-%d"))
    bad <- ev$map_id[is.na(d) | d > Sys.Date()]
    if (length(bad))
      stop("Mapping row(s) with an accepted_date that is not an ISO date at or ",
           "before today: ", paste(bad, collapse = ", "), ".")

    doc <- ev$evidence_type %in% c("fulltext", "registry")
    bad <- ev$map_id[doc & !grepl(LOCATOR_DOCUMENT, ev$evidence_locator, ignore.case = TRUE)]
    if (length(bad))
      stop("Mapping row(s) citing a document without a checkable locator: ",
           paste(bad, collapse = ", "),
           ". Name a page, table, figure, section, appendix or supplement.")

    ## author_confirmation means the study's own authors confirmed it. Anything
    ## else that happens to arrive by email is `correspondence`.
    con <- ev$evidence_type %in% c("correspondence", "author_confirmation")
    bad <- ev$map_id[con & !grepl(LOCATOR_CONTACT, ev$evidence_locator, ignore.case = TRUE)]
    if (length(bad))
      stop("Mapping row(s) citing correspondence without identifying it: ",
           paste(bad, collapse = ", "),
           ". Name the exchange, for example correspondence: author e-mail 2026-08-04.")
  }

  ## Evidence is prepared and accepted by different parties, and research
  ## prepared by a machine is labelled as such wherever it is relied on.
  bad <- smap$map_id[smap$evidence_status != "unevidenced" & blank(smap$evidence_prepared_by)]
  if (length(bad))
    stop("Mapping row(s) carrying evidence without recording who prepared it: ",
         paste(bad, collapse = ", "), ".")

  invisible(TRUE)
}

#' Attach the curated identity to one pathway's effect rows.
#'
#' Runs BEFORE instrument selection. That ordering is the whole repair: the
#' previous code chose a primary instrument first and asked who the participants
#' were afterwards, at which point the answer could no longer change anything.
apply_sample_map <- function(df, smap, path = "data/provenance/sample_map.csv") {
  key <- df$pathway[1]
  j   <- match(df$source_effect_id, smap$source_effect_id)

  none <- which(is.na(j))
  if (length(none))
    stop(key, ": ", length(none), " eligible effect row(s) have no curated mapping in ",
         path, ". Identity is asserted, not inferred, so the build stops here. Add:\n  ",
         paste(sprintf("<map_id>,%s,%s,%s,%d,%s,%s,%d,%s,%g,%s,<sample_id>,single,curated_confirm,unevidenced",
                       df$source_effect_id[none], df$source_workbook[none],
                       df$source_sheet[none], df$source_row[none], key,
                       df$studyID[none], df$year[none],
                       ifelse(is.na(df$instrument[none]), "", df$instrument[none]),
                       df$n[none], df$row_fingerprint[none]),
               collapse = "\n  "))

  ## The fingerprint is the fail-loud half of the key. The row index keeps an
  ## assertion attached to its row when a cell is corrected; the fingerprint
  ## makes sure the row is still the row the curator looked at.
  moved <- which(!is.na(smap$row_fingerprint[j]) &
                   nzchar(smap$row_fingerprint[j]) &
                   smap$row_fingerprint[j] != df$row_fingerprint)
  if (length(moved))
    stop(key, ": the source row behind ", length(moved), " curated mapping(s) has changed: ",
         paste(sprintf("%s (expected %s, found %s)", smap$map_id[j][moved],
                       substr(smap$row_fingerprint[j][moved], 1, 12),
                       substr(df$row_fingerprint[moved], 1, 12)), collapse = ", "),
         ". A changed row is a question about who these people are. Re-check the ",
         "cohort and update ", path, " in the same commit as the workbook change.")

  bad <- which(!is.na(smap$n[j]) & smap$n[j] != df$n)
  if (length(bad))
    stop(key, ": the curated map records n = ", paste(smap$n[j][bad], collapse = ", "),
         " for ", paste(smap$map_id[j][bad], collapse = ", "),
         " but the locked workbook gives ", paste(df$n[bad], collapse = ", "), ".")

  df$map_id        <- smap$map_id[j]
  df$sample_id     <- smap$sample_id[j]
  df$multiplicity  <- smap$multiplicity[j]
  df$mapping_basis <- smap$mapping_basis[j]
  df
}

#' Cohort-level verification, DERIVED from the mapping rows and never asserted.
#'
#' A cohort is verified only when every report mapped to it has been evidenced.
#' Storing this on samples.csv as well would allow two files to disagree about
#' whether the same thing had been checked.
derive_cohort_status <- function(sample_ids, smap) {
  vapply(sample_ids, function(s) {
    st <- smap$evidence_status[smap$sample_id == s]
    if (!length(st))               return("derived_unverified")
    if (any(st == "disputed"))     return("disputed")
    if (all(st == "evidenced"))    return("verified")
    "derived_unverified"
  }, character(1), USE.NAMES = FALSE)
}

#' Structural invariants over the assembled effect rows. Every one is fatal.
gate_sample_identity <- function(ALL, smap, reg) {
  ## G1. Nothing reaches the analysis without a curated identity.
  if (anyNA(ALL$sample_id) || anyNA(ALL$map_id))
    stop(sum(is.na(ALL$sample_id) | is.na(ALL$map_id)),
         " effect row(s) reached the analysis with no curated sample identity.")

  ## G2. THE COLLISION GATE. Two live rows in one pathway on one sample_id are
  ## one cohort measured more than once, which is a claim about participants and
  ## so is declared rather than inferred from the collision. This could not fire
  ## before: analysis_role assignment had already demoted one of any such pair to
  ## sensitivity_alternative_instrument before the gate ever ran.
  live  <- ALL[!ALL$excluded & ALL$multiplicity == "single", , drop = FALSE]
  k     <- paste(live$pathway, live$sample_id)
  clash <- live[k %in% k[duplicated(k)], , drop = FALSE]
  if (nrow(clash))
    stop("Two or more primary rows in one pathway are mapped to one sample_id and ",
         "none declares why: ",
         paste(unique(sprintf("%s %s (%s %s)", clash$pathway, clash$sample_id,
                              clash$report_key, clash$instrument)), collapse = "; "),
         ". Either they are different cohorts, so give them different sample_ids, ",
         "or they are one cohort measured more than once, so set multiplicity to ",
         "alternative_instrument or superseded_report.")

  ## G3. A mapping row that matches nothing is how a curated decision silently
  ## stops applying after a workbook is revised.
  stale <- setdiff(smap$map_id, ALL$map_id)
  if (length(stale))
    stop(length(stale), " mapping row(s) match no eligible effect row: ",
         paste(utils::head(stale, 5), collapse = ", "),
         ". The map has drifted from the workbooks.")

  ## G4. A declared cohort nothing maps to is a split that never happened.
  used   <- unique(ALL$sample_id)
  active <- reg$sample_id[reg$sample_status == "in_analysis"]
  orphan <- setdiff(active, used)
  if (length(orphan))
    stop("Declared cohort(s) that no effect row maps to: ",
         paste(orphan, collapse = ", "),
         ". Map a row to each, or set sample_status to retired. Identifiers are ",
         "never deleted and never reused, because a fit_id such as ",
         "loso_stan|negsch_del|clinical|S026 must keep meaning one cohort.")

  ## G5. A retired identifier still in use means a split or merge was recorded
  ## and then not applied.
  ghost <- setdiff(used, active)
  if (length(ghost))
    stop("Effect row(s) mapped to cohort(s) that are not in_analysis: ",
         paste(ghost, collapse = ", "), ".")

  ## G6. effect_id must be unique. apply_registry() disambiguates a within-cohort
  ## collision by appending the cohort, so reaching here means two rows collided
  ## on the identifier AND on the cohort, which no legitimate split produces.
  if (anyDuplicated(ALL$effect_id))
    stop("Duplicate effect_id after cohort disambiguation: ",
         paste(unique(ALL$effect_id[duplicated(ALL$effect_id)]), collapse = ", "),
         ". Two rows share a pathway, report label, instrument and cohort.")

  invisible(TRUE)
}

#' Build the cohort registry: derived facts, reconciled with the committed file
#' that owns the identifiers.
#'
#' @param ALL every eligible effect row, carrying `sample_id`, `report_key`, `n`.
#' @param smap the curated map.
#' @param reg the committed registry.
#' @return list(samples, review_queue)
build_sample_registry <- function(ALL, smap, reg) {
  sid <- sort(unique(ALL$sample_id))
  rows_of <- function(s) ALL[ALL$sample_id == s, , drop = FALSE]

  ## STRUCTURAL: one cohort cannot have two participant counts. A superseded
  ## report may legitimately restate a different analytic n, so it is excluded
  ## here and surfaced in the review queue instead.
  counted <- ALL[ALL$multiplicity != "superseded_report", , drop = FALSE]
  n_by <- lapply(sid, function(s) sort(unique(counted$n[counted$sample_id == s])))
  names(n_by) <- sid
  inconsistent <- names(n_by)[vapply(n_by, length, integer(1)) > 1L]
  if (length(inconsistent))
    stop("A cohort carries more than one participant count, so it is either two ",
         "cohorts or a transcription error: ",
         paste(vapply(inconsistent, function(s)
           sprintf("%s (n = %s)", s, paste(n_by[[s]], collapse = ", ")),
           character(1)), collapse = "; "),
         ". Resolve in the workbook, or map the reports to different cohorts.")

  lab <- vapply(sid, function(s) {
    r <- reg$canonical_label[reg$sample_id == s]
    if (length(r) && !is.na(r[1]) && nzchar(r[1])) r[1] else sort(rows_of(s)$report_key)[1]
  }, character(1))

  derived <- data.frame(
    sample_id       = sid,
    canonical_label = lab,
    study_label     = sub(" [0-9]{4}$", "", lab),
    year            = as.integer(sub("^.* ", "", lab)),
    n_reported      = vapply(n_by, function(x) x[1], numeric(1))[sid],
    n_reports       = vapply(sid, function(s) length(unique(rows_of(s)$report_key)), integer(1)),
    report_keys     = vapply(sid, function(s)
      paste(sort(unique(rows_of(s)$report_key)), collapse = ";"), character(1)),
    pathways        = vapply(sid, function(s)
      paste(sort(unique(rows_of(s)$pathway)), collapse = ";"), character(1)),
    stringsAsFactors = FALSE)

  own <- reg[match(sid, reg$sample_id), SAMPLE_REGISTRY_OWNED, drop = FALSE]
  derived$sample_status    <- own$sample_status
  derived$predecessor_ids  <- own$predecessor_ids
  derived$successor_ids    <- own$successor_ids
  derived$note             <- own$note

  ## Derived, not read. See derive_cohort_status().
  derived$verification_status <- derive_cohort_status(sid, smap)

  samples <- derived[order(derived$sample_id), SAMPLE_REGISTRY_COLS]
  rownames(samples) <- NULL

  list(samples = samples, review_queue = sample_review_queue(samples, ALL, smap))
}

#' Cases the map cannot settle by itself, named so a reviewer can settle them.
#'
#' Emitted, not raised. Each is a question about the world, not a contradiction
#' in the data, so stopping the build would be the wrong response.
sample_review_queue <- function(samples, ALL, smap) {
  q <- list()
  add <- function(...) q[[length(q) + 1L]] <<- data.frame(..., stringsAsFactors = FALSE)

  ## One cohort appearing in two source workbooks is only one cohort if the same
  ## participants were measured for both mediators. The item clears once every
  ## mapping row for that cohort is evidenced: the queue exists to raise
  ## questions a human must settle, so it must stop asking one that has been
  ## settled, or a reviewer learns to ignore it.
  wb <- unique(ALL[, c("sample_id", "pathway")])
  src <- vapply(split(wb$pathway, wb$sample_id), function(p)
    length(unique(sub("_.*$", "", p))), integer(1))
  for (s in names(src)[src > 1]) {
    if (all(smap$evidence_status[smap$sample_id == s] == "evidenced")) next
    add(sample_id = s, report_key = samples$report_keys[samples$sample_id == s],
        issue = "spans more than one source workbook",
        detail = paste(sort(unique(wb$pathway[wb$sample_id == s])), collapse = ";"),
        question = "Are these the same participants, measured for both mediators?")
  }

  ## A label that will not round-trip cleanly is a label that can silently fail
  ## to match its twin in another workbook.
  odd <- unique(ALL$report_key[grepl("[,;‘’‚“”]|\\s{2,}", ALL$report_key)])
  for (k in odd)
    add(sample_id = paste(sort(unique(ALL$sample_id[ALL$report_key == k])), collapse = ";"),
        report_key = k, issue = "punctuation in the study label", detail = k,
        question = "Does this label match the same cohort elsewhere?")

  ## Same first author within a year or two, which is where an accidental
  ## collapse or an accidental split is most likely.
  lab <- samples$study_label
  for (i in seq_along(lab)) for (j in seq_along(lab)) {
    if (j <= i || lab[i] != lab[j]) next
    if (abs(samples$year[i] - samples$year[j]) <= 2)
      add(sample_id = samples$sample_id[i], report_key = samples$report_keys[i],
          issue = "same first author, adjacent years",
          detail = paste(samples$canonical_label[i], "and", samples$canonical_label[j]),
          question = "Are these two reports of one cohort?")
  }

  ## An asserted merge or split that nobody has evidenced yet is exactly the
  ## kind of judgement that should not pass unnoticed into a release.
  for (s in samples$sample_id[samples$n_reports > 1L]) {
    if (any(smap$evidence_status[smap$sample_id == s] != "evidenced"))
      add(sample_id = s, report_key = samples$report_keys[samples$sample_id == s],
          issue = "merge asserted but not evidenced",
          detail = samples$report_keys[samples$sample_id == s],
          question = "Do these reports describe one set of participants?")
  }
  for (k in unique(ALL$report_key)) {
    ids <- unique(ALL$sample_id[ALL$report_key == k])
    if (length(ids) > 1L && any(smap$evidence_status[smap$sample_id %in% ids] != "evidenced"))
      add(sample_id = paste(sort(ids), collapse = ";"), report_key = k,
          issue = "split asserted but not evidenced",
          detail = paste(sort(ids), collapse = ";"),
          question = "Does this report describe more than one cohort?")
  }

  ## A superseded report whose participant count disagrees with the cohort's is
  ## a discrepancy that the structural check deliberately steps around.
  sup <- ALL[ALL$multiplicity == "superseded_report", , drop = FALSE]
  for (i in seq_len(nrow(sup))) {
    nr <- samples$n_reported[samples$sample_id == sup$sample_id[i]]
    if (length(nr) && !is.na(nr) && nr != sup$n[i])
      add(sample_id = sup$sample_id[i], report_key = sup$report_key[i],
          issue = "superseded report disagrees on participant count",
          detail = sprintf("cohort n = %g, this report n = %g", nr, sup$n[i]),
          question = "Which participant count describes the analysed cohort?")
  }

  if (!length(q)) return(NULL)
  out <- do.call(rbind, q); rownames(out) <- NULL; out
}

#' The one sentence stating what the analytical unit currently rests on.
#'
#' Generated here and asserted verbatim by check_documents(), so the prose and
#' the registry cannot drift apart: editing the sentence fails the build, and a
#' change in the evidence rewrites both at once.
#'
#' @param effects lock$effects, one row per pathway/instrument/effect estimate,
#'   with a `sample_id` and an `excluded` flag. Read only to confirm a disputed
#'   cohort has actually stopped contributing before that is asserted in
#'   prose; never stored on samples.csv or sample_map.csv, so this function
#'   takes it as a fourth, optional argument rather than growing the registry.
identity_caveat <- function(samples, smap, effects = NULL) {
  n  <- nrow(smap)
  ne <- sum(smap$evidence_status == "evidenced")
  nd <- sum(smap$evidence_status == "disputed")
  nu <- n - ne - nd                       # NOT n - ne: a disputed row is not an
                                          # unchecked one, and counting it in
                                          # both places overstates the shortfall
  if (ne == n && n > 0L)
    return(paste0("Sample identity is curated in `data/provenance/sample_map.csv`; all ",
                  n, " report-to-cohort assertions were checked against a named source, ",
                  "with the evidence, the person who accepted it and the date recorded."))

  ## Every assertion has been checked (nu == 0) but at least one is disputed
  ## rather than evidenced. This is not "some remain unevidenced": it is a
  ## separate, stronger claim, and it may only be made once the disputed
  ## cohort has actually stopped contributing to the analysis, not merely
  ## been flagged. If it has not, that is an inconsistency worth stopping the
  ## build for rather than describing in careful prose.
  if (nu == 0L && nd > 0L) {
    disputed_ids <- smap$sample_id[smap$evidence_status == "disputed"]
    if (is.null(effects))
      stop("identity_caveat() needs `effects` to confirm a disputed sample_id ",
           "has been excluded from the live analysis before it can describe ",
           "the dispute as settled.")
    ## all() over zero rows is vacuously TRUE, which would let a disputed
    ## sample_id with no matching effect row at all pass as "excluded" when it
    ## was never checked in the first place. Require at least one matching
    ## row, none of them missing an excluded value, and every one excluded.
    disputed_rows <- effects[effects$sample_id %in% disputed_ids, , drop = FALSE]
    if (!nrow(disputed_rows) || anyNA(disputed_rows$excluded) ||
        !all(disputed_rows$excluded))
      stop("Row(s) for a disputed sample_id remain in the live analysis, or no ",
           "effect row could be found to confirm exclusion, for: ",
           paste(sort(unique(disputed_ids)), collapse = ", "),
           ". A disputed identity must be excluded from the analysed set before ",
           "its resolution can be described as settled.")
    return(sprintf(paste0(
      "Sample identity is curated in `data/provenance/sample_map.csv`. All %d ",
      "report-to-cohort assertions have been checked against named sources: %d are ",
      "evidenced and %d %s disputed. The disputed %s excluded from the analysed ",
      "set. Consequently, no assertion remains unevidenced, and all report-to-cohort ",
      "mappings used in the analysis are evidenced against named sources."),
      n, ne, nd, if (nd == 1L) "is" else "are",
      if (nd == 1L) "assertion is" else "assertions are"))
  }

  sprintf(paste0("Sample identity is curated in `data/provenance/sample_map.csv`. %d of %d ",
                 "report-to-cohort assertions have been evidenced against a named source%s; ",
                 "the remaining %d rest on the author and year printed in the extraction ",
                 "workbook and have not been checked against the papers. The analytical unit ",
                 "is therefore derived and has not been externally verified."),
          ne, n,
          if (nd) sprintf(" and %d %s disputed", nd, if (nd == 1L) "is" else "are") else "",
          nu)
}

## ---- canonicalisation ------------------------------------------------------

#' Write a data frame in a form whose SHA-256 is reproducible across machines.
#'
#' Fixed column order and row sort, UTF-8, NA as empty, numerics at 17
#' significant digits, LF line endings. Without this the hash of the locked
#' analysis table would depend on locale and platform.
write_canonical <- function(df, path, sort_by) {
  missing_cols <- setdiff(sort_by, names(df))
  if (length(missing_cols))
    stop("Cannot sort canonically: missing ", paste(missing_cols, collapse = ", "))
  df <- df[do.call(order, c(lapply(sort_by, function(k) df[[k]]), list(method = "radix"))), ,
           drop = FALSE]
  rownames(df) <- NULL
  chr <- as.data.frame(lapply(df, function(col)
    vapply(seq_along(col), function(i) canon_value(col[i]), character(1))),
    stringsAsFactors = FALSE)
  names(chr) <- names(df)
  con <- file(path, open = "wb", encoding = "UTF-8")
  on.exit(close(con))
  lines <- c(paste(names(chr), collapse = ","),
             apply(chr, 1, function(r) paste(gsub('"', '""', r) |>
                                               (\(x) ifelse(grepl('[",]', x), paste0('"', x, '"'), x))(),
                                             collapse = ",")))
  writeLines(lines, con, sep = "\n")
  invisible(df)
}
