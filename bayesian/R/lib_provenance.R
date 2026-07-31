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

## ---- the independent-sample registry ---------------------------------------
#
# The analytical unit is one independent participant sample. Until this was
# added, `sample_id` was a serial relabelling of `paste(studyID, year)`:
# samples.csv was WRITTEN by the lock and never read back, so the claim that a
# curated registry defined the unit was false, and nothing detected two cohorts
# published by one author in one year or one cohort appearing under two labels.
#
# The registry does not, and cannot, decide sample identity by itself. That
# needs a human with the papers. What it does is make the derivation an explicit,
# reviewable artefact, hold the identifiers stable across workbook revisions, and
# separate two different kinds of problem:
#
#   STRUCTURAL failures stop the build. The data contradict themselves and no
#   amount of judgement rescues them.
#   REVIEW items do not stop the build. The derivation may be wrong in a way
#   only an author can settle, so it is surfaced by name rather than buried.

SAMPLE_REGISTRY_COLS <- c("sample_id", "sample_key", "study_label", "year",
                          "n_reported", "pathways", "verification_status",
                          "verification_source", "verified_by", "verified_date")

VERIFICATION_STATUSES <- c("derived_unverified", "verified", "disputed")

#' Derive the registry from the observed effect rows, then reconcile it with the
#' committed source table if one exists.
#'
#' @param ALL every eligible effect row, carrying `sample_key`, `pathway`, `n`.
#' @param path committed registry. Absent on a first run, which seeds it.
#' @return list(samples, review_queue)
build_sample_registry <- function(ALL, path = "data/provenance/samples.csv") {
  sk <- unique(ALL$sample_key)
  derived <- data.frame(
    sample_key  = sk,
    study_label = sub(" [0-9]{4}$", "", sk),
    year        = as.integer(sub("^.* ", "", sk)),
    stringsAsFactors = FALSE)
  derived$pathways <- vapply(derived$sample_key, function(k)
    paste(sort(unique(ALL$pathway[ALL$sample_key == k])), collapse = ";"), character(1))

  ## STRUCTURAL: one sample cannot have two participant counts. The previous
  ## code took whichever row happened to come first, so a genuine disagreement
  ## between two workbooks about the same cohort was silently resolved by row
  ## order.
  n_by_key <- lapply(sk, function(k) sort(unique(ALL$n[ALL$sample_key == k])))
  names(n_by_key) <- sk
  inconsistent <- names(n_by_key)[vapply(n_by_key, length, integer(1)) > 1L]
  if (length(inconsistent))
    stop("A sample carries more than one participant count, so it is either two ",
         "samples or a transcription error: ",
         paste(vapply(inconsistent, function(k)
           sprintf("%s (n = %s)", k, paste(n_by_key[[k]], collapse = ", ")),
           character(1)), collapse = "; "),
         ". Resolve in the workbook or split the sample key before proceeding.")
  derived$n_reported <- vapply(n_by_key, function(x) x[1], numeric(1))[derived$sample_key]

  ## STRUCTURAL: two rows in one pathway sharing an author-year key means the
  ## key cannot separate them, so the instrument-priority rule would demote one
  ## arbitrarily. Rows already marked as alternative instruments are excluded,
  ## since those are a deliberate second measurement of ONE sample.
  prim <- ALL[ALL$analysis_role == "primary" & !ALL$excluded, ]
  clash <- prim[duplicated(paste(prim$pathway, prim$sample_key)), ]
  if (nrow(clash))
    stop("Two primary rows in one pathway share an author-year key, so the key ",
         "cannot identify the sample: ",
         paste(unique(paste(clash$pathway, clash$sample_key)), collapse = ", "), ".")

  ## Reconcile with the committed registry, which owns the identifiers.
  if (file.exists(path)) {
    reg <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "")
    miss_col <- setdiff(SAMPLE_REGISTRY_COLS, names(reg))
    if (length(miss_col))
      stop("samples.csv is missing required column(s): ",
           paste(miss_col, collapse = ", "),
           ". Delete the file to reseed it, then re-verify.")

    undeclared <- setdiff(derived$sample_key, reg$sample_key)
    if (length(undeclared))
      stop(length(undeclared), " effect row(s) map to no declared sample_id: ",
           paste(utils::head(undeclared, 5), collapse = ", "),
           ". Add them to data/provenance/samples.csv, with a verification status, ",
           "before they can enter the analysis.")

    bad <- setdiff(unique(reg$verification_status), VERIFICATION_STATUSES)
    if (length(bad))
      stop("Unknown verification_status in samples.csv: ",
           paste(bad, collapse = ", "), ". Allowed: ",
           paste(VERIFICATION_STATUSES, collapse = ", "), ".")

    ## A claim of verification must name who made it and when. Otherwise
    ## "verified" is a word rather than an assertion anyone is answerable for.
    v <- reg[reg$verification_status == "verified", ]
    unattributed <- v$sample_id[is.na(v$verified_by) | is.na(v$verified_date) |
                                  !nzchar(trimws(v$verified_by))]
    if (length(unattributed))
      stop("Sample(s) marked verified without verified_by and verified_date: ",
           paste(unattributed, collapse = ", "), ".")

    samples <- merge(reg[, c("sample_id", "sample_key", "verification_status",
                             "verification_source", "verified_by", "verified_date")],
                     derived, by = "sample_key", all.y = TRUE)
    samples <- samples[order(samples$sample_id), SAMPLE_REGISTRY_COLS]
  } else {
    ## First run: seed the registry. Every row is unverified by construction.
    derived <- derived[order(derived$year, derived$study_label), ]
    samples <- cbind(
      data.frame(sample_id = sprintf("S%03d", seq_len(nrow(derived))),
                 stringsAsFactors = FALSE),
      derived,
      data.frame(verification_status = "derived_unverified",
                 verification_source = NA_character_,
                 verified_by = NA_character_, verified_date = NA_character_,
                 stringsAsFactors = FALSE))[, SAMPLE_REGISTRY_COLS]
  }
  rownames(samples) <- NULL

  list(samples = samples, review_queue = sample_review_queue(samples, ALL))
}

#' Cases the derivation cannot settle, named so a reviewer can settle them.
#'
#' Emitted, not raised. Each is a question about the world, not a contradiction
#' in the data, so stopping the build would be the wrong response.
sample_review_queue <- function(samples, ALL) {
  q <- list()
  add <- function(...) q[[length(q) + 1L]] <<- data.frame(..., stringsAsFactors = FALSE)

  ## One key appearing in two source workbooks is only one sample if the same
  ## participants were measured for both mediators. Nothing in the spreadsheets
  ## records that, so it is asserted by a string match until someone checks.
  wb <- unique(ALL[, c("sample_key", "pathway")])
  src <- vapply(split(wb$pathway, wb$sample_key), function(p)
    length(unique(sub("_.*$", "", p))), integer(1))
  for (k in names(src)[src > 1])
    add(sample_key = k, issue = "spans more than one source workbook",
        detail = paste(sort(unique(wb$pathway[wb$sample_key == k])), collapse = ";"),
        question = "Are these the same participants, measured for both mediators?")

  ## A label that will not round-trip cleanly is a label that can silently fail
  ## to match its twin in another workbook.
  odd <- samples$sample_key[grepl("[,;‘’‚“”]|\\s{2,}", samples$sample_key)]
  for (k in odd)
    add(sample_key = k, issue = "punctuation in the study label",
        detail = k,
        question = "Does this label match the same cohort elsewhere?")

  ## Same first author within a year or two, which is where an accidental
  ## collapse or an accidental split is most likely.
  lab <- samples$study_label
  for (i in seq_along(lab)) for (j in seq_along(lab)) {
    if (j <= i || lab[i] != lab[j]) next
    if (abs(samples$year[i] - samples$year[j]) <= 2)
      add(sample_key = samples$sample_key[i], issue = "same first author, adjacent years",
          detail = paste(samples$sample_key[i], "and", samples$sample_key[j]),
          question = "Are these two reports of one cohort?")
  }

  if (!length(q)) return(NULL)
  out <- do.call(rbind, q); rownames(out) <- NULL; out
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
