# Provenance: the correction ledger must address cells uniquely and must refuse
# to apply if anything about the target has moved. These tests operate on
# FIXTURES built in memory, never on the release inputs.

test_that("cells_differ sees a blank becoming a value", {
  # The defect this guards against: which(!(x == y | (is.na(x) & is.na(y))))
  # evaluates to NA when x is NA and y is not, and which() drops NA silently.
  # Nineteen Newcastle-Ottawa scores were invisible to that comparison.
  expect_true(cells_differ(NA, 6))
  expect_true(cells_differ(6, NA))
  expect_false(cells_differ(NA, NA))
  expect_false(cells_differ(6, 6))
  expect_equal(sum(cells_differ(c(NA, 1, NA, 3), c(6, 1, NA, 4))), 2L)
})

test_that("canon_value round-trips a double without padding", {
  expect_equal(canon_value(0.395), "0.39500000000000002")
  expect_equal(canon_value(39.5), "39.5")
  expect_equal(as.numeric(canon_value(1/3)), 1/3)
  expect_equal(canon_value(NA), "")
  expect_equal(canon_value("  DES-II "), "DES-II")
})

test_that("excel_col_letter handles the wrap past Z", {
  expect_equal(excel_col_letter(c(1, 26, 27, 28, 52, 53)),
               c("A", "Z", "AA", "AB", "AZ", "BA"))
})

# --- fixture ------------------------------------------------------------------
# Mirrors the real hazard: two rows agreeing on author, year and the corrected
# field, separable only by their correlations. In the dissociation workbook
# Evans 2015 occupies six such rows.
fixture <- function() {
  data.frame(studyID = c("Evans", "Evans", "Cole"), year = c(2015L, 2015L, 2016L),
             sample = c("Mixed", "Mixed", "GP"), r_XM = c(0.013, 0.040, 0.58),
             NOS = c(NA_real_, NA_real_, 5), stringsAsFactors = FALSE)
}
fixture_ledger <- function(df, row, column, old, new, id = "T001") {
  data.frame(correction_id = id, source_blob_sha = "SHA", workbook = "fix.xlsx",
             sheet = "1", cell_address = "C2", row_index = row, column = column,
             row_fingerprint = row_fingerprint(df, row),
             expected_old = old, new_value = new,
             reason = "test", evidence_source = "test", authority = "test",
             decision_date = "2026-07-30", stringsAsFactors = FALSE)
}

test_that("a correction applies to exactly the addressed cell", {
  df <- fixture()
  led <- fixture_ledger(df, 2, "sample", "Mixed", "Clinical")
  out <- apply_corrections(df, led, "fix.xlsx", "1", "SHA")
  # Row 2 changes; the identically-valued row 1 does not. Author and year cannot
  # separate these two rows, which is why the ledger addresses cells.
  expect_equal(out$sample, c("Mixed", "Clinical", "GP"))
})

test_that("application refuses when the old value is not what the ledger declares", {
  df <- fixture()
  led <- fixture_ledger(df, 2, "sample", "Clinical", "Mixed")   # wrong old value
  expect_error(apply_corrections(df, led, "fix.xlsx", "1", "SHA"),
               "expected .* to hold 'Clinical'")
})

test_that("application refuses when the target row has moved", {
  df  <- fixture()
  led <- fixture_ledger(df, 2, "sample", "Mixed", "Clinical")
  moved <- df[c(2, 1, 3), ]; rownames(moved) <- NULL
  expect_error(apply_corrections(moved, led, "fix.xlsx", "1", "SHA"),
               "fingerprint has changed")
})

test_that("application refuses when the source hash is wrong", {
  df  <- fixture()
  led <- fixture_ledger(df, 2, "sample", "Mixed", "Clinical")
  expect_error(apply_corrections(df, led, "fix.xlsx", "1", "DIFFERENT"),
               "derived against a different")
})

test_that("two corrections on one row both apply", {
  # A corrected value and its provenance note share a row. Fingerprints are
  # checked against the pristine sheet so the first does not invalidate the
  # second.
  df <- fixture()
  led <- rbind(fixture_ledger(df, 3, "sample", "GP", "Clinical", "T001"),
               fixture_ledger(df, 3, "NOS", "5", "6", "T002"))
  out <- apply_corrections(df, led, "fix.xlsx", "1", "SHA")
  expect_equal(out$sample[3], "Clinical")
  expect_equal(out$NOS[3], 6)
})

test_that("quality scores refuse to overwrite a conflicting workbook value", {
  df <- data.frame(study_author = c("Morgan", "Jaya"), year = c(2014L, 2017L),
                   NOS = c(6, NA_real_), stringsAsFactors = FALSE)
  qs <- data.frame(workbook = "fix.xlsx", study_label = c("Morgan", "Jaya"),
                   year = c(2014L, 2017L), nos_total = c(6, 4),
                   stringsAsFactors = FALSE)
  out <- apply_quality_scores(df, qs, "fix.xlsx", "study_author", "year")
  expect_equal(out$NOS, c(6, 4))

  qs$nos_total[1] <- 7                       # disagrees with the workbook
  expect_error(apply_quality_scores(df, qs, "fix.xlsx", "study_author", "year"),
               "Quality score conflict")
})

test_that("quality scores refuse an ambiguous or absent study match", {
  df <- data.frame(study_author = c("Fisher", "Fisher"), year = c(2012L, 2012L),
                   NOS = c(NA_real_, NA_real_), stringsAsFactors = FALSE)
  qs <- data.frame(workbook = "fix.xlsx", study_label = "Fisher", year = 2012L,
                   nos_total = 6, stringsAsFactors = FALSE)
  expect_error(apply_quality_scores(df, qs, "fix.xlsx", "study_author", "year"),
               "matches 2 rows")
})

test_that("write_canonical is stable under row permutation", {
  d1 <- data.frame(pathway = c("b", "a"), sample_id = c("S2", "S1"),
                   effect_id = c("e2", "e1"), x = c(1/3, 2/7), stringsAsFactors = FALSE)
  d2 <- d1[c(2, 1), ]
  f1 <- tempfile(); f2 <- tempfile()
  write_canonical(d1, f1, c("pathway", "sample_id", "effect_id"))
  write_canonical(d2, f2, c("pathway", "sample_id", "effect_id"))
  expect_equal(sha256_file(f1), sha256_file(f2))
})
