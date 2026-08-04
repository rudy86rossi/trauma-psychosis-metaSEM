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

# ---- the cohort registry ----------------------------------------------------
# Before the curated map existed, `sample_id` was a serial relabelling of
# paste(studyID, year): samples.csv was written by the lock and never read, so
# the comment claiming a curated registry defined the analytical unit was false.
# Identity assertions now live in sample_map.csv and are tested in
# test-sample-identity.R. These tests cover what the registry is still for:
# structural contradictions stop the build, and judgement calls are surfaced.

test_that("the registry owns the identifiers and every effect row is declared", {
  reg <- read.csv("../../../data/provenance/samples.csv", stringsAsFactors = FALSE,
                  na.strings = "")
  expect_true(all(SAMPLE_REGISTRY_OWNED %in% names(reg)))
  expect_setequal(unique(unlist(lapply(LOCK$analysis, `[[`, "sample_id"))),
                  intersect(reg$sample_id, unlist(lapply(LOCK$analysis, `[[`, "sample_id"))))
  # No effect row may reach the analysis without a declared cohort, and none
  # without the mapping row that put it there.
  expect_false(anyNA(LOCK$effects$sample_id))
  expect_false(anyNA(LOCK$effects$map_id))
  # The derived half of the registry is published but never read back.
  expect_true(all(SAMPLE_REGISTRY_COLS %in% names(LOCK$samples)))
})

test_that("one cohort cannot carry two participant counts", {
  # The previous code took whichever row came first, so a genuine disagreement
  # between two workbooks about the same cohort was resolved by row order. That
  # is a contradiction in the data, not a judgement call, so it must be fatal.
  # It now bites at the level a merge can actually create it: two reports the
  # curator has asserted to be one cohort, disagreeing about how many people.
  ALL <- do.call(rbind, LOCK$analysis)
  ALL$analysis_role <- "primary"; ALL$excluded <- FALSE
  bad <- ALL
  bad$sample_id[nrow(bad)] <- bad$sample_id[1]
  bad$n[nrow(bad)] <- bad$n[1] + 1
  expect_error(build_sample_registry(bad, LOCK$sample_map, LOCK$samples),
               "more than one participant count")
})

test_that("cohort verification is derived from the mapping rows, never asserted", {
  # Storing a status on samples.csv as well as on sample_map.csv would let two
  # files disagree about whether the same thing had been checked. The registry
  # therefore has no verification column to edit: the value is computed.
  reg <- read.csv("../../../data/provenance/samples.csv", stringsAsFactors = FALSE,
                  na.strings = "")
  expect_false("verification_status" %in% names(reg))

  smap <- LOCK$sample_map
  s <- smap$sample_id[1]

  ## Each state is constructed here rather than read off whichever cohort
  ## happens to be first in the live registry. Completing the identity
  ## verification evidenced every mapping, so an unevidenced example no longer
  ## exists in the data, and this line was asserting a fact about the curation
  ## rather than about the function it names.
  un <- smap; un$evidence_status[un$sample_id == s] <- "unevidenced"
  expect_equal(derive_cohort_status(s, un), "derived_unverified")

  ev <- smap; ev$evidence_status[ev$sample_id == s] <- "evidenced"
  expect_equal(derive_cohort_status(s, ev), "verified")

  # One unevidenced report is enough to withhold the cohort-level claim, which
  # is the point: the cohort claim is strictly stronger than its parts.
  part <- ev; part$evidence_status[which(part$sample_id == s)[1]] <- "unevidenced"
  expect_equal(derive_cohort_status(s, part), "derived_unverified")

  dis <- ev; dis$evidence_status[which(dis$sample_id == s)[1]] <- "disputed"
  expect_equal(derive_cohort_status(s, dis), "disputed")
})

test_that("the review queue names judgement calls without stopping the build", {
  q <- LOCK$review_queue
  expect_true(is.data.frame(q) && nrow(q) > 0)
  expect_true(all(c("sample_id", "report_key", "issue", "detail", "question") %in% names(q)))
  # Labels that will not round-trip cleanly are flagged, because a punctuation
  # difference is exactly how one cohort silently becomes two.
  expect_true(any(grepl("punctuation", q$issue)))
})

test_that("a cross-workbook cohort is queried until it is evidenced, then not", {
  # Gibson 2019 supplies both a dissociation and a negative-schema triplet, and
  # only a human with the paper can say whether those are the same participants.
  # The queue must raise that while it is open and stop raising it once it is
  # answered: a queue that keeps asking a settled question teaches a reviewer to
  # ignore it. Gibson's full text establishes one sample of 945 undergraduates
  # given both the DES and the BCSS, so the item is now closed.
  ALL <- do.call(rbind, LOCK$analysis)
  ALL$report_key <- paste(ALL$studyID, ALL$year)
  spans <- function(q) if (is.null(q)) character(0) else
    q$report_key[q$issue == "spans more than one source workbook"]

  expect_false("Gibson 2019" %in% spans(LOCK$review_queue))

  open_map <- LOCK$sample_map
  open_map$evidence_status[open_map$sample_id == "S013"] <- "unevidenced"
  expect_true("Gibson 2019" %in% spans(sample_review_queue(LOCK$samples, ALL, open_map)))
})

test_that("promotion is refused from a worktree that does not match HEAD", {
  # The manifest names a commit as the source of the numbers. That is a claim
  # about a tree, and it is false the moment an uncommitted edit is present:
  # the recorded commit then describes source that produced nothing. The
  # preceding release, run_c9cb1a366dfb, carries the related defect in its
  # manifest, which is what this exists to stop recurring.
  #
  # Exercised on a throwaway repository rather than on this one, because the
  # project worktree is legitimately dirty while anyone is working in it, and a
  # test that asserted otherwise would fail for the wrong reason.
  skip_if(Sys.which("git") == "", "git is not available")
  d <- file.path(tempfile("cleanrepo"))
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  git <- function(...) system2("git", c("-C", d, ...), stdout = FALSE, stderr = FALSE)
  git("init", "-q")
  git("config", "user.email", "t@example.com")
  git("config", "user.name", "t")
  writeLines("a", file.path(d, "f.txt"))
  git("add", "f.txt")
  git("commit", "-qm", "first")

  expect_true(assert_clean_worktree(d))

  writeLines("b", file.path(d, "f.txt"))
  expect_error(assert_clean_worktree(d), "dirty worktree")

  # An untracked file is a dirty worktree too: it is exactly how an input that
  # was never committed reaches a released bundle.
  git("checkout", "--", "f.txt")
  writeLines("x", file.path(d, "untracked.txt"))
  expect_error(assert_clean_worktree(d), "dirty worktree")

  # Not a git repository at all: refused rather than silently treated as clean,
  # because system2 returns character(0) for both.
  nd <- tempfile("notarepo")
  dir.create(nd)
  on.exit(unlink(nd, recursive = TRUE), add = TRUE)
  expect_error(assert_clean_worktree(nd), "must be identified by a commit")
})
