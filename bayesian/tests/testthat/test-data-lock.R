# The data lock. These assertions encode the scientific decisions that must not
# be reversed, and they DERIVE the expected outcome from the pipeline rather
# than hard-coding it, so a change in inclusion rules shows up as a failure
# rather than being absorbed silently.

test_that("every locked input matches its recorded hash", {
  for (nm in names(CFG$inputs)) {
    p <- file.path(ROOT, CFG$inputs[[nm]]$path)
    expect_true(file.exists(p), info = nm)
    expect_equal(sha256_file(p), CFG$inputs[[nm]]$sha256, info = nm)
  }
})

test_that("all six pathways are built and every correlation is admissible", {
  expect_setequal(names(LOCK$analysis), PATH_NAMES(CFG))
  for (k in names(LOCK$analysis)) {
    d <- LOCK$analysis[[k]]
    expect_gt(nrow(d), 0)
    expect_true(all(abs(d$r_XM) <= 1 & abs(d$r_XY) <= 1 & abs(d$r_MY) <= 1), info = k)
    expect_true(all(d$min_eigen > 0), info = k)
    expect_true(all(d$n > 3), info = k)          # 1/(n-3) must be finite
  }
})

test_that("one independent sample contributes at most one triplet per pathway", {
  for (k in names(LOCK$analysis)) {
    d <- LOCK$analysis[[k]]
    expect_equal(anyDuplicated(d$sample_id), 0L, info = k)
  }
})

test_that("Blose contributes once, by DES-II, with PDEQ held as a replacement", {
  d <- LOCK$analysis$dissoc_cps
  blose <- d[d$studyID == "Blose", ]
  expect_equal(nrow(blose), 1L)
  expect_equal(blose$instrument, "DES-II")

  # PDEQ exists, but as a replacement for the same sample, never alongside it.
  alt <- LOCK$alternative$dissoc_cps
  expect_false(is.null(alt))
  expect_equal(nrow(alt), nrow(d))
  expect_equal(sort(alt$sample_id), sort(d$sample_id))
  expect_equal(alt$instrument[alt$studyID == "Blose"], "(PDEQ)")
})

test_that("Evans and Uyan are excluded with their recorded status", {
  d <- LOCK$analysis$dissoc_cps
  expect_false(any(d$studyID %in% c("Evans", "Uyan,")))
  ex <- LOCK$effects[LOCK$effects$pathway == "dissoc_cps" & LOCK$effects$excluded, ]
  expect_setequal(ex$studyID, c("Evans", "Uyan,"))
  expect_equal(ex$exclusion_status[ex$studyID == "Uyan,"], "pending_source_query")
  expect_equal(ex$exclusion_status[ex$studyID == "Evans"], "excluded_permanent")
})

test_that("LoPilato female composition is the corrected percentage", {
  e <- LOCK$effects[LOCK$effects$studyID == "LoPilato", ]
  expect_gt(nrow(e), 0)
  expect_true(all(abs(e$female - 39.5) < 1e-9))
})

test_that("the ten dissociation reclassifications change provenance, not membership", {
  # Evans is excluded regardless. Levi 2025 has no complete correlation triplet.
  # Khosravi 2021 is trauma_type = abuse with non-composite dissociation
  # domains. None of the three therefore enters an analysed pathway, so the
  # sample-type corrections must leave every k and every guard untouched.
  reclassified <- c("Evans", "Khosravi", "Levi")
  for (k in names(LOCK$analysis))
    expect_false(any(LOCK$analysis[[k]]$studyID %in% reclassified), info = k)

  r <- LOCK$reportability
  expect_equal(r$status[r$pathway == "dissoc_cps" & r$moderator == "clinical"],
               "not reported")
  expect_equal(nrow(LOCK$analysis$dissoc_cps), 4L)
  d <- LOCK$analysis$dissoc_cps
  expect_equal(sum(d$clinical == 1), 1L)
  expect_equal(sum(d$clinical == 0), 3L)
})

test_that("supplied quality scores cannot change pathway membership", {
  # complete_rows() gates on the three correlations and n only, so a newly
  # populated NOS can never add a study. Asserted rather than assumed.
  ids <- lapply(LOCK$analysis, function(d) sort(d$sample_id))
  stripped <- LOCK
  for (k in names(stripped$analysis)) stripped$analysis[[k]]$nos <- NA_real_
  ids2 <- lapply(stripped$analysis, function(d) sort(d$sample_id))
  expect_equal(ids, ids2)
})

test_that("the reportability guards fire where an arm is uninformative", {
  r <- LOCK$reportability
  expect_setequal(unique(r$status), c("ok", "fragile", "not reported"))
  for (i in seq_len(nrow(r))) {
    if (r$moderator[i] == "clinical") next
    if (r$status[i] == "not reported") expect_lt(r$k[i], CFG$reportability$continuous_min_k)
    if (r$status[i] == "ok")           expect_gt(r$k[i], CFG$reportability$continuous_fragile_k)
  }
})

test_that("every sample and effect in the analysis appears in the registries", {
  for (k in names(LOCK$analysis)) {
    d <- LOCK$analysis[[k]]
    expect_true(all(d$sample_id %in% LOCK$samples$sample_id), info = k)
    expect_true(all(d$effect_id %in% LOCK$effects$effect_id), info = k)
  }
  expect_equal(anyDuplicated(LOCK$samples$sample_id), 0L)
  expect_equal(anyDuplicated(LOCK$effects$effect_id), 0L)
})

test_that("the sampling covariance is positive definite in every construction", {
  for (k in names(LOCK$analysis)) {
    for (Ls in list(LOCK$vz_chol[[k]], LOCK$vz_chol_weighted[[k]])) {
      for (L in Ls) {
        V <- L %*% t(L)
        expect_gt(min(eigen(V, symmetric = TRUE, only.values = TRUE)$values), 0)
      }
    }
  }
})

test_that("rcalc and asyCov differ only in the characterised ways", {
  v <- LOCK$vz_comparison
  # The delta method puts 1/(n-1) on the z diagonal where Fisher's is 1/(n-3);
  # small, systematic, largest in the smallest studies.
  expect_true(all(v$max_abs_diff_individual < 0.01))
  # acov = "weighted" borrows a pooled correlation matrix and differs by roughly
  # an order of magnitude more. It is a sensitivity construction, never primary.
  expect_true(all(v$max_abs_diff_weighted > v$max_abs_diff_individual))
})

test_that("the provenance registries are byte-reproducible across platforms", {
  # A cross-platform finding, not a hypothetical. min_eigen is computed by
  # LAPACK and its final digits differ between macOS and Linux: Simpson 2020
  # is 0.519788893624411 on one and ...412 on the other. Written at full
  # precision that made effects.csv differ from a rebuild on another machine,
  # which is exactly what the release gate "the build left no tracked file
  # modified" exists to catch, and it caught it.
  #
  # min_eigen is a diagnostic used only for a > 0 test, so recording it at ten
  # decimal places loses nothing and makes the file reproducible.
  e <- LOCK$effects
  expect_true("min_eigen" %in% names(e))
  expect_equal(e$min_eigen, round(e$min_eigen, 10))

  # It must still do its job: every retained matrix is positive definite.
  expect_true(all(e$min_eigen[!e$excluded] > 0))

  # And the canonical analysis table, whose hash pins the whole run, never
  # carried this column, so no platform-dependent value can reach the
  # analysis_id. Read from the written file, not from a column list this test
  # would otherwise be asserting against itself.
  tab <- read.csv("../../../data/inputs/analysis_table.csv", stringsAsFactors = FALSE)
  expect_false("min_eigen" %in% names(tab))
})
