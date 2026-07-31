# Curated sample identity.
#
# The defect these guard: identity used to be minted as paste(studyID, year) in
# lib_data.R, used immediately to demote duplicate rows by instrument priority,
# and only then joined to samples.csv with match(). One report could therefore
# never map to two cohorts, two reports could never map to one, and both
# structural collision gates were unreachable because the demotion had already
# guaranteed one primary row per (pathway, key) before either could run.

## A minimal two-row pathway plus the map that claims to describe it. Built by
## hand rather than taken from the workbooks, so each test can create exactly the
## contradiction it is about.
fixture <- function(sample_ids = c("S900", "S901"),
                    instruments = c("DES-II", "DES"),
                    multiplicity = c("single", "single"),
                    studyID = "Fisher", year = 2012L, n = c(120, 120),
                    pathway = "dissoc_del", rows = c(11L, 12L)) {
  df <- data.frame(
    pathway = pathway, label = "test",
    source_workbook = "test.xlsx", source_sheet = "Extraction",
    source_row = rows,
    source_effect_id = source_effect_id("test.xlsx", "Extraction", rows, pathway),
    row_fingerprint = sprintf("fp%02d", rows),
    studyID = studyID, year = year, n = n,
    r_XM = 0.3, r_XY = 0.25, r_MY = 0.35,
    age = 30, female = 0.5, nos = 6, clinical = 1,
    instrument = instruments, stringsAsFactors = FALSE)

  smap <- data.frame(
    map_id = sprintf("M%03d", seq_along(rows)),
    source_effect_id = df$source_effect_id,
    workbook = df$source_workbook, sheet = df$source_sheet, source_row = rows,
    pathway = pathway, studyID = studyID, year = year, instrument = instruments,
    n = n, row_fingerprint = df$row_fingerprint,
    sample_id = sample_ids, multiplicity = multiplicity,
    mapping_basis = "curated_confirm", evidence_status = "unevidenced",
    evidence_type = NA_character_, evidence_locator = NA_character_,
    evidence_prepared_by = NA_character_, accepted_by = NA_character_,
    accepted_date = NA_character_, note = NA_character_,
    stringsAsFactors = FALSE)[, SAMPLE_MAP_COLS]

  reg <- data.frame(sample_id = sort(unique(sample_ids)), canonical_label = "Fisher 2012",
                    sample_status = "in_analysis", predecessor_ids = NA_character_,
                    successor_ids = NA_character_, note = NA_character_,
                    stringsAsFactors = FALSE)
  list(df = df, smap = smap, reg = reg)
}

## Everything apply_registry() does before the map, so a fixture can be handed
## straight to it.
prep <- function(df) {
  df$excluded <- FALSE; df$exclusion_reason <- NA_character_
  df$exclusion_status <- NA_character_; df$min_eigen <- 0.5
  df$report_key <- paste(df$studyID, df$year)
  df
}

assemble <- function(f) {
  df <- apply_registry(CFG, prep(f$df), f$smap)
  gate_sample_identity(df, f$smap, f$reg)
  df
}

test_that("one author-year report can map to two independent cohorts", {
  # The case the old key could not express. Two cohorts published by one author
  # in one year, in one pathway, were collapsed to a single sample_key and one
  # of them was silently demoted to a sensitivity analysis.
  f <- fixture()
  df <- assemble(f)
  expect_equal(sum(df$analysis_role == "primary"), 2L)
  expect_equal(sort(df$sample_id), c("S900", "S901"))
})

test_that("two rows agreeing on pathway, author, year and instrument stay separable", {
  # A semantic key of (pathway, studyID, year, instrument) cannot tell these
  # apart at all. Identity is anchored to the source row, so it can.
  f <- fixture(instruments = c("DES-II", "DES-II"), n = c(120, 80))
  df <- assemble(f)
  expect_equal(length(unique(df$source_effect_id)), 2L)
  expect_equal(sort(df$sample_id), c("S900", "S901"))
  expect_true(all(df$analysis_role == "primary"))
})

test_that("two reports of one cohort map to one sample_id", {
  # The mirror case. The registry held one row per author-year string and the
  # join was many-to-one, so a merge was inexpressible.
  f <- fixture(sample_ids = c("S900", "S900"), pathway = "dissoc_del",
               multiplicity = c("single", "superseded_report"))
  f$reg <- f$reg[f$reg$sample_id == "S900", ]
  df <- assemble(f)
  expect_equal(unique(df$sample_id), "S900")
  expect_equal(sum(df$analysis_role == "primary"), 1L)
  expect_equal(sum(df$analysis_role == "duplicate_report"), 1L)

  reg <- build_sample_registry(df, f$smap, f$reg)
  expect_equal(reg$samples$n_reports, 1L)   # one report_key, two reports of it
})

test_that("an effect row with no curated mapping stops the build", {
  # Identity used to be invented from author and year, so a row nobody had
  # looked at silently acquired one.
  f <- fixture()
  f$smap <- f$smap[1, ]
  expect_error(apply_registry(CFG, prep(f$df), f$smap), "no curated mapping")
})

test_that("a source row that has changed under its assertion stops the build", {
  # The row index keeps an assertion attached when a cell is corrected; the
  # fingerprint makes sure the row is still the row the curator read.
  f <- fixture()
  f$smap$row_fingerprint[2] <- "something else"
  expect_error(apply_registry(CFG, prep(f$df), f$smap), "has changed")
})

test_that("the curated map must agree with the workbook about how many people", {
  f <- fixture()
  f$smap$n[1] <- f$smap$n[1] + 1
  expect_error(apply_registry(CFG, prep(f$df), f$smap), "but the locked workbook gives")
})

test_that("two undeclared rows on one sample_id in one pathway are fatal", {
  # THE collision gate. It could not fire before, because analysis_role
  # assignment had already demoted one of any such pair.
  f <- fixture(sample_ids = c("S900", "S900"))
  f$reg <- f$reg[f$reg$sample_id == "S900", ]
  expect_error(assemble(f), "none declares why")

  # Declaring what they are is what makes the demotion legitimate.
  f$smap$multiplicity <- "alternative_instrument"
  df <- assemble(f)
  expect_equal(sum(df$analysis_role == "primary"), 1L)
  expect_equal(sum(df$analysis_role == "sensitivity_alternative_instrument"), 1L)
})

test_that("instrument priority acts only inside a declared group", {
  # Demotion used to be unconditional, so an undeclared collision was resolved
  # silently by instrument priority instead of being reported.
  f <- fixture(sample_ids = c("S900", "S901"), instruments = c("DES-II", "DES"))
  df <- assemble(f)
  expect_true(all(df$analysis_role == "primary"))
})

test_that("an unranked instrument tie is fatal rather than settled by row order", {
  # which.min() takes the first on a tie, which made the primary triplet depend
  # on the order rows happen to appear in the spreadsheet.
  f <- fixture(sample_ids = c("S900", "S900"),
               instruments = c("Not in priority", "Also not in priority"),
               multiplicity = c("alternative_instrument", "alternative_instrument"))
  f$reg <- f$reg[f$reg$sample_id == "S900", ]
  expect_error(apply_registry(CFG, prep(f$df), f$smap), "does not separate")
})

test_that("a mapping row that matches no effect row stops the build", {
  # A curated split that stops matching after a workbook revision would revert
  # to the previous identity in silence.
  f <- fixture()
  extra <- f$smap[1, ]
  extra$map_id <- "M999"
  extra$source_effect_id <- source_effect_id("test.xlsx", "Extraction", 99L, "dissoc_del")
  f$smap <- rbind(f$smap, extra)
  expect_error(assemble(f), "match no eligible effect row")
})

test_that("a declared cohort that nothing maps to stops the build", {
  # A split recorded in samples.csv but never applied in the map would look
  # done and do nothing.
  f <- fixture()
  f$reg <- rbind(f$reg, data.frame(sample_id = "S999", canonical_label = "Ghost 2000",
                                   sample_status = "in_analysis",
                                   predecessor_ids = NA_character_,
                                   successor_ids = NA_character_, note = NA_character_,
                                   stringsAsFactors = FALSE))
  expect_error(assemble(f), "no effect row maps to")

  # Retiring it is the recorded way to remove a cohort. Identifiers are never
  # deleted and never reused, because a LOSO fit_id must keep meaning one cohort.
  f$reg$sample_status[f$reg$sample_id == "S999"] <- "retired"
  expect_silent(invisible(assemble(f)))
})

test_that("a retired identifier still in use stops the build", {
  f <- fixture()
  f$reg$sample_status[f$reg$sample_id == "S901"] <- "retired"
  expect_error(assemble(f), "not in_analysis")
})

test_that("a colliding effect_id is disambiguated by cohort, not left to collide", {
  # effect_id's sanitisation is lossy: "Sun," 2018 and "Sun" 2018 both reduce to
  # Sun2018, and a curated split within one pathway and instrument produces two
  # rows that reduce to one string. A legitimate split must not be a dead end,
  # so the cohort is appended where and only where the identifier would repeat.
  f <- fixture(instruments = c("DES-II", "DES-II"), n = c(120, 80))
  df <- apply_registry(CFG, prep(f$df), f$smap)
  expect_equal(sort(df$effect_id),
               c("dissoc_del:Fisher2012:DESII:S900", "dissoc_del:Fisher2012:DESII:S901"))
  expect_silent(gate_sample_identity(df, f$smap, f$reg))

  # No row in the release collides, so no identifier is rewritten.
  expect_false(any(grepl(":S[0-9]{3}$", LOCK$effects$effect_id)))
})

# ---- evidence -----------------------------------------------------------------
# Approval of a plan is not verification. These assert that "evidenced" is a
# claim someone is answerable for, against a source another person can check.

test_that("an evidenced identity must name its evidence, its acceptor and a date", {
  f <- fixture()
  ok <- f$smap
  ok$evidence_status <- "evidenced"
  ok$evidence_type <- "fulltext"
  ok$evidence_locator <- "Blose 2023 p. 7, participants"
  ok$evidence_prepared_by <- "AI-assisted source check (Claude Opus 5)"
  ok$accepted_by <- "R Twumasi"
  ok$accepted_date <- as.character(Sys.Date())
  expect_silent(validate_sample_map(ok, f$reg))

  for (field in c("evidence_type", "evidence_locator", "accepted_by", "accepted_date")) {
    bad <- ok; bad[[field]] <- NA_character_
    expect_error(validate_sample_map(bad, f$reg), "evidenced without|reached")
  }

  bad <- ok; bad$accepted_date <- "31/07/2026"
  expect_error(validate_sample_map(bad, f$reg), "ISO date")
  bad <- ok; bad$accepted_date <- as.character(Sys.Date() + 1)
  expect_error(validate_sample_map(bad, f$reg), "ISO date")

  # Research prepared by a machine is labelled as such wherever it is relied on.
  bad <- ok; bad$evidence_prepared_by <- NA_character_
  expect_error(validate_sample_map(bad, f$reg), "who prepared it")
})

test_that("a locator has to be checkable by somebody else", {
  # "fulltext: yes" is not evidence. The locator must name a place in a document
  # that another reader can turn to.
  f <- fixture()
  m <- f$smap
  m$evidence_status <- "evidenced"; m$evidence_type <- "fulltext"
  m$evidence_prepared_by <- "AI-assisted source check (Claude Opus 5)"
  m$accepted_by <- "R Twumasi"; m$accepted_date <- as.character(Sys.Date())
  for (vague in c("checked the paper", "confirmed by reading it", "yes", "the full text")) {
    m$evidence_locator <- vague
    expect_error(validate_sample_map(m, f$reg), "checkable locator", info = vague)
  }

  # Numbered locators, and the named structural sections in which a sample is
  # actually described. Journal articles do not number their sections, so
  # requiring a number would exclude the commonest admissible evidence, which is
  # a recruitment passage.
  for (good in c("Methods, Table 2", "p. 7", "Table 3", "Results, Sample",
                 "Method, Participants and Measures", "Subjects and study design",
                 "section 2.1 Participants", "Supplement, Figure S1")) {
    m$evidence_locator <- good
    expect_silent(validate_sample_map(m, f$reg))
  }
})

test_that("author confirmation is reserved for the study's own authors", {
  # It is the strongest claim available and the easiest to reach for loosely, so
  # it must at least identify the exchange it rests on.
  f <- fixture()
  m <- f$smap
  m$evidence_status <- "evidenced"; m$evidence_type <- "author_confirmation"
  m$evidence_prepared_by <- "R Twumasi"; m$accepted_by <- "R Twumasi"
  m$accepted_date <- as.character(Sys.Date())
  m$evidence_locator <- "confirmed"
  expect_error(validate_sample_map(m, f$reg), "without identifying it")

  m$evidence_locator <- "author e-mail, 2026-07-30"
  expect_silent(validate_sample_map(m, f$reg))
})

test_that("a mapping row cannot point at an undeclared cohort", {
  f <- fixture()
  f$reg <- f$reg[f$reg$sample_id != "S901", ]
  expect_error(validate_sample_map(f$smap, f$reg), "does not declare")
})

test_that("two mapping rows cannot address the same source row", {
  f <- fixture()
  f$smap$source_effect_id[2] <- f$smap$source_effect_id[1]
  expect_error(validate_sample_map(f$smap, f$reg), "same source row")
})

# ---- the release map ----------------------------------------------------------

test_that("the committed map reproduces the release identity assignment", {
  # data_sha pins every fit, so a migration that quietly reassigns an id
  # invalidates the promoted run. When a genuine split or merge lands, this test
  # is updated in the same commit as the map, and analysis_id changes with it.
  tab <- read.csv(file.path(ROOT, "data/inputs/analysis_table.csv"),
                  stringsAsFactors = FALSE)
  built <- do.call(rbind, LOCK$analysis)
  expect_setequal(paste(tab$pathway, tab$sample_id, tab$effect_id),
                  paste(built$pathway, built$sample_id, built$effect_id))
})

test_that("every eligible effect row is mapped exactly once", {
  expect_equal(nrow(LOCK$sample_map), nrow(LOCK$effects))
  expect_false(anyDuplicated(LOCK$sample_map$source_effect_id) > 0)
  expect_false(anyDuplicated(LOCK$effects$source_effect_id) > 0)
})
