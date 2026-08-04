# The document verifier, and the reporting counts it checks.
#
# The defect these guard: check_documents() blocked four literal phrases, none
# of which was the sentence the methods document actually printed. It said "The
# analytical unit was one independent participant sample", with no
# qualification, while all 32 registry rows were derived_unverified, and the
# check passed.

stub_lock <- function(n = 4L, evidenced = 0L, disputed = 0L, disputed_excluded = TRUE) {
  status <- c(rep("evidenced", evidenced), rep("disputed", disputed),
             rep("unevidenced", n - evidenced - disputed))
  smap <- data.frame(
    sample_id = sprintf("S%03d", seq_len(n)),
    evidence_status = status,
    stringsAsFactors = FALSE)
  samples <- data.frame(sample_id = smap$sample_id,
                        verification_status = derive_cohort_status(smap$sample_id, smap),
                        stringsAsFactors = FALSE)
  ## One effect row per sample_id, standing in for lock$effects. Only the
  ## disputed sample_id(s)' `excluded` flag is ever consulted, by
  ## identity_caveat()'s "everything checked, one disputed" branch.
  effects <- data.frame(sample_id = smap$sample_id,
                        excluded = ifelse(smap$evidence_status == "disputed",
                                          disputed_excluded, TRUE),
                        stringsAsFactors = FALSE)
  list(samples = samples, sample_map = smap, effects = effects)
}

## A document containing only what the test puts in it, plus the caveat unless
## the test is about the caveat being absent.
doc <- function(lines, lock, with_caveat = TRUE) {
  p <- tempfile(fileext = ".md")
  if (with_caveat)
    lines <- c(lines, "", identity_caveat(lock$samples, lock$sample_map, lock$effects))
  writeLines(lines, p)
  p
}

test_that("an unqualified independence assertion is blocked while any identity is unevidenced", {
  lock <- stub_lock()
  p <- doc("The analytical unit was one independent participant sample, not one publication.",
           lock)
  expect_error(check_documents(p, list(), CFG, lock = lock), "unqualified assertion")
})

test_that("the same assertion passes once qualified in the following sentence", {
  # The rule has to be about proximity, not about banning the vocabulary. A
  # paper must be able to describe the limitation it is under.
  lock <- stub_lock()
  p <- doc(paste("The intended analytical unit was one independent participant sample.",
                 "That identity is unevidenced for every cohort and rests on the author",
                 "and year printed in the extraction workbook."), lock)
  expect_silent(invisible(check_documents(p, list(), CFG, lock = lock)))
})

test_that("the generated identity caveat must appear verbatim", {
  # A checker that only forbids phrasings passes a document that says nothing at
  # all, which is the other way to mislead a reader about the analytical unit.
  lock <- stub_lock()
  p <- doc("Nothing of interest is claimed here.", lock, with_caveat = FALSE)
  expect_error(check_documents(p, list(), CFG, lock = lock), "caveat is absent")

  # An edited count is an edited claim.
  cav <- identity_caveat(lock$samples, lock$sample_map, lock$effects)
  p2 <- doc(sub("0 of 4", "3 of 4", cav), lock, with_caveat = FALSE)
  expect_error(check_documents(p2, list(), CFG, lock = lock), "caveat is absent")
})

test_that("a fully evidenced registry allows the assertion and switches the caveat", {
  # The check must not harden into a permanent claim that the work was never
  # done. When every assertion is evidenced, the caveat states that instead.
  lock <- stub_lock(evidenced = 4L)
  expect_match(identity_caveat(lock$samples, lock$sample_map, lock$effects),
               "all 4 report-to-cohort assertions were checked")
  p <- doc("The analytical unit was one independent participant sample.", lock)
  expect_silent(invisible(check_documents(p, list(), CFG, lock = lock)))
})

test_that("zero unevidenced but one disputed and excluded states the analysed-set claim", {
  # Not the same as "all evidenced": one assertion is disputed rather than
  # evidenced, so the strongest sentence (identity_caveat's first branch) would
  # overclaim. But it is also not "some remain unevidenced" (nu == 0), so the
  # generic fallback sentence used to print "the remaining 0 rest on the
  # author and year... and have not been checked", which contradicts itself.
  lock <- stub_lock(n = 4L, evidenced = 3L, disputed = 1L, disputed_excluded = TRUE)
  cav <- identity_caveat(lock$samples, lock$sample_map, lock$effects)
  expect_match(cav, "All 4 report-to-cohort assertions have been checked")
  expect_match(cav, "3 are evidenced and 1 is disputed")
  expect_match(cav, "no assertion remains unevidenced")
  expect_match(cav, "excluded from the analysed set")
  expect_no_match(cav, "remaining 0")
})

test_that("a disputed identity that has not been excluded from analysis stops the build", {
  # The stronger sentence claims every analysed mapping is evidenced. That
  # claim is false if a disputed cohort is still contributing a live row, so
  # identity_caveat() must refuse to print it rather than describe an
  # unresolved dispute as settled.
  lock <- stub_lock(n = 4L, evidenced = 3L, disputed = 1L, disputed_excluded = FALSE)
  expect_error(identity_caveat(lock$samples, lock$sample_map, lock$effects),
              "remain in the live analysis")
})

test_that("a disputed identity with no matching effect row is not vacuously excluded", {
  # all() over zero rows is TRUE in R. Without an explicit row-count check, a
  # disputed sample_id absent from `effects` entirely (a typo, or a cohort
  # dropped from the analysis for an unrelated reason before anyone checked
  # this) would pass the exclusion test by having nothing to fail it.
  lock <- stub_lock(n = 4L, evidenced = 3L, disputed = 1L, disputed_excluded = TRUE)
  lock$effects <- lock$effects[lock$effects$sample_id != "S004", , drop = FALSE]
  expect_error(identity_caveat(lock$samples, lock$sample_map, lock$effects),
              "no effect row could be found")
})

test_that("every blocked pattern matches the phrasing it exists to block", {
  # A decorative pattern list that matches nothing passes vacuously and looks
  # like protection.
  examples <- c(
    "\\bindependent (participant |cohort )?samples?\\b" = "one independent participant sample",
    "\\bindependent cohorts?\\b"                        = "thirty-two independent cohorts",
    "\\banalytical unit\\b"                             = "the analytical unit was a cohort",
    "\\bnon-?overlapping (samples|cohorts|participants)\\b" = "non-overlapping samples throughout",
    "\\bno (participant |sample )?overlap\\b"           = "there was no participant overlap",
    "\\bdistinct (samples|cohorts|participants)\\b"     = "five distinct cohorts contributed",
    "\\b(each|every) sample (is|was) independent\\b"    = "every sample was independent",
    "\\bverified (independent |analytical |sample |cohort )" = "a verified independent sample",
    "\\bindependently verified\\b"                      = "identities were independently verified",
    "\\bcurated (sample )?registry\\b"                  = "the curated sample registry defines it",
    "\\b(identity|identities) (was|were|is|are) (verified|confirmed)\\b" = "identities were confirmed",
    "\\bconfirmed (independent|identity|cohort)"        = "confirmed independent cohorts")
  for (pat in names(examples))
    expect_true(grepl(pat, examples[[pat]], ignore.case = TRUE), info = pat)

  qualifiers <- c("derived_unverified", "these are unevidenced", "the key is a heuristic",
                  "identity has not been externally verified",
                  "derived from the author and year printed in the workbook",
                  "awaiting author sign-off",
                  "curated in `data/provenance/sample_map.csv`")
  for (q in qualifiers)
    expect_true(any(vapply(INDEPENDENCE_QUALIFIERS, grepl, logical(1), x = q,
                           ignore.case = TRUE)), info = q)
})

# ---- power scaling ------------------------------------------------------------

test_that("a blank power-scaling diagnosis is not a flag", {
  # priorsense uses three states and the prose used two. "-" means evaluated and
  # unflagged; a BLANK diagnosis means the quantity could not be evaluated at
  # all, because d_z is a moderator contrast and an intercept-only model has no
  # contrast to span. Counting flagged rows as `diagnosis != "-"` therefore
  # counted every non-evaluable row as a positive flag, and the release reported
  # 164 of 342 flagged where the truth was 146 of 324 evaluable.
  ps <- data.frame(
    fit_id = "stan|x|null",
    variable = c("alpha[1]", "tau[1]", "d_z[1]", "d_z[2]", "beta[1]"),
    diagnosis = c("-", "potential prior-data conflict", "", NA_character_,
                  "potential strong prior / weak likelihood"),
    stringsAsFactors = FALSE)
  st <- powerscale_status(ps)
  expect_equal(st$ps_status,
               c("unflagged", "flagged", "inapplicable", "inapplicable", "flagged"))
  expect_equal(sum(st$ps_status == "flagged"), 2L)
  expect_equal(sum(st$ps_status != "inapplicable"), 3L)

  # NA and "" must not be distinguished: which one appears is an artefact of how
  # the CSV was written, not a fact about the diagnostic.
  expect_equal(st$ps_status[3], st$ps_status[4])

  # The old expression, kept here so the regression is stated rather than
  # implied. It counts four flags where there are two.
  expect_equal(sum(ps$diagnosis != "-", na.rm = TRUE), 3L)
})

test_that("power-scaling status covers every row exactly once", {
  ps <- data.frame(fit_id = "f", variable = letters[1:6],
                   diagnosis = c("-", "-", "", NA, "conflict", "strong prior"),
                   stringsAsFactors = FALSE)
  st <- powerscale_status(ps)
  expect_equal(nrow(st), nrow(ps))
  expect_true(all(st$ps_status %in% c("flagged", "unflagged", "inapplicable")))
  expect_equal(sum(table(st$ps_status)), nrow(ps))
})

# ---- SBC denominators ---------------------------------------------------------

test_that("the primary SBC denominator excludes diagnostically invalid fits", {
  # The README claimed failed fits stayed in the denominator while the code
  # excluded them. The code was right, and this states which is which so the two
  # cannot drift apart again.
  ranks <- data.frame(
    scenario = "s1", replicate = rep(1:4, each = 2),
    parameter = rep(c("alpha[1]", "tau[1]"), 4),
    truth = 0, rank = c(1:6, NA, NA),
    status = rep(c("ok", "ok", "diagnostics failed", "fit failed"), each = 2),
    stringsAsFactors = FALSE)
  s <- sbc_summary(ranks, n_bins = 7, n_cells = 2)

  expect_equal(s$by_scenario$attempted, 4L)
  expect_equal(s$by_scenario$used_primary, 2L)     # the two ok replicates
  expect_equal(s$by_scenario$used_all_ranks, 3L)   # plus the one with a rank but bad diagnostics
  expect_equal(s$audit$total_fits, 4L)
  expect_equal(s$audit$fits_failed, 1L)
  expect_equal(s$audit$fits_diag_failed, 1L)
  # Every attempted fit stays in the accounting, which is what makes the
  # exclusion visible rather than flattering.
  expect_equal(s$audit$total_fits,
               s$by_scenario$used_primary + s$audit$fits_diag_failed + s$audit$fits_failed)
})
