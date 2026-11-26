## ---- synthesis eligibility and the protocol-specified conversion sensitivity --
##
## The PROSPERO pre-registration (CRD42024542972, version 2.0) allows a
## random-effects synthesis only when at least five studies examine the same
## mediator, trauma category and outcome, and asks that effect sizes obtained by
## conversion be recorded for sensitivity analysis. These tests encode both
## rules, and the NBR revision's source-verified data corrections.

test_that("every pathway is still extracted, but only eligible ones are synthesised", {
  # The extraction lock is provenance: dropping a pathway from it would hide
  # what was extracted and why it was not analysed.
  expect_setequal(names(LOCK$analysis), PATH_NAMES(CFG))
  el <- LOCK$synthesis_eligibility
  expect_setequal(el$pathway, PATH_NAMES(CFG))
  expect_setequal(SYNTH_NAMES(LOCK), el$pathway[el$eligible])
  # dissoc_cps has four independent cohorts, one short of the minimum.
  expect_false(el$eligible[el$pathway == "dissoc_cps"])
  expect_equal(el$k_independent[el$pathway == "dissoc_cps"], 4L)
  expect_equal(length(SYNTH_NAMES(LOCK)), 5L)
  expect_true(all(el$k_independent[el$eligible] >= CFG$synthesis$min_independent_cohorts))
})

test_that("the minimum counts independent cohorts, not rows", {
  # A cohort entered twice must not make up the number: five rows from four
  # samples is four cohorts and is ineligible; five distinct samples is eligible.
  mk <- function(ids) data.frame(sample_id = ids, stringsAsFactors = FALSE)
  an <- list(four_rows_four = mk(c("A", "B", "C", "D")),
             five_rows_four = mk(c("A", "B", "C", "D", "D")),
             five_distinct  = mk(c("A", "B", "C", "D", "E")))
  el <- synthesis_eligibility(CFG, an)
  expect_equal(el$eligible, c(FALSE, FALSE, TRUE))
  expect_equal(el$k_independent, c(4L, 4L, 5L))
})

test_that("no gate, plan or table can demand results for an ineligible pathway", {
  # gate_expected_reportable() reads reportability; if it saw dissoc_cps it
  # would demand contrasts that are, correctly, never fitted.
  rp <- synth_reportability(LOCK)
  expect_false("dissoc_cps" %in% rp$pathway)
  expect_setequal(unique(rp$pathway), SYNTH_NAMES(LOCK))
  plan <- build_fit_plan(CFG, LOCK)
  expect_false(any(plan$pathway == "dissoc_cps"))
  expect_false(any(grepl("dissoc_cps", plan$fit_id)))
  fq <- frequentist_slopes
  expect_true(is.function(fq))  # signature unchanged; it loops over SYNTH_NAMES
})

test_that("the conversion sensitivity keeps only direct correlations, and says when it cannot", {
  ds <- direct_r_summary(CFG, LOCK)
  st <- setNames(ds$status, ds$pathway)
  # Dissociation pathways keep at least five direct triplets, so they are fitted.
  expect_equal(unname(st["dissoc_hal"]), "fitted")
  expect_equal(unname(st["dissoc_del"]), "fitted")
  expect_equal(ds$dropped_samples[ds$pathway == "dissoc_hal"], "S002;S004;S006")
  expect_equal(ds$dropped_samples[ds$pathway == "dissoc_del"], "S006")
  # Negative-schema pathways rest mostly on converted or adjusted coefficients:
  # two direct triplets each, below the minimum, so the sensitivity is reported
  # as not estimable rather than fitted on two cohorts.
  expect_equal(unname(st["negsch_cps"]), "not estimable")
  expect_equal(unname(st["negsch_del"]), "not estimable")
  # Every depression triplet is direct, so the refit would be the primary model.
  expect_equal(unname(st["depress_cps"]), "identical")
  plan <- build_fit_plan(CFG, LOCK)
  dr <- plan[plan$fit_class == "sens_direct_r", ]
  expect_setequal(dr$pathway, c("dissoc_hal", "dissoc_del"))
  expect_true(all(dr$moderator == "null"))
  # The refit drops exactly the non-direct cohorts and nothing else.
  dd <- drop_samples(CFG, LOCK, "dissoc_hal", NULL, c("S002", "S004", "S006"))
  expect_equal(nrow(dd$df), 7L)
  expect_false(any(dd$df$sample_id %in% c("S002", "S004", "S006")))
  expect_equal(length(dd$vz), 7L)
  expect_error(drop_samples(CFG, LOCK, "dissoc_hal", NULL, "S999"), "not in the pathway")
})

test_that("an unclassified or stale triplet classification is fatal", {
  # Otherwise the sensitivity would silently keep or drop the wrong cohorts.
  src <- file.path(ROOT, "data/provenance/triplet_source.csv")
  ts <- read.csv(src, stringsAsFactors = FALSE)
  tmp <- tempfile(fileext = ".csv")
  write.csv(ts[-1, ], tmp, row.names = FALSE)
  expect_error(read_triplet_source(tmp, LOCK$analysis[SYNTH_NAMES(LOCK)]), "Unclassified")
  ts2 <- ts; ts2$source_type[1] <- "guessed"
  write.csv(ts2, tmp, row.names = FALSE)
  expect_error(read_triplet_source(tmp, LOCK$analysis[SYNTH_NAMES(LOCK)]), "unknown source_type")
})

test_that("deletions that fall below the minimum are labelled, not dropped", {
  # Removing Appiah (the only case-control contribution) from negsch_del leaves
  # four cohorts: a legitimate deletion sensitivity, but not a synthesis.
  plan <- build_fit_plan(CFG, LOCK)
  lo <- plan[plan$fit_class == "sens_loso_stan" & plan$moderator == "null", ]
  appiah <- lo[lo$pathway == "negsch_del" & lo$drop_sample == "S025", ]
  expect_equal(nrow(appiah), 1L)
  expect_equal(appiah$variant, "leave_one_out_below_minimum")
  expect_true(all(lo$variant[lo$pathway == "dissoc_hal"] == "leave_one_out"))
})

test_that("source-verified corrections reach the analysed triplets", {
  # Each value below was checked against the published table (ledger C013 to
  # C019). A regression here would silently restore a transcription error.
  g <- function(k, s) LOCK$analysis[[k]][LOCK$analysis[[k]]$sample_id == s, ]
  expect_equal(g("dissoc_hal", "S010")$female, 72)        # Pearce, not the mean age
  expect_equal(g("dissoc_del", "S010")$female, 72)
  expect_equal(g("dissoc_del", "S001")$r_XY, 0.44)        # Cole, CATS-PDI
  expect_equal(g("dissoc_del", "S001")$r_MY, 0.48)        # Cole, DES-II-PDI
  expect_equal(g("dissoc_hal", "S001")$r_MY, 0.57)        # Cole, DES-II-LSHS-R
  expect_equal(g("dissoc_hal", "S008")$r_MY, 0.58)        # Perona, DES total
  expect_equal(g("dissoc_del", "S008")$r_MY, 0.29)
  # Pearce remains clinical: the paper required a psychosis diagnosis or treatment.
  expect_equal(g("dissoc_hal", "S010")$clinical, 1)
})
