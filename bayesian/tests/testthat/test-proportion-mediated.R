# The percentage carried by the indirect path.
#
# This quantity was removed from a previous release and reinstated as a post hoc
# amendment, so the properties that made reinstatement defensible are asserted
# here rather than relied on. Each test below corresponds to a specific way the
# earlier implementation went wrong, or to a specific way this one could.

## ---- the summary must not be able to report moments -------------------------

test_that("ratio_summary reports no mean and no standard deviation", {
  # The ratio has no finite moments: the denominator is tanh of a coordinate
  # with unbounded support, so its density at zero is strictly positive. The
  # earlier release printed enormous standard deviations beside modest central
  # intervals, which is the symptom. A column named here would put a meaningless
  # number in a released table, so its absence is enforced rather than assumed.
  set.seed(11)
  s <- ratio_summary(rnorm(4000, 0.16, 0.04) / rnorm(4000, 0.29, 0.05))
  expect_false(any(names(s) %in% RATIO_FORBIDDEN))
  expect_true(all(c("median", "q_lo", "q_hi", "mcse_median",
                    "mcse_q_lo", "mcse_q_hi") %in% names(s)))
})

test_that("ratio_summary fails loudly on a non-finite draw", {
  # Dropping the draws where the denominator hit zero would turn a quantity that
  # does not exist over part of the posterior into a plausible-looking one. It
  # must raise, and the message must say how many draws were involved.
  expect_error(ratio_summary(c(0.5, 0.6, Inf)), "not finite")
  expect_error(ratio_summary(c(0.5, NaN)), "not finite")
  expect_error(ratio_summary(c(0.5, 0.6, Inf)), "1 of 3")
})

test_that("Monte Carlo error is reported at both interval endpoints, not only the median", {
  # An interval whose endpoints are resolved to fewer effective draws than its
  # centre is not adequately summarised by the median's Monte Carlo error alone.
  set.seed(12)
  s <- ratio_summary(rnorm(8000, 0.16, 0.04) / rnorm(8000, 0.29, 0.05))
  expect_true(is.finite(s$mcse_median))
  expect_true(is.finite(s$mcse_q_lo))
  expect_true(is.finite(s$mcse_q_hi))
})

test_that("per-chain medians and endpoints are retained when chains are supplied", {
  # A pooled median cannot be checked for between-chain agreement. For a
  # possibly multimodal ratio that is the check a reader most wants.
  set.seed(13)
  d <- rnorm(4000, 0.16, 0.04) / rnorm(4000, 0.29, 0.05)
  s <- ratio_summary(d, chain_id = rep(1:4, each = 1000))
  expect_equal(s$n_chains, 4L)
  expect_true(s$chain_median_min <= s$median && s$median <= s$chain_median_max)
  expect_equal(length(strsplit(s$chain_medians, " ")[[1]]), 4L)
})

## ---- units ------------------------------------------------------------------

test_that("the percentage is exactly one hundred times the fraction", {
  # The manuscript and the frequentist comparators are on 0 to 100; the natural
  # scale of ab / total is a fraction. Carrying one name for both invites a
  # hundredfold error at the point of transcription. A known example pins it.
  ind <- 0.16; total <- 0.32
  expect_equal(ind / total, 0.5, tolerance = 0)
  expect_equal(100 * (ind / total), 50, tolerance = 0)

  set.seed(14)
  num <- rnorm(2000, 0.16, 0.04); den <- rnorm(2000, 0.32, 0.05)
  s_prop <- ratio_summary(num / den, upper = 1)
  s_pct  <- ratio_summary(100 * num / den, upper = 100)
  expect_equal(s_pct$median, 100 * s_prop$median, tolerance = 1e-10)
  expect_equal(s_pct$q_lo,   100 * s_prop$q_lo,   tolerance = 1e-10)
  expect_equal(s_pct$q_hi,   100 * s_prop$q_hi,   tolerance = 1e-10)
})

test_that("the interpretable range follows the scale of the draws", {
  # This is a regression test for a real bug caught by the first smoke run. The
  # summary was applied to percentage draws while testing membership of (0, 1),
  # so a pathway with a median of 55 reported P(in range) = 0.0001 and would
  # have been withheld along with every other pathway. Applying a fraction's
  # range to a percentage is the 100-fold error in its most damaging form,
  # because it fails silently in the safe direction.
  set.seed(17)
  num <- rnorm(4000, 0.16, 0.04); den <- rnorm(4000, 0.29, 0.05)
  prop <- num / den

  expect_equal(ratio_summary(prop, upper = 1)$p_in_range,
               ratio_summary(100 * prop, upper = 100)$p_in_range, tolerance = 1e-12)

  # The wrong pairing must be visibly wrong, not marginally different.
  wrong <- ratio_summary(100 * prop, upper = 1)$p_in_range
  right <- ratio_summary(100 * prop, upper = 100)$p_in_range
  expect_lt(wrong, 0.01)
  expect_gt(right, 0.90)

  # The range that produced a probability is carried beside it, so a released
  # table can be audited without knowing which call site produced it.
  expect_equal(ratio_summary(100 * prop, upper = 100)$range_upper, 100)
  expect_error(ratio_summary(prop, upper = 0), "positive number")
  expect_error(ratio_summary(prop, upper = c(1, 100)), "single positive number")
})

## ---- the ratio must be draw-wise --------------------------------------------

test_that("shuffling the denominator draws changes the ratio, so the pairing is real", {
  # This is the test that makes "draw-wise" a claim with content. If the
  # extraction ever paired a numerator draw with a denominator draw from a
  # different iteration, the marginal summaries of numerator and denominator
  # would be unchanged and only the ratio would move. So the ratio must move.
  set.seed(15)
  num <- rnorm(6000, 0.16, 0.04); den <- rnorm(6000, 0.29, 0.05)
  paired   <- num / den
  shuffled <- num / sample(den)
  expect_false(isTRUE(all.equal(paired, shuffled)))
  # The marginals really are identical, which is what makes the ratio the only
  # place the mistake could ever have been caught.
  expect_equal(sort(den), sort(sample(den)))
  expect_true(abs(median(paired) - median(shuffled)) > 1e-6)
})

test_that("the recomputation tolerance admits CSV rounding and nothing more", {
  # extract_stan() compares the Stan quantity with ind/total on a RELATIVE
  # tolerance of 1e-3 rather than exactly, because cmdstan writes draws at six
  # significant figures and the numerator, denominator and ratio are each
  # rounded independently before R sees them. That is a real weakening of the
  # check, so the two regimes are pinned here: rounding must pass, and a
  # mispairing must fail by orders of magnitude.
  set.seed(16)
  ind <- rnorm(4000, 0.16, 0.04); total <- rnorm(4000, 0.29, 0.05)
  exact <- ind / total
  rel <- function(x, y) max(abs(x - y) / pmax(abs(x), 1e-12))

  # Six significant figures, as cmdstan stores them.
  sig6 <- function(x) signif(x, 6)
  rounded <- rel(sig6(exact), sig6(ind) / sig6(total))
  expect_lt(rounded, 1e-3)

  # A mispairing is not a near miss. It has to be visible without a threshold
  # judgement, or the tolerance above would be hiding it.
  mispaired <- rel(exact, ind / sample(total))
  expect_gt(mispaired, 1)
  expect_gt(mispaired / max(rounded, .Machine$double.eps), 1e3)
})

## ---- the reportability rule -------------------------------------------------

## A minimal stand-in for the collated pmed table, so the rule can be tested
## without a fit. The columns are exactly those extract_stan() produces.
fake_pmed <- function(pathway, median, q_lo, q_hi, p_unit, p_total_gt,
                      delta = CFG$proportion_mediated$delta_primary) {
  d <- data.frame(
    fit_id = paste0("stan|", pathway, "|null"), fit_class = "stan_primary",
    variant = "primary", drop_sample = NA_character_,
    pathway = pathway, label = pathway, moderator = "null",
    k_samples = 7L, reportable = "ok", contrast_index = 1L,
    quantity = "pmed_pct", scale = "per cent",
    median = median, q_lo = q_lo, q_hi = q_hi, q01 = q_lo, q99 = q_hi,
    range_upper = 100, p_in_range = p_unit, p_lt0 = 0, p_gt_upper = 0,
    p_abs_gt_twice_range = 0, p_gt0 = 1,
    mcse_median = 0.1, mcse_q_lo = 0.1, mcse_q_hi = 0.1,
    n_draws = 12000L, n_nonfinite = 0L,
    p_abs_total_lt_delta = 0, p_sign_consistent = p_unit,
    total_median = 0.3, total_sd = 0.05, total_median_sd_ratio = 6,
    stringsAsFactors = FALSE)
  for (g in unlist(CFG$proportion_mediated$delta_grid))
    d[[sprintf("p_total_gt_%g", g)]] <- p_total_gt
  d
}

fake_diagnostics <- function(pmed, pass = TRUE) {
  data.frame(fit_id = pmed$fit_id,
             max_rhat = if (pass) 1.001 else 1.2,
             min_ess_bulk = 5000, min_ess_tail = 5000,
             num_divergent = 0, num_max_treedepth = 0, min_ebfmi = 0.9,
             stringsAsFactors = FALSE)
}

test_that("a denominator that straddles zero fails the rule and carries no percentage", {
  # The case the whole rule exists for. The percentage is arithmetically
  # computable and would print as a modest-looking number; it must not be
  # released, and the withheld table must not contain it.
  p <- fake_pmed("straddler", median = 55, q_lo = -400, q_hi = 900,
                 p_unit = 0.42, p_total_gt = 0.61)
  out <- proportion_mediated_tables(CFG, p, fake_diagnostics(p))
  expect_equal(nrow(out$released), 0L)
  expect_equal(nrow(out$withheld), 1L)
  expect_false("median" %in% names(out$withheld))
  expect_match(out$withheld$failed_conditions, "P\\(total")
  expect_match(out$withheld$failed_conditions, "percentage")
})

test_that("a well separated pathway inside the unit interval is released", {
  p <- fake_pmed("clean", median = 55.7, q_lo = 26.0, q_hi = 96.7,
                 p_unit = 0.98, p_total_gt = 0.999)
  out <- proportion_mediated_tables(CFG, p, fake_diagnostics(p))
  expect_equal(nrow(out$released), 1L)
  expect_equal(nrow(out$withheld), 0L)
  expect_equal(out$released$median, 55.7)
})

test_that("a failed sampler diagnostic withholds the percentage on its own", {
  # Condition one. A ratio read off a fit that did not converge is not rescued
  # by the ratio's own posterior looking tidy.
  p <- fake_pmed("bad_fit", median = 55.7, q_lo = 26.0, q_hi = 96.7,
                 p_unit = 0.98, p_total_gt = 0.999)
  out <- proportion_mediated_tables(CFG, p, fake_diagnostics(p, pass = FALSE))
  expect_equal(nrow(out$released), 0L)
  expect_match(out$withheld$failed_conditions, "sampler diagnostics")
})

test_that("a median outside 0 to 100 is retained in the audit, not discarded", {
  # Inconsistent mediation and suppression put the ratio outside the unit
  # interval. That is a finding about the pathway, not a computational failure,
  # so it must fail the unit-interval condition on its merits and still appear.
  p <- fake_pmed("suppressed", median = 143, q_lo = 101, q_hi = 260,
                 p_unit = 0.02, p_total_gt = 0.999)
  out <- proportion_mediated_tables(CFG, p, fake_diagnostics(p))
  expect_equal(nrow(out$released), 0L)
  expect_equal(nrow(out$audit), 1L)
  expect_equal(out$audit$median, 143)
  expect_match(out$withheld$failed_conditions, "percentage")
})

test_that("released and withheld partition every pathway, with none lost", {
  # A silently dropped row would read as an absent result rather than a
  # withheld one. The audit must account for every pathway either way.
  p <- rbind(
    fake_pmed("a", 55, 26, 96, 0.98, 0.999),
    fake_pmed("b", 44, 20, 80, 0.97, 0.998),
    fake_pmed("c", 60, -300, 700, 0.40, 0.60),
    fake_pmed("d", 30, 10, 70, 0.96, 0.94))            # separation fails only
  out <- proportion_mediated_tables(CFG, p, fake_diagnostics(p))
  expect_equal(nrow(out$audit), 4L)
  expect_equal(nrow(out$released) + nrow(out$withheld), 4L)
  expect_equal(sort(c(out$released$pathway, out$withheld$pathway)),
               sort(p$pathway))
  expect_equal(length(intersect(out$released$pathway, out$withheld$pathway)), 0L)
  # Pathway d fails ONLY the separation condition, at 0.94 against 0.95.
  expect_match(out$withheld$failed_conditions[out$withheld$pathway == "d"],
               "^P\\(total")
})

test_that("moderator-endpoint rows never reach the percentage tables", {
  # A ratio conditional on a moderator value, in an analysis that concluded it
  # cannot establish moderation, invites exactly the over-reading this release
  # works to prevent.
  p <- fake_pmed("a", 55, 26, 96, 0.98, 0.999)
  p$moderator <- "female"
  expect_null(proportion_mediated_tables(CFG, p, fake_diagnostics(p)))
})

## ---- configuration ----------------------------------------------------------

test_that("the configuration rejects a threshold typed as a percentage", {
  # 95 instead of 0.95 would pass every comparison downstream silently, because
  # no probability ever reaches it and every pathway would be withheld.
  bad <- CFG
  bad$proportion_mediated$min_p_in_unit_interval <- 95
  expect_error(validate_config(bad), "probability")
})

test_that("the primary displacement must be one point of the sensitivity grid", {
  bad <- CFG
  bad$proportion_mediated$delta_primary <- 0.07
  expect_error(validate_config(bad), "delta_grid")
})

test_that("the released configuration reports the percentage for unmoderated models only", {
  expect_true(isTRUE(CFG$proportion_mediated$unmoderated_only))
  bad <- CFG
  bad$proportion_mediated$unmoderated_only <- FALSE
  expect_error(validate_config(bad), "moderator endpoint")
})

## ---- the fit plan gives the percentage its sensitivity analyses --------------

test_that("unmoderated models carry a prior grid and a cohort-deletion analysis", {
  # Both existed only inside the moderator branch. The percentage is read off
  # the unmoderated models, so without these it would be the one prominent
  # quantity in the release with no prior and no deletion sensitivity at all.
  plan <- build_fit_plan(CFG, LOCK)
  u <- plan[plan$moderator == "null", ]
  for (k in PATH_NAMES(CFG)) {
    g <- u[u$pathway == k, ]
    expect_gt(sum(g$fit_class == "sens_prior_grid"), 0)
    expect_equal(sum(g$fit_class == "sens_loso_stan"),
                 length(unique(LOCK$analysis[[k]]$sample_id)))
  }
  # beta_full_sd is inert when P = 0, so varying it would refit an identical
  # model and publish the agreement as evidence.
  expect_false(any(grepl("beta_full_sd", u$variant[u$fit_class == "sens_prior_grid"])))
  # The brms layer models zero-order correlations and recovers no path
  # decomposition, so it has no ratio to be sensitive about.
  expect_equal(sum(u$fit_class == "sens_loso_brms"), 0)
})

test_that("every unmoderated cohort deletion can rebuild its moderator structures", {
  # Deletions used to exist only where loso_is_estimable() permitted them. The
  # unmoderated branch deletes unconditionally, so the rebuild has to survive
  # subsets that branch never produced.
  plan <- build_fit_plan(CFG, LOCK)
  rows <- plan[plan$moderator == "null" & plan$fit_class == "sens_loso_stan", ]
  expect_gt(nrow(rows), 0)
  for (i in seq_len(nrow(rows)))
    expect_silent(drop_one_sample(CFG, LOCK, rows$pathway[i], NULL, rows$drop_sample[i]))
})

## ---- the Stan programme -----------------------------------------------------

test_that("the Stan model computes the ratio without guarding the division", {
  # Clamping a near-zero denominator would hide precisely the information the
  # reportability rule consumes, and would make a withheld pathway look
  # reportable.
  src <- readLines(file.path(ROOT, "bayesian", "stan", "masem_valid.stan"))
  expect_true(any(grepl("pmed_prop[c] = ind_pop[c] / total_pop[c];", src, fixed = TRUE)))
  expect_true(any(grepl("pmed_pct[c]  = 100 * pmed_prop[c];", src, fixed = TRUE)))
  # No predictive percentage: the existing prediction intervals are for ab.
  expect_false(any(grepl("pmed_new", src, fixed = TRUE)))
})

test_that("the ratio is kept out of the moment-based diagnostic gate", {
  # fit_diagnostics() requests mcse_mean and mcse_sd for everything it audits,
  # and reduces its variables to a single maximum the release gate judges. Both
  # are wrong for this quantity: the first is undefined, and the second would
  # let a deliberately withheld pathway fail the entire release.
  expect_false(any(grepl("pmed", GATED_PARS)))
  expect_false(any(grepl("pmed", STOCHASTIC_PARS)))
})
