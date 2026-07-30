# Scale translation. The prior and the smallest effect size of interest are both
# anchored on the nominal scientific contrast; a shortened supported contrast
# must keep the same per-unit threshold rather than being granted a looser one.

test_that("the nominal contrast spans the expected number of fitted units", {
  expect_equal(scale_spec(CFG, "age")$nominal_span, 1)
  expect_equal(scale_spec(CFG, "nos")$nominal_span, 1)
  expect_equal(scale_spec(CFG, "clinical")$nominal_span, 1)
  expect_equal(scale_spec(CFG, "female")$nominal_span, 2)
})

test_that("the SESOI divides down to the coefficient scale", {
  # 0.05 Fisher-z across 40 to 60 per cent female is 0.025 per ten points.
  expect_equal(scale_spec(CFG, "female")$sesoi_per_unit, 0.025)
  expect_equal(scale_spec(CFG, "age")$sesoi_per_unit, 0.05)
})

test_that("a clamped contrast keeps the per-unit threshold and scales its bound", {
  # This is the correction the release review required. Re-anchoring 0.05 onto
  # each shortened contrast would silently redefine the scientific threshold and
  # make short contrasts easier to declare interesting.
  s <- scale_spec(CFG, "female", c(52, 60))
  expect_equal(s$supported_span, 0.8)
  expect_equal(s$sesoi_per_unit, 0.025)          # unchanged
  expect_equal(s$sesoi_reported, 0.020)          # 0.025 * 0.8, not 0.05
  expect_true(s$clamped)

  s2 <- scale_spec(CFG, "female", c(42.4, 60))
  expect_equal(s2$supported_span, 1.76)
  expect_equal(s2$sesoi_per_unit, 0.025)
  expect_equal(s2$sesoi_reported, 0.044, tolerance = 1e-12)
})

test_that("an unclamped contrast reports the full nominal bound", {
  s <- scale_spec(CFG, "female", c(40, 60))
  expect_false(s$clamped)
  expect_equal(s$sesoi_reported, CFG$decision$sesoi_full)
  expect_equal(scale_spec(CFG, "age", c(20, 30))$sesoi_reported, CFG$decision$sesoi_full)
})

test_that("the prior uses the same anchor as the SESOI", {
  # A moderator must not be more or less regularised purely because of its unit.
  expect_equal(scale_spec(CFG, "female")$prior_beta_unit_sd,
               CFG$priors$primary$beta_full_sd / 2)
  expect_equal(scale_spec(CFG, "age")$prior_beta_unit_sd,
               CFG$priors$primary$beta_full_sd)
  # On the full-contrast scale every moderator shares one prior.
  for (m in MOD_NAMES(CFG)) {
    s <- scale_spec(CFG, m)
    expect_equal(s$prior_beta_unit_sd * s$nominal_span, CFG$priors$primary$beta_full_sd)
    expect_equal(s$sesoi_per_unit * s$nominal_span, CFG$decision$sesoi_full)
  }
})

test_that("the prior grid varies the scientific scale, not the coefficient scale", {
  for (b in unlist(CFG$priors$grid$beta_full_sd)) {
    expect_equal(prior_unit_sd_for(CFG, "female", b), b / 2)
    expect_equal(prior_unit_sd_for(CFG, "nos", b), b)
  }
})

test_that("a clamped contrast is never given the nominal label", {
  expect_equal(contrast_label(scale_spec(CFG, "female", c(52, 60))), "52 to 60")
  expect_equal(contrast_label(scale_spec(CFG, "female")), "40 to 60")
  expect_equal(contrast_label(scale_spec(CFG, "clinical")), "non-clinical vs clinical")
})

test_that("the lock records the clamped contrasts the data actually support", {
  s <- LOCK$support
  cl <- s[s$clamped, ]
  expect_true(all(cl$moderator == "female"))
  for (i in seq_len(nrow(cl))) {
    d <- LOCK$analysis[[cl$pathway[i]]]
    # The clamped bound must lie inside observed support.
    expect_gte(cl$supported_lo[i], min(d$female, na.rm = TRUE) - 1e-9)
    expect_lte(cl$supported_hi[i], max(d$female, na.rm = TRUE) + 1e-9)
    expect_equal(cl$sesoi_reported[i], cl$sesoi_per_unit[i] * cl$supported_span[i])
  }
  # A not-reported model is not permitted a decision.
  r <- LOCK$reportability
  for (i in seq_len(nrow(r))) {
    sp <- s[s$pathway == r$pathway[i] & s$moderator == r$moderator[i], ]
    expect_equal(sp$decision_permitted, r$status[i] != "not reported")
  }
})

test_that("fit seeds are deterministic and distinct", {
  expect_equal(fit_seed(CFG, "stan_dissoc_hal_age"), fit_seed(CFG, "stan_dissoc_hal_age"))
  expect_false(fit_seed(CFG, "stan_dissoc_hal_age") == fit_seed(CFG, "stan_dissoc_hal_nos"))
})

test_that("a configuration that re-enables Bayes factors is rejected", {
  bad <- CFG; bad$bayes_factors <- TRUE
  expect_error(validate_config(bad), "Bayes factors are removed")
})
