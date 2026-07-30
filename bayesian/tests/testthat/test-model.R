# The valid-correlation transform, the matched estimand and the fit plan.
# These are the assertions that stop the two model layers from being compared on
# different quantities.

test_that("the valid-correlation map always yields a positive-definite matrix", {
  set.seed(1)
  for (i in 1:400) {
    eta <- rnorm(3, 0, 2)                   # deliberately far into the tails
    rho <- eta_to_rho_R(eta)
    expect_true(all(abs(rho) < 1))
    R <- matrix(c(1, rho[1], rho[2], rho[1], 1, rho[3], rho[2], rho[3], 1), 3, byrow = TRUE)
    expect_gt(min(eigen(R, symmetric = TRUE, only.values = TRUE)$values), 0)
  }
})

test_that("path coefficients agree with an independent solve() route", {
  set.seed(2)
  for (i in 1:200) {
    rho <- eta_to_rho_R(rnorm(3))
    expect_equal(rho_to_path_R(rho), rho_to_path_solve(rho), tolerance = 1e-12)
  }
})

test_that("the total effect equals r_XY exactly", {
  # total = c' + ab = r_XY is an identity of the standardised model, not an
  # approximation. If it drifts, the recovery algebra is wrong.
  set.seed(3)
  for (i in 1:200) {
    rho <- eta_to_rho_R(rnorm(3))
    p <- rho_to_path_R(rho)
    expect_equal(p[["cprime"]] + p[["indirect"]], rho[2], tolerance = 1e-12)
    expect_equal(p[["total"]], rho[2], tolerance = 1e-12)
  }
})

test_that("eta_to_z inverts to the zero-order correlations", {
  set.seed(4)
  for (i in 1:100) {
    eta <- rnorm(3)
    expect_equal(tanh(eta_to_z_R(eta)), eta_to_rho_R(eta), tolerance = 1e-12)
  }
})

test_that("the helpers strip names from posterior draws", {
  # Draws extracted from a posterior matrix carry names like "alpha[1]". If they
  # propagate, name-based lookup downstream silently returns the wrong element.
  eta <- c(`alpha[1]` = 0.3, `alpha[2]` = 0.2, `alpha[3]` = 0.1)
  expect_null(names(eta_to_rho_R(eta)))
  expect_equal(names(rho_to_path_R(eta_to_rho_R(eta))),
               c("a", "b", "cprime", "indirect", "total"))
})

test_that("d_z is linear in beta for the first two coordinates only", {
  # This is why the matched estimand exists. Coordinates one and two are linear
  # in eta, so their full-contrast change is exactly beta * span. The third is
  # reached through the nonlinear valid-correlation map, so the model's own
  # beta[3] is NOT the zero-order r_MY slope and must never be tabulated as one.
  set.seed(5)
  alpha <- c(0.4, 0.3, 0.35); beta <- c(-0.2, -0.06, -0.15); span <- 1
  dz <- eta_to_z_R(alpha + beta * span) - eta_to_z_R(alpha)
  expect_equal(dz[1], beta[1] * span, tolerance = 1e-12)
  expect_equal(dz[2], beta[2] * span, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(dz[3], beta[3] * span, tolerance = 1e-6)))
})

test_that("compare_posteriors does not pair draws from separate fits", {
  # Two identical marginals in different draw ORDER must compare as identical.
  # An implementation that paired draw i with draw i would report a spurious
  # difference here.
  set.seed(6)
  x <- rnorm(4000, 0.1, 0.05)
  y <- sample(x)
  cmp <- compare_posteriors(x, y)
  expect_equal(cmp$median_diff, 0, tolerance = 1e-12)
  expect_equal(cmp$sd_ratio, 1, tolerance = 1e-12)
  expect_gt(cmp$density_overlap, 0.99)

  # And genuinely different marginals must separate.
  cmp2 <- compare_posteriors(x, rnorm(4000, 0.4, 0.05))
  expect_gt(abs(cmp2$median_diff_in_pooled_sd), 3)
  expect_lt(cmp2$density_overlap, 0.1)
})

test_that("dienes_summary applies the Kruschke rules and finds the robustness region", {
  grid <- seq(0.01, 0.30, by = 0.005)
  reject <- dienes_summary(rnorm(8000, 0.5, 0.02), sesoi = 0.05, sesoi_grid = grid)
  expect_equal(reject$decision, "reject null interval")
  accept <- dienes_summary(rnorm(8000, 0, 0.002), sesoi = 0.05, sesoi_grid = grid)
  expect_equal(accept$decision, "accept null interval")
  undec <- dienes_summary(rnorm(8000, 0.04, 0.05), sesoi = 0.05, sesoi_grid = grid)
  expect_equal(undec$decision, "undecided")
  expect_true(reject$rr_lower <= 0.05 && reject$rr_upper >= 0.05)
})

test_that("decision = FALSE returns estimation with no verdict", {
  s <- dienes_summary(rnorm(4000, 0.5, 0.01), decision = FALSE)
  expect_equal(s$decision, "estimation only")
  expect_true(is.na(s$sesoi))
  expect_true(is.finite(s$median) && is.finite(s$hdi_lower))
})

test_that("a shorter contrast is not granted a looser test", {
  # Same posterior, two contrast lengths. Under the preserved per-unit
  # threshold the shorter contrast has the tighter bound, so it cannot be
  # easier to declare interesting.
  draws <- rnorm(8000, 0.03, 0.005)
  full <- scale_spec(CFG, "female", c(40, 60))
  short <- scale_spec(CFG, "female", c(52, 60))
  expect_lt(short$sesoi_reported, full$sesoi_reported)
  expect_equal(dienes_summary(draws, sesoi = full$sesoi_reported)$decision,
               "accept null interval")
  expect_equal(dienes_summary(draws, sesoi = short$sesoi_reported)$decision,
               "reject null interval")
})

test_that("the fit plan is well formed and derives its own size", {
  plan <- build_fit_plan(CFG, LOCK)
  expect_equal(anyDuplicated(plan$fit_id), 0L)
  expect_true(all(plan$layer %in% c("stan", "brms")))
  expect_setequal(unique(plan$pathway), PATH_NAMES(CFG))

  # Both layers get every pathway-by-moderator model, including the null.
  n_models <- length(PATH_NAMES(CFG)) * (length(MOD_NAMES(CFG)) + 1L)
  expect_equal(sum(plan$fit_class == "stan_primary"), n_models)
  expect_equal(sum(plan$fit_class == "brms_primary"), n_models)

  # A leave-one-out row exists only for a study actually in that model, so the
  # plan is not a Cartesian product.
  loso <- plan[plan$fit_class == "sens_loso_stan", ]
  for (i in seq_len(nrow(loso))) {
    m <- loso$moderator[i]
    present <- LOCK$analysis[[loso$pathway[i]]]$sample_id[LOCK$mods[[loso$pathway[i]]][[m]]$keep]
    expect_true(loso$drop_sample[i] %in% present)
  }

  # An alternative-instrument row exists only where a second instrument does.
  alt <- unique(plan$pathway[plan$fit_class == "sens_alt_instrument"])
  expect_setequal(alt, names(LOCK$alternative)[!vapply(LOCK$alternative, is.null, logical(1))])
})

test_that("the completeness gate fails on a missing or duplicated fit", {
  plan <- data.frame(fit_id = c("a", "b", "c"), stringsAsFactors = FALSE)
  full <- data.frame(fit_id = c("a", "b", "c"), status = "ok", stringsAsFactors = FALSE)
  expect_true(gate_fit_completeness(plan, full)$ok)

  short <- full[1:2, ]
  expect_error(gate_fit_completeness(plan, short), "Missing 1")

  extra <- rbind(full, data.frame(fit_id = "d", status = "ok"))
  expect_error(gate_fit_completeness(plan, extra), "Unexpected 1")

  failed <- full; failed$status[2] <- "error"
  expect_error(gate_fit_completeness(plan, failed), "Missing 1")
})

test_that("the diagnostic gate fails loudly rather than reporting success", {
  # The previous pipeline's verifiers printed their failures and returned zero,
  # which is how a mismatch reached a results document.
  good <- data.frame(fit_id = "x", max_rhat = 1.001, min_ess_bulk = 2000,
                     min_ess_tail = 2000, num_divergent = 0, num_max_treedepth = 0,
                     min_ebfmi = 0.8, escalations = 0)
  expect_true(gate_diagnostic_thresholds(CFG, list(diagnostics = good))$ok)

  for (field in c("max_rhat", "min_ess_tail", "num_max_treedepth", "min_ebfmi")) {
    bad <- good
    bad[[field]] <- switch(field, max_rhat = 1.05, min_ess_tail = 50,
                           num_max_treedepth = 4, min_ebfmi = 0.05)
    expect_error(gate_diagnostic_thresholds(CFG, list(diagnostics = bad)),
                 "breached a convergence threshold")
  }
  expect_error(gate_diagnostic_thresholds(CFG, list(diagnostics = good[0, ])),
               "No diagnostics")
})

test_that("a primary fit gets no divergence tolerance at all", {
  # A reported result must rest on a pristine fit. The tolerance below exists for
  # sensitivity refits and must never reach a fit whose numbers are published.
  base <- data.frame(fit_id = "x", fit_class = "stan_primary", max_rhat = 1.001,
                     min_ess_bulk = 2000, min_ess_tail = 2000, num_divergent = 1,
                     num_max_treedepth = 0, min_ebfmi = 0.8, escalations = 2)
  expect_error(gate_diagnostic_thresholds(CFG, list(diagnostics = base)),
               "PRIMARY fits retained divergent")
  base$fit_class <- "brms_primary"
  expect_error(gate_diagnostic_thresholds(CFG, list(diagnostics = base)),
               "PRIMARY fits retained divergent")
  base$fit_class <- "prior_predictive"
  expect_error(gate_diagnostic_thresholds(CFG, list(diagnostics = base)),
               "PRIMARY fits retained divergent")
})

test_that("a sensitivity refit is tolerated only under the declared policy", {
  mk <- function(n, div, esc = 2, cls = "sens_loso_brms")
    data.frame(fit_id = paste0("s", seq_len(n)), fit_class = cls, max_rhat = 1.001,
               min_ess_bulk = 2000, min_ess_tail = 2000,
               num_divergent = div, num_max_treedepth = 0, min_ebfmi = 0.8,
               escalations = esc)

  # 200 sensitivity fits, cap is one per cent, so two may retain divergences.
  d <- mk(200, c(1, 1, rep(0, 198)))
  out <- gate_diagnostic_thresholds(CFG, list(diagnostics = d))
  expect_true(out$ok)
  expect_equal(out$sensitivity_fits_with_divergence, 2)
  expect_equal(out$sensitivity_divergence_cap, 2)
  expect_match(out$tolerated, "s1 \\(1\\)")

  # Three exceeds the cap.
  expect_error(gate_diagnostic_thresholds(CFG,
                 list(diagnostics = mk(200, c(1, 1, 1, rep(0, 197))))),
               "above the declared cap")

  # A single fit with many divergences is not a near miss.
  expect_error(gate_diagnostic_thresholds(CFG,
                 list(diagnostics = mk(200, c(50, rep(0, 199))))),
               "per-fit divergence limit")

  # Tolerance applies only after the sampler has been pushed as far as it goes.
  expect_error(gate_diagnostic_thresholds(CFG,
                 list(diagnostics = mk(200, c(1, rep(0, 199)), esc = 0))),
               "without exhausting the adapt_delta ladder")
})

test_that("rbind_fill unions columns without inventing values", {
  a <- data.frame(x = 1, y = 2)
  b <- data.frame(x = 3, z = 4)
  out <- rbind_fill(list(a, b, NULL))
  expect_setequal(names(out), c("x", "y", "z"))
  expect_equal(out$x, c(1, 3))
  expect_true(is.na(out$z[1]) && is.na(out$y[2]))
})

test_that("SBC ranks stay inside their support and ties are broken at random", {
  set.seed(7)
  draws <- rnorm(500)
  r <- replicate(200, sbc_rank(0, draws))
  expect_true(all(r >= 0 & r <= 500))
  expect_equal(sbc_rank(-Inf, draws), 0L)
  expect_equal(sbc_rank(Inf, draws), 500L)
  # With ties present the rank must not be deterministic.
  tied <- rep(0, 100)
  expect_gt(length(unique(replicate(50, sbc_rank(0, tied)))), 1)
  expect_true(is.na(sbc_rank(NA_real_, draws)))
})

test_that("the moderator vector is masked identically to the data", {
  # negsch_cps has seven studies but only six report mean age. Passing the
  # unmasked moderator gives Stan seven values for six rows, which it rejects at
  # data initialisation with every chain failing at once and an error naming no
  # fit. Only a model with an incomplete moderator exposes this.
  for (key in PATH_NAMES(CFG)) {
    for (m in MOD_NAMES(CFG)) {
      sd_ <- make_stan_data(CFG, LOCK$analysis[[key]], LOCK$vz_chol[[key]],
                            m, LOCK$mods[[key]])
      expect_equal(length(sd_$x), sd_$K, info = paste(key, m))
      expect_equal(nrow(sd_$z), sd_$K, info = paste(key, m))
      expect_equal(dim(sd_$L_V)[1], sd_$K, info = paste(key, m))
      expect_true(all(is.finite(sd_$x)), info = paste(key, m))
    }
    sd0 <- make_stan_data(CFG, LOCK$analysis[[key]], LOCK$vz_chol[[key]])
    expect_equal(length(sd0$x), sd0$K, info = key)
    expect_equal(sd0$P, 0L)
  }

  # And the guard fires when the moderator structure describes a different set
  # of studies than the data. The sensitivity classes rebuild that structure for
  # a subset, so a mismatched pair is a real possibility rather than a
  # hypothetical one.
  expect_error(
    make_stan_data(CFG, LOCK$analysis$negsch_cps[1:3, ], LOCK$vz_chol$negsch_cps[1:3],
                   "age", LOCK$mods$negsch_cps),
    "must be rebuilt for this subset")

  # Rebuilding it for that subset is what the sensitivity path actually does,
  # and it must then succeed.
  sub <- LOCK$analysis$negsch_cps[1:3, ]
  ok <- make_stan_data(CFG, sub, LOCK$vz_chol$negsch_cps[1:3], "age",
                       rebuild_mods(CFG, sub))
  expect_equal(length(ok$x), ok$K)
})

test_that("negsch_cps age, the only incomplete moderator, has six studies of seven", {
  # Guards the premise of the test above: if the data change so that every
  # moderator is complete, the masking test stops exercising anything and this
  # failure says so rather than leaving it silently vacuous.
  n_incomplete <- 0
  for (key in PATH_NAMES(CFG))
    for (m in MOD_NAMES(CFG))
      if (sum(LOCK$mods[[key]][[m]]$keep) < nrow(LOCK$analysis[[key]]))
        n_incomplete <- n_incomplete + 1
  expect_gt(n_incomplete, 0)
  expect_equal(sum(LOCK$mods$negsch_cps$age$keep), 6L)
  expect_equal(nrow(LOCK$analysis$negsch_cps), 7L)
})

test_that("summaries and PPCs use the fitted subset, not the whole pathway", {
  # negsch_cps age fits six of seven studies. Passing the full pathway frame to
  # the extraction and posterior predictive code indexes replicated quantities
  # past the end of the draws, and reports k as seven when six were used.
  for (key in PATH_NAMES(CFG)) {
    for (m in MOD_NAMES(CFG)) {
      keep <- LOCK$mods[[key]][[m]]$keep
      df_fit <- LOCK$analysis[[key]][keep, , drop = FALSE]
      sd_ <- make_stan_data(CFG, LOCK$analysis[[key]], LOCK$vz_chol[[key]],
                            m, LOCK$mods[[key]])
      expect_equal(nrow(df_fit), sd_$K, info = paste(key, m))
      expect_equal(length(unique(df_fit$sample_id)), sd_$S, info = paste(key, m))
    }
  }
})
