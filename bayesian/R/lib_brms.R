# =============================================================================
# lib_brms.R
# Layer 2, reference: the correlation-level multivariate meta-regression.
#
# This is the simpler, transparent model and the like-for-like comparison with
# the frequentist zero-order correlation analysis. It is NOT the arbiter when it
# disagrees with the primary valid-correlation model, because it places no
# constraint keeping the implied 3x3 population matrix positive definite.
#
# One row per study per correlation pair. The known sampling covariance enters
# through fcor(); because fcor() multiplies the supplied matrix by the residual
# variance, sigma is fixed to 1 by a constant prior and no se() term is added.
# The `||` gives diagonal between-study effects by correlation type, which is
# the appropriate primary specification at this k.
# =============================================================================

suppressPackageStartupMessages(library(brms))

#' Long-format data plus the block-diagonal V_z over independent samples.
#'
#' @param drop_sample a sample_id to omit, for leave-one-study-out.
build_long <- function(cfg, df, vz_chol, mod = NULL, mods = NULL, drop_sample = NULL) {
  keep <- rep(TRUE, nrow(df))
  if (!is.null(mod)) keep <- keep & mods[[mod]]$keep
  if (!is.null(drop_sample)) keep <- keep & df$sample_id != drop_sample
  d  <- df[keep, , drop = FALSE]
  Lv <- vz_chol[keep]
  if (!nrow(d)) stop("No rows left after filtering.")

  ## The moderator is recentred on the SURVIVING studies, so a leave-one-out
  ## refit is centred on the data it actually uses.
  x <- rep(0, nrow(d))
  if (!is.null(mod)) {
    mm <- cfg$moderators[[mod]]
    v  <- d[[mod]]
    x  <- if (identical(mm$kind, "binary")) v else (v - mean(v)) / mm$unit
  }

  pairs <- PAIRS(cfg)
  long <- data.frame(
    study_id  = rep(d$sample_id, each = 3),
    pair      = factor(rep(pairs, times = nrow(d)), levels = pairs),
    z_r       = as.vector(t(atanh(as.matrix(d[, pairs])))),
    moderator = rep(x, each = 3),
    stringsAsFactors = FALSE)
  long$obs <- factor(seq_len(nrow(long)))

  V <- matrix(0, nrow(long), nrow(long))
  for (i in seq_len(nrow(d))) {
    idx <- (3 * (i - 1) + 1):(3 * i)
    V[idx, idx] <- Lv[[i]] %*% t(Lv[[i]])
  }
  dimnames(V) <- list(levels(long$obs), levels(long$obs))

  list(long = long, V = V, d = d,
       centre = if (!is.null(mod) && !identical(cfg$moderators[[mod]]$kind, "binary"))
         mean(d[[mod]]) else 0)
}

brms_formula <- function(has_mod) {
  if (has_mod)
    brms::bf(z_r ~ 0 + pair + pair:moderator + (0 + pair || study_id) + fcor(V))
  else
    brms::bf(z_r ~ 0 + pair + (0 + pair || study_id) + fcor(V))
}

#' Priors on the same scale as the Stan layer. The moderator prior is specified
#' on the full-contrast scale in the configuration and divided by the nominal
#' span here, exactly as in make_stan_data().
brms_priors <- function(cfg, mod, priors = NULL) {
  priors <- priors %||% cfg$priors$primary
  p <- c(brms::set_prior(sprintf("normal(0, %g)", priors$alpha_sd), class = "b"),
         brms::set_prior(sprintf("normal(0, %g)", priors$tau_sd), class = "sd"),
         ## fcor() scales the supplied matrix by the residual variance, so the
         ## residual SD must be exactly 1 for V_z to enter as the known
         ## sampling covariance rather than an estimated multiple of it.
         brms::set_prior("constant(1)", class = "sigma"))
  if (!is.null(mod)) {
    bsd <- prior_unit_sd_for(cfg, mod, priors$beta_full_sd)
    for (lv in PAIRS(cfg))
      p <- c(p, brms::set_prior(sprintf("normal(0, %g)", bsd), class = "b",
                                coef = paste0("pair", lv, ":moderator")))
  }
  p
}

fit_brms_model <- function(cfg, bl, mod, fit_id, priors = NULL, sampling = NULL) {
  s <- sampling %||% cfg$sampling
  seed <- fit_seed(cfg, fit_id)

  run <- function(adapt_delta) suppressMessages(suppressWarnings(
    brms::brm(brms_formula(!is.null(mod)), data = bl$long,
              data2 = list(V = bl$V), family = gaussian(),
              prior = brms_priors(cfg, mod, priors),
              chains = s$chains, cores = s$chains,
              warmup = s$iter_warmup, iter = s$iter_warmup + s$iter_sampling,
              control = list(adapt_delta = adapt_delta, max_treedepth = s$max_treedepth),
              seed = seed, refresh = 0, silent = 2, backend = "cmdstanr")))

  ladder <- unlist(cfg$sampling$adapt_delta_ladder)
  ladder <- unique(ladder[ladder >= s$adapt_delta])
  fit <- run(ladder[1]); escalated <- 0L
  for (step in ladder[-1]) {
    if (sum(brms::nuts_params(fit, pars = "divergent__")$Value) == 0) break
    fit <- run(step); escalated <- escalated + 1L
  }
  attr(fit, "escalated") <- escalated
  attr(fit, "fit_id") <- fit_id
  fit
}

#' brms diagnostics, extended to tree depth and energy as the release audit
#' requires. brms does not expose diagnostic_summary(), so these are read from
#' the NUTS parameters directly.
brms_diagnostics <- function(fit, fit_id) {
  np <- brms::nuts_params(fit)
  sm <- posterior::summarise_draws(as_draws_df(fit),
                                   "rhat", "ess_bulk", "ess_tail",
                                   "mcse_mean", "mcse_sd")
  sm <- sm[!grepl("^(lprior|lp__)$", sm$variable), ]
  md <- fit$fit@sim$control$max_treedepth %||% 12
  energy <- np$Value[np$Parameter == "energy__"]
  ## E-BFMI per chain, per Betancourt: mean squared successive difference over
  ## the marginal energy variance.
  ch <- np$Chain[np$Parameter == "energy__"]
  ebfmi <- min(vapply(unique(ch), function(cc) {
    e <- energy[ch == cc]
    sum(diff(e)^2) / length(e) / stats::var(e)
  }, numeric(1)))

  ## Same columns as the Stan layer, so the two are comparable rather than
  ## pooled into one min/max over incomparable parameter sets. brms audits every
  ## population-level, group-level and scale parameter, which is a WIDER scope
  ## than the Stan gate; `params_audited` records that on each row so a reader
  ## can see whose extremum they are looking at.
  i_rhat <- which.max(replace(sm$rhat, is.na(sm$rhat), -Inf))
  i_bulk <- which.min(replace(sm$ess_bulk, is.na(sm$ess_bulk), Inf))
  i_tail <- which.min(replace(sm$ess_tail, is.na(sm$ess_tail), Inf))

  data.frame(
    fit_id = fit_id,
    max_rhat = max(sm$rhat, na.rm = TRUE),
    min_ess_bulk = min(sm$ess_bulk, na.rm = TRUE),
    min_ess_tail = min(sm$ess_tail, na.rm = TRUE),
    max_mcse_mean = max(sm$mcse_mean, na.rm = TRUE),
    max_mcse_sd = max(sm$mcse_sd, na.rm = TRUE),
    worst_rhat_variable = sm$variable[i_rhat],
    worst_ess_bulk_variable = sm$variable[i_bulk],
    worst_ess_tail_variable = sm$variable[i_tail],
    n_variables_audited = nrow(sm),
    params_audited = "all brms parameters except lprior and lp__",
    predictive_max_rhat = NA_real_,
    predictive_min_ess_bulk = NA_real_,
    num_divergent = sum(np$Value[np$Parameter == "divergent__"]),
    num_max_treedepth = sum(np$Value[np$Parameter == "treedepth__"] >= md),
    min_ebfmi = ebfmi,
    escalations = as.integer(attr(fit, "escalated") %||% 0L),
    stringsAsFactors = FALSE)
}

#' The brms full-contrast change, on the SAME estimand as the Stan layer.
#'
#' The fitted coefficient is per fitted unit; the contrast spans `span` units.
#' Multiplying is a draw-wise transform WITHIN one fit, where draws correspond,
#' so it is legitimate. Comparing the result with the Stan layer is a comparison
#' of two marginals from separate fits and is done by compare_posteriors().
brms_d_z <- function(cfg, fit, spec) {
  dr <- posterior::as_draws_df(fit)
  pairs <- PAIRS(cfg)
  span <- spec$supported_span
  out <- vapply(pairs, function(pr) {
    nm <- sprintf("b_pair%s:moderator", pr)
    if (!nm %in% names(dr)) stop("brms coefficient not found: ", nm)
    dr[[nm]] * span
  }, numeric(posterior::ndraws(dr)))
  colnames(out) <- pairs
  out
}

#' Pooled zero-order Fisher-z at a given moderator value, for the pooled table.
brms_pooled_z <- function(cfg, fit, at = NULL, centre = 0, unit = 1) {
  dr <- posterior::as_draws_df(fit)
  pairs <- PAIRS(cfg)
  x <- if (is.null(at)) 0 else (at - centre) / unit
  vapply(pairs, function(pr) {
    b <- dr[[sprintf("b_pair%s", pr)]]
    m <- sprintf("b_pair%s:moderator", pr)
    if (m %in% names(dr)) b + dr[[m]] * x else b
  }, numeric(posterior::ndraws(dr)))
}
