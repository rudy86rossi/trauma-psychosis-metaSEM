# =============================================================================
# lib_sbc.R
# Formal rank-based simulation-based calibration (Talts et al. 2018).
#
# For each replicate: draw theta from the prior, simulate data from the joint
# model at the REAL k, n and V_z, refit, and record the RANK of each simulated
# truth among the retained posterior draws. If the sampler and the model are
# correctly implemented those ranks are uniform.
#
# What this validates and what it does not.
#
#   It validates the implemented likelihood, the generated quantities and the
#   sampler, CONDITIONAL ON the plug-in sampling covariance matrices V_z. It
#   does not validate the uncertainty that arises from estimating V_z from
#   participant-level data, because V_z is treated as known here exactly as it
#   is in the substantive analysis.
#
#   At around 100 datasets per scenario this is a SCREENING-level check. It will
#   detect a gross implementation error; it is not high-powered proof of
#   calibration. The number of independent datasets and the Monte Carlo envelope
#   are reported so the reader can see that.
#
# Every fit contributes the same number of retained draws, the thinning rule is
# fixed before any rank distribution is inspected, ties are broken at random,
# and fits that fail or breach a diagnostic threshold stay in the denominator.
# =============================================================================

#' One prior draw of the parameters, on the model's own scale.
sbc_draw_prior <- function(cfg, S, P, priors, beta_unit_sd) {
  list(alpha = rnorm(3, 0, priors$alpha_sd),
       beta  = if (P) rnorm(3, 0, beta_unit_sd) else numeric(0),
       tau   = abs(rnorm(3, 0, priors$tau_sd)))
}

#' Simulate one dataset from the joint model, at the real design.
sbc_simulate <- function(theta, stan_data) {
  K <- stan_data$K; S <- stan_data$S
  u <- matrix(rnorm(3 * S), 3, S) * theta$tau
  z <- matrix(NA_real_, K, 3)
  for (i in seq_len(K)) {
    eta <- theta$alpha + u[, stan_data$sample_id[i]]
    if (stan_data$P) eta <- eta + theta$beta * stan_data$x[i]
    mu <- eta_to_z_R(eta)
    L  <- matrix(stan_data$L_V[i, , ], 3, 3)
    z[i, ] <- mu + L %*% rnorm(3)
  }
  z
}

#' Derived truth: the full-contrast change implied by a parameter draw. Included
#' so that calibration is checked on the quantity the release actually reports,
#' not only on the raw coefficients.
sbc_truth_dz <- function(theta, xc) {
  if (!length(theta$beta) || length(xc) < 2) return(rep(NA_real_, 3))
  eta_to_z_R(theta$alpha + theta$beta * xc[length(xc)]) -
    eta_to_z_R(theta$alpha + theta$beta * xc[1])
}

#' Rank of a scalar truth among retained draws, with ties broken at random.
#' Range is 0..length(draws), so a uniform histogram has length+1 cells.
sbc_rank <- function(truth, draws) {
  ## Only a MISSING truth is skipped, which happens for d_z in an
  ## intercept-only scenario. An infinite truth ranks normally: it would signal
  ## a fault elsewhere, and silently returning NA would hide it from the audit.
  if (is.na(truth)) return(NA_integer_)
  lt <- sum(draws < truth)
  eq <- sum(draws == truth)
  as.integer(lt + if (eq > 0) sample.int(eq + 1L, 1L) - 1L else 0L)
}

#' Run one SBC scenario.
run_sbc_scenario <- function(cfg, scenario, lock, n_datasets = NULL) {
  key <- scenario$pathway
  mod <- if (is.null(scenario$moderator) || identical(scenario$moderator, "null"))
    NULL else scenario$moderator
  n_datasets <- n_datasets %||% cfg$sbc$n_datasets

  df   <- lock$analysis[[key]]
  spec <- if (is.null(mod)) NULL else
    lock$support[lock$support$pathway == key & lock$support$moderator == mod, ]
  base <- make_stan_data(cfg, df, lock$vz_chol[[key]], mod, lock$mods[[key]])

  priors <- cfg$priors$primary
  beta_unit_sd <- if (is.null(mod)) priors$beta_full_sd else
    prior_unit_sd_for(cfg, mod, priors$beta_full_sd)

  s <- cfg$sbc
  L <- s$retained_draws
  samp <- list(chains = s$chains, iter_warmup = s$iter_warmup,
               iter_sampling = s$iter_sampling,
               adapt_delta = cfg$sampling$adapt_delta,
               max_treedepth = cfg$sampling$max_treedepth)
  ## Thinning is fixed HERE, before any rank is seen.
  total_draws <- s$chains * s$iter_sampling
  if (total_draws < L) stop("SBC: fewer post-warm-up draws than retained_draws.")
  thin_idx <- round(seq(1, total_draws, length.out = L))

  set.seed(fit_seed(cfg, paste0("sbc_", scenario$id)))
  rows <- vector("list", n_datasets)

  for (r in seq_len(n_datasets)) {
    theta <- sbc_draw_prior(cfg, base$S, base$P, priors, beta_unit_sd)
    sd_r  <- base
    sd_r$z <- sbc_simulate(theta, base)

    fit <- tryCatch(
      fit_masem(cfg, sd_r, sprintf("sbc_%s_%03d", scenario$id, r), sampling = samp),
      error = function(e) NULL)

    ## A failed or diagnostically invalid fit is recorded, not dropped: removing
    ## it would flatter the calibration audit.
    if (is.null(fit)) {
      rows[[r]] <- data.frame(scenario = scenario$id, replicate = r, parameter = NA_character_,
                              rank = NA_integer_, status = "fit failed",
                              max_rhat = NA_real_, num_divergent = NA_integer_,
                              stringsAsFactors = FALSE)
      next
    }
    dg <- fit_diagnostics(fit, "sbc", cfg)
    ok <- diagnostics_pass(dg, cfg)

    dr <- as.data.frame(fit$draws(format = "df"))
    take <- function(nm) dr[[nm]][thin_idx]

    truths <- c(setNames(theta$alpha, sprintf("alpha[%d]", 1:3)),
                setNames(theta$tau,   sprintf("tau[%d]", 1:3)))
    if (base$P) truths <- c(truths, setNames(theta$beta, sprintf("beta[1,%d]", 1:3)))
    dz_true <- sbc_truth_dz(theta, base$xc)
    if (base$P) truths <- c(truths, setNames(dz_true, sprintf("d_z[%d]", 1:3)))

    rows[[r]] <- do.call(rbind, lapply(names(truths), function(nm)
      data.frame(scenario = scenario$id, replicate = r, parameter = nm,
                 rank = sbc_rank(truths[[nm]], take(nm)),
                 status = if (ok) "ok" else "diagnostics failed",
                 max_rhat = dg$max_rhat, num_divergent = dg$num_divergent,
                 stringsAsFactors = FALSE)))
  }
  out <- do.call(rbind, rows)
  attr(out, "n_bins") <- L + 1L
  out
}

#' Rank uniformity by parameter family, with a Monte Carlo envelope.
#'
#' Parameters within one dataset are NOT independent replications, so the
#' denominator is the number of independent datasets contributing to each
#' parameter, and the chi-squared test is applied per scalar parameter.
sbc_summary <- function(ranks, n_bins, n_cells = 20) {
  ok <- ranks[!is.na(ranks$rank) & ranks$status == "ok", ]
  fam <- function(p) sub("\\[.*$", "", p)

  per_par <- do.call(rbind, lapply(split(ok, list(ok$scenario, ok$parameter), drop = TRUE),
                                   function(g) {
    n <- nrow(g)
    ## Bin the ranks into n_cells equal cells and test uniformity.
    cells <- cut(g$rank, breaks = seq(-0.5, n_bins - 0.5, length.out = n_cells + 1),
                 labels = FALSE)
    obs <- tabulate(cells, n_cells)
    expct <- n / n_cells
    chi <- sum((obs - expct)^2 / expct)
    data.frame(scenario = g$scenario[1], parameter = g$parameter[1],
               family = fam(g$parameter[1]), n_datasets = n,
               chisq = chi, df = n_cells - 1,
               p_value = stats::pchisq(chi, n_cells - 1, lower.tail = FALSE),
               ## Monte Carlo envelope for a single cell count under uniformity.
               mc_lower = stats::qbinom(0.005, n, 1 / n_cells),
               mc_upper = stats::qbinom(0.995, n, 1 / n_cells),
               expected_per_cell = expct,
               max_cell = max(obs), min_cell = min(obs),
               stringsAsFactors = FALSE)
  }))
  rownames(per_par) <- NULL

  audit <- data.frame(
    total_fits = length(unique(paste(ranks$scenario, ranks$replicate))),
    fits_failed = length(unique(paste(ranks$scenario, ranks$replicate)[ranks$status == "fit failed"])),
    fits_diag_failed = length(unique(paste(ranks$scenario, ranks$replicate)[ranks$status == "diagnostics failed"])),
    n_bins = n_bins, n_cells = n_cells,
    stringsAsFactors = FALSE)

  list(per_parameter = per_par, audit = audit)
}
