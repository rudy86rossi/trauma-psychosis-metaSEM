# =============================================================================
# lib_summaries.R
# Posterior summaries, the Dienes decision rule, and cross-layer comparison.
#
# Estimation is primary throughout. A decision is an additional statement about
# a posterior, never a substitute for reporting it.
# =============================================================================

#' Highest density interval from draws.
hdi <- function(x, prob = 0.95) {
  x <- sort(x[is.finite(x)])
  n <- length(x)
  if (n < 2) return(c(lower = NA_real_, upper = NA_real_))
  m <- max(1, floor(n * prob))
  w <- x[(m + 1):n] - x[1:(n - m)]
  i <- which.min(w)
  c(lower = x[i], upper = x[i + m])
}

#' Estimation summary for any posterior quantity. No decision.
estimate_summary <- function(draws, prob = 0.95) {
  draws <- draws[is.finite(draws)]
  h <- hdi(draws, prob)
  q <- unname(quantile(draws, c((1 - prob) / 2, 0.5, 1 - (1 - prob) / 2)))
  data.frame(median = q[2], sd = sd(draws), q_lo = q[1], q_hi = q[3],
             hdi_lower = unname(h[["lower"]]), hdi_upper = unname(h[["upper"]]),
             p_gt0 = mean(draws > 0), n_draws = length(draws),
             stringsAsFactors = FALSE)
}

## ---- the ratio -------------------------------------------------------------

## Summaries for a quantity with no finite moments.
##
## ab / total is a ratio whose denominator is tanh of a coordinate with unbounded
## support, so the denominator's density at zero is strictly positive and the
## expectation of the ratio does not exist. Quantiles do not require moments:
## the median and the equal-tailed percentiles are finite with probability one
## and converge under the usual ergodic conditions, and the median is invariant
## under monotone reparameterisation, which the mean is not.
##
## The remedy for a quantity with no first moment is to stop reporting its first
## and second moments, not to stop reporting the quantity. So this function is
## structurally incapable of returning a mean or a standard deviation, and it
## must be used INSTEAD of estimate_summary() rather than alongside it.
##
## No highest-density interval and no contiguity flag. Both presuppose a
## unimodal posterior. This one can be bimodal with mass running off in both
## directions, and an HDI computed on such a sample is an artefact of where the
## draws happened to fall rather than a summary of the distribution.

RATIO_FORBIDDEN <- c("mean", "sd", "se", "var", "mcse_mean", "mcse_sd")

#' Estimation summary for a ratio. Quantiles and probabilities only.
#'
#' @param draws posterior draws of the ratio.
#' @param chain_id optional chain index per draw. When supplied, the per-chain
#'   median and interval endpoints are returned as well, so a reader can see
#'   whether the chains agree about the quantity rather than trusting a single
#'   pooled number.
#' @param upper top of the interpretable range ON THE SCALE OF `draws`: 1 for a
#'   fraction, 100 for a percentage. This is not decoration. The probability of
#'   lying inside the interpretable range is the quantity the reportability rule
#'   turns on, and a fraction's rule applied to percentage draws reads P(0 < x <
#'   1) on a variable whose median is 55, returning almost zero and withholding
#'   every pathway. There is no safe default that covers both, so the caller
#'   states the scale and `range_upper` is carried into the output beside the
#'   probability it defines.
#' @param prob central interval mass.
#'
#' Non-finite draws are COUNTED and then fail. A ratio whose denominator hit
#' exactly zero in some draw is a fact about the posterior, and dropping such
#' draws quietly would turn an inestimable quantity into a plausible-looking one.
ratio_summary <- function(draws, chain_id = NULL, upper = 1, prob = 0.95) {
  if (!is.numeric(upper) || length(upper) != 1 || !is.finite(upper) || upper <= 0)
    stop("ratio_summary(): `upper` must be a single positive number giving the ",
         "top of the interpretable range on the scale of the draws.")
  draws <- unname(as.numeric(draws))
  n_nonfinite <- sum(!is.finite(draws))
  if (n_nonfinite > 0)
    stop("ratio_summary(): ", n_nonfinite, " of ", length(draws),
         " draws are not finite. The denominator reached zero to machine ",
         "precision. These draws are not removed: a quantity that does not ",
         "exist for some of the posterior must fail here rather than be ",
         "summarised over the draws where it happens to exist.")
  if (!length(draws)) stop("ratio_summary(): no draws.")

  a <- (1 - prob) / 2
  q <- unname(quantile(draws, c(0.01, a, 0.5, 1 - a, 0.99)))

  ## Monte Carlo standard error of each REPORTED quantile. mcse_mean and
  ## mcse_sd are undefined here and are never computed.
  mcse_q <- function(p) tryCatch(
    posterior::mcse_quantile(draws, probs = p)[[1]],
    error = function(e) NA_real_)

  out <- data.frame(
    median = q[3], q_lo = q[2], q_hi = q[4], q01 = q[1], q99 = q[5],
    ## Where the ratio sits relative to the interpretable range. Outside it is
    ## the inconsistent-mediation or suppression regime, not an error.
    range_upper = upper,
    p_in_range = mean(draws > 0 & draws < upper),
    p_lt0 = mean(draws < 0), p_gt_upper = mean(draws > upper),
    p_abs_gt_twice_range = mean(abs(draws) > 2 * upper),
    p_gt0 = mean(draws > 0),
    mcse_median = mcse_q(0.5), mcse_q_lo = mcse_q(a), mcse_q_hi = mcse_q(1 - a),
    n_draws = length(draws), n_nonfinite = n_nonfinite,
    stringsAsFactors = FALSE)

  if (!is.null(chain_id)) {
    ch <- split(draws, chain_id)
    med <- vapply(ch, stats::median, numeric(1))
    lo  <- vapply(ch, function(x) unname(quantile(x, a)), numeric(1))
    hi  <- vapply(ch, function(x) unname(quantile(x, 1 - a)), numeric(1))
    out$n_chains <- length(ch)
    out$chain_median_min <- min(med); out$chain_median_max <- max(med)
    out$chain_q_lo_min   <- min(lo);  out$chain_q_lo_max   <- max(lo)
    out$chain_q_hi_min   <- min(hi);  out$chain_q_hi_max   <- max(hi)
    out$chain_medians <- paste(signif(med, 6), collapse = " ")
  }

  bad <- intersect(names(out), RATIO_FORBIDDEN)
  if (length(bad))
    stop("ratio_summary() produced a moment-based column: ",
         paste(bad, collapse = ", "), ". This quantity has no moments.")
  out
}

#' Convergence diagnostics for a ratio, kept apart from fit_diagnostics().
#'
#' Two reasons it is separate rather than an extra entry in GATED_PARS.
#'
#' First, fit_diagnostics() requests mcse_mean and mcse_sd for every variable it
#' audits, and both are undefined for this quantity; feeding it a ratio would
#' publish two meaningless numbers next to three meaningful ones.
#'
#' Second, that function reduces its variables to a single maximum R-hat which
#' the release gate then judges. A pathway whose percentage is deliberately
#' withheld would therefore be able to fail the entire release on a quantity
#' nobody reads. The percentage's own reportability rule is the right place for
#' that judgement, and it is applied per pathway.
#'
#' Rank-normalised R-hat is what makes any of this legitimate. The classical
#' statistic is dominated by tail behaviour for a heavy-tailed estimand, which
#' was a sound objection to the earlier release. Vehtari et al. (2021) replace
#' each draw by its normal score across chains before computing the statistic,
#' so the diagnostic depends only on the ORDERING of the draws and is insensitive
#' to whether the estimand has moments. CmdStanR returns the rank-normalised form
#' by default.
ratio_diagnostics <- function(fit, variables, prob = 0.95) {
  vars <- intersect(variables, fit$metadata()$stan_variables)
  if (!length(vars)) return(NULL)
  a <- (1 - prob) / 2
  sm <- fit$summary(variables = vars, "rhat", "ess_bulk", "ess_tail")
  dr <- fit$draws(variables = vars, format = "draws_df")

  ## Effective sample size AT the reported quantiles. Bulk ESS describes the
  ## centre of the distribution and says nothing about how well an interval
  ## endpoint is resolved.
  qess <- function(v, p) tryCatch(
    posterior::ess_quantile(dr[[v]], probs = p)[[1]],
    error = function(e) NA_real_)
  mq <- function(v, p) tryCatch(
    posterior::mcse_quantile(dr[[v]], probs = p)[[1]],
    error = function(e) NA_real_)

  data.frame(
    variable = sm$variable,
    rhat_rank_normalised = sm$rhat,
    ess_bulk = sm$ess_bulk, ess_tail = sm$ess_tail,
    ess_median = vapply(sm$variable, qess, numeric(1), p = 0.5),
    ess_q_lo   = vapply(sm$variable, qess, numeric(1), p = a),
    ess_q_hi   = vapply(sm$variable, qess, numeric(1), p = 1 - a),
    mcse_median = vapply(sm$variable, mq, numeric(1), p = 0.5),
    mcse_q_lo   = vapply(sm$variable, mq, numeric(1), p = a),
    mcse_q_hi   = vapply(sm$variable, mq, numeric(1), p = 1 - a),
    stringsAsFactors = FALSE, row.names = NULL)
}

#' Dienes (2020) summary with the Kruschke (2018) interval rules.
#'
#' @param draws posterior draws of the quantity the SESOI is defined on. For a
#'   moderator this is the FULL-CONTRAST change, not a per-unit coefficient:
#'   the smallest effect size of interest is a scientific statement about the
#'   contrast, so the decision must be taken on the contrast's own posterior.
#' @param sesoi half-width of the null interval, on the same scale as `draws`.
#' @param decision FALSE returns estimation only, with no verdict. Used for the
#'   derived mediation contrasts, where no defensible threshold exists and
#'   manufacturing one to obtain a categorical answer is not justified.
#' @param prior_sd prior standard deviation of the same quantity, for the
#'   posterior contraction statistic.
dienes_summary <- function(draws, sesoi = NA_real_, decision = TRUE,
                           sesoi_grid = NULL, prior_sd = NA_real_, prob = 0.95) {
  est <- estimate_summary(draws, prob)
  out <- cbind(est, data.frame(
    sesoi = if (decision) sesoi else NA_real_,
    p_in_null = if (decision) mean(abs(draws) < sesoi) else NA_real_,
    decision = if (decision) NA_character_ else "estimation only",
    rr_lower = NA_real_, rr_upper = NA_real_, rr_covers_grid = NA,
    prior_sd = prior_sd,
    contraction = if (is.na(prior_sd)) NA_real_ else 1 - (est$sd / prior_sd)^2,
    stringsAsFactors = FALSE))
  if (!decision) return(out)

  lo <- est$hdi_lower; hi <- est$hdi_upper
  decide <- function(m) {
    if (lo > m || hi < -m) "reject null interval"
    else if (lo >= -m && hi <= m) "accept null interval"
    else "undecided"
  }
  out$decision <- decide(sesoi)

  ## Robustness region: the contiguous run of minimally interesting effect sizes
  ## around `sesoi` that yields the same verdict.
  if (!is.null(sesoi_grid) && length(sesoi_grid)) {
    same <- vapply(sesoi_grid, function(m) decide(m) == out$decision, logical(1))
    idx  <- which.min(abs(sesoi_grid - sesoi))
    if (isTRUE(same[idx])) {
      l <- idx; while (l > 1 && same[l - 1]) l <- l - 1
      u <- idx; while (u < length(same) && same[u + 1]) u <- u + 1
      out$rr_lower <- sesoi_grid[l]; out$rr_upper <- sesoi_grid[u]
      out$rr_covers_grid <- out$rr_lower <= min(sesoi_grid) && out$rr_upper >= max(sesoi_grid)
    }
  }
  out
}

## ---- cross-layer comparison -------------------------------------------------

#' Compare two marginal posteriors of the same estimand from SEPARATE fits.
#'
#' The two model layers are fitted independently, so draw i of one has no
#' correspondence with draw i of the other. Pairing them elementwise and
#' summarising the difference would fabricate a joint distribution that does not
#' exist. Everything here is a comparison of the two MARGINALS.
#'
#' Rescaling a coefficient to its contrast is a different matter: that happens
#' draw-wise WITHIN one fit, where the draws do correspond, and is done by the
#' caller before this function sees them.
compare_posteriors <- function(x, y, labels = c("stan", "brms"), prob = 0.95) {
  x <- x[is.finite(x)]; y <- y[is.finite(y)]
  qx <- unname(quantile(x, c((1 - prob) / 2, 0.5, 1 - (1 - prob) / 2)))
  qy <- unname(quantile(y, c((1 - prob) / 2, 0.5, 1 - (1 - prob) / 2)))
  sx <- sd(x); sy <- sd(y)
  pooled <- sqrt((sx^2 + sy^2) / 2)

  ## Overlap coefficient of the two densities, on a shared grid.
  rng <- range(c(x, y))
  grid <- seq(rng[1], rng[2], length.out = 512)
  bw <- max(stats::bw.nrd0(x), stats::bw.nrd0(y))
  dx <- stats::density(x, from = rng[1], to = rng[2], n = 512, bw = bw)$y
  dy <- stats::density(y, from = rng[1], to = rng[2], n = 512, bw = bw)$y
  ovl <- sum(pmin(dx, dy)) * diff(grid)[1]

  ## Interval overlap, as a fraction of the union of the two intervals.
  inter <- max(0, min(qx[3], qy[3]) - max(qx[1], qy[1]))
  union <- max(qx[3], qy[3]) - min(qx[1], qy[1])

  data.frame(
    median_x = qx[2], median_y = qy[2],
    sd_x = sx, sd_y = sy,
    median_diff = qx[2] - qy[2],
    median_diff_in_pooled_sd = (qx[2] - qy[2]) / pooled,
    sd_ratio = sx / sy,
    interval_overlap = if (union > 0) inter / union else NA_real_,
    density_overlap = ovl,
    ecdf_distance = suppressWarnings(stats::ks.test(x, y)$statistic[[1]]),
    label_x = labels[1], label_y = labels[2],
    stringsAsFactors = FALSE)
}

## ---- diagnostics ------------------------------------------------------------

## Which variables the convergence gate is entitled to judge.
##
## GATED: every sampled parameter, plus the deterministic estimands the results
## are read off. A conclusion rests on d_z and on the pooled mediation
## quantities, so an R-hat that never looked at them is not evidence that the
## conclusion converged. The earlier default of alpha, tau and lp__ omitted
## beta, the study effects and d_z, which is to say it omitted the parameter the
## sole decision was made on.
##
## NOT GATED: the `*_new` predictive quantities. Each draw adds a fresh
## normal_rng(0, tau) random effect, so their between-chain variance contains
## deliberate Monte Carlo noise that has nothing to do with whether the sampler
## converged. Gating them would manufacture failures that mean nothing. They are
## summarised and reported, in their own columns, so the distinction is visible
## rather than implicit.
GATED_PARS   <- c("alpha", "beta", "tau", "u", "d_z",
                  "a_pop", "b_pop", "cprime_pop", "ind_pop", "total_pop",
                  "d_a", "d_b", "d_cprime", "d_ind", "d_total",
                  "ind_study_mean", "lp__")
STOCHASTIC_PARS <- c("a_new", "b_new", "cprime_new", "ind_new", "total_new")

#' Convergence and sampler diagnostics for one Stan fit.
#'
#' `pmed_prop` and `pmed_pct` are audited by ratio_diagnostics() above, not here.
#' That is not an exemption from scrutiny: they receive a stricter diagnostic
#' regime than anything in this table, including effective sample size and Monte
#' Carlo error at each reported quantile rather than only in the bulk. What they
#' are exempt from is the moment-based columns below, which are undefined for
#' them, and from a global maximum that would let a withheld pathway fail the
#' whole release. See bayesian/docs/proportion_mediated.md.
fit_diagnostics <- function(fit, fit_id, cfg, pars = NULL) {
  ds <- fit$diagnostic_summary(quiet = TRUE)

  gated <- pars %||% GATED_PARS
  gated <- gated[gated %in% c(fit$metadata()$stan_variables, "lp__")]
  sm <- fit$summary(variables = gated,
                    "rhat", "ess_bulk", "ess_tail", "mcse_mean", "mcse_sd")

  ## Name the offending variable. An anonymous extremum cannot be investigated,
  ## and "max R-hat 1.004" tells a reader nothing about whether it sat on a
  ## nuisance parameter or on the reported contrast.
  i_rhat <- which.max(replace(sm$rhat, is.na(sm$rhat), -Inf))
  i_bulk <- which.min(replace(sm$ess_bulk, is.na(sm$ess_bulk), Inf))
  i_tail <- which.min(replace(sm$ess_tail, is.na(sm$ess_tail), Inf))

  stoch <- STOCHASTIC_PARS[STOCHASTIC_PARS %in% fit$metadata()$stan_variables]
  sn <- if (length(stoch))
    fit$summary(variables = stoch, "rhat", "ess_bulk") else NULL

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
    params_audited = paste(gated, collapse = " "),
    ## Reported, never gated. See the comment on STOCHASTIC_PARS.
    predictive_max_rhat = if (is.null(sn)) NA_real_ else max(sn$rhat, na.rm = TRUE),
    predictive_min_ess_bulk = if (is.null(sn)) NA_real_ else min(sn$ess_bulk, na.rm = TRUE),
    num_divergent = sum(ds$num_divergent),
    num_max_treedepth = sum(ds$num_max_treedepth),
    min_ebfmi = min(ds$ebfmi, na.rm = TRUE),
    escalations = as.integer(attr(fit, "escalated") %||% 0L),
    stringsAsFactors = FALSE)
}

diagnostics_pass <- function(d, cfg) {
  g <- cfg$diagnostics
  d$max_rhat < g$max_rhat & d$min_ess_bulk > g$min_ess_bulk &
    d$min_ess_tail > g$min_ess_tail & d$num_divergent <= g$max_divergent &
    d$num_max_treedepth <= g$max_treedepth_hits & d$min_ebfmi > g$min_ebfmi
}

`%||%` <- function(a, b) if (is.null(a)) b else a
