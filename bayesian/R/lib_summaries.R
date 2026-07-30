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

fit_diagnostics <- function(fit, fit_id, cfg, pars = NULL) {
  ds <- fit$diagnostic_summary(quiet = TRUE)
  sm <- fit$summary(variables = pars %||% c("alpha", "tau", "lp__"))
  data.frame(
    fit_id = fit_id,
    max_rhat = max(sm$rhat, na.rm = TRUE),
    min_ess_bulk = min(sm$ess_bulk, na.rm = TRUE),
    min_ess_tail = min(sm$ess_tail, na.rm = TRUE),
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
