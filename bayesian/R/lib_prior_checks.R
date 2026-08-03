# =============================================================================
# lib_prior_checks.R
# Prior predictive plausibility, and prior/likelihood power scaling.
#
# Both existed only as claims before this file. Six prior-only fits were run and
# converged, but nothing was extracted from them: no distribution, no criterion,
# no table, no figure. The methods nonetheless stated that the priors had been
# "checked by prior predictive simulation before any posterior was inspected".
# A successful MCMC run with the likelihood switched off is not a prior
# predictive check; it establishes only that the prior is proper.
#
# Likewise `priorsense: true` was set in the configuration and read by nothing.
# The Stan programme already emits `log_lik` and `lprior` for exactly this
# purpose and both were discarded.
#
# The thresholds live in config/analysis.yml and are committed BEFORE the run
# that tests them. The configuration hash is part of the analysis_id, so the
# ordering is provable rather than asserted. A criterion chosen after seeing the
# distribution it judges cannot fail.
# =============================================================================

#' Prior predictive summaries and the prespecified plausibility verdict.
#'
#' @param fit a prior-only fit (`prior_only = 1`), so every generated quantity
#'   is a draw from the prior predictive distribution.
#' @return list(summary, verdict): the implied distributions, and one row per
#'   prespecified criterion with a pass or fail.
prior_predictive_check <- function(cfg, fit, key) {
  pp <- cfg$prior_predictive
  if (is.null(pp))
    stop("config/analysis.yml has no prior_predictive block. The plausibility ",
         "thresholds must be prespecified before the run that tests them.")

  pairs <- PAIRS(cfg)
  dr <- as.data.frame(fit$draws(
    variables = c("rho_pop", "ind_pop", "total_pop", "a_pop", "b_pop", "cprime_pop"),
    format = "df"))

  q <- function(x, p) unname(stats::quantile(x, p, na.rm = TRUE))
  lo <- (1 - pp$anchor_interval_prob) / 2
  hi <- 1 - lo

  ## --- the implied prior on each zero-order correlation
  rho <- lapply(seq_along(pairs), function(j) dr[[sprintf("rho_pop[1,%d]", j)]])
  names(rho) <- pairs

  summary <- do.call(rbind, lapply(pairs, function(pr) {
    x <- rho[[pr]]
    data.frame(pathway = key, quantity = pr, scale = "correlation",
               median = stats::median(x), q_lo = q(x, lo), q_hi = q(x, hi),
               q025 = q(x, 0.025), q975 = q(x, 0.975),
               mass_within_bound = mean(abs(x) <= pp$max_abs_r),
               stringsAsFactors = FALSE)
  }))

  ind <- dr[["ind_pop[1]"]]; tot <- dr[["total_pop[1]"]]
  summary <- rbind(summary, do.call(rbind, lapply(
    list(c("indirect", "ind"), c("total", "total")), function(z) {
      x <- if (z[2] == "ind") ind else tot
      data.frame(pathway = key, quantity = z[1], scale = "standardised effect",
                 median = stats::median(x), q_lo = q(x, lo), q_hi = q(x, hi),
                 q025 = q(x, 0.025), q975 = q(x, 0.975),
                 mass_within_bound = NA_real_, stringsAsFactors = FALSE)
    })))

  ## --- the prespecified criteria, each a pass or a fail
  v <- list()
  add <- function(...) v[[length(v) + 1L]] <<- data.frame(..., stringsAsFactors = FALSE)

  ## 1. Coverage of the published anchors. The requirement is that the prior does
  ## not EXCLUDE values the external literature reports. It is not a claim that
  ## the prior encodes them.
  anchors <- unlist(pp$anchors_z)
  z_all <- atanh(unlist(rho))
  a_lo <- q(z_all, lo); a_hi <- q(z_all, hi)
  for (an in anchors)
    add(pathway = key, criterion = "covers published anchor",
        target = sprintf("Fisher-z %.3f inside the central %.0f%% of the implied prior",
                         an, 100 * pp$anchor_interval_prob),
        observed = sprintf("[%.3f, %.3f]", a_lo, a_hi),
        pass = an >= a_lo && an <= a_hi)

  ## 2. The implied correlations must be possible, and mostly not absurd.
  for (pr in pairs) {
    m <- mean(abs(rho[[pr]]) <= pp$max_abs_r)
    add(pathway = key, criterion = sprintf("%s within |r| <= %.2f", pr, pp$max_abs_r),
        target = sprintf(">= %.2f of prior mass", pp$min_mass_within_max_abs_r),
        observed = sprintf("%.3f", m), pass = m >= pp$min_mass_within_max_abs_r)
  }

  ## 3. The prior must not presuppose mediation, in either direction.
  p_pos <- mean(ind > 0)
  add(pathway = key, criterion = "indirect effect not presupposed",
      target = sprintf("P(ab > 0) in [%.2f, %.2f]", pp$indirect_min_p_positive,
                       pp$indirect_max_p_positive),
      observed = sprintf("%.3f", p_pos),
      pass = p_pos >= pp$indirect_min_p_positive && p_pos <= pp$indirect_max_p_positive)
  add(pathway = key, criterion = "indirect effect centred near zero",
      target = sprintf("|median| <= %.3f", pp$indirect_median_abs_max),
      observed = sprintf("%.4f", stats::median(ind)),
      pass = abs(stats::median(ind)) <= pp$indirect_median_abs_max)

  ## 4. But it must still admit effects large enough to matter, or it would rule
  ## the finding out before any data were seen.
  u <- q(ind, hi)
  add(pathway = key, criterion = "indirect effect can reach a relevant magnitude",
      target = sprintf("upper %.0f%% quantile >= %.2f", 100 * hi, pp$indirect_min_upper_q),
      observed = sprintf("%.3f", u), pass = u >= pp$indirect_min_upper_q)

  list(summary = summary, verdict = do.call(rbind, v),
       draws = data.frame(pathway = key, indirect = ind, total = tot,
                          stringsAsFactors = FALSE))
}

#' Prior and likelihood power scaling (Kallioinen et al.), on a cmdstanr fit.
#'
#' Answers a different question from the discrete prior grid. The grid refits the
#' model at other prior widths and asks whether the CONCLUSION moves; power
#' scaling perturbs the prior and the likelihood continuously by importance
#' resampling of the fit already in hand, and asks which of the two is driving
#' each posterior. A quantity sensitive to the prior and insensitive to the
#' likelihood is prior-dominated, which at k = 4 to 10 is worth knowing and is
#' exactly what a reader cannot tell from the grid alone.
#'
#' Returns NULL with a warning rather than raising: this is reported evidence,
#' not a release gate, and priorsense is not guaranteed to support every fit.
#' The caller adds `pmed_prop` for the unmoderated fits, whose percentage is
#' reported, so a reader can see whether it is prior-driven. Note what that does
#' NOT establish. Power scaling measures how a posterior moves when the prior and
#' the likelihood are tempered. It is not a check that a derived ratio is a stable
#' or interpretable estimand, and neither is simulation-based calibration, which
#' tests the sampler's recovery of the model's own parameters. Neither claim is
#' made anywhere in this release.
power_scaling <- function(cfg, fit, fit_id, variables = c("alpha", "tau", "beta", "d_z")) {
  if (!requireNamespace("priorsense", quietly = TRUE)) {
    warning("priorsense is not installed; power scaling skipped for ", fit_id)
    return(NULL)
  }
  vars <- intersect(variables, fit$metadata()$stan_variables)
  if (!length(vars)) return(NULL)

  out <- tryCatch({
    psd <- priorsense::create_priorsense_data(fit)
    ## powerscale_sensitivity() returns the summary data frame itself, not a
    ## list wrapping one. Columns: variable, prior, likelihood, diagnosis.
    d <- as.data.frame(priorsense::powerscale_sensitivity(psd, variable = vars))
    d$fit_id <- fit_id
    d[, c("fit_id", setdiff(names(d), "fit_id"))]
  }, error = function(e) {
    warning("Power scaling failed for ", fit_id, ": ", conditionMessage(e))
    NULL
  })
  out
}

#' Classify each archived power-scaling row as flagged, unflagged or
#' inapplicable.
#'
#' priorsense uses three states and the generated prose used two. "-" means the
#' quantity was evaluated and not flagged. A BLANK diagnosis means it could not
#' be evaluated: `d_z` is a moderator contrast, and an intercept-only model has
#' no contrast to span, so priorsense returns blank prior and likelihood
#' sensitivities for it. Counting a flagged row as `diagnosis != "-"` therefore
#' counted every non-evaluable row as a positive flag, which is how the release
#' came to report 164 of 342 flagged instead of 146 of 324 evaluable.
#'
#' NA is treated as blank rather than as its own case: read.csv gives NA for an
#' empty unquoted field and "" for an empty quoted one, and the distinction is an
#' artefact of the writer, not a fact about the diagnostic.
powerscale_status <- function(ps) {
  d <- ps$diagnosis
  blank <- is.na(d) | !nzchar(trimws(d))
  ps$ps_status <- ifelse(blank, "inapplicable",
                         ifelse(trimws(d) == "-", "unflagged", "flagged"))
  ps
}

#' Collate the prior-predictive verdicts and fail the release if any criterion
#' was not met.
#'
#' Fail loud. The alternative, quietly reporting a failed plausibility check
#' beside the results it was supposed to qualify, is how the previous release
#' came to assert a check it had never performed.
gate_prior_predictive <- function(verdicts) {
  if (is.null(verdicts) || !nrow(verdicts))
    stop("No prior predictive verdicts were produced, but the configuration ",
         "prespecifies them. The check must run or be removed from the config.")
  bad <- verdicts[!verdicts$pass, ]
  if (nrow(bad))
    stop(nrow(bad), " prior predictive plausibility criteria failed: ",
         paste(sprintf("%s / %s (target %s, observed %s)", bad$pathway, bad$criterion,
                       bad$target, bad$observed), collapse = "; "),
         ". These thresholds were prespecified in config/analysis.yml before the ",
         "run. Revise the priors, or revise the thresholds and say so, but do ",
         "not report the priors as checked.")
  data.frame(criteria = nrow(verdicts), passed = sum(verdicts$pass),
             pathways = length(unique(verdicts$pathway)), stringsAsFactors = FALSE)
}
