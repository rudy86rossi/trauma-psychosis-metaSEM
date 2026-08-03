# =============================================================================
# lib_stan.R
# Assembling and fitting the primary valid-correlation MASEM.
#
# Moderation is reported as posterior contrasts in the mediation quantities and
# in the matched zero-order Fisher-z coordinates, never by relabelling a
# correlation slope as a path slope.
# =============================================================================

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
})

.masem_model <- new.env(parent = emptyenv())

masem_model <- function(file = "bayesian/stan/masem_valid.stan") {
  if (is.null(.masem_model$m)) {
    Sys.setenv(CMDSTANR_NO_VER_CHECK = "TRUE")
    .masem_model$m <- cmdstanr::cmdstan_model(file)
  }
  .masem_model$m
}

## ---- deterministic transforms, mirrored from the Stan functions block -------
## Independent R implementations, so the Stan generated quantities can be
## verified against code that shares none of their lines.

## Inputs are unnamed defensively: draws extracted from a posterior matrix carry
## names like "alpha[1]", which would otherwise propagate into the returned
## vectors and break name-based lookup downstream.

eta_to_rho_R <- function(eta) {
  eta <- unname(as.numeric(eta))
  rXM <- tanh(eta[1]); rXY <- tanh(eta[2]); rMYX <- tanh(eta[3])
  c(rXM, rXY, rXM * rXY + rMYX * sqrt((1 - rXM^2) * (1 - rXY^2)))
}

eta_to_z_R <- function(eta) {
  eta <- unname(as.numeric(eta))
  c(eta[1], eta[2], atanh(eta_to_rho_R(eta)[3]))
}

rho_to_path_R <- function(rho) {
  rho <- unname(as.numeric(rho))
  d <- 1 - rho[1]^2
  b  <- (rho[3] - rho[1] * rho[2]) / d
  cp <- (rho[2] - rho[1] * rho[3]) / d
  c(a = rho[1], b = b, cprime = cp, indirect = rho[1] * b, total = rho[2])
}

#' Independent route to (a, b, c'): solve the standardised regression of Y on
#' (X, M) straight from the 3x3 correlation matrix. Shares no algebra with
#' rho_to_path_R, so agreement between them is evidence rather than tautology.
rho_to_path_solve <- function(rho) {
  rho <- unname(as.numeric(rho))
  R <- matrix(c(1, rho[1], rho[2], rho[1], 1, rho[3], rho[2], rho[3], 1), 3, byrow = TRUE)
  bc <- solve(R[c(1, 2), c(1, 2)], R[c(1, 2), 3])
  c(a = rho[1], b = unname(bc[2]), cprime = unname(bc[1]),
    indirect = rho[1] * unname(bc[2]), total = rho[2])
}

## ---- data assembly ----------------------------------------------------------

#' Build the Stan data list for one pathway and moderator.
#'
#' @param priors list with alpha_sd, beta_full_sd, tau_sd on the FULL-CONTRAST
#'   scale. The per-unit coefficient prior is derived here by dividing by the
#'   moderator's nominal span, so that the scientific scale is what the
#'   configuration and the sensitivity grid vary.
make_stan_data <- function(cfg, df, vz_chol, mod = NULL, mods = NULL,
                           priors = NULL, robust = FALSE, prior_only = FALSE) {
  priors <- priors %||% cfg$priors$primary

  if (is.null(mod)) {
    keep <- rep(TRUE, nrow(df)); x <- rep(0, nrow(df)); xc <- 0; P <- 0L
    beta_unit_sd <- priors$beta_full_sd
  } else {
    mi <- mods[[mod]]
    ## The moderator structure must describe THIS data frame. The sensitivity
    ## classes rebuild it for a subset (a deleted study, a substituted
    ## instrument), so a mismatched pair is a real possibility and would
    ## otherwise surface as an opaque Stan initialisation failure.
    if (length(mi$keep) != nrow(df))
      stop("Moderator '", mod, "' describes ", length(mi$keep), " studies but the data have ",
           nrow(df), ". The moderator structure must be rebuilt for this subset.")

    ## mi$x holds one value per study in the pathway. The model is fitted on the
    ## studies that HAVE the moderator, so x is masked exactly as the data are.
    ## Omitting this passes more moderator values than there are rows, which
    ## Stan rejects at data initialisation with every chain failing at once and
    ## an error naming no fit. Only an incomplete moderator exposes it.
    keep <- mi$keep; x <- mi$x[keep]; xc <- mi$xc; P <- 1L
    beta_unit_sd <- prior_unit_sd_for(cfg, mod, priors$beta_full_sd)
  }

  d  <- df[keep, , drop = FALSE]
  Lv <- vz_chol[keep]
  sid <- as.integer(factor(d$sample_id, levels = unique(d$sample_id)))

  stopifnot(length(x) == nrow(d), length(Lv) == nrow(d))

  z <- cbind(atanh(d$r_XM), atanh(d$r_XY), atanh(d$r_MY))
  if (any(!is.finite(z)))
    stop("Non-finite Fisher-z value: an out-of-range correlation reached the model.")

  list(K = nrow(d), S = max(sid), sample_id = sid, z = z,
       L_V = aperm(array(unlist(lapply(Lv, as.vector)), dim = c(3, 3, length(Lv))),
                   c(3, 1, 2)),
       P = P, x = as.array(x),
       prior_alpha_sd = priors$alpha_sd,
       prior_beta_sd  = beta_unit_sd,
       prior_tau_sd   = priors$tau_sd,
       NC = length(xc), xc = as.array(xc),
       robust = as.integer(robust), nu = cfg$sensitivity$student_t_df %||% 5,
       prior_only = as.integer(prior_only))
}

## ---- fitting ----------------------------------------------------------------

#' Fit the model, escalating adapt_delta up the configured ladder while
#' divergences remain. The diagnostic target is zero divergences, and one
#' escalation is not always enough.
fit_masem <- function(cfg, stan_data, fit_id, sampling = NULL, sig_figs = NULL,
                      refresh = 0, quiet = TRUE) {
  s <- sampling %||% cfg$sampling
  mod  <- masem_model()
  seed <- fit_seed(cfg, fit_id)

  run <- function(adapt_delta) suppressMessages(suppressWarnings(
    mod$sample(data = stan_data, seed = seed,
               chains = s$chains, parallel_chains = s$chains,
               iter_warmup = s$iter_warmup, iter_sampling = s$iter_sampling,
               adapt_delta = adapt_delta, max_treedepth = s$max_treedepth,
               refresh = refresh, show_messages = !quiet, show_exceptions = FALSE,
               sig_figs = sig_figs)))

  ladder <- unlist(cfg$sampling$adapt_delta_ladder)
  ladder <- unique(ladder[ladder >= s$adapt_delta])
  if (!length(ladder)) ladder <- s$adapt_delta

  fit <- run(ladder[1]); escalated <- 0L
  for (step in ladder[-1]) {
    if (sum(fit$diagnostic_summary(quiet = TRUE)$num_divergent) == 0) break
    fit <- run(step); escalated <- escalated + 1L
  }
  attr(fit, "escalated") <- escalated
  attr(fit, "fit_id") <- fit_id
  fit
}

## ---- extraction -------------------------------------------------------------

#' Matched zero-order full-contrast change, as a draws matrix (3 columns).
stan_d_z <- function(fit) {
  as.matrix(fit$draws(variables = "d_z", format = "draws_matrix"))
}

#' Everything the tables need from one fit, in one place, so that no summary is
#' recomputed from a different set of draws than the one it is reported beside.
extract_stan <- function(cfg, fit, key, mod, spec, df, mods, reportable) {
  dr <- fit$draws(format = "df")
  pairs <- PAIRS(cfg)
  grid <- sesoi_grid(cfg)
  kk <- length(unique(df$sample_id))
  is_mod <- !is.null(mod)

  base <- data.frame(pathway = key, label = df$label[1],
                     moderator = if (is_mod) mod else "null",
                     k_samples = kk, reportable = reportable,
                     stringsAsFactors = FALSE)

  ## --- matched zero-order contrasts. The decision lives here, on the FULL
  ## contrast, using the bound the supported contrast earns.
  slopes <- NULL
  if (is_mod) {
    slopes <- do.call(rbind, lapply(1:3, function(j) {
      v <- dr[[sprintf("d_z[%d]", j)]]
      s <- dienes_summary(v, sesoi = spec$sesoi_reported,
                          decision = isTRUE(spec$decision_permitted),
                          sesoi_grid = grid,
                          prior_sd = spec$prior_beta_unit_sd * spec$supported_span)
      cbind(base, data.frame(coordinate = pairs[j], estimand = "d_z",
                             contrast = contrast_label(spec),
                             contrast_lo = spec$supported_lo,
                             contrast_hi = spec$supported_hi,
                             span_units = spec$supported_span,
                             clamped = spec$clamped, stringsAsFactors = FALSE), s)
    }))

    ## The model's own coefficients, recorded for completeness and for the
    ## sensitivity machinery, but explicitly NOT given a scientific verdict: the
    ## third coordinate is a partial correlation, a parameterisation quantity.
    eta_coords <- c("atanh_rho_XM", "atanh_rho_XY", "atanh_rho_MY.X")
    slopes <- rbind(slopes, do.call(rbind, lapply(1:3, function(j) {
      v <- dr[[sprintf("beta[1,%d]", j)]]
      s <- dienes_summary(v, decision = FALSE, prior_sd = spec$prior_beta_unit_sd)
      cbind(base, data.frame(coordinate = eta_coords[j], estimand = "beta_per_unit",
                             contrast = "per fitted unit",
                             contrast_lo = NA_real_, contrast_hi = NA_real_,
                             span_units = 1, clamped = spec$clamped,
                             stringsAsFactors = FALSE), s)
    })))
  }

  ## --- between-study heterogeneity.
  ## The prediction intervals are entirely tau-driven (eta_new = eta + N(0, tau)),
  ## and the write-up appeals to heterogeneity being weakly identified at this k
  ## to explain their width. Neither claim was checkable, because tau appeared in
  ## no released table at all.
  heterogeneity <- do.call(rbind, lapply(1:3, function(j) {
    v <- dr[[sprintf("tau[%d]", j)]]
    cbind(base, data.frame(coordinate = pairs[j], quantity = "tau",
                           stringsAsFactors = FALSE), estimate_summary(v))
  }))

  ## --- pooled mediation quantities at each contrast value
  ncc <- if (is_mod) length(mods[[mod]]$xc) else 1L
  pooled <- do.call(rbind, lapply(seq_len(ncc), function(ci) {
    mv <- if (is_mod) c(spec$supported_lo, spec$supported_hi)[ci] else NA_real_
    do.call(rbind, lapply(c("a", "b", "cprime", "ind", "total"), function(q) {
      v <- dr[[sprintf("%s_pop[%d]", q, ci)]]
      if (is.null(v)) return(NULL)
      cbind(base, data.frame(contrast_index = ci, moderator_value = mv, quantity = q,
                             stringsAsFactors = FALSE), estimate_summary(v))
    }))
  }))

  ## --- new-study prediction intervals
  predict <- do.call(rbind, lapply(seq_len(ncc), function(ci) {
    mv <- if (is_mod) c(spec$supported_lo, spec$supported_hi)[ci] else NA_real_
    do.call(rbind, lapply(c("a", "b", "cprime", "ind", "total"), function(q) {
      v <- dr[[sprintf("%s_new[%d]", q, ci)]]
      if (is.null(v)) return(NULL)
      cbind(base, data.frame(contrast_index = ci, moderator_value = mv, quantity = q,
                             stringsAsFactors = FALSE), estimate_summary(v))
    }))
  }))

  ## --- percentage of the pooled association carried by the indirect path.
  ##
  ## Emitted into its OWN frame. A ratio and a path coefficient never share a
  ## table, because the column sets differ by construction: this one carries no
  ## mean and no standard deviation, and a shared table would have to invent one
  ## or leave a suspicious gap.
  ##
  ## Unmoderated fits only. A percentage evaluated at, say, 60 per cent female is
  ## conditional on a moderator value in an analysis that has concluded it cannot
  ## establish moderation. See config/analysis.yml.
  pmed <- NULL
  if (!is_mod && isTRUE(cfg$proportion_mediated$unmoderated_only)) {
    prop  <- dr[["pmed_prop[1]"]]
    pct   <- dr[["pmed_pct[1]"]]
    ind   <- dr[["ind_pop[1]"]]
    cpr   <- dr[["cprime_pop[1]"]]
    total <- dr[["total_pop[1]"]]

    ## The transformation is recomputed here from the numerator and denominator
    ## draws, by code that shares no line with the Stan programme. This is what
    ## makes the ratio draw-wise rather than a claim that it is: if the
    ## extraction ever paired mismatched draws, or the generated quantity were
    ## reordered, the two would diverge immediately.
    ##
    ## The comparison is RELATIVE and tolerant, which needs justifying because
    ## exactness is the obvious expectation. cmdstan writes its draws to CSV at
    ## six significant figures, so the numerator, the denominator and the ratio
    ## are each rounded independently before R ever sees them, and a quotient
    ## reconstructed from two rounded operands cannot equal the rounded quotient.
    ## The observed disagreement is around 1e-5 relative, sitting on the draws
    ## where the ratio is largest, exactly as that rounding predicts. Shuffling
    ## the denominator draws moves it to order 10. The threshold below is two
    ## orders of magnitude above the storage precision and six below a
    ## mispairing, so it separates the two without ambiguity. The sharp version
    ## of this check, on unrounded doubles, is in the test suite.
    RATIO_PAIRING_TOL <- 1e-3
    rel <- function(x, y) max(abs(x - y) / pmax(abs(x), 1e-12))
    d_recompute <- rel(prop, ind / total)
    if (d_recompute > RATIO_PAIRING_TOL)
      stop("pmed_prop from Stan disagrees with the independent R recomputation ",
           "for ", key, ". Maximum relative difference ", format(d_recompute),
           ", against a tolerance of ", RATIO_PAIRING_TOL, " chosen to admit ",
           "six-significant-figure storage and nothing else. The numerator and ",
           "denominator draws are not paired.")
    d_units <- rel(pct, 100 * prop)
    if (d_units > RATIO_PAIRING_TOL)
      stop("pmed_pct is not 100 times pmed_prop for ", key,
           ". Maximum relative difference ", format(d_units), ".")

    ## Gate inputs, from the JOINT draws. Computing these from marginals would
    ## discard the dependence between numerator and denominator, which is
    ## precisely what makes the ratio behave.
    dg <- unlist(cfg$proportion_mediated$delta_grid)
    gate <- as.data.frame(setNames(
      lapply(dg, function(d) mean(total > d)),
      sprintf("p_total_gt_%g", dg)))
    gate$p_abs_total_lt_delta <- mean(abs(total) < cfg$proportion_mediated$delta_primary)
    gate$p_sign_consistent    <- mean(ind > 0 & cpr > 0)
    ## The denominator's own signal-to-noise, reported as a heuristic only. The
    ## standard deviation here is the DENOMINATOR's, which is well behaved; the
    ## ratio's own standard deviation does not exist and is never computed.
    gate$total_median   <- median(total)
    gate$total_sd       <- sd(total)
    gate$total_median_sd_ratio <- median(total) / sd(total)

    ## Summarised on the PERCENTAGE scale, which is the scale reported, so the
    ## interpretable range runs to 100 rather than to 1.
    pmed <- cbind(base,
                  data.frame(contrast_index = 1L, quantity = "pmed_pct",
                             scale = "per cent", stringsAsFactors = FALSE),
                  ratio_summary(pct, chain_id = dr$.chain, upper = 100), gate)

    ## The draws themselves, so the ratio can be recomputed, replotted or
    ## re-summarised by a reader without refitting. Chain and draw identifiers
    ## are kept: a pooled column cannot be checked for between-chain agreement.
    attr(pmed, "draws") <- data.frame(
      pathway = key, .chain = dr$.chain, .draw = dr$.draw,
      ind_pop = ind, cprime_pop = cpr, total_pop = total, pmed_prop = prop,
      stringsAsFactors = FALSE)
  }

  ## --- mediation contrasts. Estimation only: no defensible SESOI exists for a
  ## change in ab, and inventing one to obtain a verdict is not justified.
  contrasts <- NULL
  if (is_mod) {
    contrasts <- do.call(rbind, lapply(
      c("d_a", "d_b", "d_cprime", "d_ind", "d_total", "ind_study_mean"), function(q) {
        v <- dr[[q]]
        if (is.null(v)) return(NULL)
        cbind(base, data.frame(quantity = sub("^d_", "", q),
                               contrast = if (q == "ind_study_mean") "study average"
                                          else contrast_label(spec),
                               stringsAsFactors = FALSE),
              dienes_summary(v, decision = FALSE))
      }))
  }

  ## Ratio diagnostics only where a ratio was extracted, and in their own table.
  pmed_diag <- NULL
  if (!is.null(pmed)) {
    pmed_diag <- ratio_diagnostics(fit, c("pmed_prop", "pmed_pct"))
    if (!is.null(pmed_diag)) pmed_diag <- cbind(base, pmed_diag)
  }

  list(slopes = slopes, pooled = pooled, predict = predict, contrasts = contrasts,
       heterogeneity = heterogeneity, pmed = pmed, pmed_diag = pmed_diag,
       diagnostics = cbind(base, fit_diagnostics(fit, attr(fit, "fit_id"), cfg)))
}
