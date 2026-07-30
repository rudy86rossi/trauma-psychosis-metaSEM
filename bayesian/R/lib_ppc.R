# =============================================================================
# lib_ppc.R
# Posterior predictive checking, in both forms.
#
# CONDITIONAL replication reuses each study's fitted random effect and asks
# whether the likelihood describes that study's observed triplet given where the
# model has placed it. NEW-STUDY replication draws a fresh random effect and
# asks whether the between-study distribution could generate data like these at
# all.
#
# Both are archived because a conditional check alone can look satisfactory
# simply because the random effects have absorbed an outlying study. Prior
# predictive plots do not substitute for either.
# =============================================================================

#' Study-level and coordinate-level posterior predictive summaries for one fit.
ppc_summaries <- function(cfg, fit, key, mod, df) {
  dr <- as.data.frame(fit$draws(
    variables = c("z_rep_cond", "z_rep_new", "maha_obs", "maha_rep", "resid_z"),
    format = "df"))
  pairs <- PAIRS(cfg)
  K <- nrow(df)
  z_obs <- atanh(as.matrix(df[, pairs]))
  mname <- mod %||% "null"

  ## --- per study: discrepancy, its predictive p-value, and both replications
  study <- do.call(rbind, lapply(seq_len(K), function(i) {
    mo <- dr[[sprintf("maha_obs[%d]", i)]]
    mr <- dr[[sprintf("maha_rep[%d]", i)]]
    data.frame(
      pathway = key, moderator = mname,
      sample_id = df$sample_id[i], study = paste(df$studyID[i], df$year[i]),
      n = df$n[i],
      maha_obs_median = median(mo), maha_rep_median = median(mr),
      ## A Bayesian posterior predictive p-value. Values near 0 or 1 flag a
      ## study the model reproduces poorly. With three coordinates and this k,
      ## treat it as a flag for inspection rather than a test.
      ppp = mean(mr >= mo),
      extreme = as.integer(mean(mr >= mo) < 0.05 | mean(mr >= mo) > 0.95),
      stringsAsFactors = FALSE)
  }))

  ## --- per study per coordinate: observed against both replication forms
  coord <- do.call(rbind, lapply(seq_len(K), function(i) {
    do.call(rbind, lapply(1:3, function(j) {
      rc <- dr[[sprintf("z_rep_cond[%d,%d]", i, j)]]
      rn <- dr[[sprintf("z_rep_new[%d,%d]", i, j)]]
      rs <- dr[[sprintf("resid_z[%d,%d]", i, j)]]
      data.frame(
        pathway = key, moderator = mname, sample_id = df$sample_id[i],
        study = paste(df$studyID[i], df$year[i]), coordinate = pairs[j],
        z_observed = z_obs[i, j],
        cond_median = median(rc), cond_lo = quantile(rc, 0.025, names = FALSE),
        cond_hi = quantile(rc, 0.975, names = FALSE),
        cond_covers = z_obs[i, j] >= quantile(rc, 0.025, names = FALSE) &
                      z_obs[i, j] <= quantile(rc, 0.975, names = FALSE),
        new_median = median(rn), new_lo = quantile(rn, 0.025, names = FALSE),
        new_hi = quantile(rn, 0.975, names = FALSE),
        new_covers = z_obs[i, j] >= quantile(rn, 0.025, names = FALSE) &
                     z_obs[i, j] <= quantile(rn, 0.975, names = FALSE),
        resid_median = median(rs),
        stringsAsFactors = FALSE)
    }))
  }))

  ## --- whole-model summary
  overall <- data.frame(
    pathway = key, moderator = mname, k_studies = K,
    coverage_conditional = mean(coord$cond_covers),
    coverage_new_study = mean(coord$new_covers),
    n_extreme_ppp = sum(study$extreme),
    worst_study = study$study[which.max(abs(study$ppp - 0.5))],
    worst_ppp = study$ppp[which.max(abs(study$ppp - 0.5))],
    max_maha_obs = max(study$maha_obs_median),
    stringsAsFactors = FALSE)

  list(study = study, coordinate = coord, overall = overall)
}

#' Long-format observed and replicated values for the graphical check, thinned so
#' the archived table stays small.
ppc_draws_for_plot <- function(cfg, fit, key, mod, df, n_rep = 100) {
  dr <- as.data.frame(fit$draws(variables = c("z_rep_cond", "z_rep_new"), format = "df"))
  idx <- round(seq(1, nrow(dr), length.out = min(n_rep, nrow(dr))))
  pairs <- PAIRS(cfg)
  do.call(rbind, lapply(seq_len(nrow(df)), function(i) {
    do.call(rbind, lapply(1:3, function(j) {
      data.frame(
        pathway = key, moderator = mod %||% "null",
        study = paste(df$studyID[i], df$year[i]), coordinate = pairs[j],
        draw = seq_along(idx),
        conditional = dr[[sprintf("z_rep_cond[%d,%d]", i, j)]][idx],
        new_study   = dr[[sprintf("z_rep_new[%d,%d]", i, j)]][idx],
        observed    = atanh(df[[pairs[j]]][i]),
        stringsAsFactors = FALSE)
    }))
  }))
}
