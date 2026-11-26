# =============================================================================
# lib_frequentist.R
# The corrected frequentist meta-regressions, refitted on the locked data.
#
# These are INTERNAL VALIDATION TARGETS, not co-primary results. Their p values
# are not used to arbitrate a disagreement with the posterior. They exist so
# that the Bayesian analysis can be shown to be operating on the same data, and
# so the manuscript can report how many slopes the frequentist analysis called
# significant without that number being transcribed by hand.
#
# The models are metaSEM::meta() with a diagonal Tau2, matching the corrected
# scripts under metaSEM/.
# =============================================================================

#' Fit every pathway-by-moderator frequentist meta-regression.
frequentist_slopes <- function(cfg, lock) {
  pairs <- PAIRS(cfg)
  out <- list()

  for (key in SYNTH_NAMES(lock)) {
    df <- lock$analysis[[key]]
    V  <- metaSEM::asyCov(
      lapply(seq_len(nrow(df)), function(i) make_R(df$r_XM[i], df$r_XY[i], df$r_MY[i])),
      df$n)
    y <- as.matrix(df[, pairs])

    for (m in MOD_NAMES(cfg)) {
      mm  <- cfg$moderators[[m]]
      idx <- which(!is.na(df[[m]]))
      if (length(idx) < 3) next
      spec <- lock$support[lock$support$pathway == key & lock$support$moderator == m, ]
      rp   <- synth_reportability(lock)
      status <- rp$status[rp$pathway == key & rp$moderator == m]

      x <- df[[m]][idx]
      if (!identical(mm$kind, "binary")) x <- (x - mean(x)) / mm$unit

      ## Matches the corrected frequentist scripts: a diagonal Tau2 built with
      ## metaSEM::Diag(), and the moderator supplied as a one-column matrix.
      fit <- tryCatch(suppressWarnings(metaSEM::meta(
        y = y[idx, , drop = FALSE], v = V[idx, , drop = FALSE],
        x = matrix(x, ncol = 1),
        RE.constraints = metaSEM::Diag(paste0("0.1*Tau2_", 1:3, "_", 1:3)),
        model.name = paste(key, m))), error = function(e) NULL)
      if (is.null(fit)) next

      s <- summary(fit)$coefficients
      sl <- s[grep("^Slope", rownames(s)), , drop = FALSE]
      if (!nrow(sl)) next

      out[[paste(key, m)]] <- data.frame(
        pathway = key, label = df$label[1], moderator = m, coordinate = pairs,
        k = length(idx), reportable = status,
        status_code = fit$mx.fit@output$status[[1]],
        ## The frequentist coefficient is per fitted unit; the Bayesian decision
        ## is taken on the full contrast. Both are given so the comparison in
        ## the manuscript is on one scale.
        slope_per_unit = sl[, "Estimate"],
        se_per_unit = sl[, "Std.Error"],
        slope_full_contrast = sl[, "Estimate"] * spec$supported_span,
        se_full_contrast = sl[, "Std.Error"] * spec$supported_span,
        z = sl[, "Estimate"] / sl[, "Std.Error"],
        p = 2 * pnorm(-abs(sl[, "Estimate"] / sl[, "Std.Error"])),
        stringsAsFactors = FALSE)
    }
  }
  res <- do.call(rbind, out); rownames(res) <- NULL
  res
}

#' Headline counts, generated rather than transcribed.
frequentist_counts <- function(ft) {
  sig <- ft[is.finite(ft$p) & ft$p < 0.05, ]
  list(n_slopes = nrow(ft),
       n_significant = nrow(sig),
       n_models = nrow(unique(ft[, c("pathway", "moderator")])),
       n_models_with_significant = nrow(unique(sig[, c("pathway", "moderator")])),
       n_reportable = sum(ft$reportable != "not reported"),
       n_significant_reportable = sum(sig$reportable != "not reported"))
}
