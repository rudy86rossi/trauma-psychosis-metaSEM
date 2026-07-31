# =============================================================================
# lib_fitplan.R
# The normalised fit plan, and the dispatcher that executes one row of it.
#
# The plan is a table of VALID model specifications with a stable `fit_id`, not
# a Cartesian product of labels: a leave-one-study-out row exists only for a
# study that is actually in that model, and an alternative-instrument row only
# where a sample supplied a second instrument.
#
# The release gate requires exact set equality between the expected `fit_id`
# values and the completed ones. The expected count is derived from this table
# and is not written down anywhere as a constant.
#
# Every branch returns COMPACT results: summaries, diagnostics and ranks. Fitted
# objects and full draws are not carried through the pipeline.
# =============================================================================

#' Build the complete fit plan.
build_fit_plan <- function(cfg, lock) {
  rows <- list()
  add <- function(...) rows[[length(rows) + 1L]] <<- data.frame(..., stringsAsFactors = FALSE)

  mods_all <- c(list(NULL), as.list(MOD_NAMES(cfg)))
  rp <- lock$reportability

  for (key in PATH_NAMES(cfg)) {
    df <- lock$analysis[[key]]
    for (m in mods_all) {
      mname <- m %||% "null"
      status <- if (is.null(m)) "ok" else rp$status[rp$pathway == key & rp$moderator == m]

      ## --- primary fits, both layers
      add(fit_class = "stan_primary", fit_id = sprintf("stan|%s|%s", key, mname),
          pathway = key, moderator = mname, reportable = status,
          variant = "primary", drop_sample = NA_character_)
      add(fit_class = "brms_primary", fit_id = sprintf("brms|%s|%s", key, mname),
          pathway = key, moderator = mname, reportable = status,
          variant = "primary", drop_sample = NA_character_)

      ## --- prior predictive: one per pathway, no moderator
      if (is.null(m))
        add(fit_class = "prior_predictive", fit_id = sprintf("prior|%s", key),
            pathway = key, moderator = mname, reportable = status,
            variant = "prior_only", drop_sample = NA_character_)

      ## --- robust Student-t and weighted-asyCov V_z: outlier and construction
      ## sensitivity, prespecified for every model
      if (isTRUE(cfg$sensitivity$robust_student_t))
        add(fit_class = "sens_robust_t", fit_id = sprintf("robust|%s|%s", key, mname),
            pathway = key, moderator = mname, reportable = status,
            variant = "student_t", drop_sample = NA_character_)
      if (isTRUE(cfg$sensitivity$vz_weighted))
        add(fit_class = "sens_vz_weighted", fit_id = sprintf("vzw|%s|%s", key, mname),
            pathway = key, moderator = mname, reportable = status,
            variant = "asyCov_weighted", drop_sample = NA_character_)

      ## --- alternative instrument, only where a second one exists
      if (isTRUE(cfg$sensitivity$alternative_instrument) && !is.null(lock$alternative[[key]]))
        add(fit_class = "sens_alt_instrument", fit_id = sprintf("alt|%s|%s", key, mname),
            pathway = key, moderator = mname, reportable = status,
            variant = "alternative_instrument", drop_sample = NA_character_)

      ## --- moderator-only classes
      if (!is.null(m)) {
        ## Prior grid: vary ONE component at a time about the primary, on the
        ## scientific (full-contrast) scale.
        if (isTRUE(cfg$sensitivity$prior_grid)) {
          for (comp in c("alpha_sd", "beta_full_sd", "tau_sd")) {
            for (v in setdiff(unlist(cfg$priors$grid[[comp]]), cfg$priors$primary[[comp]]))
              add(fit_class = "sens_prior_grid",
                  fit_id = sprintf("prior_grid|%s|%s|%s=%g", key, mname, comp, v),
                  pathway = key, moderator = mname, reportable = status,
                  variant = sprintf("%s=%g", comp, v), drop_sample = NA_character_)
          }
        }
        ## Leave one study out, on both layers. Only studies actually present in
        ## this model produce a row.
        if (isTRUE(cfg$sensitivity$leave_one_study_out)) {
          present <- df$sample_id[lock$mods[[key]][[m]]$keep]
          for (s in present) {
            ## Skip a deletion that leaves no contrast to estimate: an empty
            ## binary arm, or a continuous moderator reduced to one distinct
            ## value. Such a model has no identified slope, so fitting it would
            ## spend a chain to produce a number that means nothing. Deletions
            ## that merely become non-reportable ARE kept: they are legitimate
            ## estimates, they are labelled as unreportable by moderator_spec(),
            ## and excluding them silently would hide the fragility.
            if (!loso_is_estimable(cfg, df, m, s)) next
            add(fit_class = "sens_loso_stan",
                fit_id = sprintf("loso_stan|%s|%s|%s", key, mname, s),
                pathway = key, moderator = mname, reportable = status,
                variant = "leave_one_out", drop_sample = s)
            add(fit_class = "sens_loso_brms",
                fit_id = sprintf("loso_brms|%s|%s|%s", key, mname, s),
                pathway = key, moderator = mname, reportable = status,
                variant = "leave_one_out", drop_sample = s)
          }
        }
      }
    }
  }

  plan <- do.call(rbind, rows)
  if (anyDuplicated(plan$fit_id))
    stop("Fit plan contains duplicate fit_id values: ",
         paste(unique(plan$fit_id[duplicated(plan$fit_id)]), collapse = ", "))
  plan$layer <- ifelse(grepl("brms", plan$fit_class), "brms", "stan")
  rownames(plan) <- NULL
  plan
}

#' Does deleting `sample_id` leave a moderator slope that can be estimated at
#' all? Distinct from reportability: this is identification, not policy.
loso_is_estimable <- function(cfg, df, m, sample_id) {
  v <- df[[m]][df$sample_id != sample_id]
  v <- v[!is.na(v)]
  if (length(v) < 2) return(FALSE)
  if (identical(cfg$moderators[[m]]$kind, "binary"))
    return(sum(v == 1) >= 1 && sum(v == 0) >= 1)
  length(unique(v)) >= 2
}

#' Prior list for a sensitivity variant, on the full-contrast scale.
variant_priors <- function(cfg, variant) {
  p <- cfg$priors$primary
  if (grepl("=", variant, fixed = TRUE)) {
    kv <- strsplit(variant, "=", fixed = TRUE)[[1]]
    p[[kv[1]]] <- as.numeric(kv[2])
  }
  p
}

#' Drop one sample from a pathway's data and its aligned V_z and moderator.
drop_one_sample <- function(cfg, lock, key, mod, sample_id) {
  df <- lock$analysis[[key]]
  keep <- df$sample_id != sample_id
  d <- df[keep, , drop = FALSE]
  v <- lock$vz_chol[[key]][keep]
  ## The moderator is recentred on the surviving studies, and the reported
  ## contrast is re-clamped to their observed support, so a deletion analysis is
  ## centred and bounded by the data it actually uses.
  mods <- rebuild_mods(cfg, d)
  list(df = d, vz = v, mods = mods)
}

#' Rebuild the moderator structures for an arbitrary subset of studies.
#'
#' Reportability is RE-DERIVED here, not inherited. Deleting a study can collapse
#' a binary arm below the minimum (negsch_del by clinical is 3 clinical to 2
#' non-clinical, so two of its five deletions leave a group of one), and it can
#' shorten the supported span of a continuous moderator. A refit that is no
#' longer reportable must not carry a categorical verdict, which is why the
#' status travels with the structure rather than beside it.
rebuild_mods <- function(cfg, d) {
  out <- list()
  for (m in MOD_NAMES(cfg)) {
    ms <- moderator_spec(cfg, d, m)
    out[[m]] <- ms[c("keep", "x", "centre", "unit", "xc", "spec")]
    out[[m]]$status <- ms$status
    out[[m]]$k      <- ms$k
    out[[m]]$note   <- ms$note
  }
  out
}

## ---- the dispatcher ---------------------------------------------------------

#' Execute one row of the fit plan, recording a failure rather than aborting the
#' whole branch.
#'
#' A failed fit must still be fatal to the RELEASE, and it is: the completeness
#' gate compares the expected fit_id set against the completed one and stops if
#' any is missing. What this adds is identity. When a Stan fit produces no
#' usable chains, cmdstanr raises from inside the diagnostic call and the
#' exception says nothing about which of several hundred specifications died,
#' so the run has to be repeated blind to find out. Recording the fit_id and the
#' message means one run identifies the culprit.
safe_run_fit <- function(cfg, lock, row) {
  tryCatch(run_fit(cfg, lock, row), error = function(e) {
    msg <- gsub("[\r\n]+", " ", conditionMessage(e))
    message("FIT FAILED: ", row$fit_id, " : ", msg)
    list(fit_id = row$fit_id, fit_class = row$fit_class, status = "error",
         error = msg, diagnostics = NULL, slopes = NULL, pooled = NULL,
         predict = NULL, contrasts = NULL, ppc = NULL, draws_d_z = NULL,
         seconds = NA_real_)
  })
}

#' Execute one row of the fit plan and return compact results.
run_fit <- function(cfg, lock, row) {
  key   <- row$pathway
  mod   <- if (identical(row$moderator, "null")) NULL else row$moderator
  t0    <- Sys.time()

  df   <- lock$analysis[[key]]
  vz   <- lock$vz_chol[[key]]
  mods <- lock$mods[[key]]
  priors <- cfg$priors$primary
  robust <- FALSE; prior_only <- FALSE

  switch(row$fit_class,
    sens_vz_weighted    = { vz <- lock$vz_chol_weighted[[key]] },
    sens_robust_t       = { robust <- TRUE },
    prior_predictive    = { prior_only <- TRUE },
    sens_prior_grid     = { priors <- variant_priors(cfg, row$variant) },
    sens_alt_instrument = {
      df   <- lock$alternative[[key]]
      vz   <- lock$vz_chol_alt[[key]]
      mods <- rebuild_mods(cfg, df)
    },
    NULL)

  if (row$fit_class %in% c("sens_loso_stan", "sens_loso_brms")) {
    dd   <- drop_one_sample(cfg, lock, key, mod, row$drop_sample)
    df   <- dd$df; vz <- dd$vz; mods <- dd$mods
  }

  spec <- if (is.null(mod)) NULL else mods[[mod]]$spec

  ## Reportability of THIS fit, not of the model it was derived from. For a
  ## primary fit the two agree by construction; for a deletion or an instrument
  ## substitution they can differ, and the refit's own status is the honest one.
  ## row$reportable is retained in the plan as the parent's status.
  reportable <- if (is.null(mod)) row$reportable else mods[[mod]]$status

  ## The studies the model is actually fitted on. A moderator with a missing
  ## value drops its study, so the pathway data frame is not the fitted set.
  ## Summaries and posterior predictive checks must use the fitted set: indexing
  ## replicated quantities by the full frame reads past the end of the draws,
  ## and the reported number of samples would otherwise overstate k.
  keep_fit <- if (is.null(mod)) rep(TRUE, nrow(df)) else mods[[mod]]$keep
  df_fit   <- df[keep_fit, , drop = FALSE]

  ## --- brms branches
  if (identical(row$layer, "brms")) {
    bl  <- build_long(cfg, df, vz, mod, mods)      # applies the same mask
    fit <- fit_brms_model(cfg, bl, mod, row$fit_id, priors)
    dg  <- cbind(data.frame(pathway = key, moderator = row$moderator,
                            fit_class = row$fit_class, variant = row$variant,
                            drop_sample = row$drop_sample,
                            reportable = reportable, parent_reportable = row$reportable,
                            stringsAsFactors = FALSE),
                 brms_diagnostics(fit, row$fit_id))
    slopes <- NULL
    if (!is.null(mod)) {
      dz <- brms_d_z(cfg, fit, spec)
      slopes <- do.call(rbind, lapply(PAIRS(cfg), function(pr) {
        s <- dienes_summary(dz[, pr], sesoi = spec$sesoi_reported,
                            decision = isTRUE(spec$decision_permitted),
                            sesoi_grid = sesoi_grid(cfg),
                            prior_sd = spec$prior_beta_unit_sd * spec$supported_span)
        cbind(data.frame(fit_id = row$fit_id, fit_class = row$fit_class,
                         pathway = key, moderator = row$moderator,
                         variant = row$variant, drop_sample = row$drop_sample,
                         coordinate = pr, estimand = "d_z",
                         contrast = contrast_label(spec),
                         span_units = spec$supported_span, clamped = spec$clamped,
                         reportable = reportable, stringsAsFactors = FALSE), s)
      }))
    }
    draws <- if (identical(row$fit_class, "brms_primary") && !is.null(mod))
      brms_d_z(cfg, fit, spec) else NULL
    return(list(fit_id = row$fit_id, fit_class = row$fit_class, status = "ok",
                diagnostics = dg, slopes = slopes, draws_d_z = draws,
                seconds = as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }

  ## --- Stan branches
  sd_ <- make_stan_data(cfg, df, vz, mod, mods, priors = priors,
                        robust = robust, prior_only = prior_only)
  fit <- fit_masem(cfg, sd_, row$fit_id)

  ex <- extract_stan(cfg, fit, key, mod,
                     spec %||% scale_spec(cfg, MOD_NAMES(cfg)[1]),
                     df_fit, mods, reportable)
  for (nm in c("slopes", "pooled", "predict", "contrasts")) {
    if (!is.null(ex[[nm]]))
      ex[[nm]] <- cbind(data.frame(fit_id = row$fit_id, fit_class = row$fit_class,
                                   variant = row$variant, drop_sample = row$drop_sample,
                                   stringsAsFactors = FALSE), ex[[nm]])
  }
  ex$diagnostics <- cbind(data.frame(fit_class = row$fit_class, variant = row$variant,
                                     drop_sample = row$drop_sample,
                                     parent_reportable = row$reportable,
                                     stringsAsFactors = FALSE), ex$diagnostics)

  ## Posterior predictive checks and matched draws, for the primary fits only.
  ppc <- NULL; draws <- NULL
  if (identical(row$fit_class, "stan_primary")) {
    ppc <- ppc_summaries(cfg, fit, key, mod, df_fit)
    if (!is.null(mod)) draws <- stan_d_z(fit)
  }

  list(fit_id = row$fit_id, fit_class = row$fit_class, status = "ok",
       diagnostics = ex$diagnostics, slopes = ex$slopes, pooled = ex$pooled,
       predict = ex$predict, contrasts = ex$contrasts, ppc = ppc, draws_d_z = draws,
       seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}
