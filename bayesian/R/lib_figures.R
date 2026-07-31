# =============================================================================
# lib_figures.R
# Publication figures: vector PDF plus 300-dpi PNG, manuscript labels rather
# than internal codes, and reportability shown rather than implied.
#
# A figure that hides a `not reported` model, or that labels a clamped 52-to-60
# contrast as a full 40-to-60 change, misleads more efficiently than prose.
# =============================================================================

suppressPackageStartupMessages(library(ggplot2))

PATH_LABELS <- c(
  dissoc_hal  = "Dissociation → hallucinations",
  dissoc_del  = "Dissociation → delusions",
  dissoc_cps  = "Dissociation → composite psychosis",
  negsch_cps  = "Negative schema → composite psychosis",
  negsch_del  = "Negative schema → delusions",
  depress_cps = "Depression → composite psychosis")

COORD_LABELS <- c(r_XM = "r(trauma, mediator)",
                  r_XY = "r(trauma, psychosis)",
                  r_MY = "r(mediator, psychosis)")

REPORT_COLOURS <- c(ok = "#1B4965", fragile = "#C77D24", `not reported` = "#9AA0A6")

theme_release <- function(base_size = 9) {
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold", size = base_size - 1),
          ## Right-hand strips are rotated 90 degrees by default, so a long
          ## facet label is clipped to a fragment: the released figure 1 read
          ## "ale (per 10 percentage po" and "lamped amped". Horizontal text
          ## with a wrapped label is legible and cannot be silently truncated.
          strip.text.y.right = element_text(angle = 0, hjust = 0),
          panel.spacing.x = unit(1.1, "lines"),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.margin = margin(0, 0, 0, 0),
          plot.caption = element_text(hjust = 0, colour = "grey30", size = base_size - 2),
          plot.caption.position = "plot",
          plot.margin = margin(6, 14, 6, 6))
}

#' Hard-wrap a caption.
#'
#' ggplot does not wrap captions, so a long one runs off the canvas and is lost
#' at the device edge. The released figure 1 caption ended mid-sentence at
#' "so a", with the remainder simply absent.
wrap_caption <- function(..., width = 118) {
  paste(vapply(c(...), function(s)
    paste(strwrap(s, width = width), collapse = "\n"), character(1)),
    collapse = "\n")
}

#' Fewer, rounder x-axis breaks.
#'
#' Adjacent free-scale facets each drew their own labels at the panel edges, so
#' "-0.50" and "0.50" collided into "-0.50.50" between every pair of columns.
x_breaks <- function(n = 4) function(lim) pretty(lim, n = n)

#' Save one figure in both formats. PDF is the artefact of record; the PNG is
#' for convenience and is written at 300 dpi, not screen resolution.
#'
#' Default width is a manuscript double-column measure rather than an arbitrary
#' nine inches, so labels are sized as they will be printed.
save_figure <- function(p, dir, name, width = 8.5, height = 5) {
  ggsave(file.path(dir, paste0(name, ".pdf")), p, width = width, height = height,
         device = grDevices::cairo_pdf)
  ggsave(file.path(dir, paste0(name, ".png")), p, width = width, height = height,
         dpi = 300, bg = "white")
  invisible(file.path(dir, paste0(name, c(".pdf", ".png"))))
}

make_figures <- function(cfg, tables, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  s <- tables$slopes
  if (is.null(s) || !nrow(s)) return(invisible(NULL))
  s$pathway_label   <- PATH_LABELS[s$pathway]
  s$coordinate_label <- COORD_LABELS[s$coordinate]
  s$moderator_label <- vapply(s$moderator, function(m) cfg$moderators[[m]]$label %||% m,
                              character(1))
  s$reportable <- factor(s$reportable, levels = names(REPORT_COLOURS))
  s$layer <- ifelse(s$fit_class == "stan_primary",
                    "Primary (valid-correlation)", "Reference (brms)")
  ## The contrast actually estimated, never the nominal label when they differ.
  ## The moderator name is NOT repeated here: it is the subtitle of the file
  ## this panel appears in, and repeating it made the strip label three wrapped
  ## lines deep, which overflowed the row and clipped "(clamped to support)".
  s$panel <- ifelse(s$clamped, paste0(s$contrast, "\n(clamped to support)"), s$contrast)

  cap_sesoi <- paste(
    "Shaded band is the null interval, scaled to the contrast actually estimated:",
    "0.05 Fisher-z across the nominal contrast, held at a constant per-unit threshold,",
    "so a clamped contrast is not granted a looser test. Both layers are shown on the",
    "same estimand. A negative value for sample type means the correlation is lower in",
    "clinical samples.")

  # --- Figure 1: full-contrast change in each zero-order coordinate ----------
  #
  # One file per moderator. The combined version was 9 by 11 inches with four
  # moderators stacked, which is unusable at manuscript size and was the reason
  # its facet labels were clipped. Each moderator is a self-contained panel that
  # can be placed, or dropped, on its own.
  fig1_panel <- function(d, subtitle, cap) {
    ggplot(d, aes(x = median, y = pathway_label, colour = reportable)) +
      geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.3) +
      geom_rect(aes(xmin = -sesoi, xmax = sesoi, ymin = -Inf, ymax = Inf),
                fill = "grey92", colour = NA, alpha = 0.35, inherit.aes = FALSE,
                data = unique(d[!is.na(d$sesoi), c("panel", "sesoi")])) +
      geom_linerange(aes(xmin = hdi_lower, xmax = hdi_upper),
                     position = position_dodge(width = 0.55), linewidth = 0.5) +
      geom_point(aes(shape = layer), position = position_dodge(width = 0.55), size = 1.7) +
      facet_grid(panel ~ coordinate_label, scales = "free_y", space = "free_y",
                 labeller = labeller(panel = label_wrap_gen(18))) +
      scale_x_continuous(breaks = x_breaks(4), expand = expansion(mult = 0.08)) +
      scale_colour_manual(values = REPORT_COLOURS, drop = FALSE, name = "Reportability") +
      scale_shape_manual(values = c(16, 1), name = "Model layer") +
      labs(x = "Change in Fisher-z across the estimated contrast (95% HDI)", y = NULL,
           title = "Moderation as a full-contrast change in each zero-order correlation",
           subtitle = subtitle, caption = wrap_caption(cap)) +
      theme_release()
  }

  for (m in unique(s$moderator)) {
    d <- s[s$moderator == m, ]
    h <- 2.4 + 0.34 * length(unique(paste(d$panel, d$pathway_label)))
    save_figure(fig1_panel(d, unique(d$moderator_label)[1], cap_sesoi),
                dir, paste0("fig1_moderator_contrasts_", m), height = h)
  }

  # --- Figure 2: pooled mediation quantities ---------------------------------
  p <- tables$pooled[tables$pooled$moderator == "null" &
                       tables$pooled$quantity %in% c("a", "b", "cprime", "ind", "total"), ]
  if (nrow(p)) {
    p$pathway_label <- PATH_LABELS[p$pathway]
    p$quantity <- factor(p$quantity, levels = c("a", "b", "cprime", "ind", "total"),
                         labels = c("a (X→M)", "b (M→Y|X)", "c' (X→Y|M)",
                                    "ab (indirect)", "total"))
    f2 <- ggplot(p, aes(x = median, y = pathway_label)) +
      geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.3) +
      geom_linerange(aes(xmin = q_lo, xmax = q_hi), linewidth = 0.5, colour = "#1B4965") +
      geom_point(size = 1.9, colour = "#1B4965") +
      facet_wrap(~ quantity, nrow = 1, scales = "free_x") +
      scale_x_continuous(breaks = x_breaks(3), expand = expansion(mult = 0.12)) +
      labs(x = "Standardised coefficient (95% equal-tailed credible interval)", y = NULL,
           title = "Pooled mediation quantities, unmoderated models",
           caption = wrap_caption(paste(
             "Population-average quantities from the primary valid-correlation model.",
             "Intervals are equal-tailed; one highest-density interval includes zero where",
             "the equal-tailed interval does not, so the convention is stated."))) +
      theme_release()
    save_figure(f2, dir, "fig2_pooled_quantities", height = 3.6)
  }

  # --- Figure 3: new-study prediction intervals for the indirect effect ------
  pr <- tables$predict[tables$predict$moderator == "null" & tables$predict$quantity == "ind", ]
  if (nrow(pr)) {
    pr$pathway_label <- PATH_LABELS[pr$pathway]
    pr$includes_zero <- pr$q_lo < 0 & pr$q_hi > 0
    f3 <- ggplot(pr, aes(x = median, y = pathway_label, colour = includes_zero)) +
      geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.3) +
      geom_linerange(aes(xmin = q_lo, xmax = q_hi), linewidth = 0.6) +
      geom_point(size = 2) +
      scale_colour_manual(values = c(`TRUE` = "#9AA0A6", `FALSE` = "#1B4965"),
                          labels = c(`TRUE` = "includes zero", `FALSE` = "excludes zero"),
                          name = NULL) +
      labs(x = "Latent true indirect effect ab in a new study (95% prediction interval)", y = NULL,
           title = "What to expect in a new study",
           caption = wrap_caption(paste(
             "Intervals are for the LATENT TRUE indirect effect in a new study drawn from the",
             "estimated population; they do not add the sampling error a future observed",
             "estimate would also carry, so a realised estimate would scatter more widely.",
             "Their width is driven entirely by the between-study heterogeneity tau, which is",
             "weakly identified at this number of studies and therefore substantially",
             "prior-informed."))) +
      theme_release()
    save_figure(f3, dir, "fig3_prediction_intervals", height = 3.6)
  }

  # --- Figure 4: posterior predictive check, both replication forms ----------
  cc <- tables$ppc_coordinate
  if (!is.null(cc) && nrow(cc)) {
    cc <- cc[cc$moderator == "null", ]
    cc$pathway_label <- PATH_LABELS[cc$pathway]
    cc$coordinate_label <- COORD_LABELS[cc$coordinate]
    long <- rbind(
      data.frame(cc[, c("pathway_label", "coordinate_label", "study", "z_observed")],
                 kind = "Conditional (fitted study effects)",
                 lo = cc$cond_lo, hi = cc$cond_hi, mid = cc$cond_median),
      data.frame(cc[, c("pathway_label", "coordinate_label", "study", "z_observed")],
                 kind = "New study (fresh random effects)",
                 lo = cc$new_lo, hi = cc$new_hi, mid = cc$new_median))
    ## One file per pathway. The combined sheet was 9 by 13 inches, which no
    ## journal will set, and its facet labels were truncated for the same reason
    ## as figure 1.
    cap4 <- paste(
      "Crosses are observed values; bars are 95% replicated intervals. Both forms are shown",
      "because a conditional check alone can look satisfactory simply because the study",
      "effects have absorbed an outlying study. Complete coverage is a weak result at this",
      "number of studies and does not demonstrate predictive calibration.")
    for (pl in unique(long$pathway_label)) {
      d <- long[long$pathway_label == pl, ]
      f4 <- ggplot(d, aes(y = study)) +
        geom_linerange(aes(xmin = lo, xmax = hi, colour = kind),
                       position = position_dodge(width = 0.6), linewidth = 0.5) +
        geom_point(aes(x = z_observed), shape = 4, size = 1.6, colour = "black") +
        facet_wrap(~ coordinate_label, nrow = 1) +
        scale_x_continuous(breaks = x_breaks(3), expand = expansion(mult = 0.1)) +
        scale_colour_manual(values = c("#1B4965", "#C77D24"), name = NULL) +
        labs(x = "Fisher-z", y = NULL,
             title = "Posterior predictive check: both replication forms",
             subtitle = pl, caption = wrap_caption(cap4)) +
        theme_release()
      save_figure(f4, dir, paste0("fig4_posterior_predictive_",
                                  names(PATH_LABELS)[match(pl, PATH_LABELS)]),
                  height = 1.9 + 0.22 * length(unique(d$study)))
    }
  }

  # --- Figure 5: SBC rank uniformity ----------------------------------------
  #
  # The caption previously printed "NA simulated datasets per scenario" because
  # it read tables$sbc_audit$n_datasets, a column that has never existed on that
  # data frame, and then asserted that failed fits remained in the denominator,
  # which the code did not do. Both numbers now come from the realised counts.
  sb <- tables$sbc
  if (!is.null(sb) && nrow(sb)) {
    a  <- tables$sbc_audit
    by <- tables$sbc_by_scenario
    n_lab <- if (!is.null(by) && nrow(by)) {
      u <- sort(unique(by$used_primary))
      if (length(u) == 1) sprintf("%d", u) else sprintf("%d to %d", min(u), max(u))
    } else "an unrecorded number of"

    sb$family <- factor(sb$family)
    f5 <- ggplot(sb, aes(x = p_value, y = parameter, colour = family)) +
      geom_vline(xintercept = 0.05, linetype = 2, colour = "grey55") +
      geom_point(size = 2) +
      facet_wrap(~ scenario, scales = "free_y") +
      scale_x_continuous(limits = c(0, 1)) +
      labs(x = "Rank-uniformity p value", y = NULL, colour = "Parameter family",
           title = "Simulation-based calibration: rank uniformity by parameter",
           caption = paste(
             sprintf(paste("Screening level. %s of %d attempted datasets per scenario",
                           "entered the primary test; %d were excluded for breaching a",
                           "diagnostic threshold and %d failed outright."),
                     n_lab, a$total_fits / max(1, nrow(by)),
                     a$fits_diag_failed, a$fits_failed),
             sprintf(paste("%d rank cells. Conditional on the plug-in sampling covariance",
                           "matrices. p values are correlated across parameters and",
                           "scenarios, so they are not an independent family."),
                     a$n_cells),
             sep = "\n")) +
      theme_release()
    save_figure(f5, dir, "fig5_sbc_rank_uniformity", height = 6.5)
  }
  invisible(TRUE)
}
