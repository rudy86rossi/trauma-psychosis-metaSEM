# Journal artwork for the NBR revision, drawn from a release's tables.
#   Rscript nbr_figures.R ANALYSIS_REPO RUN_DIR OUT_DIR
#
# Figure 2 (pooled mediation quantities) and Figure S1 (new-study prediction
# intervals for ab), redrawn with the release's own labels and theme but for
# a journal page: no title or caption inside the image (those belong in the
# legend), k shown in each pathway label, 190 mm full-page width, Arial.
# Outputs per figure: vector PDF (cairo, fonts embedded), EPS (cairo), a
# 1000 dpi LZW TIFF only as a fallback for systems that reject vector files,
# and a 300 dpi PNG preview. The release's save_figure() is not changed.

args <- commandArgs(trailingOnly = TRUE); stopifnot(length(args) == 3)
repo <- args[1]; run <- args[2]; out <- args[3]
suppressPackageStartupMessages(library(ggplot2))
source(file.path(repo, "bayesian", "R", "lib_figures.R"))
dir.create(out, showWarnings = FALSE, recursive = TRUE)

WIDTH_MM <- 190
FAMILY <- "Arial"
## Elsevier: lettering of at least 7 pt at final printed size. theme_minimal()
## scales axis text to 0.8 of the base, which would print at 6.4 pt, so the
## smaller elements are pinned at 7 pt explicitly.
theme_journal <- function() theme_release(base_size = 8) +
  theme(text = element_text(family = FAMILY), axis.text = element_text(size = 7),
        strip.text = element_text(size = 7, face = "bold"), legend.text = element_text(size = 7),
        plot.margin = margin(4, 8, 4, 4))

save_journal <- function(p, name, height_mm) {
  w <- WIDTH_MM / 25.4; h <- height_mm / 25.4
  f <- file.path(out, name)
  ggsave(paste0(f, ".pdf"), p, width = w, height = h, device = grDevices::cairo_pdf)
  ggsave(paste0(f, ".eps"), p, width = w, height = h, device = grDevices::cairo_ps,
         fallback_resolution = 1000)
  ## type = "cairo": the default quartz device on macOS ignores compression.
  ggsave(paste0(f, ".tiff"), p, width = w, height = h, dpi = 1000, compression = "lzw",
         type = "cairo", bg = "white")
  ggsave(paste0(f, ".png"), p, width = w, height = h, dpi = 300, bg = "white")
  invisible(f)
}

rd <- function(x) read.csv(file.path(run, "tables", x), stringsAsFactors = FALSE)
pooled <- rd("pooled_quantities.csv"); predict <- rd("prediction_intervals.csv")
## Only synthesised pathways. A release without the eligibility table predates
## the five-cohort rule (used here only to test the script on the old run).
el_path <- file.path(run, "tables", "synthesis_eligibility.csv")
eligible <- if (file.exists(el_path)) {
  el <- read.csv(el_path, stringsAsFactors = FALSE); el$pathway[el$eligible]
} else { message("No synthesis_eligibility.csv: test mode, dropping dissoc_cps"); setdiff(unique(pooled$pathway), "dissoc_cps") }
pooled <- pooled[pooled$pathway %in% eligible, ]; predict <- predict[predict$pathway %in% eligible, ]
order_paths <- c("dissoc_hal", "dissoc_del", "negsch_cps", "negsch_del", "depress_cps")
lab <- function(d) {
  k <- tapply(d$k_samples, d$pathway, max)
  f <- intersect(order_paths, unique(d$pathway))
  factor(sprintf("%s (k = %d)", PATH_LABELS[d$pathway], k[d$pathway]),
         levels = rev(sprintf("%s (k = %d)", PATH_LABELS[f], k[f])))
}

## Figure 2: population-average a, b, c', ab and total, unmoderated primary model.
p <- pooled[pooled$fit_class == "stan_primary" & pooled$moderator == "null" &
              pooled$quantity %in% c("a", "b", "cprime", "ind", "total"), ]
stopifnot(nrow(p) == 5 * length(unique(p$pathway)), !"dissoc_cps" %in% p$pathway)
p$pathway_label <- lab(p)
p$quantity <- factor(p$quantity, levels = c("a", "b", "cprime", "ind", "total"),
                     labels = c("a (CA → mediator)", "b (mediator → psychosis | CA)",
                                "c′ (CA → psychosis | mediator)", "ab (indirect)",
                                "Total (c′ + ab)"))
f2 <- ggplot(p, aes(x = median, y = pathway_label)) +
  geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.3) +
  geom_linerange(aes(xmin = q_lo, xmax = q_hi), linewidth = 0.5, colour = "#1B4965") +
  geom_point(size = 1.6, colour = "#1B4965") +
  facet_wrap(~ quantity, nrow = 1, scales = "free_x", labeller = label_wrap_gen(16)) +
  scale_x_continuous(breaks = x_breaks(3), expand = expansion(mult = 0.12)) +
  labs(x = "Standardised coefficient (posterior median, 95% equal-tailed credible interval)", y = NULL) +
  theme_journal()
save_journal(f2, "Figure2_pooled_quantities", 62)

## Figure S1: new-study prediction intervals for the latent true ab.
pr <- predict[predict$fit_class == "stan_primary" & predict$moderator == "null" &
                predict$quantity == "ind", ]
pr$pathway_label <- lab(pr)
pr$includes_zero <- pr$q_lo < 0 & pr$q_hi > 0
fs <- ggplot(pr, aes(x = median, y = pathway_label, colour = includes_zero)) +
  geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.3) +
  geom_linerange(aes(xmin = q_lo, xmax = q_hi), linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = c(`TRUE` = "#6B7178", `FALSE` = "#1B4965"),
                      labels = c(`TRUE` = "includes zero", `FALSE` = "excludes zero"),
                      name = NULL, drop = FALSE) +
  labs(x = "Latent true indirect effect ab in a new study (posterior median, 95% prediction interval)",
       y = NULL) +
  theme_journal()
save_journal(fs, "FigureS1_prediction_intervals", 55)
cat("written to", out, "\n")
