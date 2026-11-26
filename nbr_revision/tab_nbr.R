# Extract the NBR revision's manuscript numbers from a targets store.
#   Rscript tab_nbr.R STORE CONFIG OUT_DIR TAG
# Writes, for the unmoderated Stan models of the synthesised pathways:
#   primary_<TAG>.csv      Table 2: ab (CrI, HDI, P > 0), new-study PI, total, percentage
#   prior_grid_<TAG>.csv   the same under each prior variant
#   sensitivity_<TAG>.csv  case-control (Appiah) deletion and direct-correlation refits
# The percentage carries its release status under the post hoc rule in
# config/analysis.yml (proportion_mediated): a refit's ratio is reported only if
# P(total > delta) and P(0 < pct < 100) both meet their thresholds and the fit
# passed its sampler diagnostics. Withheld ratios are labelled, never printed
# as estimates.

args <- commandArgs(trailingOnly = TRUE); stopifnot(length(args) == 4)
store <- args[1]; cfg <- yaml::read_yaml(args[2]); out <- args[3]; tag <- args[4]
suppressPackageStartupMessages(library(targets))
R <- tar_read(results, store = store)
pm <- cfg$proportion_mediated
dg <- R$diagnostics
diag_ok <- function(id) {
  d <- dg[dg$fit_id == id, ]
  nrow(d) == 1 && d$max_rhat <= cfg$diagnostics$max_rhat && d$min_ess_bulk >= cfg$diagnostics$min_ess_bulk &&
    d$min_ess_tail >= cfg$diagnostics$min_ess_tail && d$num_divergent == 0
}

one <- function(id) {
  g <- function(tab, q) { x <- tab[tab$fit_id == id & tab$quantity == q, ]; stopifnot(nrow(x) == 1); x }
  ab <- g(R$pooled, "ind"); pi <- g(R$predict, "ind"); tot <- g(R$pooled, "total")
  pc <- R$pmed[R$pmed$fit_id == id & R$pmed$quantity == "pmed_pct", ]
  stopifnot(nrow(pc) == 1)
  sep <- pc[[sprintf("p_total_gt_%g", pm$delta_primary)]]
  ok <- sep >= pm$min_p_total_gt_delta && pc$p_in_range >= pm$min_p_in_unit_interval && diag_ok(id)
  why <- c(if (sep < pm$min_p_total_gt_delta) sprintf("P(total > %g) = %.3f", pm$delta_primary, sep),
           if (pc$p_in_range < pm$min_p_in_unit_interval) sprintf("P(0 < pct < 100) = %.3f", pc$p_in_range),
           if (!diag_ok(id)) "sampler diagnostics")
  data.frame(fit_id = id, fit_class = ab$fit_class, pathway = ab$pathway, label = ab$label,
             variant = ab$variant, drop_sample = ab$drop_sample, k = ab$k_samples,
             ab = ab$median, ab_crI_lo = ab$q_lo, ab_crI_hi = ab$q_hi,
             ab_hdi_lo = ab$hdi_lower, ab_hdi_hi = ab$hdi_upper, p_ab_gt0 = ab$p_gt0,
             ab_pi_lo = pi$q_lo, ab_pi_hi = pi$q_hi,
             total = tot$median, total_crI_lo = tot$q_lo, total_crI_hi = tot$q_hi,
             total_hdi_lo = tot$hdi_lower, total_hdi_hi = tot$hdi_upper, p_total_gt0 = tot$p_gt0,
             pct = pc$median, pct_crI_lo = pc$q_lo, pct_crI_hi = pc$q_hi,
             p_total_gt_delta = sep, p_pct_in_range = pc$p_in_range,
             pct_status = if (ok) "released" else "withheld",
             pct_withheld_reason = if (ok) "" else paste(why, collapse = "; "),
             max_rhat = dg$max_rhat[dg$fit_id == id], divergent = dg$num_divergent[dg$fit_id == id],
             stringsAsFactors = FALSE)
}

synth <- if (!is.null(tryCatch(tar_read(lock, store = store)$synthesis, error = function(e) NULL)))
  tar_read(lock, store = store)$synthesis else unique(R$pooled$pathway[R$pooled$pathway != "dissoc_cps"])
u <- unique(R$pooled[R$pooled$moderator == "null" & R$pooled$pathway %in% synth, c("fit_id", "fit_class")])

prim <- do.call(rbind, lapply(u$fit_id[u$fit_class == "stan_primary"], one))
grid <- do.call(rbind, lapply(u$fit_id[u$fit_class %in% c("stan_primary", "sens_prior_grid")], one))
grid$prior <- ifelse(grid$fit_class == "stan_primary", "primary", grid$variant)
ref <- prim[match(grid$pathway, prim$pathway), ]
grid$ab_change_pct <- 100 * (grid$ab / ref$ab - 1)
grid$pct_shift_points <- grid$pct - ref$pct
grid$crI_includes_zero <- grid$ab_crI_lo <= 0
grid$pi_includes_zero <- grid$ab_pi_lo <= 0 & grid$ab_pi_hi >= 0
ord <- c("primary", "alpha_sd=0.2", "alpha_sd=0.5", "tau_sd=0.1", "tau_sd=0.3")
grid <- grid[order(grid$pathway, match(grid$prior, ord)), ]

sid <- u$fit_id[u$fit_class == "sens_direct_r" |
                (u$fit_class == "sens_loso_stan" & grepl("|negsch_del|null|S025", u$fit_id, fixed = TRUE))]
sens <- if (length(sid)) do.call(rbind, lapply(sid, one)) else NULL

dir.create(out, showWarnings = FALSE, recursive = TRUE)
write.csv(prim, file.path(out, sprintf("primary_%s.csv", tag)), row.names = FALSE)
write.csv(grid, file.path(out, sprintf("prior_grid_%s.csv", tag)), row.names = FALSE)
if (!is.null(sens)) write.csv(sens, file.path(out, sprintf("sensitivity_%s.csv", tag)), row.names = FALSE)

r3 <- function(d) { n <- sapply(d, is.numeric); d[n] <- lapply(d[n], signif, 4); d }
print(r3(prim[, c("pathway", "k", "ab", "ab_crI_lo", "ab_crI_hi", "ab_hdi_lo", "ab_hdi_hi", "p_ab_gt0",
                  "ab_pi_lo", "ab_pi_hi", "total", "total_crI_lo", "total_crI_hi", "pct", "pct_status")]),
      row.names = FALSE)
alt <- grid[grid$prior != "primary", ]
cat(sprintf("\nPrior grid: %d refits; ab > 0 posterior probability min %.7f; ab change %.3f%% to %.3f%%;",
            nrow(alt), min(alt$p_ab_gt0), min(alt$ab_change_pct), max(alt$ab_change_pct)),
    sprintf("CrI includes zero in %d; PI excludes zero in %d; max |pct shift| %.2f points; withheld %d\n",
            sum(alt$crI_includes_zero), sum(!alt$pi_includes_zero),
            max(abs(alt$pct_shift_points)), sum(alt$pct_status == "withheld")))
if (!is.null(sens)) print(r3(sens[, c("fit_id", "k", "ab", "ab_crI_lo", "ab_crI_hi", "p_ab_gt0", "ab_pi_lo",
                                       "ab_pi_hi", "pct", "pct_status")]), row.names = FALSE)
