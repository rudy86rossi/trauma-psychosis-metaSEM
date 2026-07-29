#### packages ####
# install.packages("metaSEM")
# install.packages("OpenMx")
# install.packages("readxl")
# install.packages("flextable")
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("gridExtra")

library(ggplot2)
library(OpenMx)
library(metaSEM)
library(readxl)
library(semPlot)
library(flextable)
library(tidyverse)
library(gridExtra)
library(grid)

#### load data ####
setwd("/Users/rodolforossi/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/Research projects/metaSEM/negative_schemata")

dat <- read_excel("Negative schemas mediation_workcopy.xlsx", sheet = "data")

print(head(dat))

# impose columns as numeric
num_cols <- c("n", "mean_age", "%_female", "r_XM", "r_MY", "r_XY")
dat[num_cols] <- lapply(dat[num_cols], as.numeric)


# ---------------------------------------------------------------------------
# Helpers added by ricardotwumasi for the corrected meta-regressions (issue #2)
# and the per-cent-mediated confidence interval (issue #3).
#
# fit_metareg(): fits meta() with the correct within-study sampling covariances
#   from asyCov() and a diagonal Tau2 that matches the Stage-1 RE.type = "Diag"
#   specification, in place of the earlier zero off-diagonal covariances and the
#   default full (six-parameter) Tau2, which was degenerate at small k. Rows with
#   a missing moderator are dropped (and Rlist/n subset to match); rerun() is used
#   if the optimiser does not reach OpenMx status 0 or 1. Use center = FALSE for
#   the 0/1 sample-type indicator.
fit_metareg <- function(Rlist, n, mod_raw, center = TRUE) {
  mod_raw <- suppressWarnings(as.numeric(mod_raw))   # coerce (also covers NOS)
  keep <- !is.na(mod_raw)
  Rl <- Rlist[keep]; nn <- n[keep]; m <- mod_raw[keep]
  if (center) m <- as.numeric(scale(m, scale = FALSE))
  y  <- t(sapply(Rl, function(R) c(R[2, 1], R[3, 1], R[3, 2])))
  v  <- asyCov(Rl, nn)
  re <- Diag(paste0("0.1*Tau2_", 1:3, "_", 1:3))
  fit <- tryCatch(meta(y = y, v = v, x = matrix(m, ncol = 1), RE.constraints = re),
                  error = function(e) NULL)
  if (!is.null(fit)) {
    st <- tryCatch(fit$mx.fit@output$status$code, error = function(e) NA)
    if (!is.na(st) && !(st %in% c(0, 1)))
      fit <- tryCatch(suppressWarnings(rerun(fit, silent = TRUE)), error = function(e) fit)
  }
  fit
}

# metareg_grob(): render a meta-regression table (or a note if the model could
# not be fitted at all). Boundary fits at small k are still shown, matching the
# reported output; see issue #7 for the exploratory-reporting caveats.
metareg_grob <- function(fit, mod_label, outcome) {
  if (is.null(fit))
    textGrob("Meta-regression could not be fitted for this moderator.",
             gp = gpar(fontsize = 12))
  else
    tableGrob(meta_to_df(fit, mod_label, outcome), rows = NULL,
              theme = ttheme_minimal(base_size = 11))
}

# pct_ci(): format a proportion CI (0-1) as a percentage CI for the % mediated row.
pct_ci <- function(lb, ub) {
  f <- function(z) if (is.na(z)) "NA" else paste0(formatC(round(z * 100, 2), digits = 2, format = "f"), "%")
  paste0("[", f(lb), ", ", f(ub), "]")
}

# ── shared formatting helpers (used in PDF export) ──────────────────────────
fmt2 <- function(x) formatC(round(x, 2), digits = 2, format = "f")
ci   <- function(lb, ub) paste0("[", fmt2(lb), ", ", fmt2(ub), "]")

ptitle <- function(txt)
  textGrob(txt, gp = gpar(fontsize = 13, fontface = "bold"), just = "centre")

meta_to_df <- function(m, mod_label, outcome_name = "Outcome") {
  cf <- summary(m)$coefficients
  cf <- cf[!grepl("^Tau2", rownames(cf)), , drop = FALSE]
  param_map <- c(
    "Intercept1" = "Intercept r(Trauma, NegSchema)",
    "Intercept2" = paste0("Intercept r(Trauma, ",     outcome_name, ")"),
    "Intercept3" = paste0("Intercept r(NegSchema, ",  outcome_name, ")"),
    "Slope1_1"   = "r(Trauma, NegSchema)",
    "Slope2_1"   = paste0("r(Trauma, ",     outcome_name, ")"),
    "Slope3_1"   = paste0("r(NegSchema, ",  outcome_name, ")")
  )
  param_labels <- ifelse(rownames(cf) %in% names(param_map),
                         param_map[rownames(cf)], rownames(cf))
  data.frame(
    Parameter = param_labels,
    Estimate  = fmt2(cf[, "Estimate"]),
    `95% CI`  = paste0("[",
                       fmt2(cf[, "Estimate"] - 1.96 * cf[, "Std.Error"]),
                       ", ",
                       fmt2(cf[, "Estimate"] + 1.96 * cf[, "Std.Error"]),
                       "]"),
    SE        = fmt2(cf[, "Std.Error"]),
    `z value` = fmt2(cf[, "z value"]),
    `p value` = formatC(round(cf[, "Pr(>|z|)"], 3), digits = 3, format = "f"),
    check.names = FALSE,
    row.names   = NULL
  )
}

loo_num <- c("a", "b", "c", "ind", "ind_lbound", "ind_ubound")


# ============================================================================
# META-ANALYSIS 1: COMPOSITE PSYCHOSIS SCORE (CPS) ----
# Trauma → Negative Self-Schema → Composite Psychosis Score
# Inclusion: included_in_metaanalysis == 1, complete correlations
# ============================================================================

dat_cps <- subset(dat,
                  included_in_metaanalysis == 1 &
                  !is.na(r_XM) & !is.na(r_XY) & !is.na(r_MY) & !is.na(n))

cat("\n=== MA1: CPS — included studies ===\n")
print(dat_cps[, c("study_author", "year", "mediator_domain", "n")])
cat("k =", nrow(dat_cps), "| total N =", sum(dat_cps$n), "\n\n")

# Variable names in the correlation matrices
vars_cps <- c("Trauma", "NegSchema", "CPS")

# Build list of per-study 3x3 correlation matrices
Rlist_cps <- lapply(seq_len(nrow(dat_cps)), function(i) {
  r_xm <- dat_cps$r_XM[i]
  r_xy <- dat_cps$r_XY[i]
  r_my <- dat_cps$r_MY[i]
  R <- matrix(c(
    1,    r_xm, r_xy,
    r_xm, 1,    r_my,
    r_xy, r_my, 1
  ), nrow = 3, byrow = TRUE)
  dimnames(R) <- list(vars_cps, vars_cps)
  R
})

# Vector of sample sizes
n_cps <- dat_cps$n

# Stage 1: Random-effects meta-analysis of correlation matrices ----
stage1_cps <- tssem1(Rlist_cps, n_cps, method = "REM", RE.type = "Diag")
summary(stage1_cps)

## Average correlation matrix under random-effects model
rand_corr_cps <- vec2symMat(coef(stage1_cps, select = "fixed"), diag = FALSE)
dimnames(rand_corr_cps) <- list(vars_cps, vars_cps)
cat("\nPooled correlation matrix (CPS):\n")
print(round(rand_corr_cps, 2))
coef(stage1_cps, select = "random")

## Proposed model in lavaan syntax
model_cps <- "CPS ~ c*Trauma + b*NegSchema
NegSchema ~ a*Trauma
Trauma ~~ 1*Trauma"
plot(model_cps)

## Convert to RAM specification
RAM_cps <- lavaan2RAM(model_cps, obs.variables = vars_cps)
RAM_cps

#### Stage 2 ####
stage2_cps <- tssem2(stage1_cps,
                     RAM              = RAM_cps,
                     intervals.type   = "LB",
                     diag.constraints = TRUE,
                     mx.algebras      = list(
                       ind   = mxAlgebra(a * b,     name = "ind"),
                       dir   = mxAlgebra(c,         name = "dir"),
                       total = mxAlgebra(c + a * b, name = "total"),
                       pmed  = mxAlgebra(a * b / (c + a * b), name = "pmed")))
stage2_cps <- metaSEM::rerun(stage2_cps)
summary(stage2_cps)

#### SEM plot ####
plot(stage2_cps, color = "green")


# ── Per-study indirect effects: CPS ─────────────────────────────────────────
n_studies_cps    <- nrow(dat_cps)
study_contrib_cps <- vector("list", n_studies_cps)

for (i in seq_len(n_studies_cps)) {
  stage1_s <- tryCatch(
    tssem1(Rlist_cps[i], n_cps[i], method = "FEM"),
    error = function(e) NULL
  )
  stage2_s <- if (!is.null(stage1_s)) tryCatch(
    tssem2(stage1_s, RAM = RAM_cps, intervals.type = "LB",
           diag.constraints = TRUE,
           mx.algebras = list(ind   = mxAlgebra(a * b,     name = "ind"),
                              dir   = mxAlgebra(c,         name = "dir"),
                              total = mxAlgebra(c + a * b, name = "total"))),
    error = function(e) NULL
  ) else NULL
  if (!is.null(stage2_s)) stage2_s <- metaSEM::rerun(stage2_s)
  if (is.null(stage2_s)) {
    study_contrib_cps[[i]] <- data.frame(
      Study    = paste(trimws(dat_cps$study_author[i]), dat_cps$year[i]),
      N        = n_cps[i],
      Indirect = NA, CI_lower = NA, CI_upper = NA)
  } else {
    algs_s <- summary(stage2_s)$mx.algebras
    study_contrib_cps[[i]] <- data.frame(
      Study    = paste(trimws(dat_cps$study_author[i]), dat_cps$year[i]),
      N        = n_cps[i],
      Indirect = algs_s["ind", "Estimate"],
      CI_lower = algs_s["ind", "lbound"],
      CI_upper = algs_s["ind", "ubound"])
  }
}

algs_cps_pool  <- summary(stage2_cps)$mx.algebras
contrib_df_cps <- do.call(rbind, study_contrib_cps)
contrib_df_cps <- rbind(
  contrib_df_cps,
  data.frame(Study    = "Pooled (TSSEM)",
             N        = sum(n_cps),
             Indirect = algs_cps_pool["ind", "Estimate"],
             CI_lower = algs_cps_pool["ind", "lbound"],
             CI_upper = algs_cps_pool["ind", "ubound"])
)
print(contrib_df_cps)


# ── Leave-one-out sensitivity analysis: CPS ──────────────────────────────────
k_cps           <- nrow(dat_cps)
loo_results_cps <- vector("list", k_cps)

for (i in seq_len(k_cps)) {
  Rlist_loo <- Rlist_cps[-i]
  n_loo     <- n_cps[-i]

  stage1_loo <- tryCatch(
    tssem1(Rlist_loo, n_loo, method = "REM", RE.type = "Diag"),
    error = function(e) NULL
  )
  if (is.null(stage1_loo)) {
    loo_results_cps[[i]] <- data.frame(
      excluded_study = paste(trimws(dat_cps$study_author[i]), dat_cps$year[i]),
      excluded_N     = dat_cps$n[i],
      a = NA, b = NA, c = NA,
      ind = NA, ind_lbound = NA, ind_ubound = NA,
      converged = FALSE)
    next
  }

  stage2_loo <- tryCatch(
    tssem2(stage1_loo, RAM = RAM_cps, intervals.type = "LB",
           diag.constraints = TRUE,
           mx.algebras = list(
             ind   = mxAlgebra(a * b,     name = "ind"),
             dir   = mxAlgebra(c,         name = "dir"),
             total = mxAlgebra(c + a * b, name = "total"))),
    error = function(e) NULL
  )
  if (!is.null(stage2_loo)) stage2_loo <- metaSEM::rerun(stage2_loo)

  if (is.null(stage2_loo)) {
    loo_results_cps[[i]] <- data.frame(
      excluded_study = paste(trimws(dat_cps$study_author[i]), dat_cps$year[i]),
      excluded_N     = dat_cps$n[i],
      a = NA, b = NA, c = NA,
      ind = NA, ind_lbound = NA, ind_ubound = NA,
      converged = FALSE)
    next
  }

  s     <- summary(stage2_loo)
  coefs <- s$coefficients
  algs  <- s$mx.algebras
  loo_results_cps[[i]] <- data.frame(
    excluded_study = paste(trimws(dat_cps$study_author[i]), dat_cps$year[i]),
    excluded_N     = dat_cps$n[i],
    a              = coefs["a", "Estimate"],
    b              = coefs["b", "Estimate"],
    c              = coefs["c", "Estimate"],
    ind            = algs["ind",   "Estimate"],
    ind_lbound     = algs["ind",   "lbound"],
    ind_ubound     = algs["ind",   "ubound"],
    converged      = TRUE)
}

loo_df_cps <- do.call(rbind, loo_results_cps)
print(loo_df_cps)

# LOO plot
ind_full_cps <- summary(stage2_cps)$mx.algebras["ind", "Estimate"]
lb_full_cps  <- summary(stage2_cps)$mx.algebras["ind", "lbound"]
ub_full_cps  <- summary(stage2_cps)$mx.algebras["ind", "ubound"]

loo_df_cps$label <- paste0("Excl. ", loo_df_cps$excluded_study,
                            " (N=", loo_df_cps$excluded_N, ")")

p_loo_cps <- ggplot(loo_df_cps, aes(x = ind, y = reorder(label, ind))) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = ind_lbound, xmax = ind_ubound),
                 height = 0.25, color = "steelblue") +
  geom_vline(xintercept = ind_full_cps, linetype = "dashed",
             color = "black", linewidth = 0.7) +
  geom_vline(xintercept = 0, linetype = "solid",
             color = "red", linewidth = 0.5) +
  annotate("rect",
           xmin = lb_full_cps, xmax = ub_full_cps,
           ymin = -Inf, ymax = Inf,
           alpha = 0.1, fill = "black") +
  labs(
    title    = "Leave-One-Out Sensitivity Analysis",
    subtitle = "Indirect effect (a×b): Trauma → Neg. Self-Schema → Composite Psychosis Score",
    x        = "Indirect effect estimate", y = NULL,
    caption  = "Dashed line = full-sample estimate. Shaded band = full-sample 95% CI.\nRed line = zero."
  ) +
  theme_minimal(base_size = 12)
print(p_loo_cps)


# ── Meta-regression: CPS ────────────────────────────────────────────────────
# Corrected Stage-1 meta-regressions (issue #2). NOS is now available for every
# included study, so the NOS meta-regression is run; sample type is added too.
metareg_cps_age    <- fit_metareg(Rlist_cps, n_cps, dat_cps$mean_age)
metareg_cps_female <- fit_metareg(Rlist_cps, n_cps, dat_cps$`%_female`)
metareg_cps_nos    <- fit_metareg(Rlist_cps, n_cps, dat_cps$NOS)
metareg_cps_sample <- fit_metareg(Rlist_cps, n_cps,
                                  ifelse(dat_cps$clinical_sample == "GP", 0, 1),
                                  center = FALSE)


# ── PDF export: CPS ──────────────────────────────────────────────────────────
pdf_path_cps <- file.path(getwd(), "Composite Psychosis results_NegSchema.pdf")
pdf(pdf_path_cps, width = 11, height = 8.5)

# PAGE 1: TITLE
grid.newpage()
grid.text("Composite Psychosis Score – Meta-SEM Results",
          gp = gpar(fontsize = 22, fontface = "bold"), y = 0.60)
grid.text(paste("Generated:", format(Sys.Date(), "%d %B %Y")),
          gp = gpar(fontsize = 12), y = 0.48)
grid.text("Trauma → Negative Self-Schema → Composite Psychosis Score",
          gp = gpar(fontsize = 12, fontface = "italic"), y = 0.42)
grid.text(paste0("k = ", nrow(dat_cps), " studies | N = ", sum(n_cps)),
          gp = gpar(fontsize = 11), y = 0.35)

# PAGE 2: POOLED CORRELATION MATRIX
corr_disp <- round(rand_corr_cps, 2)
corr_disp[upper.tri(corr_disp)] <- NA
diag(corr_disp)                 <- NA
corr_str <- matrix(
  ifelse(is.na(corr_disp), "—", formatC(corr_disp, digits = 2, format = "f")),
  nrow = nrow(corr_disp), dimnames = dimnames(corr_disp))
corr_tbl <- cbind(Variable = rownames(corr_str), as.data.frame(corr_str))
grid.arrange(
  ptitle("Section 1 — Pooled Correlation Matrix (Random-Effects Model)"),
  tableGrob(corr_tbl, rows = NULL, theme = ttheme_minimal(base_size = 13)),
  ncol = 1, heights = c(0.08, 0.92)
)

# PAGE 3: PATH COEFFICIENTS & MEDIATION EFFECTS
s2_cps        <- summary(stage2_cps)
coefs_cps2    <- s2_cps$coefficients
algs_cps2     <- s2_cps$mx.algebras
total_est_cps <- algs_cps2["total", "Estimate"]
ind_est_cps   <- algs_cps2["ind",   "Estimate"]
pct_med_cps   <- if (!is.na(total_est_cps) && total_est_cps != 0)
  paste0(fmt2(ind_est_cps / total_est_cps * 100), "%") else "—"

path_df_cps <- data.frame(
  Effect   = c("a  (Trauma → Neg. Self-Schema)",
               "b  (Neg. Self-Schema → Composite Psychosis Score)",
               "c  (Trauma → Composite Psychosis Score, direct)",
               "Indirect effect (a × b)",
               "Total effect (c + a × b)",
               "% of total effect mediated"),
  Estimate = c(fmt2(coefs_cps2["a", "Estimate"]),
               fmt2(coefs_cps2["b", "Estimate"]),
               fmt2(coefs_cps2["c", "Estimate"]),
               fmt2(ind_est_cps),
               fmt2(total_est_cps),
               pct_med_cps),
  `95% CI` = c(ci(coefs_cps2["a", "lbound"],    coefs_cps2["a", "ubound"]),
               ci(coefs_cps2["b", "lbound"],    coefs_cps2["b", "ubound"]),
               ci(coefs_cps2["c", "lbound"],    coefs_cps2["c", "ubound"]),
               ci(algs_cps2["ind",   "lbound"], algs_cps2["ind",   "ubound"]),
               ci(algs_cps2["total", "lbound"], algs_cps2["total", "ubound"]),
               "—"),
  check.names = FALSE
)
# Issue #3: CI for the % of total effect mediated, from the pmed algebra (LB interval)
path_df_cps[nrow(path_df_cps), "95% CI"] <- pct_ci(algs_cps2["pmed", "lbound"], algs_cps2["pmed", "ubound"])

grid.arrange(
  ptitle("Section 1 — Path Coefficients and Mediation Effects"),
  tableGrob(path_df_cps, rows = NULL, theme = ttheme_minimal(base_size = 12)),
  ncol = 1, heights = c(0.08, 0.92)
)

# PAGE 4: SEM PATH DIAGRAM
plot(stage2_cps, color = "green",
     main = "Section 1 — SEM Path Diagram")

# PAGE 5: PER-STUDY INDIRECT EFFECTS
contrib_disp_cps <- contrib_df_cps
contrib_disp_cps$`95% CI` <- paste0("[", fmt2(contrib_disp_cps$CI_lower),
                                     ", ", fmt2(contrib_disp_cps$CI_upper), "]")
contrib_disp_cps$Indirect  <- fmt2(contrib_disp_cps$Indirect)
contrib_disp_cps <- contrib_disp_cps[, c("Study", "N", "Indirect", "95% CI")]
grid.arrange(
  ptitle("Section 1 — Per-Study Indirect Effects: Trauma → Neg. Self-Schema → CPS"),
  tableGrob(contrib_disp_cps, rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.07, 0.93)
)

# PAGE 6: LOO TABLE
loo_disp_cps <- loo_df_cps
loo_disp_cps[, loo_num] <- lapply(loo_disp_cps[, loo_num], function(x) fmt2(x))
loo_disp_cps <- loo_disp_cps[, c("excluded_study","excluded_N",
                                  "a","b","c","ind","ind_lbound","ind_ubound","converged")]
colnames(loo_disp_cps) <- c("Study","N","a","b","c","Indirect","CI lower","CI upper","Converged")
grid.arrange(
  ptitle("Section 2 — Leave-One-Out Sensitivity Analysis: Results Table"),
  tableGrob(loo_disp_cps, rows = NULL, theme = ttheme_minimal(base_size = 9)),
  ncol = 1, heights = c(0.06, 0.94)
)

# PAGE 7: LOO PLOT
print(p_loo_cps)

# PAGE 8: META-REGRESSION — MEAN AGE
grid.arrange(
  ptitle("Section 3 — Meta-Regression: Mean Age as Moderator"),
  tableGrob(meta_to_df(metareg_cps_age, "Mean Age (grand-mean centred)", "CPS"),
            rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.08, 0.92)
)

# PAGE 9: META-REGRESSION — % FEMALE
grid.arrange(
  ptitle("Section 3 — Meta-Regression: % Female as Moderator"),
  tableGrob(meta_to_df(metareg_cps_female, "% Female (grand-mean centred)", "CPS"),
            rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.08, 0.92)
)

# PAGE 10: META-REGRESSION — NOS (STUDY QUALITY); now available for all included studies
grid.arrange(
  ptitle("Section 3 — Meta-Regression: Study Quality (NOS) as Moderator"),
  metareg_grob(metareg_cps_nos, "NOS (grand-mean centred)", "CPS"),
  ncol = 1, heights = c(0.08, 0.92)
)

# PAGE 11: META-REGRESSION — SAMPLE TYPE (clinical vs non-clinical; GP = reference)
grid.arrange(
  ptitle("Section 3 — Meta-Regression: Sample Type (Clinical vs Non-clinical; GP = reference)"),
  metareg_grob(metareg_cps_sample, "Sample type (0 = non-clinical/GP, 1 = clinical)", "CPS"),
  ncol = 1, heights = c(0.08, 0.92)
)

dev.off()
message("PDF saved: ", pdf_path_cps)


# ============================================================================
# META-ANALYSIS 2: DELUSIONS ----
# Trauma → Negative Self-Schema → Delusions
# Inclusion: included_in_metaanalysis == 2, complete correlations
# ============================================================================

dat_del <- subset(dat,
                  included_in_metaanalysis == 2 &
                  !is.na(r_XM) & !is.na(r_XY) & !is.na(r_MY) & !is.na(n))

cat("\n=== MA2: Delusions — included studies ===\n")
print(dat_del[, c("study_author", "year", "mediator_domain", "n")])
cat("k =", nrow(dat_del), "| total N =", sum(dat_del$n), "\n\n")

# Variable names in the correlation matrices
vars_del <- c("Trauma", "NegSchema", "Delusions")

# Build list of per-study 3x3 correlation matrices
Rlist_del <- lapply(seq_len(nrow(dat_del)), function(i) {
  r_xm <- dat_del$r_XM[i]
  r_xy <- dat_del$r_XY[i]
  r_my <- dat_del$r_MY[i]
  R <- matrix(c(
    1,    r_xm, r_xy,
    r_xm, 1,    r_my,
    r_xy, r_my, 1
  ), nrow = 3, byrow = TRUE)
  dimnames(R) <- list(vars_del, vars_del)
  R
})

# Vector of sample sizes
n_del <- dat_del$n

# Stage 1: Random-effects meta-analysis of correlation matrices ----
stage1_del <- tssem1(Rlist_del, n_del, method = "REM", RE.type = "Diag")
summary(stage1_del)

## Average correlation matrix under random-effects model
rand_corr_del <- vec2symMat(coef(stage1_del, select = "fixed"), diag = FALSE)
dimnames(rand_corr_del) <- list(vars_del, vars_del)
cat("\nPooled correlation matrix (Delusions):\n")
print(round(rand_corr_del, 2))
coef(stage1_del, select = "random")

## Proposed model in lavaan syntax
model_del <- "Delusions ~ c*Trauma + b*NegSchema
NegSchema ~ a*Trauma
Trauma ~~ 1*Trauma"
plot(model_del)

## Convert to RAM specification
RAM_del <- lavaan2RAM(model_del, obs.variables = vars_del)
RAM_del

#### Stage 2 ####
stage2_del <- tssem2(stage1_del,
                     RAM              = RAM_del,
                     intervals.type   = "LB",
                     diag.constraints = TRUE,
                     mx.algebras      = list(
                       ind   = mxAlgebra(a * b,     name = "ind"),
                       dir   = mxAlgebra(c,         name = "dir"),
                       total = mxAlgebra(c + a * b, name = "total"),
                       pmed  = mxAlgebra(a * b / (c + a * b), name = "pmed")))
stage2_del <- metaSEM::rerun(stage2_del)
summary(stage2_del)

#### SEM plot ####
plot(stage2_del, color = "green")


# ── Per-study indirect effects: Delusions ───────────────────────────────────
n_studies_del    <- nrow(dat_del)
study_contrib_del <- vector("list", n_studies_del)

for (i in seq_len(n_studies_del)) {
  stage1_s <- tryCatch(
    tssem1(Rlist_del[i], n_del[i], method = "FEM"),
    error = function(e) NULL
  )
  stage2_s <- if (!is.null(stage1_s)) tryCatch(
    tssem2(stage1_s, RAM = RAM_del, intervals.type = "LB",
           diag.constraints = TRUE,
           mx.algebras = list(ind   = mxAlgebra(a * b,     name = "ind"),
                              dir   = mxAlgebra(c,         name = "dir"),
                              total = mxAlgebra(c + a * b, name = "total"))),
    error = function(e) NULL
  ) else NULL
  if (!is.null(stage2_s)) stage2_s <- metaSEM::rerun(stage2_s)
  if (is.null(stage2_s)) {
    study_contrib_del[[i]] <- data.frame(
      Study    = paste(trimws(dat_del$study_author[i]), dat_del$year[i]),
      N        = n_del[i],
      Indirect = NA, CI_lower = NA, CI_upper = NA)
  } else {
    algs_s <- summary(stage2_s)$mx.algebras
    study_contrib_del[[i]] <- data.frame(
      Study    = paste(trimws(dat_del$study_author[i]), dat_del$year[i]),
      N        = n_del[i],
      Indirect = algs_s["ind", "Estimate"],
      CI_lower = algs_s["ind", "lbound"],
      CI_upper = algs_s["ind", "ubound"])
  }
}

algs_del_pool  <- summary(stage2_del)$mx.algebras
contrib_df_del <- do.call(rbind, study_contrib_del)
contrib_df_del <- rbind(
  contrib_df_del,
  data.frame(Study    = "Pooled (TSSEM)",
             N        = sum(n_del),
             Indirect = algs_del_pool["ind", "Estimate"],
             CI_lower = algs_del_pool["ind", "lbound"],
             CI_upper = algs_del_pool["ind", "ubound"])
)
print(contrib_df_del)


# ── Leave-one-out sensitivity analysis: Delusions ────────────────────────────
k_del           <- nrow(dat_del)
loo_results_del <- vector("list", k_del)

for (i in seq_len(k_del)) {
  Rlist_loo <- Rlist_del[-i]
  n_loo     <- n_del[-i]

  stage1_loo <- tryCatch(
    tssem1(Rlist_loo, n_loo, method = "REM", RE.type = "Diag"),
    error = function(e) NULL
  )
  if (is.null(stage1_loo)) {
    loo_results_del[[i]] <- data.frame(
      excluded_study = paste(trimws(dat_del$study_author[i]), dat_del$year[i]),
      excluded_N     = dat_del$n[i],
      a = NA, b = NA, c = NA,
      ind = NA, ind_lbound = NA, ind_ubound = NA,
      converged = FALSE)
    next
  }

  stage2_loo <- tryCatch(
    tssem2(stage1_loo, RAM = RAM_del, intervals.type = "LB",
           diag.constraints = TRUE,
           mx.algebras = list(
             ind   = mxAlgebra(a * b,     name = "ind"),
             dir   = mxAlgebra(c,         name = "dir"),
             total = mxAlgebra(c + a * b, name = "total"))),
    error = function(e) NULL
  )
  if (!is.null(stage2_loo)) stage2_loo <- metaSEM::rerun(stage2_loo)

  if (is.null(stage2_loo)) {
    loo_results_del[[i]] <- data.frame(
      excluded_study = paste(trimws(dat_del$study_author[i]), dat_del$year[i]),
      excluded_N     = dat_del$n[i],
      a = NA, b = NA, c = NA,
      ind = NA, ind_lbound = NA, ind_ubound = NA,
      converged = FALSE)
    next
  }

  s     <- summary(stage2_loo)
  coefs <- s$coefficients
  algs  <- s$mx.algebras
  loo_results_del[[i]] <- data.frame(
    excluded_study = paste(trimws(dat_del$study_author[i]), dat_del$year[i]),
    excluded_N     = dat_del$n[i],
    a              = coefs["a", "Estimate"],
    b              = coefs["b", "Estimate"],
    c              = coefs["c", "Estimate"],
    ind            = algs["ind",   "Estimate"],
    ind_lbound     = algs["ind",   "lbound"],
    ind_ubound     = algs["ind",   "ubound"],
    converged      = TRUE)
}

loo_df_del <- do.call(rbind, loo_results_del)
print(loo_df_del)

# LOO plot
ind_full_del <- summary(stage2_del)$mx.algebras["ind", "Estimate"]
lb_full_del  <- summary(stage2_del)$mx.algebras["ind", "lbound"]
ub_full_del  <- summary(stage2_del)$mx.algebras["ind", "ubound"]

loo_df_del$label <- paste0("Excl. ", loo_df_del$excluded_study,
                            " (N=", loo_df_del$excluded_N, ")")

p_loo_del <- ggplot(loo_df_del, aes(x = ind, y = reorder(label, ind))) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = ind_lbound, xmax = ind_ubound),
                 height = 0.25, color = "steelblue") +
  geom_vline(xintercept = ind_full_del, linetype = "dashed",
             color = "black", linewidth = 0.7) +
  geom_vline(xintercept = 0, linetype = "solid",
             color = "red", linewidth = 0.5) +
  annotate("rect",
           xmin = lb_full_del, xmax = ub_full_del,
           ymin = -Inf, ymax = Inf,
           alpha = 0.1, fill = "black") +
  labs(
    title    = "Leave-One-Out Sensitivity Analysis",
    subtitle = "Indirect effect (a×b): Trauma → Neg. Self-Schema → Delusions",
    x        = "Indirect effect estimate", y = NULL,
    caption  = "Dashed line = full-sample estimate. Shaded band = full-sample 95% CI.\nRed line = zero."
  ) +
  theme_minimal(base_size = 12)
print(p_loo_del)


# ── Meta-regression: Delusions ───────────────────────────────────────────────
# Corrected Stage-1 meta-regressions (issue #2). NOS is now available for every
# included study, so the NOS meta-regression is run; sample type is added too.
metareg_del_age    <- fit_metareg(Rlist_del, n_del, dat_del$mean_age)
metareg_del_female <- fit_metareg(Rlist_del, n_del, dat_del$`%_female`)
metareg_del_nos    <- fit_metareg(Rlist_del, n_del, dat_del$NOS)
metareg_del_sample <- fit_metareg(Rlist_del, n_del,
                                  ifelse(dat_del$clinical_sample == "GP", 0, 1),
                                  center = FALSE)


# ── PDF export: Delusions ─────────────────────────────────────────────────────
pdf_path_del <- file.path(getwd(), "Delusions results_NegSchema.pdf")
pdf(pdf_path_del, width = 11, height = 8.5)

# PAGE 1: TITLE
grid.newpage()
grid.text("Delusions – Meta-SEM Results",
          gp = gpar(fontsize = 22, fontface = "bold"), y = 0.60)
grid.text(paste("Generated:", format(Sys.Date(), "%d %B %Y")),
          gp = gpar(fontsize = 12), y = 0.48)
grid.text("Trauma → Negative Self-Schema → Delusions mediation model",
          gp = gpar(fontsize = 12, fontface = "italic"), y = 0.42)
grid.text(paste0("k = ", nrow(dat_del), " studies | N = ", sum(n_del)),
          gp = gpar(fontsize = 11), y = 0.35)

# PAGE 2: POOLED CORRELATION MATRIX
corr_disp <- round(rand_corr_del, 2)
corr_disp[upper.tri(corr_disp)] <- NA
diag(corr_disp)                 <- NA
corr_str <- matrix(
  ifelse(is.na(corr_disp), "—", formatC(corr_disp, digits = 2, format = "f")),
  nrow = nrow(corr_disp), dimnames = dimnames(corr_disp))
corr_tbl <- cbind(Variable = rownames(corr_str), as.data.frame(corr_str))
grid.arrange(
  ptitle("Section 1 — Pooled Correlation Matrix (Random-Effects Model)"),
  tableGrob(corr_tbl, rows = NULL, theme = ttheme_minimal(base_size = 13)),
  ncol = 1, heights = c(0.08, 0.92)
)

# PAGE 3: PATH COEFFICIENTS & MEDIATION EFFECTS
s2_del        <- summary(stage2_del)
coefs_del2    <- s2_del$coefficients
algs_del2     <- s2_del$mx.algebras
total_est_del <- algs_del2["total", "Estimate"]
ind_est_del   <- algs_del2["ind",   "Estimate"]
pct_med_del   <- if (!is.na(total_est_del) && total_est_del != 0)
  paste0(fmt2(ind_est_del / total_est_del * 100), "%") else "—"

path_df_del <- data.frame(
  Effect   = c("a  (Trauma → Neg. Self-Schema)",
               "b  (Neg. Self-Schema → Delusions)",
               "c  (Trauma → Delusions, direct)",
               "Indirect effect (a × b)",
               "Total effect (c + a × b)",
               "% of total effect mediated"),
  Estimate = c(fmt2(coefs_del2["a", "Estimate"]),
               fmt2(coefs_del2["b", "Estimate"]),
               fmt2(coefs_del2["c", "Estimate"]),
               fmt2(ind_est_del),
               fmt2(total_est_del),
               pct_med_del),
  `95% CI` = c(ci(coefs_del2["a", "lbound"],    coefs_del2["a", "ubound"]),
               ci(coefs_del2["b", "lbound"],    coefs_del2["b", "ubound"]),
               ci(coefs_del2["c", "lbound"],    coefs_del2["c", "ubound"]),
               ci(algs_del2["ind",   "lbound"], algs_del2["ind",   "ubound"]),
               ci(algs_del2["total", "lbound"], algs_del2["total", "ubound"]),
               "—"),
  check.names = FALSE
)
# Issue #3: CI for the % of total effect mediated, from the pmed algebra (LB interval)
path_df_del[nrow(path_df_del), "95% CI"] <- pct_ci(algs_del2["pmed", "lbound"], algs_del2["pmed", "ubound"])

grid.arrange(
  ptitle("Section 1 — Path Coefficients and Mediation Effects"),
  tableGrob(path_df_del, rows = NULL, theme = ttheme_minimal(base_size = 12)),
  ncol = 1, heights = c(0.08, 0.92)
)

# PAGE 4: SEM PATH DIAGRAM
plot(stage2_del, color = "green",
     main = "Section 1 — SEM Path Diagram")

# PAGE 5: PER-STUDY INDIRECT EFFECTS
contrib_disp_del <- contrib_df_del
contrib_disp_del$`95% CI` <- paste0("[", fmt2(contrib_disp_del$CI_lower),
                                     ", ", fmt2(contrib_disp_del$CI_upper), "]")
contrib_disp_del$Indirect  <- fmt2(contrib_disp_del$Indirect)
contrib_disp_del <- contrib_disp_del[, c("Study", "N", "Indirect", "95% CI")]
grid.arrange(
  ptitle("Section 1 — Per-Study Indirect Effects: Trauma → Neg. Self-Schema → Delusions"),
  tableGrob(contrib_disp_del, rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.07, 0.93)
)

# PAGE 6: LOO TABLE
loo_disp_del <- loo_df_del
loo_disp_del[, loo_num] <- lapply(loo_disp_del[, loo_num], function(x) fmt2(x))
loo_disp_del <- loo_disp_del[, c("excluded_study","excluded_N",
                                  "a","b","c","ind","ind_lbound","ind_ubound","converged")]
colnames(loo_disp_del) <- c("Study","N","a","b","c","Indirect","CI lower","CI upper","Converged")
grid.arrange(
  ptitle("Section 2 — Leave-One-Out Sensitivity Analysis: Results Table"),
  tableGrob(loo_disp_del, rows = NULL, theme = ttheme_minimal(base_size = 9)),
  ncol = 1, heights = c(0.06, 0.94)
)

# PAGE 7: LOO PLOT
print(p_loo_del)

# PAGE 8: META-REGRESSION — MEAN AGE
grid.arrange(
  ptitle("Section 3 — Meta-Regression: Mean Age as Moderator"),
  tableGrob(meta_to_df(metareg_del_age, "Mean Age (grand-mean centred)", "Delusions"),
            rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.08, 0.92)
)

# PAGE 9: META-REGRESSION — % FEMALE
grid.arrange(
  ptitle("Section 3 — Meta-Regression: % Female as Moderator"),
  tableGrob(meta_to_df(metareg_del_female, "% Female (grand-mean centred)", "Delusions"),
            rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.08, 0.92)
)

# PAGE 10: META-REGRESSION — NOS (STUDY QUALITY); now available for all included studies
grid.arrange(
  ptitle("Section 3 — Meta-Regression: Study Quality (NOS) as Moderator"),
  metareg_grob(metareg_del_nos, "NOS (grand-mean centred)", "Delusions"),
  ncol = 1, heights = c(0.08, 0.92)
)

# PAGE 11: META-REGRESSION — SAMPLE TYPE (clinical vs non-clinical; GP = reference)
# Note: at k = 5 this is a boundary fit (Tau2 at 0), so report as exploratory (issue #7).
grid.arrange(
  ptitle("Section 3 — Meta-Regression: Sample Type (Clinical vs Non-clinical; GP = reference)"),
  metareg_grob(metareg_del_sample, "Sample type (0 = non-clinical/GP, 1 = clinical)", "Delusions"),
  ncol = 1, heights = c(0.08, 0.92)
)

dev.off()
message("PDF saved: ", pdf_path_del)
