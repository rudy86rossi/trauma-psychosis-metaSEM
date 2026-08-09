####packages####
# install.packages("metaSEM")
# install.packages("OpenMx")
# install.packages("readxl")
# install.packages("flextable")
# install.packages("ggplot2")
# install.packages("tidyverse")

library(ggplot2)
library(OpenMx)
library(metaSEM)
library(readxl)
library(semPlot)
library(flextable)
library(tidyverse)

####load data####
setwd("/Users/rodolforossi/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/Research projects/metaSEM/depression")
dat <- read_excel("depression_workcopy.xlsx")

print(head(dat))

# impose columns as numeric
num_cols <- c("n", "mean age", "%_female", "r_XM", "r_MY", "r_XY")
dat[num_cols] <- lapply(dat[num_cols], as.numeric)


# ---------------------------------------------------------------------------
# Helpers added by ricardotwumasi for the corrected meta-regressions (issue #2)
# and the per-cent-mediated confidence interval (issue #3).
#
# fit_metareg(): fits meta() with the correct within-study sampling covariances
#   from asyCov() and a diagonal Tau2 that matches a RE.type = "Diag" Stage-1
#   specification, in place of the earlier zero off-diagonal covariances and the
#   default full (six-parameter) Tau2, which was degenerate at small k. Rows with
#   a missing moderator are dropped (and Rlist/n subset to match); rerun() is used
#   if the optimiser does not reach OpenMx status 0 or 1. Use center = FALSE for
#   the 0/1 sample-type indicator. NB the depression Stage-1 pooled matrix is
#   fixed-effects (K = 5), but the moderator meta-regression here uses the
#   diagonal random-effects specification; this is a deliberate, documented choice.
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


# COMPOSITE PSYCHOTIC SYMPTOMS (CPS) ----

# create subset: composite psychosis & composite depression & any trauma
dat_cps_comp <- subset(dat, psychosis_construct == "composite" &
                             `depression domain` == "composite" &
                             !is.na(r_XM) & !is.na(r_XY) & !is.na(r_MY) & !is.na(n))

# Variable names in the correlation matrices
vars_cps <- c("Trauma", "Depression", "CPS")

# Build list of per-study 3x3 correlation matrices
Rlist_cps <- lapply(seq_len(nrow(dat_cps_comp)), function(i) {
  r_xm <- dat_cps_comp$r_XM[i]
  r_xy <- dat_cps_comp$r_XY[i]
  r_my <- dat_cps_comp$r_MY[i]

  R <- matrix(c(
    1,    r_xm, r_xy,
    r_xm, 1,    r_my,
    r_xy, r_my, 1
  ), nrow = 3, byrow = TRUE)

  dimnames(R) <- list(vars_cps, vars_cps)
  R
})

# Vector of sample sizes
n_cps_comp <- dat_cps_comp$n


# Stage 1: meta-analysis of correlation matrices ----
# NOTE: depression uses a FIXED-effects model (method = "FEM"), unlike the
# dissociation and negative-schema scripts (which use random effects). With only
# K = 5 studies the random-effects model is not estimable here: REM Stage 1 hits
# the heterogeneity boundary (two of three Tau2 estimated at 0, OpenMx status 6)
# and REM Stage 2 fails with a non-positive-definite aCov under every interval and
# constraint setting. This deviation should also be stated in the Methods/supplement
# so that manuscript, code and figures agree.
stage1_cps_comp <- tssem1(Rlist_cps, n_cps_comp, method = "FEM")
summary(stage1_cps_comp)

## Pooled correlation matrix under the fixed-effects model, taken directly from the
## tssem1() fit. This replaces the earlier 'rand_corr', which was a hand-computed,
## sample-size-weighted average that was misnamed as random effects and duplicated
## what the fit already provides.
fe_corr <- coef(stage1_cps_comp)
round(fe_corr, 2)


#### STAGE 2 ####

## Proposed model in lavaan syntax
model1 <- "CPS ~ c*Trauma + b*Depression
Depression ~ a*Trauma
Trauma ~~ 1*Trauma"
plot(model1)

## Convert the lavaan syntax to RAM specification used in metaSEM
RAM1 <- lavaan2RAM(model1, obs.variables = vars_cps)
RAM1

#### fit model stage 2 ####
stage2_cps_comp <- tssem2(stage1_cps_comp,
                          RAM              = RAM1,
                          intervals.type   = "LB",   # if aCov error persists, change to "z"
                          diag.constraints = TRUE,
                          mx.algebras      = list(
                            ind   = mxAlgebra(a * b,     name = "ind"),
                            dir   = mxAlgebra(c,         name = "dir"),
                            total = mxAlgebra(c + a * b, name = "total"),
                            pmed  = mxAlgebra(a * b / (c + a * b), name = "pmed")
                          ))
stage2_cps_comp <- metaSEM::rerun(stage2_cps_comp)
summary(stage2_cps_comp)

#### SEM plot ####
plot(stage2_cps_comp, color = "green")


# per-study indirect effect table ----
n_studies_cps <- nrow(dat_cps_comp)
study_contrib_cps <- vector("list", n_studies_cps)

for (i in seq_len(n_studies_cps)) {
  stage1_s <- tryCatch(
    tssem1(Rlist_cps[i], n_cps_comp[i], method = "FEM"),
    error = function(e) NULL
  )
  stage2_s <- if (!is.null(stage1_s)) tryCatch(
    tssem2(stage1_s, RAM = RAM1, intervals.type = "LB",
           diag.constraints = TRUE,
           mx.algebras = list(ind   = mxAlgebra(a * b,     name = "ind"),
                              dir   = mxAlgebra(c,         name = "dir"),
                              total = mxAlgebra(c + a * b, name = "total"))),
    error = function(e) NULL
  ) else NULL
  if (!is.null(stage2_s)) stage2_s <- metaSEM::rerun(stage2_s)
  if (is.null(stage2_s)) {
    study_contrib_cps[[i]] <- data.frame(
      Study    = dat_cps_comp$Citation[i],
      N        = n_cps_comp[i],
      Indirect = NA, CI_lower = NA, CI_upper = NA)
  } else {
    algs_s <- summary(stage2_s)$mx.algebras
    study_contrib_cps[[i]] <- data.frame(
      Study    = dat_cps_comp$Citation[i],
      N        = n_cps_comp[i],
      Indirect = algs_s["ind", "Estimate"],
      CI_lower = algs_s["ind", "lbound"],
      CI_upper = algs_s["ind", "ubound"])
  }
}

algs_cps_pool  <- summary(stage2_cps_comp)$mx.algebras
contrib_df_cps <- do.call(rbind, study_contrib_cps)
contrib_df_cps <- rbind(
  contrib_df_cps,
  data.frame(Study    = "Pooled (TSSEM)",
             N        = sum(n_cps_comp),
             Indirect = algs_cps_pool["ind", "Estimate"],
             CI_lower = algs_cps_pool["ind", "lbound"],
             CI_upper = algs_cps_pool["ind", "ubound"])
)
print(contrib_df_cps)


# leave-one-out sensitivity analysis ----
k_cps <- nrow(dat_cps_comp)
loo_results_cps <- vector("list", k_cps)

for (i in seq_len(k_cps)) {
  Rlist_loo_cps <- Rlist_cps[-i]
  n_loo_cps     <- n_cps_comp[-i]

  stage1_loo_cps <- tryCatch(
    tssem1(Rlist_loo_cps, n_loo_cps, method = "FEM"),
    error = function(e) NULL
  )

  if (is.null(stage1_loo_cps)) {
    loo_results_cps[[i]] <- data.frame(
      excluded_study = dat_cps_comp$Citation[i],
      excluded_N     = dat_cps_comp$n[i],
      a = NA, b = NA, c = NA,
      ind = NA, ind_lbound = NA, ind_ubound = NA,
      converged = FALSE
    )
    next
  }

  stage2_loo_cps <- tryCatch(
    tssem2(stage1_loo_cps,
           RAM              = RAM1,
           intervals.type   = "LB",
           diag.constraints = TRUE,
           mx.algebras      = list(
             ind   = mxAlgebra(a * b,     name = "ind"),
             dir   = mxAlgebra(c,         name = "dir"),
             total = mxAlgebra(c + a * b, name = "total")
           )),
    error = function(e) NULL
  )

  if (!is.null(stage2_loo_cps)) {
    stage2_loo_cps <- metaSEM::rerun(stage2_loo_cps)
  }

  if (is.null(stage2_loo_cps)) {
    loo_results_cps[[i]] <- data.frame(
      excluded_study = dat_cps_comp$Citation[i],
      excluded_N     = dat_cps_comp$n[i],
      a = NA, b = NA, c = NA,
      ind = NA, ind_lbound = NA, ind_ubound = NA,
      converged = FALSE
    )
    next
  }

  s_cps     <- summary(stage2_loo_cps)
  coefs_cps <- s_cps$coefficients
  algs_cps  <- s_cps$mx.algebras

  loo_results_cps[[i]] <- data.frame(
    excluded_study = dat_cps_comp$Citation[i],
    excluded_N     = dat_cps_comp$n[i],
    a              = coefs_cps["a", "Estimate"],
    b              = coefs_cps["b", "Estimate"],
    c              = coefs_cps["c", "Estimate"],
    ind            = algs_cps["ind",   "Estimate"],
    ind_lbound     = algs_cps["ind",   "lbound"],
    ind_ubound     = algs_cps["ind",   "ubound"],
    converged      = TRUE
  )
}

loo_df_cps <- do.call(rbind, loo_results_cps)
print(loo_df_cps)

# LOO plot ----
ind_full_cps <- summary(stage2_cps_comp)$mx.algebras["ind", "Estimate"]
lb_full_cps  <- summary(stage2_cps_comp)$mx.algebras["ind", "lbound"]
ub_full_cps  <- summary(stage2_cps_comp)$mx.algebras["ind", "ubound"]

loo_df_cps$label <- paste0("Excl. ", loo_df_cps$excluded_study,
                            " (N=", loo_df_cps$excluded_N, ")")

ggplot(loo_df_cps, aes(x = ind, y = reorder(label, ind))) +
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
    subtitle = "Indirect effect (a×b): Trauma → Depression → Composite Psychosis Score",
    x        = "Indirect effect estimate", y = NULL,
    caption  = "Dashed line = full-sample estimate. Shaded band = full-sample 95% CI.\nRed line = zero."
  ) +
  theme_minimal(base_size = 12)


# meta-regression ----
# Corrected Stage-1 meta-regressions (issue #2), diagonal random-effects Tau2,
# plus sample type as a moderator (0 = non-clinical / GP reference, 1 = clinical).
metareg_cps_age    <- fit_metareg(Rlist_cps, n_cps_comp, dat_cps_comp$`mean age`)
metareg_cps_female <- fit_metareg(Rlist_cps, n_cps_comp, dat_cps_comp$`%_female`)
metareg_cps_nos    <- fit_metareg(Rlist_cps, n_cps_comp, dat_cps_comp$NOS)
metareg_cps_sample <- fit_metareg(Rlist_cps, n_cps_comp,
                                  ifelse(dat_cps_comp$`clinicalsample (GP 1, CS 2)` == "GP", 0, 1),
                                  center = FALSE)


# PDF export ----
library(gridExtra)
library(grid)

pdf_path_cps <- file.path(getwd(), "Depression mediation results.pdf")
pdf(pdf_path_cps, width = 11, height = 8.5)

ptitle <- function(txt)
  textGrob(txt, gp = gpar(fontsize = 13, fontface = "bold"), just = "centre")

fmt2 <- function(x) formatC(round(x, 2), digits = 2, format = "f")
ci   <- function(lb, ub) paste0("[", fmt2(lb), ", ", fmt2(ub), "]")

# helper: meta() summary -> data frame
meta_to_df <- function(m, mod_label, outcome_name = "Outcome") {
  cf <- summary(m)$coefficients

  # Remove Tau2 (heterogeneity) rows
  cf <- cf[!grepl("^Tau2", rownames(cf)), , drop = FALSE]

  # Readable parameter labels
  param_map <- c(
    "Intercept1" = "Intercept r(Trauma, Depression)",
    "Intercept2" = paste0("Intercept r(Trauma, ",      outcome_name, ")"),
    "Intercept3" = paste0("Intercept r(Depression, ",  outcome_name, ")"),
    "Slope1_1"   = "r(Trauma, Depression)",
    "Slope2_1"   = paste0("r(Trauma, ",      outcome_name, ")"),
    "Slope3_1"   = paste0("r(Depression, ",  outcome_name, ")")
  )
  param_labels <- ifelse(rownames(cf) %in% names(param_map),
                         param_map[rownames(cf)],
                         rownames(cf))

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

# PAGE 1: TITLE
grid.newpage()
grid.text("Depression as Mediator – Meta-SEM Results",
          gp = gpar(fontsize = 22, fontface = "bold"), y = 0.58)
grid.text(paste("Generated:", format(Sys.Date(), "%d %B %Y")),
          gp = gpar(fontsize = 12), y = 0.46)
grid.text("Childhood Trauma → Depression → Psychosis mediation model (Fixed-Effects)",
          gp = gpar(fontsize = 12, fontface = "italic"), y = 0.40)

# PAGE 2: POOLED CORRELATION MATRIX
corr_disp <- round(fe_corr, 2)
corr_disp[upper.tri(corr_disp)] <- NA
diag(corr_disp)                 <- NA
corr_str <- matrix(
  ifelse(is.na(corr_disp), "—",
         formatC(corr_disp, digits = 2, format = "f")),
  nrow = nrow(corr_disp), dimnames = dimnames(corr_disp)
)
corr_tbl <- cbind(Variable = rownames(corr_str), as.data.frame(corr_str))
grid.arrange(
  ptitle("Section 1 — Pooled Correlation Matrix (Fixed-Effects Model)"),
  tableGrob(corr_tbl, rows = NULL, theme = ttheme_minimal(base_size = 13)),
  ncol = 1, heights = c(0.08, 0.92)
)

# PAGE 3: PATH COEFFICIENTS & MEDIATION EFFECTS
s2_cps        <- summary(stage2_cps_comp)
coefs_cps2    <- s2_cps$coefficients
algs_cps2     <- s2_cps$mx.algebras
total_est_cps <- algs_cps2["total", "Estimate"]
ind_est_cps   <- algs_cps2["ind",   "Estimate"]
pct_med_cps   <- if (!is.na(total_est_cps) && total_est_cps != 0)
                   paste0(fmt2(ind_est_cps / total_est_cps * 100), "%") else "—"

path_df_cps <- data.frame(
  Effect   = c("a  (Trauma → Depression)",
               "b  (Depression → Composite Psychosis Score)",
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
plot(stage2_cps_comp, color = "green",
     main = "Section 1 — SEM Path Diagram")

# PAGE 5: PER-STUDY INDIRECT EFFECTS
contrib_disp_cps <- contrib_df_cps
contrib_disp_cps$`95% CI` <- paste0("[", fmt2(contrib_disp_cps$CI_lower),
                                     ", ", fmt2(contrib_disp_cps$CI_upper), "]")
contrib_disp_cps$Indirect  <- fmt2(contrib_disp_cps$Indirect)
contrib_disp_cps <- contrib_disp_cps[, c("Study", "N", "Indirect", "95% CI")]
grid.arrange(
  ptitle("Section 1 — Per-Study Indirect Effects: Trauma → Depression → CPS"),
  tableGrob(contrib_disp_cps, rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.07, 0.93)
)

# PAGE 6: LOO SENSITIVITY ANALYSIS TABLE
loo_num <- c("a", "b", "c", "ind", "ind_lbound", "ind_ubound")
loo_disp_cps <- loo_df_cps
loo_disp_cps[, loo_num] <- lapply(loo_disp_cps[, loo_num], function(x) fmt2(x))
loo_disp_cps <- loo_disp_cps[, c("excluded_study", "excluded_N",
                                  "a", "b", "c", "ind", "ind_lbound", "ind_ubound", "converged")]
colnames(loo_disp_cps) <- c("Study", "N", "a", "b", "c",
                             "Indirect", "CI lower", "CI upper", "Converged")
grid.arrange(
  ptitle("Section 2 — Leave-One-Out Sensitivity Analysis: Results Table"),
  tableGrob(loo_disp_cps, rows = NULL, theme = ttheme_minimal(base_size = 9)),
  ncol = 1, heights = c(0.06, 0.94)
)

# PAGE 7: LOO SENSITIVITY ANALYSIS PLOT
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
    title    = "Section 2 — Leave-One-Out Sensitivity Analysis",
    subtitle = "Indirect effect (a×b): Trauma → Depression → Composite Psychosis Score",
    x        = "Indirect effect estimate", y = NULL,
    caption  = paste("Dashed line = full-sample estimate.",
                     "Shaded band = full-sample 95% CI. Red line = zero.")
  ) +
  theme_minimal(base_size = 12)
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

# PAGE 10: META-REGRESSION — NOS (STUDY QUALITY)
grid.arrange(
  ptitle("Section 3 — Meta-Regression: Study Quality (NOS) as Moderator"),
  tableGrob(meta_to_df(metareg_cps_nos, "NOS (grand-mean centred)", "CPS"),
            rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.08, 0.92)
)

# PAGE 11: META-REGRESSION — SAMPLE TYPE (clinical vs non-clinical; GP = reference)
# Note: k = 5 exploratory result; report with caution (issue #7).
grid.arrange(
  ptitle("Section 3 — Meta-Regression: Sample Type (Clinical vs Non-clinical; GP = reference)"),
  metareg_grob(metareg_cps_sample, "Sample type (0 = non-clinical/GP, 1 = clinical)", "CPS"),
  ncol = 1, heights = c(0.08, 0.92)
)

dev.off()
message("PDF saved: ", pdf_path_cps)
