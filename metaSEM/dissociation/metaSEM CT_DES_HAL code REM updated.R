####packages####
install.packages("metaSEM")
install.packages("OpenMx")
install.packages("readxl")
install.packages("flextable")
install.packages("ggplot2")
install.packages("tidyverse")

library (ggplot2)
library(OpenMx)
library(metaSEM)
library(readxl)
library(semPlot)
library(flextable)
library(tidyverse)

####load data####
setwd("/Users/rodolforossi/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/Research projects/metaSEM/dissociation")
dat <- read_excel("metanalisis mediation dissociation workcopy.xlsx")

print(head(dat))


#impose columns as numeric
num_cols <- c("n", "mean age", "%_female", "r_XM",	"r_MY",	"r_XY")
dat[num_cols] <- lapply(dat[num_cols], as.numeric)





# HALLUCINATIONS ----


# create subset for hallucinations ---- 
dat_hal <- subset(dat, psychosis_construct == "hallucinations"  & !is.na(r_XM) & !is.na(r_XY) & !is.na(r_MY) & !is.na(n))
 
# create sub-subset for hallucinations & composite trauma & composite dissociation
dat_hal_comp <- subset(dat, psychosis_construct == "hallucinations"  & trauma_type == "composite" & dissociation_domain == "composite" & !is.na(r_XM) & !is.na(r_XY) & !is.na(r_MY) & !is.na(n))


# Variable names in the correlation matrices
dat_hal_comp
vars_hal <- c("Trauma", "Dissociation", "Hallucinations")

# Build list of per-study 3x3 correlation matrices

Rlist_hal <- lapply(seq_len(nrow(dat_hal_comp)), function(i) {
  r_xm  <- dat_hal_comp$r_XM[i]
  r_xy  <- dat_hal_comp$r_XY[i]
  r_my  <- dat_hal_comp$r_MY[i]
  
  R <- matrix(c(
    1,    r_xm, r_xy,
    r_xm, 1,    r_my,
    r_xy, r_my, 1
  ), nrow = 3, byrow = TRUE)
  
  dimnames(R) <- list(vars_hal, vars_hal)
  R
})


# Vector of sample sizes
n_hal_comp <- dat_hal_comp$n



# Stage 1: Random-effects meta-analysis of correlation matrices ----

stage1_hal_comp <- tssem1(Rlist_hal, n_hal_comp, method="REM", RE.type="Diag")
summary(stage1_hal_comp)

## Average correlation matrix under a random-effects model
rand_corr <- vec2symMat(coef(stage1_hal_comp, select="fixed"), diag = FALSE)
dimnames(rand_corr)<-list(vars_hal, vars_hal)
round(rand_corr, 2)


#need to export this as well!!
coef(stage1_hal_comp, select="random")

#### STAGE 2 see https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/sem.html)  and Cheung 2022 supp materials #### 


## Proposed model in lavaan syntax
model1 <- "Hallucinations ~ c*Trauma + b*Dissociation
Dissociation ~ a*Trauma
Trauma ~~ 1*Trauma"
plot(model1)

## Convert the lavaan syntax to RAM specification used in metaSEM
RAM1 <- lavaan2RAM(model1, obs.variables=vars_hal)
RAM1


####fit model stage2####

stage2_hal_comp <- tssem2(stage1_hal_comp, 
               RAM=RAM1, 
               intervals.type = "LB", 
               diag.constraints = TRUE,
               mx.algebras = list(ind=mxAlgebra(a*b, name="ind"),
                                  dir=mxAlgebra(c, name="dir"),
                                  total = mxAlgebra(c + a*b,name = "total")))

#stage2_hal_comp <- rerun(stage2_hal_comp)   # overwrite with better solution
summary(stage2_hal_comp)

#### SEM plot ####
plot(stage2_hal_comp, color="green")


# ── PER-STUDY INDIRECT EFFECT TABLE: HALLUCINATIONS ─────────────────────────
n_studies_hal <- nrow(dat_hal_comp)
study_contrib_hal <- vector("list", n_studies_hal)

for (i in seq_len(n_studies_hal)) {
  stage1_s <- tryCatch(
    tssem1(Rlist_hal[i], n_hal_comp[i], method = "FEM"),
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
    study_contrib_hal[[i]] <- data.frame(
      Study    = paste(trimws(dat_hal_comp$studyID[i]), dat_hal_comp$year[i]),
      N        = n_hal_comp[i],
      Indirect = NA, CI_lower = NA, CI_upper = NA)
  } else {
    algs_s <- summary(stage2_s)$mx.algebras
    study_contrib_hal[[i]] <- data.frame(
      Study    = paste(trimws(dat_hal_comp$studyID[i]), dat_hal_comp$year[i]),
      N        = n_hal_comp[i],
      Indirect = algs_s["ind", "Estimate"],
      CI_lower = algs_s["ind", "lbound"],
      CI_upper = algs_s["ind", "ubound"])
  }
}

algs_hal_pool  <- summary(stage2_hal_comp)$mx.algebras
contrib_df_hal <- do.call(rbind, study_contrib_hal)
contrib_df_hal <- rbind(
  contrib_df_hal,
  data.frame(Study    = "Pooled (TSSEM)",
             N        = sum(n_hal_comp),
             Indirect = algs_hal_pool["ind", "Estimate"],
             CI_lower = algs_hal_pool["ind", "lbound"],
             CI_upper = algs_hal_pool["ind", "ubound"])
)
print(contrib_df_hal)


# sensitivity analysis by Claude ----
# ── LEAVE-ONE-OUT SENSITIVITY ANALYSIS ─────────────────────────────────────

k <- nrow(dat_hal_comp)

loo_results <- vector("list", k)


for (i in seq_len(k)) {
  
  Rlist_loo <- Rlist_hal[-i]
  n_loo     <- n_hal_comp[-i]
  
  stage1_loo <- tryCatch(
    tssem1(Rlist_loo, n_loo, method = "REM", RE.type = "Diag"),
    error = function(e) NULL
  )
  
  if (is.null(stage1_loo)) {
    loo_results[[i]] <- data.frame(
      excluded_study = paste(trimws(dat_hal_comp$studyID[i]), dat_hal_comp$year[i]),
      excluded_N     = dat_hal_comp$n[i],
      a              = NA, b = NA, c = NA,
      ind            = NA, ind_lbound = NA, ind_ubound = NA,
      converged      = FALSE
    )
    next
  }
  
  stage2_loo <- tryCatch(
    tssem2(stage1_loo,
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
  
  if (!is.null(stage2_loo)) {
    stage2_loo <- metaSEM::rerun(stage2_loo)
  }
  
  if (is.null(stage2_loo)) {
    loo_results[[i]] <- data.frame(
      excluded_study = paste(trimws(dat_hal_comp$studyID[i]), dat_hal_comp$year[i]),
      excluded_N     = dat_hal_comp$n[i],
      a              = NA, b = NA, c = NA,
      ind            = NA, ind_lbound = NA, ind_ubound = NA,
      converged      = FALSE
    )
    next
  }
  
  s     <- summary(stage2_loo)
  coefs <- s$coefficients
  algs  <- s$mx.algebras
  
  loo_results[[i]] <- data.frame(
    excluded_study = paste(trimws(dat_hal_comp$studyID[i]), dat_hal_comp$year[i]),
    excluded_N     = dat_hal_comp$n[i],
    a              = coefs["a", "Estimate"],
    b              = coefs["b", "Estimate"],
    c              = coefs["c", "Estimate"],
    ind            = algs["ind",   "Estimate"],
    ind_lbound     = algs["ind",   "lbound"],
    ind_ubound     = algs["ind",   "ubound"],
    converged      = TRUE
  )
}

loo_df <- do.call(rbind, loo_results)
print(loo_df)

# ── PLOT ────────────────────────────────────────────────────────────────────

ind_full <- summary(stage2_hal_comp)$mx.algebras["ind", "Estimate"]
lb_full  <- summary(stage2_hal_comp)$mx.algebras["ind", "lbound"]
ub_full  <- summary(stage2_hal_comp)$mx.algebras["ind", "ubound"]

loo_df$label <- paste0("Excl. study ", loo_df$excluded_study,
                       " (N=", loo_df$excluded_N, ")")

ggplot(loo_df, aes(x = ind, y = reorder(label, ind))) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = ind_lbound, xmax = ind_ubound),
                 height = 0.25, color = "steelblue") +
  geom_vline(xintercept = ind_full, linetype = "dashed",
             color = "black", linewidth = 0.7) +
  geom_vline(xintercept = 0, linetype = "solid",
             color = "red", linewidth = 0.5) +
  annotate("rect",
           xmin = lb_full, xmax = ub_full,
           ymin = -Inf,    ymax = Inf,
           alpha = 0.1, fill = "black") +
  labs(
    title    = "Leave-One-Out Sensitivity Analysis",
    subtitle = "Indirect effect (a×b): Trauma → Dissociation → Hallucinations",
    x        = "Indirect effect estimate",
    y        = NULL,
    caption  = "Dashed line = full-sample estimate. Shaded band = full-sample 95% CI.\nRed line = zero."
  ) +
  theme_minimal(base_size = 12)





# ── META-REGRESSION: HALLUCINATIONS ─────────────────────────────────────────
# Note: tssem1() passes ... to mxRun(), not meta(), so moderators cannot be
# passed via tssem1(). Meta-regression is done here at Stage 1 using meta()
# directly on the vectorized correlations. tssem2() accepts only tssem1 objects,
# so moderation is tested on the correlations, not on the path coefficients.

# Create grand-mean-centred moderators
dat_hal_comp$age_c    <- as.numeric(scale(dat_hal_comp$`mean age`, scale = FALSE))
dat_hal_comp$female_c <- as.numeric(scale(dat_hal_comp$`%_female`, scale = FALSE))
dat_hal_comp$nos_c    <- as.numeric(scale(dat_hal_comp$NOS,        scale = FALSE))

# Vectorise lower-triangle correlations per study: r_TrDis, r_TrHal, r_DisHal
cors_mat <- t(sapply(Rlist_hal, function(R) c(R[2,1], R[3,1], R[3,2])))
colnames(cors_mat) <- c("r_TrDis", "r_TrHal", "r_DisHal")

# Sampling variances: var(r) ≈ (1 - r²)² / (n - 1)
# meta() requires v as vech of the sampling covariance matrix (lower triangle):
# columns = [v11, v21, v31, v22, v32, v33]; off-diagonal set to 0 (independent assumption)
v1 <- (1 - cors_mat[, "r_TrDis"]^2)^2  / (n_hal_comp - 1)
v2 <- (1 - cors_mat[, "r_TrHal"]^2)^2  / (n_hal_comp - 1)
v3 <- (1 - cors_mat[, "r_DisHal"]^2)^2 / (n_hal_comp - 1)
var_mat <- cbind(v1, 0, 0, v2, 0, v3)

# Meta-regression Stage 1: mean age as moderator
metareg_hal_age <- meta(y = cors_mat,
                        v = var_mat,
                        x = matrix(dat_hal_comp$age_c, ncol = 1))
summary(metareg_hal_age)

# Meta-regression Stage 1: proportion female as moderator
metareg_hal_female <- meta(y = cors_mat,
                           v = var_mat,
                           x = matrix(dat_hal_comp$female_c, ncol = 1))
summary(metareg_hal_female)

# Meta-regression Stage 1: study quality (NOS) as moderator
metareg_hal_nos <- meta(y = cors_mat,
                        v = var_mat,
                        x = matrix(dat_hal_comp$nos_c, ncol = 1))
summary(metareg_hal_nos)

# ── PDF EXPORT: Hallucinations Results ────────────────────────────────────
library(gridExtra)
library(grid)

pdf_path <- file.path(getwd(), "Hallucinations results.pdf")
pdf(pdf_path, width = 11, height = 8.5)

ptitle <- function(txt)
  textGrob(txt, gp = gpar(fontsize = 13, fontface = "bold"), just = "centre")

fmt2 <- function(x) formatC(round(x, 2), digits = 2, format = "f")
ci   <- function(lb, ub) paste0("[", fmt2(lb), ", ", fmt2(ub), "]")

# ── PAGE 1: TITLE ──────────────────────────────────────────────────────────
grid.newpage()
grid.text("Hallucinations – Meta-SEM Results",
          gp = gpar(fontsize = 22, fontface = "bold"), y = 0.58)
grid.text(paste("Generated:", format(Sys.Date(), "%d %B %Y")),
          gp = gpar(fontsize = 12), y = 0.46)
grid.text("Trauma \u2192 Dissociation \u2192 Hallucinations mediation model",
          gp = gpar(fontsize = 12, fontface = "italic"), y = 0.40)

# ── PAGE 2: POOLED CORRELATION MATRIX ─────────────────────────────────────
corr_disp <- round(rand_corr, 2)
corr_disp[upper.tri(corr_disp)] <- NA
diag(corr_disp)                 <- NA
corr_str <- matrix(
  ifelse(is.na(corr_disp), "\u2014",
         formatC(corr_disp, digits = 2, format = "f")),
  nrow = nrow(corr_disp), dimnames = dimnames(corr_disp)
)
corr_tbl <- cbind(Variable = rownames(corr_str), as.data.frame(corr_str))

grid.arrange(
  ptitle("Section 1 \u2014 Pooled Correlation Matrix (Random-Effects Model)"),
  tableGrob(corr_tbl, rows = NULL, theme = ttheme_minimal(base_size = 13)),
  ncol = 1, heights = c(0.08, 0.92)
)

# ── PAGE 3: PATH COEFFICIENTS & MEDIATION EFFECTS ─────────────────────────
s2        <- summary(stage2_hal_comp)
coefs     <- s2$coefficients
algs      <- s2$mx.algebras
total_est <- algs["total", "Estimate"]
ind_est   <- algs["ind",   "Estimate"]
pct_med   <- if (!is.na(total_est) && total_est != 0)
               paste0(fmt2(ind_est / total_est * 100), "%") else "\u2014"

path_df <- data.frame(
  Effect   = c("a  (Trauma \u2192 Dissociation)",
               "b  (Dissociation \u2192 Hallucinations)",
               "c  (Trauma \u2192 Hallucinations, direct)",
               "Indirect effect (a \u00d7 b)",
               "Total effect (c + a \u00d7 b)",
               "% of total effect mediated"),
  Estimate = c(fmt2(coefs["a","Estimate"]),
               fmt2(coefs["b","Estimate"]),
               fmt2(coefs["c","Estimate"]),
               fmt2(ind_est),
               fmt2(total_est),
               pct_med),
  `95% CI` = c(ci(coefs["a","lbound"],    coefs["a","ubound"]),
               ci(coefs["b","lbound"],    coefs["b","ubound"]),
               ci(coefs["c","lbound"],    coefs["c","ubound"]),
               ci(algs["ind",  "lbound"], algs["ind",  "ubound"]),
               ci(algs["total","lbound"], algs["total","ubound"]),
               "\u2014"),
  check.names = FALSE
)

grid.arrange(
  ptitle("Section 1 \u2014 Path Coefficients and Mediation Effects"),
  tableGrob(path_df, rows = NULL, theme = ttheme_minimal(base_size = 12)),
  ncol = 1, heights = c(0.08, 0.92)
)

# ── PAGE 4: SEM PATH DIAGRAM ───────────────────────────────────────────────
plot(stage2_hal_comp, color = "green",
     main = "Section 1 \u2014 SEM Path Diagram")

# ── PAGE 5: SENSITIVITY ANALYSIS TABLE ────────────────────────────────────
# PAGE 4 (per-study indirect effects) ──────────────────────────────────────
contrib_disp_hal <- contrib_df_hal
contrib_disp_hal$`95% CI` <- paste0("[", fmt2(contrib_disp_hal$CI_lower),
                                     ", ", fmt2(contrib_disp_hal$CI_upper), "]")
contrib_disp_hal$Indirect  <- fmt2(contrib_disp_hal$Indirect)
contrib_disp_hal <- contrib_disp_hal[, c("Study", "N", "Indirect", "95% CI")]
grid.arrange(
  ptitle("Section 1 -- Per-Study Indirect Effects: Trauma -> Dissociation -> Hallucinations"),
  tableGrob(contrib_disp_hal, rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.07, 0.93)
)

loo_num <- c("a", "b", "c", "ind", "ind_lbound", "ind_ubound")
loo_disp <- loo_df
loo_disp[, loo_num] <- lapply(loo_disp[, loo_num],
                               function(x) fmt2(x))
loo_disp <- loo_disp[, c("excluded_study","excluded_N",
                          "a","b","c","ind","ind_lbound","ind_ubound","converged")]
colnames(loo_disp) <- c("Study","N","a","b","c",
                         "Indirect","CI lower","CI upper","Converged")

grid.arrange(
  ptitle("Section 2 \u2014 Leave-One-Out Sensitivity Analysis: Results Table"),
  tableGrob(loo_disp, rows = NULL, theme = ttheme_minimal(base_size = 9)),
  ncol = 1, heights = c(0.06, 0.94)
)

# ── PAGE 6: SENSITIVITY ANALYSIS PLOT ─────────────────────────────────────
p_loo <- ggplot(loo_df, aes(x = ind, y = reorder(label, ind))) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = ind_lbound, xmax = ind_ubound),
                 height = 0.25, color = "steelblue") +
  geom_vline(xintercept = ind_full, linetype = "dashed",
             color = "black", linewidth = 0.7) +
  geom_vline(xintercept = 0, linetype = "solid",
             color = "red", linewidth = 0.5) +
  annotate("rect",
           xmin = lb_full, xmax = ub_full,
           ymin = -Inf, ymax = Inf,
           alpha = 0.1, fill = "black") +
  labs(
    title    = "Section 2 \u2014 Leave-One-Out Sensitivity Analysis",
    subtitle = "Indirect effect (a\u00d7b): Trauma \u2192 Dissociation \u2192 Hallucinations",
    x        = "Indirect effect estimate", y = NULL,
    caption  = paste("Dashed line = full-sample estimate.",
                     "Shaded band = full-sample 95% CI. Red line = zero.")
  ) +
  theme_minimal(base_size = 12)
print(p_loo)

# ── helper: meta() summary \u2192 data frame ─────────────────────────────────────
meta_to_df <- function(m, mod_label, outcome_name = "Outcome") {
  cf <- summary(m)$coefficients

  # Remove Tau2 (heterogeneity) rows
  cf <- cf[!grepl("^Tau2", rownames(cf)), , drop = FALSE]

  # Readable parameter labels
  param_map <- c(
    "Intercept1" = "Intercept r(Trauma, Dissociation)",
    "Intercept2" = paste0("Intercept r(Trauma, ",       outcome_name, ")"),
    "Intercept3" = paste0("Intercept r(Dissociation, ", outcome_name, ")"),
    "Slope1_1"   = "r(Trauma, Dissociation)",
    "Slope2_1"   = paste0("r(Trauma, ",       outcome_name, ")"),
    "Slope3_1"   = paste0("r(Dissociation, ", outcome_name, ")")
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

# ── PAGE 7: META-REGRESSION — MEAN AGE ────────────────────────────────────
grid.arrange(
  ptitle("Section 3 \u2014 Meta-Regression: Mean Age as Moderator"),
  tableGrob(meta_to_df(metareg_hal_age, "Mean Age (grand-mean centred)", "Hallucinations"),
            rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.08, 0.92)
)

# ── PAGE 8: META-REGRESSION — % FEMALE ────────────────────────────────────
grid.arrange(
  ptitle("Section 3 \u2014 Meta-Regression: % Female as Moderator"),
  tableGrob(meta_to_df(metareg_hal_female, "% Female (grand-mean centred)", "Hallucinations"),
            rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.08, 0.92)
)

# \u2500\u2500 PAGE 9: META-REGRESSION \u2014 NOS (STUDY QUALITY) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
grid.arrange(
  ptitle("Section 3 \u2014 Meta-Regression: Study Quality (NOS) as Moderator"),
  tableGrob(meta_to_df(metareg_hal_nos, "NOS (grand-mean centred)", "Hallucinations"),
            rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.08, 0.92)
)

dev.off()
message("PDF saved: ", pdf_path)











# DELUSIONS ----

# create subset for delusions ----
dat_del <- subset(dat, psychosis_construct == "delusions" & !is.na(r_XM) & !is.na(r_XY) & !is.na(r_MY) & !is.na(n))

# create sub-subset for delusions & composite dissociation
dat_del_comp <- subset(dat, psychosis_construct == "delusions" & dissociation_domain == "composite" & !is.na(r_XM) & !is.na(r_XY) & !is.na(r_MY) & !is.na(n))

# Variable names in the correlation matrices
vars_del <- c("Trauma", "Dissociation", "Delusions")

# Build list of per-study 3x3 correlation matrices
Rlist_del <- lapply(seq_len(nrow(dat_del_comp)), function(i) {
  r_xm <- dat_del_comp$r_XM[i]
  r_xy <- dat_del_comp$r_XY[i]
  r_my <- dat_del_comp$r_MY[i]
  R <- matrix(c(
    1,    r_xm, r_xy,
    r_xm, 1,    r_my,
    r_xy, r_my, 1
  ), nrow = 3, byrow = TRUE)
  dimnames(R) <- list(vars_del, vars_del)
  R
})

# Vector of sample sizes
n_del_comp <- dat_del_comp$n

# Stage 1: Random-effects meta-analysis of correlation matrices ----
stage1_del_comp <- tssem1(Rlist_del, n_del_comp, method = "REM", RE.type = "Diag")
summary(stage1_del_comp)

## Average correlation matrix under a random-effects model
rand_corr <- vec2symMat(coef(stage1_del_comp, select = "fixed"), diag = FALSE)
dimnames(rand_corr) <- list(vars_del, vars_del)
round(rand_corr, 2)
coef(stage1_del_comp, select = "random")

## Proposed model in lavaan syntax
model1 <- "Delusions ~ c*Trauma + b*Dissociation
Dissociation ~ a*Trauma
Trauma ~~ 1*Trauma"
plot(model1)

## Convert the lavaan syntax to RAM specification used in metaSEM
RAM1 <- lavaan2RAM(model1, obs.variables = vars_del)
RAM1

####fit model stage2####
stage2_del_comp <- tssem2(stage1_del_comp,
                          RAM              = RAM1,
                          intervals.type   = "LB",
                          diag.constraints = TRUE,
                          mx.algebras      = list(ind   = mxAlgebra(a * b,     name = "ind"),
                                                  dir   = mxAlgebra(c,         name = "dir"),
                                                  total = mxAlgebra(c + a * b, name = "total")))
stage2_del_comp <- metaSEM::rerun(stage2_del_comp)
summary(stage2_del_comp)

#### SEM plot ####
plot(stage2_del_comp, color = "green")


# ── PER-STUDY INDIRECT EFFECT TABLE: DELUSIONS ───────────────────────────────
n_studies_del <- nrow(dat_del_comp)
study_contrib_del <- vector("list", n_studies_del)

for (i in seq_len(n_studies_del)) {
  stage1_s <- tryCatch(
    tssem1(Rlist_del[i], n_del_comp[i], method = "FEM"),
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
    study_contrib_del[[i]] <- data.frame(
      Study    = paste(trimws(dat_del_comp$studyID[i]), dat_del_comp$year[i]),
      N        = n_del_comp[i],
      Indirect = NA, CI_lower = NA, CI_upper = NA)
  } else {
    algs_s <- summary(stage2_s)$mx.algebras
    study_contrib_del[[i]] <- data.frame(
      Study    = paste(trimws(dat_del_comp$studyID[i]), dat_del_comp$year[i]),
      N        = n_del_comp[i],
      Indirect = algs_s["ind", "Estimate"],
      CI_lower = algs_s["ind", "lbound"],
      CI_upper = algs_s["ind", "ubound"])
  }
}

algs_del_pool  <- summary(stage2_del_comp)$mx.algebras
contrib_df_del <- do.call(rbind, study_contrib_del)
contrib_df_del <- rbind(
  contrib_df_del,
  data.frame(Study    = "Pooled (TSSEM)",
             N        = sum(n_del_comp),
             Indirect = algs_del_pool["ind", "Estimate"],
             CI_lower = algs_del_pool["ind", "lbound"],
             CI_upper = algs_del_pool["ind", "ubound"])
)
print(contrib_df_del)


# ── LEAVE-ONE-OUT SENSITIVITY ANALYSIS ─────────────────────────────────────
k_del <- nrow(dat_del_comp)
loo_results_del <- vector("list", k_del)

for (i in seq_len(k_del)) {
  Rlist_loo_del <- Rlist_del[-i]
  n_loo_del     <- n_del_comp[-i]

  stage1_loo_del <- tryCatch(
    tssem1(Rlist_loo_del, n_loo_del, method = "REM", RE.type = "Diag"),
    error = function(e) NULL
  )

  if (is.null(stage1_loo_del)) {
    loo_results_del[[i]] <- data.frame(
      excluded_study = paste(trimws(dat_del_comp$studyID[i]), dat_del_comp$year[i]),
      excluded_N     = dat_del_comp$n[i],
      a = NA, b = NA, c = NA,
      ind = NA, ind_lbound = NA, ind_ubound = NA,
      converged = FALSE
    )
    next
  }

  stage2_loo_del <- tryCatch(
    tssem2(stage1_loo_del,
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

  if (!is.null(stage2_loo_del)) {
    stage2_loo_del <- metaSEM::rerun(stage2_loo_del)
  }

  if (is.null(stage2_loo_del)) {
    loo_results_del[[i]] <- data.frame(
      excluded_study = paste(trimws(dat_del_comp$studyID[i]), dat_del_comp$year[i]),
      excluded_N     = dat_del_comp$n[i],
      a = NA, b = NA, c = NA,
      ind = NA, ind_lbound = NA, ind_ubound = NA,
      converged = FALSE
    )
    next
  }

  s_del     <- summary(stage2_loo_del)
  coefs_del <- s_del$coefficients
  algs_del  <- s_del$mx.algebras

  loo_results_del[[i]] <- data.frame(
    excluded_study = paste(trimws(dat_del_comp$studyID[i]), dat_del_comp$year[i]),
    excluded_N     = dat_del_comp$n[i],
    a              = coefs_del["a", "Estimate"],
    b              = coefs_del["b", "Estimate"],
    c              = coefs_del["c", "Estimate"],
    ind            = algs_del["ind",   "Estimate"],
    ind_lbound     = algs_del["ind",   "lbound"],
    ind_ubound     = algs_del["ind",   "ubound"],
    converged      = TRUE
  )
}

loo_df_del <- do.call(rbind, loo_results_del)
print(loo_df_del)

# ── PLOT ────────────────────────────────────────────────────────────────────
ind_full_del <- summary(stage2_del_comp)$mx.algebras["ind", "Estimate"]
lb_full_del  <- summary(stage2_del_comp)$mx.algebras["ind", "lbound"]
ub_full_del  <- summary(stage2_del_comp)$mx.algebras["ind", "ubound"]

loo_df_del$label <- paste0("Excl. study ", loo_df_del$excluded_study,
                            " (N=", loo_df_del$excluded_N, ")")

ggplot(loo_df_del, aes(x = ind, y = reorder(label, ind))) +
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
    subtitle = "Indirect effect (a\u00d7b): Trauma \u2192 Dissociation \u2192 Delusions",
    x        = "Indirect effect estimate", y = NULL,
    caption  = "Dashed line = full-sample estimate. Shaded band = full-sample 95% CI.\nRed line = zero."
  ) +
  theme_minimal(base_size = 12)

# ── META-REGRESSION ──────────────────────────────────────────────────────────
dat_del_comp$age_c    <- as.numeric(scale(dat_del_comp$`mean age`, scale = FALSE))
dat_del_comp$female_c <- as.numeric(scale(dat_del_comp$`%_female`, scale = FALSE))
dat_del_comp$nos_c    <- as.numeric(scale(dat_del_comp$NOS,        scale = FALSE))

cors_mat_del <- t(sapply(Rlist_del, function(R) c(R[2,1], R[3,1], R[3,2])))
colnames(cors_mat_del) <- c("r_TrDis", "r_TrDel", "r_DisDel")

v1_del <- (1 - cors_mat_del[, "r_TrDis"]^2)^2  / (n_del_comp - 1)
v2_del <- (1 - cors_mat_del[, "r_TrDel"]^2)^2  / (n_del_comp - 1)
v3_del <- (1 - cors_mat_del[, "r_DisDel"]^2)^2 / (n_del_comp - 1)
var_mat_del <- cbind(v1_del, 0, 0, v2_del, 0, v3_del)

metareg_del_age <- meta(y = cors_mat_del,
                        v = var_mat_del,
                        x = matrix(dat_del_comp$age_c, ncol = 1))
summary(metareg_del_age)

metareg_del_female <- meta(y = cors_mat_del,
                           v = var_mat_del,
                           x = matrix(dat_del_comp$female_c, ncol = 1))
summary(metareg_del_female)

# Meta-regression Stage 1: study quality (NOS) as moderator
metareg_del_nos <- meta(y = cors_mat_del,
                        v = var_mat_del,
                        x = matrix(dat_del_comp$nos_c, ncol = 1))
summary(metareg_del_nos)

# ── PDF EXPORT ───────────────────────────────────────────────────────────────
pdf_path_del <- file.path(getwd(), "Delusions results.pdf")
pdf(pdf_path_del, width = 11, height = 8.5)

# PAGE 1: TITLE
grid.newpage()
grid.text("Delusions \u2013 Meta-SEM Results",
          gp = gpar(fontsize = 22, fontface = "bold"), y = 0.58)
grid.text(paste("Generated:", format(Sys.Date(), "%d %B %Y")),
          gp = gpar(fontsize = 12), y = 0.46)
grid.text("Trauma \u2192 Dissociation \u2192 Delusions mediation model",
          gp = gpar(fontsize = 12, fontface = "italic"), y = 0.40)

# PAGE 2: POOLED CORRELATION MATRIX
corr_disp <- round(rand_corr, 2)
corr_disp[upper.tri(corr_disp)] <- NA
diag(corr_disp)                 <- NA
corr_str <- matrix(
  ifelse(is.na(corr_disp), "\u2014", formatC(corr_disp, digits = 2, format = "f")),
  nrow = nrow(corr_disp), dimnames = dimnames(corr_disp)
)
corr_tbl <- cbind(Variable = rownames(corr_str), as.data.frame(corr_str))
grid.arrange(
  ptitle("Section 1 \u2014 Pooled Correlation Matrix (Random-Effects Model)"),
  tableGrob(corr_tbl, rows = NULL, theme = ttheme_minimal(base_size = 13)),
  ncol = 1, heights = c(0.08, 0.92)
)

# PAGE 3: PATH COEFFICIENTS & MEDIATION EFFECTS
s2_del        <- summary(stage2_del_comp)
coefs_del2    <- s2_del$coefficients
algs_del2     <- s2_del$mx.algebras
total_est_del <- algs_del2["total", "Estimate"]
ind_est_del   <- algs_del2["ind",   "Estimate"]
pct_med_del   <- if (!is.na(total_est_del) && total_est_del != 0)
                   paste0(fmt2(ind_est_del / total_est_del * 100), "%") else "\u2014"

path_df_del <- data.frame(
  Effect   = c("a  (Trauma \u2192 Dissociation)",
               "b  (Dissociation \u2192 Delusions)",
               "c  (Trauma \u2192 Delusions, direct)",
               "Indirect effect (a \u00d7 b)",
               "Total effect (c + a \u00d7 b)",
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
               "\u2014"),
  check.names = FALSE
)
grid.arrange(
  ptitle("Section 1 \u2014 Path Coefficients and Mediation Effects"),
  tableGrob(path_df_del, rows = NULL, theme = ttheme_minimal(base_size = 12)),
  ncol = 1, heights = c(0.08, 0.92)
)

# PAGE 4: SEM PATH DIAGRAM
plot(stage2_del_comp, color = "green",
     main = "Section 1 \u2014 SEM Path Diagram")

# PAGE 5: SENSITIVITY ANALYSIS TABLE
# PAGE 4 (per-study indirect effects) ──────────────────────────────────────
contrib_disp_del <- contrib_df_del
contrib_disp_del$`95% CI` <- paste0("[", fmt2(contrib_disp_del$CI_lower),
                                     ", ", fmt2(contrib_disp_del$CI_upper), "]")
contrib_disp_del$Indirect  <- fmt2(contrib_disp_del$Indirect)
contrib_disp_del <- contrib_disp_del[, c("Study", "N", "Indirect", "95% CI")]
grid.arrange(
  ptitle("Section 1 -- Per-Study Indirect Effects: Trauma -> Dissociation -> Delusions"),
  tableGrob(contrib_disp_del, rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.07, 0.93)
)

loo_disp_del <- loo_df_del
loo_disp_del[, loo_num] <- lapply(loo_disp_del[, loo_num], function(x) fmt2(x))
loo_disp_del <- loo_disp_del[, c("excluded_study","excluded_N",
                                  "a","b","c","ind","ind_lbound","ind_ubound","converged")]
colnames(loo_disp_del) <- c("Study","N","a","b","c","Indirect","CI lower","CI upper","Converged")
grid.arrange(
  ptitle("Section 2 \u2014 Leave-One-Out Sensitivity Analysis: Results Table"),
  tableGrob(loo_disp_del, rows = NULL, theme = ttheme_minimal(base_size = 9)),
  ncol = 1, heights = c(0.06, 0.94)
)

# PAGE 6: SENSITIVITY ANALYSIS PLOT
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
    title    = "Section 2 \u2014 Leave-One-Out Sensitivity Analysis",
    subtitle = "Indirect effect (a\u00d7b): Trauma \u2192 Dissociation \u2192 Delusions",
    x        = "Indirect effect estimate", y = NULL,
    caption  = paste("Dashed line = full-sample estimate.",
                     "Shaded band = full-sample 95% CI. Red line = zero.")
  ) +
  theme_minimal(base_size = 12)
print(p_loo_del)

# PAGE 7: META-REGRESSION — MEAN AGE
grid.arrange(
  ptitle("Section 3 \u2014 Meta-Regression: Mean Age as Moderator"),
  tableGrob(meta_to_df(metareg_del_age, "Mean Age (grand-mean centred)", "Delusions"),
            rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.08, 0.92)
)

# PAGE 8: META-REGRESSION — % FEMALE
grid.arrange(
  ptitle("Section 3 \u2014 Meta-Regression: % Female as Moderator"),
  tableGrob(meta_to_df(metareg_del_female, "% Female (grand-mean centred)", "Delusions"),
            rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.08, 0.92)
)

# PAGE 9: META-REGRESSION — NOS (STUDY QUALITY)
grid.arrange(
  ptitle("Section 3 -- Meta-Regression: Study Quality (NOS) as Moderator"),
  tableGrob(meta_to_df(metareg_del_nos, "NOS (grand-mean centred)", "Delusions"),
            rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.08, 0.92)
)

dev.off()
message("PDF saved: ", pdf_path_del)


# COMPOSITE PSYCHOTIC SYMPTOMS (CPS) ----

# create subset for cps ----
dat_cps <- subset(dat, psychosis_construct == "composite" & !is.na(r_XM) & !is.na(r_XY) & !is.na(r_MY) & !is.na(n))

# create sub-subset for cps & composite dissociation & composite trauma
dat_cps_comp <- subset(dat, psychosis_construct == "composite" & dissociation_domain == "composite" & trauma_type == "composite" & !is.na(r_XM) & !is.na(r_XY) & !is.na(r_MY) & !is.na(n))

# Variable names in the correlation matrices
vars_cps <- c("Trauma", "Dissociation", "CPS")

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

# Stage 1: Random-effects meta-analysis of correlation matrices ----
stage1_cps_comp <- tssem1(Rlist_cps, n_cps_comp, method = "REM", RE.type = "Diag")
summary(stage1_cps_comp)

## Average correlation matrix under a random-effects model
rand_corr <- vec2symMat(coef(stage1_cps_comp, select = "fixed"), diag = FALSE)
dimnames(rand_corr) <- list(vars_cps, vars_cps)
round(rand_corr, 2)
coef(stage1_cps_comp, select = "random")

## Proposed model in lavaan syntax
model1 <- "CPS ~ c*Trauma + b*Dissociation
Dissociation ~ a*Trauma
Trauma ~~ 1*Trauma"
plot(model1)

## Convert the lavaan syntax to RAM specification used in metaSEM
RAM1 <- lavaan2RAM(model1, obs.variables = vars_cps)
RAM1

####fit model stage2####
stage2_cps_comp <- tssem2(stage1_cps_comp,
                          RAM              = RAM1,
                          intervals.type   = "LB",
                          diag.constraints = TRUE,
                          mx.algebras      = list(ind   = mxAlgebra(a * b,     name = "ind"),
                                                  dir   = mxAlgebra(c,         name = "dir"),
                                                  total = mxAlgebra(c + a * b, name = "total")))
stage2_cps_comp <- metaSEM::rerun(stage2_cps_comp)
summary(stage2_cps_comp)

#### SEM plot ####
plot(stage2_cps_comp, color = "green")


# ── PER-STUDY INDIRECT EFFECT TABLE: CPS ───────────────────────────────────
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
      Study    = paste(trimws(dat_cps_comp$studyID[i]), dat_cps_comp$year[i]),
      N        = n_cps_comp[i],
      Indirect = NA, CI_lower = NA, CI_upper = NA)
  } else {
    algs_s <- summary(stage2_s)$mx.algebras
    study_contrib_cps[[i]] <- data.frame(
      Study    = paste(trimws(dat_cps_comp$studyID[i]), dat_cps_comp$year[i]),
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


# ── LEAVE-ONE-OUT SENSITIVITY ANALYSIS ─────────────────────────────────────
k_cps <- nrow(dat_cps_comp)
loo_results_cps <- vector("list", k_cps)

for (i in seq_len(k_cps)) {
  Rlist_loo_cps <- Rlist_cps[-i]
  n_loo_cps     <- n_cps_comp[-i]

  stage1_loo_cps <- tryCatch(
    tssem1(Rlist_loo_cps, n_loo_cps, method = "REM", RE.type = "Diag"),
    error = function(e) NULL
  )

  if (is.null(stage1_loo_cps)) {
    loo_results_cps[[i]] <- data.frame(
      excluded_study = paste(trimws(dat_cps_comp$studyID[i]), dat_cps_comp$year[i]),
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
      excluded_study = paste(trimws(dat_cps_comp$studyID[i]), dat_cps_comp$year[i]),
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
    excluded_study = paste(trimws(dat_cps_comp$studyID[i]), dat_cps_comp$year[i]),
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

# ── PLOT ────────────────────────────────────────────────────────────────────
ind_full_cps <- summary(stage2_cps_comp)$mx.algebras["ind", "Estimate"]
lb_full_cps  <- summary(stage2_cps_comp)$mx.algebras["ind", "lbound"]
ub_full_cps  <- summary(stage2_cps_comp)$mx.algebras["ind", "ubound"]

loo_df_cps$label <- paste0("Excl. study ", loo_df_cps$excluded_study,
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
    subtitle = "Indirect effect (a\u00d7b): Trauma \u2192 Dissociation \u2192 Composite Psychosis Score",
    x        = "Indirect effect estimate", y = NULL,
    caption  = "Dashed line = full-sample estimate. Shaded band = full-sample 95% CI.\nRed line = zero."
  ) +
  theme_minimal(base_size = 12)

# ── META-REGRESSION ──────────────────────────────────────────────────────────
dat_cps_comp$age_c    <- as.numeric(scale(dat_cps_comp$`mean age`, scale = FALSE))
dat_cps_comp$female_c <- as.numeric(scale(dat_cps_comp$`%_female`, scale = FALSE))
dat_cps_comp$nos_c    <- as.numeric(scale(dat_cps_comp$NOS,        scale = FALSE))

cors_mat_cps <- t(sapply(Rlist_cps, function(R) c(R[2,1], R[3,1], R[3,2])))
colnames(cors_mat_cps) <- c("r_TrDis", "r_TrCPS", "r_DisCPS")

v1_cps <- (1 - cors_mat_cps[, "r_TrDis"]^2)^2  / (n_cps_comp - 1)
v2_cps <- (1 - cors_mat_cps[, "r_TrCPS"]^2)^2  / (n_cps_comp - 1)
v3_cps <- (1 - cors_mat_cps[, "r_DisCPS"]^2)^2 / (n_cps_comp - 1)
var_mat_cps <- cbind(v1_cps, 0, 0, v2_cps, 0, v3_cps)

metareg_cps_age <- meta(y = cors_mat_cps,
                        v = var_mat_cps,
                        x = matrix(dat_cps_comp$age_c, ncol = 1))
summary(metareg_cps_age)

metareg_cps_female <- meta(y = cors_mat_cps,
                           v = var_mat_cps,
                           x = matrix(dat_cps_comp$female_c, ncol = 1))
summary(metareg_cps_female)

# Meta-regression Stage 1: study quality (NOS) as moderator
metareg_cps_nos <- meta(y = cors_mat_cps,
                        v = var_mat_cps,
                        x = matrix(dat_cps_comp$nos_c, ncol = 1))
summary(metareg_cps_nos)

# ── PDF EXPORT ───────────────────────────────────────────────────────────────
pdf_path_cps <- file.path(getwd(), "Composite Psychosis results.pdf")
pdf(pdf_path_cps, width = 11, height = 8.5)

# PAGE 1: TITLE
grid.newpage()
grid.text("Composite Psychosis Score \u2013 Meta-SEM Results",
          gp = gpar(fontsize = 22, fontface = "bold"), y = 0.58)
grid.text(paste("Generated:", format(Sys.Date(), "%d %B %Y")),
          gp = gpar(fontsize = 12), y = 0.46)
grid.text("Trauma \u2192 Dissociation \u2192 Composite Psychosis Score mediation model",
          gp = gpar(fontsize = 12, fontface = "italic"), y = 0.40)

# PAGE 2: POOLED CORRELATION MATRIX
corr_disp <- round(rand_corr, 2)
corr_disp[upper.tri(corr_disp)] <- NA
diag(corr_disp)                 <- NA
corr_str <- matrix(
  ifelse(is.na(corr_disp), "\u2014", formatC(corr_disp, digits = 2, format = "f")),
  nrow = nrow(corr_disp), dimnames = dimnames(corr_disp)
)
corr_tbl <- cbind(Variable = rownames(corr_str), as.data.frame(corr_str))
grid.arrange(
  ptitle("Section 1 \u2014 Pooled Correlation Matrix (Random-Effects Model)"),
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
                   paste0(fmt2(ind_est_cps / total_est_cps * 100), "%") else "\u2014"

path_df_cps <- data.frame(
  Effect   = c("a  (Trauma \u2192 Dissociation)",
               "b  (Dissociation \u2192 Composite Psychosis Score)",
               "c  (Trauma \u2192 Composite Psychosis Score, direct)",
               "Indirect effect (a \u00d7 b)",
               "Total effect (c + a \u00d7 b)",
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
               "\u2014"),
  check.names = FALSE
)
grid.arrange(
  ptitle("Section 1 \u2014 Path Coefficients and Mediation Effects"),
  tableGrob(path_df_cps, rows = NULL, theme = ttheme_minimal(base_size = 12)),
  ncol = 1, heights = c(0.08, 0.92)
)

# PAGE 4: SEM PATH DIAGRAM
plot(stage2_cps_comp, color = "green",
     main = "Section 1 \u2014 SEM Path Diagram")

# PAGE 5: SENSITIVITY ANALYSIS TABLE
# PAGE 4 (per-study indirect effects) ──────────────────────────────────────
contrib_disp_cps <- contrib_df_cps
contrib_disp_cps$`95% CI` <- paste0("[", fmt2(contrib_disp_cps$CI_lower),
                                     ", ", fmt2(contrib_disp_cps$CI_upper), "]")
contrib_disp_cps$Indirect  <- fmt2(contrib_disp_cps$Indirect)
contrib_disp_cps <- contrib_disp_cps[, c("Study", "N", "Indirect", "95% CI")]
grid.arrange(
  ptitle("Section 1 -- Per-Study Indirect Effects: Trauma -> Dissociation -> CPS"),
  tableGrob(contrib_disp_cps, rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.07, 0.93)
)

loo_disp_cps <- loo_df_cps
loo_disp_cps[, loo_num] <- lapply(loo_disp_cps[, loo_num], function(x) fmt2(x))
loo_disp_cps <- loo_disp_cps[, c("excluded_study","excluded_N",
                                  "a","b","c","ind","ind_lbound","ind_ubound","converged")]
colnames(loo_disp_cps) <- c("Study","N","a","b","c","Indirect","CI lower","CI upper","Converged")
grid.arrange(
  ptitle("Section 2 \u2014 Leave-One-Out Sensitivity Analysis: Results Table"),
  tableGrob(loo_disp_cps, rows = NULL, theme = ttheme_minimal(base_size = 9)),
  ncol = 1, heights = c(0.06, 0.94)
)

# PAGE 6: SENSITIVITY ANALYSIS PLOT
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
    title    = "Section 2 \u2014 Leave-One-Out Sensitivity Analysis",
    subtitle = "Indirect effect (a\u00d7b): Trauma \u2192 Dissociation \u2192 Composite Psychosis Score",
    x        = "Indirect effect estimate", y = NULL,
    caption  = paste("Dashed line = full-sample estimate.",
                     "Shaded band = full-sample 95% CI. Red line = zero.")
  ) +
  theme_minimal(base_size = 12)
print(p_loo_cps)

# PAGE 7: META-REGRESSION — MEAN AGE
grid.arrange(
  ptitle("Section 3 \u2014 Meta-Regression: Mean Age as Moderator"),
  tableGrob(meta_to_df(metareg_cps_age, "Mean Age (grand-mean centred)", "CPS"),
            rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.08, 0.92)
)

# PAGE 8: META-REGRESSION — % FEMALE
grid.arrange(
  ptitle("Section 3 \u2014 Meta-Regression: % Female as Moderator"),
  tableGrob(meta_to_df(metareg_cps_female, "% Female (grand-mean centred)", "CPS"),
            rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.08, 0.92)
)

# PAGE 9: META-REGRESSION — NOS (STUDY QUALITY)
grid.arrange(
  ptitle("Section 3 -- Meta-Regression: Study Quality (NOS) as Moderator"),
  tableGrob(meta_to_df(metareg_cps_nos, "NOS (grand-mean centred)", "CPS"),
            rows = NULL, theme = ttheme_minimal(base_size = 11)),
  ncol = 1, heights = c(0.08, 0.92)
)

dev.off()
message("PDF saved: ", pdf_path_cps)


