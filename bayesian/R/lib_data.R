# =============================================================================
# lib_data.R
# The data lock: upstream workbooks, plus the committed ledger and quality
# scores, become one canonical analysis table with a reproducible hash.
#
# The analytical unit is one participant cohort, as asserted in the curated
# sample map. A cohort contributes at most one correlation triplet to a pathway.
# Where a cohort supplied more than one eligible instrument, one is chosen by the
# configured priority and the other is retained as a REPLACEMENT sensitivity
# analysis; instruments are never averaged, because an average describes no
# defined construct and conceals the dependence induced by shared participants.
#
# `paste(studyID, year)` is a REPORT LABEL, not an identifier. It survives as
# `report_key`, for effect_id and for messages, and it decides nothing. Identity
# comes from data/provenance/sample_map.csv, which maps each source workbook row
# to a cohort, and it is applied before instrument selection so that a collision
# the curator has not accounted for is still visible to the gates. See the
# curated-sample-identity section of lib_provenance.R for why the previous
# ordering made both structural gates unreachable.
# =============================================================================

suppressPackageStartupMessages({
  library(metafor)
  library(metaSEM)
})

## ---- exclusions ------------------------------------------------------------
## Permanent decisions from the source audit. Both are recorded in
## data/provenance/exclusions.csv; this is the machine-readable form the build
## enforces, and an out-of-range correlation NOT listed here is fatal.

EXCLUSIONS <- data.frame(
  pathway = c("dissoc_cps", "dissoc_cps"),
  studyID = c("Evans", "Uyan,"),
  year    = c(2015L, 2022L),
  status  = c("excluded_permanent", "pending_source_query"),
  reason  = c(paste("Table 3 reports unstandardised regression coefficients with a binary",
                    "clinical-group outcome, not the zero-order correlation triplet the model",
                    "requires. The estimand is incommensurate; reinstatement needs",
                    "participant-level data or a complete correlation matrix."),
              paste("The extracted row holds Figure 2 case-control mediation coefficients with a",
                    "binary schizophrenia-status outcome across N = 200. The patient-only sample",
                    "(N = 100) reports r_MY = 0.106 and r_XY = 0.193 in Table 3, but r_XM is not",
                    "numerically reported. Reinstatable if the CTQ-to-DES correlation is obtained.")),
  stringsAsFactors = FALSE)

## ---- column mappings -------------------------------------------------------

COLMAP <- list(
  dissociation = list(id = "studyID", year = "year", age = "mean age",
                      female = "%_female", nos = "NOS", sample = "sample",
                      instrument = "dissociation_instrument"),
  negative_schema = list(id = "study_author", year = "year", age = "mean_age",
                         female = "%_female", nos = "NOS", sample = "clinical_sample",
                         instrument = "negative_schemas_instrument"),
  depression = list(id = "Authors, et al.", year = "year", age = "mean age",
                    female = "%_female", nos = "NOS",
                    sample = "clinicalsample (GP 1, CS 2)", instrument = NULL)
)

PATH_FILTER <- list(
  dissoc_hal  = quote(psychosis_construct == "hallucinations" & trauma_type == "composite" &
                        dissociation_domain == "composite"),
  dissoc_del  = quote(psychosis_construct == "delusions" & dissociation_domain == "composite"),
  dissoc_cps  = quote(psychosis_construct == "composite" & dissociation_domain == "composite" &
                        trauma_type == "composite"),
  negsch_cps  = quote(included_in_metaanalysis == 1),
  negsch_del  = quote(included_in_metaanalysis == 2),
  depress_cps = quote(psychosis_construct == "composite" & `depression domain` == "composite")
)

## ---- helpers ---------------------------------------------------------------

as_num <- function(d, cols) {
  for (cn in intersect(cols, names(d))) d[[cn]] <- suppressWarnings(as.numeric(d[[cn]]))
  d
}

complete_rows <- function(d)
  !is.na(d$r_XM) & !is.na(d$r_XY) & !is.na(d$r_MY) & !is.na(d$n)

make_R <- function(r_xm, r_xy, r_my, vars = c("X", "M", "Y")) {
  R <- matrix(c(1, r_xm, r_xy, r_xm, 1, r_my, r_xy, r_my, 1), nrow = 3, byrow = TRUE)
  dimnames(R) <- list(vars, vars)
  R
}

min_eigen <- function(R) min(eigen(R, symmetric = TRUE, only.values = TRUE)$values)

instrument_rank <- function(x, priority) {
  r <- match(trimws(x), priority)
  ifelse(is.na(r), length(priority) + 1L, r)
}

## ---- step 1: locked source sheets -----------------------------------------

#' Read every upstream workbook, verify its hash, and apply the ledger.
lock_sources <- function(cfg, root = ".",
                         corrections_path = "data/provenance/corrections.csv",
                         quality_path     = "data/provenance/quality_scores.csv") {
  ledger <- read.csv(file.path(root, corrections_path), stringsAsFactors = FALSE,
                     colClasses = "character")
  ledger$row_index <- as.integer(ledger$row_index)
  qs <- read.csv(file.path(root, quality_path), stringsAsFactors = FALSE)

  out <- list(); hashes <- list()
  for (nm in names(cfg$inputs)) {
    spec <- cfg$inputs[[nm]]
    path <- file.path(root, spec$path)
    if (!file.exists(path)) stop("Locked input missing: ", spec$path)
    sha <- sha256_file(path)
    if (!identical(sha, spec$sha256))
      stop("Input hash mismatch for ", spec$path, ".\n  expected ", spec$sha256,
           "\n  found    ", sha, "\nThe analysis is defined against the upstream file.")
    hashes[[nm]] <- sha

    sheet <- if (is.character(spec$sheet)) spec$sheet else as.integer(spec$sheet)
    d <- read_sheet(path, sheet)
    d <- apply_corrections(d, ledger, basename(spec$path), sheet, sha)
    cm <- COLMAP[[nm]]
    d <- apply_quality_scores(d, qs, basename(spec$path), cm$id, cm$year, cm$nos)
    out[[nm]] <- d
  }
  list(sheets = out, hashes = hashes, ledger = ledger, quality = qs)
}

## ---- step 2: pathway extraction -------------------------------------------

build_pathway <- function(cfg, key, sheets) {
  p    <- cfg$pathways[[key]]
  src  <- p$source
  cm   <- COLMAP[[src]]
  spec <- cfg$inputs[[src]]
  book <- basename(spec$path)
  shet <- as.character(spec$sheet)

  ## The fingerprint is taken from the locked sheet as read, before as_num()
  ## coerces types, so it is a statement about the source row rather than about
  ## this function's parsing of it.
  locked <- sheets[[src]]
  raw <- as_num(locked, c("n", cm$age, cm$female, cm$nos, "r_XM", "r_MY", "r_XY"))

  keep <- eval(PATH_FILTER[[key]], raw, parent.frame()) & complete_rows(raw)
  keep[is.na(keep)] <- FALSE
  idx <- which(keep)
  d <- raw[idx, , drop = FALSE]

  data.frame(
    pathway    = key,
    label      = p$label,
    ## Where this row came from. A cohort is identified by position in its
    ## source workbook, because two cohorts can agree on every semantic field.
    source_workbook = book,
    source_sheet    = shet,
    source_row      = idx,
    source_effect_id = source_effect_id(book, shet, idx, key),
    row_fingerprint  = vapply(idx, function(i) row_fingerprint(locked, i), character(1)),
    studyID    = trimws(as.character(d[[cm$id]])),
    year       = suppressWarnings(as.integer(d[[cm$year]])),
    n          = as.numeric(d$n),
    r_XM       = as.numeric(d$r_XM),
    r_XY       = as.numeric(d$r_XY),
    r_MY       = as.numeric(d$r_MY),
    age        = as.numeric(d[[cm$age]]),
    female     = as.numeric(d[[cm$female]]),
    nos        = as.numeric(d[[cm$nos]]),
    clinical   = ifelse(is.na(d[[cm$sample]]), NA_real_,
                        as.numeric(trimws(as.character(d[[cm$sample]])) %in%
                                     c("Clinical", "CS", "2"))),
    instrument = if (!is.null(cm$instrument) && cm$instrument %in% names(d))
      trimws(as.character(d[[cm$instrument]])) else NA_character_,
    stringsAsFactors = FALSE)
}

## ---- step 3: exclusions, sample identity, instrument choice ---------------

apply_registry <- function(cfg, df, smap) {
  key <- df$pathway[1]

  ## Exclusions, by explicit decision.
  ex <- EXCLUSIONS[EXCLUSIONS$pathway == key, , drop = FALSE]
  df$excluded <- FALSE; df$exclusion_reason <- NA_character_; df$exclusion_status <- NA_character_
  if (nrow(ex)) {
    hit <- match(paste(df$studyID, df$year), paste(ex$studyID, ex$year))
    df$excluded[!is.na(hit)]         <- TRUE
    df$exclusion_reason[!is.na(hit)] <- ex$reason[hit[!is.na(hit)]]
    df$exclusion_status[!is.na(hit)] <- ex$status[hit[!is.na(hit)]]
  }

  ## Any impossible matrix that is NOT already an explicit exclusion is fatal.
  ## asyCov() returns finite numbers for a non-positive-definite input rather
  ## than failing, which is how the published frequentist run produced output on
  ## these rows without complaint.
  live <- !df$excluded
  rng  <- live & (abs(df$r_XM) > 1 | abs(df$r_XY) > 1 | abs(df$r_MY) > 1)
  if (any(rng))
    stop(key, ": correlation outside [-1, 1] for ",
         paste(paste(df$studyID[rng], df$year[rng]), collapse = ", "),
         ". Add to EXCLUSIONS with a documented reason before proceeding.")
  df$min_eigen <- vapply(seq_len(nrow(df)), function(i)
    min_eigen(make_R(df$r_XM[i], df$r_XY[i], df$r_MY[i])), numeric(1))
  npd <- live & df$min_eigen <= 0
  if (any(npd))
    stop(key, ": non-positive-definite correlation matrix for ",
         paste(paste(df$studyID[npd], df$year[npd]), collapse = ", "),
         ". Add to EXCLUSIONS with a documented reason before proceeding.")

  ## Sample identity, BEFORE any row is demoted. `paste(studyID, year)` is a
  ## label for a report and decides nothing; the curated map says which cohort
  ## this row's participants are, and it is applied here so that an undeclared
  ## collision survives to be caught by gate_sample_identity() rather than being
  ## resolved out of existence by instrument priority.
  df$report_key <- paste(df$studyID, df$year)
  df <- apply_sample_map(df, smap)

  ## One triplet per cohort. Demotion happens ONLY inside a group the curator
  ## has declared to be one cohort measured with more than one instrument. That
  ## declaration is what licenses it; two rows that merely collide are left
  ## alone, which is what makes the collision gate reachable.
  live <- !df$excluded
  df$analysis_role <- ifelse(df$excluded, "excluded", "primary")
  df$analysis_role[live & df$multiplicity == "superseded_report"] <- "duplicate_report"

  pri <- unlist(cfg$instrument_priority)
  for (sid in unique(df$sample_id[live])) {
    idx <- which(live & df$sample_id == sid & df$multiplicity == "alternative_instrument")
    if (length(idx) <= 1) next
    rk <- instrument_rank(df$instrument[idx], pri)
    ## which.min() takes the first on a tie, which makes the primary triplet
    ## depend on workbook row order. Two unranked instruments are a gap in the
    ## configuration, not something to settle by position.
    if (sum(rk == min(rk)) > 1L)
      stop(key, ": instrument priority does not separate ", sid, " (",
           paste(df$instrument[idx][rk == min(rk)], collapse = ", "),
           "). Extend instrument_priority in config/analysis.yml.")
    df$analysis_role[setdiff(idx, idx[which.min(rk)])] <- "sensitivity_alternative_instrument"
    message("  ", key, ": ", sid, " supplied ", length(idx), " declared instruments; ",
            df$instrument[idx][which.min(rk)], " is primary.")
  }

  ## The effect identifier keeps its historical form, so every identifier already
  ## in the canonical table and in every LOSO fit_id keeps its value. Its
  ## sanitisation is lossy, and a curated split within one pathway and instrument
  ## produces two rows that reduce to one string, so the cohort is appended where
  ## and ONLY where that would otherwise collide. No row in the current release
  ## collides, so nothing is renamed.
  df$effect_id <- sprintf("%s:%s:%s", df$pathway, gsub("[^A-Za-z0-9]", "", df$report_key),
                          ifelse(is.na(df$instrument), "NA",
                                 gsub("[^A-Za-z0-9]", "", df$instrument)))
  clash <- df$effect_id %in% df$effect_id[duplicated(df$effect_id)]
  df$effect_id[clash] <- paste0(df$effect_id[clash], ":", df$sample_id[clash])
  df
}

## ---- step 4: sampling covariance ------------------------------------------

#' Within-study sampling covariance on the Fisher-z scale.
#'
#' Primary construction is metafor::rcalc(rtoz = TRUE). On the z scale its
#' diagonal is 1/(n-3), which does not depend on the observed correlation, so
#' the coupling between an estimate and its own sampling variance disappears.
#' asyCov()'s default acov = "weighted" borrows a weighted average correlation
#' matrix across studies; since the Bayesian hierarchy already pools, using it
#' would pool twice. It is retained as a sensitivity construction only.
vz_rcalc <- function(df, pairs = c("r_XM", "r_XY", "r_MY")) {
  k <- nrow(df)
  long <- data.frame(
    study = rep(seq_len(k), each = 3),
    var1  = rep(c("X", "X", "M"), k),
    var2  = rep(c("M", "Y", "Y"), k),
    ri    = as.vector(t(as.matrix(df[, c("r_XM", "r_XY", "r_MY")]))),
    ni    = rep(df$n, each = 3))
  V <- as.matrix(metafor::rcalc(ri ~ var1 + var2 | study, ni = ni,
                                data = long, rtoz = TRUE)$V)
  lapply(seq_len(k), function(i) {
    idx <- (3 * (i - 1) + 1):(3 * i)
    m <- V[idx, idx, drop = FALSE]
    dimnames(m) <- list(pairs, pairs)
    m
  })
}

vz_asycov <- function(df, acov = c("individual", "weighted"),
                      pairs = c("r_XM", "r_XY", "r_MY")) {
  acov <- match.arg(acov)
  Rl <- lapply(seq_len(nrow(df)), function(i)
    make_R(df$r_XM[i], df$r_XY[i], df$r_MY[i]))
  A <- metaSEM::asyCov(Rl, df$n, acov = acov)
  lapply(seq_len(nrow(df)), function(i) {
    ## vech to full symmetric, then delta method to the z scale. This puts
    ## 1/(n-1) on the z diagonal where the standard Fisher variance is 1/(n-3),
    ## which is the expected and characterised difference from rcalc.
    Vr <- matrix(0, 3, 3)
    Vr[lower.tri(Vr, diag = TRUE)] <- A[i, ]
    Vr <- Vr + t(Vr) - diag(diag(Vr))
    r  <- c(df$r_XM[i], df$r_XY[i], df$r_MY[i])
    D  <- diag(1 / (1 - r^2))
    m  <- D %*% Vr %*% D
    dimnames(m) <- list(pairs, pairs)
    m
  })
}

chol_list <- function(Vs, labels) {
  lapply(seq_along(Vs), function(i) {
    V <- Vs[[i]]
    ev <- min(eigen(V, symmetric = TRUE, only.values = TRUE)$values)
    if (ev <= 0)
      stop("Sampling covariance is not positive definite for ", labels[i],
           " (minimum eigenvalue ", signif(ev, 3), ").")
    t(chol(V))
  })
}
