# Regression gate for the NBR revision release (25 November 2026).
#
# Compares every fit in a new targets store with the archived abc9305d0d01
# store, keyed on scientific identifiers (fit_id plus the row's quantity
# keys), never on release IDs or metadata. Each row is classified in advance:
#
#   identical  negsch_cps, negsch_del, depress_cps: inputs unchanged, seeds
#              depend only on cfg$seed and fit_id, so values must match exactly
#   changed    dissoc_hal, dissoc_del: Cole, Perona and Pearce corrections
#   removed    dissoc_cps (below the synthesis minimum) and sens_alt_instrument
#              (its only case, Blose, belonged to dissoc_cps)
#   new        sens_direct_r (the protocol-specified conversion sensitivity)
#
# Exact equality is the expected result under an unchanged environment. A
# difference in an "identical" row is a finding to diagnose, not a tolerance to
# tune. Usage, run from the analysis repository:
#   Rscript regression_gate.R OLD_STORE NEW_STORE OUT_DIR

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3)
old_store <- args[1]; new_store <- args[2]; out <- args[3]
dir.create(out, showWarnings = FALSE, recursive = TRUE)
suppressPackageStartupMessages(library(targets))

R0 <- tar_read(results, store = old_store)
R1 <- tar_read(results, store = new_store)

KEYS <- list(
  pooled        = c("fit_id", "quantity", "contrast_index", "moderator_value"),
  predict       = c("fit_id", "quantity", "contrast_index", "moderator_value"),
  heterogeneity = c("fit_id", "coordinate", "quantity"),
  slopes        = c("fit_id", "coordinate", "estimand"),
  contrasts     = c("fit_id", "quantity", "contrast"),
  pmed          = c("fit_id", "quantity"),
  ppc_overall   = c("pathway", "moderator"),
  ppc_study     = c("pathway", "moderator", "sample_id"),
  prior_predictive = c("pathway", "quantity"),
  pmed_draws    = c("pathway", ".chain", ".draw"))
## Labels and release metadata, never compared.
META <- c("variant", "label", "parent_reportable", "k_samples")

expect_class <- function(d) {
  fc <- if ("fit_class" %in% names(d)) d$fit_class else rep("", nrow(d))
  ifelse(d$pathway == "dissoc_cps" | fc == "sens_alt_instrument", "removed",
  ifelse(fc == "sens_direct_r", "new",
  ifelse(d$pathway %in% c("dissoc_hal", "dissoc_del"), "changed", "identical")))
}

summ <- list(); detail <- list()
for (nm in names(KEYS)) {
  a <- R0[[nm]]; b <- R1[[nm]]
  if (is.null(a) || is.null(b)) { message("skip ", nm, ": missing in one store"); next }
  k <- KEYS[[nm]]
  key <- function(d) do.call(paste, c(lapply(k, function(x) as.character(d[[x]])), sep = "|"))
  a$.key <- key(a); b$.key <- key(b)
  stopifnot(!anyDuplicated(a$.key), !anyDuplicated(b$.key))
  vals <- setdiff(intersect(names(a), names(b)), c(k, META, ".key", "fit_class", "pathway",
                                                    "moderator", "drop_sample"))
  m <- merge(a, b, by = ".key", all = TRUE, suffixes = c(".old", ".new"))
  pw <- ifelse(is.na(m$pathway.old), m$pathway.new, m$pathway.old)
  fc <- if ("fit_class.old" %in% names(m))
    ifelse(is.na(m$fit_class.old), m$fit_class.new, m$fit_class.old) else rep("", nrow(m))
  m$pathway <- pw; m$fit_class <- fc
  m$expect <- expect_class(m)
  in_old <- m$.key %in% a$.key; in_new <- m$.key %in% b$.key
  same <- rep(TRUE, nrow(m))
  for (v in vals) {
    x <- m[[paste0(v, ".old")]]; y <- m[[paste0(v, ".new")]]
    eq <- (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
    same <- same & eq
  }
  m$status <- ifelse(in_old & in_new, ifelse(same, "equal", "different"),
                     ifelse(in_old, "only_old", "only_new"))
  m$frame <- nm
  m$verdict <- with(m, ifelse(
    (expect == "identical" & status == "equal") |
    (expect == "changed" & status %in% c("equal", "different")) |
    (expect == "removed" & status == "only_old") |
    (expect == "new" & status == "only_new"), "as expected", "UNEXPECTED"))
  s <- aggregate(list(rows = m$.key), by = list(frame = m$frame, pathway = m$pathway,
                 expect = m$expect, status = m$status, verdict = m$verdict), FUN = length)
  summ[[nm]] <- s
  bad <- m[m$verdict == "UNEXPECTED", c("frame", ".key", "pathway", "fit_class", "expect", "status")]
  if (nrow(bad)) detail[[nm]] <- bad
  ## Magnitude of change for the expected-change pathways, for the record.
  if (nm %in% c("pooled", "pmed") && "median.old" %in% names(m)) {
    ch <- m[m$expect == "changed" & m$status == "different",
            c(".key", "pathway", "fit_class", "median.old", "median.new")]
    write.csv(ch, file.path(out, paste0("changed_", nm, ".csv")), row.names = FALSE)
  }
}
S <- do.call(rbind, summ)
write.csv(S, file.path(out, "gate_summary.csv"), row.names = FALSE)
D <- if (length(detail)) do.call(rbind, detail) else data.frame()
write.csv(D, file.path(out, "gate_unexpected.csv"), row.names = FALSE)
print(S[order(S$frame, S$pathway, S$status), ], row.names = FALSE)
cat("\nUNEXPECTED rows:", nrow(D), "\n")
if (nrow(D)) { print(head(D, 30), row.names = FALSE); quit(status = 1) }
