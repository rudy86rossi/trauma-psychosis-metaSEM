# Shared setup for the test suite. Locates the repository root from wherever
# testthat was invoked, then sources the libraries once.

repo_root <- function() {
  d <- normalizePath(getwd())
  while (!dir.exists(file.path(d, "metaSEM")) && dirname(d) != d) d <- dirname(d)
  if (!dir.exists(file.path(d, "metaSEM")))
    stop("Cannot locate the repository root from ", getwd())
  d
}

ROOT <- repo_root()
suppressPackageStartupMessages(library(digest))
## lib_prior_checks, lib_writeup, lib_writeup_methods and 12_documents were
## absent, which is why the power-scaling miscount and the document verifier
## reached a release with no test able to see them.
LIBS <- c("lib_provenance", "lib_config", "lib_data", "lib_summaries", "lib_stan",
          "lib_brms", "lib_ppc", "lib_sbc", "lib_prior_checks", "lib_fitplan",
          "lib_frequentist", "lib_report", "lib_figures",
          "lib_writeup", "lib_writeup_methods", "12_documents", "01_data_lock")
for (f in LIBS) source(file.path(ROOT, "bayesian", "R", paste0(f, ".R")))

CFG <- load_config(file.path(ROOT, "config", "analysis.yml"))

## The data lock is expensive, so build it once for the whole suite.
LOCK <- local({
  old <- getwd(); setwd(ROOT); on.exit(setwd(old))
  build_data_lock(CFG, ROOT)
})
