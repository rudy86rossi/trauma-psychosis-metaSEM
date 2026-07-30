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
for (f in c("lib_provenance", "lib_config", "lib_data", "lib_scales_check",
            "01_data_lock"))
  if (file.exists(file.path(ROOT, "bayesian", "R", paste0(f, ".R"))))
    source(file.path(ROOT, "bayesian", "R", paste0(f, ".R")))

CFG <- load_config(file.path(ROOT, "config", "analysis.yml"))

## The data lock is expensive, so build it once for the whole suite.
LOCK <- local({
  old <- getwd(); setwd(ROOT); on.exit(setwd(old))
  build_data_lock(CFG, ROOT)
})
