# Project profile.
#
# renv.lock is a committed project INPUT: restore from it, never re-snapshot
# during an analysis run. CmdStan is not an R package and is installed
# separately at the pinned version. See bayesian/README.md.
source("renv/activate.R")

options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  warnPartialMatchArgs = TRUE
)
Sys.setenv(CMDSTANR_NO_VER_CHECK = "TRUE")
