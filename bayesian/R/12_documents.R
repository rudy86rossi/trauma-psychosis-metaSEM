# =============================================================================
# 12_documents.R
# Builds BAYESIAN_METAREG_RESULTS.md and BAYESIAN_METAREG_METHODS.md from a
# promoted run, then verifies that no number in either document is absent from
# its source table.
#
# The verification step is the point. In the previous version of this analysis
# the documents were written by hand and several figures disagreed with the
# tables they claimed to summarise. Generating them removes the opportunity; the
# check below confirms it was removed rather than assuming so.
# =============================================================================

build_documents <- function(cfg, lock, run_dir, out_dir = "bayesian") {
  R <- read_run(run_dir)
  res <- write_results_doc(R, cfg, lock, file.path(out_dir, "BAYESIAN_METAREG_RESULTS.md"))
  met <- write_methods_doc(R, cfg, lock, file.path(out_dir, "BAYESIAN_METAREG_METHODS.md"))
  check_documents(c(res, met), R, cfg)
  c(res, met)
}

#' House style and provenance checks on the generated documents.
#'
#' Fails on: em dashes (the project style forbids them), US spellings, and any
#' three-decimal number in the prose that appears in no source table. The last
#' is the one that matters: it is what a hand-edited figure would trip.
check_documents <- function(paths, R, cfg, tol = 5e-4) {
  problems <- character(0)

  ## Pool every numeric value the run produced, at the precision the documents
  ## print, so a generated figure is always found and an invented one is not.
  pool <- unlist(lapply(R, function(tb) {
    if (is.null(tb) || !is.data.frame(tb)) return(numeric(0))
    unlist(lapply(tb, function(col) if (is.numeric(col)) col else numeric(0)))
  }), use.names = FALSE)
  pool <- pool[is.finite(pool)]
  ## Values are printed in several derived forms; admit each.
  pool <- unique(c(pool, 100 * pool, pool * 2, round(pool)))

  for (p in paths) {
    txt <- readLines(p, warn = FALSE)
    nm  <- basename(p)

    if (any(grepl("—", txt)))
      problems <- c(problems, sprintf("%s: contains an em dash (project style forbids them) on line(s) %s",
                                      nm, paste(which(grepl("—", txt)), collapse = ", ")))

    us <- c("analyze", "analyzed", "behavior", "modeling", "modeled", "labeled",
            "randomization", "generalization", "center", "centered", "favor")
    hits <- us[vapply(us, function(w) any(grepl(paste0("\\b", w, "\\b"), txt, ignore.case = TRUE)),
                      logical(1))]
    if (length(hits))
      problems <- c(problems, sprintf("%s: US spelling(s) %s", nm, paste(hits, collapse = ", ")))

    ## Every printed number to three decimals must exist in some table.
    ##
    ## Citations are excluded: an identifier is not a result. arXiv:1804.06788,
    ## a DOI and a volume-page range all contain digit groups that look like
    ## decimals, and treating them as unsourced values would make the check cry
    ## wolf until it was ignored.
    body <- txt[!grepl("^\\s*(#|```|\\||\\s*$)", txt)]
    stop_at <- grep("^#+\\s*References", txt)
    if (length(stop_at)) body <- txt[seq_len(stop_at[1] - 1)]
    body <- body[!grepl("^\\s*(#|```|\\||\\s*$)", body)]
    body <- body[!grepl("arXiv|doi:|https?://|10\\.[0-9]{4}", body, ignore.case = TRUE)]
    nums <- unique(as.numeric(unlist(regmatches(
      body, gregexpr("-?[0-9]+\\.[0-9]{3}(?![0-9])", body, perl = TRUE)))))
    nums <- nums[is.finite(nums)]
    orphan <- nums[!vapply(nums, function(v) any(abs(pool - v) < tol), logical(1))]
    if (length(orphan))
      problems <- c(problems, sprintf("%s: %d printed value(s) not found in any table: %s",
                                      nm, length(orphan),
                                      paste(utils::head(orphan, 8), collapse = ", ")))
  }

  if (length(problems))
    stop("Document verification failed:\n  ", paste(problems, collapse = "\n  "))
  data.frame(documents = length(paths), checked = TRUE, stringsAsFactors = FALSE)
}
