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
  check_documents(c(res, met), R, cfg, lock = lock)
  c(res, met)
}

## Assertions about the independence of the analytical unit.
##
## A FAMILY of patterns, not a list of phrases. The four literal strings this
## replaces did not contain the sentence the methods document actually printed,
## "The analytical unit was one independent participant sample", so the check
## passed while the document made the strongest available claim with no
## qualification at all. A longer list of literals has the same failure mode.
INDEPENDENCE_ASSERTIONS <- c(
  "\\bindependent (participant |cohort )?samples?\\b",
  "\\bindependent cohorts?\\b",
  "\\banalytical unit\\b",
  "\\bnon-?overlapping (samples|cohorts|participants)\\b",
  "\\bno (participant |sample )?overlap\\b",
  "\\bdistinct (samples|cohorts|participants)\\b",
  "\\b(each|every) sample (is|was) independent\\b",
  "\\bverified (independent |analytical |sample |cohort )",
  "\\bindependently verified\\b",
  "\\bcurated (sample )?registry\\b",
  "\\b(identity|identities) (was|were|is|are) (verified|confirmed)\\b",
  "\\bconfirmed (independent|identity|cohort)")

## Any one of these, in the same sentence or the one that follows, discharges an
## assertion. They are the vocabulary in which the limitation is admissible.
INDEPENDENCE_QUALIFIERS <- c(
  "derived_unverified", "\\bunevidenced\\b", "\\bunverified\\b", "\\bheuristic\\b",
  "\\bnot .{0,40}verified\\b", "\\bhas not been (externally )?verified\\b",
  "\\bderived from (the )?author and year\\b",
  "\\bawaiting .{0,25}(verification|sign-?off)\\b",
  "\\bcurated in `data/provenance/sample_map\\.csv`")

#' House style and provenance checks on the generated documents.
#'
#' Fails on: em dashes (the project style forbids them), US spellings, any
#' three-decimal number in the prose that appears in no source table, an
#' unqualified assertion about sample independence while any identity is
#' unevidenced, and absence of the generated identity caveat.
check_documents <- function(paths, R, cfg, tol = 5e-4, lock = NULL) {
  problems <- character(0)

  ## The analytical unit may not be described as independent, without
  ## qualification, while any report-to-cohort assertion is still unevidenced.
  ## Identity rests on the author and year printed in the workbook until someone
  ## with the papers says otherwise, and that limitation must survive every
  ## rewrite of the prose, so it is enforced here rather than remembered.
  have_lock  <- !is.null(lock) && !is.null(lock$samples) && !is.null(lock$sample_map)
  unverified <- if (have_lock) sum(lock$sample_map$evidence_status != "evidenced") else NA_integer_
  caveat     <- if (have_lock) identity_caveat(lock$samples, lock$sample_map, lock$effects) else NA_character_
  verified_claims <- c("verified independent sample", "verified analytical unit",
                       "curated sample registry", "independently verified sample",
                       "confirmed independent", "verified cohort",
                       "no participant overlap", "non-overlapping samples")

  ## Pool every numeric value the run produced, at the precision the documents
  ## print, so a generated figure is always found and an invented one is not.
  pool <- unlist(lapply(R, function(tb) {
    if (is.null(tb) || !is.data.frame(tb)) return(numeric(0))
    unlist(lapply(tb, function(col) if (is.numeric(col)) col else numeric(0)))
  }), use.names = FALSE)
  ## as.numeric() so an empty run gives numeric(0) rather than NULL, which
  ## round() rejects. A document check must not fall over on the case where
  ## there is nothing to check against.
  pool <- as.numeric(pool)
  pool <- pool[is.finite(pool)]
  ## Values are printed in several derived forms; admit each.
  pool <- unique(c(pool, 100 * pool, pool * 2, round(pool)))

  for (p in paths) {
    txt <- readLines(p, warn = FALSE)
    nm  <- basename(p)

    if (any(grepl("—", txt)))
      problems <- c(problems, sprintf("%s: contains an em dash (project style forbids them) on line(s) %s",
                                      nm, paste(which(grepl("—", txt)), collapse = ", ")))

    ## The required disclosure. A checker that only forbids phrasings passes a
    ## document that says nothing at all, so the caveat must be present and its
    ## counts must be the ones the registry currently supports.
    if (!is.na(caveat) && !any(vapply(txt, function(l) grepl(caveat, l, fixed = TRUE),
                                      logical(1))))
      problems <- c(problems, sprintf(
        "%s: the generated sample-identity caveat is absent or has been edited. It must read, verbatim:\n    %s",
        nm, caveat))

    if (!is.na(unverified) && unverified > 0) {
      ## Layer 3, the literal backstop.
      claimed <- verified_claims[vapply(verified_claims, function(w)
        any(grepl(w, txt, ignore.case = TRUE)), logical(1))]
      if (length(claimed))
        problems <- c(problems, sprintf(
          paste("%s: claims %s while %d of %d curated identities are not",
                "evidenced (unevidenced or disputed). Sample identity rests on",
                "the author and year printed in the workbook for those rows and",
                "has not been externally verified."),
          nm, paste(sQuote(claimed), collapse = ", "), unverified,
          nrow(lock$sample_map)))

      ## Layer 2, the proximity rule. Prose only: headings, tables, code fences
      ## and the references section are excluded for the same reason as below.
      ##
      ## The caveat is removed from the text before the scan, not filtered out
      ## afterwards. It is multi-sentence, so filtering by equality left its
      ## parts in place, and since it is itself a qualifier it then discharged
      ## whatever unqualified assertion happened to precede it. The caveat is a
      ## separate required disclosure; it does not license a claim made
      ## elsewhere in the document.
      prose <- txt[!grepl("^\\s*(#|```|\\||\\s*$)", txt)]
      cut   <- grep("^#+\\s*References", txt)
      if (length(cut)) prose <- txt[seq_len(cut[1] - 1)]
      prose <- prose[!grepl("^\\s*(#|```|\\||\\s*$)", prose)]
      body_txt <- gsub(caveat, " ", paste(prose, collapse = " "), fixed = TRUE)
      sents <- unlist(strsplit(body_txt, "(?<=[.!?])\\s+(?=[A-Z(*`])", perl = TRUE))
      hits <- which(vapply(sents, function(s)
        any(vapply(INDEPENDENCE_ASSERTIONS, grepl, logical(1), x = s,
                   ignore.case = TRUE)), logical(1)))
      for (i in hits) {
        win <- paste(sents[i:min(i + 1L, length(sents))], collapse = " ")
        if (!any(vapply(INDEPENDENCE_QUALIFIERS, grepl, logical(1), x = win,
                        ignore.case = TRUE)))
          problems <- c(problems, sprintf(
            paste("%s: unqualified assertion about sample independence while %d of",
                  "%d curated identities are not evidenced (unevidenced or",
                  "disputed):\n    \"%s\""),
            nm, unverified, nrow(lock$sample_map), trimws(sents[i])))
      }
    }

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
