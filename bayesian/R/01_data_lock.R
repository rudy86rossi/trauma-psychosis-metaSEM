# =============================================================================
# 01_data_lock.R
# Produces the locked analysis table and its provenance registries.
#
#   sources -> ledger -> quality scores -> pathways -> exclusions ->
#   one triplet per sample -> moderator support -> reportability -> V_z
#
# Everything downstream reads data/inputs/analysis_table.csv and the registries,
# never a workbook. Any failure here is fatal by design.
# =============================================================================

build_data_lock <- function(cfg, root = ".") {
  locked <- lock_sources(cfg, root)

  ## ---- pathways, exclusions, instrument choice ----------------------------
  paths <- list()
  for (key in PATH_NAMES(cfg)) {
    df <- build_pathway(cfg, key, locked$sheets)
    if (!nrow(df)) stop("Pathway ", key, " selected no rows.")
    paths[[key]] <- apply_registry(cfg, df)
  }

  ALL <- do.call(rbind, paths); rownames(ALL) <- NULL

  ## ---- registries ---------------------------------------------------------
  ## samples.csv: one row per independent participant sample.
  ## effects.csv: one row per pathway, instrument and effect estimate.
  ## Blose DES-II and PDEQ share a sample_id and differ by effect_id and role.
  ## The registry is READ, not merely written. It owns the identifiers, so they
  ## stay stable when a workbook is revised, and it enforces the structural
  ## invariants the author-year heuristic cannot. Cases needing human judgement
  ## are surfaced in the review queue rather than silently resolved.
  reg <- build_sample_registry(ALL, file.path(root, "data/provenance/samples.csv"))
  samples <- reg$samples
  review_queue <- reg$review_queue

  ALL$sample_id <- samples$sample_id[match(ALL$sample_key, samples$sample_key)]
  if (anyNA(ALL$sample_id))
    stop("Effect rows with no sample_id after registry join: ",
         paste(unique(ALL$sample_key[is.na(ALL$sample_id)]), collapse = ", "))

  effects <- ALL[, c("effect_id", "sample_id", "pathway", "label", "studyID", "year", "n",
                     "r_XM", "r_XY", "r_MY", "min_eigen", "age", "female", "nos", "clinical",
                     "instrument", "analysis_role", "excluded", "exclusion_status",
                     "exclusion_reason")]

  ## ---- gate: one triplet per sample per pathway ---------------------------
  prim <- ALL[ALL$analysis_role == "primary", ]
  dupe <- prim[duplicated(paste(prim$pathway, prim$sample_id)), ]
  if (nrow(dupe))
    stop("A sample contributes more than one primary triplet: ",
         paste(unique(paste(dupe$pathway, dupe$sample_key)), collapse = ", "))

  ## ---- analysis sets ------------------------------------------------------
  ANALYSIS <- lapply(PATH_NAMES(cfg), function(k) {
    d <- prim[prim$pathway == k, , drop = FALSE]; rownames(d) <- NULL; d
  })
  names(ANALYSIS) <- PATH_NAMES(cfg)

  ## Alternative-instrument sensitivity: REPLACE the primary triplet from the
  ## affected sample, never add alongside it.
  ALTERNATIVE <- lapply(PATH_NAMES(cfg), function(k) {
    alt <- ALL[ALL$pathway == k & ALL$analysis_role == "sensitivity_alternative_instrument", ]
    if (!nrow(alt)) return(NULL)
    d <- ANALYSIS[[k]]
    d <- d[!(d$sample_id %in% alt$sample_id), , drop = FALSE]
    d <- rbind(d, alt); rownames(d) <- NULL; d
  })
  names(ALTERNATIVE) <- PATH_NAMES(cfg)

  ## ---- moderator support, contrast clamping, reportability ---------------
  support <- list(); report <- list(); mods <- list()
  for (k in PATH_NAMES(cfg)) {
    d <- ANALYSIS[[k]]
    mk <- list()
    for (m in MOD_NAMES(cfg)) {
      ## One derivation, shared with the deletion refits. See moderator_spec().
      ms <- moderator_spec(cfg, d, m)

      spec <- ms$spec
      spec$pathway <- k
      support[[paste(k, m)]] <- spec
      report[[paste(k, m)]] <- data.frame(pathway = k, moderator = m, k = ms$k,
                                          status = ms$status, note = ms$note,
                                          stringsAsFactors = FALSE)

      mk[[m]] <- ms[c("keep", "x", "centre", "unit", "xc", "spec",
                      "status", "k", "note")]
    }
    mods[[k]] <- mk
  }
  SUPPORT <- do.call(rbind, support); rownames(SUPPORT) <- NULL
  REPORT  <- do.call(rbind, report);  rownames(REPORT)  <- NULL

  ## ---- sampling covariance ------------------------------------------------
  VZ <- list(); VZ_ALT <- list(); VZ_W <- list()
  for (k in PATH_NAMES(cfg)) {
    d <- ANALYSIS[[k]]
    lab <- paste(d$studyID, d$year)
    VZ[[k]]   <- chol_list(vz_rcalc(d), lab)
    VZ_W[[k]] <- chol_list(vz_asycov(d, "weighted"), lab)
    if (!is.null(ALTERNATIVE[[k]])) {
      da <- ALTERNATIVE[[k]]
      VZ_ALT[[k]] <- chol_list(vz_rcalc(da), paste(da$studyID, da$year))
    }
  }

  ## ---- V_z construction comparison ---------------------------------------
  vzc <- do.call(rbind, lapply(PATH_NAMES(cfg), function(k) {
    d <- ANALYSIS[[k]]
    a <- vz_rcalc(d); b <- vz_asycov(d, "individual"); w <- vz_asycov(d, "weighted")
    data.frame(pathway = k,
               max_abs_diff_individual = max(abs(unlist(a) - unlist(b))),
               max_abs_diff_weighted   = max(abs(unlist(a) - unlist(w))),
               stringsAsFactors = FALSE)
  }))

  list(analysis = ANALYSIS, alternative = ALTERNATIVE, mods = mods,
       vz_chol = VZ, vz_chol_alt = VZ_ALT, vz_chol_weighted = VZ_W,
       samples = samples, review_queue = review_queue,
       effects = effects, support = SUPPORT, reportability = REPORT,
       vz_comparison = vzc, hashes = locked$hashes, exclusions = EXCLUSIONS,
       ledger = locked$ledger, quality = locked$quality)
}

#' Write the canonical analysis table and the provenance registries, and return
#' the PATH to the table.
#'
#' The path, not the hash: this is a `format = "file"` target, so targets tracks
#' the file itself. The hash is taken from it downstream, and every fit depends
#' on that hash, so a change in the data invalidates the whole run rather than
#' silently mixing generations.
write_data_lock <- function(lock, root = ".") {
  tab <- do.call(rbind, lock$analysis); rownames(tab) <- NULL
  cols <- c("pathway", "sample_id", "effect_id", "label", "studyID", "year", "n",
            "r_XM", "r_XY", "r_MY", "age", "female", "nos", "clinical",
            "instrument", "analysis_role")
  dir.create(file.path(root, "data", "inputs"), showWarnings = FALSE, recursive = TRUE)
  path <- file.path(root, "data", "inputs", "analysis_table.csv")
  write_canonical(tab[, cols], path, sort_by = c("pathway", "sample_id", "effect_id"))

  prov <- file.path(root, "data", "provenance")

  ## samples.csv is a SOURCE table, so the pipeline seeds it once and never
  ## writes over it again. Overwriting would silently discard a reviewer's
  ## verification entries on the next run, which is the opposite of what a
  ## curated registry is for. Changes to it are made by hand and reviewed.
  spath <- file.path(prov, "samples.csv")
  if (!file.exists(spath))
    write.csv(lock$samples, spath, row.names = FALSE, na = "")

  ## The review queue IS derived, so it is regenerated every run. An empty queue
  ## is written as an empty file rather than omitted, so its absence always
  ## means the lock did not run, never that there was nothing to review.
  rq <- lock$review_queue
  if (is.null(rq))
    rq <- data.frame(sample_key = character(0), issue = character(0),
                     detail = character(0), question = character(0))
  write.csv(rq, file.path(prov, "sample_review_queue.csv"), row.names = FALSE, na = "")

  write.csv(lock$effects,       file.path(prov, "effects.csv"), row.names = FALSE, na = "")
  write.csv(lock$exclusions,    file.path(prov, "exclusions.csv"), row.names = FALSE, na = "")
  write.csv(lock$reportability, file.path(prov, "reportability.csv"), row.names = FALSE, na = "")
  write.csv(lock$support,       file.path(prov, "contrast_support.csv"), row.names = FALSE, na = "")
  write.csv(lock$vz_comparison, file.path(prov, "vz_comparison.csv"), row.names = FALSE, na = "")

  ic <- do.call(rbind, lapply(names(lock$analysis), function(k) {
    e <- lock$effects[lock$effects$pathway == k & lock$effects$analysis_role !=
                        "excluded" & !is.na(lock$effects$instrument), ]
    e <- e[e$sample_id %in% e$sample_id[duplicated(e$sample_id)], ]
    if (!nrow(e)) return(NULL)
    e[, c("pathway", "sample_id", "studyID", "year", "instrument", "analysis_role", "effect_id")]
  }))
  if (is.null(ic)) ic <- data.frame(pathway = character(0), sample_id = character(0),
                                    studyID = character(0), year = integer(0),
                                    instrument = character(0), analysis_role = character(0),
                                    effect_id = character(0))
  write.csv(ic, file.path(prov, "instrument_choices.csv"), row.names = FALSE, na = "")

  path
}
