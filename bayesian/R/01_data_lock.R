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

  ## ---- curated identity, read and validated BEFORE anything is selected ---
  ## MAP -> VALIDATE -> INSTRUMENT SELECTION -> GATES. The map owns identity, so
  ## it is read before a single row is demoted or a gate evaluated. Previously
  ## identity was minted from paste(studyID, year), used to demote rows, and only
  ## then joined to the registry, which made both structural gates unreachable.
  smap_path <- file.path(root, "data/provenance/sample_map.csv")
  reg_path  <- file.path(root, "data/provenance/samples.csv")
  smap <- read_sample_map(smap_path)
  reg  <- read_sample_registry(reg_path)
  validate_sample_map(smap, reg, smap_path, reg_path)

  ## ---- pathways, exclusions, identity, instrument choice ------------------
  paths <- list()
  for (key in PATH_NAMES(cfg)) {
    df <- build_pathway(cfg, key, locked$sheets)
    if (!nrow(df)) stop("Pathway ", key, " selected no rows.")
    paths[[key]] <- apply_registry(cfg, df, smap)
  }

  ALL <- do.call(rbind, paths); rownames(ALL) <- NULL

  ## ---- structural gates on the curated identity ---------------------------
  gate_sample_identity(ALL, smap, reg)

  ## ---- registries ---------------------------------------------------------
  ## samples.csv: one row per cohort. It owns the identifiers and their lineage,
  ## and DERIVES its verification status from the mapping rows.
  ## effects.csv: one row per pathway, instrument and effect estimate.
  ## Blose DES-II and PDEQ share a sample_id and differ by effect_id and role.
  ## Cases needing human judgement are surfaced in the review queue.
  regout <- build_sample_registry(ALL, smap, reg)
  samples <- regout$samples
  review_queue <- regout$review_queue

  effects <- ALL[, c("effect_id", "sample_id", "map_id", "source_effect_id", "pathway",
                     "label", "studyID", "year", "n",
                     "r_XM", "r_XY", "r_MY", "min_eigen", "age", "female", "nos", "clinical",
                     "instrument", "multiplicity", "mapping_basis",
                     "analysis_role", "excluded", "exclusion_status",
                     "exclusion_reason")]
  ## min_eigen is a diagnostic, not an input: it exists so a non-positive-definite
  ## matrix can be caught and named. Its last digits are LAPACK's, and they differ
  ## between platforms. Simpson 2020 comes out as 0.519788893624411 on macOS and
  ## ...412 on Linux, which is enough to make the committed registry differ from a
  ## rebuild and to fail the "build left no tracked file modified" gate. Rounding
  ## the RECORDED value makes the file reproducible; the in-memory value used for
  ## the positive-definiteness test is untouched, so nothing is weakened. The
  ## canonical analysis table never contained this column, so no hash changes.
  effects$min_eigen <- round(effects$min_eigen, 10)

  ## ---- gate: one triplet per cohort per pathway ---------------------------
  ## A POST-CONDITION of selection now, not a restatement of it. Selection only
  ## touches groups the curator declared to be one cohort measured twice, so this
  ## catches a fault in the selection code itself rather than a fact about the
  ## data, which gate_sample_identity() has already settled.
  prim <- ALL[ALL$analysis_role == "primary", ]
  dupe <- prim[duplicated(paste(prim$pathway, prim$sample_id)), ]
  if (nrow(dupe))
    stop("A cohort contributes more than one primary triplet: ",
         paste(unique(paste(dupe$pathway, dupe$sample_id)), collapse = ", "))

  ## ---- analysis sets ------------------------------------------------------
  ANALYSIS <- lapply(PATH_NAMES(cfg), function(k) {
    d <- prim[prim$pathway == k, , drop = FALSE]; rownames(d) <- NULL; d
  })
  names(ANALYSIS) <- PATH_NAMES(cfg)

  ## ---- synthesis eligibility ----------------------------------------------
  ## Every pathway stays extracted and locked above; only the ELIGIBLE set is
  ## fitted and reported. The excluded pathways are kept as an explicit record.
  ELIG <- synthesis_eligibility(cfg, ANALYSIS)
  SYNTH <- ELIG$pathway[ELIG$eligible]
  if (!length(SYNTH)) stop("No pathway meets the synthesis minimum.")

  ## ---- triplet source, for the protocol-specified conversion sensitivity --
  TSRC <- read_triplet_source(file.path(root, "data/provenance/triplet_source.csv"),
                              ANALYSIS[SYNTH])

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
       samples = samples, sample_map = smap, review_queue = review_queue,
       effects = effects, support = SUPPORT, reportability = REPORT,
       vz_comparison = vzc, hashes = locked$hashes, exclusions = EXCLUSIONS,
       synthesis = SYNTH, synthesis_eligibility = ELIG, triplet_source = TSRC,
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

  ## samples.csv and sample_map.csv are SOURCE tables. The pipeline never writes
  ## over either, because doing so would silently discard a curator's identity
  ## and evidence entries on the next run, which is the opposite of what they are
  ## for. They are seeded once by bayesian/tools/seed_sample_map.R and thereafter
  ## edited by hand and reviewed. The DERIVED half of the registry is published
  ## separately so a reader can see it without it being mistaken for an input.
  write.csv(lock$samples, file.path(prov, "samples_derived.csv"),
            row.names = FALSE, na = "")

  ## The review queue IS derived, so it is regenerated every run. An empty queue
  ## is written as an empty file rather than omitted, so its absence always
  ## means the lock did not run, never that there was nothing to review.
  rq <- lock$review_queue
  if (is.null(rq))
    rq <- data.frame(sample_id = character(0), report_key = character(0),
                     issue = character(0), detail = character(0),
                     question = character(0))
  write.csv(rq, file.path(prov, "sample_review_queue.csv"), row.names = FALSE, na = "")

  write.csv(lock$effects,       file.path(prov, "effects.csv"), row.names = FALSE, na = "")
  write.csv(lock$exclusions,    file.path(prov, "exclusions.csv"), row.names = FALSE, na = "")
  write.csv(lock$reportability, file.path(prov, "reportability.csv"), row.names = FALSE, na = "")
  write.csv(lock$support,       file.path(prov, "contrast_support.csv"), row.names = FALSE, na = "")
  write.csv(lock$vz_comparison, file.path(prov, "vz_comparison.csv"), row.names = FALSE, na = "")
  write.csv(lock$synthesis_eligibility, file.path(prov, "synthesis_eligibility.csv"),
            row.names = FALSE, na = "")

  ## Only declared multi-instrument groups belong here. A superseded report is a
  ## different decision and is not an instrument choice.
  ic <- do.call(rbind, lapply(names(lock$analysis), function(k) {
    e <- lock$effects[lock$effects$pathway == k & !is.na(lock$effects$instrument) &
                        lock$effects$analysis_role %in%
                          c("primary", "sensitivity_alternative_instrument"), ]
    e <- e[e$sample_id %in% e$sample_id[duplicated(e$sample_id)], ]
    if (!nrow(e)) return(NULL)
    e[, c("pathway", "sample_id", "studyID", "year", "instrument", "multiplicity",
          "analysis_role", "effect_id")]
  }))
  if (is.null(ic)) ic <- data.frame(pathway = character(0), sample_id = character(0),
                                    studyID = character(0), year = integer(0),
                                    instrument = character(0), multiplicity = character(0),
                                    analysis_role = character(0), effect_id = character(0))
  write.csv(ic, file.path(prov, "instrument_choices.csv"), row.names = FALSE, na = "")

  path
}


#' Read the per-triplet source classification and check it covers EXACTLY the
#' primary triplets of the eligible pathways: one row each, a known type, and no
#' row for anything that is not analysed. A missing or stale classification is
#' fatal, because the conversion sensitivity would otherwise silently keep or
#' drop the wrong cohorts.
read_triplet_source <- function(path, analysis) {
  if (!file.exists(path)) stop("Missing ", path, ".")
  ts <- read.csv(path, stringsAsFactors = FALSE, na.strings = "")
  need <- c("effect_id", "pathway", "sample_id", "source_type")
  if (length(setdiff(need, names(ts))))
    stop(basename(path), " lacks: ", paste(setdiff(need, names(ts)), collapse = ", "))
  types <- c("direct", "converted", "adjusted", "untraceable")
  bad <- setdiff(unique(ts$source_type), types)
  if (length(bad)) stop(basename(path), " has unknown source_type: ", paste(bad, collapse = ", "))
  want <- do.call(rbind, lapply(analysis, function(d) d[, c("effect_id", "pathway", "sample_id")]))
  kw <- paste(want$pathway, want$sample_id, want$effect_id)
  kt <- paste(ts$pathway, ts$sample_id, ts$effect_id)
  if (anyDuplicated(kt)) stop(basename(path), " classifies a triplet twice.")
  miss <- setdiff(kw, kt); extra <- setdiff(kt, kw)
  if (length(miss) || length(extra))
    stop(basename(path), " does not match the analysed triplets. Unclassified: ",
         paste(miss, collapse = "; "), ". Not analysed: ", paste(extra, collapse = "; "), ".")
  ts
}
