# =============================================================================
# seed_sample_map.R
# Maintainer tool. Seeds data/provenance/sample_map.csv and migrates
# data/provenance/samples.csv to the curated-identity schema.
#
#   Rscript bayesian/tools/seed_sample_map.R
#
# Run once. Both files are SOURCE tables afterwards: the pipeline reads them and
# never writes them, so re-running this would discard a curator's evidence.
# It therefore refuses to overwrite an existing map unless --force is given.
#
# The seed reproduces the assignment the previous author-year join produced, row
# for row, taking sample_id from the committed data/provenance/effects.csv. That
# is the point: the migration must not reassign anything, so that a rebuild
# leaves data/inputs/analysis_table.csv byte-identical and the audited run stays
# a valid regression benchmark. Every row is seeded `unevidenced`, because
# nothing has been checked against a paper yet.
# =============================================================================

args  <- commandArgs(trailingOnly = TRUE)
force <- "--force" %in% args

root <- normalizePath(".")
while (!dir.exists(file.path(root, "metaSEM")) && dirname(root) != root)
  root <- dirname(root)
setwd(root)

for (f in c("lib_provenance", "lib_config", "lib_data"))
  source(file.path("bayesian", "R", paste0(f, ".R")))

cfg    <- load_config("config/analysis.yml")
locked <- lock_sources(cfg, ".")

map_path <- "data/provenance/sample_map.csv"
reg_path <- "data/provenance/samples.csv"
if (file.exists(map_path) && !force)
  stop(map_path, " already exists. It is a source table holding curated ",
       "evidence; pass --force only if you mean to discard it.")

## ---- the rows the analysis will see -----------------------------------------
ALL <- do.call(rbind, lapply(PATH_NAMES(cfg), function(k)
  build_pathway(cfg, k, locked$sheets)))
rownames(ALL) <- NULL
ALL$report_key <- paste(ALL$studyID, ALL$year)
ALL$effect_id  <- sprintf("%s:%s:%s", ALL$pathway, gsub("[^A-Za-z0-9]", "", ALL$report_key),
                          ifelse(is.na(ALL$instrument), "NA",
                                 gsub("[^A-Za-z0-9]", "", ALL$instrument)))

## ---- the assignment being preserved ----------------------------------------
old <- utils::read.csv("data/provenance/effects.csv", stringsAsFactors = FALSE,
                       na.strings = "")
j <- match(ALL$effect_id, old$effect_id)
if (anyNA(j))
  stop("No committed effects.csv row for: ",
       paste(ALL$effect_id[is.na(j)], collapse = ", "),
       ". The seed cannot preserve an assignment it cannot find.")
ALL$sample_id     <- old$sample_id[j]
ALL$analysis_role <- old$analysis_role[j]

## A cohort contributing more than one live row to one pathway is, today, always
## the Blose DES-II and PDEQ pair: one cohort measured with two instruments. It
## is recorded as a DECLARATION rather than left to be inferred from the
## collision, because that inference is what made the collision gate unreachable.
live <- ALL$analysis_role != "excluded"
grp  <- paste(ALL$pathway, ALL$sample_id)
ALL$multiplicity <- ifelse(live & grp %in% grp[live][duplicated(grp[live])],
                           "alternative_instrument", "single")

smap <- data.frame(
  map_id               = sprintf("M%03d", seq_len(nrow(ALL))),
  source_effect_id     = ALL$source_effect_id,
  workbook             = ALL$source_workbook,
  sheet                = ALL$source_sheet,
  source_row           = ALL$source_row,
  pathway              = ALL$pathway,
  studyID              = ALL$studyID,
  year                 = ALL$year,
  instrument           = ALL$instrument,
  n                    = ALL$n,
  row_fingerprint      = ALL$row_fingerprint,
  sample_id            = ALL$sample_id,
  multiplicity         = ALL$multiplicity,
  mapping_basis        = "author_year_default",
  evidence_status      = "unevidenced",
  evidence_type        = NA_character_,
  evidence_locator     = NA_character_,
  evidence_prepared_by = NA_character_,
  accepted_by          = NA_character_,
  accepted_date        = NA_character_,
  note                 = NA_character_,
  stringsAsFactors = FALSE)
smap <- smap[order(smap$sample_id, smap$pathway, smap$source_row), SAMPLE_MAP_COLS]
smap$map_id <- sprintf("M%03d", seq_len(nrow(smap)))
write.csv(smap, map_path, row.names = FALSE, na = "")

## ---- migrate the registry ---------------------------------------------------
## Identifiers and their meaning are carried over untouched. Only the schema
## changes: sample_key becomes canonical_label, because the file no longer joins
## on it and a column named for a join is how the old defect would return.
prev <- utils::read.csv(reg_path, stringsAsFactors = FALSE, na.strings = "")
reg <- data.frame(
  sample_id       = prev$sample_id,
  canonical_label = prev$sample_key,
  sample_status   = "in_analysis",
  predecessor_ids = NA_character_,
  successor_ids   = NA_character_,
  note            = NA_character_,
  stringsAsFactors = FALSE)
reg <- reg[order(reg$sample_id), ]
write.csv(reg, reg_path, row.names = FALSE, na = "")

cat("Seeded", nrow(smap), "mapping rows over", length(unique(smap$sample_id)),
    "cohorts, all unevidenced.\n")
cat("Migrated", nrow(reg), "registry rows to the curated-identity schema.\n")
