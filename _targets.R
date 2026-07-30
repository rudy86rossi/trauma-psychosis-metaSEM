# =============================================================================
# _targets.R
# The single workflow graph for the Bayesian reanalysis.
#
#   Rscript --vanilla -e 'targets::tar_make()'
#
# There is deliberately no second, sequential driver. Two orchestration paths
# diverge, and the one that is not routinely exercised is the one that silently
# stops matching the other.
#
# Outputs go to a staging directory first. Only a complete bundle that has
# passed every gate is promoted to bayesian/results/run_<analysis_id>/, and the
# run manifest is written last, so an interrupted attempt can never be mistaken
# for a finished release.
# =============================================================================

suppressPackageStartupMessages({
  library(targets)
  library(tarchetypes)
})

LIBS <- c("lib_provenance", "lib_config", "lib_data", "lib_summaries", "lib_stan",
          "lib_brms", "lib_ppc", "lib_sbc", "lib_fitplan", "lib_frequentist",
          "lib_report", "lib_figures", "01_data_lock")
for (f in LIBS) source(file.path("bayesian", "R", paste0(f, ".R")))

tar_option_set(
  packages = c("digest", "yaml", "readxl", "metafor", "metaSEM", "cmdstanr",
               "posterior", "brms", "ggplot2"),
  format = "rds",
  storage = "worker", retrieval = "worker",
  ## Three workers, four chains each, on twelve physical cores:
  ##   concurrent fits x chains per fit <= available physical cores.
  ## crew is optional; without it the identical graph runs sequentially.
  controller = if (requireNamespace("crew", quietly = TRUE))
    crew::crew_controller_local(workers = 3, seconds_idle = 60) else NULL
)

list(
  # ---- configuration and data lock ------------------------------------------
  tar_target(config_file, "config/analysis.yml", format = "file"),
  tar_target(cfg, load_config(config_file)),

  tar_target(corrections_file, "data/provenance/corrections.csv", format = "file"),
  tar_target(quality_file, "data/provenance/quality_scores.csv", format = "file"),
  tar_target(stan_file, "bayesian/stan/masem_valid.stan", format = "file"),

  tar_target(lock, {
    corrections_file; quality_file          # declare the dependency
    build_data_lock(cfg, ".")
  }),
  tar_target(analysis_table, write_data_lock(lock, "."), format = "file"),
  tar_target(data_sha, sha256_file(analysis_table)),

  # The analysis identifier is deterministic in everything that can change a
  # result: the frozen source commit, the locked data, the validated
  # configuration, the Stan source and the R environment.
  tar_target(analysis_id, {
    src <- tryCatch(system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
                    error = function(e) "nogit")
    substr(digest::digest(paste(src, data_sha, cfg$.sha, sha256_file(stan_file),
                                sha256_file("renv.lock")), algo = "sha256"), 1, 12)
  }),

  # ---- frequentist validation targets, retained as targets only ------------
  tar_target(frequentist_targets, frequentist_slopes(cfg, lock)),

  # ---- the fit plan ---------------------------------------------------------
  # Branches are grouped by class and pathway so the store holds tens of
  # objects rather than hundreds, while each branch stays a few minutes long.
  tar_target(fit_plan, build_fit_plan(cfg, lock)),
  tar_group_by(fit_plan_groups, fit_plan, fit_class, pathway),

  tar_target(
    fit_results,
    lapply(seq_len(nrow(fit_plan_groups)),
           function(i) run_fit(cfg, lock, fit_plan_groups[i, ])),
    pattern = map(fit_plan_groups),
    iteration = "list"
  ),

  # ---- simulation-based calibration ----------------------------------------
  tar_target(sbc_scenarios, cfg$sbc$scenarios, iteration = "list"),
  tar_target(sbc_ranks, run_sbc_scenario(cfg, sbc_scenarios, lock),
             pattern = map(sbc_scenarios), iteration = "list"),
  tar_target(sbc_report, {
    r <- do.call(rbind, sbc_ranks)
    sbc_summary(r, n_bins = cfg$sbc$retained_draws + 1L)
  }),

  # ---- collation ------------------------------------------------------------
  tar_target(results, collate_results(fit_results)),
  tar_target(fit_ledger, build_fit_ledger(fit_plan, results, analysis_id, data_sha, cfg)),

  tar_target(cross_layer, cross_layer_table(cfg, results, lock)),
  tar_target(tables, build_tables(cfg, lock, results, cross_layer,
                                  frequentist_targets, sbc_report)),

  # ---- gates ----------------------------------------------------------------
  # Every one of these calls stop() on failure, so a breach returns a non-zero
  # process status and no output is promoted.
  tar_target(gate_fits, gate_fit_completeness(fit_plan, fit_ledger)),
  tar_target(gate_diagnostics, gate_diagnostic_thresholds(cfg, results)),
  tar_target(gate_expected_rows, gate_expected_reportable(cfg, lock, tables)),

  # ---- promotion ------------------------------------------------------------
  # Runs only after every gate has returned. The manifest is written last.
  tar_target(release, {
    gate_fits; gate_diagnostics; gate_expected_rows
    promote_run(cfg, analysis_id, data_sha, lock, results, tables, cross_layer,
                fit_ledger, sbc_report, frequentist_targets)
  }, format = "file")
)
