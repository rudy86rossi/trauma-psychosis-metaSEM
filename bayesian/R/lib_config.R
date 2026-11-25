# =============================================================================
# lib_config.R
# Loads and validates config/analysis.yml, and derives every scale-dependent
# quantity from it.
#
# The central idea: the prior and the smallest effect size of interest are both
# anchored on the NOMINAL scientific contrast. The fitted coefficient is per
# unit, so both are divided by the nominal span to reach the coefficient scale.
# Reporting then scales back out to whatever contrast the data support. A
# shortened contrast therefore keeps the same per-unit threshold rather than
# being granted a more permissive one.
# =============================================================================

suppressPackageStartupMessages(library(yaml))

load_config <- function(path = "config/analysis.yml") {
  if (!file.exists(path)) stop("Configuration not found: ", path)
  cfg <- yaml::read_yaml(path)
  validate_config(cfg)
  cfg$.path <- path
  cfg$.sha  <- digest::digest(file = path, algo = "sha256")
  cfg
}

validate_config <- function(cfg) {
  need <- c("seed", "inputs", "pairs", "pathways", "moderators", "decision",
            "priors", "reportability", "instrument_priority", "sampling",
            "diagnostics", "sbc", "sensitivity", "proportion_mediated", "synthesis")
  miss <- setdiff(need, names(cfg))
  if (length(miss)) stop("analysis.yml is missing: ", paste(miss, collapse = ", "))

  if (!identical(cfg$pairs, list("r_XM", "r_XY", "r_MY")) &&
      !identical(unlist(cfg$pairs), c("r_XM", "r_XY", "r_MY")))
    stop("pairs must be exactly r_XM, r_XY, r_MY: the ordering is assumed throughout.")

  for (m in names(cfg$moderators)) {
    mm <- cfg$moderators[[m]]
    if (is.null(mm$unit) || mm$unit <= 0) stop("Moderator ", m, " needs a positive unit.")
    if (length(mm$nominal_contrast) != 2)
      stop("Moderator ", m, " needs a two-element nominal_contrast.")
    if (diff(unlist(mm$nominal_contrast)) <= 0)
      stop("Moderator ", m, " nominal_contrast must be increasing.")
  }
  if (cfg$decision$sesoi_full <= 0) stop("sesoi_full must be positive.")

  kmin <- cfg$synthesis$min_independent_cohorts
  if (is.null(kmin) || kmin != round(kmin) || kmin < 2)
    stop("synthesis$min_independent_cohorts must be a whole number of at least 2.")

  ## The percentage's reportability rule. Validated here so that a threshold
  ## typed as a percentage rather than a probability, which would pass every one
  ## of these tests silently at report time, fails at load.
  pm <- cfg$proportion_mediated
  if (pm$delta_primary <= 0) stop("proportion_mediated$delta_primary must be positive.")
  if (!any(abs(unlist(pm$delta_grid) - pm$delta_primary) < 1e-12))
    stop("proportion_mediated$delta_primary must appear in delta_grid, so the ",
         "reported rule is one point of the sensitivity range rather than a ",
         "separate choice.")
  for (p in c("min_p_total_gt_delta", "min_p_in_unit_interval"))
    if (pm[[p]] <= 0 || pm[[p]] >= 1)
      stop("proportion_mediated$", p, " is a probability and must lie in (0, 1); got ", pm[[p]])
  if (!isTRUE(pm$unmoderated_only))
    stop("proportion_mediated$unmoderated_only is FALSE. A ratio evaluated at a ",
         "moderator endpoint is conditional on a moderator value in an analysis ",
         "that cannot establish moderation; reporting it is not supported.")

  if (isTRUE(cfg$bayes_factors))
    stop("Bayes factors are removed from this release; see bayesian/docs/bayes_factors_removed.md.")
  invisible(TRUE)
}

MOD_NAMES  <- function(cfg) names(cfg$moderators)
PATH_NAMES <- function(cfg) names(cfg$pathways)
PAIRS      <- function(cfg) unlist(cfg$pairs)

#' The pathways eligible for synthesis. PATH_NAMES() is every EXTRACTED pathway
#' and is used only to build the lock; everything downstream of the lock (fit
#' plan, frequentist targets, gates, tables, denominators) uses this set, so an
#' extracted pathway below the cohort minimum cannot reappear in any of them.
SYNTH_NAMES <- function(lock) lock$synthesis

#' Reportability restricted to the synthesis set. The lock keeps the extraction
#' level table (all pathways) for provenance; consumers that build or check
#' results must read this one.
synth_reportability <- function(lock)
  lock$reportability[lock$reportability$pathway %in% lock$synthesis, , drop = FALSE]

#' Which extracted pathways meet the cohort minimum, counted as DISTINCT sample
#' identifiers so that one cohort entered twice cannot make up the number.
synthesis_eligibility <- function(cfg, analysis) {
  kmin <- cfg$synthesis$min_independent_cohorts
  k <- vapply(analysis, function(d) length(unique(d$sample_id)), integer(1))
  data.frame(pathway = names(analysis), k_independent = unname(k),
             min_independent_cohorts = kmin, eligible = unname(k >= kmin),
             rule = sprintf("at least %d independent cohorts (PROSPERO CRD42024542972, version 2.0)", kmin),
             stringsAsFactors = FALSE)
}

sesoi_grid <- function(cfg) {
  g <- unlist(cfg$decision$sesoi_grid)
  seq(g[1], g[2], by = g[3])
}

#' Everything that depends on the moderator's scale, in one place.
#'
#' @param supported a length-2 numeric giving the contrast the data actually
#'   support, on the ORIGINAL moderator scale. NULL means the nominal contrast
#'   is fully supported.
#' @return a one-row data frame. `sesoi_reported` is the bound applied to the
#'   posterior of the full reported contrast; `prior_beta_unit_sd` is the prior
#'   standard deviation for the per-unit coefficient the model actually fits.
scale_spec <- function(cfg, moderator, supported = NULL) {
  mm <- cfg$moderators[[moderator]]
  if (is.null(mm)) stop("Unknown moderator: ", moderator)

  nominal <- unlist(mm$nominal_contrast)
  unit    <- mm$unit
  nominal_span <- diff(nominal) / unit

  sup <- if (is.null(supported)) nominal else as.numeric(supported)
  supported_span <- diff(sup) / unit
  if (supported_span <= 0)
    stop("Supported contrast for ", moderator, " has non-positive span.")

  sesoi_per_unit <- cfg$decision$sesoi_full / nominal_span
  prior_unit_sd  <- cfg$priors$primary$beta_full_sd / nominal_span

  data.frame(
    moderator          = moderator,
    kind               = mm$kind,
    unit               = unit,
    nominal_lo         = nominal[1],
    nominal_hi         = nominal[2],
    nominal_span       = nominal_span,
    supported_lo       = sup[1],
    supported_hi       = sup[2],
    supported_span     = supported_span,
    clamped            = !isTRUE(all.equal(sup, nominal)),
    sesoi_full         = cfg$decision$sesoi_full,
    sesoi_per_unit     = sesoi_per_unit,
    sesoi_reported     = sesoi_per_unit * supported_span,
    prior_beta_full_sd = cfg$priors$primary$beta_full_sd,
    prior_beta_unit_sd = prior_unit_sd,
    decision_permitted = TRUE,
    stringsAsFactors   = FALSE)
}

#' Per-unit prior standard deviation for an arbitrary full-contrast scale, used
#' by the prior-sensitivity grid so that the grid varies the SCIENTIFIC scale
#' rather than the coefficient scale.
prior_unit_sd_for <- function(cfg, moderator, beta_full_sd) {
  mm <- cfg$moderators[[moderator]]
  beta_full_sd / (diff(unlist(mm$nominal_contrast)) / mm$unit)
}

#' Everything one moderator implies for one set of studies: support, centring,
#' clamped contrast, reportability and whether a decision is permitted.
#'
#' This is the SINGLE derivation. The data lock calls it for the full pathway;
#' the leave-one-study-out and alternative-instrument refits call it again for
#' their own subset. Previously the two paths had separate implementations and
#' only the first computed reportability, so a deletion that collapsed a binary
#' arm inherited its parent's status and still received a categorical verdict on
#' a group of one. Deriving both from the same function is what makes that
#' impossible rather than merely fixed.
#'
#' @param d data frame of the studies actually being fitted.
#' @return list with the model inputs (keep, x, centre, unit, xc, spec) and the
#'   reporting status (k, status, note). `spec$decision_permitted` is already set.
moderator_spec <- function(cfg, d, m) {
  mm   <- cfg$moderators[[m]]
  if (is.null(mm)) stop("Unknown moderator: ", m)
  vals <- d[[m]]
  keep <- !is.na(vals)
  kk   <- sum(keep)

  if (identical(mm$kind, "binary")) {
    n1 <- sum(vals[keep] == 1); n0 <- sum(vals[keep] == 0)
    status <- if (min(n0, n1) < cfg$reportability$binary_min_per_arm) "not reported"
              else if (min(n0, n1) <= cfg$reportability$binary_fragile_per_arm) "fragile"
              else "ok"
    note   <- sprintf("clinical k = %d, non-clinical k = %d", n1, n0)
    spec   <- scale_spec(cfg, m, NULL)
    centre <- 0
    x      <- vals
    xc     <- c(0, 1)
  } else {
    status <- if (kk < cfg$reportability$continuous_min_k) "not reported"
              else if (kk <= cfg$reportability$continuous_fragile_k) "fragile"
              else "ok"
    note <- sprintf("k = %d of %d rows", kk, nrow(d))
    ## Clamp the reported contrast to observed support. The nominal contrast is
    ## retained in the record; the label is not reused for a shorter one.
    rng <- if (any(keep)) range(vals[keep]) else unlist(mm$nominal_contrast)
    nom <- unlist(mm$nominal_contrast)
    sup <- c(max(nom[1], rng[1]), min(nom[2], rng[2]))
    if (sup[2] <= sup[1]) sup <- rng
    spec   <- scale_spec(cfg, m, sup)
    centre <- if (any(keep)) mean(vals[keep]) else 0
    x      <- (vals - centre) / mm$unit
    xc     <- (c(spec$supported_lo, spec$supported_hi) - centre) / mm$unit
  }

  ## A model that is not reported still needs no decision.
  spec$decision_permitted <- status != "not reported"

  list(keep = keep, x = ifelse(is.na(x), 0, x), centre = centre,
       unit = mm$unit, xc = xc, spec = spec,
       k = kk, status = status, note = note)
}

#' Contrast label for tables and figures. A clamped contrast is never described
#' with its nominal label.
#'
#' The binary label states the SUBTRACTION, not the level names in an arbitrary
#' order. `d_z = z_pop[NC] - z_pop[1]` with xc = c(0, 1) and clinical coded 1 is
#' clinical minus non-clinical, so a label reading "non-clinical vs clinical"
#' invites a reader to take a negative estimate as the opposite finding.
contrast_label <- function(spec) {
  if (identical(spec$kind, "binary")) return("clinical minus non-clinical")
  sprintf("%g to %g", spec$supported_lo, spec$supported_hi)
}

#' Deterministic per-fit seed from a fit identifier, so that refitting one model
#' reproduces exactly regardless of the order the others ran in.
fit_seed <- function(cfg, fit_id) {
  h <- utf8ToInt(fit_id)
  as.integer((cfg$seed + sum(h * seq_along(h) * 7919L)) %% .Machine$integer.max)
}
