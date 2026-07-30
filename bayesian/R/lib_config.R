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
            "diagnostics", "sbc", "sensitivity")
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
  if (isTRUE(cfg$bayes_factors))
    stop("Bayes factors are removed from this release; see bayesian/docs/bayes_factors_removed.md.")
  invisible(TRUE)
}

MOD_NAMES  <- function(cfg) names(cfg$moderators)
PATH_NAMES <- function(cfg) names(cfg$pathways)
PAIRS      <- function(cfg) unlist(cfg$pairs)

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

#' Contrast label for tables and figures. A clamped contrast is never described
#' with its nominal label.
contrast_label <- function(spec) {
  if (identical(spec$kind, "binary")) return("non-clinical vs clinical")
  sprintf("%g to %g", spec$supported_lo, spec$supported_hi)
}

#' Deterministic per-fit seed from a fit identifier, so that refitting one model
#' reproduces exactly regardless of the order the others ran in.
fit_seed <- function(cfg, fit_id) {
  h <- utf8ToInt(fit_id)
  as.integer((cfg$seed + sum(h * seq_along(h) * 7919L)) %% .Machine$integer.max)
}
