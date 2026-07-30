# Bayesian reanalysis

British English throughout. Prepared by ricardotwumasi.

A Bayesian replacement for the frequentist two-stage meta-analytic SEM in
`metaSEM/`. The frequentist results are retained as internal validation targets
only; they are not co-primary, and their p values are not used to arbitrate a
disagreement with the posterior.

## Restoring and running

```sh
# 1. R packages
Rscript --vanilla -e 'renv::restore(prompt = FALSE)'

# 2. CmdStan is NOT managed by renv and must be installed separately at the
#    pinned version. check_cmdstan_toolchain() checks the compiler; it does not
#    install CmdStan.
Rscript --vanilla -e 'cmdstanr::check_cmdstan_toolchain(fix = FALSE)'
Rscript --vanilla -e 'cmdstanr::install_cmdstan(version = "2.36.0")'
Rscript --vanilla -e 'stopifnot(cmdstanr::cmdstan_version() == "2.36.0")'

# 3. Tests and a short sampling smoke test
Rscript --vanilla -e 'testthat::test_dir("bayesian/tests/testthat")'
Rscript --vanilla -e 'targets::tar_make(names = c("lock", "analysis_table", "fit_plan"))'

# 4. The full analysis
Rscript --vanilla -e 'targets::tar_make()'
```

There is deliberately **no second, sequential driver**. Two orchestration paths
diverge, and the one not routinely exercised is the one that silently stops
matching the other.

## Model layers

**Primary: `stan/masem_valid.stan`.** The between-study hierarchy is placed on
`(atanh r_XM, atanh r_XY, atanh r_MY|X)`, the last a partial correlation.
Recovering

    r_MY = r_XM r_XY + r_MY|X sqrt[(1 - r_XM^2)(1 - r_XY^2)]

guarantees that every posterior draw defines a positive-definite correlation
matrix, which a hierarchy on the three zero-order correlations does not. The
standardised mediation model is recovered exactly from each draw.

**Reference: `brms`.** The correlation-level multivariate meta-regression, with
the known sampling covariance supplied through `fcor()` and the residual SD
fixed to 1. It is the like-for-like comparison with the frequentist zero-order
analysis and the bridge to `metaSEM`. It is **not** the arbiter when it
disagrees with the primary model.

### Comparing them

The layers are compared on one estimand: `d_z`, the change in each **zero-order**
Fisher-z coordinate across the reported contrast.

The primary model's own `beta[3]` acts on a **partial** correlation and is not
the brms `r_MY` slope. In a fitted example the two differ by 0.247, so
tabulating one against the other would compare different quantities. `d_z`
exists precisely to avoid that.

Scaling a brms coefficient to its contrast (`beta * span`) is a draw-wise
transform **within** one fit, where draws correspond. The comparison **across**
the two fits is distributional: median difference in pooled SD units, SD ratio,
interval and density overlap, ECDF distance. Draws from separate fits are never
paired elementwise.

For `r_MY` the endpoint is comparable but the architectures differ: brms models
the marginal Fisher-z linearly, the primary model reaches it nonlinearly. Close
agreement there is an architecture-sensitivity result, not a proof that
preprocessing matches.

## Decision rule

Dienes (2020) with the Kruschke (2018) interval rules. Estimation is primary
throughout; a decision is an additional statement about a posterior, never a
substitute for reporting it.

The smallest effect size of interest is 0.05 Fisher-z **across the nominal
scientific contrast**, and it anchors the prior too. Both are divided by the
nominal span to reach the fitted coefficient scale, and reporting scales back
out to the contrast the data support:

    sesoi_per_unit     = 0.05 / nominal_span
    sesoi_reported     = sesoi_per_unit * supported_span
    prior_beta_unit_sd = 0.20 / nominal_span

A contrast clamped to observed support therefore keeps the same per-unit
threshold rather than being granted a looser one. Clamped rows are labelled with
the contrast actually estimated; the nominal label is never reused.

Derived mediation contrasts get no ROPE verdict: no defensible threshold exists
for a change in `ab`, and inventing one to obtain a categorical answer is not
justified. The partial-correlation coordinate gets none either, being a
parameterisation quantity rather than a scientific estimand.

## Validation

- **Formal rank-based SBC** over six prespecified scenarios. Screening level;
  conditional on the plug-in sampling covariance matrices. Failed fits stay in
  the denominator.
- **Posterior predictive checks in both forms**: conditional on the fitted study
  effects, and with fresh random effects. A conditional check alone can look
  satisfactory because the random effects have absorbed an outlier.
- **Generated quantities** verified against an independent R implementation, and
  the path coefficients against an independent `solve()` route.
- **Gates** that call `stop()`: exact `fit_id` set equality, diagnostic
  thresholds, and an explicit expected-reportable set.

## Not in this release

- **Bayes factors.** See `docs/bayes_factors_removed.md`.
- **Informative location priors.** See `docs/prior_scope.md`.
- **`bayesianmasem`.** Both interfaces fail internally on these models. It is
  not described as a successful independent validation; the brms layer supplies
  the independent check.
