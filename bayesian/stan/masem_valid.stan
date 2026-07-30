// =============================================================================
// masem_valid.stan
// Bayesian meta-analytic SEM / meta-regression on valid-correlation coordinates.
//
// Each row supplies three Fisher-z transformed correlations from one sample,
// in the fixed order (r_XM, r_XY, r_MY), with a known within-study sampling
// covariance V_z supplied as its Cholesky factor.
//
// The hierarchy is placed on
//     eta = (atanh rho_XM, atanh rho_XY, atanh rho_MY.X)
// where the third element is a PARTIAL correlation. Recovering
//     rho_MY = rho_XM rho_XY + rho_MY.X sqrt((1 - rho_XM^2)(1 - rho_XY^2))
// guarantees a positive-definite 3x3 correlation matrix for every draw, which a
// hierarchy placed directly on the three zero-order correlations does not.
//
// Rows drawn from the same participants share one study-level random effect via
// sample_id, so duplicate rows from a single sample are not treated as
// independent studies.
// =============================================================================

functions {
  // eta (valid-correlation coordinates) -> (rho_XM, rho_XY, rho_MY)
  vector eta_to_rho(vector eta) {
    real rXM  = tanh(eta[1]);
    real rXY  = tanh(eta[2]);
    real rMYX = tanh(eta[3]);
    real rMY  = rXM * rXY + rMYX * sqrt((1 - square(rXM)) * (1 - square(rXY)));
    return [rXM, rXY, rMY]';
  }

  // Mean vector on the Fisher-z scale. The first two coordinates are already
  // Fisher-z by construction; only rho_MY needs transforming back.
  vector eta_to_z(vector eta) {
    vector[3] rho = eta_to_rho(eta);
    return [eta[1], eta[2], atanh(rho[3])]';
  }

  // (rho_XM, rho_XY, rho_MY) -> (a, b, c', indirect, total)
  // Standardised mediation model M = aX + e, Y = c'X + bM + e.
  // total = c' + ab = rho_XY exactly.
  vector rho_to_path(vector rho) {
    real d  = 1 - square(rho[1]);
    real a  = rho[1];
    real b  = (rho[3] - rho[1] * rho[2]) / d;
    real cp = (rho[2] - rho[1] * rho[3]) / d;
    return [a, b, cp, a * b, rho[2]]';
  }
}

data {
  int<lower=1> K;                                  // rows
  int<lower=1> S;                                  // independent samples
  array[K] int<lower=1, upper=S> sample_id;

  array[K] vector[3] z;                            // Fisher-z (r_XM, r_XY, r_MY)
  array[K] matrix[3, 3] L_V;                       // Cholesky of V_z per row

  int<lower=0, upper=1> P;                         // 1 if a moderator is present
  vector[K] x;                                     // moderator (zeros when P = 0)

  real<lower=0> prior_alpha_sd;
  real<lower=0> prior_beta_sd;
  real<lower=0> prior_tau_sd;

  int<lower=1> NC;                                 // number of contrast values
  vector[NC] xc;                                   // moderator values to report at

  int<lower=0, upper=1> robust;                    // 1 = multivariate Student t
  real<lower=2> nu;                                // t degrees of freedom

  int<lower=0, upper=1> prior_only;                // 1 = prior predictive
}

parameters {
  vector[3] alpha;
  array[P] vector[3] beta;
  vector<lower=0>[3] tau;
  matrix[3, S] u_raw;                              // non-centred
}

transformed parameters {
  matrix[3, S] u;
  for (s in 1 : S) {
    u[ : , s] = tau .* u_raw[ : , s];
  }
}

model {
  alpha ~ normal(0, prior_alpha_sd);
  if (P) {
    beta[1] ~ normal(0, prior_beta_sd);
  }
  tau ~ normal(0, prior_tau_sd);                   // half-normal via lower = 0
  to_vector(u_raw) ~ std_normal();

  if (!prior_only) {
    for (i in 1 : K) {
      vector[3] eta = alpha + u[ : , sample_id[i]];
      if (P) {
        eta += beta[1] * x[i];
      }
      vector[3] mu = eta_to_z(eta);
      if (robust) {
        z[i] ~ multi_student_t_cholesky(nu, mu, L_V[i]);
      } else {
        z[i] ~ multi_normal_cholesky(mu, L_V[i]);
      }
    }
  }
}

generated quantities {
  // Population-average quantities at each contrast value (u = 0). This is the
  // direct analogue of the TSSEM estimand and is the primary quantity.
  array[NC] vector[3] rho_pop;
  vector[NC] a_pop;    vector[NC] b_pop;    vector[NC] cprime_pop;
  vector[NC] ind_pop;  vector[NC] total_pop; vector[NC] pmed_pop;

  // ZERO-ORDER coordinates on the Fisher-z scale at each contrast value, and
  // their change across the reported contrast.
  //
  // These exist so this model can be compared with the brms reference on the
  // SAME estimand. The model's own beta acts on
  //     eta = (atanh rho_XM, atanh rho_XY, atanh rho_MY.X),
  // whose third element is a PARTIAL correlation, so beta[3] is not the brms
  // r_MY slope and must never be tabulated as though it were. d_z is the
  // matched quantity: the full-contrast change in each zero-order Fisher-z.
  //
  // Note the asymmetry the methods must state. The first two coordinates are
  // linear in eta, so their d_z equals beta * (xc[NC] - xc[1]) exactly. The
  // third is reached nonlinearly through the valid-correlation map, so its d_z
  // is not a rescaled coefficient at all: agreement with brms there is an
  // architecture comparison, not a check that preprocessing matches.
  array[NC] vector[3] z_pop;
  vector[3] d_z = rep_vector(0, 3);

  // New-study predictive quantities: what to expect in a new study.
  vector[NC] a_new;    vector[NC] b_new;    vector[NC] cprime_new;
  vector[NC] ind_new;  vector[NC] total_new;

  // Posterior predictive replications, in BOTH forms.
  //
  // Conditional replications reuse each study's fitted random effect, so they
  // ask whether the likelihood describes the observed triplets given where the
  // model has placed those studies. New-study replications draw a fresh random
  // effect, so they ask whether the between-study distribution can generate
  // data like these at all. Conditional checks alone can look satisfactory
  // simply because the study effects have absorbed an outlying study, which is
  // why both are archived.
  array[K] vector[3] z_rep_cond;
  array[K] vector[3] z_rep_new;
  vector[K] maha_obs;                    // observed discrepancy, given the fit
  vector[K] maha_rep;                    // same statistic on conditional replicates
  array[K] vector[3] resid_z;            // observed minus fitted, per coordinate

  // Study-specific quantities, and the average of the study-specific indirect
  // effects. This differs from ind_pop because the transformation is nonlinear.
  vector[S] ind_study;
  real ind_study_mean;

  // Contrasts between the two extreme contrast values, when there are two.
  real d_a = 0;  real d_b = 0;  real d_cprime = 0;  real d_ind = 0;  real d_total = 0;

  vector[K] log_lik;

  // Joint density of the CHOSEN priors, for priorsense power-scaling. The
  // structural std_normal on u_raw is excluded: the question is sensitivity to
  // the priors that were selected, not to the non-centring.
  //
  // The name must be exactly `lprior`: that is the variable priorsense looks
  // for, following the brms convention. Naming it log_prior makes
  // create_priorsense_data() fail.
  real lprior = normal_lpdf(alpha | 0, prior_alpha_sd)
                + normal_lpdf(tau | 0, prior_tau_sd)
                + (P ? normal_lpdf(beta[1] | 0, prior_beta_sd) : 0);

  {
    for (c in 1 : NC) {
      vector[3] eta = alpha;
      if (P) {
        eta += beta[1] * xc[c];
      }
      rho_pop[c] = eta_to_rho(eta);
      z_pop[c] = eta_to_z(eta);
      vector[5] pth = rho_to_path(rho_pop[c]);
      a_pop[c] = pth[1];  b_pop[c] = pth[2];  cprime_pop[c] = pth[3];
      ind_pop[c] = pth[4];  total_pop[c] = pth[5];
      pmed_pop[c] = pth[4] / pth[5];

      vector[3] eta_new;
      for (j in 1 : 3) {
        eta_new[j] = eta[j] + normal_rng(0, tau[j]);
      }
      vector[5] pnew = rho_to_path(eta_to_rho(eta_new));
      a_new[c] = pnew[1];  b_new[c] = pnew[2];  cprime_new[c] = pnew[3];
      ind_new[c] = pnew[4];  total_new[c] = pnew[5];
    }

    if (NC >= 2) {
      d_a      = a_pop[NC]      - a_pop[1];
      d_b      = b_pop[NC]      - b_pop[1];
      d_cprime = cprime_pop[NC] - cprime_pop[1];
      d_ind    = ind_pop[NC]    - ind_pop[1];
      d_total  = total_pop[NC]  - total_pop[1];
      d_z      = z_pop[NC]      - z_pop[1];
    }

    // Study-specific: evaluated at each sample's own moderator value.
    array[S] real x_of_sample = rep_array(0.0, S);
    for (i in 1 : K) {
      x_of_sample[sample_id[i]] = x[i];
    }
    for (s in 1 : S) {
      vector[3] eta_s = alpha + u[ : , s];
      if (P) {
        eta_s += beta[1] * x_of_sample[s];
      }
      ind_study[s] = rho_to_path(eta_to_rho(eta_s))[4];
    }
    ind_study_mean = mean(ind_study);

    for (i in 1 : K) {
      vector[3] eta = alpha + u[ : , sample_id[i]];
      if (P) {
        eta += beta[1] * x[i];
      }
      vector[3] mu = eta_to_z(eta);
      log_lik[i] = robust
                   ? multi_student_t_cholesky_lpdf(z[i] | nu, mu, L_V[i])
                   : multi_normal_cholesky_lpdf(z[i] | mu, L_V[i]);

      // --- posterior predictive replication, conditional on the fitted study
      // effect. Answers: given where the model has put this study, does the
      // likelihood describe its observed triplet?
      z_rep_cond[i] = robust
                      ? multi_student_t_cholesky_rng(nu, mu, L_V[i])
                      : multi_normal_cholesky_rng(mu, L_V[i]);

      // --- replication with a FRESH study effect. Answers: could the
      // between-study distribution produce data like these at all? A model
      // whose random effects have absorbed an outlier can pass the conditional
      // check and fail this one.
      vector[3] eta_fresh = alpha;
      if (P) {
        eta_fresh += beta[1] * x[i];
      }
      for (j in 1 : 3) {
        eta_fresh[j] += normal_rng(0, tau[j]);
      }
      vector[3] mu_fresh = eta_to_z(eta_fresh);
      z_rep_new[i] = robust
                     ? multi_student_t_cholesky_rng(nu, mu_fresh, L_V[i])
                     : multi_normal_cholesky_rng(mu_fresh, L_V[i]);

      // Study-level discrepancy on the sampling-covariance metric, for observed
      // and replicated data. Comparing the two gives a Bayesian posterior
      // predictive p-value per study and identifies poorly reproduced samples.
      resid_z[i]  = z[i] - mu;
      maha_obs[i] = dot_self(mdivide_left_tri_low(L_V[i], z[i] - mu));
      maha_rep[i] = dot_self(mdivide_left_tri_low(L_V[i], z_rep_cond[i] - mu));
    }
  }
}
