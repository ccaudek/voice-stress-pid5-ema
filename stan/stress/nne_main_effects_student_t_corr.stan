// ============================================================================
// nne_main_effects_student_t_corr.stan
// Hierarchical Bayesian model for Normalized Noise Energy (NNE)
// Main effects of stress (PRE vs BASELINE) and recovery (POST vs PRE)
// Robust Student-t likelihood + correlated random effects
// Includes observation-level log_lik and subject-level log_lik_subj
// ============================================================================

data {
  int<lower=1> N_subj;                             // number of subjects
  int<lower=1> N_obs;                              // total observations
  array[N_obs] int<lower=1, upper=N_subj> subj_id; // subject index per observation
  vector[N_obs] y;                                 // NNE (dB)
  vector[N_obs] c1;                                // stress contrast: PRE vs BASELINE
  vector[N_obs] c2;                                // recovery contrast: POST vs PRE
}

transformed data {
  real<lower=2> nu = 4;                            // fixed df for robust sensitivity model
}

parameters {
  // Fixed effects
  real alpha;                                      // grand intercept
  real b1;                                         // stress main effect (c1)
  real b2;                                         // recovery main effect (c2)

  // Correlated random effects, non-centered parameterization
  matrix[3, N_subj] z_u;                           // standardized random effects
  vector<lower=0>[3] tau;                          // SDs: intercept, c1 slope, c2 slope
  cholesky_factor_corr[3] L_Omega;                 // Cholesky factor of RE correlation matrix

  real<lower=0> sigma_y;                           // residual scale
}

transformed parameters {
  // u[s, 1] = random intercept for subject s
  // u[s, 2] = random slope for c1 for subject s
  // u[s, 3] = random slope for c2 for subject s
  matrix[N_subj, 3] u;

  u = (diag_pre_multiply(tau, L_Omega) * z_u)';
}

model {
  // Priors for fixed effects
  alpha ~ normal(-28, 5);                          // typical NNE values around -20 to -30 dB
  b1 ~ normal(0, 5);                               // stress effect
  b2 ~ normal(0, 5);                               // recovery effect

  // Priors for random effects
  tau ~ exponential(0.2);
  L_Omega ~ lkj_corr_cholesky(2);
  to_vector(z_u) ~ std_normal();

  // Residual scale
  sigma_y ~ exponential(0.2);

  // Robust likelihood
  for (n in 1:N_obs) {
    int s = subj_id[n];
    real mu = alpha + u[s, 1]
            + (b1 + u[s, 2]) * c1[n]
            + (b2 + u[s, 3]) * c2[n];

    y[n] ~ student_t(nu, mu, sigma_y);
  }
}

generated quantities {
  // Correlation matrix for random effects
  corr_matrix[3] Omega;

  // Posterior predictive replicates for model checking
  vector[N_obs] y_rep;

  // Observation-level log-likelihood for ordinary PSIS-LOO / WAIC
  vector[N_obs] log_lik;

  // Subject-level summed log-likelihood for cluster-level PSIS-LOO diagnostics
  vector[N_subj] log_lik_subj;

  Omega = multiply_lower_tri_self_transpose(L_Omega);
  log_lik_subj = rep_vector(0, N_subj);

  for (n in 1:N_obs) {
    int s = subj_id[n];
    real mu = alpha + u[s, 1]
            + (b1 + u[s, 2]) * c1[n]
            + (b2 + u[s, 3]) * c2[n];

    y_rep[n] = student_t_rng(nu, mu, sigma_y);
    log_lik[n] = student_t_lpdf(y[n] | nu, mu, sigma_y);
    log_lik_subj[s] += student_t_lpdf(y[n] | nu, mu, sigma_y);
  }
}
