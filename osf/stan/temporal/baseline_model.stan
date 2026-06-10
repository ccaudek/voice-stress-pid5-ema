// ==============================================================================
// baseline_model_ncp.stan
// Fixed effects only + random intercept (NON-CENTERED)
// ==============================================================================

data {
  int<lower=1> N;
  int<lower=1> N_subj;
  int<lower=1> D;
  array[N] int<lower=1, upper=N_subj> subj;
  matrix[N, D] X_wp;
  vector[N] y_wp;
}

parameters {
  real alpha;
  vector[D] beta_wp;

  // Non-centered random intercepts
  vector[N_subj] u_alpha_raw;
  real<lower=0> sigma_alpha;

  real<lower=0> sigma_y;
}

transformed parameters {
  vector[N_subj] u_alpha = sigma_alpha * u_alpha_raw;
}

model {
  vector[N] mu;

  // Priors (puoi stringere un po' se serve)
  alpha ~ normal(0, 10);
  beta_wp ~ normal(0, 3);

  u_alpha_raw ~ normal(0, 1);
  sigma_alpha ~ exponential(1);

  sigma_y ~ exponential(1);

  // Linear predictor (vettorializzato)
  mu = alpha + u_alpha[subj] + X_wp * beta_wp;

  // Likelihood
  y_wp ~ normal(mu, sigma_y);
}

generated quantities {
  vector[N] y_rep;
  vector[N] log_lik;
  real r2_within;

  {
    vector[N] mu = alpha + u_alpha[subj] + X_wp * beta_wp;

    for (i in 1:N) {
      y_rep[i] = normal_rng(mu[i], sigma_y);
      log_lik[i] = normal_lpdf(y_wp[i] | mu[i], sigma_y);
    }
  }

  // R²
  {
  vector[N] mu_pred = alpha + u_alpha[subj] + X_wp * beta_wp;
  real var_mu = variance(mu_pred);
  real var_e  = square(sigma_y);     // per Normal con sd = sigma_y
  r2_within = var_mu / (var_mu + var_e);
  }
}
