// ==============================================================================
// random_slopes_noncentered.stan
// Random slopes con non-centered parametrization + regularizing priors
// 
// Permette eterogeneità individuale: ogni persona ha propri slopes
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
  vector[D] beta_wp;           // Fixed effects (population mean slopes)
  
  // Random intercepts (centered)
  vector[N_subj] u_alpha;
  real<lower=0> sigma_alpha;
  
  // Random slopes (NON-CENTERED for better convergence)
  matrix[N_subj, D] z_beta;    // Standard normal
  vector<lower=0>[D] sigma_beta;  // SD of random slopes
  
  real<lower=0> sigma_y;
}

transformed parameters {
  // Non-centered transformation: u_beta = z_beta * sigma_beta
  matrix[N_subj, D] u_beta;
  
  for (d in 1:D) {
    u_beta[, d] = z_beta[, d] * sigma_beta[d];
  }
}

model {
  vector[N] mu;
  
  // ========================================================================
  // PRIORS
  // ========================================================================
  
  // Fixed effects
  alpha ~ normal(0, 10);
  beta_wp ~ normal(0, 3);
  
  // Random intercepts
  u_alpha ~ normal(0, sigma_alpha);
  sigma_alpha ~ exponential(1);
  
  // Random slopes: NON-CENTERED
  to_vector(z_beta) ~ std_normal();  // z ~ N(0,1)
  
  // Regularizing priors on SD (limita a range ragionevole)
  sigma_beta ~ normal(0, 2);
  
  // Residual
  sigma_y ~ exponential(1);
  
  // ========================================================================
  // LIKELIHOOD
  // ========================================================================
  
  for (i in 1:N) {
    mu[i] = alpha + u_alpha[subj[i]];
    
    // Each person has own slope: beta_wp + u_beta
    for (d in 1:D) {
      mu[i] += (beta_wp[d] + u_beta[subj[i], d]) * X_wp[i, d];
    }
  }
  
  y_wp ~ normal(mu, sigma_y);
}

generated quantities {
  vector[N] y_rep;
  vector[N] log_lik;
  real r2_within;
  
  // Individual total slopes (fixed + random) per domain
  matrix[N_subj, D] total_slopes;
  
  // Posterior predictive
  {
    vector[N] mu;
    for (i in 1:N) {
      mu[i] = alpha + u_alpha[subj[i]];
      for (d in 1:D) {
        mu[i] += (beta_wp[d] + u_beta[subj[i], d]) * X_wp[i, d];
      }
      y_rep[i] = normal_rng(mu[i], sigma_y);
      log_lik[i] = normal_lpdf(y_wp[i] | mu[i], sigma_y);
    }
  }
  
  // R²
  {
  vector[N] mu_pred;
  for (i in 1:N) {
    mu_pred[i] = alpha + u_alpha[subj[i]];
    for (d in 1:D)
      mu_pred[i] += (beta_wp[d] + u_beta[subj[i], d]) * X_wp[i, d];
  }
  r2_within = variance(mu_pred) / (variance(mu_pred) + square(sigma_y));
}
  
  // Calcola total slopes per ogni soggetto-dominio
  for (s in 1:N_subj) {
    for (d in 1:D) {
      total_slopes[s, d] = beta_wp[d] + u_beta[s, d];
    }
  }
}
