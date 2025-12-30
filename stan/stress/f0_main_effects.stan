// ============================================================================
// f0_main_effects.stan
// Hierarchical Bayesian model for F0 mean
// Main effects of stress (PRE vs BASELINE) and recovery (POST vs PRE)
// No personality trait moderation
// ============================================================================

data {
  int<lower=1> N_subj;                             // number of subjects
  int<lower=1> N_obs;                              // total observations
  array[N_obs] int<lower=1, upper=N_subj> subj_id; // subject index per observation
  vector[N_obs] y;                                 // F0 mean (Hz)
  vector[N_obs] c1;                                // stress contrast: PRE vs BASELINE
  vector[N_obs] c2;                                // recovery contrast: POST vs PRE
}

parameters {
  // Fixed effects
  real alpha;                    // grand intercept (baseline F0)
  real b1;                       // stress main effect (c1)
  real b2;                       // recovery main effect (c2)
  
  // Random effects (non-centered parameterization)
  vector[N_subj] z_u0;           // random intercepts (standardized)
  vector[N_subj] z_u1;           // random slopes for c1 (standardized)
  vector[N_subj] z_u2;           // random slopes for c2 (standardized)
  vector<lower=0>[3] tau;        // SDs: tau[1]=intercept, tau[2]=c1, tau[3]=c2
  
  real<lower=0> sigma_y;         // residual SD
}

transformed parameters {
  // Non-centered random effects
  vector[N_subj] u0 = tau[1] * z_u0;
  vector[N_subj] u1 = tau[2] * z_u1;
  vector[N_subj] u2 = tau[3] * z_u2;
}

model {
  // Priors
  alpha ~ normal(220, 30);       // typical F0 for adult males: 100-150 Hz; females: 200-250 Hz
  b1 ~ normal(0, 10);            // weakly informative for stress effect
  b2 ~ normal(0, 10);            // weakly informative for recovery effect
  
  tau ~ exponential(0.5);        // half-life at ~1.4, allows moderate heterogeneity
  sigma_y ~ exponential(0.1);    // half-life at ~7, residual variation
  
  z_u0 ~ std_normal();
  z_u1 ~ std_normal();
  z_u2 ~ std_normal();
  
  // Likelihood
  for (n in 1:N_obs) {
    int s = subj_id[n];
    real mu = alpha + u0[s] 
            + (b1 + u1[s]) * c1[n] 
            + (b2 + u2[s]) * c2[n];
    y[n] ~ normal(mu, sigma_y);
  }
}

generated quantities {
  // Posterior predictive replicates for model checking
  vector[N_obs] y_rep;
  
  // Log-likelihood for model comparison (LOO, WAIC)
  vector[N_obs] log_lik;
  
  for (n in 1:N_obs) {
    int s = subj_id[n];
    real mu = alpha + u0[s] 
            + (b1 + u1[s]) * c1[n] 
            + (b2 + u2[s]) * c2[n];
    
    y_rep[n] = normal_rng(mu, sigma_y);
    log_lik[n] = normal_lpdf(y[n] | mu, sigma_y);
  }
}
