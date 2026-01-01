data {
  int<lower=1> N_subj;

  // voice
  int<lower=1> N_voice;
  array[N_voice] int<lower=1, upper=N_subj> subj_voice;
  vector[N_voice] y;
  vector[N_voice] c1;
  vector[N_voice] c2;

  // EMA measurement model
  int<lower=1> N_ema;
  array[N_ema] int<lower=1, upper=N_subj> subj_ema;
  int<lower=1> D_ema;
  matrix[N_ema, D_ema] X; // z-scored EMA observed domain scores

  // Baseline domains (subject-level)
  int<lower=1> D_base;
  matrix[N_subj, D_base] Z; // z-scored baseline domains
}

parameters {
  // voice fixed effects
  real alpha;
  real b1;
  real b2;

  vector[D_ema] g1_ema;
  vector[D_ema] g2_ema;

  vector[D_base] g1_base;
  vector[D_base] g2_base;

  // NON-CENTERED subject random effects
  matrix[N_subj, 3] z_u;  // standard normals [intercept, slope1, slope2]
  vector<lower=0>[3] tau;
  real<lower=0> sigma_y;

  // NON-CENTERED latent EMA trait
  matrix[N_subj, D_ema] z_theta;
  vector<lower=0>[D_ema] sigma_ema;
}

transformed parameters {
  // Reconstruct random effects from standardized versions
  matrix[N_subj, 3] u;
  for (i in 1:N_subj) {
    u[i, 1] = z_u[i, 1] * tau[1];  // u0
    u[i, 2] = z_u[i, 2] * tau[2];  // u1
    u[i, 3] = z_u[i, 3] * tau[3];  // u2
  }
  
  // Reconstruct latent traits (already standardized)
  matrix[N_subj, D_ema] theta = z_theta;
}

model {
  // Priors (more informative)
  alpha ~ normal(0, 50);
  b1 ~ normal(0, 10);
  b2 ~ normal(0, 10);

  g1_ema ~ normal(0, 5);   // tighter prior
  g2_ema ~ normal(0, 5);
  g1_base ~ normal(0, 5);
  g2_base ~ normal(0, 5);

  // Exponential priors for scale parameters
  tau ~ exponential(0.5);
  sigma_y ~ exponential(0.1);
  sigma_ema ~ exponential(1);

  // Standard normals for non-centered parametrization
  to_vector(z_u) ~ std_normal();
  to_vector(z_theta) ~ std_normal();

  // Measurement model (EMA) - vectorized where possible
  for (n in 1:N_ema) {
    int s = subj_ema[n];
    X[n] ~ normal(theta[s], sigma_ema);
  }

  // Voice model - vectorized
  {
    vector[N_voice] mu;
    for (n in 1:N_voice) {
      int s = subj_voice[n];

      real b1s = b1
        + dot_product(g1_ema, theta[s])
        + dot_product(g1_base, Z[s]);

      real b2s = b2
        + dot_product(g2_ema, theta[s])
        + dot_product(g2_base, Z[s]);

      mu[n] = alpha + u[s, 1] + (b1s + u[s, 2]) * c1[n] + (b2s + u[s, 3]) * c2[n];
    }
    y ~ normal(mu, sigma_y);
  }
}

generated quantities {
  real delta_stress = b1 - 0.5*b2;
  real delta_recovery = b2 - 0.5*b1;
  
  // log_lik for LOO-CV
  vector[N_voice] log_lik;
  for (n in 1:N_voice) {
    int s = subj_voice[n];
    
    real b1s = b1
      + dot_product(g1_ema, theta[s])
      + dot_product(g1_base, Z[s]);
    
    real b2s = b2
      + dot_product(g2_ema, theta[s])
      + dot_product(g2_base, Z[s]);
    
    real mu = alpha + u[s, 1] + (b1s + u[s, 2]) * c1[n] + (b2s + u[s, 3]) * c2[n];
    log_lik[n] = normal_lpdf(y[n] | mu, sigma_y);
  }
  
  // posterior predictive for checking
  vector[N_voice] y_rep;
  for (n in 1:N_voice) {
    int s = subj_voice[n];
    
    real b1s = b1
      + dot_product(g1_ema, theta[s])
      + dot_product(g1_base, Z[s]);
    
    real b2s = b2
      + dot_product(g2_ema, theta[s])
      + dot_product(g2_base, Z[s]);
    
    real mu = alpha + u[s, 1] + (b1s + u[s, 2]) * c1[n] + (b2s + u[s, 3]) * c2[n];
    y_rep[n] = normal_rng(mu, sigma_y);
  }
}
