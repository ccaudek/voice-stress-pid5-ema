data {
  int<lower=1> N_subj;

  // Voice outcome
  int<lower=1> N_voice;
  array[N_voice] int<lower=1, upper=N_subj> subj_voice;
  vector[N_voice] y;
  vector[N_voice] c1;  // stress contrast
  vector[N_voice] c2;  // recovery contrast

  // EMA measurement model
  int<lower=1> N_ema;
  array[N_ema] int<lower=1, upper=N_subj> subj_ema;
  int<lower=1> D;              // 5 domains
  matrix[N_ema, D] X;          // standardized EMA domain scores
}

parameters {
  // NON-CENTERED latent traits: z_theta ~ N(0,1)
  matrix[N_subj, D] z_theta;
  vector<lower=0>[D] sigma_ema;

  // Fixed effects for voice
  real alpha;          // grand intercept
  real b1;             // stress main effect
  real b2;             // recovery main effect

  // Main effects of traits on baseline voice
  vector[D] a_trait;

  // Moderation: trait × stress and trait × recovery
  vector[D] g1;        // stress moderation (c1 * theta)
  vector[D] g2;        // recovery moderation (c2 * theta)

  // NON-CENTERED random effects
  vector<lower=0>[3] tau;      // SDs for random intercept, stress slope, recovery slope
  matrix[N_subj, 3] z_u;       // standard normals
  real<lower=0> sigma_y;       // residual SD
}

transformed parameters {
  // Reconstruct theta from standardized version
  // theta[i,d] = z_theta[i,d] * 1 (already standardized, no scaling needed)
  matrix[N_subj, D] theta = z_theta;
  
  // Reconstruct random effects
  matrix[N_subj, 3] u;
  for (i in 1:N_subj) {
    u[i, 1] = z_u[i, 1] * tau[1];
    u[i, 2] = z_u[i, 2] * tau[2];
    u[i, 3] = z_u[i, 3] * tau[3];
  }
}

model {
  // -------------------------
  // Priors
  // -------------------------
  to_vector(z_theta) ~ std_normal();
  sigma_ema ~ exponential(1);

  alpha ~ normal(220, 30);
  b1 ~ normal(0, 10);
  b2 ~ normal(0, 10);

  a_trait ~ normal(0, 5);

  // moderation: tighter shrinkage
  g1 ~ normal(0, 3);
  g2 ~ normal(0, 3);

  tau ~ exponential(0.5);
  to_vector(z_u) ~ std_normal();

  sigma_y ~ exponential(0.1);

  // -------------------------
  // Measurement model (EMA) - vectorized
  // -------------------------
  for (n in 1:N_ema) {
    int s = subj_ema[n];
    X[n] ~ normal(theta[s], sigma_ema);
  }

  // -------------------------
  // Voice outcome model - vectorized
  // -------------------------
  {
    vector[N_voice] mu;
    for (j in 1:N_voice) {
      int i = subj_voice[j];
      
      // Base prediction
      mu[j] = alpha + u[i, 1] + (b1 + u[i, 2]) * c1[j] + (b2 + u[i, 3]) * c2[j];
      
      // Add trait effects and moderations
      for (d in 1:D) {
        mu[j] += a_trait[d] * theta[i, d]
              + g1[d] * c1[j] * theta[i, d]
              + g2[d] * c2[j] * theta[i, d];
      }
    }
    y ~ normal(mu, sigma_y);
  }
}

generated quantities {
  // Implied contrasts
  real delta_stress = b1 - 0.5*b2;
  real delta_recovery = b2 - 0.5*b1;
  
  // log_lik for LOO-CV
  vector[N_voice] log_lik;
  for (j in 1:N_voice) {
    int i = subj_voice[j];
    real mu = alpha + u[i, 1] + (b1 + u[i, 2]) * c1[j] + (b2 + u[i, 3]) * c2[j];
    
    for (d in 1:D) {
      mu += a_trait[d] * theta[i, d]
          + g1[d] * c1[j] * theta[i, d]
          + g2[d] * c2[j] * theta[i, d];
    }
    log_lik[j] = normal_lpdf(y[j] | mu, sigma_y);
  }
  
  // Posterior predictive
  vector[N_voice] y_rep;
  for (j in 1:N_voice) {
    int i = subj_voice[j];
    real mu = alpha + u[i, 1] + (b1 + u[i, 2]) * c1[j] + (b2 + u[i, 3]) * c2[j];
    
    for (d in 1:D) {
      mu += a_trait[d] * theta[i, d]
          + g1[d] * c1[j] * theta[i, d]
          + g2[d] * c2[j] * theta[i, d];
    }
    y_rep[j] = normal_rng(mu, sigma_y);
  }
}
