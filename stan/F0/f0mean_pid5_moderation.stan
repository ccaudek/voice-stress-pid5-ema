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
  // Latent traits (true person means): theta[i,d]
  matrix[N_subj, D] theta;
  vector<lower=0>[D] sigma_ema;

  // Fixed effects for voice
  real alpha;          // grand intercept
  real b1;             // stress main effect
  real b2;             // recovery main effect

  // Main effects of traits on baseline voice (optional)
  vector[D] a_trait;

  // Moderation: trait × stress and trait × recovery
  vector[D] g1;        // stress moderation (c1 * theta)
  vector[D] g2;        // recovery moderation (c2 * theta)

  // Random effects (no correlations): intercept + slopes
  vector<lower=0>[3] tau;      // SDs for random intercept, stress slope, recovery slope
  matrix[N_subj, 3] z_u;       // standard normals
  real<lower=0> sigma_y;       // residual SD
}

transformed parameters {
  matrix[N_subj, 3] u;
  u = z_u;
  for (i in 1:N_subj) {
    u[i,1] = u[i,1] * tau[1];
    u[i,2] = u[i,2] * tau[2];
    u[i,3] = u[i,3] * tau[3];
  }
}

model {
  // -------------------------
  // Priors
  // -------------------------
  to_vector(theta) ~ normal(0, 1);
  sigma_ema ~ exponential(1);

  alpha ~ normal(220, 30);
  b1 ~ normal(0, 10);
  b2 ~ normal(0, 10);

  a_trait ~ normal(0, 5);

  // moderation: shrinkage (second-order)
  g1 ~ normal(0, 3);
  g2 ~ normal(0, 3);

  tau ~ exponential(0.5);
  to_vector(z_u) ~ normal(0, 1);

  sigma_y ~ exponential(0.1);

  // -------------------------
  // Measurement model (EMA)
  // -------------------------
  for (n in 1:N_ema) {
    for (d in 1:D) {
      X[n,d] ~ normal(theta[subj_ema[n], d], sigma_ema[d]);
    }
  }

  // -------------------------
  // Voice outcome model
  // -------------------------
  for (j in 1:N_voice) {
    int i = subj_voice[j];

    real mu = alpha
      + u[i,1]
      + (b1 + u[i,2]) * c1[j]
      + (b2 + u[i,3]) * c2[j];

    for (d in 1:D) {
      mu += a_trait[d] * theta[i,d]
          + g1[d] * c1[j] * theta[i,d]
          + g2[d] * c2[j] * theta[i,d];
    }

    y[j] ~ normal(mu, sigma_y);
  }
}

generated quantities {
  vector[N_voice] y_rep;
  for (j in 1:N_voice) {
    int i = subj_voice[j];
    real mu = alpha + u[i,1] + (b1 + u[i,2]) * c1[j] + (b2 + u[i,3]) * c2[j];
    for (d in 1:D) {
      mu += a_trait[d] * theta[i,d]
          + g1[d] * c1[j] * theta[i,d]
          + g2[d] * c2[j] * theta[i,d];
    }
    y_rep[j] = normal_rng(mu, sigma_y);
  }
}
