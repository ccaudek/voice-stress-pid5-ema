data {
  int<lower=1> N_subj;

  // Voice/articulatory outcome (standardized in R)
  int<lower=1> N_voice;
  array[N_voice] int<lower=1, upper=N_subj> subj_voice;
  vector[N_voice] y;
  vector[N_voice] c1;  // stress contrast: PRE vs BASELINE
  vector[N_voice] c2;  // recovery contrast: POST vs PRE

  // EMA measurement model
  int<lower=1> N_ema;
  array[N_ema] int<lower=1, upper=N_subj> subj_ema;
  int<lower=1> D;              // PID-5 domains
  matrix[N_ema, D] X;          // standardized EMA domain scores
}

parameters {
  // Latent PID-5 traits (true person means): theta[i,d]
  matrix[N_subj, D] theta;
  vector<lower=0>[D] sigma_ema;

  // Fixed effects for standardized voice/articulatory outcome
  real alpha;          // grand intercept
  real b1;             // stress main effect
  real b2;             // recovery main effect

  // Main effects of traits on baseline outcome
  vector[D] a_trait;

  // Moderation: trait x stress and trait x recovery
  vector[D] g1;        // stress moderation: c1 * theta
  vector[D] g2;        // recovery moderation: c2 * theta

  // Random effects (no correlations): intercept + stress slope + recovery slope
  vector<lower=0>[3] tau;
  matrix[N_subj, 3] z_u;
  real<lower=0> sigma_y;
}

transformed parameters {
  matrix[N_subj, 3] u;

  u = z_u;
  for (i in 1:N_subj) {
    u[i, 1] = u[i, 1] * tau[1];
    u[i, 2] = u[i, 2] * tau[2];
    u[i, 3] = u[i, 3] * tau[3];
  }
}

model {
  // -------------------------
  // Priors: outcome y is standardized in R
  // -------------------------
  to_vector(theta) ~ normal(0, 1);
  sigma_ema ~ exponential(1);

  alpha ~ normal(0, 1);
  b1 ~ normal(0, 0.5);
  b2 ~ normal(0, 0.5);

  a_trait ~ normal(0, 0.5);

  // Moderation terms: conservative shrinkage on standardized scale
  g1 ~ normal(0, 0.25);
  g2 ~ normal(0, 0.25);

  tau ~ exponential(2);
  to_vector(z_u) ~ normal(0, 1);
  sigma_y ~ exponential(1);

  // -------------------------
  // Measurement model (EMA)
  // -------------------------
  for (n in 1:N_ema) {
    for (d in 1:D) {
      X[n, d] ~ normal(theta[subj_ema[n], d], sigma_ema[d]);
    }
  }

  // -------------------------
  // Voice/articulatory outcome model
  // -------------------------
  for (j in 1:N_voice) {
    int i = subj_voice[j];
    real mu = alpha
      + u[i, 1]
      + (b1 + u[i, 2]) * c1[j]
      + (b2 + u[i, 3]) * c2[j];

    for (d in 1:D) {
      mu += a_trait[d] * theta[i, d]
          + g1[d] * c1[j] * theta[i, d]
          + g2[d] * c2[j] * theta[i, d];
    }

    y[j] ~ normal(mu, sigma_y);
  }
}

generated quantities {
  vector[N_voice] y_rep;

  for (j in 1:N_voice) {
    int i = subj_voice[j];
    real mu = alpha
      + u[i, 1]
      + (b1 + u[i, 2]) * c1[j]
      + (b2 + u[i, 3]) * c2[j];

    for (d in 1:D) {
      mu += a_trait[d] * theta[i, d]
          + g1[d] * c1[j] * theta[i, d]
          + g2[d] * c2[j] * theta[i, d];
    }

    y_rep[j] = normal_rng(mu, sigma_y);
  }
}
