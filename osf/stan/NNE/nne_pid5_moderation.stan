// nne_pid5_moderation.stan
// Joint measurement-and-outcome model for Normalized Noise Energy (NNE)
// × PID-5 personality pathology domains.
//
// Architecture is identical to f0mean_pid5_moderation.stan; only the
// intercept prior and residual SD prior are adapted for NNE (dB scale,
// typically –35 to –20 dB in female speakers).
//
// Outcome y = session-level NNE (dB), averaged across /a/, /i/, /u/.
// Positive moderation coefficients (g1, g2) indicate *less negative* NNE
// (increased glottal noise / reduced phonatory control).

data {
  int<lower=1> N_subj;

  // Voice outcome
  int<lower=1> N_voice;
  array[N_voice] int<lower=1, upper=N_subj> subj_voice;
  vector[N_voice] y;
  vector[N_voice] c1;   // stress contrast   (pre vs baseline: +0.5/-0.5/0.0)
  vector[N_voice] c2;   // recovery contrast (post vs pre:      0.0/-0.5/+0.5)

  // EMA measurement model
  int<lower=1> N_ema;
  array[N_ema] int<lower=1, upper=N_subj> subj_ema;
  int<lower=1> D;             // 5 PID-5 domains
  matrix[N_ema, D] X;         // standardised EMA domain scores (mean 0, SD 1)
}

parameters {
  // Latent traits: theta[i, d] = true person-level standing on domain d
  matrix[N_subj, D] theta;
  vector<lower=0>[D] sigma_ema;   // occasion-level EMA variability per domain

  // Fixed effects for voice outcome
  real alpha;       // grand intercept (population mean NNE at baseline)
  real b1;          // population-average stress effect
  real b2;          // population-average recovery effect

  // Main effects of traits on baseline NNE level
  vector[D] a_trait;

  // Moderation: trait × contrast interactions
  vector[D] g1;     // stress moderation   (c1 × theta)
  vector[D] g2;     // recovery moderation (c2 × theta)

  // Random effects (non-centred): intercept + stress slope + recovery slope
  vector<lower=0>[3] tau;     // SDs for random effects
  matrix[N_subj, 3] z_u;      // standard-normal variates
  real<lower=0> sigma_y;      // residual SD
}

transformed parameters {
  matrix[N_subj, 3] u;
  for (i in 1:N_subj) {
    u[i, 1] = z_u[i, 1] * tau[1];
    u[i, 2] = z_u[i, 2] * tau[2];
    u[i, 3] = z_u[i, 3] * tau[3];
  }
}

model {
  // -----------------------------------------------------------------------
  // Priors
  // -----------------------------------------------------------------------
  to_vector(theta) ~ normal(0, 1);
  sigma_ema ~ exponential(1);

  // NNE intercept: centred on typical female NNE (~-27 dB)
  alpha ~ normal(-27, 3);
  b1    ~ normal(0, 3);
  b2    ~ normal(0, 3);

  a_trait ~ normal(0, 2);
  g1      ~ normal(0, 2);
  g2      ~ normal(0, 2);

  tau             ~ exponential(0.5);
  to_vector(z_u)  ~ normal(0, 1);
  sigma_y         ~ exponential(1);     // residual SDs of ~1 dB are plausible

  // -----------------------------------------------------------------------
  // Measurement model (EMA observations → latent traits)
  // -----------------------------------------------------------------------
  for (n in 1:N_ema) {
    for (d in 1:D) {
      X[n, d] ~ normal(theta[subj_ema[n], d], sigma_ema[d]);
    }
  }

  // -----------------------------------------------------------------------
  // Outcome model (NNE)
  // -----------------------------------------------------------------------
  for (j in 1:N_voice) {
    int i = subj_voice[j];

    real mu = alpha
      + u[i, 1]
      + (b1 + u[i, 2]) * c1[j]
      + (b2 + u[i, 3]) * c2[j];

    for (d in 1:D) {
      mu += a_trait[d] * theta[i, d]
          + g1[d]       * c1[j] * theta[i, d]
          + g2[d]       * c2[j] * theta[i, d];
    }

    y[j] ~ normal(mu, sigma_y);
  }
}

generated quantities {
  vector[N_voice] y_rep;
  vector[N_voice] log_lik;

  for (j in 1:N_voice) {
    int i = subj_voice[j];

    real mu = alpha + u[i, 1]
      + (b1 + u[i, 2]) * c1[j]
      + (b2 + u[i, 3]) * c2[j];

    for (d in 1:D) {
      mu += a_trait[d] * theta[i, d]
          + g1[d]       * c1[j] * theta[i, d]
          + g2[d]       * c2[j] * theta[i, d];
    }

    y_rep[j]   = normal_rng(mu, sigma_y);
    log_lik[j] = normal_lpdf(y[j] | mu, sigma_y);
  }
}
