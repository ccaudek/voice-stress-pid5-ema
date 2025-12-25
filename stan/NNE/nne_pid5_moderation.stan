// ============================================================================
// nne_pid5_moderation.stan
// Voice outcome: NNE (aggregated across vowels)
// Moderation by latent PID-5 EMA traits (measurement error model)
// Contrasts: c1 (stress PRE vs BASELINE), c2 (recovery POST vs PRE)
// ============================================================================

data {
  int<lower=1> N_subj;

  // Voice
  int<lower=1> N_voice;
  array[N_voice] int<lower=1, upper=N_subj> subj_voice;
  vector[N_voice] y;
  vector[N_voice] c1;
  vector[N_voice] c2;

  // EMA measurement model
  int<lower=1> N_ema;
  array[N_ema] int<lower=1, upper=N_subj> subj_ema;
  int<lower=1> D;
  matrix[N_ema, D] X; // z-scored, imputed (no NA)
}

parameters {
  // Voice fixed effects
  real alpha;
  real b1;                 // main stress effect on NNE
  real b2;                 // main recovery effect on NNE
  vector[D] g1;            // moderation of stress by traits
  vector[D] g2;            // moderation of recovery by traits

  // Voice random effects (non-centered)
  vector[N_subj] z_u0;
  vector[N_subj] z_u1;
  vector[N_subj] z_u2;
  vector<lower=0>[3] tau;  // SDs for random intercept, c1 slope, c2 slope

  real<lower=0> sigma_y;   // residual SD

  // Latent traits (between-person)
  matrix[N_subj, D] theta;             // trait levels (latent)
  vector<lower=0>[D] sigma_ema;        // measurement noise per domain
}

transformed parameters {
  vector[N_subj] u0 = tau[1] * z_u0;
  vector[N_subj] u1 = tau[2] * z_u1;
  vector[N_subj] u2 = tau[3] * z_u2;
}

model {
  // Priors (weakly-informative)
  alpha ~ normal(-28, 5);      // NNE typical around -20/-30 in your descriptives
  b1 ~ normal(0, 5);
  b2 ~ normal(0, 5);

  g1 ~ normal(0, 3);
  g2 ~ normal(0, 3);

  tau ~ exponential(0.2);      // allows sizable between-person heterogeneity
  sigma_y ~ exponential(0.2);

  z_u0 ~ std_normal();
  z_u1 ~ std_normal();
  z_u2 ~ std_normal();

  // Latent traits prior (z-scale)
  to_vector(theta) ~ normal(0, 1);

  // EMA measurement noise (on z-scale)
  sigma_ema ~ exponential(2);

  // Measurement model
  for (n in 1:N_ema) {
    int s = subj_ema[n];
    for (d in 1:D) {
      X[n, d] ~ normal(theta[s, d], sigma_ema[d]);
    }
  }

  // Voice model
  for (n in 1:N_voice) {
    int s = subj_voice[n];

    real mod_stress = dot_product(g1, to_vector(theta[s]));
    real mod_recov  = dot_product(g2, to_vector(theta[s]));

    real mu = alpha
      + (b1 + mod_stress) * c1[n]
      + (b2 + mod_recov)  * c2[n]
      + u0[s] + u1[s] * c1[n] + u2[s] * c2[n];

    y[n] ~ normal(mu, sigma_y);
  }
}

generated quantities {
  // Simple effects for +1 SD on each trait (holding others at 0),
  // useful for interpretation in R without extra post-processing.
  vector[D] stress_effect_at_plus1sd;
  vector[D] recovery_effect_at_plus1sd;

  for (d in 1:D) {
    stress_effect_at_plus1sd[d]   = b1 + g1[d];
    recovery_effect_at_plus1sd[d] = b2 + g2[d];
  }
}
