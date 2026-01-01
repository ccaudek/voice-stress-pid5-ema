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

  // subject random effects
  vector[N_subj] u0;
  vector[N_subj] u1;
  vector[N_subj] u2;
  real<lower=0> tau0;
  real<lower=0> tau1;
  real<lower=0> tau2;
  real<lower=0> sigma_y;

  // latent EMA trait (true score for each domain)
  matrix[N_subj, D_ema] theta;
  vector<lower=0>[D_ema] sigma_ema;
}

model {
  // priors
  alpha ~ normal(0, 50);
  b1 ~ normal(0, 10);
  b2 ~ normal(0, 10);

  g1_ema ~ normal(0, 10);
  g2_ema ~ normal(0, 10);
  g1_base ~ normal(0, 10);
  g2_base ~ normal(0, 10);

  tau0 ~ normal(0, 20);
  tau1 ~ normal(0, 10);
  tau2 ~ normal(0, 10);

  u0 ~ normal(0, tau0);
  u1 ~ normal(0, tau1);
  u2 ~ normal(0, tau2);

  sigma_y ~ normal(0, 20);

  to_vector(theta) ~ normal(0, 1);
  sigma_ema ~ normal(0, 1);

  // measurement model
  for (n in 1:N_ema) {
    int s = subj_ema[n];
    for (d in 1:D_ema) {
      X[n, d] ~ normal(theta[s, d], sigma_ema[d]);
    }
  }

  // voice model
  for (n in 1:N_voice) {
    int s = subj_voice[n];

    real b1s = b1
      + dot_product(g1_ema, theta[s]')
      + dot_product(g1_base, Z[s]');

    real b2s = b2
      + dot_product(g2_ema, theta[s]')
      + dot_product(g2_base, Z[s]');

    y[n] ~ normal(alpha + u0[s] + (b1s + u1[s]) * c1[n] + (b2s + u2[s]) * c2[n], sigma_y);
  }
}

generated quantities{
  real delta_stress = b1 - 0.5*b2;
  real delta_recovery = b2 - 0.5*b1;
  
  // log_lik for LOO-CV
  vector[N_voice] log_lik;
  for (n in 1:N_voice) {
    int s = subj_voice[n];
    
    real b1s = b1
      + dot_product(g1_ema, theta[s]')
      + dot_product(g1_base, Z[s]');
    
    real b2s = b2
      + dot_product(g2_ema, theta[s]')
      + dot_product(g2_base, Z[s]');
    
    real mu = alpha + u0[s] + (b1s + u1[s]) * c1[n] + (b2s + u2[s]) * c2[n];
    log_lik[n] = normal_lpdf(y[n] | mu, sigma_y);
  }
  
  // posterior predictive for checking
  vector[N_voice] y_rep;
  for (n in 1:N_voice) {
    int s = subj_voice[n];
    
    real b1s = b1
      + dot_product(g1_ema, theta[s]')
      + dot_product(g1_base, Z[s]');
    
    real b2s = b2
      + dot_product(g2_ema, theta[s]')
      + dot_product(g2_base, Z[s]');
    
    real mu = alpha + u0[s] + (b1s + u1[s]) * c1[n] + (b2s + u2[s]) * c2[n];
    y_rep[n] = normal_rng(mu, sigma_y);
  }
}
