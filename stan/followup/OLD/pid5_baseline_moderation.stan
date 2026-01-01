data {
  int<lower=1> N_subj;
  int<lower=1> N_voice;
  array[N_voice] int<lower=1, upper=N_subj> subj_voice;
  vector[N_voice] y;
  vector[N_voice] c1;
  vector[N_voice] c2;

  int<lower=1> D_base;
  matrix[N_subj, D_base] Z; // baseline domains, standardized
}

parameters {
  real alpha;

  real b1;
  real b2;

  vector[D_base] g1;  // moderation on stress contrast
  vector[D_base] g2;  // moderation on recovery contrast

  vector[N_subj] u0;
  vector[N_subj] u1;
  vector[N_subj] u2;

  real<lower=0> tau0;
  real<lower=0> tau1;
  real<lower=0> tau2;

  real<lower=0> sigma_y;
}

model {
  // priors (weakly informative; adjust if you want)
  alpha ~ normal(0, 50);
  b1 ~ normal(0, 10);
  b2 ~ normal(0, 10);
  g1 ~ normal(0, 10);
  g2 ~ normal(0, 10);

  tau0 ~ normal(0, 20);
  tau1 ~ normal(0, 10);
  tau2 ~ normal(0, 10);

  u0 ~ normal(0, tau0);
  u1 ~ normal(0, tau1);
  u2 ~ normal(0, tau2);

  sigma_y ~ normal(0, 20);

  // likelihood
  for (n in 1:N_voice) {
    int s = subj_voice[n];
    real b1s = b1 + dot_product(g1, Z[s]');
    real b2s = b2 + dot_product(g2, Z[s]');
    y[n] ~ normal(alpha + u0[s] + (b1s + u1[s]) * c1[n] + (b2s + u2[s]) * c2[n], sigma_y);
  }
}

generated quantities{
  // implied contrasts under your coding (baseline(-.5,0), pre(.5,-.5), post(0,.5))
  real delta_stress = b1 - 0.5*b2;   // E[pre]-E[baseline]
  real delta_recovery = b2 - 0.5*b1; // E[post]-E[pre]
  
  // log_lik for LOO-CV
  vector[N_voice] log_lik;
  for (n in 1:N_voice) {
    int s = subj_voice[n];
    real b1s = b1 + dot_product(g1, Z[s]');
    real b2s = b2 + dot_product(g2, Z[s]');
    real mu = alpha + u0[s] + (b1s + u1[s]) * c1[n] + (b2s + u2[s]) * c2[n];
    log_lik[n] = normal_lpdf(y[n] | mu, sigma_y);
  }
  
  // posterior predictive for checking
  vector[N_voice] y_rep;
  for (n in 1:N_voice) {
    int s = subj_voice[n];
    real b1s = b1 + dot_product(g1, Z[s]');
    real b2s = b2 + dot_product(g2, Z[s]');
    real mu = alpha + u0[s] + (b1s + u1[s]) * c1[n] + (b2s + u2[s]) * c2[n];
    y_rep[n] = normal_rng(mu, sigma_y);
  }
}
