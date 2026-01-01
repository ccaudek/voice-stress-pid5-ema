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

  // NON-CENTERED parametrization for random effects
  matrix[N_subj, 3] z_u;  // standard normals [intercept, slope1, slope2]
  
  vector<lower=0>[3] tau;  // [tau0, tau1, tau2]
  real<lower=0> sigma_y;
}

transformed parameters {
  // Reconstruct random effects from standardized versions
  matrix[N_subj, 3] u;
  for (i in 1:N_subj) {
    u[i, 1] = z_u[i, 1] * tau[1];  // u0
    u[i, 2] = z_u[i, 2] * tau[2];  // u1
    u[i, 3] = z_u[i, 3] * tau[3];  // u2
  }
}

model {
  // Priors (more informative)
  alpha ~ normal(0, 50);
  b1 ~ normal(0, 10);
  b2 ~ normal(0, 10);
  g1 ~ normal(0, 5);  // tighter prior for moderation
  g2 ~ normal(0, 5);

  // Exponential priors for scale parameters (better than half-normal)
  tau ~ exponential(0.5);
  sigma_y ~ exponential(0.1);
  
  // Standard normals for non-centered parametrization
  to_vector(z_u) ~ std_normal();

  // Vectorized likelihood
  {
    vector[N_voice] mu;
    for (n in 1:N_voice) {
      int s = subj_voice[n];
      real b1s = b1 + dot_product(g1, Z[s]);
      real b2s = b2 + dot_product(g2, Z[s]);
      mu[n] = alpha + u[s, 1] + (b1s + u[s, 2]) * c1[n] + (b2s + u[s, 3]) * c2[n];
    }
    y ~ normal(mu, sigma_y);
  }
}

generated quantities {
  // implied contrasts under your coding (baseline(-.5,0), pre(.5,-.5), post(0,.5))
  real delta_stress = b1 - 0.5*b2;   // E[pre]-E[baseline]
  real delta_recovery = b2 - 0.5*b1; // E[post]-E[pre]
  
  // log_lik for LOO-CV
  vector[N_voice] log_lik;
  for (n in 1:N_voice) {
    int s = subj_voice[n];
    real b1s = b1 + dot_product(g1, Z[s]);
    real b2s = b2 + dot_product(g2, Z[s]);
    real mu = alpha + u[s, 1] + (b1s + u[s, 2]) * c1[n] + (b2s + u[s, 3]) * c2[n];
    log_lik[n] = normal_lpdf(y[n] | mu, sigma_y);
  }
  
  // posterior predictive for checking
  vector[N_voice] y_rep;
  for (n in 1:N_voice) {
    int s = subj_voice[n];
    real b1s = b1 + dot_product(g1, Z[s]);
    real b2s = b2 + dot_product(g2, Z[s]);
    real mu = alpha + u[s, 1] + (b1s + u[s, 2]) * c1[n] + (b2s + u[s, 3]) * c2[n];
    y_rep[n] = normal_rng(mu, sigma_y);
  }
}
