// ==============================================================================
// within_person_covariation.stan
// Modello per covariazione within-person tra PID-5 EMA e caratteristiche vocali
//
// STRUTTURA:
// - Livello 1 (within-person): deviazioni momentanee PID-5 predicono deviazioni F0
// - Livello 2 (between-person): intercette random per soggetto
// - Errore di misurazione su PID-5 e voce
//
// PARAMETRI CHIAVE:
// - beta_wp: coefficienti within-person (effetto delle deviazioni PID-5 su F0)
// - sigma_subj: SD delle intercette random tra soggetti
// - sigma_y: SD residua dell'outcome vocale
// ==============================================================================

data {
  // Dimensioni
  int<lower=1> N;              // N osservazioni totali (N_subj * 3 timepoint)
  int<lower=1> N_subj;         // N soggetti
  int<lower=1> D;              // N domini PID-5 (5)
  
  // Indici
  array[N] int<lower=1,upper=N_subj> subj;  // Indice soggetto per ogni obs
  array[N] int<lower=1,upper=3> time;       // Timepoint (1=baseline, 2=pre, 3=post)
  
  // Dati osservati
  matrix[N, D] X_wp;           // Deviazioni within-person PID-5 (N x D)
  vector[N] y_wp;              // Deviazioni within-person F0 (N)
}

transformed data {
  // Centramento globale (già fatto in R, ma per sicurezza)
  vector[D] X_mean = rep_vector(0.0, D);
  real y_mean = 0.0;
  
  for (d in 1:D) {
    X_mean[d] = mean(X_wp[, d]);
  }
  y_mean = mean(y_wp);
}

parameters {
  // Effetti fissi within-person
  vector[D] beta_wp;           // Coefficienti WP: quanto le deviazioni PID-5 
                               // predicono deviazioni F0
  
  real alpha;                  // Intercetta globale (dovrebbe essere ~0)
  
  // Effetti random
  vector[N_subj] z_subj;       // Random intercepts (non-centrato)
  real<lower=0> sigma_subj;    // SD random intercepts
  
  // Errore residuo
  real<lower=0> sigma_y;       // SD residua outcome
}

transformed parameters {
  // Random intercepts (centrati)
  vector[N_subj] alpha_subj = sigma_subj * z_subj;
}

model {
  // Priors
  
  // Intercetta globale (debolmente informativo attorno a 0)
  alpha ~ normal(0, 5);
  
  // Coefficienti within-person (weakly informative)
  beta_wp ~ normal(0, 2);
  
  // Random effects
  z_subj ~ std_normal();       // Non-centered parameterization
  sigma_subj ~ normal(0, 10);  // Half-normal (automatico con <lower=0>)
  
  // Residual error
  sigma_y ~ normal(0, 20);     // Half-normal
  
  // Likelihood
  for (n in 1:N) {
    y_wp[n] ~ normal(
      alpha + alpha_subj[subj[n]] + X_wp[n,] * beta_wp,
      sigma_y
    );
  }
}

generated quantities {
  // Posterior predictive check
  vector[N] y_rep;
  vector[N] log_lik;
  
  // R² within-person
  real var_fitted = variance(X_wp * beta_wp);
  real var_resid = square(sigma_y);
  real r2_within = var_fitted / (var_fitted + var_resid);
  
  // Correlazioni within-person per dominio
  vector[D] cor_wp;
  
  for (n in 1:N) {
    real mu_n = alpha + alpha_subj[subj[n]] + X_wp[n,] * beta_wp;
    y_rep[n] = normal_rng(mu_n, sigma_y);
    log_lik[n] = normal_lpdf(y_wp[n] | mu_n, sigma_y);
  }
  
  // Approssimazione correlazioni (assumendo SD ~ costanti)
  for (d in 1:D) {
    real sd_x = sd(X_wp[, d]);
    real sd_y_fitted = sd(y_wp);
    
    if (sd_x > 0 && sd_y_fitted > 0) {
      cor_wp[d] = (beta_wp[d] * sd_x) / sd_y_fitted;
    } else {
      cor_wp[d] = 0;
    }
  }
}
