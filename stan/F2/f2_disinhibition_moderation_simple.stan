// ==============================================================================
// f2_disinhibition_moderation_simple.stan
// Versione SEMPLIFICATA per testing iniziale
//
// Differenze vs. modello completo:
// - NO random slopes (solo random intercepts)
// - NO correlazione random effects
// - Più facile convergenza
// - Usa questa per verificare che dati/setup funzionano
// ==============================================================================

data {
  // Dimensioni
  int<lower=1> N_subj;
  
  // Voice measurements
  int<lower=1> N_voice;
  array[N_voice] int<lower=1,upper=N_subj> subj_voice;
  vector[N_voice] y;
  vector[N_voice] c1;
  vector[N_voice] c2;
  
  // EMA measurements
  int<lower=1> N_ema;
  array[N_ema] int<lower=1,upper=N_subj> subj_ema;
  int<lower=1> D;
  matrix[N_ema, D] X;
}

parameters {
  // Fixed effects
  real alpha;
  real beta_c1;
  real beta_c2;
  real beta_disinhibition;
  real beta_c1_x_disinhibition;
  
  // Random intercepts only (semplificato)
  vector[N_subj] z_alpha;
  real<lower=0> sigma_alpha;
  
  // Residual
  real<lower=0> sigma_y;
  
  // Latent trait
  vector[N_subj] eta_disinhibition;
  real mu_disinhibition;
  real<lower=0> sigma_disinhibition;
  
  // Measurement error
  vector<lower=0>[D] sigma_x;
}

transformed parameters {
  // Random intercepts (centered)
  vector[N_subj] alpha_subj = sigma_alpha * z_alpha;
}

model {
  // Priors
  alpha ~ normal(0, 10);
  beta_c1 ~ normal(0, 5);
  beta_c2 ~ normal(0, 5);
  beta_disinhibition ~ normal(0, 3);
  beta_c1_x_disinhibition ~ normal(0, 2);
  
  z_alpha ~ std_normal();
  sigma_alpha ~ normal(0, 5);
  sigma_y ~ normal(0, 10);
  
  // Latent trait
  mu_disinhibition ~ normal(0, 1);
  sigma_disinhibition ~ normal(0, 1);
  eta_disinhibition ~ normal(mu_disinhibition, sigma_disinhibition);
  
  sigma_x ~ normal(0, 0.5);
  
  // Measurement model
  for (n in 1:N_ema) {
    int s = subj_ema[n];
    
    // Disinhibition
    X[n, 4] ~ normal(eta_disinhibition[s], sigma_x[4]);
    
    // Altri domini
    X[n, 1] ~ normal(0, sigma_x[1]);
    X[n, 2] ~ normal(0, sigma_x[2]);
    X[n, 3] ~ normal(0, sigma_x[3]);
    X[n, 5] ~ normal(0, sigma_x[5]);
  }
  
  // Voice model
  for (n in 1:N_voice) {
    int s = subj_voice[n];
    
    real mu = alpha + alpha_subj[s] +
              beta_c1 * c1[n] +
              beta_c2 * c2[n] +
              beta_disinhibition * eta_disinhibition[s] +
              beta_c1_x_disinhibition * c1[n] * eta_disinhibition[s];
    
    y[n] ~ normal(mu, sigma_y);
  }
}

generated quantities {
  vector[N_voice] y_rep;
  vector[N_voice] log_lik;
  
  // Effetti condizionali
  real effect_stress_low_disinhibition = beta_c1 + beta_c1_x_disinhibition * (-1);
  real effect_stress_high_disinhibition = beta_c1 + beta_c1_x_disinhibition * (1);
  real moderation_effect = effect_stress_high_disinhibition - effect_stress_low_disinhibition;
  
  // PPC
  for (n in 1:N_voice) {
    int s = subj_voice[n];
    
    real mu_n = alpha + alpha_subj[s] +
                beta_c1 * c1[n] +
                beta_c2 * c2[n] +
                beta_disinhibition * eta_disinhibition[s] +
                beta_c1_x_disinhibition * c1[n] * eta_disinhibition[s];
    
    y_rep[n] = normal_rng(mu_n, sigma_y);
    log_lik[n] = normal_lpdf(y[n] | mu_n, sigma_y);
  }
}
