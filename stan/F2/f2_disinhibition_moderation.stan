// ==============================================================================
// f2_disinhibition_moderation.stan
// Moderazione Disinhibition su metriche articolatorie (F2, VSA)
//
// OBIETTIVO:
// Testare se Disinhibition (latent trait) modera effetti stress su articolazione
//
// IPOTESI:
// Disinhibition × Stress → maggiore centralizzazione vocalica / 
// ridotta precisione articolatoria
//
// FEATURES:
// - Latent trait Disinhibition con measurement error
// - Contrasti stress (c1: baseline→pre, c2: pre→post)  
// - Interazione Disinhibition × c1_stress
// - Measurement error su outcome (F2 più rumoroso di F0)
// - Random intercepts e slopes
// ==============================================================================

data {
  // Dimensioni
  int<lower=1> N_subj;         // N soggetti
  
  // Voice measurements
  int<lower=1> N_voice;        // N osservazioni vocali (N_subj × 3)
  array[N_voice] int<lower=1,upper=N_subj> subj_voice;
  vector[N_voice] y;           // Outcome (VSA_log, F2_range_norm, ecc.)
  vector[N_voice] c1;          // Contrasto stress (baseline→pre)
  vector[N_voice] c2;          // Contrasto recovery (pre→post)
  
  // EMA measurements (per latent trait estimation)
  int<lower=1> N_ema;          // N misurazioni EMA totali
  array[N_ema] int<lower=1,upper=N_subj> subj_ema;
  int<lower=1> D;              // N domini PID-5 (5)
  matrix[N_ema, D] X;          // PID-5 EMA observations (standardized)
}

parameters {
  // === VOICE MODEL ===
  
  // Fixed effects
  real alpha;                  // Intercetta globale
  real beta_c1;                // Effetto stress (main effect)
  real beta_c2;                // Effetto recovery
  
  // === FOCUS: DISINHIBITION MODERATION ===
  real beta_disinhibition;     // Main effect Disinhibition (between-person)
  real beta_c1_x_disinhibition;// Interazione Disinhibition × Stress
  
  // (Opzionale) Altri domini come covariati
  vector[D-1] beta_other;      // Altri 4 domini (no Disinhibition)
  
  // Random effects
  vector[N_subj] z_alpha;      // Random intercepts (non-centered)
  vector[N_subj] z_beta_c1;    // Random slopes stress (non-centered)
  
  real<lower=0> sigma_alpha;   // SD random intercepts
  real<lower=0> sigma_beta_c1; // SD random slopes
  real<lower=0,upper=1> rho_alpha_beta;  // Correlazione intercepts-slopes
  
  // Residual
  real<lower=0> sigma_y;       // SD residua (measurement error outcome)
  
  // === LATENT TRAIT MODEL ===
  
  vector[N_subj] eta_disinhibition;  // Latent Disinhibition per soggetto
  
  // Hyperparameters latent traits
  real mu_disinhibition;       // Media Disinhibition
  real<lower=0> sigma_disinhibition;  // SD Disinhibition
  
  // Measurement error EMA
  vector<lower=0>[D] sigma_x;  // SD measurement error per dominio
}

transformed parameters {
  // Random effects (centered parameterization)
  vector[N_subj] alpha_subj;
  vector[N_subj] beta_c1_subj;
  
  // Matrice covarianza random effects (2×2)
  matrix[2, 2] Sigma_re;
  
  // Cholesky factor per sampling efficiente
  {
    matrix[2, 2] L_re;
    vector[2] sigma_re = [sigma_alpha, sigma_beta_c1]';
    
    // Correlation matrix
    matrix[2, 2] R_re;
    R_re[1, 1] = 1.0;
    R_re[2, 2] = 1.0;
    R_re[1, 2] = rho_alpha_beta;
    R_re[2, 1] = rho_alpha_beta;
    
    // Covariance matrix
    Sigma_re = quad_form_diag(R_re, sigma_re);
    
    // Cholesky decomposition
    L_re = cholesky_decompose(Sigma_re);
    
    // Random effects
    for (i in 1:N_subj) {
      vector[2] z = [z_alpha[i], z_beta_c1[i]]';
      vector[2] re = L_re * z;
      alpha_subj[i] = re[1];
      beta_c1_subj[i] = re[2];
    }
  }
}

model {
  // === PRIORS ===
  
  // Fixed effects voice model
  alpha ~ normal(0, 10);
  beta_c1 ~ normal(0, 5);
  beta_c2 ~ normal(0, 5);
  
  // Disinhibition effects (key parameters)
  beta_disinhibition ~ normal(0, 3);
  beta_c1_x_disinhibition ~ normal(0, 2);  // Interaction (più conservativo)
  
  // Altri domini (covariati)
  beta_other ~ normal(0, 2);
  
  // Random effects
  z_alpha ~ std_normal();
  z_beta_c1 ~ std_normal();
  
  sigma_alpha ~ normal(0, 5);
  sigma_beta_c1 ~ normal(0, 3);
  rho_alpha_beta ~ uniform(-1, 1);  // O beta(2, 2) per prior weakly informative
  
  // Residual
  sigma_y ~ normal(0, 10);  // F2 più rumoroso di F0 → prior più ampio
  
  // === LATENT TRAIT MODEL ===
  
  // Hyperparameters
  mu_disinhibition ~ normal(0, 1);
  sigma_disinhibition ~ normal(0, 1);
  
  // Latent traits
  eta_disinhibition ~ normal(mu_disinhibition, sigma_disinhibition);
  
  // Measurement error EMA
  sigma_x ~ normal(0, 0.5);
  
  // Measurement model: EMA observations
  for (n in 1:N_ema) {
    int s = subj_ema[n];
    
    // Disinhibition (colonna 4 in PID-5)
    X[n, 4] ~ normal(eta_disinhibition[s], sigma_x[4]);
    
    // Altri domini (mean 0, sd 1 perché standardizzati)
    // Negative Affectivity (1)
    X[n, 1] ~ normal(0, sigma_x[1]);
    // Detachment (2)
    X[n, 2] ~ normal(0, sigma_x[2]);
    // Antagonism (3)
    X[n, 3] ~ normal(0, sigma_x[3]);
    // Psychoticism (5)
    X[n, 5] ~ normal(0, sigma_x[5]);
  }
  
  // === VOICE MODEL LIKELIHOOD ===
  
  for (n in 1:N_voice) {
    int s = subj_voice[n];
    
    // Predittore lineare
    real mu = alpha + alpha_subj[s] +
              (beta_c1 + beta_c1_subj[s]) * c1[n] +
              beta_c2 * c2[n] +
              beta_disinhibition * eta_disinhibition[s] +
              beta_c1_x_disinhibition * c1[n] * eta_disinhibition[s];
    
    // (Opzionale) Aggiungi altri domini come covariati
    // mu += beta_other[1] * eta_negative_affectivity[s];
    // ...
    
    // Likelihood con measurement error
    y[n] ~ normal(mu, sigma_y);
  }
}

generated quantities {
  // Posterior predictive check
  vector[N_voice] y_rep;
  vector[N_voice] log_lik;
  
  // R² (marginal - fixed effects only)
  real var_pred = variance(
    alpha + 
    beta_c1 * c1 + 
    beta_c2 * c2
  );
  real var_resid = square(sigma_y) + square(sigma_alpha);
  real r2_marginal = var_pred / (var_pred + var_resid);
  
  // Effetto stress condizionale su Disinhibition
  // A basso Disinhibition (-1 SD)
  real effect_stress_low_disinhibition = beta_c1 + beta_c1_x_disinhibition * (-1);
  
  // A alto Disinhibition (+1 SD)
  real effect_stress_high_disinhibition = beta_c1 + beta_c1_x_disinhibition * (1);
  
  // Difference in differences
  real moderation_effect = effect_stress_high_disinhibition - effect_stress_low_disinhibition;
  
  // PPC
  for (n in 1:N_voice) {
    int s = subj_voice[n];
    
    real mu_n = alpha + alpha_subj[s] +
                (beta_c1 + beta_c1_subj[s]) * c1[n] +
                beta_c2 * c2[n] +
                beta_disinhibition * eta_disinhibition[s] +
                beta_c1_x_disinhibition * c1[n] * eta_disinhibition[s];
    
    y_rep[n] = normal_rng(mu_n, sigma_y);
    log_lik[n] = normal_lpdf(y[n] | mu_n, sigma_y);
  }
}
