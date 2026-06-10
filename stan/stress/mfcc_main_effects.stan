// =============================================================================
// mfcc_main_effects.stan
// Modello gerarchico MULTIVARIATO per i MFCC della frase (K coefficienti).
// Test OMNIBUS dell'effetto dello stress da esame sul profilo MFCC.
//
// Stesso disegno dei modelli f0_main_effects / nne_main_effects:
//   - contrasto stress    c1 (PRE vs BASELINE)
//   - contrasto recovery  c2 (POST vs PRE)
//   - effetti random a livello soggetto (misure ripetute)
// generalizzato a un outcome MULTIVARIATO: il vettore delle K medie MFCC.
//
// Scelta di modellazione (motivata):
//   * Intercetta random K-dimensionale per soggetto -> gestisce la dipendenza
//     entro-soggetto tra i 3 timepoint (l'analogo del grande tau1 in F0).
//   * NESSUNA slope random: con 3 osservazioni/soggetto e outcome a 13 dim,
//     2 slope random K-dim (= 39 effetti random/soggetto) NON sono identificabili
//     (cfr. tau2/tau3 quasi non identificati gia' nel modello univariato).
//   * Y standardizzato per coefficiente (z-score) a monte => effetti in unita'
//     di SD e priors sulla scala SD (NB: NON le priors in Hz dei modelli F0).
//
// Inferenza omnibus (in generated quantities):
//   D_stress / D_recovery = lunghezza di Mahalanobis del vettore di shift,
//   sqrt(beta' Sigma^-1 beta). E' l'analogo bayesiano della T^2 di Hotelling
//   (direttamente confrontabile con la D frequentista del primo passo).
// =============================================================================

data {
  int<lower=1> N_obs;                                  // righe soggetto x timepoint
  int<lower=1> N_subj;                                 // soggetti
  int<lower=1> K;                                      // n. coefficienti MFCC (13)
  array[N_obs] int<lower=1, upper=N_subj> subj_id;     // indice soggetto per riga
  matrix[N_obs, K] Y;                                  // MFCC medie standardizzate
  vector[N_obs] c1;                                    // contrasto stress   (PRE vs BASELINE)
  vector[N_obs] c2;                                    // contrasto recovery (POST vs PRE)
}

parameters {
  vector[K] alpha;                  // media generale per coefficiente
  vector[K] beta1;                  // effetto stress   per coefficiente
  vector[K] beta2;                  // effetto recovery per coefficiente

  // intercette random per soggetto (non-centrate), K-dimensionali
  matrix[K, N_subj] z_u;
  cholesky_factor_corr[K] L_u;      // correlazioni tra intercette random
  vector<lower=0>[K] tau_u;         // SD delle intercette random

  // covarianza residua (entro soggetto-occasione)
  cholesky_factor_corr[K] L_e;
  vector<lower=0>[K] sigma;         // SD residue per coefficiente
}

transformed parameters {
  matrix[N_subj, K] u;              // deviazioni di intercetta specifiche del soggetto
  u = (diag_pre_multiply(tau_u, L_u) * z_u)';
}

model {
  // ---- priors (outcome standardizzato -> scala SD) ----
  alpha ~ normal(0, 2);
  beta1 ~ normal(0, 1);             // debolmente informativo + lieve regolarizzazione
  beta2 ~ normal(0, 1);             // (aiuta contro la molteplicita' su 13 coefficienti)
  tau_u ~ exponential(1);
  sigma ~ exponential(1);
  L_u   ~ lkj_corr_cholesky(2);
  L_e   ~ lkj_corr_cholesky(2);
  to_vector(z_u) ~ std_normal();

  // ---- verosimiglianza multivariata ----
  {
    matrix[K, K] L_Sigma = diag_pre_multiply(sigma, L_e);
    for (n in 1:N_obs) {
      vector[K] mu = alpha + to_vector(u[subj_id[n]]) + beta1 * c1[n] + beta2 * c2[n];
      target += multi_normal_cholesky_lpdf(to_vector(Y[n]) | mu, L_Sigma);
    }
  }
}

generated quantities {
  vector[N_obs] log_lik;            // log-lik puntuale (LOO: 1 vettore K per riga)
  matrix[N_obs, K] Y_rep;           // replicati per posterior predictive checks
  real D_stress;                    // omnibus: || L_Sigma^-1 beta1 ||
  real D_recovery;                  // omnibus: || L_Sigma^-1 beta2 ||
  {
    matrix[K, K] L_Sigma = diag_pre_multiply(sigma, L_e);
    for (n in 1:N_obs) {
      vector[K] mu = alpha + to_vector(u[subj_id[n]]) + beta1 * c1[n] + beta2 * c2[n];
      log_lik[n] = multi_normal_cholesky_lpdf(to_vector(Y[n]) | mu, L_Sigma);
      Y_rep[n]   = to_row_vector(multi_normal_cholesky_rng(mu, L_Sigma));
    }
    // D = sqrt( beta' Sigma^{-1} beta ) = || L_Sigma^{-1} beta ||
    D_stress   = sqrt(dot_self(mdivide_left_tri_low(L_Sigma, beta1)));
    D_recovery = sqrt(dot_self(mdivide_left_tri_low(L_Sigma, beta2)));
  }
}
