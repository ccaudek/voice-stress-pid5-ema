# ==============================================================================
# 08_mfcc_omnibus_bayesian.R
# Omnibus BAYESIANO a bassa complessita' per l'effetto stress/recovery sul
# profilo MFCC della frase. Sostituisce la T^2 di Hotelling.
#
# Indice: test di WALD sulla POSTERIOR (analogo bayesiano della T^2 di Hotelling):
#     M = beta_bar' V^{-1} beta_bar
#   con  beta_bar = media a posteriori del vettore di shift (K=13 coefficienti)
#        V        = covarianza a posteriori dello stesso vettore
#   Riferimento chi^2_K (approssimazione MVN della posterior, grande campione),
#   esattamente come Hotelling usa un riferimento F.
#
# Perche' QUESTO e non un Bayes factor (bridge sampling / Savage-Dickey):
#   * COSTO: usa SOLO i draws gia' salvati di fit_full -> nessun refit, nessuna
#     stima di marginal likelihood. La piu' economica delle opzioni.
#   * ROBUSTEZZA: un BF sul null puntuale 13-dim contro prior N(0,1) e' dominato
#     dal fattore di Occam (paradosso di Lindley/Bartlett) ed e' prior-dipendente:
#     con effetti per-coefficiente modesti (~0.2 SD) tenderebbe a favorire il null
#     "per complessita'", contraddicendo Hotelling. Bridge sampling sarebbe inoltre
#     instabile in ~1800 dimensioni e macchinoso con cmdstanr.
#   * V e' la covarianza A POSTERIORI di beta (incorpora struttura gerarchica e
#     shrinkage del modello), non la covarianza grezza dei punteggi-differenza.
# ==============================================================================

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(tidyverse)
  library(here)
})

K <- 13L
contrasts_par <- c(stress = "beta1", recovery = "beta2") # contrasto -> parametro Stan

# ----------------------------
# 1) CARICA IL FIT GIA' STIMATO (nessun refit)
# ----------------------------
fit_path <- here("results", "mfcc", "models", "fit_mfcc_main_effects.rds")
bundle_path <- here("results", "mfcc", "mfcc_analysis_bundle.rds")

if (file.exists(fit_path)) {
  fit_full <- readRDS(fit_path) # CmdStanMCMC salvato con $save_object()
} else if (file.exists(bundle_path)) {
  fit_full <- readRDS(bundle_path)$fit_full
} else {
  stop("Fit non trovato: esegui prima 02_mfcc_main_effects_analysis.R")
}

# ----------------------------
# 2) FUNZIONE: omnibus di Wald a posteriori
# ----------------------------
bayes_wald_omnibus <- function(fit, par, K) {
  vars <- sprintf("%s[%d]", par, seq_len(K))
  B <- posterior::as_draws_matrix(fit$draws(variables = vars)) # ndraws x K
  beta_bar <- colMeans(B)
  V <- cov(B) # covarianza a posteriori (K x K)
  M <- as.numeric(crossprod(beta_bar, solve(V, beta_bar))) # beta' V^{-1} beta
  tibble(
    M_wald = M,
    df = K,
    p_omnibus = pchisq(M, df = K, lower.tail = FALSE),
    cond_V = kappa(V) # condizionamento di V (diagnostica)
  )
}

# ----------------------------
# 3) CALCOLA PER STRESS E RECOVERY
# ----------------------------
omnibus_bayes <- imap_dfr(contrasts_par, function(par, label) {
  bayes_wald_omnibus(fit_full, par, K) |>
    mutate(contrast = label, .before = 1)
})

cat("\n=== OMNIBUS BAYESIANO (Wald a posteriori, riferimento chi^2_13) ===\n")
print(as.data.frame(omnibus_bayes), digits = 4)
#   contrast M_wald df p_omnibus cond_V
# 1   stress  22.33 13   0.05044  9.236
# 2 recovery  10.47 13   0.65493  9.554

cat(
  "\n[riferimento frequentista del primo passo, Hotelling]:",
  "stress p ~ .024 ; recovery p ~ .11\n"
)
cat(
  "\nNOTA: M_wald e p_omnibus sono una CALIBRAZIONE FREQUENTISTA descrittiva del\n",
  "profilo multivariato, da riportare come riferimento e NON come test a soglia\n",
  "(nessun verdetto a .05). L'omnibus bayesiano di riferimento e' la distanza di\n",
  "Mahalanobis a posteriori (mediana + CrI 89%) calcolata in 02_mfcc_main_effects;\n",
  "l'inferenza sostantiva resta per-coefficiente (mediana, CrI 89%, pd).\n",
  sep = ""
)

# ----------------------------
# 4) SALVA
# ----------------------------
dir.create(
  here("results", "mfcc", "tables"),
  recursive = TRUE,
  showWarnings = FALSE
)
write_csv(
  omnibus_bayes,
  here("results", "mfcc", "tables", "mfcc_omnibus_bayes_wald.csv")
)
cat("\nSalvato: results/mfcc/tables/mfcc_omnibus_bayes_wald.csv\n")

# eof ---
