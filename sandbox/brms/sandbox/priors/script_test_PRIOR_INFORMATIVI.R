# ============================================================
# 00) Setup
# ============================================================
library(tidyverse)
library(brms)
library(tidybayes)

# (consigliato per cmdstanr)
options(mc.cores = parallel::detectCores())
set.seed(123)

# ============================================================
# 01) Carica dati
# ============================================================
df_analysis <- readRDS("results/df_analysis.rds")

# ============================================================
# 02) Contrasti + id (solo se mancanti)
# ============================================================
needed <- c("c1_stress", "c2_recovery", "id")
if (!all(needed %in% names(df_analysis))) {
  df_analysis <- df_analysis %>%
    mutate(
      # contrasti ortogonali per 3 timepoint:
      # c1: baseline (-0.5) vs pre-exam (+0.5), post-exam 0
      c1_stress = case_when(
        timepoint == "baseline" ~ -0.5,
        timepoint == "pre-exam" ~ 0.5,
        timepoint == "post-exam" ~ 0,
        TRUE ~ NA_real_
      ),
      # c2: pre-exam (-0.5) vs post-exam (+0.5), baseline 0
      c2_recovery = case_when(
        timepoint == "baseline" ~ 0,
        timepoint == "pre-exam" ~ -0.5,
        timepoint == "post-exam" ~ 0.5,
        TRUE ~ NA_real_
      ),
      id = factor(ID)
    )
} else {
  df_analysis <- df_analysis %>% mutate(id = factor(id))
}

# ============================================================
# 03) Definisci predittori usati nel modello (evita NA warning)
# ============================================================
pid5_vars <- c(
  "pid5_negative_affectivity_c",
  "pid5_detachment_c",
  "pid5_antagonism_c",
  "pid5_disinhibition_c",
  "pid5_psychoticism_c"
)

required_cols <- c(
  "id",
  "ID",
  "timepoint",
  "c1_stress",
  "c2_recovery",
  "f2_mean_a",
  pid5_vars
)

# Se mancano colonne: stop esplicito (meglio che errori silenziosi)
missing_cols <- setdiff(required_cols, names(df_analysis))
if (length(missing_cols) > 0) {
  stop(
    "Mancano queste colonne in df_analysis: ",
    paste(missing_cols, collapse = ", ")
  )
}

df_test <- df_analysis %>%
  select(all_of(required_cols)) %>%
  drop_na() # IMPORTANTISSIMO: elimina NA in tutti i predittori usati

cat("N observations:", nrow(df_test), "\n")
cat("N participants:", n_distinct(df_test$ID), "\n")

# ============================================================
# 04) Winsorizzazione (SCEGLI UNA SOLA MODALITÀ)
# ============================================================
winsor_mode <- "within_id" # "global" oppure "within_id"
p_lo <- 0.01
p_hi <- 0.99

if (winsor_mode == "global") {
  q <- quantile(df_test$f2_mean_a, probs = c(p_lo, p_hi), na.rm = TRUE)
  df_test <- df_test %>%
    mutate(
      f2_mean_a_w = pmin(pmax(f2_mean_a, q[1]), q[2])
    )
} else if (winsor_mode == "within_id") {
  # winsorizza per soggetto: spesso risolve "non funziona completamente"
  df_test <- df_test %>%
    group_by(id) %>%
    mutate(
      qlo = quantile(f2_mean_a, p_lo, na.rm = TRUE),
      qhi = quantile(f2_mean_a, p_hi, na.rm = TRUE),
      f2_mean_a_w = pmin(pmax(f2_mean_a, qlo), qhi)
    ) %>%
    ungroup() %>%
    select(-qlo, -qhi)
} else {
  stop("winsor_mode deve essere 'global' o 'within_id'")
}

# Verifica che la winsorizzazione abbia davvero cambiato qualcosa
cat("\n=== CHECK WINSORIZATION ===\n")
cat("Range original:", paste(range(df_test$f2_mean_a), collapse = " - "), "\n")
cat(
  "Range winsor  :",
  paste(range(df_test$f2_mean_a_w), collapse = " - "),
  "\n"
)
cat("Prop changed  :", mean(df_test$f2_mean_a != df_test$f2_mean_a_w), "\n")

# ============================================================
# 05) Statistiche per prior (SULLA VARIABILE WINSORIZZATA!)
# ============================================================
f2_stats <- df_test %>%
  summarise(
    mean = mean(f2_mean_a_w),
    sd = sd(f2_mean_a_w)
  )

mu_mean <- as.numeric(f2_stats$mean)
mu_sd <- as.numeric(f2_stats$sd)

cat("\n=== STATISTICHE F2 MEAN WINSORIZED (/a/) ===\n")
cat("Mean:", round(mu_mean, 1), "Hz\n")
cat("SD  :", round(mu_sd, 1), "Hz\n")

# ============================================================
# 06) Modello
# ============================================================
formula_test <- bf(
  f2_mean_a_w ~
    1 +
      c1_stress *
        (pid5_negative_affectivity_c +
          pid5_detachment_c +
          pid5_antagonism_c +
          pid5_disinhibition_c +
          pid5_psychoticism_c) +
      c2_recovery:(pid5_negative_affectivity_c +
        pid5_detachment_c +
        pid5_antagonism_c +
        pid5_disinhibition_c +
        pid5_psychoticism_c) +
      (1 + c1_stress + c2_recovery | id),
  sigma ~ 1 + c1_stress + c2_recovery
)

# ============================================================
# 07) Prior informativi "sensati" e STABILI
#    (evita del tutto paste0/sprintf dentro prior() -> usa set_prior)
# ============================================================
# Intercetta mu: centrata sulla media, sd ampia ma non folle (circa 2*SD osservata)
p_mu_intercept <- paste0(
  "normal(",
  round(mu_mean, 0),
  ", ",
  round(2 * mu_sd, 0),
  ")"
)

# Coefficienti mu: effetti plausibili in Hz
# Se i pid5_*_c sono z-score, normal(0, 60) è spesso ragionevole;
# se sono solo centrati (non scalati), valuta 30 o 40.
p_b <- "normal(0, 60)"

# Intercetta sigma è su scala log in brms
p_sigma_intercept <- paste0("normal(", round(log(mu_sd), 2), ", 0.35)")

# Effetti su sigma (log): piccoli
p_b_sigma <- "normal(0, 0.2)"

# Random effects SD: evita exponential(0.01) (troppo permissivo)
p_sd <- "exponential(0.1)" # media 10

priors_informative <- c(
  set_prior(p_mu_intercept, class = "Intercept"),
  set_prior(p_b, class = "b"),
  set_prior(p_sigma_intercept, class = "Intercept", dpar = "sigma"),
  set_prior(p_b_sigma, class = "b", dpar = "sigma"),
  set_prior(p_sd, class = "sd"),
  set_prior("lkj(2)", class = "cor")
)

cat("\n=== PRIOR INFORMATIVI (FINAL) ===\n")
print(priors_informative)

# Controllo extra: se qui non vedi "normal(...)" ma nomi di oggetti, STOP
if (
  any(grepl("prior_|as\\.character|paste0|sprintf", priors_informative$prior))
) {
  stop(
    "I prior non sono stringhe finali: controlla la costruzione di priors_informative."
  )
}

# ============================================================
# 08) Verifica che lo Stan code NON contenga funzioni strane
# ============================================================
cat("\n=== STAN CODE CHECK (search for paste0/sprintf/as.character) ===\n")
stancode <- make_stancode(
  formula = formula_test,
  data = df_test,
  family = gaussian(),
  prior = priors_informative
)
cat(stancode)

if (grepl("paste0|sprintf|as\\.character", stancode)) {
  stop(
    "Stan code contiene paste0/sprintf/as.character: i prior NON sono stati passati correttamente."
  )
}

# ============================================================
# 09) PRIOR PREDICTIVE CHECK (sample_prior = 'only')
# ============================================================
cat("\n=== PRIOR PREDICTIVE SAMPLING ===\n")

m_prior <- brm(
  formula = formula_test,
  data = df_test,
  family = gaussian(),
  prior = priors_informative,
  sample_prior = "only",
  chains = 2,
  iter = 1000,
  warmup = 500,
  backend = "cmdstanr",
  seed = 123,
  refresh = 200
)

# PPC base
pp_check(m_prior, ndraws = 50)

# PPC con tidybayes: overlay più controllabile
yrep <- posterior_predict(m_prior, ndraws = 200)
yobs <- df_test$f2_mean_a_w

# semplice check numerico: media/SD delle repliche vs osservato
cat("\n=== PPC SUMMARY ===\n")
cat("Observed mean:", round(mean(yobs), 1), " SD:", round(sd(yobs), 1), "\n")
cat(
  "PriorPred mean (avg):",
  round(mean(rowMeans(yrep)), 1),
  " PriorPred SD (avg):",
  round(mean(apply(yrep, 1, sd)), 1),
  "\n"
)

# ============================================================
# 10) (Opzionale) Fit del modello vero (NON prior-only)
# ============================================================
# m_fit <- brm(
#   formula = formula_test,
#   data = df_test,
#   family = gaussian(),
#   prior = priors_informative,
#   chains = 4,
#   iter = 4000,
#   warmup = 2000,
#   backend = "cmdstanr",
#   seed = 123,
#   refresh = 200
# )
# summary(m_fit)
