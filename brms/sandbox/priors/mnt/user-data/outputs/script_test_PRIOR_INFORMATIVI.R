# =============================================================================
# SCRIPT TEST CON PRIOR INFORMATIVI
# =============================================================================

library(tidyverse)
library(brms)
library(tidybayes)

df_analysis <- readRDS("results/df_analysis.rds")

# Crea contrasti se non esistono
if (!"c1_stress" %in% names(df_analysis)) {
  df_analysis <- df_analysis %>%
    mutate(
      c1_stress = case_when(
        timepoint == "baseline" ~ -0.5,
        timepoint == "pre-exam" ~ 0.5,
        timepoint == "post-exam" ~ 0,
        TRUE ~ NA_real_
      ),
      c2_recovery = case_when(
        timepoint == "baseline" ~ 0,
        timepoint == "pre-exam" ~ -0.5,
        timepoint == "post-exam" ~ 0.5,
        TRUE ~ NA_real_
      ),
      id = as.factor(ID)
    )
}

# Prepara dati
df_test <- df_analysis %>%
  select(id, ID, timepoint, c1_stress, c2_recovery,
         f2_mean_a, starts_with("pid5_")) %>%
  drop_na(f2_mean_a)

cat("N observations:", nrow(df_test), "\n")
cat("N participants:", n_distinct(df_test$ID), "\n")

# =============================================================================
# CALCOLA STATISTICHE PER PRIOR INFORMATIVI
# =============================================================================

f2_stats <- df_test %>%
  summarise(
    mean = mean(f2_mean_a, na.rm = TRUE),
    sd = sd(f2_mean_a, na.rm = TRUE)
  )

cat("\n=== STATISTICHE F2 MEAN (VOWEL /a/) ===\n")
cat("Mean:", round(f2_stats$mean, 1), "Hz\n")
cat("SD:", round(f2_stats$sd, 1), "Hz\n")
cat("Range:", round(f2_stats$mean - 2*f2_stats$sd, 0), "-", 
    round(f2_stats$mean + 2*f2_stats$sd, 0), "Hz (±2SD)\n\n")

# =============================================================================
# FORMULA
# =============================================================================

formula_test <- bf(
  f2_mean_a ~ 1 + 
    c1_stress * (pid5_negative_affectivity_c + pid5_detachment_c + 
                 pid5_antagonism_c + pid5_disinhibition_c + pid5_psychoticism_c) +
    c2_recovery:(pid5_negative_affectivity_c + pid5_detachment_c + 
                 pid5_antagonism_c + pid5_disinhibition_c + pid5_psychoticism_c) +
    (1 + c1_stress + c2_recovery | id),
  sigma ~ 1 + c1_stress + c2_recovery
)

# =============================================================================
# PRIOR INFORMATIVI
# =============================================================================

# Calcola parametri prior basati su dati
intercept_prior_mean <- round(f2_stats$mean, 0)
intercept_prior_sd <- round(f2_stats$sd * 2, 0)  # Permette ±2SD dalla media

sigma_intercept_log <- round(log(f2_stats$sd), 1)

priors_informative <- c(
  # Intercetta: centrata sulla media osservata
  prior(paste0("normal(", intercept_prior_mean, ", ", intercept_prior_sd, ")"), 
        class = "Intercept"),
  
  # Coefficienti: effetti plausibili di stress/personality
  # ±200 Hz copre range realistico (fino a ~15% della media)
  prior(normal(0, 100), class = "b"),
  
  # Intercetta sigma: sulla scala log
  prior(paste0("normal(", sigma_intercept_log, ", 0.5)"), 
        class = "Intercept", dpar = "sigma"),
  
  # Coefficienti sigma: piccoli cambiamenti proporzionali
  prior(normal(0, 0.3), class = "b", dpar = "sigma"),
  
  # Random effects: permette sostanziale variabilità tra soggetti
  prior(exponential(0.01), class = "sd")
)

cat("=== PRIOR INFORMATIVI SPECIFICATI ===\n\n")
print(priors_informative)

# =============================================================================
# PRIOR PREDICTIVE CHECK
# =============================================================================

cat("\n=== PRIOR PREDICTIVE CHECK ===\n")
cat("Sampling from prior...\n")

set.seed(123)
m_prior <- brm(
  formula = formula_test,
  data = df_test,
  family = gaussian(),
  prior = priors_informative,
  sample_prior = "only",
  chains = 1,
  iter = 200,
  warmup = 100,
  cores = 1,
  backend = "cmdstanr",
  seed = 123
)

# Plot prior predictive
library(bayesplot)

pp_prior <- pp_check(m_prior, type = "dens_overlay", ndraws = 50) +
  labs(
    title = "Prior Predictive Check - F2 Mean (vowel /a/)",
    subtitle = paste0("Dati: M=", round(f2_stats$mean, 0), 
                     " Hz, SD=", round(f2_stats$sd, 0), " Hz")
  ) +
  theme_minimal()

print(pp_prior)
ggsave("figures/prior_predictive_informative_f2.pdf", pp_prior, 
       width = 10, height = 6)

cat("\nPrior predictive plot salvato.\n")
cat("VERIFICA: Le curve yrep dovrebbero sovrapporsi ragionevolmente ai dati (y).\n")
cat("Se yrep è ancora troppo lontano dai dati, i prior vanno ulteriormente ajustati.\n\n")

# =============================================================================
# FIT MODELLO CON PRIOR INFORMATIVI
# =============================================================================

cat("=== FITTING MODELLO (versione breve) ===\n")
cat("Questo richiederà circa 10 minuti...\n\n")

m_test <- brm(
  formula = formula_test,
  data = df_test,
  family = gaussian(),
  prior = priors_informative,
  chains = 2,
  iter = 2000,
  warmup = 1000,
  cores = 2,
  control = list(adapt_delta = 0.95),
  backend = "cmdstanr",
  seed = 123,
  file = "models/test_f2_mean_a_informative"
)

# =============================================================================
# DIAGNOSTICA
# =============================================================================

cat("\n=== DIAGNOSTICA MODELLO ===\n")
print(summary(m_test))

cat("\nMax Rhat:", max(rhat(m_test), na.rm=TRUE), "\n")
cat("Min ESS:", min(neff_ratio(m_test), na.rm=TRUE), "\n")

# Posterior predictive check
pp_post <- pp_check(m_test, type = "dens_overlay", ndraws = 50) +
  labs(title = "Posterior Predictive Check - F2 Mean (vowel /a/)") +
  theme_minimal()

print(pp_post)
ggsave("figures/posterior_predictive_informative_f2.pdf", pp_post, 
       width = 10, height = 6)

cat("\nPosterior predictive check salvato.\n")
cat("VERIFICA: yrep dovrebbe essere molto vicino a y.\n\n")

# =============================================================================
# RISULTATI CHIAVE
# =============================================================================

cat("=== MAIN EFFECTS ===\n")
fixef(m_test)[c("c1_stress", "c2_recovery"), ] %>% round(2) %>% print()

cat("\n=== INTERACTIONS (c1_stress × personality) ===\n")
fixef(m_test)[str_detect(rownames(fixef(m_test)), "c1_stress:pid5"), ] %>%
  round(2) %>% print()

cat("\n=== INTERACTIONS (c2_recovery × personality) ===\n")
fixef(m_test)[str_detect(rownames(fixef(m_test)), ":c2_recovery"), ] %>%
  round(2) %>% print()

# Salva risultati
results_test <- fixef(m_test) %>%
  as_tibble(rownames = "parameter") %>%
  mutate(
    outcome = "f2_mean",
    vowel = "a",
    significant = (`l-95% CI` > 0 & `u-95% CI` > 0) | 
                  (`l-95% CI` < 0 & `u-95% CI` < 0)
  )

write_csv(results_test, "results/test_f2_mean_a_informative_results.csv")

# Identifica effetti significativi
sig_effects <- results_test %>%
  filter(significant == TRUE) %>%
  select(parameter, Estimate, `l-95% CI`, `u-95% CI`)

if (nrow(sig_effects) > 0) {
  cat("\n=== EFFETTI SIGNIFICATIVI ===\n")
  print(sig_effects)
} else {
  cat("\nNessun effetto significativo trovato.\n")
}

cat("\n=== TEST COMPLETATO ===\n")
cat("File salvati:\n")
cat("- models/test_f2_mean_a_informative.rds\n")
cat("- results/test_f2_mean_a_informative_results.csv\n")
cat("- figures/prior_predictive_informative_f2.pdf\n")
cat("- figures/posterior_predictive_informative_f2.pdf\n")

if (max(rhat(m_test), na.rm=TRUE) < 1.01) {
  cat("\n✓ Convergenza OK! Puoi procedere con lo script completo.\n")
} else {
  cat("\n⚠ ATTENZIONE: Problemi di convergenza. Aumenta adapt_delta o iter.\n")
}
