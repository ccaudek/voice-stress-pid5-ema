# =============================================================================
# SCRIPT TEST: UN SINGOLO MODELLO FORMANT
# =============================================================================

library(tidyverse)
library(brms)
library(tidybayes)

# =============================================================================
# 1. CARICA E PREPARA DATI
# =============================================================================

df_analysis <- readRDS("results/df_analysis.rds")

# Crea contrasti se non esistono
if (!"c1_stress" %in% names(df_analysis)) {
  cat("Creando contrasti c1_stress e c2_recovery...\n")

  # Verifica livelli timepoint
  cat("Livelli timepoint:", unique(df_analysis$timepoint), "\n")

  df_analysis <- df_analysis %>%
    mutate(
      c1_stress = case_when(
        timepoint == "baseline" ~ -0.5,
        timepoint == "pre" ~ 0.5,
        timepoint == "post" ~ 0,
        TRUE ~ NA_real_
      ),
      c2_recovery = case_when(
        timepoint == "baseline" ~ 0,
        timepoint == "pre" ~ -0.5,
        timepoint == "post" ~ 0.5,
        TRUE ~ NA_real_
      ),
      id = as.factor(ID)
    )

  # Verifica
  df_analysis %>%
    group_by(timepoint) %>%
    summarise(
      c1_stress = mean(c1_stress, na.rm = TRUE),
      c2_recovery = mean(c2_recovery, na.rm = TRUE),
      n = n()
    ) %>%
    print()
}

# =============================================================================
# 2. PREPARA DATI PER UN SINGOLO MODELLO: f2_mean per vowel /a/
# =============================================================================

cat("\n=== PREPARAZIONE DATI PER TEST ===\n")

# Seleziona solo le colonne necessarie
df_test <- df_analysis %>%
  select(
    id,
    ID,
    timepoint,
    c1_stress,
    c2_recovery,
    f2_mean_a,
    f2_std_a,
    starts_with("pid5_")
  ) %>%
  # Rimuovi missing su outcome
  drop_na(f2_mean_a)

cat("N osservazioni:", nrow(df_test), "\n")
cat("N partecipanti:", n_distinct(df_test$ID), "\n")

# Statistiche descrittive F2
cat("\nStatistiche F2 mean (vowel /a/):\n")
df_test %>%
  group_by(timepoint) %>%
  summarise(
    M = mean(f2_mean_a, na.rm = TRUE),
    SD = sd(f2_mean_a, na.rm = TRUE),
    n = n()
  ) %>%
  print()

# =============================================================================
# 3. DEFINISCI E TESTA FORMULA
# =============================================================================

cat("\n=== FORMULA MODELLO ===\n")

formula_test <- bf(
  f2_mean_a ~
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

print(formula_test)

# =============================================================================
# STEP 1: ESTRAI STATISTICHE DESCRITTIVE PER PRIOR INFORMATIVI
# =============================================================================

cat("=== STATISTICHE DESCRITTIVE F2 PER VOCALE ===\n\n")

# Statistiche per ogni vocale
stats_f2 <- df_analysis %>%
  summarise(
    # F2 mean per /a/
    f2_mean_a_M = mean(f2_mean_a, na.rm = TRUE),
    f2_mean_a_SD = sd(f2_mean_a, na.rm = TRUE),
    f2_std_a_M = mean(f2_std_a, na.rm = TRUE),
    f2_std_a_SD = sd(f2_std_a, na.rm = TRUE),
    # F2 mean per /i/
    f2_mean_i_M = mean(f2_mean_i, na.rm = TRUE),
    f2_mean_i_SD = sd(f2_mean_i, na.rm = TRUE),
    f2_std_i_M = mean(f2_std_i, na.rm = TRUE),
    f2_std_i_SD = sd(f2_std_i, na.rm = TRUE),
    # F2 mean per /u/
    f2_mean_u_M = mean(f2_mean_u, na.rm = TRUE),
    f2_mean_u_SD = sd(f2_mean_u, na.rm = TRUE),
    f2_std_u_M = mean(f2_std_u, na.rm = TRUE),
    f2_std_u_SD = sd(f2_std_u, na.rm = TRUE)
  )

data.frame(stats_f2)

# Visualizza distribuzioni
p1 <- df_analysis %>%
  select(starts_with("f2_mean")) %>%
  pivot_longer(everything(), names_to = "vowel", values_to = "f2") %>%
  mutate(vowel = str_extract(vowel, "[aiu]$")) %>%
  ggplot(aes(x = f2, fill = vowel)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribuzione F2 mean per vocale",
    x = "F2 (Hz)",
    y = "Density"
  ) +
  theme_minimal()

print(p1)
ggsave("figures/f2_distributions_by_vowel.pdf", width = 10, height = 6)

# Range effetti plausibili
cat("\n=== RANGE EFFETTI PLAUSIBILI ===\n")
cat("Per stress/personality effects su F2:\n")
cat("- Small effect: ~20-50 Hz (< 5% della media)\n")
cat("- Medium effect: ~50-100 Hz (5-10% della media)\n")
cat("- Large effect: ~100-200 Hz (10-15% della media)\n\n")

# Calcola prior informativi
cat("=== PRIOR INFORMATIVI SUGGERITI ===\n\n")

# Per f2_mean_a
f2_mean_a_mean <- stats_f2$f2_mean_a_M
f2_mean_a_sd <- stats_f2$f2_mean_a_SD

cat("Per f2_mean vowel /a/:\n")
cat(
  "Intercept: normal(",
  round(f2_mean_a_mean, 0),
  ", ",
  round(f2_mean_a_sd * 2, 0),
  ")\n",
  sep = ""
)
cat(
  "  Rationale: Centrato sulla media osservata, SD permette +/-2SD dalla media\n\n"
)

cat("Coefficienti (b): normal(0, 100)\n")
cat(
  "  Rationale: Effetti da -200 a +200 Hz (range plausibile per moderazione)\n\n"
)

cat(
  "Intercept sigma: normal(",
  round(log(f2_mean_a_sd), 1),
  ", 0.5)\n",
  sep = ""
)
cat(
  "  Rationale: log(",
  round(f2_mean_a_sd, 0),
  ") = ",
  round(log(f2_mean_a_sd), 1),
  " sulla scala log\n",
  sep = ""
)
cat(
  "  Questo permette SD da ~",
  round(exp(log(f2_mean_a_sd) - 1), 0),
  " a ~",
  round(exp(log(f2_mean_a_sd) + 1), 0),
  " Hz\n\n",
  sep = ""
)

cat("Coefficienti sigma (b, dpar='sigma'): normal(0, 0.3)\n")
cat("  Rationale: Piccoli cambiamenti in log-SD (±30% variazione in SD)\n\n")

cat("Random effects (sd): exponential(0.01)\n")
cat("  Rationale: Media a 100, permette ampia variabilità tra soggetti\n\n")

# Salva stats
write_csv(stats_f2, "results/f2_descriptive_stats.csv")

cat("\n=== FILE SALVATI ===\n")
cat("- results/f2_descriptive_stats.csv\n")
cat("- figures/f2_distributions_by_vowel.pdf\n")

# =============================================================================
# 4. PRIOR PREDICTIVE CHECK
# =============================================================================

cat("\n=== PRIOR PREDICTIVE CHECK ===\n")

# Prior
priors <- c(
  prior(normal(0, 100), class = "b"), # Weakly informative
  prior(exponential(1), class = "sd"),
  prior(normal(0, 1), class = "b", dpar = "sigma"),
  prior(normal(0, 1), class = "Intercept", dpar = "sigma")
)

# Sample from prior (veloce, solo 100 iter)
m_prior <- brm(
  formula = formula_test,
  data = df_test,
  family = gaussian(),
  prior = priors,
  sample_prior = "only",
  chains = 1,
  iter = 200,
  warmup = 100,
  cores = 1,
  backend = "cmdstanr",
  seed = 123
)

# Plot prior predictive
pp_check(m_prior, type = "dens_overlay", ndraws = 50) +
  labs(title = "Prior Predictive Distribution for F2 Mean (vowel /a/)")

ggsave("figures/prior_predictive_f2_mean_a.pdf", width = 8, height = 6)

cat("\nPrior predictive plot salvato in figures/\n")

# =============================================================================
# 5. FIT MODELLO COMPLETO (VERSIONE BREVE PER TEST)
# =============================================================================

cat("\n=== FITTING MODELLO (versione breve per test) ===\n")
cat("Questo richiederà circa 5-10 minuti...\n")

m_test <- brm(
  formula = formula_test,
  data = df_test,
  family = gaussian(),
  prior = priors,
  chains = 2, # Solo 2 chains per test veloce
  iter = 2000, # Meno iter per test
  warmup = 1000,
  cores = 2,
  control = list(adapt_delta = 0.95),
  backend = "cmdstanr",
  seed = 123,
  file = "models/test_f2_mean_a"
)

# =============================================================================
# 6. DIAGNOSTICA
# =============================================================================

cat("\n=== DIAGNOSTICA MODELLO ===\n")

# Summary
print(summary(m_test))

# Convergence
cat("\nMax Rhat:", max(rhat(m_test), na.rm = TRUE), "\n")
cat("Min ESS:", min(neff_ratio(m_test), na.rm = TRUE), "\n")

# Trace plots per parametri chiave
plot(m_test, variable = c("b_c1_stress", "b_c2_recovery"), regex = FALSE)
ggsave("figures/trace_plot_test.pdf", width = 10, height = 6)

# Posterior predictive check
pp <- pp_check(m_test, type = "dens_overlay", ndraws = 50)
print(pp)
ggsave("figures/posterior_predictive_test.pdf", pp, width = 8, height = 6)

# =============================================================================
# 7. ESTRAI RISULTATI
# =============================================================================

cat("\n=== RISULTATI CHIAVE ===\n")

# Main effects
cat("\nMain effects:\n")
fixef(m_test)[c("c1_stress", "c2_recovery"), ] %>%
  round(2) %>%
  print()

# Interazioni con stress
cat("\nInterazioni c1_stress × personality:\n")
fixef(m_test)[str_detect(rownames(fixef(m_test)), "c1_stress:pid5"), ] %>%
  round(2) %>%
  print()

# Interazioni con recovery
cat("\nInterazioni c2_recovery × personality:\n")
fixef(m_test)[str_detect(rownames(fixef(m_test)), ":c2_recovery"), ] %>%
  round(2) %>%
  print()

# =============================================================================
# 8. PLOT INTERAZIONE CHIAVE
# =============================================================================

# Plot condizionale per Psychoticism × Stress
if (
  fixef(m_test)["c1_stress:pid5_psychoticism_c", "l-95% CI"] *
    fixef(m_test)["c1_stress:pid5_psychoticism_c", "u-95% CI"] >
    0
) {
  cat("\nCreando plot per interazione significativa Psychoticism × Stress...\n")

  conditional_effects(m_test, "c1_stress:pid5_psychoticism_c") %>%
    plot(points = TRUE) %>%
    print()

  ggsave(
    "figures/interaction_psychoticism_stress_f2.pdf",
    width = 8,
    height = 6
  )
}

# =============================================================================
# 9. SALVA RISULTATI
# =============================================================================

# Estrai tutti i parametri
results_test <- fixef(m_test) %>%
  as_tibble(rownames = "parameter") %>%
  mutate(
    outcome = "f2_mean",
    vowel = "a",
    significant = (`l-95% CI` > 0 & `u-95% CI` > 0) |
      (`l-95% CI` < 0 & `u-95% CI` < 0)
  )

write_csv(results_test, "results/test_f2_mean_a_results.csv")

cat("\n=== TEST COMPLETATO ===\n")
cat("File salvati:\n")
cat("- models/test_f2_mean_a.rds (modello)\n")
cat("- results/test_f2_mean_a_results.csv (risultati)\n")
cat("- figures/prior_predictive_f2_mean_a.pdf\n")
cat("- figures/posterior_predictive_test.pdf\n")
cat("- figures/trace_plot_test.pdf\n")

cat("\nSe tutto funziona correttamente, puoi lanciare lo script completo!\n")
