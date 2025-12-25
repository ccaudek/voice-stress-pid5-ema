# =============================================================================
# 04 - TEST MODELLO (F2 mean vowel /a/)
# =============================================================================

library(tidyverse)
library(brms)
library(bayesplot)

cat("Caricamento dati e prior...\n")

# Carica dati
# if (file.exists("results/df_analysis_winsorized.rds")) {
#   df_analysis <- readRDS("results/df_analysis_winsorized.rds")
# } else {
#   df_analysis <- readRDS("results/df_analysis.rds")
# }

df_analysis <- readRDS("results/df_analysis_winsorized.rds")

# Carica parametri prior
prior_params <- readRDS("results/03_prior_parameters.rds")

# Crea contrasti
if (!"c1_stress" %in% names(df_analysis)) {
  df_analysis <- df_analysis %>%
    mutate(
      c1_stress = case_when(
        timepoint == "baseline" ~ -0.5,
        timepoint == "pre-exam" ~ 0.5,
        timepoint == "post-exam" ~ 0
      ),
      c2_recovery = case_when(
        timepoint == "baseline" ~ 0,
        timepoint == "pre-exam" ~ -0.5,
        timepoint == "post-exam" ~ 0.5
      ),
      id = as.factor(ID)
    )
}

# Prepara dati per test
df_test <- df_analysis %>%
  select(
    id,
    ID,
    timepoint,
    c1_stress,
    c2_recovery,
    f2_mean_a,
    starts_with("pid5_")
  ) %>%
  drop_na(f2_mean_a)

cat("N osservazioni:", nrow(df_test), "\n")
cat("N partecipanti:", n_distinct(df_test$ID), "\n\n")

# Formula
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

# Prior dalla tabella
params_a <- prior_params %>% filter(vowel == "a", outcome == "f2_mean")

priors <- c(
  prior(normal(1205, 585), class = "Intercept"),
  prior(normal(0, 100), class = "b"),
  prior(normal(5.5, 0.5), class = "Intercept", dpar = "sigma"),
  prior(normal(0, 0.3), class = "b", dpar = "sigma"),
  prior(exponential(0.01), class = "sd")
)

cat("Prior specificati.\n\n")

# Prior predictive check
cat("Prior predictive check...\n")
set.seed(123)

m_prior <- brm(
  formula = formula_test,
  data = df_test,
  family = gaussian(),
  prior = priors,
  sample_prior = "only",
  chains = 1,
  iter = 2000,
  warmup = 1000,
  cores = 1,
  backend = "cmdstanr",
  seed = 123,
  silent = 2,
  refresh = 0
)

pp_prior <- pp_check(m_prior, type = "dens_overlay", ndraws = 50) +
  labs(
    title = "Prior Predictive Check - F2 Mean /a/",
    subtitle = "yrep (colore) deve sovrapporsi a y (nero)"
  ) +
  theme_minimal()

ggsave("figures/04_prior_check.pdf", pp_prior, width = 10, height = 6)

cat("Prior check salvato in figures/04_prior_check.pdf\n")
cat("VERIFICA VISIVA RICHIESTA prima di procedere!\n\n")

# Fitting
cat("Fitting modello test (circa 10 minuti)...\n")

m_test <- brm(
  formula = formula_test,
  data = df_test,
  family = gaussian(),
  prior = priors,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  control = list(adapt_delta = 0.95),
  backend = "cmdstanr",
  seed = 123,
  file = "models/04_test_f2_mean_a",
  silent = 2,
  refresh = 0
)

# Diagnostica
cat("\nDiagnostica convergenza:\n")
max_rhat <- max(posterior::rhat(m_test), na.rm = TRUE)
min_ess <- min(neff_ratio(m_test), na.rm = TRUE)

cat("  Max Rhat:", round(max_rhat, 4), "\n")
cat("  Min ESS ratio:", round(min_ess, 3), "\n\n")

# Posterior check
pp_post <- pp_check(m_test, type = "dens_overlay", ndraws = 50) +
  labs(title = "Posterior Predictive Check") +
  theme_minimal()

ggsave("figures/04_posterior_check.pdf", pp_post, width = 10, height = 6)

# Convergenza OK?
test_converged <- (max_rhat < 1.01) && (min_ess > 0.1)

if (test_converged) {
  cat("✓ CONVERGENZA OK!\n")
  cat("  Max Rhat < 1.01\n")
  cat("  Min ESS > 0.1\n\n")

  # Salva risultati test
  results_test <- fixef(m_test) %>%
    as_tibble(rownames = "parameter") %>%
    mutate(
      significant = (`l-95% CI` > 0 & `u-95% CI` > 0) |
        (`l-95% CI` < 0 & `u-95% CI` < 0)
    )

  write_csv(results_test, "results/04_test_results.csv")

  cat("Risultati test salvati in results/04_test_results.csv\n")
} else {
  cat("✗ PROBLEMI CONVERGENZA!\n")
  if (max_rhat >= 1.01) cat("  Rhat troppo alto (", max_rhat, ")\n")
  if (min_ess <= 0.1) cat("  ESS troppo basso (", min_ess, ")\n")
  cat("\nNON procedere con analisi completa.\n")
  test_converged <- FALSE
}
