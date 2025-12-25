# =============================================================================
# 05 - ANALISI FORMANT COMPLETA
# =============================================================================

library(tidyverse)
library(brms)

cat("Caricamento dati...\n")

# Carica dati
if (file.exists("results/df_analysis_winsorized.rds")) {
  df_analysis <- readRDS("results/df_analysis_winsorized.rds")
} else {
  df_analysis <- readRDS("results/df_analysis.rds")
}

# Carica prior
prior_params <- readRDS("results/03_prior_parameters.rds")

# Crea contrasti se necessario
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

# Formula base
formula_base <- bf(
  OUTCOME ~
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

# Funzione per run un modello
run_model <- function(outcome_name, vowel) {
  cat("\n=== Modello:", outcome_name, "vowel /", vowel, "/ ===\n", sep = "")

  # Prepara dati
  outcome_col <- paste0(outcome_name, "_", vowel)

  df_model <- df_analysis %>%
    select(
      id,
      ID,
      timepoint,
      c1_stress,
      c2_recovery,
      all_of(outcome_col),
      starts_with("pid5_")
    ) %>%
    drop_na(all_of(outcome_col))

  cat("N obs:", nrow(df_model), "\n")

  # Sostituisci OUTCOME nella formula
  formula_str <- deparse(formula_base$formula)
  formula_str <- gsub("OUTCOME", outcome_col, formula_str)
  formula_final <- bf(
    as.formula(formula_str),
    sigma ~ 1 + c1_stress + c2_recovery
  )

  # Prior (usa valori generici robusti)
  priors <- c(
    prior(normal(1200, 600), class = "Intercept"),
    prior(normal(0, 100), class = "b"),
    prior(normal(5, 0.5), class = "Intercept", dpar = "sigma"),
    prior(normal(0, 0.3), class = "b", dpar = "sigma"),
    prior(exponential(0.01), class = "sd")
  )

  # Fit
  model_name <- paste0("m_", outcome_name, "_", vowel)

  m <- brm(
    formula = formula_final,
    data = df_model,
    family = gaussian(),
    prior = priors,
    chains = 4,
    iter = 4000,
    warmup = 2000,
    cores = 4,
    control = list(adapt_delta = 0.95),
    backend = "cmdstanr",
    seed = 123,
    file = paste0("models/", model_name),
    silent = 2,
    refresh = 0
  )

  # Quick diagnostica
  max_rhat <- max(posterior::rhat(m), na.rm = TRUE)
  min_ess <- min(neff_ratio(m), na.rm = TRUE)

  cat("  Rhat:", round(max_rhat, 3), "\n")
  cat("  ESS:", round(min_ess, 3), "\n")

  if (max_rhat > 1.01) {
    warning("  ⚠ Rhat > 1.01!")
  }

  return(m)
}

# Run tutti i modelli
outcomes <- c("f2_mean", "f2_std")
vowels <- c("a", "i", "u")

models <- list()

for (outcome in outcomes) {
  for (vowel in vowels) {
    key <- paste0(outcome, "_", vowel)
    cat("\n>>> Running", key, "...\n")
    models[[key]] <- run_model(outcome, vowel)

    # Salva periodicamente
    saveRDS(models, "models/05_all_formant_models.rds")
  }
}

cat("\n=== TUTTI I MODELLI COMPLETATI ===\n")
cat("Salvati in models/05_all_formant_models.rds\n")
