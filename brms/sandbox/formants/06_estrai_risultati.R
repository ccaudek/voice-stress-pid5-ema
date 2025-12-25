# =============================================================================
# 06 - ESTRAZIONE RISULTATI
# =============================================================================

library(tidyverse)
library(brms)

cat("Caricamento modelli...\n")
models <- readRDS("models/05_all_formant_models.rds")

cat("N modelli:", length(models), "\n\n")

# Funzione estrazione
extract_results <- function(model, model_name) {
  
  # Parse nome
  parts <- str_split(model_name, "_")[[1]]
  outcome <- paste(parts[1:2], collapse = "_")
  vowel <- parts[3]
  
  # Estrai fixed effects
  fe <- fixef(model) %>%
    as_tibble(rownames = "parameter") %>%
    mutate(
      model = model_name,
      outcome = outcome,
      vowel = vowel,
      significant = (`l-95% CI` > 0 & `u-95% CI` > 0) | 
                    (`l-95% CI` < 0 & `u-95% CI` < 0),
      type = case_when(
        parameter %in% c("Intercept", "c1_stress", "c2_recovery") ~ "Main",
        str_detect(parameter, ":") ~ "Interaction",
        TRUE ~ "Covariate"
      )
    )
  
  return(fe)
}

# Estrai tutti
all_results <- map2_dfr(models, names(models), extract_results)

# Salva completo
write_csv(all_results, "results/05_formant_moderation_results.csv")

cat("Risultati completi salvati in:\n")
cat("  results/05_formant_moderation_results.csv\n\n")

# Filtra solo significativi
sig_results <- all_results %>%
  filter(significant == TRUE) %>%
  select(outcome, vowel, parameter, Estimate, `l-95% CI`, `u-95% CI`) %>%
  arrange(outcome, vowel, parameter)

write_csv(sig_results, "results/05_formant_significant_effects.csv")

cat("Effetti significativi salvati in:\n")
cat("  results/05_formant_significant_effects.csv\n\n")

# Summary tabella
cat("=== SUMMARY EFFETTI SIGNIFICATIVI ===\n\n")

summary_table <- sig_results %>%
  group_by(outcome, type = ifelse(str_detect(parameter, ":"), "Interaction", "Main")) %>%
  summarise(n_sig = n(), .groups = "drop") %>%
  pivot_wider(names_from = type, values_from = n_sig, values_fill = 0)

print(summary_table)

# Interazioni chiave per abstract
cat("\n=== INTERAZIONI CHIAVE ===\n\n")

key_interactions <- sig_results %>%
  filter(str_detect(parameter, "c1_stress.*psychoticism|c1_stress.*negative_affectivity|c2_recovery.*detachment"))

if (nrow(key_interactions) > 0) {
  print(key_interactions)
} else {
  cat("Nessuna interazione chiave significativa trovata.\n")
}

# Crea summary convergenza
cat("\n=== DIAGNOSTICA CONVERGENZA ===\n\n")

convergence_summary <- map_dfr(names(models), function(name) {
  m <- models[[name]]
  tibble(
    model = name,
    max_rhat = max(rhat(m), na.rm = TRUE),
    min_ess_bulk = min(neff_ratio(m), na.rm = TRUE),
    converged = max_rhat < 1.01 && min_ess_bulk > 0.1
  )
})

write_csv(convergence_summary, "results/06_convergence_summary.csv")

print(convergence_summary)

cat("\n")
if (all(convergence_summary$converged)) {
  cat("✓ TUTTI I MODELLI CONVERGITI!\n")
} else {
  cat("✗ ATTENZIONE: Alcuni modelli non convergiti:\n")
  print(convergence_summary %>% filter(!converged))
}
