# =============================================================================
# SCRIPT CORRETTO PER ANALISI FORMANT
# Adattato alla struttura wide del dataset
# =============================================================================

library(tidyverse)
library(brms)
library(tidybayes)

# =============================================================================
# 1. CARICA E PREPARA DATI
# =============================================================================

df_analysis <- readRDS("results/df_analysis.rds")

# Verifica struttura timepoints
table(df_analysis$timepoint)

# =============================================================================
# 2. CREA CONTRASTI STRESS/RECOVERY
# =============================================================================

# Assumo che timepoint abbia livelli: baseline, pre-exam, post-exam
# c1_stress: baseline vs pre-exam
# c2_recovery: pre-exam vs post-exam

df_analysis <- df_analysis %>%
  mutate(
    # Contrasto 1: Stress effect (pre-exam vs baseline)
    c1_stress = case_when(
      timepoint == "baseline" ~ -0.5,
      timepoint == "pre" ~ 0.5,
      timepoint == "post" ~ 0
    ),
    # Contrasto 2: Recovery effect (post-exam vs pre-exam)
    c2_recovery = case_when(
      timepoint == "baseline" ~ 0,
      timepoint == "pre" ~ -0.5,
      timepoint == "post" ~ 0.5
    ),
    # ID numerico per random effects
    id = as.factor(ID)
  )

# Verifica contrasti
df_analysis %>%
  group_by(timepoint) %>%
  summarise(
    c1_stress = mean(c1_stress, na.rm = TRUE),
    c2_recovery = mean(c2_recovery, na.rm = TRUE)
  )

# =============================================================================
# 3. RESHAPE DATA TO LONG FORMAT (per vocali)
# =============================================================================

# Per ogni outcome acustico, crea formato long con colonna vowel
reshape_for_vowels <- function(data, outcome_base) {
  # Nomi colonne per le tre vocali
  cols <- paste0(outcome_base, "_", c("a", "i", "u"))

  # Controlla che le colonne esistano
  if (!all(cols %in% names(data))) {
    stop(paste(
      "Missing columns:",
      paste(cols[!cols %in% names(data)], collapse = ", ")
    ))
  }

  # Reshape to long
  data_long <- data %>%
    select(
      id,
      ID,
      timepoint,
      c1_stress,
      c2_recovery,
      starts_with("pid5_"),
      all_of(cols)
    ) %>%
    pivot_longer(
      cols = all_of(cols),
      names_to = "vowel",
      values_to = "outcome_value"
    ) %>%
    mutate(
      vowel = str_extract(vowel, "[aiu]$"), # Estrai ultima lettera
      vowel = factor(vowel, levels = c("a", "i", "u"))
    ) %>%
    rename(!!outcome_base := outcome_value)

  return(data_long)
}

# Test la funzione
df_f2_mean <- reshape_for_vowels(df_analysis, "f2_mean")
glimpse(df_f2_mean)

# =============================================================================
# 4. RUN MODELLI PER OGNI OUTCOME E VOCALE
# =============================================================================

# Lista outcomes da analizzare
formant_outcomes <- c("f2_mean", "f2_std") # Inizia con questi, poi aggiungi f1

run_vowel_models <- function(data_wide, outcome_base, vowel_filter = NULL) {
  # Reshape data
  message(paste("Reshaping data for:", outcome_base))
  data_long <- reshape_for_vowels(data_wide, outcome_base)

  # Filtra per vocale se specificato
  if (!is.null(vowel_filter)) {
    data_long <- data_long %>% filter(vowel == vowel_filter)
  }

  # Remove missing
  data_long <- data_long %>% drop_na(all_of(outcome_base))

  message(paste("N observations:", nrow(data_long)))

  # Formula
  formula_str <- paste0(
    outcome_base,
    " ~ 1 + ",
    "c1_stress * (pid5_negative_affectivity_c + pid5_detachment_c + ",
    "pid5_antagonism_c + pid5_disinhibition_c + pid5_psychoticism_c) + ",
    "c2_recovery:(pid5_negative_affectivity_c + pid5_detachment_c + ",
    "pid5_antagonism_c + pid5_disinhibition_c + pid5_psychoticism_c) + ",
    "(1 + c1_stress + c2_recovery | id) + ",
    "(1 | vowel)" # Random intercept per vocale se non filtrato
  )

  # Se filtrato per vocale, rimuovi random effect vocale
  if (!is.null(vowel_filter)) {
    formula_str <- str_replace(formula_str, "\\+ \\(1 \\| vowel\\)", "")
  }

  form <- bf(
    as.formula(formula_str),
    sigma ~ 1 + c1_stress + c2_recovery
  )

  # Prior
  priors <- c(
    prior(normal(0, 100), class = "b"), # Weakly informative
    prior(exponential(1), class = "sd"),
    prior(normal(0, 1), class = "b", dpar = "sigma"),
    prior(normal(0, 1), class = "Intercept", dpar = "sigma")
  )

  # Model name
  vowel_suffix <- ifelse(is.null(vowel_filter), "all", vowel_filter)
  model_name <- paste0("m_", outcome_base, "_", vowel_suffix)

  message(paste("Fitting model:", model_name))

  # Fit
  fit <- brm(
    formula = form,
    data = data_long,
    family = gaussian(),
    prior = priors,
    chains = 4,
    iter = 4000,
    warmup = 2000,
    cores = 4,
    control = list(adapt_delta = 0.95, max_treedepth = 12),
    backend = "cmdstanr",
    seed = 123,
    file = here::here("models", model_name),
    file_refit = "on_change"
  )

  return(fit)
}

# =============================================================================
# 5. RUN MODELLI - APPROCCIO 1: MODELLO PER OGNI VOCALE SEPARATAMENTE
# =============================================================================

# Questo replica la struttura che hai già usato per F0
vowels <- c("a", "i", "u")
outcomes <- c("f2_mean", "f2_std")

models_separate <- list()

for (outcome in outcomes) {
  for (vowel in vowels) {
    model_key <- paste0(outcome, "_", vowel)
    message(paste("\n=== Running model:", model_key, "==="))

    models_separate[[model_key]] <- run_vowel_models(
      data_wide = df_analysis,
      outcome_base = outcome,
      vowel_filter = vowel
    )

    # Salva periodicamente
    saveRDS(models_separate, "models/models_formant_separate.rds")
  }
}

# =============================================================================
# 6. ESTRAI RISULTATI
# =============================================================================

extract_results <- function(model, outcome, vowel) {
  # Get posterior draws
  draws <- as_draws_df(model)

  # Identifica colonne di interesse (parametri beta)
  beta_cols <- names(draws)[str_detect(names(draws), "^b_")]

  # Escludi parametri sigma
  beta_cols <- beta_cols[!str_detect(beta_cols, "^b_sigma")]

  # Summary per ogni parametro
  results <- map_dfr(beta_cols, function(param) {
    values <- draws[[param]]

    tibble(
      parameter = param,
      estimate = mean(values),
      se = sd(values),
      ci_lower = quantile(values, 0.025),
      ci_upper = quantile(values, 0.975),
      significant = (quantile(values, 0.025) > 0 &
        quantile(values, 0.975) > 0) |
        (quantile(values, 0.025) < 0 & quantile(values, 0.975) < 0)
    )
  }) %>%
    mutate(
      model = paste0("m_", outcome, "_", vowel),
      outcome = outcome,
      vowel = vowel,
      type = case_when(
        parameter %in% c("b_Intercept", "b_c1_stress", "b_c2_recovery") ~
          "Main",
        str_detect(parameter, ":") ~ "Interaction",
        TRUE ~ "Main"
      )
    )

  return(results)
}

# Estrai per tutti i modelli
all_results <- map2_dfr(
  models_separate,
  names(models_separate),
  function(model, key) {
    parts <- str_split(key, "_", n = 3)[[1]]
    outcome <- paste(parts[1:2], collapse = "_")
    vowel <- parts[3]
    extract_results(model, outcome, vowel)
  }
)

# Salva
write_csv(all_results, "results/05_formant_moderation_results.csv")

# =============================================================================
# 7. VISUALIZZA RISULTATI CHIAVE
# =============================================================================

# Filtra interazioni significative
significant_interactions <- all_results %>%
  filter(type == "Interaction", significant == TRUE) %>%
  arrange(outcome, vowel, parameter)

print(significant_interactions)

# Plot interazioni chiave
plot_key_interactions <- all_results %>%
  filter(
    type == "Interaction",
    str_detect(
      parameter,
      "c1_stress.*negative_affectivity|c1_stress.*psychoticism|c2_recovery.*detachment"
    )
  ) %>%
  ggplot(aes(x = estimate, y = parameter, color = vowel)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbarh(
    aes(xmin = ci_lower, xmax = ci_upper),
    height = 0,
    position = position_dodge(width = 0.5)
  ) +
  facet_wrap(~outcome, scales = "free_x") +
  labs(
    x = "Effect Size (with 95% CI)",
    y = "Interaction Parameter",
    color = "Vowel",
    title = "Key Personality × Stress Interactions for Formants"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

ggsave(
  "figures/formant_interactions_key.pdf",
  plot_key_interactions,
  width = 12,
  height = 8
)

# =============================================================================
# 8. TABELLA RIASSUNTIVA PER MANOSCRITTO
# =============================================================================

# Crea tabella con solo effetti significativi
table_sig <- all_results %>%
  filter(significant == TRUE) %>%
  select(outcome, vowel, parameter, estimate, ci_lower, ci_upper) %>%
  mutate(
    ci = paste0("[", round(ci_lower, 2), ", ", round(ci_upper, 2), "]"),
    estimate = round(estimate, 2)
  ) %>%
  select(outcome, vowel, parameter, estimate, ci) %>%
  arrange(outcome, vowel, parameter)

write_csv(table_sig, "results/05_formant_significant_effects.csv")

print("=== SIGNIFICANT EFFECTS ===")
print(table_sig)

# =============================================================================
# NOTA FINALE
# =============================================================================

cat("\n\n=== ANALISI COMPLETATA ===\n")
cat("File salvati:\n")
cat("- models/models_formant_separate.rds\n")
cat("- results/05_formant_moderation_results.csv\n")
cat("- results/05_formant_significant_effects.csv\n")
cat("- figures/formant_interactions_key.pdf\n")
