# =============================================================================
# SCRIPT PER GENERARE ANALISI FORMANT MANCANTI
# Progetto: Stress-Voice-Personality
# =============================================================================

library(tidyverse)
library(brms)
library(tidybayes)

# =============================================================================
# 1. MODELLI FORMANT - MODERAZIONE PERSONALITY × STRESS
# =============================================================================

# Definisci outcomes formant da analizzare
formant_outcomes <- c("f1_mean", "f1_std", "f2_mean", "f2_std")
vowels <- c("a", "i", "u")
personality_domains <- c("pid5_negative_affectivity_c", 
                         "pid5_detachment_c",
                         "pid5_antagonism_c", 
                         "pid5_disinhibition_c",
                         "pid5_psychoticism_c")

# Formula base per modelli di moderazione
# Assumo stessa struttura dei modelli F0
formula_template <- bf(
  OUTCOME ~ 1 + 
    c1_stress * (pid5_negative_affectivity_c + pid5_detachment_c + 
                 pid5_antagonism_c + pid5_disinhibition_c + pid5_psychoticism_c) +
    c2_recovery:(pid5_negative_affectivity_c + pid5_detachment_c + 
                 pid5_antagonism_c + pid5_disinhibition_c + pid5_psychoticism_c) +
    (1 + c1_stress + c2_recovery | id),
  sigma ~ 1 + c1_stress + c2_recovery
)

# Funzione per run modelli
run_formant_models <- function(data, outcome, vowel) {
  
  # Filtra dati per vocale specifica
  dat <- data %>% 
    filter(vowel_type == vowel) %>%
    drop_na(all_of(outcome))
  
  # Sostituisci OUTCOME nella formula
  form <- formula_template
  form$formula[[2]] <- as.name(outcome)
  
  # Prior (usa same weakly informative priors)
  priors <- c(
    prior(normal(0, 10), class = "b"),
    prior(exponential(1), class = "sd"),
    prior(exponential(1), class = "sigma")
  )
  
  # Fit model
  model_name <- paste0("m_", outcome, "_", vowel)
  
  message(paste("Fitting model:", model_name))
  
  fit <- brm(
    formula = form,
    data = dat,
    family = gaussian(),
    prior = priors,
    chains = 4,
    iter = 4000,
    warmup = 2000,
    cores = 4,
    control = list(adapt_delta = 0.95),
    backend = "cmdstanr",
    seed = 123,
    file = here::here("models", model_name)
  )
  
  return(fit)
}

# Loop attraverso tutti i modelli
all_models <- expand.grid(
  outcome = formant_outcomes,
  vowel = vowels,
  stringsAsFactors = FALSE
)

# Run models (questo prenderà tempo!)
models_list <- pmap(all_models, function(outcome, vowel) {
  run_formant_models(data = your_data, outcome = outcome, vowel = vowel)
})

# =============================================================================
# 2. ESTRAI RISULTATI IN FORMATO TABELLARE
# =============================================================================

extract_model_results <- function(model, outcome, vowel) {
  
  # Estrai posterior draws
  draws <- model %>%
    spread_draws(
      b_Intercept,
      b_c1_stress,
      b_c2_recovery,
      `b_c1_stress:pid5_negative_affectivity_c`,
      `b_c1_stress:pid5_detachment_c`,
      `b_c1_stress:pid5_antagonism_c`,
      `b_c1_stress:pid5_disinhibition_c`,
      `b_c1_stress:pid5_psychoticism_c`,
      `b_pid5_negative_affectivity_c:c2_recovery`,
      `b_pid5_detachment_c:c2_recovery`,
      `b_pid5_antagonism_c:c2_recovery`,
      `b_pid5_disinhibition_c:c2_recovery`,
      `b_pid5_psychoticism_c:c2_recovery`
    )
  
  # Calcola summary statistics
  results <- draws %>%
    pivot_longer(cols = starts_with("b_"), 
                 names_to = "parameter", 
                 values_to = "value") %>%
    group_by(parameter) %>%
    summarise(
      estimate = mean(value),
      se = sd(value),
      ci_lower = quantile(value, 0.025),
      ci_upper = quantile(value, 0.975),
      prob_positive = mean(value > 0),
      prob_negative = mean(value < 0),
      .groups = "drop"
    ) %>%
    mutate(
      significant = (ci_lower > 0 & ci_upper > 0) | (ci_lower < 0 & ci_upper < 0),
      outcome = outcome,
      vowel = vowel,
      type = case_when(
        str_detect(parameter, ":") ~ "Interaction",
        parameter %in% c("b_Intercept", "b_c1_stress", "b_c2_recovery") ~ "Main",
        TRUE ~ "Main"
      )
    )
  
  return(results)
}

# Estrai risultati per tutti i modelli
all_results <- map2_dfr(
  models_list, 
  all_models$outcome,
  all_models$vowel,
  extract_model_results
)

# Salva risultati
write_csv(all_results, "results/05_formant_moderation_results.csv")

# =============================================================================
# 3. TEMPORAL COVARIATION - BETWEEN/WITHIN DECOMPOSITION
# =============================================================================

# Formula per between-within decomposition
formula_bw <- bf(
  f2_std ~ 1 + 
    # Between-person effects
    pid5_negative_affectivity_mean + 
    pid5_detachment_mean +
    pid5_antagonism_mean +
    pid5_disinhibition_mean +
    pid5_psychoticism_mean +
    # Within-person effects  
    pid5_negative_affectivity_dev +
    pid5_detachment_dev +
    pid5_antagonism_dev +
    pid5_disinhibition_dev +
    pid5_psychoticism_dev +
    (1 | id)
)

# Prepara dati per between-within
data_bw <- your_data %>%
  group_by(id) %>%
  mutate(
    across(starts_with("pid5_"), 
           list(mean = mean, dev = ~. - mean(.)),
           .names = "{.col}_{.fn}")
  ) %>%
  ungroup()

# Fit model
m_f2_temporal <- brm(
  formula = formula_bw,
  data = data_bw,
  prior = c(
    prior(normal(0, 10), class = "b"),
    prior(exponential(1), class = "sd"),
    prior(exponential(1), class = "sigma")
  ),
  chains = 4,
  iter = 4000,
  cores = 4,
  backend = "cmdstanr",
  file = here::here("models", "m_f2_temporal_covariation")
)

# Estrai risultati
temporal_results <- m_f2_temporal %>%
  spread_draws(starts_with("b_pid5")) %>%
  pivot_longer(cols = starts_with("b_"), 
               names_to = "parameter", 
               values_to = "value") %>%
  group_by(parameter) %>%
  summarise(
    estimate = mean(value),
    ci_lower = quantile(value, 0.025),
    ci_upper = quantile(value, 0.975),
    significant = (quantile(value, 0.025) > 0 & quantile(value, 0.975) > 0) |
                  (quantile(value, 0.025) < 0 & quantile(value, 0.975) < 0),
    .groups = "drop"
  ) %>%
  separate(parameter, into = c("type", "trait", "level"), sep = "_") %>%
  mutate(
    trait = str_remove(trait, "pid5_"),
    effect_type = case_when(
      str_detect(level, "mean") ~ "Between",
      str_detect(level, "dev") ~ "Within"
    )
  )

write_csv(temporal_results, "results/06_temporal_covariation_formants.csv")

# =============================================================================
# 4. META-ANALISI ACROSS VOWELS (POOLED ESTIMATES)
# =============================================================================

# Funzione per meta-analisi delle interazioni
pool_across_vowels <- function(results_df, effect_name) {
  
  # Filtra per effetto specifico
  effect_data <- results_df %>%
    filter(parameter == effect_name) %>%
    select(vowel, estimate, se)
  
  # Meta-analisi con random effects
  library(metafor)
  
  meta_result <- rma(
    yi = estimate,
    sei = se,
    data = effect_data,
    method = "REML"
  )
  
  # Risultati
  tibble(
    effect = effect_name,
    pooled_estimate = meta_result$b[1],
    pooled_se = meta_result$se,
    pooled_ci_lower = meta_result$ci.lb,
    pooled_ci_upper = meta_result$ci.ub,
    pooled_pval = meta_result$pval,
    i2 = meta_result$I2,
    tau2 = meta_result$tau2
  )
}

# Identifica effetti chiave da pooling
key_effects <- c(
  "b_c1_stress:pid5_negative_affectivity_c",
  "b_pid5_detachment_c:c2_recovery",
  "b_pid5_antagonism_c:c2_recovery"
)

# Pool across vowels
pooled_results <- map_dfr(key_effects, ~pool_across_vowels(all_results, .x))

write_csv(pooled_results, "results/07_pooled_estimates.csv")

# =============================================================================
# 5. EFFECT SIZES (COHEN'S D)
# =============================================================================

# Calcola Cohen's d per main effects
calculate_cohens_d <- function(model, contrast = "c1_stress") {
  
  # Estrai posterior SD del outcome
  sigma_draws <- model %>%
    spread_draws(sigma) %>%
    pull(sigma)
  
  # Estrai effect size
  beta_draws <- model %>%
    spread_draws(b_c1_stress) %>%  # o altro parametro
    pull(b_c1_stress)
  
  # Cohen's d = beta / sigma
  d_draws <- beta_draws / sigma_draws
  
  # Summary
  tibble(
    d_mean = mean(d_draws),
    d_median = median(d_draws),
    d_ci_lower = quantile(d_draws, 0.025),
    d_ci_upper = quantile(d_draws, 0.975)
  )
}

# Calcola per tutti i modelli F0
effect_sizes <- map2_dfr(
  models_list[str_detect(all_models$outcome, "f0")],
  all_models$vowel[str_detect(all_models$outcome, "f0")],
  ~calculate_cohens_d(.x) %>% mutate(vowel = .y)
)

write_csv(effect_sizes, "results/08_effect_sizes.csv")

# =============================================================================
# 6. VISUALIZZAZIONI
# =============================================================================

# Forest plot per interazioni chiave
plot_interactions <- function(results_df) {
  
  interactions <- results_df %>%
    filter(type == "Interaction",
           str_detect(parameter, "c1_stress|c2_recovery"))
  
  ggplot(interactions, aes(x = estimate, y = parameter)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
    facet_wrap(~outcome + vowel, scales = "free_x") +
    labs(
      x = "Effect Size",
      y = "Parameter",
      title = "Personality × Stress Interactions"
    ) +
    theme_minimal()
}

p1 <- plot_interactions(all_results)
ggsave("figures/formant_interactions.pdf", p1, width = 12, height = 10)

# =============================================================================
# NOTE FINALI
# =============================================================================

# Questo script fornisce un template completo per:
# 1. Run tutti i modelli formant mancanti
# 2. Estrarre risultati in formato tidy
# 3. Fare meta-analisi across vowels
# 4. Calcolare effect sizes
# 5. Creare visualizzazioni

# IMPORTANTE: Adatta alle tue specifiche di dati e struttura!
# - Verifica nomi variabili nel tuo dataset
# - Ajusta prior se necessario
# - Controlla convergenza modelli (Rhat, ESS)
# - Fai posterior predictive checks
