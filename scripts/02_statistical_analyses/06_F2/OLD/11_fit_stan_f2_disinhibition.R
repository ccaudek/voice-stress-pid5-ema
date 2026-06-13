# ==============================================================================
# 11_fit_stan_f2_disinhibition.R
# Fitting modello Stan per moderazione Disinhibition su F2/articolazione
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(cmdstanr)
  library(posterior)
  library(bayesplot)
  library(loo)
  library(here)
})

# ==============================================================================
# 0) CONFIGURAZIONE
# ==============================================================================

# Percorsi
# SCEGLI MODELLO: "full" (con random slopes) o "simple" (solo intercepts)
model_type <- "simple" # Cambia in "full" se simple converge bene

if (model_type == "simple") {
  model_path <- "stan/F2/f2_disinhibition_moderation_simple.stan"
  cat("USANDO MODELLO SEMPLIFICATO (solo random intercepts)\n")
} else {
  model_path <- "stan/F2/f2_disinhibition_moderation.stan"
  cat("USANDO MODELLO COMPLETO (con random slopes)\n")
}

data_path <- "results/f2_disinhibition/stan_bundle_f2_disinhibition.rds"
output_dir <- "results/f2_disinhibition_stan"

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Parametri MCMC
n_chains <- 4
n_iter_warmup <- 1500 # Più warmup per modello complesso
n_iter_sampling <- 1500
n_thin <- 1
n_cores <- parallel::detectCores() - 1

# ==============================================================================
# 1) CARICA DATI
# ==============================================================================

cat("=== CARICAMENTO DATI ===\n")

bundle <- readRDS(data_path)
stan_data <- bundle$stan_data
outcome_metric <- bundle$outcome_metric

cat(sprintf(
  "Dati: N = %d, N_subj = %d, Outcome = %s\n",
  stan_data$N_voice,
  stan_data$N_subj,
  outcome_metric
))

# Verifica
stopifnot(!anyNA(stan_data$X))
stopifnot(!anyNA(stan_data$y))

# ==============================================================================
# 2) COMPILA MODELLO
# ==============================================================================

cat("\n=== COMPILAZIONE MODELLO ===\n")

if (!file.exists(model_path)) {
  stop("File modello Stan non trovato: ", model_path)
}

mod <- cmdstan_model(model_path)
cat("Modello compilato con successo.\n")

# ==============================================================================
# 3) FITTING
# ==============================================================================

cat("\n=== FITTING MODELLO ===\n")
cat(sprintf(
  "Chains: %d | Warmup: %d | Sampling: %d\n",
  n_chains,
  n_iter_warmup,
  n_iter_sampling
))

fit <- mod$sample(
  data = stan_data,
  chains = n_chains,
  parallel_chains = min(n_chains, n_cores),
  iter_warmup = n_iter_warmup,
  iter_sampling = n_iter_sampling,
  thin = n_thin,
  refresh = 100,
  adapt_delta = 0.95,
  max_treedepth = 12,
  seed = 54321,
  output_dir = output_dir,
  output_basename = "f2_disinhibition_fit"
)

# ==============================================================================
# 4) DIAGNOSTICS
# ==============================================================================

cat("\n=== DIAGNOSTICS ===\n")

# Key parameters
# key_params <- c(
#   "alpha", "beta_c1", "beta_c2",
#   "beta_disinhibition", "beta_c1_x_disinhibition",
#   "sigma_alpha", "sigma_beta_c1", "sigma_y",
#   "r2_marginal",
#   "effect_stress_low_disinhibition",
#   "effect_stress_high_disinhibition",
#   "moderation_effect"
# )
key_params <- c(
  "alpha",
  "beta_c1",
  "beta_c2",
  "beta_disinhibition",
  "beta_c1_x_disinhibition",
  "sigma_alpha",
  "sigma_y",
  "effect_stress_low_disinhibition",
  "effect_stress_high_disinhibition",
  "moderation_effect"
)

diag_summary <- fit$summary(variables = key_params)

cat("\nParametri chiave:\n")
print(diag_summary)

# Convergenza
max_rhat <- max(diag_summary$rhat, na.rm = TRUE)
min_ess <- min(c(diag_summary$ess_bulk, diag_summary$ess_tail), na.rm = TRUE)

cat(sprintf(
  "\nConvergenza: max(Rhat) = %.3f, min(ESS) = %.0f\n",
  max_rhat,
  min_ess
))

if (max_rhat > 1.01) {
  warning("Possibili problemi di convergenza: Rhat > 1.01")
}

# Salva
write_csv(
  diag_summary,
  file.path(output_dir, "parameter_summary.csv")
)

# ==============================================================================
# 5) ESTRAZIONE POSTERIOR
# ==============================================================================

cat("\n=== ESTRAZIONE POSTERIOR ===\n")

draws <- fit$draws(format = "df")

# Focus su parametri chiave
key_posterior <- fit$draws(key_params, format = "matrix")

# Summary interazione
interaction_summary <- tibble(
  parameter = "beta_c1_x_disinhibition",
  mean = mean(key_posterior[, "beta_c1_x_disinhibition"]),
  median = median(key_posterior[, "beta_c1_x_disinhibition"]),
  sd = sd(key_posterior[, "beta_c1_x_disinhibition"]),
  q025 = quantile(key_posterior[, "beta_c1_x_disinhibition"], 0.025),
  q975 = quantile(key_posterior[, "beta_c1_x_disinhibition"], 0.975),
  prob_neg = mean(key_posterior[, "beta_c1_x_disinhibition"] < 0)
)

cat("\n=== PARAMETRO CHIAVE: INTERAZIONE DISINHIBITION × STRESS ===\n")
print(interaction_summary)

write_csv(
  interaction_summary,
  file.path(output_dir, "interaction_summary.csv")
)

# Effetti condizionali
conditional_effects <- tibble(
  parameter = c(
    "effect_stress_low_disinhibition",
    "effect_stress_high_disinhibition",
    "moderation_effect"
  )
) %>%
  mutate(
    mean = map_dbl(parameter, ~ mean(key_posterior[, .x])),
    median = map_dbl(parameter, ~ median(key_posterior[, .x])),
    sd = map_dbl(parameter, ~ sd(key_posterior[, .x])),
    q025 = map_dbl(parameter, ~ quantile(key_posterior[, .x], 0.025)),
    q975 = map_dbl(parameter, ~ quantile(key_posterior[, .x], 0.975))
  )

cat("\nEffetti condizionali stress per livello Disinhibition:\n")
print(conditional_effects)

write_csv(
  conditional_effects,
  file.path(output_dir, "conditional_effects.csv")
)

# ==============================================================================
# 6) VISUALIZZAZIONI
# ==============================================================================

cat("\n=== CREAZIONE GRAFICI ===\n")

# Trace plots
pdf(file.path(output_dir, "trace_plots.pdf"), width = 12, height = 8)
mcmc_trace(
  draws,
  pars = c(
    "alpha",
    "beta_c1",
    "beta_disinhibition",
    "beta_c1_x_disinhibition",
    "sigma_y"
  )
)
dev.off()

# Posterior distributions
pdf(file.path(output_dir, "posterior_densities.pdf"), width = 12, height = 10)
mcmc_dens(
  draws,
  pars = c(
    "beta_c1",
    "beta_c2",
    "beta_disinhibition",
    "beta_c1_x_disinhibition"
  )
) +
  labs(title = "Posterior Distributions: Main Effects & Interaction")
dev.off()

# Intervals plot
pdf(file.path(output_dir, "posterior_intervals.pdf"), width = 10, height = 8)
mcmc_intervals(
  draws,
  pars = c(
    "beta_c1",
    "beta_c2",
    "beta_disinhibition",
    "beta_c1_x_disinhibition",
    "effect_stress_low_disinhibition",
    "effect_stress_high_disinhibition"
  ),
  prob = 0.5,
  prob_outer = 0.95
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Posterior Intervals: Effects of Interest")
dev.off()

# Interaction plot (conditional effects)
pdf(file.path(output_dir, "interaction_plot.pdf"), width = 10, height = 6)

# Crea dataframe per plot
interaction_df <- tibble(
  disinhibition = c(-1, 1),
  disinhibition_label = c(
    "Low Disinhibition (-1 SD)",
    "High Disinhibition (+1 SD)"
  )
) %>%
  mutate(
    stress_effect = c(
      mean(key_posterior[, "effect_stress_low_disinhibition"]),
      mean(key_posterior[, "effect_stress_high_disinhibition"])
    ),
    lower = c(
      quantile(key_posterior[, "effect_stress_low_disinhibition"], 0.025),
      quantile(key_posterior[, "effect_stress_high_disinhibition"], 0.025)
    ),
    upper = c(
      quantile(key_posterior[, "effect_stress_low_disinhibition"], 0.975),
      quantile(key_posterior[, "effect_stress_high_disinhibition"], 0.975)
    )
  )

ggplot(interaction_df, aes(x = disinhibition, y = stress_effect)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 4, color = "steelblue") +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.1,
    size = 1,
    color = "steelblue"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_x_continuous(
    breaks = c(-1, 1),
    labels = c("Low (-1 SD)", "High (+1 SD)")
  ) +
  labs(
    title = "Stress Effect on Articulation by Disinhibition Level",
    subtitle = sprintf("Outcome: %s", outcome_metric),
    x = "Disinhibition",
    y = "Stress Effect (baseline → pre-exam)",
    caption = "Error bars: 95% credible intervals"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

dev.off()

cat("✓ Grafici salvati\n")

# ==============================================================================
# 7) POSTERIOR PREDICTIVE CHECK
# ==============================================================================

cat("\n=== POSTERIOR PREDICTIVE CHECK ===\n")

y_rep <- fit$draws("y_rep", format = "matrix")
y_rep_sample <- y_rep[sample(nrow(y_rep), 100), ]

# PPC density
pdf(file.path(output_dir, "ppc_density.pdf"), width = 10, height = 6)
ppc_dens_overlay(stan_data$y, y_rep_sample) +
  labs(
    title = "Posterior Predictive Check: Density Overlay",
    subtitle = sprintf("Outcome: %s", outcome_metric),
    x = outcome_metric,
    y = "Density"
  )
dev.off()

# PPC intervals
pdf(file.path(output_dir, "ppc_intervals.pdf"), width = 10, height = 6)
ppc_intervals(
  stan_data$y,
  y_rep,
  prob = 0.5,
  prob_outer = 0.95
) +
  labs(
    title = "Posterior Predictive Check: 50% and 95% Intervals",
    subtitle = sprintf("Outcome: %s", outcome_metric),
    x = "Observation index",
    y = outcome_metric
  )
dev.off()

cat("✓ PPC grafici salvati\n")

# ==============================================================================
# 8) LOO-CV
# ==============================================================================

cat("\n=== LOO CROSS-VALIDATION ===\n")

log_lik <- fit$draws("log_lik", format = "matrix")
loo_result <- loo(log_lik)

cat("\nLOO summary:\n")
print(loo_result)

saveRDS(
  loo_result,
  file.path(output_dir, "loo_result.rds")
)

# Plot Pareto k
pdf(file.path(output_dir, "loo_pareto_k.pdf"), width = 10, height = 6)
plot(loo_result, label_points = TRUE) +
  labs(
    title = "LOO: Pareto k Diagnostic",
    subtitle = sprintf(
      "ELPD LOO: %.1f ± %.1f",
      loo_result$estimates["elpd_loo", "Estimate"],
      loo_result$estimates["elpd_loo", "SE"]
    )
  )
dev.off()

# ==============================================================================
# 9) SALVATAGGIO
# ==============================================================================

cat("\n=== SALVATAGGIO RISULTATI ===\n")

fit$save_object(file.path(output_dir, "fit_f2_disinhibition.rds"))

write_csv(
  as_tibble(draws),
  file.path(output_dir, "posterior_draws.csv.gz")
)

results_bundle <- list(
  fit = fit,
  stan_data = stan_data,
  interaction_summary = interaction_summary,
  conditional_effects = conditional_effects,
  loo_result = loo_result,
  diagnostics = diag_summary,
  outcome_metric = outcome_metric
)

saveRDS(
  results_bundle,
  file.path(output_dir, "results_bundle.rds")
)

# ==============================================================================
# 10) INTERPRETAZIONE
# ==============================================================================

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("INTERPRETAZIONE RISULTATI\n")
cat(rep("=", 80), "\n", sep = "")

cat(sprintf("\nOUTCOME: %s\n", outcome_metric))

cat("\n--- Effetto Stress (main effect) ---\n")
cat(sprintf(
  "β_c1 = %.3f [%.3f, %.3f]\n",
  mean(key_posterior[, "beta_c1"]),
  quantile(key_posterior[, "beta_c1"], 0.025),
  quantile(key_posterior[, "beta_c1"], 0.975)
))

cat("\n--- Interazione Disinhibition × Stress ---\n")
cat(sprintf(
  "β_c1×Disinhibition = %.3f [%.3f, %.3f]\n",
  interaction_summary$mean,
  interaction_summary$q025,
  interaction_summary$q975
))

if (interaction_summary$q025 * interaction_summary$q975 > 0) {
  cat("  → Credible interval DOES NOT include 0\n")
  cat("  → ROBUST evidence of moderation\n")
} else {
  cat("  → Credible interval includes 0\n")
  cat("  → WEAK/NO evidence of moderation\n")
}

cat("\n--- Effetti Condizionali ---\n")
cat(sprintf(
  "Low Disinhibition:  %.3f [%.3f, %.3f]\n",
  conditional_effects$mean[1],
  conditional_effects$q025[1],
  conditional_effects$q975[1]
))
cat(sprintf(
  "High Disinhibition: %.3f [%.3f, %.3f]\n",
  conditional_effects$mean[2],
  conditional_effects$q025[2],
  conditional_effects$q975[2]
))

cat("\n--- Interpretazione ---\n")

if (grepl("vsa", outcome_metric, ignore.case = TRUE)) {
  cat("Outcome = Vowel Space Area (VSA)\n")
  cat("  Positivo → espansione spazio vocalico\n")
  cat("  Negativo → centralizzazione (ridotta precisione)\n")
} else if (grepl("range", outcome_metric, ignore.case = TRUE)) {
  cat("Outcome = F2 Range\n")
  cat("  Positivo → maggiore estensione articolatoria\n")
  cat("  Negativo → compressione articolatoria\n")
} else if (grepl("centralization", outcome_metric, ignore.case = TRUE)) {
  cat("Outcome = Centralization Index\n")
  cat("  Positivo → maggiore dispersione (precisione)\n")
  cat("  Negativo → centralizzazione\n")
}

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("ANALISI COMPLETATA\n")
cat(rep("=", 80), "\n", sep = "")

cat("\nOUTPUT SALVATI IN:", output_dir, "\n")
cat("\nFile creati:\n")
cat("  - parameter_summary.csv\n")
cat("  - interaction_summary.csv\n")
cat("  - conditional_effects.csv\n")
cat("  - trace_plots.pdf\n")
cat("  - posterior_densities.pdf\n")
cat("  - posterior_intervals.pdf\n")
cat("  - interaction_plot.pdf\n")
cat("  - ppc_density.pdf\n")
cat("  - ppc_intervals.pdf\n")
cat("  - loo_pareto_k.pdf\n")
cat("  - fit_f2_disinhibition.rds\n")
cat("  - results_bundle.rds\n")
cat(rep("=", 80), "\n\n", sep = "")

# eof ---

#' In Methods - Acoustic Measures:
#'
#' We extracted formant frequencies (F1, F2) for vowels /a/, /i/, /u/ and
#' calculated vowel space area (VSA) as an index of articulatory precision.
#' However, preliminary analyses revealed that VSA showed inconsistent patterns
#' across stress conditions and high measurement variability, precluding robust
#' moderation analysis.
#'
#'
#' In Discussion - Limitations:
#' The present study focused on fundamental frequency as the primary vocal
#' marker of stress reactivity. While we attempted to examine articulatory
#' precision via VSA, formant-based measures showed high measurement variability,
#' likely reflecting the challenge of extracting reliable formants from brief,
#' naturalistic vocalizations collected in field settings. Future studies with
#' controlled phonetic elicitation tasks or articulographic methods may better
#' capture articulatory stress responses.
#'
#' ESM (Electronic Supplementary Materials)
#'
#' **ESM Table X: Bayesian Model Results for VSA Moderation**
#' We tested whether Disinhibition moderated stress effects on vowel space
#' area (VSA) using the same Bayesian hierarchical framework as for F0.
#' The model showed excellent convergence (all Rhat < 1.01, ESS > 2000),
#' but revealed no evidence of stress effects on VSA (β_stress = 0.01,
#' 95% CI [-0.23, 0.26]) nor of Disinhibition moderation (β_interaction =
#' 0.003, 95% CI [-0.37, 0.36]). Posterior predictive checks indicated
#' poor model fit, suggesting VSA may require alternative distributional
#' assumptions (e.g., skew-normal, mixture models) or more controlled
#' measurement conditions.
