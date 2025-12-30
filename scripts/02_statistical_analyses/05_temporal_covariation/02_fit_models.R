# ==============================================================================
# 02_fit_models.R
# Fitting baseline e random slopes models
#
# Parametri MCMC ottimizzati per convergenza:
# - warmup: 2000
# - sampling: 5000
# - adapt_delta: 0.999
# - max_treedepth: 15
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(cmdstanr)
  library(posterior)
  library(loo)
  library(here)
})

# ==============================================================================
# SETUP
# ==============================================================================

data_dir <- here::here("results/within_person/data")
model_dir <- here::here("stan/within_person")
output_dir <- here::here("results/within_person/fitted_models")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Parametri MCMC (ottimizzati per convergenza)
MCMC_PARAMS <- list(
  chains = 4,
  iter_warmup = 2000,
  iter_sampling = 5000,
  adapt_delta = 0.999,
  max_treedepth = 15,
  refresh = 500,
  seed = 12345
)

n_cores <- parallel::detectCores() - 1

cat("=== FITTING MODELS ===\n\n")
cat("Parametri MCMC:\n")
for (param in names(MCMC_PARAMS)) {
  cat(sprintf("  %s: %s\n", param, MCMC_PARAMS[[param]]))
}
cat("\n")

# ==============================================================================
# CARICA DATI
# ==============================================================================

cat("Caricamento dati...\n")
stan_data <- readRDS(file.path(data_dir, "stan_data_within_person.rds"))
metadata <- readRDS(file.path(data_dir, "metadata.rds"))

cat(sprintf(
  "  N = %d, N_subj = %d, D = %d\n\n",
  stan_data$N,
  stan_data$N_subj,
  stan_data$D
))

# ==============================================================================
# MODELLO 1: BASELINE (Fixed Effects Only)
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("MODELLO 1: BASELINE (Fixed Effects Only)\n")
cat(rep("=", 80), "\n\n")

cat("Compilazione...\n")
mod_baseline <- cmdstan_model(file.path(model_dir, "baseline_model.stan"))

cat("Fitting...\n")
cat("(Questo richiederà ~1 minuti...)\n\n")

fit_baseline <- mod_baseline$sample(
  data = stan_data,
  chains = MCMC_PARAMS$chains,
  parallel_chains = min(MCMC_PARAMS$chains, n_cores),
  iter_warmup = MCMC_PARAMS$iter_warmup,
  iter_sampling = MCMC_PARAMS$iter_sampling,
  adapt_delta = MCMC_PARAMS$adapt_delta,
  max_treedepth = MCMC_PARAMS$max_treedepth,
  refresh = MCMC_PARAMS$refresh,
  seed = MCMC_PARAMS$seed
)

# Diagnostics
cat("\n--- DIAGNOSTICS BASELINE ---\n")
diag_baseline <- fit_baseline$summary(
  variables = c("alpha", "beta_wp", "sigma_alpha", "sigma_y", "r2_within")
)

max_rhat <- max(diag_baseline$rhat, na.rm = TRUE)
min_ess <- min(diag_baseline$ess_bulk, na.rm = TRUE)
r2_mean <- diag_baseline$mean[diag_baseline$variable == "r2_within"]

cat(sprintf("  max(Rhat): %.4f\n", max_rhat))
cat(sprintf("  min(ESS_bulk): %.0f\n", min_ess))
cat(sprintf("  R² within: %.4f\n", r2_mean))

if (max_rhat < 1.01 && min_ess > 400) {
  cat("  ✓ Convergenza ECCELLENTE\n")
} else if (max_rhat < 1.05 && min_ess > 100) {
  cat("  ✓ Convergenza ACCETTABILE\n")
} else {
  cat("  ⚠ Possibili problemi di convergenza\n")
}

# LOO
cat("\nCalcolo LOO...\n")
log_lik_baseline <- fit_baseline$draws("log_lik", format = "matrix")
loo_baseline <- loo(log_lik_baseline)

cat(sprintf(
  "  ELPD LOO: %.2f (SE: %.2f)\n",
  loo_baseline$estimates["elpd_loo", "Estimate"],
  loo_baseline$estimates["elpd_loo", "SE"]
))

# Salva
cat("\nSalvataggio risultati...\n")
fit_baseline$save_object(file.path(output_dir, "fit_baseline.rds"))
saveRDS(loo_baseline, file.path(output_dir, "loo_baseline.rds"))
write_csv(diag_baseline, file.path(output_dir, "diagnostics_baseline.csv"))

cat("✓ Baseline completato\n\n")

# ==============================================================================
# MODELLO 2: RANDOM SLOPES (Non-Centered)
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("MODELLO 2: RANDOM SLOPES (Non-Centered Parametrization)\n")
cat(rep("=", 80), "\n\n")

cat("Compilazione...\n")
mod_rs <- cmdstan_model(file.path(model_dir, "random_slopes_noncentered.stan"))

cat("Fitting...\n")
cat("(Questo richiederà ~4 minuti...)\n")
cat("☕ Vai a prendere un caffè (o due)!\n\n")

fit_rs <- mod_rs$sample(
  data = stan_data,
  chains = MCMC_PARAMS$chains,
  parallel_chains = min(MCMC_PARAMS$chains, n_cores),
  iter_warmup = MCMC_PARAMS$iter_warmup,
  iter_sampling = MCMC_PARAMS$iter_sampling,
  adapt_delta = MCMC_PARAMS$adapt_delta,
  max_treedepth = MCMC_PARAMS$max_treedepth,
  refresh = MCMC_PARAMS$refresh,
  seed = MCMC_PARAMS$seed
)

# Diagnostics
cat("\n--- DIAGNOSTICS RANDOM SLOPES ---\n")
diag_rs <- fit_rs$summary(
  variables = c("alpha", "beta_wp", "sigma_beta", "sigma_y", "r2_within")
)

max_rhat <- max(diag_rs$rhat, na.rm = TRUE)
min_ess <- min(diag_rs$ess_bulk, na.rm = TRUE)
r2_mean <- diag_rs$mean[diag_rs$variable == "r2_within"]

cat(sprintf("  max(Rhat): %.4f\n", max_rhat))
cat(sprintf("  min(ESS_bulk): %.0f\n", min_ess))
cat(sprintf("  R² within: %.4f\n", r2_mean))

if (max_rhat < 1.01 && min_ess > 400) {
  cat("  ✓ Convergenza ECCELLENTE\n")
} else if (max_rhat < 1.05 && min_ess > 100) {
  cat("  ✓ Convergenza ACCETTABILE\n")
} else {
  cat("  ⚠ Possibili problemi di convergenza\n")
}

# LOO
cat("\nCalcolo LOO...\n")
log_lik_rs <- fit_rs$draws("log_lik", format = "matrix")
loo_rs <- loo(log_lik_rs)

cat(sprintf(
  "  ELPD LOO: %.2f (SE: %.2f)\n",
  loo_rs$estimates["elpd_loo", "Estimate"],
  loo_rs$estimates["elpd_loo", "SE"]
))

# Salva
cat("\nSalvataggio risultati...\n")
fit_rs$save_object(file.path(output_dir, "fit_random_slopes.rds"))
saveRDS(loo_rs, file.path(output_dir, "loo_random_slopes.rds"))
write_csv(diag_rs, file.path(output_dir, "diagnostics_random_slopes.csv"))

cat("✓ Random Slopes completato\n\n")

# ==============================================================================
# MODEL COMPARISON
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("MODEL COMPARISON\n")
cat(rep("=", 80), "\n\n")

loo_comp <- loo_compare(list(
  baseline = loo_baseline,
  random_slopes = loo_rs
))

cat("LOO comparison:\n")
print(loo_comp)

# Delta ELPD
delta_elpd <- loo_comp[2, "elpd_diff"]
se_diff <- loo_comp[2, "se_diff"]

cat(sprintf("\nRandom Slopes vs Baseline:\n"))
cat(sprintf("  ΔELPD: %.2f (SE: %.2f)\n", abs(delta_elpd), se_diff))

if (abs(delta_elpd) > 4) {
  cat("  → DIFFERENZA SOSTANZIALE (ΔELPD > 4)\n")
} else if (abs(delta_elpd) > 2 * se_diff) {
  cat("  → Differenza significativa (ΔELPD > 2 SE)\n")
} else {
  cat("  → Differenza non significativa\n")
}

# R² comparison
r2_baseline <- diag_baseline$mean[diag_baseline$variable == "r2_within"]
r2_rs <- diag_rs$mean[diag_rs$variable == "r2_within"]

cat(sprintf("\nR² comparison:\n"))
cat(sprintf("  Baseline: %.4f\n", r2_baseline))
cat(sprintf("  Random Slopes: %.4f\n", r2_rs))
cat(sprintf(
  "  Improvement: %.1fx (%.1f%% points)\n",
  r2_rs / r2_baseline,
  (r2_rs - r2_baseline) * 100
))

# Salva comparison
comparison_results <- tibble(
  model = c("baseline", "random_slopes"),
  elpd = c(
    loo_baseline$estimates["elpd_loo", "Estimate"],
    loo_rs$estimates["elpd_loo", "Estimate"]
  ),
  se_elpd = c(
    loo_baseline$estimates["elpd_loo", "SE"],
    loo_rs$estimates["elpd_loo", "SE"]
  ),
  r2 = c(r2_baseline, r2_rs)
)

write_csv(comparison_results, file.path(output_dir, "model_comparison.csv"))

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("FITTING COMPLETATO\n")
cat(rep("=", 80), "\n\n")

cat("File salvati in:", output_dir, "\n")
cat("  - fit_baseline.rds\n")
cat("  - fit_random_slopes.rds\n")
cat("  - loo_baseline.rds\n")
cat("  - loo_random_slopes.rds\n")
cat("  - diagnostics_*.csv\n")
cat("  - model_comparison.csv\n\n")

cat("RISULTATO CHIAVE:\n")
if (abs(delta_elpd) > 4 && r2_rs > r2_baseline * 2) {
  cat("  ✓ Random slopes SOSTANZIALMENTE migliore!\n")
  cat("  → Eterogeneità individuale confermata\n")
  cat("  → Vocal-affective coupling è IDIOGRAFICO\n")
} else {
  cat("  → Risultati ambigui, verifica diagnostics\n")
}

cat("\n✓ Prossimo step: source('03_analyze_heterogeneity.R')\n\n")

# eof ---
