# Script di Test per i Modelli Stan Migliorati
# ============================================

library(cmdstanr)
library(posterior)
library(bayesplot)
library(loo)

# Sampling settings (inizia conservativo)
n_chains <- 4
n_warmup <- 2000
n_sampling <- 4000 # ridotto per test iniziali
adapt_delta <- 0.995 # ridotto da 0.999
max_treedepth <- 15 # ridotto da 17

# ============================================
# 1. TEST MODELLO BASELINE (il più semplice)
# ============================================

cat("\n=== Testing BASELINE model ===\n")

# Compila
mod_baseline <- cmdstan_model(
  "stan/followup/pid5_baseline_moderation_improved.stan"
)

# Fit (assumendo che tu abbia già preparato stan_data_baseline)
fit_baseline <- mod_baseline$sample(
  data = stan_data_baseline,
  chains = n_chains,
  parallel_chains = n_chains,
  iter_warmup = n_warmup,
  iter_sampling = n_sampling,
  adapt_delta = adapt_delta,
  max_treedepth = max_treedepth,
  refresh = 500,
  show_messages = TRUE
)

# Diagnostiche immediate
cat(
  "\nDivergent transitions:",
  sum(fit_baseline$diagnostic_summary()$num_divergent),
  "\n"
)
cat(
  "Max treedepth hits:",
  sum(fit_baseline$diagnostic_summary()$num_max_treedepth),
  "\n"
)

# Controlla Rhat e ESS
summ_baseline <- fit_baseline$summary()
cat("\nParametri con Rhat > 1.01:\n")
print(summ_baseline[
  summ_baseline$rhat > 1.01,
  c("variable", "rhat", "ess_bulk", "ess_tail")
])

# LOO diagnostics
loo_baseline <- fit_baseline$loo(cores = 4)
print(loo_baseline)
plot(loo_baseline, label_points = TRUE)

# Se buono, salva
if (max(summ_baseline$rhat, na.rm = TRUE) < 1.01) {
  cat("\n✓ Baseline model converged well!\n")
  fit_baseline$save_object("fit_baseline_improved.rds")

  # Puoi aumentare sampling
  cat("\nConsider increasing n_sampling to 6000 for final run\n")
} else {
  cat("\n✗ Convergence issues detected. Check diagnostics.\n")
}

# ============================================
# 2. TEST MODELLO EMA
# ============================================

cat("\n=== Testing EMA model ===\n")

# Compila
mod_ema <- cmdstan_model("f0mean_pid5_moderation_improved.stan")

# Fit
fit_ema <- mod_ema$sample(
  data = stan_data_ema,
  chains = n_chains,
  parallel_chains = n_chains,
  iter_warmup = n_warmup,
  iter_sampling = n_sampling,
  adapt_delta = adapt_delta,
  max_treedepth = max_treedepth,
  refresh = 500,
  show_messages = TRUE
)

# Diagnostiche
cat(
  "\nDivergent transitions:",
  sum(fit_ema$diagnostic_summary()$num_divergent),
  "\n"
)
cat(
  "Max treedepth hits:",
  sum(fit_ema$diagnostic_summary()$num_max_treedepth),
  "\n"
)

summ_ema <- fit_ema$summary()
cat("\nParametri con Rhat > 1.01:\n")
print(summ_ema[
  summ_ema$rhat > 1.01,
  c("variable", "rhat", "ess_bulk", "ess_tail")
])

# LOO
loo_ema <- fit_ema$loo(cores = 4)
print(loo_ema)
plot(loo_ema, label_points = TRUE)

if (max(summ_ema$rhat, na.rm = TRUE) < 1.01) {
  cat("\n✓ EMA model converged well!\n")
  fit_ema$save_object("fit_ema_improved.rds")
} else {
  cat("\n✗ Convergence issues detected.\n")
}

# ============================================
# 3. TEST MODELLO COMBINED (il più complesso)
# ============================================

cat("\n=== Testing COMBINED model ===\n")

# Compila
mod_combined <- cmdstan_model(
  "stan/followup/pid5_ema_plus_baseline_moderation_improved.stan"
)

# Fit
fit_combined <- mod_combined$sample(
  data = stan_data_combined,
  chains = n_chains,
  parallel_chains = n_chains,
  iter_warmup = n_warmup,
  iter_sampling = n_sampling,
  adapt_delta = adapt_delta,
  max_treedepth = max_treedepth,
  refresh = 500,
  show_messages = TRUE
)

# Diagnostiche
cat(
  "\nDivergent transitions:",
  sum(fit_combined$diagnostic_summary()$num_divergent),
  "\n"
)
cat(
  "Max treedepth hits:",
  sum(fit_combined$diagnostic_summary()$num_max_treedepth),
  "\n"
)

summ_combined <- fit_combined$summary()
cat("\nParametri con Rhat > 1.01:\n")
print(summ_combined[
  summ_combined$rhat > 1.01,
  c("variable", "rhat", "ess_bulk", "ess_tail")
])

# LOO
loo_combined <- fit_combined$loo(cores = 4)
print(loo_combined)
plot(loo_combined, label_points = TRUE)

if (max(summ_combined$rhat, na.rm = TRUE) < 1.01) {
  cat("\n✓ Combined model converged well!\n")
  fit_combined$save_object("fit_combined_improved.rds")
} else {
  cat("\n✗ Convergence issues detected.\n")
}

# ============================================
# 4. CONFRONTO MODELLI
# ============================================

cat("\n=== Model Comparison ===\n")

# LOO comparison
loo_compare(loo_baseline, loo_ema, loo_combined)

# Summary delle Pareto k
cat("\n--- Pareto k Summary ---\n")
for (model_name in c("Baseline", "EMA", "Combined")) {
  loo_obj <- get(paste0("loo_", tolower(model_name)))

  pareto_k <- loo_obj$diagnostics$pareto_k
  cat("\n", model_name, ":\n", sep = "")
  cat("  Good (k < 0.5):", sum(pareto_k < 0.5), "\n")
  cat("  OK (0.5 ≤ k < 0.7):", sum(pareto_k >= 0.5 & pareto_k < 0.7), "\n")
  cat("  Bad (0.7 ≤ k < 1):", sum(pareto_k >= 0.7 & pareto_k < 1), "\n")
  cat("  Very bad (k ≥ 1):", sum(pareto_k >= 1), "\n")
}

# ============================================
# 5. VISUAL DIAGNOSTICS
# ============================================

# Esempio per baseline (ripeti per altri)
draws_baseline <- fit_baseline$draws()

# Trace plots per parametri chiave
bayesplot::mcmc_trace(
  draws_baseline,
  pars = c("alpha", "b1", "b2", "tau[1]", "tau[2]", "tau[3]")
)

# Pairs plot per correlazioni
bayesplot::mcmc_pairs(draws_baseline, pars = c("alpha", "b1", "b2", "sigma_y"))

# Posterior predictive check
y_rep_baseline <- as_draws_matrix(draws_baseline, variable = "y_rep")
bayesplot::ppc_dens_overlay(
  y = stan_data_baseline$y,
  yrep = y_rep_baseline[1:100, ]
)

# ============================================
# 6. SALVA RISULTATI DIAGNOSTICI
# ============================================

# Crea tabella comparativa
comparison_table <- data.frame(
  Model = c("Baseline", "EMA", "Combined"),
  Max_Rhat = c(
    max(summ_baseline$rhat, na.rm = TRUE),
    max(summ_ema$rhat, na.rm = TRUE),
    max(summ_combined$rhat, na.rm = TRUE)
  ),
  Min_ESS_bulk = c(
    min(summ_baseline$ess_bulk, na.rm = TRUE),
    min(summ_ema$ess_bulk, na.rm = TRUE),
    min(summ_combined$ess_bulk, na.rm = TRUE)
  ),
  N_divergent = c(
    sum(fit_baseline$diagnostic_summary()$num_divergent),
    sum(fit_ema$diagnostic_summary()$num_divergent),
    sum(fit_combined$diagnostic_summary()$num_divergent)
  ),
  Pareto_k_good = c(
    sum(loo_baseline$diagnostics$pareto_k < 0.5),
    sum(loo_ema$diagnostics$pareto_k < 0.5),
    sum(loo_combined$diagnostics$pareto_k < 0.5)
  ),
  Pareto_k_bad_or_worse = c(
    sum(loo_baseline$diagnostics$pareto_k >= 0.7),
    sum(loo_ema$diagnostics$pareto_k >= 0.7),
    sum(loo_combined$diagnostics$pareto_k >= 0.7)
  )
)

print(comparison_table)
write.csv(comparison_table, "model_diagnostics_improved.csv", row.names = FALSE)

cat("\n✓ All diagnostics saved!\n")

# ============================================
# 7. SE ANCORA PROBLEMI...
# ============================================

# Funzione helper per aumentare adapt_delta gradualmente
increase_adapt_delta <- function(fit, data, model, current_delta = 0.995) {
  n_div <- sum(fit$diagnostic_summary()$num_divergent)

  if (n_div > 0) {
    cat("\nFound", n_div, "divergences. Trying higher adapt_delta...\n")

    new_delta <- min(current_delta + 0.001, 0.9999)
    cat("Retrying with adapt_delta =", new_delta, "\n")

    fit_new <- model$sample(
      data = data,
      chains = n_chains,
      parallel_chains = n_chains,
      iter_warmup = n_warmup,
      iter_sampling = n_sampling,
      adapt_delta = new_delta,
      max_treedepth = max_treedepth,
      refresh = 500
    )

    return(fit_new)
  } else {
    cat("\nNo divergences detected. Model is good!\n")
    return(fit)
  }
}

# Esempio d'uso:
# fit_baseline <- increase_adapt_delta(fit_baseline, stan_data_baseline, mod_baseline)
