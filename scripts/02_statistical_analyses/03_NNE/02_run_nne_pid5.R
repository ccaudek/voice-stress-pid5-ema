# ==============================================================================
# 02_run_nne_pid5.R (REVISED)
# Fit del modello NNE × PID-5 con salvataggio risultati
# ==============================================================================

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(bayesplot)
  library(here)
  library(tidyverse)
})

# ==============================================================================
# LOAD DATA
# ==============================================================================

# Carica bundle (già preparato da script 01)
bundle_path <- here("results", "NNE", "stan_bundle_nne_pid5.rds")
if (!file.exists(bundle_path)) {
  stop("Bundle non trovato! Esegui prima 01_prepare_stan_data_nne_pid5.R")
}

bundle <- readRDS(bundle_path)
stan_data <- bundle$stan_data
pid5_vars <- bundle$pid5_vars

cat("Loaded stan_data with:\n")
cat("  N_subj =", stan_data$N_subj, "\n")
cat("  N_voice =", stan_data$N_voice, "\n")
cat("  N_ema =", stan_data$N_ema, "\n")
cat("  D =", stan_data$D, "domains\n\n")

# ==============================================================================
# COMPILE & SAMPLE (oppure carica fit esistente)
# ==============================================================================

fit_path <- here("stan", "NNE", "nne_mean_pid5_moderation.rds")

# Se fit non esiste, compila e campiona
if (!file.exists(fit_path)) {
  cat("Fitting model...\n")

  mod <- cmdstan_model(here("stan", "NNE", "nne_pid5_moderation.stan"))

  fit <- mod$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 2000,
    iter_sampling = 6000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123,
    refresh = 500
  )

  # Salva
  fit$save_object(file = fit_path)
  cat("Saved fit to:", fit_path, "\n")
} else {
  cat("Loading existing fit from:", fit_path, "\n")
  fit <- readRDS(fit_path)
}

# ==============================================================================
# DIAGNOSTICS
# ==============================================================================

cat("\n=== MCMC DIAGNOSTICS ===\n")

# Summary parametri chiave
summary_results <- fit$summary(c(
  "alpha",
  "b1",
  "b2",
  "g1",
  "g2",
  "sigma_y",
  "tau",
  "sigma_ema"
))

print(summary_results, n = Inf)

# Salva summary
write_csv(
  summary_results,
  here("results", "NNE", "model_summary_nne_moderation.csv")
)
cat("\nSaved: results/NNE/model_summary_nne_moderation.csv\n")

# Check convergence
max_rhat <- max(summary_results$rhat, na.rm = TRUE)
min_ess_bulk <- min(summary_results$ess_bulk, na.rm = TRUE)
min_ess_tail <- min(summary_results$ess_tail, na.rm = TRUE)

cat("\nConvergence checks:\n")
cat("  Max Rhat:", round(max_rhat, 4), ifelse(max_rhat < 1.01, "✓", "⚠"), "\n")
cat(
  "  Min ESS bulk:",
  round(min_ess_bulk),
  ifelse(min_ess_bulk > 1000, "✓", "⚠"),
  "\n"
)
cat(
  "  Min ESS tail:",
  round(min_ess_tail),
  ifelse(min_ess_tail > 1000, "✓", "⚠"),
  "\n"
)

# ==============================================================================
# POSTERIOR PREDICTIVE CHECK
# ==============================================================================

cat("\n=== POSTERIOR PREDICTIVE CHECK ===\n")

y_rep_draws <- fit$draws("y_rep", format = "matrix")

ppc_plot <- ppc_dens_overlay(
  y = stan_data$y,
  yrep = y_rep_draws[1:100, ]
) +
  labs(
    title = "Posterior Predictive Check: NNE",
    subtitle = "Dark line = observed data; Light lines = replications"
  )

print(ppc_plot)

ggsave(
  filename = here("results", "NNE", "ppc_nne_moderation.png"),
  plot = ppc_plot,
  width = 8,
  height = 6,
  dpi = 300
)

cat("Saved: results/NNE/ppc_nne_moderation.png\n")

# ==============================================================================
# EXTRACT KEY RESULTS
# ==============================================================================

cat("\n=== EXTRACTING KEY PARAMETERS ===\n")

draws <- fit$draws()

# Helper functions
pd <- function(x) max(mean(x > 0), mean(x < 0))

# Main effects
b1 <- as.numeric(draws[,, "b1"])
b2 <- as.numeric(draws[,, "b2"])

cat("\nMain Effects:\n")
cat(sprintf(
  "  b1 (stress):   median=%.3f, PD=%.3f, P(>0)=%.3f\n",
  median(b1),
  pd(b1),
  mean(b1 > 0)
))
cat(sprintf(
  "  b2 (recovery): median=%.3f, PD=%.3f, P(>0)=%.3f\n",
  median(b2),
  pd(b2),
  mean(b2 > 0)
))

# Moderation effects
cat("\nModeration Effects (PD):\n")
for (d in 1:5) {
  g1d <- as.numeric(draws[,, paste0("g1[", d, "]")])
  g2d <- as.numeric(draws[,, paste0("g2[", d, "]")])

  cat(sprintf("  %s:\n", pid5_vars[d]))
  cat(sprintf(
    "    Stress (g1):   PD=%.3f, P(>0)=%.3f\n",
    pd(g1d),
    mean(g1d > 0)
  ))
  cat(sprintf(
    "    Recovery (g2): PD=%.3f, P(>0)=%.3f\n",
    pd(g2d),
    mean(g2d > 0)
  ))
}

cat("\n=== FITTING COMPLETE ===\n")
cat("Next steps:\n")
cat("  1. Run 03_check_expected_direction.R (understand NNE direction)\n")
cat("  2. Run 04_interpret_nne_pid5_fit.R (detailed interpretation)\n")
cat("  3. Run 05_differential_effects_nne.R (publication-ready analysis)\n")

# eof ---
