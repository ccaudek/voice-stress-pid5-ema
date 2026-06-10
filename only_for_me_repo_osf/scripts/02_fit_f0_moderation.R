# =============================================================================
# 02_fit_f0_moderation.R
#
# Fits the Bayesian hierarchical F0 × PID-5 moderation model in Stan.
# Requires: results/F0/stan_bundle_f0.rds (run 01_prepare_f0_data.R first)
#
# Outputs (all in results/F0/):
#   fit_f0_moderation.RDS        fitted CmdStan object
#   summary_f0_moderation.csv    posterior summary (mean, SD, quantiles, R-hat, ESS)
#   moderation_pd_f0.csv         directional probabilities for all 10 interactions
#   convergence_summary.csv      key convergence diagnostics
#   ppc_f0.png                   posterior predictive check plot
#
# Expected runtime: ~15–25 minutes (4 cores, 6 000 post-warmup draws per chain).
# =============================================================================

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(bayesplot)
  library(tidyverse)
  library(here)
})

# -----------------------------------------------------------------------------
# 0. Load prepared data
# -----------------------------------------------------------------------------
bundle_path <- here("results", "F0", "stan_bundle_f0.rds")
if (!file.exists(bundle_path))
  stop("Bundle not found. Run 01_prepare_f0_data.R first.")

bundle <- readRDS(bundle_path)
stan_data <- bundle$stan_data
pid5_vars <- bundle$pid5_vars

cat(
  "Stan data loaded: N_subj =",
  stan_data$N_subj,
  "| N_voice =",
  stan_data$N_voice,
  "| N_ema =",
  stan_data$N_ema,
  "\n"
)

# -----------------------------------------------------------------------------
# 1. Compile model
# -----------------------------------------------------------------------------
mod <- cmdstan_model(here("stan", "F0", "f0mean_pid5_moderation.stan"))

# -----------------------------------------------------------------------------
# 2. Sample (or load cached fit)
# -----------------------------------------------------------------------------
fit_path <- here("results", "F0", "fit_f0_moderation.RDS")

if (file.exists(fit_path)) {
  message("Cached fit found — loading.")
  fit <- readRDS(fit_path)
} else {
  message("Sampling...")
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
  fit$save_object(file = fit_path)
  message("Saved: ", fit_path)
}

# -----------------------------------------------------------------------------
# 3. Convergence diagnostics
# -----------------------------------------------------------------------------
key_params <- c(
  "alpha",
  "b1",
  "b2",
  paste0("g1[", 1:5, "]"),
  paste0("g2[", 1:5, "]"),
  "sigma_y",
  paste0("tau[", 1:3, "]")
)

smry <- fit$summary(key_params)

conv <- tibble(
  n_divergences = fit$diagnostic_summary()$num_divergent |> sum(),
  max_rhat = max(smry$rhat, na.rm = TRUE),
  n_rhat_above_1p01 = sum(smry$rhat > 1.01, na.rm = TRUE),
  min_ess_bulk = min(smry$ess_bulk, na.rm = TRUE),
  min_ess_tail = min(smry$ess_tail, na.rm = TRUE)
)

cat("\n=== Convergence summary ===\n")
print(conv)
write_csv(conv, here("results", "F0", "convergence_summary.csv"))

if (conv$n_divergences > 0)
  warning(conv$n_divergences, " divergent transitions detected.")
if (conv$max_rhat > 1.01)
  warning("R-hat > 1.01 for ", conv$n_rhat_above_1p01, " parameters.")

# -----------------------------------------------------------------------------
# 4. Full posterior summary
# -----------------------------------------------------------------------------
full_smry <- fit$summary(c(
  "alpha",
  "b1",
  "b2",
  paste0("a_trait[", 1:5, "]"),
  paste0("g1[", 1:5, "]"),
  paste0("g2[", 1:5, "]"),
  "sigma_y",
  paste0("tau[", 1:3, "]"),
  paste0("sigma_ema[", 1:5, "]")
))

write_csv(
  as.data.frame(full_smry),
  here("results", "F0", "summary_f0_moderation.csv")
)
cat("\nSaved: results/F0/summary_f0_moderation.csv\n")

# -----------------------------------------------------------------------------
# 5. Posterior predictive check
# -----------------------------------------------------------------------------
y_rep <- fit$draws("y_rep", format = "matrix")

ppc <- ppc_dens_overlay(y = stan_data$y, yrep = y_rep[1:100, ]) +
  labs(
    title = "Posterior Predictive Check: F0 (Hz)",
    subtitle = "Dark = observed; light = 100 posterior replications"
  )

ggsave(
  here("results", "F0", "ppc_f0.png"),
  ppc,
  width = 8,
  height = 6,
  dpi = 300
)
cat("Saved: results/F0/ppc_f0.png\n")

# -----------------------------------------------------------------------------
# 6. Directional probabilities for moderation parameters
# -----------------------------------------------------------------------------
draws <- fit$draws()

pd <- function(x) max(mean(x > 0), mean(x < 0))

domain_labels <- c(
  "Negative Affectivity",
  "Detachment",
  "Antagonism",
  "Disinhibition",
  "Psychoticism"
)

mod_pd <- map_dfr(1:5, function(d) {
  g1d <- as.numeric(draws[,, paste0("g1[", d, "]")])
  g2d <- as.numeric(draws[,, paste0("g2[", d, "]")])
  tibble(
    domain = domain_labels[d],
    g1_median = round(median(g1d), 2),
    g1_lo95 = round(quantile(g1d, 0.025), 2),
    g1_hi95 = round(quantile(g1d, 0.975), 2),
    g1_pd = round(pd(g1d), 3),
    g2_median = round(median(g2d), 2),
    g2_lo95 = round(quantile(g2d, 0.025), 2),
    g2_hi95 = round(quantile(g2d, 0.975), 2),
    g2_pd = round(pd(g2d), 3)
  )
})

print(mod_pd)
write_csv(mod_pd, here("results", "F0", "moderation_pd_f0.csv"))
cat("Saved: results/F0/moderation_pd_f0.csv\n")

cat("\n=== F0 analysis complete ===\n")
cat("Next: run 03_prepare_nne_data.R\n")
