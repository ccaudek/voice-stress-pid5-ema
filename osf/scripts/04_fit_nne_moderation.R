# =============================================================================
# 04_fit_nne_moderation.R
#
# Fits the Bayesian hierarchical NNE × PID-5 moderation model in Stan.
# Requires: results/NNE/stan_bundle_nne.rds (run 03_prepare_nne_data.R first)
#
# Outputs (all in results/NNE/):
#   fit_nne_moderation.RDS
#   summary_nne_moderation.csv
#   moderation_pd_nne.csv
#   convergence_summary.csv
#   ppc_nne.png
# =============================================================================

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(bayesplot)
  library(tidyverse)
  library(here)
})

bundle_path <- here("results", "NNE", "stan_bundle_nne.rds")
if (!file.exists(bundle_path)) stop("Bundle not found. Run 03_prepare_nne_data.R first.")

bundle    <- readRDS(bundle_path)
stan_data <- bundle$stan_data
pid5_vars <- bundle$pid5_vars

mod      <- cmdstan_model(here("stan", "NNE", "nne_pid5_moderation.stan"))
fit_path <- here("results", "NNE", "fit_nne_moderation.RDS")

if (file.exists(fit_path)) {
  message("Cached fit found — loading.")
  fit <- readRDS(fit_path)
} else {
  message("Sampling...")
  fit <- mod$sample(
    data            = stan_data,
    chains          = 4,
    parallel_chains = 4,
    iter_warmup     = 2000,
    iter_sampling   = 6000,
    adapt_delta     = 0.99,
    max_treedepth   = 15,
    seed            = 123,
    refresh         = 500
  )
  fit$save_object(file = fit_path)
  message("Saved: ", fit_path)
}

# -----------------------------------------------------------------------------
# Convergence
# -----------------------------------------------------------------------------
key_params <- c("alpha", "b1", "b2",
                paste0("g1[", 1:5, "]"), paste0("g2[", 1:5, "]"),
                "sigma_y", paste0("tau[", 1:3, "]"))

smry <- fit$summary(key_params)
conv <- tibble(
  n_divergences     = fit$diagnostic_summary()$num_divergent |> sum(),
  max_rhat          = max(smry$rhat, na.rm = TRUE),
  n_rhat_above_1p01 = sum(smry$rhat > 1.01, na.rm = TRUE),
  min_ess_bulk      = min(smry$ess_bulk, na.rm = TRUE),
  min_ess_tail      = min(smry$ess_tail, na.rm = TRUE)
)

cat("\n=== NNE convergence summary ===\n"); print(conv)
write_csv(conv, here("results", "NNE", "convergence_summary.csv"))

# -----------------------------------------------------------------------------
# Full posterior summary
# -----------------------------------------------------------------------------
full_smry <- fit$summary(c("alpha", "b1", "b2",
                            paste0("a_trait[", 1:5, "]"),
                            paste0("g1[", 1:5, "]"),
                            paste0("g2[", 1:5, "]"),
                            "sigma_y",
                            paste0("tau[", 1:3, "]"),
                            paste0("sigma_ema[", 1:5, "]")))

write_csv(as.data.frame(full_smry),
          here("results", "NNE", "summary_nne_moderation.csv"))

# -----------------------------------------------------------------------------
# Posterior predictive check
# -----------------------------------------------------------------------------
y_rep <- fit$draws("y_rep", format = "matrix")
ppc <- ppc_dens_overlay(y = stan_data$y, yrep = y_rep[1:100, ]) +
  labs(title = "Posterior Predictive Check: NNE (dB)",
       subtitle = "Dark = observed; light = 100 posterior replications")

ggsave(here("results", "NNE", "ppc_nne.png"), ppc,
       width = 8, height = 6, dpi = 300)

# -----------------------------------------------------------------------------
# Directional probabilities (Table 3 in manuscript)
# -----------------------------------------------------------------------------
draws <- fit$draws()
pd    <- function(x) max(mean(x > 0), mean(x < 0))

domain_labels <- c(
  "Negative Affectivity", "Detachment", "Antagonism",
  "Disinhibition", "Psychoticism"
)

mod_pd <- map_dfr(1:5, function(d) {
  g1d <- as.numeric(draws[,, paste0("g1[", d, "]")])
  g2d <- as.numeric(draws[,, paste0("g2[", d, "]")])
  tibble(
    domain    = domain_labels[d],
    g1_median = round(median(g1d), 2),
    g1_lo95   = round(quantile(g1d, 0.025), 2),
    g1_hi95   = round(quantile(g1d, 0.975), 2),
    g1_pd     = round(pd(g1d), 3),
    g2_median = round(median(g2d), 2),
    g2_lo95   = round(quantile(g2d, 0.025), 2),
    g2_hi95   = round(quantile(g2d, 0.975), 2),
    g2_pd     = round(pd(g2d), 3)
  )
})

print(mod_pd)
write_csv(mod_pd, here("results", "NNE", "moderation_pd_nne.csv"))
cat("\n=== NNE analysis complete ===\n")
cat("Next: run 05_loo_comparison.R\n")
