# ==============================================================================
# 06_mcmc_diagnostics.R
# MCMC Convergence Diagnostics for F0 Moderation Model
#
# Produces:
# - Table of convergence diagnostics (Rhat, ESS) for key parameters
# - Trace plots for key parameters
# - Posterior predictive check figures
# - Summary saved to CSV for supplementary materials
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(cmdstanr)
  library(posterior)
  library(bayesplot)
  library(here)
})

# Set bayesplot color scheme
color_scheme_set("brightblue")

# ==============================================================================
# 1) LOAD FITTED MODEL
# ==============================================================================

fit_rds <- here("stan", "F0", "f0mean_pid5_moderation.RDS")
bundle_rds <- here("results", "stan_bundle_f0mean_pid5.rds")

if (!file.exists(fit_rds)) {
  stop(
    "Fitted model not found: ",
    fit_rds,
    "\nRun 02_f0mean_pid5_moderation.R first."
  )
}

fit <- readRDS(fit_rds)
bundle <- readRDS(bundle_rds)
stan_data <- bundle$stan_data
pid5_vars <- bundle$pid5_vars

# Create output directory
out_dir <- here("results", "diagnostics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== MCMC DIAGNOSTICS FOR F0 MODERATION MODEL ===\n\n")

# ==============================================================================
# 2) SAMPLING DIAGNOSTICS
# ==============================================================================

cat("--- Sampling Information ---\n")

# Get sampler diagnostics
sampler_diag <- fit$diagnostic_summary()

cat("Divergent transitions:", sum(sampler_diag$num_divergent), "\n")
cat("Max treedepth exceeded:", sum(sampler_diag$num_max_treedepth), "\n")
cat("Low E-BFMI warnings:", sum(sampler_diag$ebfmi < 0.2), "chains\n")

# Save sampler diagnostics
sampler_summary <- data.frame(
  Diagnostic = c(
    "Divergent transitions (total)",
    "Max treedepth exceeded (total)",
    "Chains with low E-BFMI (<0.2)"
  ),
  Value = c(
    sum(sampler_diag$num_divergent),
    sum(sampler_diag$num_max_treedepth),
    sum(sampler_diag$ebfmi < 0.2)
  )
)

write_csv(sampler_summary, file.path(out_dir, "sampler_diagnostics.csv"))

# ==============================================================================
# 3) CONVERGENCE DIAGNOSTICS (Rhat, ESS)
# ==============================================================================

cat("\n--- Convergence Diagnostics ---\n")

# Define key parameters to report
key_params <- c(
  "alpha",
  "b1",
  "b2",
  paste0("g1[", 1:5, "]"),
  paste0("g2[", 1:5, "]"),
  paste0("a_trait[", 1:5, "]"),
  "sigma_y",
  paste0("tau[", 1:3, "]"),
  paste0("sigma_ema[", 1:5, "]")
)

# Get summary for key parameters
summary_key <- fit$summary(variables = key_params)

# Add readable parameter names
param_labels <- c(
  "alpha" = "Intercept (α)",
  "b1" = "Stress effect (β₁)",
  "b2" = "Recovery effect (β₂)",
  "g1[1]" = "γ₁ Negative Affectivity",
  "g1[2]" = "γ₁ Detachment",
  "g1[3]" = "γ₁ Antagonism",
  "g1[4]" = "γ₁ Disinhibition",
  "g1[5]" = "γ₁ Psychoticism",
  "g2[1]" = "γ₂ Negative Affectivity",
  "g2[2]" = "γ₂ Detachment",
  "g2[3]" = "γ₂ Antagonism",
  "g2[4]" = "γ₂ Disinhibition",
  "g2[5]" = "γ₂ Psychoticism",
  "a_trait[1]" = "a Negative Affectivity",
  "a_trait[2]" = "a Detachment",
  "a_trait[3]" = "a Antagonism",
  "a_trait[4]" = "a Disinhibition",
  "a_trait[5]" = "a Psychoticism",
  "sigma_y" = "Residual SD (σ_y)",
  "tau[1]" = "Random intercept SD (τ₁)",
  "tau[2]" = "Random stress slope SD (τ₂)",
  "tau[3]" = "Random recovery slope SD (τ₃)",
  "sigma_ema[1]" = "σ_ema Negative Affectivity",
  "sigma_ema[2]" = "σ_ema Detachment",
  "sigma_ema[3]" = "σ_ema Antagonism",
  "sigma_ema[4]" = "σ_ema Disinhibition",
  "sigma_ema[5]" = "σ_ema Psychoticism"
)

summary_key <- summary_key %>%
  mutate(
    Parameter_Label = param_labels[variable],
    Parameter_Label = ifelse(is.na(Parameter_Label), variable, Parameter_Label)
  ) %>%
  select(
    Parameter = Parameter_Label,
    Mean = mean,
    SD = sd,
    `2.5%` = q5,
    `97.5%` = q95,
    Rhat = rhat,
    ESS_bulk = ess_bulk,
    ESS_tail = ess_tail
  )

# Print summary
print(summary_key, n = 30)

# Check for convergence issues
rhat_issues <- sum(summary_key$Rhat > 1.01, na.rm = TRUE)
ess_issues <- sum(
  summary_key$ESS_bulk < 400 | summary_key$ESS_tail < 400,
  na.rm = TRUE
)

cat("\nParameters with Rhat > 1.01:", rhat_issues, "\n")
cat("Parameters with ESS < 400:", ess_issues, "\n")

# Save full summary
write_csv(
  summary_key,
  file.path(out_dir, "convergence_diagnostics_key_params.csv")
)

# ==============================================================================
# 4) SUMMARY STATISTICS FOR SUPPLEMENTARY TEXT
# ==============================================================================

# Calculate summary statistics for reporting
rhat_range <- range(summary_key$Rhat, na.rm = TRUE)
rhat_max <- max(summary_key$Rhat, na.rm = TRUE)
ess_bulk_range <- range(summary_key$ESS_bulk, na.rm = TRUE)
ess_bulk_min <- min(summary_key$ESS_bulk, na.rm = TRUE)
ess_tail_min <- min(summary_key$ESS_tail, na.rm = TRUE)

# Get total number of parameters (including theta, u, z_u)
all_params <- fit$summary()
n_total_params <- nrow(all_params)
n_rhat_above_101 <- sum(all_params$rhat > 1.01, na.rm = TRUE)
n_rhat_above_105 <- sum(all_params$rhat > 1.05, na.rm = TRUE)

cat("\n--- Summary for Supplementary Text ---\n")
cat("Total parameters:", n_total_params, "\n")
cat("Parameters with Rhat > 1.01:", n_rhat_above_101, "\n")
cat("Parameters with Rhat > 1.05:", n_rhat_above_105, "\n")
cat(
  "Rhat range (key params):",
  round(rhat_range[1], 3),
  "-",
  round(rhat_range[2], 3),
  "\n"
)
cat("Max Rhat (key params):", round(rhat_max, 3), "\n")
cat(
  "ESS bulk range (key params):",
  round(ess_bulk_range[1]),
  "-",
  round(ess_bulk_range[2]),
  "\n"
)
cat("Min ESS bulk (key params):", round(ess_bulk_min), "\n")
cat("Min ESS tail (key params):", round(ess_tail_min), "\n")

# Save summary for text
text_summary <- data.frame(
  Metric = c(
    "Total parameters",
    "Chains",
    "Iterations (warmup)",
    "Iterations (sampling)",
    "Total post-warmup draws",
    "adapt_delta",
    "max_treedepth",
    "Divergent transitions",
    "Max treedepth exceeded",
    "Rhat max (key params)",
    "Rhat > 1.01 (all params)",
    "ESS bulk min (key params)",
    "ESS tail min (key params)"
  ),
  Value = c(
    n_total_params,
    4,
    2000,
    6000,
    4 * 6000,
    0.99,
    15,
    sum(sampler_diag$num_divergent),
    sum(sampler_diag$num_max_treedepth),
    round(rhat_max, 3),
    n_rhat_above_101,
    round(ess_bulk_min),
    round(ess_tail_min)
  )
)

write_csv(text_summary, file.path(out_dir, "diagnostics_text_summary.csv"))

# ==============================================================================
# 5) TRACE PLOTS
# ==============================================================================

cat("\n--- Generating Trace Plots ---\n")

draws <- fit$draws()

# Main effects trace plot
p_trace_main <- mcmc_trace(
  draws,
  pars = c("alpha", "b1", "b2", "sigma_y"),
  facet_args = list(ncol = 2)
) +
  ggtitle("Trace Plots: Main Effects") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(
  file.path(out_dir, "trace_main_effects.png"),
  p_trace_main,
  width = 10,
  height = 8,
  dpi = 300
)

# Moderation effects (g1) trace plot
p_trace_g1 <- mcmc_trace(
  draws,
  pars = paste0("g1[", 1:5, "]"),
  facet_args = list(ncol = 2)
) +
  ggtitle("Trace Plots: Stress Moderation (γ₁)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(
  file.path(out_dir, "trace_g1_moderation.png"),
  p_trace_g1,
  width = 10,
  height = 10,
  dpi = 300
)

# Moderation effects (g2) trace plot
p_trace_g2 <- mcmc_trace(
  draws,
  pars = paste0("g2[", 1:5, "]"),
  facet_args = list(ncol = 2)
) +
  ggtitle("Trace Plots: Recovery Moderation (γ₂)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(
  file.path(out_dir, "trace_g2_moderation.png"),
  p_trace_g2,
  width = 10,
  height = 10,
  dpi = 300
)

# Variance components trace plot
p_trace_var <- mcmc_trace(
  draws,
  pars = c("tau[1]", "tau[2]", "tau[3]", "sigma_y"),
  facet_args = list(ncol = 2)
) +
  ggtitle("Trace Plots: Variance Components") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(
  file.path(out_dir, "trace_variance_components.png"),
  p_trace_var,
  width = 10,
  height = 8,
  dpi = 300
)

cat("Trace plots saved.\n")

# ==============================================================================
# 6) RHAT AND ESS PLOTS
# ==============================================================================

cat("\n--- Generating Rhat/ESS Plots ---\n")

# Rhat histogram for all parameters
p_rhat <- mcmc_rhat_hist(all_params$rhat) +
  ggtitle("Distribution of R-hat Values (All Parameters)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(
  file.path(out_dir, "rhat_histogram.png"),
  p_rhat,
  width = 8,
  height = 6,
  dpi = 300
)

# ESS plot for key parameters
# Extract ESS ratios from summary (ESS / total draws)
total_draws <- 4 * 6000 # chains * iterations

ess_ratios <- summary_key %>%
  select(Parameter, ESS_bulk) %>%
  mutate(ratio = ESS_bulk / total_draws) %>%
  arrange(ratio)

p_ess <- ggplot(ess_ratios, aes(x = ratio, y = reorder(Parameter, ratio))) +
  geom_point(size = 3, color = "steelblue") +
  geom_vline(
    xintercept = 0.1,
    linetype = "dashed",
    color = "red",
    alpha = 0.7
  ) +
  scale_x_continuous(limits = c(0, max(ess_ratios$ratio) * 1.1)) +
  labs(
    x = "ESS / Total Draws",
    y = NULL,
    title = "Effective Sample Size Ratios (Key Parameters)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 8)
  )

ggsave(
  file.path(out_dir, "ess_ratios.png"),
  p_ess,
  width = 10,
  height = 8,
  dpi = 300
)

cat("Rhat/ESS plots saved.\n")

# ==============================================================================
# 7) POSTERIOR PREDICTIVE CHECKS
# ==============================================================================

cat("\n--- Generating Posterior Predictive Checks ---\n")

y <- stan_data$y
y_rep <- fit$draws("y_rep", format = "matrix")

# Density overlay
p_ppc_dens <- ppc_dens_overlay(y, y_rep[1:100, ]) +
  ggtitle("Posterior Predictive Check: Density Overlay") +
  xlab("F0 (Hz)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(
  file.path(out_dir, "ppc_density_overlay.png"),
  p_ppc_dens,
  width = 8,
  height = 6,
  dpi = 300
)

# Stat comparison: mean
p_ppc_mean <- ppc_stat(y, y_rep, stat = "mean") +
  ggtitle("PPC: Distribution of Means") +
  xlab("Mean F0 (Hz)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(
  file.path(out_dir, "ppc_stat_mean.png"),
  p_ppc_mean,
  width = 8,
  height = 6,
  dpi = 300
)

# Stat comparison: SD
p_ppc_sd <- ppc_stat(y, y_rep, stat = "sd") +
  ggtitle("PPC: Distribution of Standard Deviations") +
  xlab("SD of F0 (Hz)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(
  file.path(out_dir, "ppc_stat_sd.png"),
  p_ppc_sd,
  width = 8,
  height = 6,
  dpi = 300
)

# Intervals by group (timepoint)
# Create group variable
timepoint <- rep(
  c("Baseline", "Pre-exam", "Post-exam"),
  times = stan_data$N_subj
)
timepoint <- factor(timepoint, levels = c("Baseline", "Pre-exam", "Post-exam"))

p_ppc_grouped <- ppc_stat_grouped(
  y,
  y_rep,
  group = timepoint,
  stat = "mean"
) +
  ggtitle("PPC: Mean by Assessment Period") +
  xlab("Mean F0 (Hz)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(
  file.path(out_dir, "ppc_grouped_mean.png"),
  p_ppc_grouped,
  width = 10,
  height = 6,
  dpi = 300
)

cat("PPC plots saved.\n")

# ==============================================================================
# 8) AUTOCORRELATION PLOTS (for key parameters)
# ==============================================================================

cat("\n--- Generating Autocorrelation Plots ---\n")

# Autocorrelation for moderation parameters
p_acf <- mcmc_acf(
  draws,
  pars = c("g1[1]", "g2[3]", "b1", "sigma_y"),
  lags = 50
) +
  ggtitle("Autocorrelation: Selected Parameters") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(
  file.path(out_dir, "autocorrelation.png"),
  p_acf,
  width = 10,
  height = 8,
  dpi = 300
)

cat("Autocorrelation plots saved.\n")

# ==============================================================================
# 9) CREATE COMBINED FIGURE FOR SUPPLEMENTARY
# ==============================================================================

cat("\n--- Creating Combined Diagnostic Figure ---\n")

library(patchwork)

# Combine key diagnostic plots
p_combined <- (p_ppc_dens | p_rhat) /
  (p_trace_main) +
  plot_annotation(
    title = "MCMC Diagnostics: F0 Moderation Model",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
  )

ggsave(
  file.path(out_dir, "diagnostics_combined.png"),
  p_combined,
  width = 14,
  height = 12,
  dpi = 300
)

cat("Combined figure saved.\n")

# ==============================================================================
# 10) FINAL SUMMARY TABLE FOR SUPPLEMENTARY
# ==============================================================================

cat("\n--- Creating Summary Table for Supplementary ---\n")

# Select most important parameters for a clean summary table
supp_table <- summary_key %>%
  filter(
    Parameter %in%
      c(
        "Intercept (α)",
        "Stress effect (β₁)",
        "Recovery effect (β₂)",
        "γ₁ Negative Affectivity",
        "γ₁ Detachment",
        "γ₁ Antagonism",
        "γ₁ Disinhibition",
        "γ₁ Psychoticism",
        "γ₂ Negative Affectivity",
        "γ₂ Detachment",
        "γ₂ Antagonism",
        "γ₂ Disinhibition",
        "γ₂ Psychoticism",
        "Residual SD (σ_y)",
        "Random intercept SD (τ₁)",
        "Random stress slope SD (τ₂)",
        "Random recovery slope SD (τ₃)"
      )
  ) %>%
  mutate(
    across(c(Mean, SD, `2.5%`, `97.5%`), ~ round(.x, 2)),
    Rhat = round(Rhat, 3),
    ESS_bulk = round(ESS_bulk),
    ESS_tail = round(ESS_tail)
  )

write_csv(supp_table, file.path(out_dir, "supplementary_table_diagnostics.csv"))

cat("\n=== DIAGNOSTICS COMPLETE ===\n")
cat("Output saved to:", out_dir, "\n")
cat("\nFiles created:\n")
list.files(out_dir) %>% paste(" -", .) %>% cat(sep = "\n")

# ==============================================================================
# 11) NNE MODEL DIAGNOSTICS (SUMMARY ONLY)
# ==============================================================================

cat("\n\n=== NNE MODEL DIAGNOSTICS (SUMMARY) ===\n")

fit_nne_rds <- here("stan", "NNE", "nne_mean_pid5_moderation.rds")

if (file.exists(fit_nne_rds)) {
  fit_nne <- readRDS(fit_nne_rds)

  # Sampler diagnostics
  sampler_diag_nne <- fit_nne$diagnostic_summary()

  cat("\n--- NNE Sampling Diagnostics ---\n")
  cat("Divergent transitions:", sum(sampler_diag_nne$num_divergent), "\n")
  cat("Max treedepth exceeded:", sum(sampler_diag_nne$num_max_treedepth), "\n")

  # Key parameters summary
  key_params_nne <- c(
    "alpha",
    "b1",
    "b2",
    paste0("g1[", 1:5, "]"),
    paste0("g2[", 1:5, "]"),
    "sigma_y",
    paste0("tau[", 1:3, "]")
  )

  summary_nne <- fit_nne$summary(variables = key_params_nne)

  # Calculate summary statistics
  rhat_max_nne <- max(summary_nne$rhat, na.rm = TRUE)
  ess_bulk_min_nne <- min(summary_nne$ess_bulk, na.rm = TRUE)
  ess_tail_min_nne <- min(summary_nne$ess_tail, na.rm = TRUE)

  # Check all parameters
  all_params_nne <- fit_nne$summary()
  n_rhat_above_101_nne <- sum(all_params_nne$rhat > 1.01, na.rm = TRUE)

  cat("\n--- NNE Convergence Summary ---\n")
  cat("Max Rhat (key params):", round(rhat_max_nne, 4), "\n")
  cat("Parameters with Rhat > 1.01:", n_rhat_above_101_nne, "\n")
  cat("Min ESS bulk (key params):", round(ess_bulk_min_nne), "\n")
  cat("Min ESS tail (key params):", round(ess_tail_min_nne), "\n")

  # Save NNE summary
  nne_summary <- data.frame(
    Model = "NNE",
    Divergences = sum(sampler_diag_nne$num_divergent),
    Max_treedepth_exceeded = sum(sampler_diag_nne$num_max_treedepth),
    Rhat_max = round(rhat_max_nne, 4),
    Rhat_above_1.01 = n_rhat_above_101_nne,
    ESS_bulk_min = round(ess_bulk_min_nne),
    ESS_tail_min = round(ess_tail_min_nne)
  )

  # Combine with F0 summary
  f0_summary <- data.frame(
    Model = "F0",
    Divergences = sum(sampler_diag$num_divergent),
    Max_treedepth_exceeded = sum(sampler_diag$num_max_treedepth),
    Rhat_max = round(rhat_max, 4),
    Rhat_above_1.01 = n_rhat_above_101,
    ESS_bulk_min = round(ess_bulk_min),
    ESS_tail_min = round(ess_tail_min)
  )

  combined_summary <- rbind(f0_summary, nne_summary)

  write_csv(
    combined_summary,
    file.path(out_dir, "convergence_summary_both_models.csv")
  )

  cat("\nCombined summary saved to: convergence_summary_both_models.csv\n")
  print(combined_summary)

  # Generate NNE PPC plot
  bundle_nne <- readRDS(here("results", "NNE", "stan_bundle_nne_pid5.rds"))
  y_nne <- bundle_nne$stan_data$y
  y_rep_nne <- fit_nne$draws("y_rep", format = "matrix")

  p_ppc_nne <- ppc_dens_overlay(y_nne, y_rep_nne[1:100, ]) +
    ggtitle("Posterior Predictive Check: NNE Model") +
    xlab("NNE (dB)") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  ggsave(
    file.path(out_dir, "ppc_density_overlay_nne.png"),
    p_ppc_nne,
    width = 8,
    height = 6,
    dpi = 300
  )

  cat("NNE PPC plot saved.\n")
} else {
  cat("NNE fit not found at:", fit_nne_rds, "\n")
  cat("Skipping NNE diagnostics.\n")
}

cat("\n=== ALL DIAGNOSTICS COMPLETE ===\n")
