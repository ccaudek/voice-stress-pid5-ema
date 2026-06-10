# ==============================================================================
# 06_mcmc_diagnostics_nne.R
# MCMC Convergence Diagnostics for the NNE Moderation Model
#
# Mirrors the structure of the F0 diagnostics script, adapted to NNE (units: dB).
# Produces:
# - Sampler diagnostics (divergences, treedepth, E-BFMI)
# - Convergence diagnostics (Rhat, ESS) for key parameters, with 89% CrIs
# - Trace plots, Rhat/ESS plots, autocorrelation plots
# - Posterior predictive check figures
# - A supplementary diagnostics table (CSV)
# - An optional combined F0 + NNE convergence summary (for Table S12)
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(cmdstanr)
  library(posterior)
  library(bayesplot)
  library(here)
})

color_scheme_set("brightblue")

# ==============================================================================
# 0) CREDIBLE INTERVAL SETTINGS
# ==============================================================================

# Central 89% credible intervals throughout, consistent with the rest of the
# pipeline. NOTE: the Rhat/ESS reference values below (Rhat < 1.01, ESS > 400,
# ESS/draws > 0.1) are sampler-quality diagnostics, not inferential thresholds.
cri_level <- 0.89
cri_probs <- c((1 - cri_level) / 2, 1 - (1 - cri_level) / 2)
cri_label <- paste0(round(cri_level * 100), "%")

# ==============================================================================
# 1) LOAD FITTED MODEL
# ==============================================================================

fit_rds <- here("stan", "NNE", "nne_mean_pid5_moderation.rds")
bundle_rds <- here("results", "NNE", "stan_bundle_nne_pid5.rds")

if (!file.exists(fit_rds)) {
  stop(
    "Fitted model not found: ",
    fit_rds,
    "\nRun 02_run_nne_pid5.R first."
  )
}

fit <- readRDS(fit_rds)
bundle <- readRDS(bundle_rds)
stan_data <- bundle$stan_data
pid5_vars <- bundle$pid5_vars

out_dir <- here("results", "NNE", "diagnostics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== MCMC DIAGNOSTICS FOR NNE MODERATION MODEL ===\n")
cat("Posterior summaries use ", cri_label, " credible intervals.\n\n", sep = "")

# Full parameter table (used for robustness checks and all-parameter summaries)
all_params <- fit$summary()
present <- all_params$variable

draws <- fit$draws()
n_chains <- posterior::nchains(draws)
n_draws_total <- posterior::ndraws(draws)
iter_sampling <- n_draws_total / n_chains

# ==============================================================================
# 2) SAMPLING DIAGNOSTICS
# ==============================================================================

cat("--- Sampling Information ---\n")

sampler_diag <- fit$diagnostic_summary()

cat("Chains:", n_chains, "| Post-warmup draws total:", n_draws_total, "\n")
cat("Divergent transitions:", sum(sampler_diag$num_divergent), "\n")
cat("Max treedepth exceeded:", sum(sampler_diag$num_max_treedepth), "\n")
cat("Low E-BFMI warnings:", sum(sampler_diag$ebfmi < 0.2), "chains\n")

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

# Candidate key parameters. The trait main-effect block may be named a_trait[d]
# or a[d] depending on the Stan file; include whichever is present (if any).
key_params <- c("alpha", "b1", "b2", paste0("g1[", 1:5, "]"), paste0("g2[", 1:5, "]"))
if (all(paste0("a_trait[", 1:5, "]") %in% present)) {
  key_params <- c(key_params, paste0("a_trait[", 1:5, "]"))
} else if (all(paste0("a[", 1:5, "]") %in% present)) {
  key_params <- c(key_params, paste0("a[", 1:5, "]"))
}
key_params <- c(key_params, "sigma_y", paste0("tau[", 1:3, "]"), paste0("sigma_ema[", 1:5, "]"))

# Keep only parameters actually present in the fit (robustness)
key_params <- key_params[key_params %in% present]

summary_key <- fit$summary(
  variables = key_params,
  mean,
  median,
  sd,
  mad,
  ~ posterior::quantile2(.x, probs = cri_probs),
  rhat,
  ess_bulk,
  ess_tail
)

# Readable parameter labels (unmatched variables fall back to their raw name)
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
  "a[1]" = "a Negative Affectivity",
  "a[2]" = "a Detachment",
  "a[3]" = "a Antagonism",
  "a[4]" = "a Disinhibition",
  "a[5]" = "a Psychoticism",
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
    Median = median,
    SD = sd,
    MAD = mad,
    CrI89_Lower = `q5.5`,
    CrI89_Upper = `q94.5`,
    Rhat = rhat,
    ESS_bulk = ess_bulk,
    ESS_tail = ess_tail
  )

print(summary_key, n = 30)

rhat_issues <- sum(summary_key$Rhat > 1.01, na.rm = TRUE)
ess_issues <- sum(
  summary_key$ESS_bulk < 400 | summary_key$ESS_tail < 400,
  na.rm = TRUE
)

cat("\nParameters with Rhat > 1.01:", rhat_issues, "\n")
cat("Parameters with ESS < 400:", ess_issues, "\n")

write_csv(
  summary_key,
  file.path(out_dir, "convergence_diagnostics_key_params_89cri.csv")
)

# ==============================================================================
# 4) SUMMARY STATISTICS FOR SUPPLEMENTARY TEXT
# ==============================================================================

rhat_range <- range(summary_key$Rhat, na.rm = TRUE)
rhat_max <- max(summary_key$Rhat, na.rm = TRUE)
ess_bulk_range <- range(summary_key$ESS_bulk, na.rm = TRUE)
ess_bulk_min <- min(summary_key$ESS_bulk, na.rm = TRUE)
ess_tail_min <- min(summary_key$ESS_tail, na.rm = TRUE)

n_total_params <- nrow(all_params)
n_rhat_above_101 <- sum(all_params$rhat > 1.01, na.rm = TRUE)
n_rhat_above_105 <- sum(all_params$rhat > 1.05, na.rm = TRUE)

cat("\n--- Summary for Supplementary Text ---\n")
cat("Total parameters:", n_total_params, "\n")
cat("Parameters with Rhat > 1.01:", n_rhat_above_101, "\n")
cat("Parameters with Rhat > 1.05:", n_rhat_above_105, "\n")
cat("Rhat range (key params):", round(rhat_range[1], 3), "-", round(rhat_range[2], 3), "\n")
cat("Max Rhat (key params):", round(rhat_max, 3), "\n")
cat("ESS bulk range (key params):", round(ess_bulk_range[1]), "-", round(ess_bulk_range[2]), "\n")
cat("Min ESS bulk (key params):", round(ess_bulk_min), "\n")
cat("Min ESS tail (key params):", round(ess_tail_min), "\n")

# adapt_delta / max_treedepth / warmup mirror the 02 run settings; iterations
# and chains are read back from the fit for accuracy.
text_summary <- data.frame(
  Metric = c(
    "Total parameters",
    "Chains",
    "Iterations (sampling, per chain)",
    "Total post-warmup draws",
    "adapt_delta",
    "max_treedepth",
    "Divergent transitions",
    "Max treedepth exceeded",
    "Rhat max (key params)",
    "Rhat > 1.01 (all params)",
    "ESS bulk min (key params)",
    "ESS tail min (key params)",
    "Credible interval level"
  ),
  Value = c(
    n_total_params,
    n_chains,
    iter_sampling,
    n_draws_total,
    0.99,
    15,
    sum(sampler_diag$num_divergent),
    sum(sampler_diag$num_max_treedepth),
    round(rhat_max, 3),
    n_rhat_above_101,
    round(ess_bulk_min),
    round(ess_tail_min),
    cri_label
  )
)

write_csv(text_summary, file.path(out_dir, "diagnostics_text_summary.csv"))

# ==============================================================================
# 5) TRACE PLOTS
# ==============================================================================

cat("\n--- Generating Trace Plots ---\n")

p_trace_main <- mcmc_trace(
  draws,
  pars = c("alpha", "b1", "b2", "sigma_y"),
  facet_args = list(ncol = 2)
) +
  ggtitle("Trace Plots: Main Effects (NNE)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(out_dir, "trace_main_effects.png"), p_trace_main, width = 10, height = 8, dpi = 300)

p_trace_g1 <- mcmc_trace(
  draws,
  pars = paste0("g1[", 1:5, "]"),
  facet_args = list(ncol = 2)
) +
  ggtitle("Trace Plots: Stress Moderation (γ₁, NNE)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(out_dir, "trace_g1_moderation.png"), p_trace_g1, width = 10, height = 10, dpi = 300)

p_trace_g2 <- mcmc_trace(
  draws,
  pars = paste0("g2[", 1:5, "]"),
  facet_args = list(ncol = 2)
) +
  ggtitle("Trace Plots: Recovery Moderation (γ₂, NNE)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(out_dir, "trace_g2_moderation.png"), p_trace_g2, width = 10, height = 10, dpi = 300)

p_trace_var <- mcmc_trace(
  draws,
  pars = c("tau[1]", "tau[2]", "tau[3]", "sigma_y"),
  facet_args = list(ncol = 2)
) +
  ggtitle("Trace Plots: Variance Components (NNE)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(out_dir, "trace_variance_components.png"), p_trace_var, width = 10, height = 8, dpi = 300)

cat("Trace plots saved.\n")

# ==============================================================================
# 6) RHAT AND ESS PLOTS
# ==============================================================================

cat("\n--- Generating Rhat/ESS Plots ---\n")

p_rhat <- mcmc_rhat_hist(all_params$rhat) +
  ggtitle("Distribution of R-hat Values (All Parameters, NNE)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(out_dir, "rhat_histogram.png"), p_rhat, width = 8, height = 6, dpi = 300)

ess_ratios <- summary_key %>%
  select(Parameter, ESS_bulk) %>%
  mutate(ratio = ESS_bulk / n_draws_total) %>%
  arrange(ratio)

p_ess <- ggplot(ess_ratios, aes(x = ratio, y = reorder(Parameter, ratio))) +
  geom_point(size = 3, color = "steelblue") +
  geom_vline(xintercept = 0.1, linetype = "dashed", color = "red", alpha = 0.7) +
  scale_x_continuous(limits = c(0, max(ess_ratios$ratio) * 1.1)) +
  labs(
    x = "ESS / Total Draws",
    y = NULL,
    title = "Effective Sample Size Ratios (Key Parameters, NNE)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 8)
  )

ggsave(file.path(out_dir, "ess_ratios.png"), p_ess, width = 10, height = 8, dpi = 300)

cat("Rhat/ESS plots saved.\n")

# ==============================================================================
# 7) POSTERIOR PREDICTIVE CHECKS
# ==============================================================================

cat("\n--- Generating Posterior Predictive Checks ---\n")

y <- stan_data$y
y_rep <- fit$draws("y_rep", format = "matrix")

p_ppc_dens <- ppc_dens_overlay(y, y_rep[1:100, ]) +
  ggtitle("Posterior Predictive Check: Density Overlay (NNE)") +
  xlab("NNE (dB)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(out_dir, "ppc_density_overlay.png"), p_ppc_dens, width = 8, height = 6, dpi = 300)

p_ppc_mean <- ppc_stat(y, y_rep, stat = "mean") +
  ggtitle("PPC: Distribution of Means (NNE)") +
  xlab("Mean NNE (dB)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(out_dir, "ppc_stat_mean.png"), p_ppc_mean, width = 8, height = 6, dpi = 300)

p_ppc_sd <- ppc_stat(y, y_rep, stat = "sd") +
  ggtitle("PPC: Distribution of Standard Deviations (NNE)") +
  xlab("SD of NNE (dB)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(out_dir, "ppc_stat_sd.png"), p_ppc_sd, width = 8, height = 6, dpi = 300)

# Grouped PPC by assessment period.
# Derive the period from df_voice (robust to data ordering); skip if unavailable.
grouped_ok <- FALSE
if (!is.null(bundle$df_voice) && nrow(bundle$df_voice) == length(y)) {
  tp_raw <- as.character(bundle$df_voice$timepoint)
  tp <- factor(
    recode(
      tp_raw,
      baseline = "Baseline",
      pre = "Pre-exam",
      post = "Post-exam"
    ),
    levels = c("Baseline", "Pre-exam", "Post-exam")
  )
  grouped_ok <- !any(is.na(tp))
}

if (grouped_ok) {
  p_ppc_grouped <- tryCatch(
    {
      ppc_stat_grouped(y, y_rep, group = tp, stat = "mean") +
        ggtitle("PPC: Mean by Assessment Period (NNE)") +
        xlab("Mean NNE (dB)") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    },
    error = function(e) NULL
  )
  if (!is.null(p_ppc_grouped)) {
    ggsave(
      file.path(out_dir, "ppc_grouped_mean.png"),
      p_ppc_grouped,
      width = 10,
      height = 6,
      dpi = 300
    )
  }
} else {
  cat("Grouped PPC skipped (timepoint not recoverable from bundle$df_voice).\n")
}

cat("PPC plots saved.\n")

# ==============================================================================
# 8) AUTOCORRELATION PLOTS
# ==============================================================================

cat("\n--- Generating Autocorrelation Plots ---\n")

# Representative parameters: a stress moderation, the recovery moderation of
# primary interest for NNE (Psychoticism = g2[5]), a main effect, and sigma_y.
p_acf <- mcmc_acf(
  draws,
  pars = c("g1[1]", "g2[5]", "b1", "sigma_y"),
  lags = 50
) +
  ggtitle("Autocorrelation: Selected Parameters (NNE)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(out_dir, "autocorrelation.png"), p_acf, width = 10, height = 8, dpi = 300)

cat("Autocorrelation plots saved.\n")

# ==============================================================================
# 9) COMBINED DIAGNOSTIC FIGURE
# ==============================================================================

cat("\n--- Creating Combined Diagnostic Figure ---\n")

suppressPackageStartupMessages(library(patchwork))

p_combined <- (p_ppc_dens | p_rhat) /
  (p_trace_main) +
  plot_annotation(
    title = "MCMC Diagnostics: NNE Moderation Model",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
  )

ggsave(file.path(out_dir, "diagnostics_combined.png"), p_combined, width = 14, height = 12, dpi = 300)

cat("Combined figure saved.\n")

# ==============================================================================
# 10) FINAL SUMMARY TABLE FOR SUPPLEMENTARY
# ==============================================================================

cat("\n--- Creating Summary Table for Supplementary ---\n")

supp_keep <- c(
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

supp_table <- summary_key %>%
  filter(Parameter %in% supp_keep) %>%
  mutate(
    across(c(Mean, Median, SD, MAD, CrI89_Lower, CrI89_Upper), ~ round(.x, 2)),
    Rhat = round(Rhat, 3),
    ESS_bulk = round(ESS_bulk),
    ESS_tail = round(ESS_tail)
  )

write_csv(supp_table, file.path(out_dir, "supplementary_table_diagnostics_89cri.csv"))

cat("\n=== NNE DIAGNOSTICS COMPLETE ===\n")
cat("Output saved to:", out_dir, "\n")
cat("\nFiles created:\n")
list.files(out_dir) %>% paste(" -", .) %>% cat(sep = "\n")

# ==============================================================================
# 11) OPTIONAL: COMBINED F0 + NNE CONVERGENCE SUMMARY (for Table S12)
# ==============================================================================

cat("\n\n=== COMBINED F0 + NNE CONVERGENCE SUMMARY ===\n")

nne_row <- data.frame(
  Model = "NNE",
  Divergences = sum(sampler_diag$num_divergent),
  Max_treedepth_exceeded = sum(sampler_diag$num_max_treedepth),
  Rhat_max = round(rhat_max, 4),
  Rhat_above_1.01 = n_rhat_above_101,
  ESS_bulk_min = round(ess_bulk_min),
  ESS_tail_min = round(ess_tail_min)
)

fit_f0_rds <- here("stan", "F0", "f0mean_pid5_moderation.RDS")

if (file.exists(fit_f0_rds)) {
  fit_f0 <- readRDS(fit_f0_rds)
  sd_f0 <- fit_f0$diagnostic_summary()
  all_f0 <- fit_f0$summary()
  key_f0 <- fit_f0$summary(
    variables = c("alpha", "b1", "b2", paste0("g1[", 1:5, "]"), paste0("g2[", 1:5, "]"), "sigma_y", paste0("tau[", 1:3, "]"))
  )

  f0_row <- data.frame(
    Model = "F0",
    Divergences = sum(sd_f0$num_divergent),
    Max_treedepth_exceeded = sum(sd_f0$num_max_treedepth),
    Rhat_max = round(max(key_f0$rhat, na.rm = TRUE), 4),
    Rhat_above_1.01 = sum(all_f0$rhat > 1.01, na.rm = TRUE),
    ESS_bulk_min = round(min(key_f0$ess_bulk, na.rm = TRUE)),
    ESS_tail_min = round(min(key_f0$ess_tail, na.rm = TRUE))
  )

  combined_summary <- rbind(f0_row, nne_row)
} else {
  cat("F0 fit not found at:", fit_f0_rds, "- writing NNE row only.\n")
  combined_summary <- nne_row
}

write_csv(combined_summary, file.path(out_dir, "convergence_summary_both_models.csv"))
print(combined_summary)

cat("\n=== ALL DIAGNOSTICS COMPLETE ===\n")

# eof ---
