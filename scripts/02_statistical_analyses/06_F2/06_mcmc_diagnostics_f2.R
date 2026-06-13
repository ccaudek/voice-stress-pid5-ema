# ==============================================================================
# 06_mcmc_diagnostics_f2.R
# MCMC convergence diagnostics for F2/articulatory moderation models.
#
# Compatible with:
# - 01_prepare_stan_data_f2_pid5.R
# - 02_f2_pid5_moderation.R
#
# Produces, for each fitted outcome:
# - sampler diagnostics
# - Rhat/ESS summaries for key parameters
# - trace plots for main effects and moderation effects
# - posterior predictive checks on the original outcome scale
# - combined diagnostics summary across outcomes
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(cmdstanr)
  library(posterior)
  library(bayesplot)
  library(here)
  library(patchwork)
})

# ==============================================================================
# SETTINGS
# ==============================================================================

color_scheme_set("brightblue")

cri_level <- 0.89
cri_probs <- c((1 - cri_level) / 2, 1 - (1 - cri_level) / 2)
cri_label <- paste0(round(cri_level * 100), "%")

# Deve corrispondere agli outcome fittati in 02_f2_pid5_moderation.R.
outcomes_to_diagnose <- c(
  "f2_range_norm",
  "f2_mean",
  "vsa_log",
  "centralization"
)

outcome_labels <- c(
  f2_mean = "Mean F2",
  f2_range = "F2 range (/i/ - /u/)",
  f2_range_norm = "Normalized F2 range",
  vsa_log = "Log vowel space area",
  centralization = "Vowel-space centralization",
  f2_slope = "F2 slope (/i/ to /u/)"
)

outcome_units <- c(
  f2_mean = "Hz",
  f2_range = "Hz",
  f2_range_norm = "F2/F1 ratio",
  vsa_log = "log Hz^2",
  centralization = "Hz",
  f2_slope = "Hz/Hz"
)

pretty_outcome <- function(outcome) {
  if (outcome %in% names(outcome_labels)) outcome_labels[[outcome]] else outcome
}

unit_outcome <- function(outcome) {
  if (outcome %in% names(outcome_units)) outcome_units[[outcome]] else "original units"
}

# ==============================================================================
# HELPERS
# ==============================================================================

read_fit_bundle <- function(outcome_name) {
  fit_rds <- here("stan", "F2", paste0(outcome_name, "_pid5_moderation.RDS"))
  bundle_rds <- here(
    "results", "F2", "data",
    paste0("stan_bundle_", outcome_name, "_pid5.rds")
  )

  if (!file.exists(fit_rds)) {
    stop(
      "Fitted model not found: ", fit_rds,
      "\nRun 02_f2_pid5_moderation.R first for outcome: ", outcome_name
    )
  }
  if (!file.exists(bundle_rds)) {
    stop(
      "Bundle not found: ", bundle_rds,
      "\nRun 01_prepare_stan_data_f2_pid5.R first."
    )
  }

  list(fit = readRDS(fit_rds), bundle = readRDS(bundle_rds))
}

make_param_labels <- function(D = 5) {
  pid5_labels <- c(
    "Negative Affectivity",
    "Detachment",
    "Antagonism",
    "Disinhibition",
    "Psychoticism"
  )

  labels <- c(
    "alpha" = "Intercept (alpha)",
    "b1" = "Stress effect (beta1)",
    "b2" = "Recovery effect (beta2)",
    "sigma_y" = "Residual SD (sigma_y)",
    "tau[1]" = "Random intercept SD (tau1)",
    "tau[2]" = "Random stress slope SD (tau2)",
    "tau[3]" = "Random recovery slope SD (tau3)"
  )

  for (d in seq_len(D)) {
    labels[paste0("g1[", d, "]")] <- paste0("gamma1 ", pid5_labels[d])
    labels[paste0("g2[", d, "]")] <- paste0("gamma2 ", pid5_labels[d])
    labels[paste0("a_trait[", d, "]")] <- paste0("a ", pid5_labels[d])
    labels[paste0("sigma_ema[", d, "]")] <- paste0("sigma_ema ", pid5_labels[d])
  }

  labels
}

summarize_one_outcome <- function(outcome_name) {
  cat("\n=== MCMC DIAGNOSTICS FOR F2 OUTCOME: ", outcome_name, " ===\n", sep = "")
  cat("Outcome label: ", pretty_outcome(outcome_name), "\n", sep = "")
  cat("Posterior summaries use ", cri_label, " credible intervals.\n\n", sep = "")

  loaded <- read_fit_bundle(outcome_name)
  fit <- loaded$fit
  bundle <- loaded$bundle
  stan_data <- bundle$stan_data
  D <- stan_data$D

  out_dir <- here("results", "F2", "diagnostics", outcome_name)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # ------------------------------------------------------------------------------
  # Sampler diagnostics
  # ------------------------------------------------------------------------------

  sampler_diag <- fit$diagnostic_summary()

  sampler_summary <- tibble(
    outcome = outcome_name,
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

  print(sampler_summary)
  write_csv(sampler_summary, file.path(out_dir, "sampler_diagnostics.csv"))

  # ------------------------------------------------------------------------------
  # Convergence diagnostics
  # ------------------------------------------------------------------------------

  key_params <- c(
    "alpha",
    "b1",
    "b2",
    paste0("g1[", seq_len(D), "]"),
    paste0("g2[", seq_len(D), "]"),
    paste0("a_trait[", seq_len(D), "]"),
    "sigma_y",
    paste0("tau[", 1:3, "]"),
    paste0("sigma_ema[", seq_len(D), "]")
  )

  key_params <- key_params[key_params %in% fit$metadata()$stan_variables]

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

  param_labels <- make_param_labels(D)

  summary_key <- summary_key %>%
    mutate(
      outcome = outcome_name,
      outcome_label = pretty_outcome(outcome_name),
      outcome_unit = unit_outcome(outcome_name),
      Parameter_Label = param_labels[variable],
      Parameter_Label = ifelse(is.na(Parameter_Label), variable, Parameter_Label)
    ) %>%
    select(
      outcome,
      outcome_label,
      outcome_unit,
      Parameter = Parameter_Label,
      variable,
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

  write_csv(summary_key, file.path(out_dir, "convergence_diagnostics_key_params_89cri.csv"))

  rhat_issues <- sum(summary_key$Rhat > 1.01, na.rm = TRUE)
  ess_issues <- sum(summary_key$ESS_bulk < 400 | summary_key$ESS_tail < 400, na.rm = TRUE)

  cat("\nParameters with Rhat > 1.01: ", rhat_issues, "\n", sep = "")
  cat("Parameters with ESS < 400: ", ess_issues, "\n", sep = "")

  all_params <- fit$summary()
  n_total_params <- nrow(all_params)
  n_rhat_above_101 <- sum(all_params$rhat > 1.01, na.rm = TRUE)
  n_rhat_above_105 <- sum(all_params$rhat > 1.05, na.rm = TRUE)

  rhat_max <- max(summary_key$Rhat, na.rm = TRUE)
  ess_bulk_min <- min(summary_key$ESS_bulk, na.rm = TRUE)
  ess_tail_min <- min(summary_key$ESS_tail, na.rm = TRUE)

  text_summary <- tibble(
    outcome = outcome_name,
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
      "Rhat > 1.05 (all params)",
      "ESS bulk min (key params)",
      "ESS tail min (key params)",
      "Credible interval level"
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
      n_rhat_above_105,
      round(ess_bulk_min),
      round(ess_tail_min),
      cri_label
    )
  )

  write_csv(text_summary, file.path(out_dir, "diagnostics_text_summary.csv"))

  # ------------------------------------------------------------------------------
  # Trace plots
  # ------------------------------------------------------------------------------

  draws <- fit$draws()

  p_trace_main <- mcmc_trace(
    draws,
    pars = c("alpha", "b1", "b2", "sigma_y"),
    facet_args = list(ncol = 2)
  ) +
    ggtitle(paste0("Trace Plots: Main Effects - ", pretty_outcome(outcome_name))) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  ggsave(file.path(out_dir, "trace_main_effects.png"), p_trace_main, width = 10, height = 8, dpi = 300)

  p_trace_g1 <- mcmc_trace(
    draws,
    pars = paste0("g1[", seq_len(D), "]"),
    facet_args = list(ncol = 2)
  ) +
    ggtitle(paste0("Trace Plots: Stress Moderation - ", pretty_outcome(outcome_name))) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  ggsave(file.path(out_dir, "trace_g1_moderation.png"), p_trace_g1, width = 10, height = 10, dpi = 300)

  p_trace_g2 <- mcmc_trace(
    draws,
    pars = paste0("g2[", seq_len(D), "]"),
    facet_args = list(ncol = 2)
  ) +
    ggtitle(paste0("Trace Plots: Recovery Moderation - ", pretty_outcome(outcome_name))) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  ggsave(file.path(out_dir, "trace_g2_moderation.png"), p_trace_g2, width = 10, height = 10, dpi = 300)

  p_trace_var <- mcmc_trace(
    draws,
    pars = c("tau[1]", "tau[2]", "tau[3]", "sigma_y"),
    facet_args = list(ncol = 2)
  ) +
    ggtitle(paste0("Trace Plots: Variance Components - ", pretty_outcome(outcome_name))) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  ggsave(file.path(out_dir, "trace_variance_components.png"), p_trace_var, width = 10, height = 8, dpi = 300)

  # ------------------------------------------------------------------------------
  # Rhat and ESS plots
  # ------------------------------------------------------------------------------

  p_rhat <- mcmc_rhat_hist(all_params$rhat) +
    ggtitle(paste0("Distribution of R-hat Values - ", pretty_outcome(outcome_name))) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  ggsave(file.path(out_dir, "rhat_histogram.png"), p_rhat, width = 8, height = 6, dpi = 300)

  total_draws <- 4 * 6000
  ess_ratios <- summary_key %>%
    select(Parameter, ESS_bulk) %>%
    mutate(ratio = ESS_bulk / total_draws) %>%
    arrange(ratio)

  p_ess <- ggplot(ess_ratios, aes(x = ratio, y = reorder(Parameter, ratio))) +
    geom_point(size = 3, color = "steelblue") +
    geom_vline(xintercept = 0.1, linetype = "dashed", color = "red", alpha = 0.7) +
    scale_x_continuous(limits = c(0, max(ess_ratios$ratio, na.rm = TRUE) * 1.1)) +
    labs(
      x = "ESS / Total Draws",
      y = NULL,
      title = paste0("Effective Sample Size Ratios - ", pretty_outcome(outcome_name))
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.text.y = element_text(size = 8))

  ggsave(file.path(out_dir, "ess_ratios.png"), p_ess, width = 10, height = 8, dpi = 300)

  # ------------------------------------------------------------------------------
  # Posterior predictive checks on original outcome scale
  # ------------------------------------------------------------------------------

  y_std <- stan_data$y
  y_rep_std <- fit$draws("y_rep", format = "matrix")
  y_raw <- y_std * bundle$y_scale + bundle$y_center
  y_rep_raw <- y_rep_std * bundle$y_scale + bundle$y_center
  ppc_rows <- seq_len(min(100, nrow(y_rep_raw)))

  p_ppc_dens <- ppc_dens_overlay(y_raw, y_rep_raw[ppc_rows, , drop = FALSE]) +
    ggtitle(paste0("Posterior Predictive Check: Density - ", pretty_outcome(outcome_name))) +
    xlab(paste0(pretty_outcome(outcome_name), " (", unit_outcome(outcome_name), ")")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  ggsave(file.path(out_dir, "ppc_density_overlay.png"), p_ppc_dens, width = 8, height = 6, dpi = 300)

  p_ppc_mean <- ppc_stat(y_raw, y_rep_raw, stat = "mean") +
    ggtitle(paste0("PPC: Distribution of Means - ", pretty_outcome(outcome_name))) +
    xlab(paste0("Mean ", unit_outcome(outcome_name))) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  ggsave(file.path(out_dir, "ppc_stat_mean.png"), p_ppc_mean, width = 8, height = 6, dpi = 300)

  p_ppc_sd <- ppc_stat(y_raw, y_rep_raw, stat = "sd") +
    ggtitle(paste0("PPC: Distribution of SDs - ", pretty_outcome(outcome_name))) +
    xlab(paste0("SD ", unit_outcome(outcome_name))) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  ggsave(file.path(out_dir, "ppc_stat_sd.png"), p_ppc_sd, width = 8, height = 6, dpi = 300)

  timepoint <- rep(c("Baseline", "Pre-exam", "Post-exam"), times = stan_data$N_subj)
  timepoint <- factor(timepoint, levels = c("Baseline", "Pre-exam", "Post-exam"))

  p_ppc_grouped <- ppc_stat_grouped(y_raw, y_rep_raw, group = timepoint, stat = "mean") +
    ggtitle(paste0("PPC: Mean by Assessment Period - ", pretty_outcome(outcome_name))) +
    xlab(paste0("Mean ", unit_outcome(outcome_name))) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  ggsave(file.path(out_dir, "ppc_grouped_mean.png"), p_ppc_grouped, width = 10, height = 6, dpi = 300)

  # ------------------------------------------------------------------------------
  # Autocorrelation and combined figure
  # ------------------------------------------------------------------------------

  p_acf <- mcmc_acf(
    draws,
    pars = c("g1[1]", "g2[3]", "b1", "sigma_y"),
    lags = 50
  ) +
    ggtitle(paste0("Autocorrelation: Selected Parameters - ", pretty_outcome(outcome_name))) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  ggsave(file.path(out_dir, "autocorrelation.png"), p_acf, width = 10, height = 8, dpi = 300)

  p_combined <- (p_ppc_dens | p_rhat) / p_trace_main +
    plot_annotation(
      title = paste0("MCMC Diagnostics: ", pretty_outcome(outcome_name)),
      theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
    )

  ggsave(file.path(out_dir, "diagnostics_combined.png"), p_combined, width = 14, height = 12, dpi = 300)

  supp_table <- summary_key %>%
    filter(
      variable %in% c(
        "alpha", "b1", "b2",
        paste0("g1[", seq_len(D), "]"),
        paste0("g2[", seq_len(D), "]"),
        "sigma_y", paste0("tau[", 1:3, "]")
      )
    ) %>%
    mutate(
      across(c(Mean, Median, SD, MAD, CrI89_Lower, CrI89_Upper), ~ round(.x, 3)),
      Rhat = round(Rhat, 3),
      ESS_bulk = round(ESS_bulk),
      ESS_tail = round(ESS_tail)
    )

  write_csv(supp_table, file.path(out_dir, "supplementary_table_diagnostics_89cri.csv"))

  cat("\nDiagnostics complete for ", outcome_name, ". Output saved to: ", out_dir, "\n", sep = "")

  tibble(
    outcome = outcome_name,
    outcome_label = pretty_outcome(outcome_name),
    divergences = sum(sampler_diag$num_divergent),
    max_treedepth_exceeded = sum(sampler_diag$num_max_treedepth),
    low_ebfmi_chains = sum(sampler_diag$ebfmi < 0.2),
    rhat_max_key_params = round(rhat_max, 4),
    rhat_above_1.01_all_params = n_rhat_above_101,
    rhat_above_1.05_all_params = n_rhat_above_105,
    ess_bulk_min_key_params = round(ess_bulk_min),
    ess_tail_min_key_params = round(ess_tail_min)
  )
}

# ==============================================================================
# RUN MULTI-OUTCOME DIAGNOSTICS
# ==============================================================================

combined_summary <- map_dfr(outcomes_to_diagnose, summarize_one_outcome)

combined_dir <- here("results", "F2", "diagnostics")
dir.create(combined_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(
  combined_summary,
  here(combined_dir, "convergence_summary_all_f2_outcomes.csv")
)

cat("\n=== ALL F2 DIAGNOSTICS COMPLETE ===\n")
cat("Combined summary saved to: results/F2/diagnostics/convergence_summary_all_f2_outcomes.csv\n")
print(combined_summary)

# eof
