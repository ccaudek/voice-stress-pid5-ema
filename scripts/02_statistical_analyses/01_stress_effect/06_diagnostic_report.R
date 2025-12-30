# ==============================================================================
# Model Diagnostics and Quality Report
# ==============================================================================
# This script generates a comprehensive diagnostic report for the fitted models
# ==============================================================================

library(tidyverse)
library(rstan)
library(bayesplot)
library(loo)
library(posterior)
library(here)

# Load fitted models
fit_f0 <- readRDS(here("models", "fit_f0_main_effects.rds"))
fit_nne <- readRDS(here("models", "fit_nne_main_effects.rds"))

# ==============================================================================
# DIAGNOSTIC REPORT FUNCTIONS
# ==============================================================================

#' Generate comprehensive diagnostic report for a Stan fit
diagnostic_report <- function(fit, model_name) {
  
  cat("\n", strrep("=", 80), "\n", sep = "")
  cat("DIAGNOSTIC REPORT:", model_name, "\n")
  cat(strrep("=", 80), "\n\n")
  
  # 1. Convergence Diagnostics
  cat("1. CONVERGENCE DIAGNOSTICS\n")
  cat(strrep("-", 80), "\n")
  
  summary_all <- summary(fit)$summary
  
  # Check R-hat
  max_rhat <- max(summary_all[, "Rhat"], na.rm = TRUE)
  problematic_rhat <- sum(summary_all[, "Rhat"] > 1.01, na.rm = TRUE)
  
  cat(sprintf("   Max R-hat: %.4f\n", max_rhat))
  cat(sprintf("   Parameters with R-hat > 1.01: %d\n", problematic_rhat))
  
  if (max_rhat < 1.01) {
    cat("   ✓ PASS: All chains converged successfully\n")
  } else {
    cat("   ✗ WARNING: Some chains may not have converged\n")
  }
  
  # Check effective sample size
  min_neff <- min(summary_all[, "n_eff"], na.rm = TRUE)
  low_neff <- sum(summary_all[, "n_eff"] < 400, na.rm = TRUE)
  
  cat(sprintf("\n   Min effective sample size: %.0f\n", min_neff))
  cat(sprintf("   Parameters with n_eff < 400: %d\n", low_neff))
  
  if (min_neff > 400) {
    cat("   ✓ PASS: Adequate effective sample sizes\n")
  } else {
    cat("   ✗ WARNING: Some parameters have low effective sample size\n")
  }
  
  # 2. HMC Diagnostics
  cat("\n2. HMC-SPECIFIC DIAGNOSTICS\n")
  cat(strrep("-", 80), "\n")
  
  sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
  
  # Divergent transitions
  divergent <- sapply(sampler_params, function(x) sum(x[, "divergent__"]))
  total_divergent <- sum(divergent)
  
  cat(sprintf("   Divergent transitions: %d\n", total_divergent))
  
  if (total_divergent == 0) {
    cat("   ✓ PASS: No divergent transitions\n")
  } else {
    cat("   ✗ WARNING: Model may be misspecified or poorly identified\n")
  }
  
  # Max treedepth
  max_treedepth <- sapply(sampler_params, function(x) sum(x[, "treedepth__"] >= 10))
  total_max_treedepth <- sum(max_treedepth)
  
  cat(sprintf("\n   Iterations hitting max treedepth: %d\n", total_max_treedepth))
  
  if (total_max_treedepth == 0) {
    cat("   ✓ PASS: No iterations hit max treedepth\n")
  } else {
    cat("   ⚠ NOTE: Consider increasing max_treedepth if this is excessive\n")
  }
  
  # 3. Parameter Summary
  cat("\n3. KEY PARAMETER ESTIMATES\n")
  cat(strrep("-", 80), "\n")
  
  key_params <- c("alpha", "b1", "b2", "sigma_y")
  key_summary <- summary_all[key_params, c("mean", "sd", "2.5%", "97.5%", "Rhat", "n_eff")]
  
  print(round(key_summary, 3))
  
  # 4. Posterior Predictive Checks
  cat("\n4. POSTERIOR PREDICTIVE CHECK SUMMARY\n")
  cat(strrep("-", 80), "\n")
  
  y_rep <- extract(fit, "y_rep")$y_rep
  y_obs <- as.vector(extract(fit, "y")$y[1,])  # Observed data from first iteration
  
  # Test statistics
  test_stats <- tibble(
    Statistic = c("Mean", "SD", "Min", "Max", "Median"),
    Observed = c(mean(y_obs, na.rm = TRUE), 
                 sd(y_obs, na.rm = TRUE),
                 min(y_obs, na.rm = TRUE),
                 max(y_obs, na.rm = TRUE),
                 median(y_obs, na.rm = TRUE)),
    Predicted_Mean = c(mean(apply(y_rep, 1, mean)),
                       mean(apply(y_rep, 1, sd)),
                       mean(apply(y_rep, 1, min)),
                       mean(apply(y_rep, 1, max)),
                       mean(apply(y_rep, 1, median))),
    Predicted_SD = c(sd(apply(y_rep, 1, mean)),
                     sd(apply(y_rep, 1, sd)),
                     sd(apply(y_rep, 1, min)),
                     sd(apply(y_rep, 1, max)),
                     sd(apply(y_rep, 1, median)))
  )
  
  print(test_stats, digits = 2)
  
  # 5. LOO Cross-Validation
  cat("\n5. LOO CROSS-VALIDATION\n")
  cat(strrep("-", 80), "\n")
  
  tryCatch({
    log_lik <- extract_log_lik(fit, merge_chains = FALSE)
    r_eff <- relative_eff(log_lik)
    loo_result <- loo(log_lik, r_eff = r_eff)
    
    print(loo_result)
    
    # Check for problematic observations
    n_high_pareto <- sum(loo_result$diagnostics$pareto_k > 0.7)
    
    cat(sprintf("\n   Observations with Pareto k > 0.7: %d\n", n_high_pareto))
    
    if (n_high_pareto == 0) {
      cat("   ✓ PASS: LOO estimates are reliable\n")
    } else {
      cat("   ⚠ WARNING: Some observations have high Pareto k values\n")
      cat("   Consider using k-fold CV or reloo() for these observations\n")
    }
    
  }, error = function(e) {
    cat("   ✗ ERROR: Could not compute LOO\n")
    cat("   ", e$message, "\n")
  })
  
  cat("\n", strrep("=", 80), "\n\n")
  
  return(invisible(NULL))
}

# ==============================================================================
# GENERATE REPORTS
# ==============================================================================

# F0 Model Report
diagnostic_report(fit_f0, "F0 MEAN MODEL")

# NNE Model Report
diagnostic_report(fit_nne, "NNE MODEL")

# ==============================================================================
# COMPARATIVE SUMMARY
# ==============================================================================

cat("\n", strrep("=", 80), "\n")
cat("COMPARATIVE MODEL SUMMARY\n")
cat(strrep("=", 80), "\n\n")

# Extract key estimates
post_f0 <- as_draws_df(fit_f0)
post_nne <- as_draws_df(fit_nne)

comparison <- tibble(
  Model = c("F0 Mean", "F0 Mean", "F0 Mean", 
            "NNE", "NNE", "NNE"),
  Parameter = rep(c("Intercept (α)", "Stress (β₁)", "Recovery (β₂)"), 2),
  Median = c(
    median(post_f0$alpha),
    median(post_f0$b1),
    median(post_f0$b2),
    median(post_nne$alpha),
    median(post_nne$b1),
    median(post_nne$b2)
  ),
  MAD = c(
    mad(post_f0$alpha),
    mad(post_f0$b1),
    mad(post_f0$b2),
    mad(post_nne$alpha),
    mad(post_nne$b1),
    mad(post_nne$b2)
  ),
  CI_Lower = c(
    quantile(post_f0$alpha, 0.025),
    quantile(post_f0$b1, 0.025),
    quantile(post_f0$b2, 0.025),
    quantile(post_nne$alpha, 0.025),
    quantile(post_nne$b1, 0.025),
    quantile(post_nne$b2, 0.025)
  ),
  CI_Upper = c(
    quantile(post_f0$alpha, 0.975),
    quantile(post_f0$b1, 0.975),
    quantile(post_f0$b2, 0.975),
    quantile(post_nne$alpha, 0.975),
    quantile(post_nne$b1, 0.975),
    quantile(post_nne$b2, 0.975)
  ),
  P_Direction = c(
    max(mean(post_f0$alpha > 0), mean(post_f0$alpha < 0)),
    mean(post_f0$b1 > 0),
    max(mean(post_f0$b2 > 0), mean(post_f0$b2 < 0)),
    max(mean(post_nne$alpha > 0), mean(post_nne$alpha < 0)),
    mean(post_nne$b1 < 0),
    max(mean(post_nne$b2 > 0), mean(post_nne$b2 < 0))
  )
)

print(comparison, digits = 3)

# ==============================================================================
# EFFECT SIZE INTERPRETATION
# ==============================================================================

cat("\n\nEFFECT SIZE INTERPRETATION\n")
cat(strrep("-", 80), "\n\n")

# F0 stress effect
f0_b1_median <- median(post_f0$b1)
f0_b1_size <- case_when(
  abs(f0_b1_median) < 3 ~ "Small",
  abs(f0_b1_median) < 5 ~ "Small-Medium",
  abs(f0_b1_median) < 10 ~ "Medium",
  TRUE ~ "Large"
)

cat("F0 Stress Effect (β₁):\n")
cat(sprintf("   Magnitude: %.2f Hz\n", f0_b1_median))
cat(sprintf("   Classification: %s\n", f0_b1_size))
cat(sprintf("   Evidence strength: P(β₁ > 0) = %.3f\n", mean(post_f0$b1 > 0)))

if (mean(post_f0$b1 > 0) > 0.95) {
  cat("   Interpretation: Strong evidence for stress-induced F0 elevation\n")
} else if (mean(post_f0$b1 > 0) > 0.80) {
  cat("   Interpretation: Moderate evidence for stress-induced F0 elevation\n")
} else {
  cat("   Interpretation: Weak or ambiguous evidence\n")
}

# NNE stress effect
nne_b1_median <- median(post_nne$b1)
nne_b1_size <- case_when(
  abs(nne_b1_median) < 0.5 ~ "Small",
  abs(nne_b1_median) < 1.5 ~ "Small-Medium",
  abs(nne_b1_median) < 3 ~ "Medium",
  TRUE ~ "Large"
)

cat("\nNNE Stress Effect (β₁):\n")
cat(sprintf("   Magnitude: %.2f dB\n", nne_b1_median))
cat(sprintf("   Classification: %s\n", nne_b1_size))
cat(sprintf("   Evidence strength: P(β₁ < 0) = %.3f\n", mean(post_nne$b1 < 0)))

if (mean(post_nne$b1 < 0) > 0.95) {
  cat("   Interpretation: Strong evidence for stress-induced noise reduction\n")
} else if (mean(post_nne$b1 < 0) > 0.80) {
  cat("   Interpretation: Moderate evidence for stress-induced noise reduction\n")
} else {
  cat("   Interpretation: Weak or ambiguous evidence\n")
}

# ==============================================================================
# RECOMMENDATIONS
# ==============================================================================

cat("\n\nRECOMMENDATIONS\n")
cat(strrep("-", 80), "\n\n")

# Check for issues and provide recommendations
issues <- c()
recommendations <- c()

# R-hat issues
if (max(summary(fit_f0)$summary[, "Rhat"], na.rm = TRUE) > 1.01 ||
    max(summary(fit_nne)$summary[, "Rhat"], na.rm = TRUE) > 1.01) {
  issues <- c(issues, "High R-hat values detected")
  recommendations <- c(recommendations, 
                      "- Increase adapt_delta to 0.99",
                      "- Run more iterations (e.g., 6000 with 3000 warmup)",
                      "- Check for data issues (outliers, coding errors)")
}

# Divergent transitions
if (sum(sapply(get_sampler_params(fit_f0, inc_warmup = FALSE), 
               function(x) sum(x[, "divergent__"]))) > 0 ||
    sum(sapply(get_sampler_params(fit_nne, inc_warmup = FALSE), 
               function(x) sum(x[, "divergent__"]))) > 0) {
  issues <- c(issues, "Divergent transitions detected")
  recommendations <- c(recommendations,
                      "- Increase adapt_delta",
                      "- Reparameterize model (e.g., use non-centered parameterization)",
                      "- Add stronger priors on problematic parameters")
}

if (length(issues) == 0) {
  cat("✓ All diagnostic checks passed!\n")
  cat("✓ Models are ready for inference and reporting\n")
} else {
  cat("⚠ Issues detected:\n")
  for (issue in issues) {
    cat("   -", issue, "\n")
  }
  cat("\nRecommendations:\n")
  for (rec in recommendations) {
    cat(rec, "\n")
  }
}

cat("\n", strrep("=", 80), "\n")

# ==============================================================================
# SAVE DIAGNOSTIC REPORT
# ==============================================================================

# Redirect output to file
sink(here("results", "diagnostic_report.txt"))

cat("COMPREHENSIVE DIAGNOSTIC REPORT\n")
cat("Generated:", Sys.time(), "\n")
cat(strrep("=", 80), "\n\n")

diagnostic_report(fit_f0, "F0 MEAN MODEL")
diagnostic_report(fit_nne, "NNE MODEL")

# Comparison table
cat("\nCOMPARATIVE SUMMARY\n")
cat(strrep("=", 80), "\n")
print(comparison, digits = 3)

sink()

cat("\nDiagnostic report saved to: results/diagnostic_report.txt\n")

# ==============================================================================
# GENERATE PUBLICATION-READY DIAGNOSTIC FIGURES
# ==============================================================================

# Trace plots for paper supplement
p_trace_f0 <- mcmc_trace(fit_f0, pars = c("alpha", "b1", "b2", "sigma_y"), 
                         facet_args = list(ncol = 2)) +
  labs(title = "F0 Model: MCMC Trace Plots") +
  theme_minimal()

ggsave(here("figures", "diagnostic_trace_f0.png"), p_trace_f0, 
       width = 10, height = 8, dpi = 300)

p_trace_nne <- mcmc_trace(fit_nne, pars = c("alpha", "b1", "b2", "sigma_y"),
                          facet_args = list(ncol = 2)) +
  labs(title = "NNE Model: MCMC Trace Plots") +
  theme_minimal()

ggsave(here("figures", "diagnostic_trace_nne.png"), p_trace_nne, 
       width = 10, height = 8, dpi = 300)

# Autocorrelation plots
p_acf_f0 <- mcmc_acf(fit_f0, pars = c("alpha", "b1", "b2", "sigma_y")) +
  labs(title = "F0 Model: Autocorrelation") +
  theme_minimal()

ggsave(here("figures", "diagnostic_acf_f0.png"), p_acf_f0, 
       width = 10, height = 8, dpi = 300)

p_acf_nne <- mcmc_acf(fit_nne, pars = c("alpha", "b1", "b2", "sigma_y")) +
  labs(title = "NNE Model: Autocorrelation") +
  theme_minimal()

ggsave(here("figures", "diagnostic_acf_nne.png"), p_acf_nne, 
       width = 10, height = 8, dpi = 300)

cat("\nDiagnostic figures saved to figures/\n")
cat("\n=== Quality control complete ===\n")
