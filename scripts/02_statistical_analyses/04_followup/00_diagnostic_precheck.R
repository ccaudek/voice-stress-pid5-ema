# ==============================================================================
# PRE-RUN DIAGNOSTIC CHECK
# Quick test of improved models before full run
# ==============================================================================
# Purpose: Test models with minimal iterations to catch problems early
# Run this BEFORE the full 01_loo_comparison script
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(cmdstanr)
  library(posterior)
  library(here)
})

cat("=== PRE-RUN DIAGNOSTIC CHECK ===\n")
cat("Testing improved Stan models with minimal iterations...\n\n")

# ==============================================================================
# Load Data (assume you've already run data prep from 01 script)
# ==============================================================================

# If you have stan_data already prepared, load it:
# load("results/followup/stan_data.RData")

# Otherwise, source the data prep section from your main script
# For now, assuming stan_data_* objects exist in environment

cat("Checking if stan_data objects exist...\n")
required_objects <- c(
  "stan_data_ema",
  "stan_data_baseline",
  "stan_data_combined"
)

missing <- required_objects[!sapply(required_objects, exists)]
if (length(missing) > 0) {
  stop(
    "Missing required data objects: ",
    paste(missing, collapse = ", "),
    "\nPlease run data preparation section from 01_loo_comparison script first"
  )
}

cat("✓ All required data objects found\n\n")

# ==============================================================================
# Compile Models
# ==============================================================================

cat("Compiling improved models...\n")

stan_file_ema <- here(
  "stan",
  "followup",
  "f0mean_pid5_moderation_improved.stan"
)
stan_file_baseline <- here(
  "stan",
  "followup",
  "pid5_baseline_moderation_improved.stan"
)
stan_file_combined <- here(
  "stan",
  "followup",
  "pid5_ema_plus_baseline_moderation_improved.stan"
)

# Check files exist
for (f in c(stan_file_ema, stan_file_baseline, stan_file_combined)) {
  if (!file.exists(f)) {
    stop("Stan file not found: ", f)
  }
}

mod_ema <- cmdstan_model(stan_file_ema)
mod_baseline <- cmdstan_model(stan_file_baseline)
mod_combined <- cmdstan_model(stan_file_combined)

cat("✓ All models compiled successfully\n\n")

# ==============================================================================
# Quick Test Fit (Minimal Iterations)
# ==============================================================================

cat("=== QUICK TEST FITS ===\n")
cat("Running minimal iterations to check for obvious problems...\n\n")

# Test settings - VERY minimal
test_settings <- list(
  chains = 2,
  parallel_chains = 2,
  iter_warmup = 100,
  iter_sampling = 100,
  adapt_delta = 0.95,
  max_treedepth = 12,
  refresh = 0,
  show_messages = FALSE
)

# Helper function to test a model
test_model <- function(model, stan_data, model_name) {
  cat("Testing", model_name, "model...\n")

  fit <- tryCatch(
    {
      model$sample(
        data = stan_data,
        chains = test_settings$chains,
        parallel_chains = test_settings$parallel_chains,
        iter_warmup = test_settings$iter_warmup,
        iter_sampling = test_settings$iter_sampling,
        adapt_delta = test_settings$adapt_delta,
        max_treedepth = test_settings$max_treedepth,
        refresh = test_settings$refresh,
        show_messages = test_settings$show_messages
      )
    },
    error = function(e) {
      cat("✗ ERROR in", model_name, "model:\n")
      cat("  ", conditionMessage(e), "\n")
      return(NULL)
    }
  )

  if (is.null(fit)) {
    return(list(success = FALSE, model = model_name))
  }

  # Quick diagnostics
  diag <- fit$diagnostic_summary()
  summ <- fit$summary()

  n_div <- sum(diag$num_divergent)
  n_tree <- sum(diag$num_max_treedepth)
  max_rhat <- max(summ$rhat, na.rm = TRUE)

  cat("  Divergences:", n_div, "\n")
  cat("  Max treedepth hits:", n_tree, "\n")
  cat("  Max Rhat:", round(max_rhat, 4), "\n")

  if (n_div == 0 && max_rhat < 1.05) {
    cat("  ✓ Looks good!\n\n")
    status <- "PASS"
  } else if (n_div > 5 || max_rhat > 1.1) {
    cat("  ⚠ WARNING: Problems detected\n\n")
    status <- "WARNING"
  } else {
    cat("  ℹ Minor issues (may improve with more iterations)\n\n")
    status <- "MINOR"
  }

  return(list(
    success = TRUE,
    model = model_name,
    status = status,
    n_div = n_div,
    n_tree = n_tree,
    max_rhat = max_rhat
  ))
}

# Test all three models
results <- list()
results$baseline <- test_model(mod_baseline, stan_data_baseline, "Baseline")
results$ema <- test_model(mod_ema, stan_data_ema, "EMA")
results$combined <- test_model(mod_combined, stan_data_combined, "Combined")

# ==============================================================================
# Summary Report
# ==============================================================================

cat("\n=== DIAGNOSTIC SUMMARY ===\n\n")

results_df <- bind_rows(
  if (results$baseline$success) {
    tibble(
      Model = results$baseline$model,
      Status = results$baseline$status,
      Divergences = results$baseline$n_div,
      Treedepth = results$baseline$n_tree,
      Max_Rhat = round(results$baseline$max_rhat, 4)
    )
  },
  if (results$ema$success) {
    tibble(
      Model = results$ema$model,
      Status = results$ema$status,
      Divergences = results$ema$n_div,
      Treedepth = results$ema$n_tree,
      Max_Rhat = round(results$ema$max_rhat, 4)
    )
  },
  if (results$combined$success) {
    tibble(
      Model = results$combined$model,
      Status = results$combined$status,
      Divergences = results$combined$n_div,
      Treedepth = results$combined$n_tree,
      Max_Rhat = round(results$combined$max_rhat, 4)
    )
  }
)

print(results_df)

# ==============================================================================
# Recommendations
# ==============================================================================

cat("\n=== RECOMMENDATIONS ===\n\n")

all_pass <- all(results_df$Status == "PASS")
any_warning <- any(results_df$Status == "WARNING")

if (all_pass) {
  cat("✓ All models passed basic tests!\n")
  cat("\nRecommended settings for full run:\n")
  cat("  n_warmup = 2000\n")
  cat("  n_sampling = 4000-6000\n")
  cat("  adapt_delta = 0.995\n")
  cat("  max_treedepth = 15\n\n")
  cat("You can now run the full 01_loo_comparison_f0_pid5_improved.R script\n")
} else if (any_warning) {
  cat("⚠ Some models showed warnings\n")
  cat("\nOptions:\n")
  cat("1. Proceed with full run using higher adapt_delta (0.998)\n")
  cat("2. Check model specification\n")
  cat("3. Examine data for outliers\n\n")
} else {
  cat("ℹ Minor issues detected\n")
  cat("\nThese are likely due to minimal iterations in this test.\n")
  cat("Proceed with full run using recommended settings:\n")
  cat("  adapt_delta = 0.995-0.998\n")
  cat("  n_warmup = 2000\n")
  cat("  n_sampling = 4000-6000\n\n")
}

# ==============================================================================
# Additional Checks
# ==============================================================================

cat("\n=== DATA SANITY CHECKS ===\n\n")

cat("Voice data:\n")
cat("  N observations:", stan_data_baseline$N_voice, "\n")
cat("  N subjects:", stan_data_baseline$N_subj, "\n")
cat(
  "  Obs per subject:",
  stan_data_baseline$N_voice / stan_data_baseline$N_subj,
  "\n"
)
cat("  Y range:", round(range(stan_data_baseline$y), 1), "\n\n")

cat("EMA data:\n")
cat("  N observations:", stan_data_ema$N_ema, "\n")
cat("  Obs per subject:", stan_data_ema$N_ema / stan_data_ema$N_subj, "\n\n")

# Check for extreme values in predictors
cat("Predictor checks:\n")
cat("  c1 range:", range(stan_data_baseline$c1), "\n")
cat("  c2 range:", range(stan_data_baseline$c2), "\n")
cat("  Baseline Z range:", round(range(stan_data_baseline$Z), 2), "\n")
cat("  EMA X range:", round(range(stan_data_ema$X), 2), "\n\n")

cat("✓ Pre-run diagnostic check complete!\n")
