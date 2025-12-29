# ==============================================================================
# LOO Comparison: EMA vs Baseline vs Combined PID-5 for F0 Moderation
# ==============================================================================
# Goal: Compare predictive performance of three approaches to PID-5 measurement:
#   1) EMA-only (15 items, multiple assessments, latent variable model)
#   2) Baseline-only (220 items, single assessment)
#   3) Combined (both EMA and baseline together)
#
# Critical requirement: ALL models must use THE SAME SUBJECTS for valid LOO comparison
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
  library(rio)
  library(jsonlite)
  library(cmdstanr)
  library(posterior)
  library(loo)
})

# ==============================================================================
# 1) LOAD VOICE DATA
# ==============================================================================

voice_path <- here(
  "data",
  "raw",
  "acustic_features",
  "datiacustici",
  "AUDIO.xlsx"
)
stopifnot(file.exists(voice_path))

# Read sheets
baseline <- read_excel(voice_path, sheet = "BASELINE") %>%
  mutate(timepoint = "baseline")
pre <- read_excel(voice_path, sheet = "PRE") %>% mutate(timepoint = "pre")
post <- read_excel(voice_path, sheet = "POST") %>% mutate(timepoint = "post")

df_voice <- bind_rows(baseline, pre, post)
names(df_voice) <- stringr::str_trim(names(df_voice))

# ID corrections
df_voice <- df_voice %>%
  mutate(
    ID = case_when(
      ID == "am_bo_1988_08_24_166" ~ "an_bo_1988_08_24_166",
      ID == "as_li_2005_04_26_447" ~ "as_si_2005_04_26_447",
      ID == "cl_bo_1987_10_16_628" ~ "ca_bo_1987_10_16_628",
      ID == "hi_na_2005_03_08_339" ~ "gi_na_2005_03_08_339",
      ID == "ma_si_2003_10_31_940" ~ "si_ma_2003_10_31_940",
      TRUE ~ ID
    )
  )

# Extract F0 mean across vowels
df_voice <- df_voice %>%
  transmute(
    ID,
    timepoint = factor(timepoint, levels = c("baseline", "pre", "post")),
    f0_mean_a = `F0 mean Hz /a/`,
    f0_mean_i = `F0 mean Hz /i/`,
    f0_mean_u = `F0 mean Hz /u/`,
    y_f0 = rowMeans(across(c(f0_mean_a, f0_mean_i, f0_mean_u)), na.rm = TRUE)
  ) %>%
  mutate(
    c1_stress = case_when(
      timepoint == "baseline" ~ -0.5,
      timepoint == "pre" ~ 0.5,
      timepoint == "post" ~ 0.0
    ),
    c2_recovery = case_when(
      timepoint == "baseline" ~ 0.0,
      timepoint == "pre" ~ -0.5,
      timepoint == "post" ~ 0.5
    )
  ) %>%
  filter(!is.na(ID), !is.na(y_f0), !is.na(c1_stress), !is.na(c2_recovery))

cat(
  "VOICE: N obs =",
  nrow(df_voice),
  "| N subj =",
  n_distinct(df_voice$ID),
  "\n"
)

# ==============================================================================
# 2) LOAD EMA AND BASELINE PID-5 DATA
# ==============================================================================

ema_path <- here("data", "processed", "ema_plus_scales_cleaned.csv")
stopifnot(file.exists(ema_path))

ema <- rio::import(ema_path) %>% as_tibble()

pid5_ema_vars <- c(
  "pid5_negative_affectivity",
  "pid5_detachment",
  "pid5_antagonism",
  "pid5_disinhibition",
  "pid5_psychoticism"
)

# EMA data
df_ema <- ema %>%
  transmute(
    ID = user_id,
    across(all_of(pid5_ema_vars), as.numeric)
  ) %>%
  filter(!is.na(ID), if_any(all_of(pid5_ema_vars), ~ !is.na(.x)))

# Impute missing EMA values with within-subject means
df_ema <- df_ema %>%
  group_by(ID) %>%
  mutate(across(
    all_of(pid5_ema_vars),
    ~ if_else(is.na(.x), mean(.x, na.rm = TRUE), .x)
  )) %>%
  ungroup() %>%
  filter(if_all(all_of(pid5_ema_vars), ~ is.finite(.x)))

cat("EMA: N rows =", nrow(df_ema), "| N subj =", n_distinct(df_ema$ID), "\n")

# Baseline PID-5 (220 items, single assessment)
# Try to find baseline domain scores in the EMA file
baseline_pid5_candidates <- c(
  "domain_negative_affect_baseline",
  "domain_detachment_baseline",
  "domain_antagonism_baseline",
  "domain_disinhibition_baseline",
  "domain_psychoticism_baseline"
)

# Check if these exist
if (!all(baseline_pid5_candidates %in% names(ema))) {
  stop(
    "Baseline PID-5 domain scores not found in EMA file.\n",
    "Expected columns: ",
    paste(baseline_pid5_candidates, collapse = ", "),
    "\n",
    "Available columns with 'baseline': ",
    paste(grep("baseline", names(ema), value = TRUE), collapse = ", ")
  )
}

# Extract baseline PID-5 (one row per subject)
df_baseline <- ema %>%
  transmute(
    ID = user_id,
    baseline_negative_affectivity = domain_negative_affect_baseline,
    baseline_detachment = domain_detachment_baseline,
    baseline_antagonism = domain_antagonism_baseline,
    baseline_disinhibition = domain_disinhibition_baseline,
    baseline_psychoticism = domain_psychoticism_baseline
  ) %>%
  distinct(ID, .keep_all = TRUE) %>%
  filter(!is.na(ID), if_all(-ID, ~ is.finite(.x)))

cat("BASELINE PID-5: N subj =", n_distinct(df_baseline$ID), "\n")

# ==============================================================================
# 3) IDENTIFY COMMON SUBJECTS ACROSS ALL THREE DATASETS
# ==============================================================================

# Find subjects who have:
# - Voice data (all 3 timepoints)
# - EMA data (multiple assessments)
# - Baseline PID-5 data (single assessment)

subj_voice <- unique(df_voice$ID)
subj_ema <- unique(df_ema$ID)
subj_baseline <- unique(df_baseline$ID)

# Common subjects
subj_common <- Reduce(intersect, list(subj_voice, subj_ema, subj_baseline))
N_subj <- length(subj_common)

cat("\n=== CRITICAL: SUBJECT OVERLAP ===\n")
cat("Voice subjects:", length(subj_voice), "\n")
cat("EMA subjects:", length(subj_ema), "\n")
cat("Baseline PID-5 subjects:", length(subj_baseline), "\n")
cat("COMMON subjects (used in ALL models):", N_subj, "\n\n")

if (N_subj < 50) {
  warning("Low N (", N_subj, ") for LOO comparison. Check data quality.")
}

# Create subject index mapping
id_map <- tibble(ID = subj_common, subj = seq_len(N_subj))

# Filter all datasets to common subjects only
df_voice_common <- df_voice %>%
  inner_join(id_map, by = "ID") %>%
  arrange(subj, timepoint)

df_ema_common <- df_ema %>%
  inner_join(id_map, by = "ID") %>%
  arrange(subj)

df_baseline_common <- df_baseline %>%
  inner_join(id_map, by = "ID") %>%
  arrange(subj)

# Verify integrity
stopifnot(
  n_distinct(df_voice_common$subj) == N_subj,
  n_distinct(df_ema_common$subj) == N_subj,
  n_distinct(df_baseline_common$subj) == N_subj
)

cat("Final data sizes:\n")
cat("  Voice obs:", nrow(df_voice_common), "\n")
cat("  EMA obs:", nrow(df_ema_common), "\n")
cat("  Baseline obs:", nrow(df_baseline_common), "\n\n")

# ==============================================================================
# 4) PREPARE STAN DATA FOR ALL THREE MODELS
# ==============================================================================

# Standardize EMA domains
X_ema <- df_ema_common %>%
  select(all_of(pid5_ema_vars)) %>%
  as.matrix() %>%
  scale()

ema_center <- attr(X_ema, "scaled:center")
ema_scale <- attr(X_ema, "scaled:scale")

# Standardize baseline domains
Z_baseline <- df_baseline_common %>%
  select(starts_with("baseline_")) %>%
  as.matrix() %>%
  scale()

baseline_center <- attr(Z_baseline, "scaled:center")
baseline_scale <- attr(Z_baseline, "scaled:scale")

# --- Data for EMA-only model ---
stan_data_ema <- list(
  N_subj = N_subj,
  N_voice = nrow(df_voice_common),
  subj_voice = as.integer(df_voice_common$subj),
  y = as.numeric(df_voice_common$y_f0),
  c1 = as.numeric(df_voice_common$c1_stress),
  c2 = as.numeric(df_voice_common$c2_recovery),
  N_ema = nrow(df_ema_common),
  subj_ema = as.integer(df_ema_common$subj),
  D = 5,
  X = X_ema
)

# --- Data for Baseline-only model ---
stan_data_baseline <- list(
  N_subj = N_subj,
  N_voice = nrow(df_voice_common),
  subj_voice = as.integer(df_voice_common$subj),
  y = as.numeric(df_voice_common$y_f0),
  c1 = as.numeric(df_voice_common$c1_stress),
  c2 = as.numeric(df_voice_common$c2_recovery),
  D_base = 5,
  Z = Z_baseline
)

# --- Data for Combined model ---
stan_data_combined <- list(
  N_subj = N_subj,
  N_voice = nrow(df_voice_common),
  subj_voice = as.integer(df_voice_common$subj),
  y = as.numeric(df_voice_common$y_f0),
  c1 = as.numeric(df_voice_common$c1_stress),
  c2 = as.numeric(df_voice_common$c2_recovery),
  N_ema = nrow(df_ema_common),
  subj_ema = as.integer(df_ema_common$subj),
  D_ema = 5,
  X = X_ema,
  D_base = 5,
  Z = Z_baseline
)

# Verify all have same y and N_voice (critical for LOO comparison)
stopifnot(
  all.equal(stan_data_ema$y, stan_data_baseline$y),
  all.equal(stan_data_ema$y, stan_data_combined$y),
  stan_data_ema$N_voice == stan_data_baseline$N_voice,
  stan_data_baseline$N_voice == stan_data_combined$N_voice
)

cat(
  "✓ All three models have identical outcome data (N_voice =",
  stan_data_ema$N_voice,
  ")\n\n"
)

# ==============================================================================
# 5) COMPILE STAN MODELS
# ==============================================================================

cat("=== COMPILING STAN MODELS ===\n")

# These should be the corrected models with log_lik in generated quantities
stan_file_ema <- "stan/followup/f0mean_pid5_moderation_with_loglik.stan"
stan_file_baseline <- "stan/followup/pid5_baseline_moderation.stan"
stan_file_combined <- "stan/followup/pid5_ema_plus_baseline_moderation.stan"

# Check files exist
for (f in c(stan_file_ema, stan_file_baseline, stan_file_combined)) {
  if (!file.exists(f)) {
    stop(
      "Stan file not found: ",
      f,
      "\nMake sure you've placed the corrected .stan files in the right location."
    )
  }
}

mod_ema <- cmdstan_model(stan_file_ema)
mod_baseline <- cmdstan_model(stan_file_baseline)
mod_combined <- cmdstan_model(stan_file_combined)

cat("✓ All models compiled successfully\n\n")

# ==============================================================================
# 6) FIT MODELS
# ==============================================================================

cat("=== FITTING MODELS ===\n")

# Sampling settings
n_chains <- 4
n_warmup <- 2000
n_sampling <- 2000
adapt_delta <- 0.99
max_treedepth <- 15

# --- Fit EMA model ---
cat("Fitting EMA model...\n")
fit_ema <- mod_ema$sample(
  data = stan_data_ema,
  chains = n_chains,
  parallel_chains = n_chains,
  iter_warmup = n_warmup,
  iter_sampling = n_sampling,
  adapt_delta = adapt_delta,
  max_treedepth = max_treedepth,
  seed = 123,
  refresh = 500
)
fit_ema$save_object("results/followup/fit_f0_ema.rds")

# --- Fit Baseline model ---
cat("Fitting Baseline model...\n")
fit_baseline <- mod_baseline$sample(
  data = stan_data_baseline,
  chains = n_chains,
  parallel_chains = n_chains,
  iter_warmup = n_warmup,
  iter_sampling = n_sampling,
  adapt_delta = adapt_delta,
  max_treedepth = max_treedepth,
  seed = 123,
  refresh = 500
)
fit_baseline$save_object("results/followup/fit_f0_baseline.rds")

# --- Fit Combined model ---
cat("Fitting Combined model...\n")
fit_combined <- mod_combined$sample(
  data = stan_data_combined,
  chains = n_chains,
  parallel_chains = n_chains,
  iter_warmup = n_warmup,
  iter_sampling = n_sampling,
  adapt_delta = adapt_delta,
  max_treedepth = max_treedepth,
  seed = 123,
  refresh = 500
)
fit_combined$save_object("results/followup/fit_f0_combined.rds")

cat("\n✓ All models fitted successfully\n\n")

# ==============================================================================
# 7) EXTRACT LOO AND COMPARE
# ==============================================================================

cat("=== LOO-CV COMPARISON ===\n")

# Extract log_lik
log_lik_ema <- fit_ema$draws("log_lik", format = "matrix")
log_lik_baseline <- fit_baseline$draws("log_lik", format = "matrix")
log_lik_combined <- fit_combined$draws("log_lik", format = "matrix")

# Compute LOO
loo_ema <- loo(log_lik_ema)
loo_baseline <- loo(log_lik_baseline)
loo_combined <- loo(log_lik_combined)

# Save individual LOO objects
saveRDS(loo_ema, "results/followup/loo_f0_ema.rds")
saveRDS(loo_baseline, "results/followup/loo_f0_baseline.rds")
saveRDS(loo_combined, "results/followup/loo_f0_combined.rds")

cat("\nIndividual LOO results:\n")
print(loo_ema)
print(loo_baseline)
print(loo_combined)

# Compare models
cat("\n=== MODEL COMPARISON ===\n")
loo_comp <- loo_compare(
  list(
    "EMA" = loo_ema,
    "Baseline" = loo_baseline,
    "Combined" = loo_combined
  )
)

print(loo_comp)

# Save comparison
saveRDS(loo_comp, "results/followup/loo_comparison_f0.rds")
write.csv(
  as.data.frame(loo_comp),
  "results/followup/loo_comparison_f0.csv",
  row.names = TRUE
)

# ==============================================================================
# 8) INTERPRET RESULTS
# ==============================================================================

cat("\n=== INTERPRETATION ===\n\n")

best_model <- rownames(loo_comp)[1]
elpd_diff <- loo_comp[2, "elpd_diff"]
se_diff <- loo_comp[2, "se_diff"]

cat("Best model:", best_model, "\n")
cat(
  "ELPD difference (best vs 2nd):",
  round(elpd_diff, 2),
  "±",
  round(se_diff, 2),
  "\n\n"
)

if (abs(elpd_diff) > 2 * se_diff) {
  cat("✓ Strong evidence for", best_model, "model\n")
} else {
  cat("⚠ Models have similar predictive performance (difference < 2 SE)\n")
}

cat("\nInterpretation guide:\n")
cat("• elpd_diff > 0: Model performs better than reference\n")
cat("• elpd_diff < -2*SE: Model performs meaningfully worse\n")
cat("• |elpd_diff| < 2*SE: Models are practically equivalent\n\n")

cat("=== ANALYSIS COMPLETE ===\n")
cat("Results saved in: results/followup/\n")

# ==============================================================================
# 9) EXTRACT AND COMPARE MODERATION EFFECTS
# ==============================================================================

cat("\n=== MODERATION EFFECT COMPARISON ===\n")

# Helper function
pd <- function(x) max(mean(x > 0), mean(x < 0))

# Extract g1 (stress moderation) for each model
extract_moderation <- function(fit, param_prefix = "g1") {
  draws <- fit$draws(format = "df")
  results <- tibble()

  for (d in 1:5) {
    param_name <- paste0(param_prefix, "[", d, "]")
    if (param_name %in% names(draws)) {
      x <- draws[[param_name]]
      results <- bind_rows(
        results,
        tibble(
          domain = d,
          mean = mean(x),
          sd = sd(x),
          q05 = quantile(x, 0.05),
          q95 = quantile(x, 0.95),
          pd = pd(x),
          p_gt0 = mean(x > 0)
        )
      )
    }
  }
  results
}

# Stress moderation (g1)
g1_ema <- extract_moderation(fit_ema, "g1") %>%
  mutate(model = "EMA", contrast = "Stress")
g1_baseline <- extract_moderation(fit_baseline, "g1") %>%
  mutate(model = "Baseline", contrast = "Stress")

# Recovery moderation (g2)
g2_ema <- extract_moderation(fit_ema, "g2") %>%
  mutate(model = "EMA", contrast = "Recovery")
g2_baseline <- extract_moderation(fit_baseline, "g2") %>%
  mutate(model = "Baseline", contrast = "Recovery")

# For combined model, need to handle both g1_ema and g1_base
if ("g1_ema[1]" %in% fit_combined$metadata()$stan_variables) {
  g1_comb_ema <- extract_moderation(fit_combined, "g1_ema") %>%
    mutate(model = "Combined_EMA", contrast = "Stress")
  g1_comb_base <- extract_moderation(fit_combined, "g1_base") %>%
    mutate(model = "Combined_Baseline", contrast = "Stress")
  g2_comb_ema <- extract_moderation(fit_combined, "g2_ema") %>%
    mutate(model = "Combined_EMA", contrast = "Recovery")
  g2_comb_base <- extract_moderation(fit_combined, "g2_base") %>%
    mutate(model = "Combined_Baseline", contrast = "Recovery")

  moderation_comparison <- bind_rows(
    g1_ema,
    g1_baseline,
    g1_comb_ema,
    g1_comb_base,
    g2_ema,
    g2_baseline,
    g2_comb_ema,
    g2_comb_base
  )
} else {
  moderation_comparison <- bind_rows(g1_ema, g1_baseline, g2_ema, g2_baseline)
}

# Add domain labels
moderation_comparison <- moderation_comparison %>%
  mutate(
    domain_name = factor(
      domain,
      levels = 1:5,
      labels = c(
        "Negative\nAffectivity",
        "Detachment",
        "Antagonism",
        "Disinhibition",
        "Psychoticism"
      )
    )
  )

write_csv(moderation_comparison, "results/followup/moderation_comparison.csv")

cat("\nModeration effects by model:\n")
print(
  moderation_comparison %>%
    filter(contrast == "Stress", domain == 1) %>% # Focus on NA stress moderation
    select(model, mean, q05, q95, pd)
)

cat("\nSaved: results/followup/moderation_comparison.csv\n")

cat("\n✓ ALL ANALYSES COMPLETE\n")
