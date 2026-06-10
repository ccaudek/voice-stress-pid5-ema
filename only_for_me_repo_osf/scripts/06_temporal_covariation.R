# =============================================================================
# 06_temporal_covariation.R
#
# Within-person temporal covariation: do momentary PID-5 fluctuations
# (EMA aggregated per exam assessment window) covary with concurrent F0?
#
# Design: 3 observations per person (baseline, pre-exam, post-exam).
# Outcome: within-person deviation of F0 from person mean (f0_wp).
# Predictors: within-person deviations of EMA PID-5 domain means per window.
#
# Two models compared via LOO-CV:
#   Model 1 (Baseline):      fixed within-person slopes + random intercepts
#   Model 2 (Random Slopes): person-specific slopes (non-centred)
#
# Key results (Supplementary Materials, p. 17):
#   Random slopes R² ≈ 35% vs fixed slopes R² ≈ 2.5%;
#   ELPD difference ≈ 40 points.
#   99.7% of individual slope CIs include zero — high uncertainty with
#   only 3 timepoints per person.
#
# Requires:
#   data/processed/audio_required_columns.csv
#   data/processed/pid5_required_columns.csv  (must include exam_period column)
#
# Stan models (place in stan/temporal/):
#   baseline_model.stan
#   random_slopes_noncentered.stan
#
# Outputs (results/temporal/):
#   stan_bundle_temporal.rds
#   fit_baseline.RDS
#   fit_random_slopes.RDS
#   model_comparison.csv
#   fixed_slopes_random_model.csv
# =============================================================================

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(loo)
  library(tidyverse)
  library(here)
})

dir.create(here("results", "temporal"), recursive = TRUE, showWarnings = FALSE)

ema_path <- here("data", "processed", "pid5_required_columns.csv")
voice_path <- here("data", "processed", "audio_required_columns.csv")

stopifnot(file.exists(ema_path), file.exists(voice_path))

ema <- read_csv(ema_path, show_col_types = FALSE)
voice <- read_csv(voice_path, show_col_types = FALSE)

pid5_ema_vars <- c(
  "pid5_negative_affectivity",
  "pid5_detachment",
  "pid5_antagonism",
  "pid5_disinhibition",
  "pid5_psychoticism"
)
domain_labels <- c(
  "Negative Affectivity",
  "Detachment",
  "Antagonism",
  "Disinhibition",
  "Psychoticism"
)

if (!"exam_period" %in% names(ema)) {
  stop(
    "'exam_period' column not found in pid5_required_columns.csv.\n",
    "Add it (values: 'baseline', 'pre_exam', 'post_exam') before running."
  )
}

# =============================================================================
# 1. Aggregate EMA by exam period (one row per participant × timepoint)
# =============================================================================
period_map <- tibble(
  exam_period = c("baseline", "pre_exam", "post_exam"),
  timepoint = c("BASELINE", "PRE", "POST")
)

ema_agg <- ema |>
  dplyr::select(user_id, exam_period, all_of(pid5_ema_vars)) |>
  filter(!is.na(exam_period), if_any(all_of(pid5_ema_vars), ~ !is.na(.x))) |>
  group_by(user_id, exam_period) |>
  summarise(
    across(all_of(pid5_ema_vars), \(x) mean(x, na.rm = TRUE)),
    n_ema = n(),
    .groups = "drop"
  ) |>
  inner_join(period_map, by = "exam_period")

# =============================================================================
# 2. F0 outcome per person × timepoint
# =============================================================================
voice_f0 <- voice |>
  mutate(
    f0_mean = rowMeans(
      across(c(`F0 mean Hz /a/`, `F0 mean Hz /i/`, `F0 mean Hz /u/`)),
      na.rm = TRUE
    )
  ) |>
  dplyr::select(ID, timepoint, f0_mean) |>
  rename(user_id = ID)

# =============================================================================
# 3. Merge and compute within-person deviations
# =============================================================================
df_combined <- inner_join(voice_f0, ema_agg, by = c("user_id", "timepoint")) |>
  filter(!is.na(f0_mean), if_all(all_of(pid5_ema_vars), ~ !is.na(.x)))

person_means <- df_combined |>
  group_by(user_id) |>
  summarise(
    f0_pm = mean(f0_mean, na.rm = TRUE),
    across(
      all_of(pid5_ema_vars),
      \(x) mean(x, na.rm = TRUE),
      .names = "{.col}_pm"
    ),
    .groups = "drop"
  )

df_wp <- df_combined |>
  left_join(person_means, by = "user_id") |>
  mutate(
    f0_wp = f0_mean - f0_pm,
    across(
      all_of(pid5_ema_vars),
      \(x) x - get(paste0(cur_column(), "_pm")),
      .names = "{.col}_wp"
    )
  ) |>
  arrange(user_id, timepoint) |>
  mutate(subj = as.integer(factor(user_id)))

wp_vars <- paste0(pid5_ema_vars, "_wp")
df_model <- df_wp |>
  filter(!is.na(f0_wp), if_all(all_of(wp_vars), ~ !is.na(.x)))

cat("Temporal covariation data:\n")
cat("  N_subj =", n_distinct(df_model$user_id), "\n")
cat("  N obs  =", nrow(df_model), "\n")
cat(
  "  Mean obs per subject:",
  round(nrow(df_model) / n_distinct(df_model$user_id), 1),
  "\n\n"
)

# Within-person SD of predictors (diagnostic)
cat("Within-person SD of PID-5 deviations (X_wp, raw units):\n")
for (v in wp_vars) {
  cat(sprintf("  %s: SD = %.3f\n", v, sd(df_model[[v]], na.rm = TRUE)))
}
cat(sprintf("  f0_wp: SD = %.3f Hz\n\n", sd(df_model$f0_wp, na.rm = TRUE)))

# =============================================================================
# 4. Stan data list  —  X_wp in RAW (non-standardised) units
# =============================================================================
# Standardising X_wp is deliberately omitted. See note at top of script.
X_wp <- as.matrix(df_model[, wp_vars])
stopifnot(!anyNA(X_wp), !anyNA(df_model$f0_wp))

stan_data <- list(
  N = nrow(df_model),
  N_subj = n_distinct(df_model$subj),
  D = length(pid5_ema_vars),
  subj = df_model$subj,
  X_wp = X_wp,
  y_wp = df_model$f0_wp
)

saveRDS(
  list(
    stan_data = stan_data,
    df_model = df_model,
    pid5_vars = pid5_ema_vars,
    wp_vars = wp_vars
  ),
  here("results", "temporal", "stan_bundle_temporal.rds")
)
cat("Saved: results/temporal/stan_bundle_temporal.rds\n\n")

# =============================================================================
# 5. Helper: compile, fit, cache
# =============================================================================
fit_temporal <- function(stan_file, label, seed = 12345) {
  fit_path <- here("results", "temporal", paste0(label, ".RDS"))
  if (file.exists(fit_path)) {
    message(label, ": loading cached fit.")
    return(readRDS(fit_path))
  }
  message(label, ": sampling...")
  mod <- cmdstan_model(here("stan", "temporal", stan_file))
  fit <- mod$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 2000,
    iter_sampling = 5000,
    adapt_delta = 0.999,
    max_treedepth = 15,
    seed = seed,
    refresh = 500
  )
  fit$save_object(file = fit_path)
  fit
}

# =============================================================================
# 6. Fit models
# =============================================================================
fit_bl <- fit_temporal("baseline_model.stan", "fit_baseline")
fit_rs <- fit_temporal("random_slopes_noncentered.stan", "fit_random_slopes")

# =============================================================================
# 7. R² and LOO comparison
# =============================================================================
r2_bl <- fit_bl$summary("r2_within")$mean
r2_rs <- fit_rs$summary("r2_within")$mean

ll_bl <- fit_bl$draws("log_lik", format = "matrix")
ll_rs <- fit_rs$draws("log_lik", format = "matrix")
loo_bl <- loo(ll_bl, cores = 4)
loo_rs <- loo(ll_rs, cores = 4)

loo_comp <- loo_compare(list(baseline = loo_bl, random_slopes = loo_rs))
delta_elpd <- loo_comp[2, "elpd_diff"]
se_diff <- loo_comp[2, "se_diff"]

cat("\n=== Model Comparison ===\n")
print(loo_comp)
cat(sprintf(
  "\nR²: Baseline = %.1f%% | Random Slopes = %.1f%% (improvement: %.0fx)\n",
  r2_bl * 100,
  r2_rs * 100,
  r2_rs / r2_bl
))
cat(sprintf(
  "ΔELPD (random slopes vs baseline): %.1f (SE = %.1f)\n",
  abs(delta_elpd),
  se_diff
))

model_comp <- tibble(
  model = c("baseline", "random_slopes"),
  r2_pct = round(c(r2_bl, r2_rs) * 100, 1),
  elpd = c(
    loo_bl$estimates["elpd_loo", "Estimate"],
    loo_rs$estimates["elpd_loo", "Estimate"]
  ),
  se_elpd = c(
    loo_bl$estimates["elpd_loo", "SE"],
    loo_rs$estimates["elpd_loo", "SE"]
  ),
  delta_elpd = c(NA, delta_elpd),
  se_delta = c(NA, se_diff)
)

write_csv(model_comp, here("results", "temporal", "model_comparison.csv"))
cat("Saved: results/temporal/model_comparison.csv\n")

# =============================================================================
# 8. Population-level slopes (fixed effects)
# =============================================================================
beta_draws <- fit_rs$draws("beta_wp", format = "matrix")

fixed_slopes <- map_dfr(1:5, function(d) {
  b <- beta_draws[, d]
  tibble(
    domain = domain_labels[d],
    median = round(median(b), 3),
    lo95 = round(quantile(b, 0.025), 3),
    hi95 = round(quantile(b, 0.975), 3),
    pd = round(max(mean(b > 0), mean(b < 0)), 3)
  )
})

cat(
  "\nFixed effects (population-mean within-person slopes, Hz per raw PID-5 unit):\n"
)
print(fixed_slopes)
write_csv(
  fixed_slopes,
  here("results", "temporal", "fixed_slopes_random_model.csv")
)
cat("Saved: results/temporal/fixed_slopes_random_model.csv\n")

cat("\n=== Temporal covariation analysis complete ===\n")
cat("Next: run 07_slope_heterogeneity.R\n")
