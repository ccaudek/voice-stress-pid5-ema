# =============================================================================
# 07_slope_heterogeneity.R
#
# Analyses individual-level heterogeneity in within-person PID-5-F0 slopes
# from the random slopes model fitted in 06_temporal_covariation.R.
#
# Key result (Supplementary Materials, p. 17):
#   "99.7% of participant × domain combinations yielded credible intervals
#    spanning zero."
#   sigma_beta (between-subject SD of slopes): 2.6–4.3 Hz across domains.
#
# Requires:
#   results/temporal/fit_random_slopes.RDS
#   results/temporal/stan_bundle_temporal.rds
#
# Outputs (results/temporal/):
#   individual_slopes_summary.csv    median + 95% CI per participant × domain
#   sigma_beta_summary.csv           between-subject SD per domain
#   heterogeneity_summary.csv        proportion of CIs excluding zero
# =============================================================================

suppressPackageStartupMessages({
  library(posterior)
  library(tidyverse)
  library(here)
})

fit_path <- here("results", "temporal", "fit_random_slopes.RDS")
bundle_path <- here("results", "temporal", "stan_bundle_temporal.rds")

if (!file.exists(fit_path))
  stop("Fit not found. Run 06_temporal_covariation.R first.")

fit <- readRDS(fit_path)
bundle <- readRDS(bundle_path)

stan_data <- bundle$stan_data
N_subj <- stan_data$N_subj
D <- stan_data$D

domain_labels <- c(
  "Negative Affectivity",
  "Detachment",
  "Antagonism",
  "Disinhibition",
  "Psychoticism"
)

# =============================================================================
# 1. Extract individual total slopes (fixed + random)
# =============================================================================
total_slopes <- fit$draws("total_slopes", format = "matrix")
n_iter <- nrow(total_slopes)
slopes_array <- array(total_slopes, dim = c(n_iter, N_subj, D))

slopes_summary <- map_dfr(1:D, function(d) {
  map_dfr(1:N_subj, function(s) {
    draws <- slopes_array[, s, d]
    tibble(
      subject = s,
      domain = domain_labels[d],
      median = round(median(draws), 3),
      lo95 = round(quantile(draws, 0.025), 3),
      hi95 = round(quantile(draws, 0.975), 3),
      pd = round(max(mean(draws > 0), mean(draws < 0)), 3),
      ci_width = round(quantile(draws, 0.975) - quantile(draws, 0.025), 3)
    )
  })
}) |>
  mutate(
    ci_status = case_when(
      lo95 > 0 ~ "Positive (CI excludes 0)",
      hi95 < 0 ~ "Negative (CI excludes 0)",
      TRUE ~ "Uncertain (CI includes 0)"
    )
  )

write_csv(
  slopes_summary,
  here("results", "temporal", "individual_slopes_summary.csv")
)
cat("Saved: individual_slopes_summary.csv\n")

# =============================================================================
# 2. Heterogeneity summary
# =============================================================================
heterogeneity <- slopes_summary |>
  group_by(domain, ci_status) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(pct = round(n / N_subj * 100, 1))

cat("\n=== CI classification by domain ===\n")
print(heterogeneity, n = Inf)

n_uncertain <- sum(slopes_summary$ci_status == "Uncertain (CI includes 0)")
n_total <- nrow(slopes_summary)
pct_uncertain <- n_uncertain / n_total * 100

cat(sprintf(
  "\nOverall: %.1f%% of slopes (%d / %d) have CIs spanning zero.\n",
  pct_uncertain,
  n_uncertain,
  n_total
))

write_csv(
  heterogeneity,
  here("results", "temporal", "heterogeneity_summary.csv")
)
cat("Saved: heterogeneity_summary.csv\n")

# =============================================================================
# 3. Between-subject SD of slopes (sigma_beta)
# =============================================================================
sigma_beta_draws <- fit$draws("sigma_beta", format = "matrix")

sigma_beta_summary <- map_dfr(1:D, function(d) {
  sb <- sigma_beta_draws[, d]
  tibble(
    domain = domain_labels[d],
    mean = round(mean(sb), 3),
    median = round(median(sb), 3),
    lo95 = round(quantile(sb, 0.025), 3),
    hi95 = round(quantile(sb, 0.975), 3)
  )
})

cat("\n=== sigma_beta: between-subject SD of slopes ===\n")
print(sigma_beta_summary)
write_csv(
  sigma_beta_summary,
  here("results", "temporal", "sigma_beta_summary.csv")
)
cat("Saved: sigma_beta_summary.csv\n")

# =============================================================================
# 4. Key numbers for Supplementary Methods
# =============================================================================
cat("\n=== Key numbers for manuscript ===\n")
cat(sprintf(
  "N_subj = %d | D = %d | total slopes = %d\n",
  N_subj,
  D,
  N_subj * D
))
cat(sprintf(
  "Uncertain (CI includes 0): %.1f%% (%d / %d)\n",
  pct_uncertain,
  n_uncertain,
  n_total
))
cat(sprintf(
  "sigma_beta range: %.2f – %.2f Hz\n",
  min(sigma_beta_summary$median),
  max(sigma_beta_summary$median)
))
cat(sprintf(
  "Mean CI width across all slopes: %.1f Hz\n",
  mean(slopes_summary$ci_width)
))

cat("\n=== Slope heterogeneity analysis complete ===\n")
