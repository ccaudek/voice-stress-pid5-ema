# ==============================================================================
# 04_differential_effects_analysis_89cri.R
# Differential moderation patterns: PID-5 domains x stress/recovery
# Uses central 89% credible intervals throughout.
# PD, SNR, and pairwise probabilities are reported descriptively, without
# binary decision thresholds such as PD > .95.
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(posterior)
  library(bayesplot)
  library(ggdist)
  library(patchwork)
  library(here)
})

# ==============================================================================
# SETUP
# ==============================================================================

# Central 89% credible intervals, following McElreath-style reporting
cri_level <- 0.89
cri_probs <- c((1 - cri_level) / 2, 1 - (1 - cri_level) / 2)
cri_label <- paste0(round(cri_level * 100), "%")

# Check: should be 0.055 and 0.945
print(cri_probs)

fit <- readRDS("stan/F0/f0mean_pid5_moderation.RDS")
bundle <- readRDS("results/stan_bundle_f0mean_pid5.rds")

stan_data <- bundle$stan_data
pid5_vars <- bundle$pid5_vars

pid5_labels <- c(
  "Negative Affectivity",
  "Detachment",
  "Antagonism",
  "Disinhibition",
  "Psychoticism"
)

draws <- fit$draws()
sigma_y <- median(as.numeric(draws[, , "sigma_y"]))

# Output directory
out_dir <- here("results", "f0mean")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n=== DIFFERENTIAL MODERATION EFFECTS ANALYSIS ===\n")
cat("Focus: descriptive posterior summaries, pairwise contrasts, and theory alignment\n")
cat("Credible intervals:", cri_label, "central intervals\n")
cat("Residual SD (sigma_y) =", round(sigma_y, 2), "Hz\n\n")

# ==============================================================================
# PART 1: DIRECTIONAL POSTERIOR SUMMARIES
# ==============================================================================

cat("=== PART 1: DIRECTIONAL POSTERIOR SUMMARIES ===\n\n")

pd_fun <- function(x) {
  max(mean(x > 0), mean(x < 0))
}

extract_metrics <- function(draws_param) {
  tibble(
    median = median(draws_param),
    mean = mean(draws_param),
    mad = mad(draws_param),
    ci_lower = unname(quantile(draws_param, cri_probs[1])),
    ci_upper = unname(quantile(draws_param, cri_probs[2])),
    pd = pd_fun(draws_param),
    p_positive = mean(draws_param > 0),
    p_negative = mean(draws_param < 0),
    snr = abs(median(draws_param)) / mad(draws_param)
  )
}

# Analyze all moderation parameters
direction_table <- tibble()

for (d in seq_along(pid5_labels)) {
  # Stress moderation
  g1 <- as.numeric(draws[, , paste0("g1[", d, "]")])
  metrics_g1 <- extract_metrics(g1)

  direction_table <- bind_rows(
    direction_table,
    metrics_g1 %>%
      mutate(
        domain = pid5_labels[d],
        parameter = "Stress (gamma1)"
      )
  )

  # Recovery moderation
  g2 <- as.numeric(draws[, , paste0("g2[", d, "]")])
  metrics_g2 <- extract_metrics(g2)

  direction_table <- bind_rows(
    direction_table,
    metrics_g2 %>%
      mutate(
        domain = pid5_labels[d],
        parameter = "Recovery (gamma2)"
      )
  )
}

# Descriptive summary only: no PD-based decision categories
direction_summary <- direction_table %>%
  mutate(
    direction = ifelse(median > 0, "positive", "negative"),
    pd_label = sprintf("PD = %.2f", pd),
    interval_label = sprintf(
      "%.2f [%0.2f, %0.2f]",
      median,
      ci_lower,
      ci_upper
    )
  ) %>%
  select(
    domain,
    parameter,
    median,
    ci_lower,
    ci_upper,
    pd,
    p_positive,
    p_negative,
    snr,
    direction,
    pd_label,
    interval_label
  )

print(direction_summary, n = Inf)

write_csv(
  direction_summary,
  here(out_dir, "direction_posterior_summary_89cri.csv")
)

# ==============================================================================
# PART 2: DIFFERENTIAL EFFECTS - PAIRWISE PROBABILITIES
# ==============================================================================

cat("\n=== PART 2: DIFFERENTIAL EFFECTS ANALYSIS ===\n\n")

# Highest PD values, reported descriptively rather than as thresholded findings
top_directional_effects <- direction_summary %>%
  arrange(desc(pd)) %>%
  slice_head(n = 5)

cat("Top moderation effects by posterior probability of direction:\n")
print(
  top_directional_effects %>%
    select(domain, parameter, median, ci_lower, ci_upper, pd)
)

cat("\n--- Pairwise Contrasts (Stress moderation) ---\n")

# Example: P(NA > each other domain)
g1_na <- as.numeric(draws[, , "g1[1]"])
for (d in 2:5) {
  g1_other <- as.numeric(draws[, , paste0("g1[", d, "]")])
  p_greater <- mean(g1_na > g1_other)
  cat(sprintf("  P(NA > %s) = %.1f%%\n", pid5_labels[d], p_greater * 100))
}

cat("\n--- Pairwise Contrasts (Recovery moderation) ---\n")

# P(Antagonism > each other domain) for recovery
g2_ant <- as.numeric(draws[, , "g2[3]"])
for (d in c(1, 2, 4, 5)) {
  g2_other <- as.numeric(draws[, , paste0("g2[", d, "]")])
  p_greater <- mean(g2_ant > g2_other)
  cat(sprintf(
    "  P(Antagonism > %s) = %.1f%%\n",
    pid5_labels[d],
    p_greater * 100
  ))
}

# ==============================================================================
# PART 3: MAGNITUDE UNCERTAINTY AND DIRECTIONAL PROBABILITY
# ==============================================================================

cat("\n=== PART 3: MAGNITUDE UNCERTAINTY AND DIRECTIONAL PROBABILITY ===\n\n")

key_effects <- direction_summary %>%
  filter(domain %in% c("Negative Affectivity", "Antagonism", "Detachment")) %>%
  filter(
    (domain == "Negative Affectivity" & parameter == "Stress (gamma1)") |
      (domain == "Antagonism" & parameter == "Recovery (gamma2)") |
      (domain == "Detachment" & parameter == "Recovery (gamma2)")
  )

cat("Key posterior summaries:\n")
for (i in seq_len(nrow(key_effects))) {
  cat(sprintf("\n%s - %s:\n", key_effects$domain[i], key_effects$parameter[i]))
  cat(sprintf(
    "  Median effect: %.2f Hz [%s CrI: %.2f, %.2f]\n",
    key_effects$median[i],
    cri_label,
    key_effects$ci_lower[i],
    key_effects$ci_upper[i]
  ))
  cat(sprintf(
    "  Probability of direction: %.1f%% (PD = %.3f)\n",
    key_effects$pd[i] * 100,
    key_effects$pd[i]
  ))
  cat(sprintf("  Signal-to-noise ratio: %.2f\n", key_effects$snr[i]))

  # Magnitude probabilities are descriptive
  if (key_effects$domain[i] == "Negative Affectivity") {
    g <- as.numeric(draws[, , "g1[1]"])
  } else if (key_effects$domain[i] == "Antagonism") {
    g <- as.numeric(draws[, , "g2[3]"])
  } else {
    g <- as.numeric(draws[, , "g2[2]"])
  }

  cat("  Magnitude probabilities:\n")
  cat(sprintf("    P(|effect| > 1 Hz) = %.1f%%\n", mean(abs(g) > 1) * 100))
  cat(sprintf("    P(|effect| > 2 Hz) = %.1f%%\n", mean(abs(g) > 2) * 100))
  cat(sprintf("    P(|effect| > 3 Hz) = %.1f%%\n", mean(abs(g) > 3) * 100))
}

# ==============================================================================
# VISUALIZATION 1: Directional posterior summaries
# ==============================================================================

direction_plot_data <- direction_summary %>%
  mutate(
    domain = factor(domain, levels = rev(pid5_labels))
  )

fig_direction <- ggplot(
  direction_plot_data,
  aes(x = median, y = domain)
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "gray50",
    linewidth = 1
  ) +
  geom_linerange(
    aes(xmin = ci_lower, xmax = ci_upper),
    linewidth = 1.3,
    alpha = 0.8,
    color = "gray40"
  ) +
  geom_point(
    size = 4,
    shape = 21,
    stroke = 0.5,
    fill = "white",
    color = "black"
  ) +
  geom_text(
    aes(
      label = sprintf("PD=%.2f", pd),
      x = ifelse(median > 0, ci_upper + 0.5, ci_lower - 0.5),
      hjust = ifelse(median > 0, 0, 1)
    ),
    size = 3,
    color = "gray30"
  ) +
  facet_wrap(~parameter) +
  labs(
    x = "Moderation Effect (Hz per SD)",
    y = NULL,
    title = "Directional Posterior Summaries of Moderation Effects",
    subtitle = paste0("Posterior medians with ", cri_label, " credible intervals; PD is descriptive")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

print(fig_direction)

ggsave(
  filename = here(out_dir, "figure_direction_posterior_summary_89cri.png"),
  plot = fig_direction,
  width = 12,
  height = 6,
  dpi = 300
)

ggsave(
  filename = here(out_dir, "figure_direction_posterior_summary_89cri.pdf"),
  plot = fig_direction,
  width = 12,
  height = 6
)

# ==============================================================================
# VISUALIZATION 2: Posterior contrasts for selected stand-out patterns
# ==============================================================================

contrast_data <- tibble()

# Stress moderation: Negative Affectivity versus each other domain
g1_list <- list()
for (d in seq_along(pid5_labels)) {
  g1_list[[d]] <- as.numeric(draws[, , paste0("g1[", d, "]")])
}

for (d in 2:5) {
  diff <- g1_list[[1]] - g1_list[[d]]
  contrast_data <- bind_rows(
    contrast_data,
    tibble(
      contrast = paste0("NA - ", pid5_labels[d]),
      parameter = "Stress",
      difference = diff,
      p_positive = mean(diff > 0)
    )
  )
}

# Recovery moderation: Antagonism versus each other domain
g2_list <- list()
for (d in seq_along(pid5_labels)) {
  g2_list[[d]] <- as.numeric(draws[, , paste0("g2[", d, "]")])
}

for (d in c(1, 2, 4, 5)) {
  diff <- g2_list[[3]] - g2_list[[d]]
  contrast_data <- bind_rows(
    contrast_data,
    tibble(
      contrast = paste0("Antagonism - ", pid5_labels[d]),
      parameter = "Recovery",
      difference = diff,
      p_positive = mean(diff > 0)
    )
  )
}

contrast_summary <- contrast_data %>%
  group_by(contrast, parameter) %>%
  summarise(
    median_diff = median(difference),
    ci_lower = unname(quantile(difference, cri_probs[1])),
    ci_upper = unname(quantile(difference, cri_probs[2])),
    p_greater = unique(p_positive),
    .groups = "drop"
  )

write_csv(
  contrast_summary,
  here(out_dir, "pairwise_contrasts_summary_89cri.csv")
)

fig_contrasts <- contrast_data %>%
  mutate(
    contrast = factor(contrast, levels = unique(contrast_summary$contrast))
  ) %>%
  ggplot(aes(x = difference, y = contrast)) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "gray50"
  ) +
  stat_halfeye(
    .width = cri_level,
    point_interval = "median_qi",
    fill = "gray70",
    alpha = 0.7,
    normalize = "xy"
  ) +
  geom_text(
    data = contrast_summary,
    aes(
      x = ci_upper + 0.5,
      y = contrast,
      label = sprintf("P>0: %.1f%%", p_greater * 100)
    ),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3
  ) +
  facet_wrap(~parameter, scales = "free_y") +
  labs(
    x = "Difference in moderation effect (Hz)",
    y = NULL,
    title = "Pairwise Posterior Contrasts for Selected Domain Comparisons",
    subtitle = paste0("Distributions show posterior differences; intervals are ", cri_label, " credible intervals")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

print(fig_contrasts)

ggsave(
  filename = here(out_dir, "figure_pairwise_contrasts_89cri.png"),
  plot = fig_contrasts,
  width = 12,
  height = 8,
  dpi = 300
)

ggsave(
  filename = here(out_dir, "figure_pairwise_contrasts_89cri.pdf"),
  plot = fig_contrasts,
  width = 12,
  height = 8
)

# ==============================================================================
# VISUALIZATION 3: Signal-to-noise ratio
# ==============================================================================

snr_plot_data <- direction_summary %>%
  mutate(
    domain = factor(domain, levels = rev(pid5_labels))
  )

fig_snr <- ggplot(
  snr_plot_data,
  aes(x = snr, y = domain)
) +
  geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 1.5, linetype = "dotted", alpha = 0.5) +
  geom_segment(
    aes(x = 0, xend = snr, y = domain, yend = domain),
    linewidth = 1.3,
    color = "gray50"
  ) +
  geom_point(size = 4, color = "black") +
  facet_wrap(~parameter) +
  labs(
    x = "Signal-to-noise ratio: |posterior median| / MAD",
    y = NULL,
    title = "Effect Magnitude Relative to Posterior Uncertainty",
    subtitle = "Reference lines at SNR = 1.0 and 1.5 are descriptive only"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

print(fig_snr)

ggsave(
  filename = here(out_dir, "figure_snr_descriptive.png"),
  plot = fig_snr,
  width = 12,
  height = 6,
  dpi = 300
)

ggsave(
  filename = here(out_dir, "figure_snr_descriptive.pdf"),
  plot = fig_snr,
  width = 12,
  height = 6
)

# ==============================================================================
# PART 4: THEORETICAL ALIGNMENT
# ==============================================================================

cat("\n=== PART 4: THEORETICAL ALIGNMENT ===\n\n")

# A priori theoretical predictions
theoretical_predictions <- tribble(
  ~domain,
  ~parameter,
  ~predicted_direction,
  ~rationale,
  "Negative Affectivity",
  "Stress (gamma1)",
  "positive",
  "High NA -> greater stress reactivity",
  "Negative Affectivity",
  "Recovery (gamma2)",
  "negative",
  "High NA -> impaired recovery",
  "Detachment",
  "Stress (gamma1)",
  "negative",
  "High Detachment -> blunted reactivity",
  "Detachment",
  "Recovery (gamma2)",
  "negative",
  "High Detachment -> slower recovery",
  "Antagonism",
  "Stress (gamma1)",
  "ambiguous",
  "Complex pattern; could suppress or amplify",
  "Antagonism",
  "Recovery (gamma2)",
  "negative",
  "High Antagonism -> impaired recovery",
  "Disinhibition",
  "Stress (gamma1)",
  "positive",
  "High Disinhibition -> greater reactivity",
  "Disinhibition",
  "Recovery (gamma2)",
  "negative",
  "High Disinhibition -> impaired regulation",
  "Psychoticism",
  "Stress (gamma1)",
  "ambiguous",
  "Unclear theoretical prediction",
  "Psychoticism",
  "Recovery (gamma2)",
  "negative",
  "High Psychoticism -> impaired recovery"
)

# Compare observed and predicted directions without thresholding PD
alignment_table <- direction_summary %>%
  left_join(theoretical_predictions, by = c("domain", "parameter")) %>%
  mutate(
    observed_direction = ifelse(median > 0, "positive", "negative"),
    alignment = case_when(
      predicted_direction == "ambiguous" ~ "No clear prediction",
      observed_direction == predicted_direction ~ "Same direction as prediction",
      observed_direction != predicted_direction ~ "Opposite direction from prediction",
      TRUE ~ "Unclear"
    )
  ) %>%
  select(
    domain,
    parameter,
    predicted_direction,
    observed_direction,
    median,
    ci_lower,
    ci_upper,
    pd,
    alignment,
    rationale
  )

cat("Theoretical alignment table:\n")
print(alignment_table, n = Inf)

write_csv(
  alignment_table,
  here(out_dir, "theoretical_alignment_table_descriptive_89cri.csv")
)

alignment_counts <- alignment_table %>%
  count(alignment) %>%
  arrange(desc(n))

cat("\nAlignment summary:\n")
print(alignment_counts)

# ==============================================================================
# PART 5: INDIVIDUAL-LEVEL HETEROGENEITY
# ==============================================================================

cat("\n=== PART 5: INDIVIDUAL-LEVEL HETEROGENEITY ===\n\n")

theta_array <- as_draws_array(fit$draws(variables = "theta"))
theta_df <- as_draws_df(theta_array)

u_array <- as_draws_array(fit$draws(variables = "u"))
u_df <- as_draws_df(u_array)

alpha <- as.numeric(draws[, , "alpha"])
b1 <- as.numeric(draws[, , "b1"])
g1_na <- as.numeric(draws[, , "g1[1]"])

individual_responses <- tibble()

for (i in 1:stan_data$N_subj) {
  theta_i <- mean(as.numeric(theta_df[[paste0("theta[", i, ",1]")]]))
  u_i_slope <- mean(as.numeric(u_df[[paste0("u[", i, ",2]")]]))

  # Stress response = change from baseline to pre-stress.
  # With the coding used in the model, Delta c1 = 1.
  stress_response <- mean(b1) + u_i_slope + mean(g1_na) * theta_i

  individual_responses <- bind_rows(
    individual_responses,
    tibble(
      subject = i,
      theta_na = theta_i,
      stress_response = stress_response
    )
  )
}

cor_theta_stress <- cor(
  individual_responses$theta_na,
  individual_responses$stress_response
)

cat("Individual-level heterogeneity (Negative Affectivity):\n")
cat(sprintf(
  "  Correlation (theta_NA, stress_response): r = %.3f\n",
  cor_theta_stress
))
cat(sprintf("  Explained variance: r^2 = %.1f%%\n", cor_theta_stress^2 * 100))
cat(sprintf(
  "  Unexplained variance: %.1f%% (random effects + other factors)\n",
  (1 - cor_theta_stress^2) * 100
))

write_csv(
  individual_responses,
  here(out_dir, "individual_predicted_stress_responses.csv")
)

fig_individual <- ggplot(
  individual_responses,
  aes(x = theta_na, y = stress_response)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(alpha = 0.6, size = 3, color = "gray40") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = sprintf(
      "r = %.3f\nr^2 = %.1f%%",
      cor_theta_stress,
      cor_theta_stress^2 * 100
    ),
    hjust = 1.1,
    vjust = 1.5,
    size = 5
  ) +
  labs(
    x = "Negative Affectivity (latent trait theta)",
    y = "Predicted Stress Response (Delta F0, Hz)",
    title = "Individual Variation in Predicted Stress Response by Negative Affectivity",
    subtitle = paste0(
      "N = ",
      stan_data$N_subj,
      " subjects; smooth line is descriptive"
    )
  ) +
  theme_minimal(base_size = 12)

print(fig_individual)

ggsave(
  filename = here(out_dir, "figure_individual_heterogeneity_descriptive.png"),
  plot = fig_individual,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = here(out_dir, "figure_individual_heterogeneity_descriptive.pdf"),
  plot = fig_individual,
  width = 8,
  height = 6
)

# ==============================================================================
# SUMMARY REPORT
# ==============================================================================

cat("\n=== ANALYSIS COMPLETE ===\n\n")

cat("Files created:\n")
cat("  1. results/f0mean/direction_posterior_summary_89cri.csv\n")
cat("  2. results/f0mean/pairwise_contrasts_summary_89cri.csv\n")
cat("  3. results/f0mean/theoretical_alignment_table_descriptive_89cri.csv\n")
cat("  4. results/f0mean/individual_predicted_stress_responses.csv\n")
cat("  5. results/f0mean/figure_direction_posterior_summary_89cri.png/pdf\n")
cat("  6. results/f0mean/figure_pairwise_contrasts_89cri.png/pdf\n")
cat("  7. results/f0mean/figure_snr_descriptive.png/pdf\n")
cat("  8. results/f0mean/figure_individual_heterogeneity_descriptive.png/pdf\n")

cat("\n=== KEY POSTERIOR DESCRIPTIVES ===\n\n")

cat("Top moderation effects by probability of direction:\n")
for (i in seq_len(nrow(top_directional_effects))) {
  cat(sprintf(
    "   %s - %s: median = %.2f Hz [%s CrI %.2f, %.2f], PD = %.3f\n",
    top_directional_effects$domain[i],
    top_directional_effects$parameter[i],
    top_directional_effects$median[i],
    cri_label,
    top_directional_effects$ci_lower[i],
    top_directional_effects$ci_upper[i],
    top_directional_effects$pd[i]
  ))
}

cat("\nPairwise contrast summaries are posterior probabilities, not binary tests.\n")
cat("The plots use 89% credible intervals throughout.\n")
cat("PD, SNR, and P(contrast > 0) are reported descriptively.\n")
cat("No color coding or labels are based on PD > .95 or similar thresholds.\n")

cat("\n=== INTERPRETATION GUIDANCE ===\n\n")
cat("These analyses describe differential posterior patterns of moderation.\n")
cat("Focus on posterior direction, magnitude, uncertainty, and pairwise probabilities.\n")
cat("Avoid interpreting PD values as hard decision thresholds.\n")
cat("Use the 89% credible intervals as the reported uncertainty intervals.\n")

# eof
