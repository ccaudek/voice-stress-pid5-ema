# ==============================================================================
# Figures for LOO Comparison: EMA vs Baseline vs Combined
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggdist)
  library(patchwork)
  library(bayesplot)
  library(loo)
})

# ==============================================================================
# Load fitted models and LOO results
# ==============================================================================

fit_ema <- readRDS("results/followup/fit_f0_ema.rds")
fit_baseline <- readRDS("results/followup/fit_f0_baseline.rds")
fit_combined <- readRDS("results/followup/fit_f0_combined.rds")

loo_ema <- readRDS("results/followup/loo_f0_ema.rds")
loo_baseline <- readRDS("results/followup/loo_f0_baseline.rds")
loo_combined <- readRDS("results/followup/loo_f0_combined.rds")

moderation_comparison <- read_csv("results/followup/moderation_comparison.csv")

# ==============================================================================
# FIGURE 1: LOO Comparison - Model Performance
# ==============================================================================

# Extract ELPD estimates
loo_summary <- tibble(
  model = c("EMA", "Combined", "Baseline"),
  elpd = c(
    loo_ema$estimates["elpd_loo", "Estimate"],
    loo_combined$estimates["elpd_loo", "Estimate"],
    loo_baseline$estimates["elpd_loo", "Estimate"]
  ),
  se = c(
    loo_ema$estimates["elpd_loo", "SE"],
    loo_combined$estimates["elpd_loo", "SE"],
    loo_baseline$estimates["elpd_loo", "SE"]
  )
) %>%
  mutate(
    model = factor(model, levels = c("Baseline", "Combined", "EMA")),
    lower = elpd - 2 * se,
    upper = elpd + 2 * se
  )

# Create comparison plot
fig1 <- ggplot(loo_summary, aes(x = elpd, y = model)) +
  geom_vline(
    xintercept = loo_summary$elpd[loo_summary$model == "EMA"],
    linetype = "dashed",
    color = "gray50",
    alpha = 0.5
  ) +
  geom_linerange(
    aes(xmin = lower, xmax = upper),
    size = 1.2,
    color = "#1f77b4"
  ) +
  geom_point(
    size = 4,
    color = "#1f77b4",
    shape = 21,
    fill = "white",
    stroke = 1.5
  ) +
  annotate(
    "text",
    x = loo_summary$elpd[loo_summary$model == "Baseline"] - 5,
    y = "Baseline",
    label = "Δ = -6.4 ± 4.4",
    hjust = 1,
    size = 3.5,
    color = "gray30"
  ) +
  annotate(
    "text",
    x = loo_summary$elpd[loo_summary$model == "Combined"] - 5,
    y = "Combined",
    label = "Δ = -4.0 ± 4.6",
    hjust = 1,
    size = 3.5,
    color = "gray30"
  ) +
  labs(
    x = "Expected Log Predictive Density (ELPD)",
    y = NULL,
    title = "Model Comparison: Out-of-Sample Predictive Performance",
    subtitle = "Error bars = ±2 SE; differences < 2 SE indicate practical equivalence"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
  )

print(fig1)

ggsave(
  "results/followup/figure1_loo_comparison.png",
  plot = fig1,
  width = 8,
  height = 4,
  dpi = 300
)

ggsave(
  "results/followup/figure1_loo_comparison.pdf",
  plot = fig1,
  width = 8,
  height = 4
)

# ==============================================================================
# FIGURE 2: Precision Comparison - Credible Interval Widths
# ==============================================================================

# Calculate CrI widths
precision_data <- moderation_comparison %>%
  filter(contrast == "Stress") %>%
  mutate(
    cri_width = q95 - q05,
    model_type = case_when(
      model == "EMA" ~ "EMA",
      model == "Baseline" ~ "Baseline",
      str_detect(model, "Combined") ~ str_remove(model, "Combined_"),
      TRUE ~ model
    )
  ) %>%
  select(domain, domain_name, model_type, mean, q05, q95, cri_width, pd) %>%
  filter(model_type %in% c("EMA", "Baseline")) # Focus on main comparison

# Width comparison plot
fig2a <- ggplot(
  precision_data,
  aes(x = domain_name, y = cri_width, fill = model_type)
) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", cri_width)),
    position = position_dodge(0.8),
    vjust = -0.5,
    size = 3
  ) +
  scale_fill_manual(
    values = c("EMA" = "#1f77b4", "Baseline" = "#ff7f0e"),
    name = "Model"
  ) +
  labs(
    x = NULL,
    y = "90% Credible Interval Width (Hz)",
    title = "Inferential Precision: EMA vs Baseline Estimates",
    subtitle = "Narrower intervals indicate more precise parameter estimates"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

# Point estimate comparison plot with error bars
fig2b <- ggplot(
  precision_data,
  aes(x = mean, y = domain_name, color = model_type)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_linerange(
    aes(xmin = q05, xmax = q95),
    position = position_dodge(0.5),
    size = 1
  ) +
  geom_point(
    position = position_dodge(0.5),
    size = 3
  ) +
  scale_color_manual(
    values = c("EMA" = "#1f77b4", "Baseline" = "#ff7f0e"),
    name = "Model"
  ) +
  labs(
    x = "Stress Moderation Effect γ₁ (Hz per SD)",
    y = NULL,
    title = "Parameter Estimates with 90% Credible Intervals"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 10)
  )

# Combine panels
fig2 <- fig2b / fig2a + plot_layout(heights = c(1, 0.8))

print(fig2)

ggsave(
  "results/followup/figure2_precision_comparison.png",
  plot = fig2,
  width = 9,
  height = 7,
  dpi = 300
)

ggsave(
  "results/followup/figure2_precision_comparison.pdf",
  plot = fig2,
  width = 9,
  height = 7
)

# ==============================================================================
# FIGURE 3: Focus on Negative Affectivity (Key Finding)
# ==============================================================================

# Extract full posterior distributions for NA stress moderation
draws_ema <- fit_ema$draws(format = "df")
draws_baseline <- fit_baseline$draws(format = "df")

na_posteriors <- bind_rows(
  tibble(
    model = "EMA",
    value = draws_ema$`g1[1]`
  ),
  tibble(
    model = "Baseline",
    value = draws_baseline$`g1[1]`
  )
) %>%
  mutate(model = factor(model, levels = c("EMA", "Baseline")))

# Density plot
fig3 <- ggplot(na_posteriors, aes(x = value, fill = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  stat_halfeye(
    alpha = 0.7,
    .width = c(0.90, 0.50),
    point_interval = "median_qi",
    position = position_dodge(width = 0.4)
  ) +
  scale_fill_manual(
    values = c("EMA" = "#1f77b4", "Baseline" = "#ff7f0e"),
    name = "Model"
  ) +
  labs(
    x = "Negative Affectivity × Stress Interaction γ₁ (Hz per SD)",
    y = "Posterior Density",
    title = "Posterior Distributions: Negative Affectivity Stress Moderation",
    subtitle = "Thick bar = 50% CrI, thin bar = 90% CrI; dot = posterior median"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = "gray80"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

print(fig3)

ggsave(
  "results/followup/figure3_na_posteriors.png",
  plot = fig3,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "results/followup/figure3_na_posteriors.pdf",
  plot = fig3,
  width = 8,
  height = 5
)

# ==============================================================================
# FIGURE 4: Pareto k Diagnostics
# ==============================================================================

# Extract Pareto k values
create_pareto_plot <- function(loo_obj, model_name) {
  k_vals <- loo_obj$diagnostics$pareto_k

  tibble(
    observation = seq_along(k_vals),
    pareto_k = k_vals,
    threshold = case_when(
      pareto_k < 0.5 ~ "Good (k < 0.5)",
      pareto_k < 0.7 ~ "OK (0.5 ≤ k < 0.7)",
      pareto_k < 1.0 ~ "Bad (0.7 ≤ k < 1.0)",
      TRUE ~ "Very bad (k ≥ 1.0)"
    )
  ) %>%
    mutate(
      model = model_name,
      threshold = factor(
        threshold,
        levels = c(
          "Good (k < 0.5)",
          "OK (0.5 ≤ k < 0.7)",
          "Bad (0.7 ≤ k < 1.0)",
          "Very bad (k ≥ 1.0)"
        )
      )
    )
}

pareto_data <- bind_rows(
  create_pareto_plot(loo_ema, "EMA"),
  create_pareto_plot(loo_baseline, "Baseline"),
  create_pareto_plot(loo_combined, "Combined")
) %>%
  mutate(model = factor(model, levels = c("EMA", "Combined", "Baseline")))

fig4 <- ggplot(pareto_data, aes(x = observation, y = pareto_k)) +
  geom_hline(
    yintercept = 0.7,
    linetype = "dashed",
    color = "red",
    alpha = 0.5
  ) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    color = "orange",
    alpha = 0.5
  ) +
  geom_point(aes(color = threshold), size = 1, alpha = 0.6) +
  facet_wrap(~model, ncol = 1) +
  scale_color_manual(
    values = c(
      "Good (k < 0.5)" = "#2ca02c",
      "OK (0.5 ≤ k < 0.7)" = "#ff7f0e",
      "Bad (0.7 ≤ k < 1.0)" = "#d62728",
      "Very bad (k ≥ 1.0)" = "#8b0000"
    ),
    name = "Reliability"
  ) +
  labs(
    x = "Observation Index",
    y = "Pareto k",
    title = "LOO-CV Diagnostics: Pareto k Values by Model",
    subtitle = "k < 0.7 indicates reliable importance sampling estimates"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

print(fig4)

ggsave(
  "results/followup/figure4_pareto_diagnostics.png",
  plot = fig4,
  width = 10,
  height = 7,
  dpi = 300
)

ggsave(
  "results/followup/figure4_pareto_diagnostics.pdf",
  plot = fig4,
  width = 10,
  height = 7
)

# ==============================================================================
# SUMMARY TABLE: Precision Gains
# ==============================================================================

# Calculate precision improvement
precision_summary <- precision_data %>%
  select(domain_name, model_type, cri_width) %>%
  pivot_wider(names_from = model_type, values_from = cri_width) %>%
  mutate(
    improvement = (Baseline - EMA) / Baseline * 100,
    narrower = EMA < Baseline
  ) %>%
  arrange(desc(improvement))

write_csv(
  precision_summary,
  "results/followup/precision_improvement_summary.csv"
)

cat("\n=== PRECISION IMPROVEMENT SUMMARY ===\n")
print(precision_summary)
cat("\nMean improvement:", round(mean(precision_summary$improvement), 1), "%\n")
cat(
  "Median improvement:",
  round(median(precision_summary$improvement), 1),
  "%\n"
)

# ==============================================================================
# Summary Statistics for Manuscript
# ==============================================================================

# Count Pareto k issues
pareto_summary <- pareto_data %>%
  group_by(model, threshold) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(model) %>%
  mutate(
    total = sum(n),
    prop = n / total * 100
  )

cat("\n=== PARETO K SUMMARY ===\n")
print(pareto_summary)

# Save for manuscript
write_csv(pareto_summary, "results/followup/pareto_k_summary.csv")

cat("\n=== FIGURE GENERATION COMPLETE ===\n")
cat("Created:\n")
cat("  - Figure 1: LOO comparison (ELPD)\n")
cat("  - Figure 2: Precision comparison (CrI widths + estimates)\n")
cat("  - Figure 3: NA posterior distributions\n")
cat("  - Figure 4: Pareto k diagnostics\n")
cat("  - Summary tables saved as CSV\n")
