# ==============================================================================
# Manuscript Tables and Figures: Main Effects
# ==============================================================================
# This script creates publication-ready tables and figures for the manuscript
# ==============================================================================

library(tidyverse)
library(rstan)
library(bayesplot)
library(gt)
library(patchwork)
library(here)

# Load fitted models
fit_f0 <- readRDS(here(
  "results",
  "stress",
  "models",
  "fit_f0_main_effects.rds"
))
fit_nne <- readRDS(here(
  "results",
  "stress",
  "models",
  "fit_nne_main_effects.rds"
))

# Load posterior samples
post_f0 <- read_csv(here(
  "results",
  "stress",
  "models",
  "f0_posterior_samples.csv"
))
post_nne <- read_csv(here(
  "results",
  "stress",
  "models",
  "nne_posterior_samples.csv"
))

# ==============================================================================
# TABLE 1: Model Parameters and Posterior Summaries
# ==============================================================================

# Helper function to format posterior summaries
format_posterior <- function(samples, param_name) {
  values <- samples[[param_name]]
  tibble(
    Median = median(values),
    MAD = mad(values),
    CI_lower = quantile(values, 0.025),
    CI_upper = quantile(values, 0.975),
    P_direction = ifelse(median(values) > 0, mean(values > 0), mean(values < 0))
  ) %>%
    mutate(
      Estimate = sprintf("%.2f (%.2f)", Median, MAD),
      CI_95 = sprintf("[%.2f, %.2f]", CI_lower, CI_upper),
      P_dir = sprintf("%.3f", P_direction)
    ) %>%
    dplyr::select(Estimate, CI_95, P_dir)
}

# F0 table
table_f0 <- bind_rows(
  format_posterior(post_f0, "alpha") %>% mutate(Parameter = "Intercept (α)"),
  format_posterior(post_f0, "b1") %>% mutate(Parameter = "Stress Effect (β₁)"),
  format_posterior(post_f0, "b2") %>%
    mutate(Parameter = "Recovery Effect (β₂)"),
  format_posterior(post_f0, "tau[1]") %>% mutate(Parameter = "SD(Intercept)"),
  format_posterior(post_f0, "tau[2]") %>% mutate(Parameter = "SD(Stress)"),
  format_posterior(post_f0, "tau[3]") %>% mutate(Parameter = "SD(Recovery)"),
  format_posterior(post_f0, "sigma_y") %>% mutate(Parameter = "Residual SD (σ)")
) %>%
  dplyr::select(Parameter, everything()) %>%
  mutate(Outcome = "F0 Mean (Hz)")

# NNE table
table_nne <- bind_rows(
  format_posterior(post_nne, "alpha") %>% mutate(Parameter = "Intercept (α)"),
  format_posterior(post_nne, "b1") %>% mutate(Parameter = "Stress Effect (β₁)"),
  format_posterior(post_nne, "b2") %>%
    mutate(Parameter = "Recovery Effect (β₂)"),
  format_posterior(post_nne, "tau[1]") %>% mutate(Parameter = "SD(Intercept)"),
  format_posterior(post_nne, "tau[2]") %>% mutate(Parameter = "SD(Stress)"),
  format_posterior(post_nne, "tau[3]") %>% mutate(Parameter = "SD(Recovery)"),
  format_posterior(post_nne, "sigma_y") %>%
    mutate(Parameter = "Residual SD (σ)")
) %>%
  dplyr::select(Parameter, everything()) %>%
  mutate(Outcome = "NNE (dB)")

# Combine tables
table_combined <- bind_rows(table_f0, table_nne) %>%
  dplyr::select(Outcome, Parameter, Estimate, CI_95, P_dir)

# Create formatted table with gt
gt_table <- table_combined %>%
  gt(groupname_col = "Outcome") %>%
  tab_header(
    title = "Hierarchical Bayesian Models: Main Effects of Exam Stress on Vocal Parameters",
    subtitle = "Posterior median (MAD) and 95% credible intervals"
  ) %>%
  cols_label(
    Parameter = "Parameter",
    Estimate = "Estimate (MAD)",
    CI_95 = "95% CI",
    P_dir = "P(direction)"
  ) %>%
  tab_footnote(
    footnote = "β₁ = stress effect (PRE vs BASELINE); β₂ = recovery effect (POST vs PRE)",
    locations = cells_column_labels(columns = Parameter)
  ) %>%
  tab_footnote(
    footnote = "P(direction) = posterior probability that parameter has the same sign as the median",
    locations = cells_column_labels(columns = P_dir)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  opt_row_striping()

# Save table
gtsave(
  gt_table,
  here("results", "stress", "tables", "table1_main_effects.html")
)
gtsave(
  gt_table,
  here("results", "stress", "tables", "table1_main_effects.docx")
)

# ==============================================================================
# FIGURE 1: Posterior Distributions of Main Effects
# ==============================================================================

# Prepare data for plotting
plot_data <- bind_rows(
  post_f0 %>%
    dplyr::select(b1, b2) %>%
    pivot_longer(everything(), names_to = "parameter", values_to = "value") %>%
    mutate(
      outcome = "F0 Mean (Hz)",
      parameter = recode(
        parameter,
        "b1" = "Stress Effect (PRE vs BASELINE)",
        "b2" = "Recovery Effect (POST vs PRE)"
      )
    ),
  post_nne %>%
    dplyr::select(b1, b2) %>%
    pivot_longer(everything(), names_to = "parameter", values_to = "value") %>%
    mutate(
      outcome = "NNE (dB)",
      parameter = recode(
        parameter,
        "b1" = "Stress Effect (PRE vs BASELINE)",
        "b2" = "Recovery Effect (POST vs PRE)"
      )
    )
)

# Create combined density plot
fig1 <- ggplot(plot_data, aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
  facet_wrap(~outcome, scales = "free", ncol = 1) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Posterior Distributions of Main Effects",
    subtitle = "Exam stress modulates fundamental frequency and glottal noise",
    x = "Effect Size",
    y = "Posterior Density",
    fill = "Contrast"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12)
  )

ggsave(
  here("results", "stress", "figures", "figure1_posterior_distributions.png"),
  fig1,
  width = 10,
  height = 8,
  dpi = 300
)

# ==============================================================================
# FIGURE 2: Model-Implied Trajectories
# ==============================================================================

# Calculate model-implied means for each timepoint
# F0 trajectory
f0_trajectory <- tibble(
  timepoint = factor(
    rep(c("BASELINE", "PRE", "POST"), each = nrow(post_f0)),
    levels = c("BASELINE", "PRE", "POST")
  ),
  value = c(
    post_f0$alpha - 0.5 * post_f0$b1, # BASELINE
    post_f0$alpha + 0.5 * post_f0$b1 - 0.5 * post_f0$b2, # PRE
    post_f0$alpha + 0.5 * post_f0$b2 # POST
  ),
  outcome = "F0 Mean (Hz)"
)

# NNE trajectory
nne_trajectory <- tibble(
  timepoint = factor(
    rep(c("BASELINE", "PRE", "POST"), each = nrow(post_nne)),
    levels = c("BASELINE", "PRE", "POST")
  ),
  value = c(
    post_nne$alpha - 0.5 * post_nne$b1, # BASELINE
    post_nne$alpha + 0.5 * post_nne$b1 - 0.5 * post_nne$b2, # PRE
    post_nne$alpha + 0.5 * post_nne$b2 # POST
  ),
  outcome = "NNE (dB)"
)

# Combine trajectories
trajectory_data <- bind_rows(f0_trajectory, nne_trajectory)

# Summarize posteriors
trajectory_summary <- trajectory_data %>%
  group_by(outcome, timepoint) %>%
  summarise(
    median = median(value),
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975),
    .groups = "drop"
  )

# Create trajectory plot
fig2 <- ggplot(trajectory_summary, aes(x = timepoint, y = median, group = 1)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.3,
    fill = "steelblue"
  ) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  facet_wrap(~outcome, scales = "free_y", ncol = 1) +
  labs(
    title = "Model-Implied Vocal Parameter Trajectories",
    subtitle = "Median estimates with 95% credible intervals",
    x = "Assessment Timepoint",
    y = "Parameter Value"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

ggsave(
  here("results", "stress", "figures", "figure2_trajectories.png"),
  fig2,
  width = 8,
  height = 8,
  dpi = 300
)

# ==============================================================================
# FIGURE 3: Effect Sizes with Uncertainty
# ==============================================================================

# Calculate effect sizes
effects_data <- bind_rows(
  # F0 effects
  tibble(
    outcome = "F0 Mean (Hz)",
    effect = "Stress\n(PRE vs BASELINE)",
    median = median(post_f0$b1),
    lower = quantile(post_f0$b1, 0.025),
    upper = quantile(post_f0$b1, 0.975),
    prob = mean(post_f0$b1 > 0)
  ),
  tibble(
    outcome = "F0 Mean (Hz)",
    effect = "Recovery\n(POST vs PRE)",
    median = median(post_f0$b2),
    lower = quantile(post_f0$b2, 0.025),
    upper = quantile(post_f0$b2, 0.975),
    prob = mean(post_f0$b2 > 0)
  ),
  # NNE effects
  tibble(
    outcome = "NNE (dB)",
    effect = "Stress\n(PRE vs BASELINE)",
    median = median(post_nne$b1),
    lower = quantile(post_nne$b1, 0.025),
    upper = quantile(post_nne$b1, 0.975),
    prob = mean(post_nne$b1 < 0)
  ),
  tibble(
    outcome = "NNE (dB)",
    effect = "Recovery\n(POST vs PRE)",
    median = median(post_nne$b2),
    lower = quantile(post_nne$b2, 0.025),
    upper = quantile(post_nne$b2, 0.975),
    prob = mean(post_nne$b2 > 0)
  )
) %>%
  mutate(
    significance = ifelse(prob > 0.95, "Strong", "Weak"),
    label = sprintf("P = %.3f", prob)
  )

# Create effect size plot
fig3 <- ggplot(
  effects_data,
  aes(x = effect, y = median, color = significance)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 1) +
  geom_point(size = 4) +
  geom_text(aes(label = label), hjust = -0.2, size = 3, color = "black") +
  facet_wrap(~outcome, scales = "free", ncol = 2) +
  scale_color_manual(values = c("Strong" = "#2C7BB6", "Weak" = "#D7191C")) +
  labs(
    title = "Effect Sizes with Posterior Uncertainty",
    subtitle = "95% credible intervals and posterior probabilities",
    x = "Contrast",
    y = "Effect Size",
    color = "Evidence Strength"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  ) +
  coord_flip()

ggsave(
  here("results", "stress", "figures", "figure3_effect_sizes.png"),
  fig3,
  width = 10,
  height = 6,
  dpi = 300
)

# ==============================================================================
# SUPPLEMENTARY: Trace Plots and Diagnostics
# ==============================================================================

# F0 trace plots
trace_f0 <- mcmc_trace(fit_f0, pars = c("alpha", "b1", "b2", "sigma_y")) +
  labs(title = "F0 Model: MCMC Trace Plots") +
  theme_minimal()

ggsave(
  here("figures", "suppl_f0_traces.png"),
  trace_f0,
  width = 10,
  height = 6,
  dpi = 300
)

# NNE trace plots
trace_nne <- mcmc_trace(fit_nne, pars = c("alpha", "b1", "b2", "sigma_y")) +
  labs(title = "NNE Model: MCMC Trace Plots") +
  theme_minimal()

ggsave(
  here("figures", "suppl_nne_traces.png"),
  trace_nne,
  width = 10,
  height = 6,
  dpi = 300
)

cat("\n=== Manuscript tables and figures created successfully ===\n")
