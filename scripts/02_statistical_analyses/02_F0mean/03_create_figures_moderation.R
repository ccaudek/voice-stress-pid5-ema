# ==============================================================================
# 03_create_figures_moderation.R
# Genera figure per il manoscritto: moderation effects PID-5 × stress/recovery
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(posterior)
  library(bayesplot)
  library(ggdist)
  library(patchwork)
})

# Carica il modello fitted
fit <- readRDS("stan/F0/f0mean_pid5_moderation.RDS")
bundle <- readRDS("results/stan_bundle_f0mean_pid5.rds")
stan_data <- bundle$stan_data
pid5_vars <- bundle$pid5_vars

# Etichette più leggibili per le figure
pid5_labels <- c(
  "Negative\nAffectivity",
  "Detachment",
  "Antagonism",
  "Disinhibition",
  "Psychoticism"
)

# ==============================================================================
# FIGURA 1: Coefficient plot per g1 (stress moderation) e g2 (recovery moderation)
# ==============================================================================

# Estrai posterior draws
draws <- fit$draws()

# Crea dataset per ggplot
moderation_data <- tibble()

for (d in 1:5) {
  g1_draws <- as.numeric(draws[,, paste0("g1[", d, "]")])
  g2_draws <- as.numeric(draws[,, paste0("g2[", d, "]")])

  moderation_data <- moderation_data %>%
    bind_rows(
      tibble(
        domain = pid5_labels[d],
        parameter = "Stress Moderation (γ₁)",
        estimate = mean(g1_draws),
        lower = quantile(g1_draws, 0.025),
        upper = quantile(g1_draws, 0.975),
        pd = max(mean(g1_draws > 0), mean(g1_draws < 0))
      ),
      tibble(
        domain = pid5_labels[d],
        parameter = "Recovery Moderation (γ₂)",
        estimate = mean(g2_draws),
        lower = quantile(g2_draws, 0.025),
        upper = quantile(g2_draws, 0.975),
        pd = max(mean(g2_draws > 0), mean(g2_draws < 0))
      )
    )
}

# Ordina per facilitare la lettura
moderation_data <- moderation_data %>%
  mutate(
    domain = factor(domain, levels = rev(pid5_labels)),
    strong = pd > 0.95
  )

# Plot
fig1 <- ggplot(moderation_data, aes(x = estimate, y = domain)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_linerange(
    aes(xmin = lower, xmax = upper, color = strong),
    size = 1.5,
    alpha = 0.8
  ) +
  geom_point(aes(fill = strong), size = 3, shape = 21) +
  facet_wrap(~parameter) +
  scale_color_manual(
    values = c("TRUE" = "#d62728", "FALSE" = "gray60"),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#d62728", "FALSE" = "gray60"),
    guide = "none"
  ) +
  labs(
    x = "Moderation Effect (Hz per SD increase in trait)",
    y = NULL,
    title = "PID-5 Moderation of Vocal Stress Response",
    subtitle = "95% credible intervals; red = PD > 0.95"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    axis.text.y = element_text(size = 10)
  )

print(fig1)

ggsave(
  filename = here::here(
    "results",
    "f0mean",
    "figure1_moderation_coefficients.png"
  ),
  plot = fig1,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = here::here(
    "results",
    "f0mean",
    "figure1_moderation_coefficients.pdf"
  ),
  plot = fig1,
  width = 10,
  height = 6
)

# ==============================================================================
# FIGURA 2: Predicted trajectories per NA e Antagonism
# ==============================================================================

# Estrai parametri necessari
alpha <- as.numeric(draws[,, "alpha"])
b1 <- as.numeric(draws[,, "b1"])
b2 <- as.numeric(draws[,, "b2"])

# Estrai theta per i due domini di interesse
# Nota: theta ha dimensioni [iteration, chain, N_subj, D]
# Dobbiamo calcolare la media su soggetti o usare valori ipotetici

# Più semplice: calcola traiettorie predette per theta = -1, 0, +1 SD
# (ricorda che theta è già standardizzato)

# Domain 1 = Negative Affectivity, Domain 3 = Antagonism
g1_na <- as.numeric(draws[,, "g1[1]"])
g2_na <- as.numeric(draws[,, "g2[1]"])
g1_ant <- as.numeric(draws[,, "g1[3]"])
g2_ant <- as.numeric(draws[,, "g2[3]"])

# Funzione per calcolare mu previsto
predict_f0 <- function(timepoint, theta, g1_vec, g2_vec) {
  c1 <- case_when(
    timepoint == "Baseline" ~ -0.5,
    timepoint == "Pre-Stress" ~ 0.5,
    timepoint == "Post-Stress" ~ 0
  )
  c2 <- case_when(
    timepoint == "Baseline" ~ 0,
    timepoint == "Pre-Stress" ~ -0.5,
    timepoint == "Post-Stress" ~ 0.5
  )

  mu <- alpha + b1 * c1 + b2 * c2 + g1_vec * c1 * theta + g2_vec * c2 * theta
  return(mu)
}

# Crea dataset per le predizioni
timepoints <- c("Baseline", "Pre-Stress", "Post-Stress")
trait_levels <- c(-1, 0, 1) # Low, Average, High

predictions_na <- expand_grid(
  timepoint = factor(timepoints, levels = timepoints),
  trait_level = trait_levels
) %>%
  rowwise() %>%
  mutate(
    domain = "Negative Affectivity",
    f0_draws = list(predict_f0(timepoint, trait_level, g1_na, g2_na)),
    f0_mean = mean(f0_draws),
    f0_lower = quantile(f0_draws, 0.025),
    f0_upper = quantile(f0_draws, 0.975)
  ) %>%
  ungroup() %>%
  mutate(
    trait_label = factor(
      trait_level,
      levels = c(-1, 0, 1),
      labels = c("Low (-1 SD)", "Average", "High (+1 SD)")
    )
  )

predictions_ant <- expand_grid(
  timepoint = factor(timepoints, levels = timepoints),
  trait_level = trait_levels
) %>%
  rowwise() %>%
  mutate(
    domain = "Antagonism",
    f0_draws = list(predict_f0(timepoint, trait_level, g1_ant, g2_ant)),
    f0_mean = mean(f0_draws),
    f0_lower = quantile(f0_draws, 0.025),
    f0_upper = quantile(f0_draws, 0.975)
  ) %>%
  ungroup() %>%
  mutate(
    trait_label = factor(
      trait_level,
      levels = c(-1, 0, 1),
      labels = c("Low (-1 SD)", "Average", "High (+1 SD)")
    )
  )

predictions_all <- bind_rows(predictions_na, predictions_ant)

# Plot traiettorie
fig2 <- ggplot(
  predictions_all,
  aes(x = timepoint, y = f0_mean, group = trait_label)
) +
  geom_ribbon(
    aes(ymin = f0_lower, ymax = f0_upper, fill = trait_label),
    alpha = 0.2
  ) +
  geom_line(aes(color = trait_label), size = 1.2) +
  geom_point(aes(color = trait_label), size = 3) +
  facet_wrap(~domain) +
  scale_color_manual(
    values = c(
      "Low (-1 SD)" = "#1f77b4",
      "Average" = "gray60",
      "High (+1 SD)" = "#d62728"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Low (-1 SD)" = "#1f77b4",
      "Average" = "gray60",
      "High (+1 SD)" = "#d62728"
    )
  ) +
  labs(
    x = "Timepoint",
    y = "Predicted F0 (Hz)",
    color = "Trait Level",
    fill = "Trait Level",
    title = "Predicted F0 Trajectories by PID-5 Trait Level",
    subtitle = "Bands represent 95% credible intervals"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(fig2)

ggsave(
  filename = here::here(
    "results",
    "f0mean",
    "figure2_predicted_trajectories.png"
  ),
  plot = fig2,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = here::here(
    "results",
    "f0mean",
    "figure2_predicted_trajectories.pdf"
  ),
  plot = fig2,
  width = 10,
  height = 5
)

# ==============================================================================
# FIGURA 3 (SUPPLEMENTARE): Posterior distributions per i parametri chiave
# ==============================================================================

# Estrai tutte le g1 e g2
g_draws <- tibble()
for (d in 1:5) {
  g_draws <- g_draws %>%
    bind_rows(
      tibble(
        domain = pid5_labels[d],
        parameter = "γ₁ (Stress)",
        value = as.numeric(draws[,, paste0("g1[", d, "]")])
      ),
      tibble(
        domain = pid5_labels[d],
        parameter = "γ₂ (Recovery)",
        value = as.numeric(draws[,, paste0("g2[", d, "]")])
      )
    )
}

fig3 <- ggplot(g_draws, aes(x = value, y = domain)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  stat_halfeye(
    .width = c(0.95),
    point_interval = "median_qi",
    normalize = "xy",
    fill = "#1f77b4",
    alpha = 0.7
  ) +
  facet_wrap(~parameter) +
  labs(
    x = "Moderation Effect (Hz per SD)",
    y = NULL,
    title = "Posterior Distributions of Moderation Effects"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

print(fig3)

ggsave(
  filename = here::here(
    "results",
    "f0mean",
    "figureS1_posterior_distributions.png"
  ),
  plot = fig3,
  width = 10,
  height = 6,
  dpi = 300
)

# ==============================================================================
# FIGURA 4 (SUPPLEMENTARE): MCMC diagnostics - trace plots
# ==============================================================================

# Seleziona parametri chiave per trace plots
params_to_plot <- c(
  "alpha",
  "b1",
  "b2",
  paste0("g1[", 1:5, "]"),
  paste0("g2[", 1:5, "]")
)

fig4 <- mcmc_trace(
  fit$draws(params_to_plot),
  facet_args = list(ncol = 3)
) +
  labs(title = "MCMC Trace Plots for Key Parameters") +
  theme_minimal(base_size = 10)

print(fig4)

ggsave(
  filename = here::here("results", "f0mean", "figureS2_trace_plots.png"),
  plot = fig4,
  width = 12,
  height = 10,
  dpi = 300
)

cat("\n=== FIGURE GENERATION COMPLETE ===\n")
cat("Created:\n")
cat("  - Figure 1: Coefficient plot (main text)\n")
cat("  - Figure 2: Predicted trajectories (main text)\n")
cat("  - Figure S1: Posterior distributions (supplementary)\n")
cat("  - Figure S2: Trace plots (supplementary)\n")

# eof ---
