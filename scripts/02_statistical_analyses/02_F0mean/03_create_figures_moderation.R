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

# ==============================================================================
# Impostazioni generali
# ==============================================================================

# Intervalli di credibilità centrali all'89% secondo la convenzione di McElreath
cri_level <- 0.89
cri_probs <- c((1 - cri_level) / 2, 1 - (1 - cri_level) / 2)
cri_label <- paste0(round(cri_level * 100), "%")

# Controllo
print(cri_probs)
# 0.055 0.945

# Carica il modello fitted
fit <- readRDS("stan/F2/f0mean_pid5_moderation.RDS")
bundle <- readRDS("results/F2/data/stan_bundle_f0mean_pid5.rds")

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

# Estrai posterior draws
draws <- fit$draws()

# Funzione per probability of direction
pd_fun <- function(x) {
  max(mean(x > 0), mean(x < 0))
}

# ==============================================================================
# FIGURA 1: Coefficient plot per g1 e g2
# ==============================================================================

moderation_data <- tibble()

for (d in 1:5) {
  g1_draws <- as.numeric(draws[,, paste0("g1[", d, "]")])
  g2_draws <- as.numeric(draws[,, paste0("g2[", d, "]")])

  moderation_data <- moderation_data %>%
    bind_rows(
      tibble(
        domain = pid5_labels[d],
        parameter = "Stress Moderation (γ₁)",
        estimate = median(g1_draws),
        lower = unname(quantile(g1_draws, cri_probs[1])),
        upper = unname(quantile(g1_draws, cri_probs[2])),
        pd = pd_fun(g1_draws)
      ),
      tibble(
        domain = pid5_labels[d],
        parameter = "Recovery Moderation (γ₂)",
        estimate = median(g2_draws),
        lower = unname(quantile(g2_draws, cri_probs[1])),
        upper = unname(quantile(g2_draws, cri_probs[2])),
        pd = pd_fun(g2_draws)
      )
    )
}

# Ordina per facilitare la lettura
# Nota: PD viene conservata come informazione descrittiva,
# ma non viene usata per creare soglie grafiche o colori.
moderation_data <- moderation_data %>%
  mutate(
    domain = factor(domain, levels = rev(pid5_labels)),
    pd_label = sprintf("PD = %.2f", pd)
  )

# Salva i dati della figura
write.csv(
  moderation_data,
  file = here::here(
    "results",
    "F0",
    "figures",
    "figure1_moderation_coefficients_data_89cri.csv"
  ),
  row.names = FALSE
)

# Plot
fig1 <- ggplot(moderation_data, aes(x = estimate, y = domain)) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "gray50"
  ) +
  geom_linerange(
    aes(xmin = lower, xmax = upper),
    linewidth = 1.2,
    alpha = 0.8,
    color = "gray40"
  ) +
  geom_point(
    size = 3,
    shape = 21,
    fill = "white",
    color = "black"
  ) +
  facet_wrap(~parameter) +
  labs(
    x = "Moderation Effect (Hz per SD increase in trait)",
    y = NULL,
    title = "PID-5 Moderation of Vocal Stress Response",
    subtitle = "Posterior medians with 89% credible intervals; PD is reported descriptively"
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
    "F0",
    "figures",
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
    "F0",
    "figures",
    "figure1_moderation_coefficients.pdf"
  ),
  plot = fig1,
  width = 10,
  height = 6
)

# ==============================================================================
# FIGURA 2: Predicted trajectories per Negative Affectivity e Antagonism
# ==============================================================================

# Estrai parametri necessari
alpha <- as.numeric(draws[,, "alpha"])
b1 <- as.numeric(draws[,, "b1"])
b2 <- as.numeric(draws[,, "b2"])

# Domain 1 = Negative Affectivity
# Domain 3 = Antagonism
g1_na <- as.numeric(draws[,, "g1[1]"])
g2_na <- as.numeric(draws[,, "g2[1]"])

g1_ant <- as.numeric(draws[,, "g1[3]"])
g2_ant <- as.numeric(draws[,, "g2[3]"])

# Funzione per calcolare F0 previsto
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

  mu <- alpha +
    b1 * c1 +
    b2 * c2 +
    g1_vec * c1 * theta +
    g2_vec * c2 * theta

  return(mu)
}

# Crea dataset per le predizioni
timepoints <- c("Baseline", "Pre-Stress", "Post-Stress")
trait_levels <- c(-1, 0, 1)

make_predictions <- function(domain_label, g1_vec, g2_vec) {
  expand_grid(
    timepoint = factor(timepoints, levels = timepoints),
    trait_level = trait_levels
  ) %>%
    rowwise() %>%
    mutate(
      domain = domain_label,
      f0_draws = list(predict_f0(timepoint, trait_level, g1_vec, g2_vec)),
      f0_estimate = median(f0_draws),
      f0_lower = unname(quantile(f0_draws, cri_probs[1])),
      f0_upper = unname(quantile(f0_draws, cri_probs[2]))
    ) %>%
    ungroup() %>%
    mutate(
      trait_label = factor(
        trait_level,
        levels = c(-1, 0, 1),
        labels = c("Low (-1 SD)", "Average", "High (+1 SD)")
      )
    )
}

predictions_na <- make_predictions(
  domain_label = "Negative Affectivity",
  g1_vec = g1_na,
  g2_vec = g2_na
)

predictions_ant <- make_predictions(
  domain_label = "Antagonism",
  g1_vec = g1_ant,
  g2_vec = g2_ant
)

predictions_all <- bind_rows(predictions_na, predictions_ant)

# Salva i dati della figura
write.csv(
  predictions_all %>%
    select(
      domain,
      timepoint,
      trait_level,
      trait_label,
      f0_estimate,
      f0_lower,
      f0_upper
    ),
  file = here::here(
    "results",
    "F0",
    "figures",
    "figure2_predicted_trajectories_data_89cri.csv"
  ),
  row.names = FALSE
)

# Plot traiettorie
fig2 <- ggplot(
  predictions_all,
  aes(x = timepoint, y = f0_estimate, group = trait_label)
) +
  geom_ribbon(
    aes(ymin = f0_lower, ymax = f0_upper, fill = trait_label),
    alpha = 0.2
  ) +
  geom_line(aes(color = trait_label), linewidth = 1.2) +
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
    subtitle = "Bands represent 89% credible intervals"
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
    "F0",
    "figures",
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
    "F0",
    "figures",
    "figure2_predicted_trajectories.pdf"
  ),
  plot = fig2,
  width = 10,
  height = 5
)

# ==============================================================================
# FIGURA 3 SUPPLEMENTARE: Posterior distributions per i parametri chiave
# ==============================================================================

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

g_draws <- g_draws %>%
  mutate(
    domain = factor(domain, levels = rev(pid5_labels))
  )

fig3 <- ggplot(g_draws, aes(x = value, y = domain)) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "gray50"
  ) +
  stat_halfeye(
    .width = cri_level,
    point_interval = "median_qi",
    normalize = "xy",
    fill = "gray70",
    alpha = 0.7
  ) +
  facet_wrap(~parameter) +
  labs(
    x = "Moderation Effect (Hz per SD)",
    y = NULL,
    title = "Posterior Distributions of Moderation Effects",
    subtitle = "Points and intervals show posterior medians and 89% credible intervals"
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
    "F0",
    "figures",
    "figureS1_posterior_distributions.png"
  ),
  plot = fig3,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = here::here(
    "results",
    "F0",
    "figures",
    "figureS1_posterior_distributions.pdf"
  ),
  plot = fig3,
  width = 10,
  height = 6
)

# ==============================================================================
# FIGURA 4 SUPPLEMENTARE: MCMC diagnostics - trace plots
# ==============================================================================

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
  filename = here::here(
    "results",
    "F0",
    "figures",
    "figureS2_trace_plots.png"
  ),
  plot = fig4,
  width = 12,
  height = 10,
  dpi = 300
)

ggsave(
  filename = here::here(
    "results",
    "F0",
    "figures",
    "figureS2_trace_plots.pdf"
  ),
  plot = fig4,
  width = 12,
  height = 10
)

# ==============================================================================
# Fine
# ==============================================================================

cat("\n=== FIGURE GENERATION COMPLETE ===\n")
cat("Created:\n")
cat("  - Figure 1: Coefficient plot, 89% CrI\n")
cat("  - Figure 2: Predicted trajectories, 89% CrI\n")
cat("  - Figure S1: Posterior distributions, 89% CrI\n")
cat("  - Figure S2: Trace plots\n")

# eof
