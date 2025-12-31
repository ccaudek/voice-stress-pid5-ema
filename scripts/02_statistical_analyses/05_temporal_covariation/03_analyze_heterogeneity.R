# ==============================================================================
# 03_analyze_heterogeneity.R - REVISED INTERPRETATION
# Analisi slopes individuali con focus su:
# - Alta incertezza con 3 timepoint
# - Effetti sostanzialmente assenti (quasi tutti CI includono zero)
# - Necessità di dense sampling per caratterizzare pattern individuali
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(posterior)
  library(bayesplot)
  library(patchwork)
  library(here)
})

# ==============================================================================
# SETUP
# ==============================================================================

data_dir <- here::here("results/within_person/data")
fitted_dir <- here::here("results/within_person/fitted_models")
output_dir <- here::here("results/within_person/heterogeneity_analysis")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

cat("=== ANALISI SLOPES INDIVIDUALI ===\n\n")

# ==============================================================================
# CARICA RISULTATI
# ==============================================================================

cat("Caricamento risultati...\n")

fit_rs <- readRDS(file.path(fitted_dir, "fit_random_slopes.rds"))
metadata <- readRDS(file.path(data_dir, "metadata.rds"))
stan_data <- readRDS(file.path(data_dir, "stan_data_within_person.rds"))

pid5_vars <- metadata$pid5_vars

# Domini labels
domain_labels <- c(
  "Negative Affectivity",
  "Detachment",
  "Antagonism",
  "Disinhibition",
  "Psychoticism"
)

cat("  ✓ Dati caricati\n")
cat(sprintf("    N_subj = %d\n", stan_data$N_subj))
cat(sprintf("    D = %d domini\n", stan_data$D))
cat(sprintf("    Obs per soggetto: ~%.1f\n", stan_data$N / stan_data$N_subj))
cat("\n")

# ==============================================================================
# ESTRAI INDIVIDUAL SLOPES
# ==============================================================================

cat("Estrazione individual slopes...\n")

total_slopes <- fit_rs$draws("total_slopes", format = "matrix")

n_iter <- nrow(total_slopes)
N_subj <- stan_data$N_subj
D <- stan_data$D

# Reshape in [iterations, N_subj, D]
total_slopes_array <- array(total_slopes, dim = c(n_iter, N_subj, D))

# Summary per ogni soggetto-dominio
individual_slopes <- tibble()

for (d in 1:D) {
  slopes_d <- total_slopes_array[,, d]

  for (s in 1:N_subj) {
    slope_draws <- slopes_d[, s]

    individual_slopes <- individual_slopes %>%
      bind_rows(tibble(
        subject = s,
        domain_id = d,
        domain = domain_labels[d],
        mean_slope = mean(slope_draws),
        median_slope = median(slope_draws),
        sd_slope = sd(slope_draws),
        q025 = quantile(slope_draws, 0.025),
        q975 = quantile(slope_draws, 0.975),
        prob_positive = mean(slope_draws > 0),
        prob_negative = mean(slope_draws < 0),
        # Width del CI come misura di incertezza
        ci_width = quantile(slope_draws, 0.975) - quantile(slope_draws, 0.025)
      ))
  }
}

cat(sprintf("  ✓ %d individual slopes estratti\n\n", nrow(individual_slopes)))

# ==============================================================================
# ANALISI INCERTEZZA
# ==============================================================================

cat("Analisi incertezza slopes individuali...\n")

# Classifica basato su 95% CI
individual_slopes <- individual_slopes %>%
  mutate(
    classification = case_when(
      q025 > 0 ~ "Positive (CI excludes 0)",
      q975 < 0 ~ "Negative (CI excludes 0)",
      TRUE ~ "Uncertain (CI includes 0)"
    ),
    classification = factor(
      classification,
      levels = c(
        "Positive (CI excludes 0)",
        "Uncertain (CI includes 0)",
        "Negative (CI excludes 0)"
      )
    )
  )

# Summary per dominio
uncertainty_summary <- individual_slopes %>%
  group_by(domain, classification) %>%
  summarise(
    n = n(),
    pct = n() / N_subj * 100,
    .groups = "drop"
  ) %>%
  arrange(domain, classification)

cat("\nClassificazione slopes (CI esclude zero?):\n")
print(uncertainty_summary, n = Inf)

# Calcola larghezza media CI
ci_width_summary <- individual_slopes %>%
  group_by(domain) %>%
  summarise(
    mean_ci_width = mean(ci_width),
    median_ci_width = median(ci_width),
    .groups = "drop"
  )

cat("\nLarghezza media 95% CI per dominio:\n")
print(ci_width_summary)

cat("\n⚠️  INTERPRETAZIONE:\n")
cat("Con solo 3 timepoint per persona, l'incertezza sugli slopes individuali\n")
cat(
  "è molto alta. Quasi tutti i CI includono zero, indicando che gli effetti\n"
)
cat("individuali sono difficili da rilevare con questo design sparse.\n\n")

# Salva
write_csv(individual_slopes, file.path(output_dir, "individual_slopes.csv"))
write_csv(uncertainty_summary, file.path(output_dir, "uncertainty_summary.csv"))
write_csv(ci_width_summary, file.path(output_dir, "ci_width_summary.csv"))

cat("✓ Analisi incertezza completata\n\n")

# ==============================================================================
# SIGMA_BETA (Between-subject variability)
# ==============================================================================

cat("Analisi sigma_beta (SD degli slopes)...\n")

sigma_beta_draws <- fit_rs$draws("sigma_beta", format = "matrix")

sigma_beta_summary <- tibble(
  domain = domain_labels,
  mean = colMeans(sigma_beta_draws),
  median = apply(sigma_beta_draws, 2, median),
  q025 = apply(sigma_beta_draws, 2, quantile, 0.025),
  q975 = apply(sigma_beta_draws, 2, quantile, 0.975)
)

cat("\nSD degli slopes (sigma_beta) per dominio:\n")
print(sigma_beta_summary)

cat("\n⚠️  NOTA:\n")
cat(
  "Sigma_beta relativamente grandi (2-4 Hz) indicano variabilità tra soggetti,\n"
)
cat(
  "MA questo non significa che gli slopes individuali siano rilevabili con certezza.\n"
)
cat("Con 3 timepoint, l'incertezza individuale rimane troppo alta.\n\n")

write_csv(sigma_beta_summary, file.path(output_dir, "sigma_beta_summary.csv"))

cat("✓ Analisi sigma_beta completata\n\n")

# ==============================================================================
# VISUALIZZAZIONI
# ==============================================================================

cat("Creazione visualizzazioni...\n\n")

# -------------------------------
# PLOT 1: Distribution of slopes con emphasis su incertezza
# -------------------------------

cat("  1. Density plots con indicazione incertezza...\n")

p1 <- ggplot(individual_slopes, aes(x = mean_slope, fill = domain)) +
  geom_density(alpha = 0.6) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "red",
    linewidth = 1
  ) +
  facet_wrap(~domain, ncol = 1, scales = "free_y") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribution of Individual Within-Person Slopes",
    subtitle = "Most estimates have high uncertainty (see forest plot)",
    x = "Individual Slope (Hz per unit PID-5 change)",
    y = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30")
  )

ggsave(
  file.path(output_dir, "01_slope_densities.pdf"),
  p1,
  width = 10,
  height = 12
)

# -------------------------------
# PLOT 2: Forest plot showing UNCERTAINTY
# -------------------------------

cat("  2. Forest plot (focus: wide CIs = high uncertainty)...\n")

slopes_na <- individual_slopes %>%
  filter(domain == "Negative Affectivity") %>%
  arrange(mean_slope) %>%
  mutate(subject = factor(subject, levels = subject))

p2 <- ggplot(
  slopes_na,
  aes(x = mean_slope, y = subject, color = classification)
) +
  geom_vline(xintercept = 0, linetype = "solid", color = "red", linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_errorbarh(
    aes(xmin = q025, xmax = q975),
    height = 0,
    linewidth = 0.4,
    alpha = 0.6
  ) +
  scale_color_manual(
    values = c(
      "Positive (CI excludes 0)" = "#2166ac",
      "Uncertain (CI includes 0)" = "gray60",
      "Negative (CI excludes 0)" = "#b2182b"
    ),
    name = "Classification"
  ) +
  labs(
    title = "Individual Within-Person Slopes: Negative Affectivity",
    subtitle = sprintf(
      "N = %d participants | Note: Nearly all 95%% CIs include zero",
      N_subj
    ),
    x = "Slope (Hz per unit NA change) - 95% CI",
    y = "Participant"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray30"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  file.path(output_dir, "02_forest_plot_NA.pdf"),
  p2,
  width = 10,
  height = 14
)

# -------------------------------
# PLOT 3: Proportion with CI excluding zero
# -------------------------------

cat("  3. Bar chart: proportion with reliable effects...\n")

p3 <- ggplot(
  uncertainty_summary,
  aes(x = domain, y = pct, fill = classification)
) +
  geom_col(position = "stack") +
  scale_fill_manual(
    values = c(
      "Positive (CI excludes 0)" = "#2166ac",
      "Uncertain (CI includes 0)" = "gray60",
      "Negative (CI excludes 0)" = "#b2182b"
    ),
    name = "Classification"
  ) +
  labs(
    title = "Uncertainty in Individual Within-Person Associations",
    subtitle = "Proportion with 95% CI excluding vs including zero",
    x = "PID-5 Domain",
    y = "Percentage of Participants"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom"
  )

ggsave(
  file.path(output_dir, "03_uncertainty_barchart.pdf"),
  p3,
  width = 10,
  height = 6
)

# -------------------------------
# PLOT 4: CI width distribution
# -------------------------------

cat("  4. CI width distribution (measure of uncertainty)...\n")

p4 <- ggplot(individual_slopes, aes(x = ci_width, fill = domain)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~domain, ncol = 1, scales = "free_y") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Uncertainty in Individual Slope Estimates",
    subtitle = "Width of 95% Credible Intervals",
    x = "CI Width (Hz)",
    y = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave(
  file.path(output_dir, "04_ci_width_distribution.pdf"),
  p4,
  width = 10,
  height = 12
)

# -------------------------------
# PLOT 5: Sigma_beta vs fixed effects
# -------------------------------

cat("  5. Between-subject variability vs population mean...\n")

beta_wp_draws <- fit_rs$draws("beta_wp", format = "matrix")

beta_wp_summary <- tibble(
  domain = domain_labels,
  beta_mean = colMeans(beta_wp_draws),
  beta_q025 = apply(beta_wp_draws, 2, quantile, 0.025),
  beta_q975 = apply(beta_wp_draws, 2, quantile, 0.975)
)

combined_summary <- beta_wp_summary %>%
  left_join(sigma_beta_summary, by = "domain")

p5 <- ggplot(combined_summary, aes(x = beta_mean, y = mean)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 4, color = "steelblue") +
  geom_errorbar(
    aes(ymin = q025.y, ymax = q975.y),
    width = 0.2,
    color = "steelblue"
  ) +
  geom_errorbarh(
    aes(xmin = beta_q025, xmax = beta_q975),
    height = 0.2,
    color = "steelblue"
  ) +
  geom_text(
    aes(label = domain),
    nudge_y = 0.3,
    size = 3,
    check_overlap = TRUE
  ) +
  labs(
    title = "Population Mean vs Between-Subject Variability",
    subtitle = "Both centered near zero (weak within-person effects)",
    x = "Fixed Effect β (population mean)",
    y = "σ_beta (SD across subjects)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray30")
  )

ggsave(
  file.path(output_dir, "05_fixed_vs_variability.pdf"),
  p5,
  width = 10,
  height = 8
)

cat("\n✓ Tutte le visualizzazioni create\n\n")

# ==============================================================================
# SUMMARY REPORT
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("SUMMARY: WEAK/UNCERTAIN WITHIN-PERSON EFFECTS\n")
cat(rep("=", 80), "\n\n")

cat("KEY FINDING:\n")
cat(
  "Within-person associations between PID-5 and F0 are weak and difficult to\n"
)
cat(
  "detect reliably with sparse temporal sampling (3 timepoints per person).\n\n"
)

cat("UNCERTAINTY ANALYSIS:\n")
total_uncertain <- uncertainty_summary %>%
  filter(classification == "Uncertain (CI includes 0)") %>%
  summarise(total = sum(n)) %>%
  pull(total)

total_reliable <- uncertainty_summary %>%
  filter(classification != "Uncertain (CI includes 0)") %>%
  summarise(total = sum(n)) %>%
  pull(total)

cat(sprintf(
  "  Slopes with CI including zero: %d (%.1f%%)\n",
  total_uncertain,
  total_uncertain / (N_subj * D) * 100
))
cat(sprintf(
  "  Slopes with CI excluding zero: %d (%.1f%%)\n\n",
  total_reliable,
  total_reliable / (N_subj * D) * 100
))

cat("BY DOMAIN (Negative Affectivity):\n")
na_summary <- uncertainty_summary %>%
  filter(domain == "Negative Affectivity")
for (i in 1:nrow(na_summary)) {
  cat(sprintf(
    "  %s: %d (%.1f%%)\n",
    na_summary$classification[i],
    na_summary$n[i],
    na_summary$pct[i]
  ))
}

cat("\nSIGMA_BETA (Between-subject SD):\n")
for (i in 1:nrow(sigma_beta_summary)) {
  cat(sprintf(
    "  %s: %.2f Hz [%.2f, %.2f]\n",
    sigma_beta_summary$domain[i],
    sigma_beta_summary$mean[i],
    sigma_beta_summary$q025[i],
    sigma_beta_summary$q975[i]
  ))
}

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("INTERPRETATION\n")
cat(rep("=", 80), "\n\n")

cat("Random slopes models fit substantially better than fixed effects\n")
cat("(R² improvement, ELPD improvement). However, this does NOT indicate\n")
cat("clear idiographic patterns. Instead:\n\n")

cat("1. SPARSE DESIGN LIMITATION:\n")
cat("   - Only 3 timepoints per person\n")
cat("   - Results in wide credible intervals\n")
cat("   - Nearly all slopes indistinguishable from zero\n\n")

cat("2. WEAK/ABSENT EFFECTS:\n")
cat("   - Population-level effects centered near zero\n")
cat("   - Individual deviations small relative to uncertainty\n")
cat("   - Only ~1-2% of participants show reliable effects\n\n")

cat("3. IMPLICATIONS:\n")
cat("   - Dense sampling (10-20+ obs) needed to characterize individuals\n")
cat("   - OR within-person effects genuinely weak/absent\n")
cat("   - Between-person effects may be more informative\n\n")

cat(rep("=", 80), "\n", sep = "")
cat("ANALISI COMPLETATA\n")
cat(rep("=", 80), "\n\n")

cat("File salvati in:", output_dir, "\n")
cat("  - individual_slopes.csv\n")
cat("  - uncertainty_summary.csv\n")
cat("  - ci_width_summary.csv\n")
cat("  - sigma_beta_summary.csv\n")
cat("  - 01_slope_densities.pdf\n")
cat("  - 02_forest_plot_NA.pdf (KEY: mostra wide CIs)\n")
cat("  - 03_uncertainty_barchart.pdf\n")
cat("  - 04_ci_width_distribution.pdf\n")
cat("  - 05_fixed_vs_variability.pdf\n\n")

cat("✓ Prossimo step: source('04_create_manuscript_materials.R')\n\n")

# eof ---
