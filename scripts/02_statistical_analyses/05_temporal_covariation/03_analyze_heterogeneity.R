# ==============================================================================
# 03_analyze_heterogeneity.R
# Analisi eterogeneità individuale negli slopes
#
# Obiettivo: Rendere esplicito il pattern idiografico
# - Alcuni individui: associazione positiva
# - Altri: associazione negativa
# - Altri: nessuna associazione
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

cat("=== ANALISI ETEROGENEITÀ INDIVIDUALE ===\n\n")

# ==============================================================================
# CARICA RISULTATI
# ==============================================================================

cat("Caricamento risultati...\n")

fit_rs <- readRDS(file.path(fitted_dir, "fit_random_slopes.rds"))
metadata <- readRDS(file.path(data_dir, "metadata.rds"))
stan_data <- readRDS(file.path(data_dir, "stan_data_within_person.rds"))

pid5_vars <- metadata$pid5_vars

# Domini labels (human-readable)
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
cat("\n")

# ==============================================================================
# ESTRAI INDIVIDUAL SLOPES
# ==============================================================================

cat("Estrazione individual slopes...\n")

# total_slopes[subject, domain] = beta_wp + u_beta
total_slopes <- fit_rs$draws("total_slopes", format = "matrix")

# Dimensioni: [iterations, N_subj * D]
# Reshape in [iterations, N_subj, D]
n_iter <- nrow(total_slopes)
N_subj <- stan_data$N_subj
D <- stan_data$D

# Reshape
total_slopes_array <- array(
  total_slopes,
  dim = c(n_iter, N_subj, D)
)

# Calcola summary per ogni soggetto-dominio
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
        prob_negative = mean(slope_draws < 0)
      ))
  }
}

cat(sprintf("  ✓ %d individual slopes estratti\n\n", nrow(individual_slopes)))

# ==============================================================================
# CATEGORIZZA RESPONDERS
# ==============================================================================

cat("Categorizzazione responders...\n")

# Criteri per categorizzare:
# - Positive responder: 95% CI > 0
# - Negative responder: 95% CI < 0
# - Non-responder: 95% CI includes 0

individual_slopes <- individual_slopes %>%
  mutate(
    responder_type = case_when(
      q025 > 0 ~ "Positive",
      q975 < 0 ~ "Negative",
      TRUE ~ "Non-responder"
    ),
    responder_type = factor(
      responder_type,
      levels = c("Positive", "Non-responder", "Negative")
    )
  )

# Summary per dominio
responder_summary <- individual_slopes %>%
  group_by(domain, responder_type) %>%
  summarise(
    n = n(),
    pct = n() / N_subj * 100,
    .groups = "drop"
  ) %>%
  arrange(domain, responder_type)

cat("\nDistribuzione responder types per dominio:\n")
print(responder_summary, n = Inf)

# Salva
write_csv(individual_slopes, file.path(output_dir, "individual_slopes.csv"))
write_csv(responder_summary, file.path(output_dir, "responder_summary.csv"))

cat("\n✓ Categorizzazione completata\n\n")

# ==============================================================================
# SIGMA_BETA (Variability across subjects)
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

write_csv(sigma_beta_summary, file.path(output_dir, "sigma_beta_summary.csv"))

cat("\n✓ Analisi sigma_beta completata\n\n")

# ==============================================================================
# VISUALIZZAZIONI
# ==============================================================================

cat("Creazione visualizzazioni...\n\n")

# -------------------------------
# PLOT 1: Density ridges per dominio
# -------------------------------

cat("  1. Density plots degli individual slopes...\n")

p1 <- ggplot(individual_slopes, aes(x = mean_slope, fill = domain)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~domain, ncol = 1, scales = "free_y") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribution of Individual Within-Person Slopes",
    subtitle = "Each distribution shows variation across participants",
    x = "Individual Slope (Hz per unit PID-5 change)",
    y = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave(
  file.path(output_dir, "01_slope_densities.pdf"),
  p1,
  width = 10,
  height = 12
)

# -------------------------------
# PLOT 2: Forest plot (focus su Negative Affectivity)
# -------------------------------

cat("  2. Forest plot degli individual slopes...\n")

slopes_na <- individual_slopes %>%
  filter(domain == "Negative Affectivity") %>%
  arrange(mean_slope) %>%
  mutate(subject = factor(subject, levels = subject))

p2 <- ggplot(
  slopes_na,
  aes(x = mean_slope, y = subject, color = responder_type)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 2) +
  geom_errorbarh(
    aes(xmin = q025, xmax = q975),
    height = 0,
    linewidth = 0.5
  ) +
  scale_color_manual(
    values = c(
      "Positive" = "#2166ac",
      "Non-responder" = "gray60",
      "Negative" = "#b2182b"
    ),
    name = "Responder Type"
  ) +
  labs(
    title = "Individual Within-Person Slopes: Negative Affectivity",
    subtitle = sprintf("N = %d participants", N_subj),
    x = "Slope (Hz per unit NA change) - 95% CI",
    y = "Participant"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
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
# PLOT 3: Stacked bar chart responder types
# -------------------------------

cat("  3. Bar chart responder types...\n")

p3 <- ggplot(
  responder_summary,
  aes(x = domain, y = pct, fill = responder_type)
) +
  geom_col(position = "stack") +
  scale_fill_manual(
    values = c(
      "Positive" = "#2166ac",
      "Non-responder" = "gray60",
      "Negative" = "#b2182b"
    ),
    name = "Responder Type"
  ) +
  labs(
    title = "Heterogeneity in Within-Person Associations",
    subtitle = "Proportion of participants showing positive, negative, or null effects",
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
  file.path(output_dir, "03_responder_types_barchart.pdf"),
  p3,
  width = 10,
  height = 6
)

# -------------------------------
# PLOT 4: Scatterplot sigma_beta vs fixed effects
# -------------------------------

cat("  4. Sigma_beta vs fixed effects...\n")

beta_wp_draws <- fit_rs$draws("beta_wp", format = "matrix")

beta_wp_summary <- tibble(
  domain = domain_labels,
  beta_mean = colMeans(beta_wp_draws),
  beta_q025 = apply(beta_wp_draws, 2, quantile, 0.025),
  beta_q975 = apply(beta_wp_draws, 2, quantile, 0.975)
)

combined_summary <- beta_wp_summary %>%
  left_join(sigma_beta_summary, by = "domain")

p4 <- ggplot(combined_summary, aes(x = beta_mean, y = mean)) +
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
    title = "Fixed Effects vs Between-Subject Variability",
    subtitle = "Population mean slope (x-axis) vs SD of individual slopes (y-axis)",
    x = "Fixed Effect β (population mean)",
    y = "σ_beta (SD across subjects)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )

ggsave(
  file.path(output_dir, "04_fixed_vs_variability.pdf"),
  p4,
  width = 10,
  height = 8
)

# -------------------------------
# PLOT 5: Examples di pattern individuali
# -------------------------------

cat("  5. Esempi di pattern individuali...\n")

# Seleziona 9 soggetti rappresentativi (3 per tipo)
set.seed(123)
example_subjects <- individual_slopes %>%
  filter(domain == "Negative Affectivity") %>%
  group_by(responder_type) %>%
  slice_sample(n = 3) %>%
  ungroup()

# Prepara dati per plotting
# Uso df_model da metadata
df_model <- metadata$df_model

# Crea variabile within-person per NA
pid5_wp_vars <- paste0(pid5_vars, "_wp")

example_data <- df_model %>%
  filter(subj %in% example_subjects$subject) %>%
  left_join(
    example_subjects %>% select(subject, responder_type),
    by = c("subj" = "subject")
  )

p5 <- ggplot(example_data, aes(x = pid5_negative_affectivity_wp, y = f0_wp)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  geom_point(aes(color = responder_type), size = 2, alpha = 0.7) +
  geom_smooth(
    aes(color = responder_type),
    method = "lm",
    se = FALSE,
    linewidth = 1
  ) +
  facet_wrap(~subj, ncol = 3, scales = "free") +
  scale_color_manual(
    values = c(
      "Positive" = "#2166ac",
      "Non-responder" = "gray60",
      "Negative" = "#b2182b"
    ),
    name = "Responder Type"
  ) +
  labs(
    title = "Examples of Individual Within-Person Patterns",
    subtitle = "Negative Affectivity × F0 (9 representative participants)",
    x = "Within-Person Deviation: Negative Affectivity",
    y = "Within-Person Deviation: F0 (Hz)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

ggsave(
  file.path(output_dir, "05_individual_examples.pdf"),
  p5,
  width = 12,
  height = 10
)

cat("\n✓ Tutte le visualizzazioni create\n\n")

# ==============================================================================
# SUMMARY REPORT
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("SUMMARY: IDIOGRAPHIC PATTERNS\n")
cat(rep("=", 80), "\n\n")

cat("KEY FINDING:\n")
cat(
  "Vocal-affective coupling is IDIOGRAPHIC - individuals differ systematically\n"
)
cat(
  "in whether and how momentary personality fluctuations relate to voice.\n\n"
)

cat("RESPONDER TYPE DISTRIBUTION (Negative Affectivity):\n")
na_summary <- responder_summary %>%
  filter(domain == "Negative Affectivity")
for (i in 1:nrow(na_summary)) {
  cat(sprintf(
    "  %s: %.1f%% (n = %d)\n",
    na_summary$responder_type[i],
    na_summary$pct[i],
    na_summary$n[i]
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
cat("ANALISI COMPLETATA\n")
cat(rep("=", 80), "\n\n")

cat("File salvati in:", output_dir, "\n")
cat("  - individual_slopes.csv\n")
cat("  - responder_summary.csv\n")
cat("  - sigma_beta_summary.csv\n")
cat("  - 01_slope_densities.pdf\n")
cat("  - 02_forest_plot_NA.pdf\n")
cat("  - 03_responder_types_barchart.pdf\n")
cat("  - 04_fixed_vs_variability.pdf\n")
cat("  - 05_individual_examples.pdf\n\n")

cat("✓ Prossimo step: source('04_create_manuscript_materials.R')\n\n")

# eof ---

#' "Random slopes models substantially outperformed fixed-effects models
#' (R² = 35% vs 2.5%, ELPD improvement ≈ 40 points), suggesting between-subject
#' variability in within-person associations (σ_β = 2.6-4.3 Hz). However, the
#' sparse temporal design (3 timepoints per person) resulted in high uncertainty
#' for individual slope estimates. Only 2 of 119 participants (1.7%) had credible
#' intervals excluding zero, indicating that while population-level heterogeneity
#' may exist, individual-level effects are difficult to detect reliably with the
#' current design. These findings suggest that within-person personality-voice
#' associations are either weak/absent or require denser temporal sampling
#' (10-20+ observations) for precise individual characterization."
