# ==============================================================================
# 05_differential_effects_nne.R
# Analisi pattern differenziali per NNE: quali domini mostrano moderazioni?
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

fit <- readRDS(here("stan", "NNE", "nne_mean_pid5_moderation.rds"))
bundle <- readRDS(here("results", "NNE", "stan_bundle_nne_pid5.rds"))
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
sigma_y <- median(as.numeric(draws[,, "sigma_y"]))

cat("\n=== DIFFERENTIAL MODERATION EFFECTS ANALYSIS (NNE) ===\n")
cat("Residual SD (sigma_y) =", round(sigma_y, 2), "dB\n\n")

# ==============================================================================
# PARTE 1: DIRECTION CERTAINTY
# ==============================================================================

cat("=== PART 1: DIRECTION CERTAINTY ===\n\n")

# Helper functions
extract_metrics <- function(draws_param) {
  tibble(
    median = median(draws_param),
    mean = mean(draws_param),
    mad = mad(draws_param),
    ci_lower = quantile(draws_param, 0.025),
    ci_upper = quantile(draws_param, 0.975),
    pd = max(mean(draws_param > 0), mean(draws_param < 0)),
    p_positive = mean(draws_param > 0),
    p_negative = mean(draws_param < 0),
    snr = abs(median(draws_param)) / mad(draws_param)
  )
}

# Analizza parametri
direction_table <- tibble()

for (d in 1:5) {
  # Stress moderation
  g1 <- as.numeric(draws[,, paste0("g1[", d, "]")])
  metrics_g1 <- extract_metrics(g1)

  direction_table <- bind_rows(
    direction_table,
    metrics_g1 %>%
      mutate(
        domain = pid5_labels[d],
        parameter = "Stress (γ₁)"
      )
  )

  # Recovery moderation
  g2 <- as.numeric(draws[,, paste0("g2[", d, "]")])
  metrics_g2 <- extract_metrics(g2)

  direction_table <- bind_rows(
    direction_table,
    metrics_g2 %>%
      mutate(
        domain = pid5_labels[d],
        parameter = "Recovery (γ₂)"
      )
  )
}

# Classificazione
direction_summary <- direction_table %>%
  mutate(
    direction = ifelse(median > 0, "positive", "negative"),
    direction_certainty = case_when(
      pd > 0.95 & abs(median) > 0.5 ~ "Strong",
      pd > 0.90 & abs(median) > 0.3 ~ "Moderate",
      pd > 0.80 ~ "Suggestive",
      TRUE ~ "Weak"
    ),
    signal_quality = case_when(
      snr > 1.5 ~ "Clear signal",
      snr > 1.0 ~ "Detectable signal",
      snr > 0.5 ~ "Weak signal",
      TRUE ~ "Noise-dominated"
    ),
    interpretation = paste0(
      direction_certainty,
      " ",
      direction,
      " (",
      signal_quality,
      ")"
    )
  ) %>%
  select(
    domain,
    parameter,
    median,
    ci_lower,
    ci_upper,
    pd,
    snr,
    direction_certainty,
    signal_quality,
    interpretation
  )

print(direction_summary, n = Inf)

write_csv(
  direction_summary,
  here("results", "NNE", "direction_certainty_table.csv")
)

# ==============================================================================
# PARTE 2: PAIRWISE CONTRASTS
# ==============================================================================

cat("\n=== PART 2: PAIRWISE CONTRASTS ===\n\n")

# Identifica effetti con Strong/Moderate certainty
standout_effects <- direction_summary %>%
  filter(direction_certainty %in% c("Strong", "Moderate")) %>%
  arrange(desc(pd))

cat("Domains with Strong/Moderate directional certainty:\n")
print(
  standout_effects %>%
    select(domain, parameter, median, pd, direction_certainty)
)

# Contrasti
contrast_data <- tibble()
g1_list <- list()
g2_list <- list()

for (d in 1:5) {
  g1_list[[d]] <- as.numeric(draws[,, paste0("g1[", d, "]")])
  g2_list[[d]] <- as.numeric(draws[,, paste0("g2[", d, "]")])
}

# Se ci sono effetti stress con Strong/Moderate, calcola contrasti
if (any(standout_effects$parameter == "Stress (γ₁)")) {
  stress_standout <- standout_effects %>%
    filter(parameter == "Stress (γ₁)") %>%
    pull(domain)

  if (length(stress_standout) > 0) {
    cat("\n--- Stress Contrasts ---\n")
    standout_idx <- which(pid5_labels == stress_standout[1])

    for (d in setdiff(1:5, standout_idx)) {
      diff <- g1_list[[standout_idx]] - g1_list[[d]]
      p_pos <- mean(diff > 0)
      cat(sprintf(
        "  P(%s > %s) = %.1f%%\n",
        stress_standout[1],
        pid5_labels[d],
        p_pos * 100
      ))

      contrast_data <- bind_rows(
        contrast_data,
        tibble(
          contrast = paste0(stress_standout[1], " - ", pid5_labels[d]),
          parameter = "Stress",
          difference = diff,
          p_positive = p_pos
        )
      )
    }
  }
}

# Recovery contrasts
if (any(standout_effects$parameter == "Recovery (γ₂)")) {
  recov_standout <- standout_effects %>%
    filter(parameter == "Recovery (γ₂)") %>%
    pull(domain)

  if (length(recov_standout) > 0) {
    cat("\n--- Recovery Contrasts ---\n")
    standout_idx <- which(pid5_labels == recov_standout[1])

    for (d in setdiff(1:5, standout_idx)) {
      diff <- g2_list[[standout_idx]] - g2_list[[d]]
      p_pos <- mean(diff > 0)
      cat(sprintf(
        "  P(%s > %s) = %.1f%%\n",
        recov_standout[1],
        pid5_labels[d],
        p_pos * 100
      ))

      contrast_data <- bind_rows(
        contrast_data,
        tibble(
          contrast = paste0(recov_standout[1], " - ", pid5_labels[d]),
          parameter = "Recovery",
          difference = diff,
          p_positive = p_pos
        )
      )
    }
  }
}

# ==============================================================================
# PARTE 3: THEORETICAL ALIGNMENT
# ==============================================================================

cat("\n=== PART 3: THEORETICAL ALIGNMENT (NNE) ===\n\n")

# Predizioni teoriche per NNE
# NNE più negativo = voce più pulita (meno rumore)
# NNE meno negativo = voce più soffiata (più rumore)
theoretical_predictions <- tribble(
  ~domain,
  ~parameter,
  ~predicted_direction,
  ~rationale,
  "Negative Affectivity",
  "Stress (γ₁)",
  "negative",
  "High NA → more tension → cleaner voice (more negative NNE)",
  "Negative Affectivity",
  "Recovery (γ₂)",
  "positive",
  "High NA → slow recovery → noisier voice (less negative NNE)",
  "Detachment",
  "Stress (γ₁)",
  "ambiguous",
  "Detachment may reduce tension or expression",
  "Detachment",
  "Recovery (γ₂)",
  "positive",
  "High Det → impaired recovery",
  "Antagonism",
  "Stress (γ₁)",
  "ambiguous",
  "Complex - could suppress or amplify",
  "Antagonism",
  "Recovery (γ₂)",
  "positive",
  "High Ant → slower recovery",
  "Disinhibition",
  "Stress (γ₁)",
  "positive",
  "High Dis → less control → noisier (less negative)",
  "Disinhibition",
  "Recovery (γ₂)",
  "positive",
  "High Dis → impaired recovery",
  "Psychoticism",
  "Stress (γ₁)",
  "ambiguous",
  "Unclear prediction",
  "Psychoticism",
  "Recovery (γ₂)",
  "positive",
  "High Psy → impaired recovery"
)

alignment_table <- direction_summary %>%
  left_join(theoretical_predictions, by = c("domain", "parameter")) %>%
  mutate(
    observed_direction = ifelse(median > 0, "positive", "negative"),
    alignment = case_when(
      predicted_direction == "ambiguous" ~ "No clear prediction",
      observed_direction == predicted_direction & pd > 0.80 ~
        "Aligned (certain)",
      observed_direction == predicted_direction & pd <= 0.80 ~
        "Aligned (uncertain)",
      observed_direction != predicted_direction & pd > 0.80 ~
        "Contrary (certain)",
      observed_direction != predicted_direction & pd <= 0.80 ~
        "Contrary (uncertain)",
      TRUE ~ "Unclear"
    )
  ) %>%
  select(
    domain,
    parameter,
    predicted_direction,
    observed_direction,
    pd,
    alignment,
    rationale
  )

cat("Theoretical Alignment:\n")
print(
  alignment_table %>%
    filter(alignment %in% c("Aligned (certain)", "Contrary (certain)")),
  n = Inf
)

write_csv(
  alignment_table,
  here("results", "NNE", "theoretical_alignment_table.csv")
)

# ==============================================================================
# VISUALIZZAZIONE 1: Direction Certainty Plot
# ==============================================================================

direction_plot_data <- direction_summary %>%
  mutate(
    domain = factor(domain, levels = rev(pid5_labels)),
    highlight = direction_certainty %in% c("Strong", "Moderate")
  )

fig_direction <- ggplot(
  direction_plot_data,
  aes(x = median, y = domain)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", size = 1) +
  geom_linerange(
    aes(xmin = ci_lower, xmax = ci_upper, color = highlight),
    size = 1.5,
    alpha = 0.8
  ) +
  geom_point(aes(fill = highlight), size = 4, shape = 21, stroke = 0.5) +
  geom_text(
    aes(
      label = sprintf("PD=%.2f", pd),
      x = ifelse(median > 0, ci_upper + 0.3, ci_lower - 0.3),
      hjust = ifelse(median > 0, 0, 1)
    ),
    size = 3,
    color = "gray30"
  ) +
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
    x = "Moderation Effect (dB per SD)",
    y = NULL,
    title = "Direction Certainty of NNE Moderation Effects",
    subtitle = "Red = Strong/Moderate directional certainty (PD > 0.90); 95% credible intervals"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

print(fig_direction)

ggsave(
  filename = here("results", "NNE", "figure_direction_certainty.png"),
  plot = fig_direction,
  width = 12,
  height = 6,
  dpi = 300
)

# ==============================================================================
# VISUALIZZAZIONE 2: SNR
# ==============================================================================

snr_plot_data <- direction_summary %>%
  mutate(
    domain = factor(domain, levels = rev(pid5_labels)),
    strong_signal = snr > 1.0
  )

fig_snr <- ggplot(
  snr_plot_data,
  aes(x = snr, y = domain, color = strong_signal)
) +
  geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 1.5, linetype = "dotted", alpha = 0.5) +
  geom_segment(aes(x = 0, xend = snr, y = domain, yend = domain), size = 1.5) +
  geom_point(size = 4) +
  facet_wrap(~parameter) +
  scale_color_manual(
    values = c("TRUE" = "#d62728", "FALSE" = "gray60"),
    guide = "none"
  ) +
  labs(
    x = "Signal-to-Noise Ratio (|median| / MAD)",
    y = NULL,
    title = "NNE: Effect Size Relative to Uncertainty",
    subtitle = "SNR > 1.0 (dashed) = detectable; >1.5 (dotted) = clear signal"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

print(fig_snr)

ggsave(
  filename = here("results", "NNE", "figure_snr.png"),
  plot = fig_snr,
  width = 12,
  height = 6,
  dpi = 300
)

# ==============================================================================
# VISUALIZZAZIONE 3: Pairwise Contrasts (se esistono)
# ==============================================================================

if (nrow(contrast_data) > 0) {
  contrast_summary <- contrast_data %>%
    group_by(contrast, parameter) %>%
    summarise(
      median_diff = median(difference),
      ci_lower = quantile(difference, 0.025),
      ci_upper = quantile(difference, 0.975),
      p_greater = unique(p_positive),
      .groups = "drop"
    )

  fig_contrasts <- contrast_data %>%
    mutate(
      contrast = factor(contrast, levels = unique(contrast_summary$contrast))
    ) %>%
    ggplot(aes(x = difference, y = contrast)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    stat_halfeye(
      .width = 0.95,
      fill = "#1f77b4",
      alpha = 0.7,
      normalize = "xy"
    ) +
    geom_text(
      data = contrast_summary,
      aes(
        x = ci_upper + 0.3,
        label = sprintf("P>0: %.1f%%", p_greater * 100)
      ),
      hjust = 0,
      size = 3
    ) +
    facet_wrap(~parameter, scales = "free_y") +
    labs(
      x = "Difference in moderation effect (dB)",
      y = NULL,
      title = "Pairwise Contrasts: NNE Stand-out Effects"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 11)
    )

  print(fig_contrasts)

  ggsave(
    filename = here("results", "NNE", "figure_pairwise_contrasts.png"),
    plot = fig_contrasts,
    width = 12,
    height = max(4, nrow(contrast_summary) * 0.5),
    dpi = 300
  )
}

# ==============================================================================
# SUMMARY REPORT
# ==============================================================================

cat("\n=== ANALYSIS COMPLETE (NNE) ===\n\n")

cat("Files created:\n")
cat("  1. results/NNE/direction_certainty_table.csv\n")
cat("  2. results/NNE/theoretical_alignment_table.csv\n")
cat("  3. results/NNE/figure_direction_certainty.png\n")
cat("  4. results/NNE/figure_snr.png\n")
if (nrow(contrast_data) > 0) {
  cat("  5. results/NNE/figure_pairwise_contrasts.png\n")
}

cat("\n=== KEY FINDINGS (NNE) ===\n\n")

cat("1. STRONG/MODERATE DIRECTIONAL EFFECTS:\n")
strong_dir <- direction_summary %>%
  filter(direction_certainty %in% c("Strong", "Moderate")) %>%
  arrange(desc(pd))

if (nrow(strong_dir) > 0) {
  for (i in 1:nrow(strong_dir)) {
    cat(sprintf(
      "   %s - %s: %.2f dB (PD = %.3f) [%s]\n",
      strong_dir$domain[i],
      strong_dir$parameter[i],
      strong_dir$median[i],
      strong_dir$pd[i],
      strong_dir$direction_certainty[i]
    ))
  }
} else {
  cat("   None detected (all effects show weak directional certainty)\n")
}

cat("\n2. THEORETICAL ALIGNMENT:\n")
aligned <- alignment_table %>%
  filter(alignment == "Aligned (certain)")

if (nrow(aligned) > 0) {
  cat("   Aligned with predictions:\n")
  for (i in 1:nrow(aligned)) {
    cat(sprintf(
      "     - %s %s: %s\n",
      aligned$domain[i],
      aligned$parameter[i],
      aligned$rationale[i]
    ))
  }
} else {
  cat("   No effects strongly aligned with predictions\n")
}

contrary <- alignment_table %>%
  filter(alignment == "Contrary (certain)")

if (nrow(contrary) > 0) {
  cat("   Contrary to predictions:\n")
  for (i in 1:nrow(contrary)) {
    cat(sprintf(
      "     - %s %s: predicted %s, observed %s (PD = %.2f)\n",
      contrary$domain[i],
      contrary$parameter[i],
      contrary$predicted_direction[i],
      contrary$observed_direction[i],
      contrary$pd[i]
    ))
  }
}

cat("\n3. INTERPRETATION:\n")
cat("   NNE (Normalized Noise Energy):\n")
cat("     - More negative = less glottal noise = 'cleaner' voice\n")
cat("     - Less negative = more glottal noise = 'breathier' voice\n")
cat("     - Unlike F0, NNE effects may be less trait-modulated\n")
cat("     - Check if main effects (b1, b2) are stronger than moderations\n")

cat("\n=== NEXT STEPS ===\n")
cat("  1. Compare with F0 results (are moderation patterns similar?)\n")
cat("  2. If NNE shows weak moderation → discuss in paper as dissociation\n")
cat("  3. Create manuscript table combining F0 + NNE results\n")

# eof ---
