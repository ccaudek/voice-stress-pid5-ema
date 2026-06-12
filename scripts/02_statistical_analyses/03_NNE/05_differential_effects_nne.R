# ==============================================================================
# 05_differential_effects_nne.R
# Differential moderation patterns: PID-5 domains x stress/recovery for NNE.
# Aligned with the F0 pipeline: central 89% credible intervals throughout;
# probability of direction (pd) and SNR are reported descriptively, with NO
# binary decision thresholds (no PD > .95, no "Strong/Moderate/Weak"
# classification, no ROPE, no Bayes factors). Conclusions follow the overall
# pattern of directional evidence across parameters, not single estimates.
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

# Central 89% credible intervals, following McElreath-style reporting.
# The width is an explicitly arbitrary descriptive choice; the interval
# summarizes magnitude uncertainty and is NOT a significance test.
cri_level <- 0.89
cri_probs <- c((1 - cri_level) / 2, 1 - (1 - cri_level) / 2) # 0.055, 0.945
cri_label <- paste0(round(cri_level * 100), "%")

# Check: should be 0.055 and 0.945
print(cri_probs)

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
sigma_y <- median(as.numeric(draws[, , "sigma_y"]))

out_dir <- here("results", "NNE")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n=== DIFFERENTIAL MODERATION EFFECTS ANALYSIS (NNE) ===\n")
cat("Credible intervals:", cri_label, "central (equal-tailed) intervals\n")
cat("pd and SNR are reported descriptively, without decision thresholds\n")
cat("Residual SD (sigma_y) =", round(sigma_y, 2), "dB\n\n")
# Residual SD (sigma_y) = 1.98 dB

# ==============================================================================
# PART 1: DIRECTIONAL POSTERIOR SUMMARIES
# ==============================================================================

cat("=== PART 1: DIRECTIONAL POSTERIOR SUMMARIES ===\n\n")

pd_fun <- function(x) max(mean(x > 0), mean(x < 0))

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

# NB: parameter labels kept in ASCII ("gamma1"/"gamma2") for consistency with
# the F0 pipeline and with the combined-table script.
direction_table <- tibble()
for (d in seq_along(pid5_labels)) {
  g1 <- as.numeric(draws[, , paste0("g1[", d, "]")])
  direction_table <- bind_rows(
    direction_table,
    extract_metrics(g1) %>%
      mutate(domain = pid5_labels[d], parameter = "Stress (gamma1)")
  )

  g2 <- as.numeric(draws[, , paste0("g2[", d, "]")])
  direction_table <- bind_rows(
    direction_table,
    extract_metrics(g2) %>%
      mutate(domain = pid5_labels[d], parameter = "Recovery (gamma2)")
  )
}

# Descriptive summary only: no PD-based decision categories
direction_summary <- direction_table %>%
  mutate(
    direction = ifelse(median > 0, "positive", "negative"),
    pd_label = sprintf("pd = %.2f", pd),
    interval_label = sprintf(
      "%.2f [%.2f, %.2f]",
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
  here(out_dir, "tables", "direction_posterior_summary_89cri.csv")
)

# ==============================================================================
# PART 2: PAIRWISE CONTRASTS (descriptive)
# ==============================================================================

cat("\n=== PART 2: PAIRWISE CONTRASTS ===\n\n")

# Moderation effects ordered by probability of direction (descriptive only)
cat("Moderation effects ordered by probability of direction:\n")
print(
  direction_summary %>%
    arrange(parameter, desc(pd)) %>%
    select(domain, parameter, median, ci_lower, ci_upper, pd),
  n = Inf
)
# domain               parameter          median ci_lower ci_upper    pd
# 1 Psychoticism         Recovery (gamma2)  0.883    0.0667    1.68  0.959
# 2 Antagonism           Recovery (gamma2) -0.432   -1.18      0.329 0.819
# 3 Negative Affectivity Recovery (gamma2) -0.393   -1.19      0.415 0.783
# 4 Disinhibition        Recovery (gamma2) -0.399   -1.34      0.564 0.75 
# 5 Detachment           Recovery (gamma2)  0.217   -0.606     1.04  0.664
# 6 Negative Affectivity Stress (gamma1)   -0.465   -1.27      0.344 0.822
# 7 Detachment           Stress (gamma1)    0.304   -0.518     1.12  0.720
# 8 Disinhibition        Stress (gamma1)    0.322   -0.644     1.27  0.705
# 9 Psychoticism         Stress (gamma1)   -0.0248  -0.828     0.789 0.520
# 10 Antagonism           Stress (gamma1)   -0.0163  -0.757     0.740 0.513

g1_list <- lapply(1:5, function(d) as.numeric(draws[, , paste0("g1[", d, "]")]))
g2_list <- lapply(1:5, function(d) as.numeric(draws[, , paste0("g2[", d, "]")]))

contrast_data <- tibble()

# Reference domain in each phase = the one with the highest pd.
# This is a purely descriptive choice for displaying pairwise comparisons;
# it does not imply a decision that the effect is "present".
ref_stress <- direction_summary %>%
  filter(parameter == "Stress (gamma1)") %>%
  arrange(desc(pd)) %>%
  slice(1) %>%
  pull(domain)

ref_recov <- direction_summary %>%
  filter(parameter == "Recovery (gamma2)") %>%
  arrange(desc(pd)) %>%
  slice(1) %>%
  pull(domain)

cat(sprintf(
  "\n--- Stress contrasts: P(%s > each other domain) ---\n",
  ref_stress
))
ref_idx <- which(pid5_labels == ref_stress)
for (d in setdiff(1:5, ref_idx)) {
  diff <- g1_list[[ref_idx]] - g1_list[[d]]
  cat(sprintf(
    "  P(%s > %s) = %.1f%%\n",
    ref_stress,
    pid5_labels[d],
    mean(diff > 0) * 100
  ))
  contrast_data <- bind_rows(
    contrast_data,
    tibble(
      contrast = paste0(ref_stress, " - ", pid5_labels[d]),
      parameter = "Stress",
      difference = diff,
      p_positive = mean(diff > 0)
    )
  )
}
# P(Negative Affectivity > Detachment) = 15.3%
# P(Negative Affectivity > Antagonism) = 22.8%
# P(Negative Affectivity > Disinhibition) = 19.1%
# P(Negative Affectivity > Psychoticism) = 29.9%

cat(sprintf(
  "\n--- Recovery contrasts: P(%s > each other domain) ---\n",
  ref_recov
))
ref_idx <- which(pid5_labels == ref_recov)
for (d in setdiff(1:5, ref_idx)) {
  diff <- g2_list[[ref_idx]] - g2_list[[d]]
  cat(sprintf(
    "  P(%s > %s) = %.1f%%\n",
    ref_recov,
    pid5_labels[d],
    mean(diff > 0) * 100
  ))
  contrast_data <- bind_rows(
    contrast_data,
    tibble(
      contrast = paste0(ref_recov, " - ", pid5_labels[d]),
      parameter = "Recovery",
      difference = diff,
      p_positive = mean(diff > 0)
    )
  )
}
# P(Psychoticism > Negative Affectivity) = 93.9%
# P(Psychoticism > Detachment) = 80.9%
# P(Psychoticism > Antagonism) = 95.4%
# P(Psychoticism > Disinhibition) = 93.5%

# ==============================================================================
# PART 3: MAGNITUDE UNCERTAINTY (descriptive magnitude probabilities)
# ==============================================================================

cat("\n=== PART 3: MAGNITUDE UNCERTAINTY ===\n\n")

# For the two highest-pd effects (one per phase), report direct posterior
# probabilities that the absolute effect exceeds substantive dB thresholds.
# These quantify magnitude uncertainty without dichotomizing.
mag_targets <- list(
  list(label = ref_stress, draws = g1_list[[which(pid5_labels == ref_stress)]], phase = "Stress"),
  list(label = ref_recov, draws = g2_list[[which(pid5_labels == ref_recov)]], phase = "Recovery")
)

for (t in mag_targets) {
  cat(sprintf("\n%s - %s moderation:\n", t$label, t$phase))
  cat(sprintf(
    "  median = %.2f dB, %s CrI [%.2f, %.2f], pd = %.3f\n",
    median(t$draws),
    cri_label,
    quantile(t$draws, cri_probs[1]),
    quantile(t$draws, cri_probs[2]),
    pd_fun(t$draws)
  ))
  cat("  Magnitude probabilities:\n")
  cat(sprintf("    P(|effect| > 0.5 dB) = %.1f%%\n", mean(abs(t$draws) > 0.5) * 100))
  cat(sprintf("    P(|effect| > 1.0 dB) = %.1f%%\n", mean(abs(t$draws) > 1.0) * 100))
  cat(sprintf("    P(|effect| > 1.5 dB) = %.1f%%\n", mean(abs(t$draws) > 1.5) * 100))
}
# Negative Affectivity - Stress moderation:
#   median = -0.46 dB, 89% CrI [-1.27, 0.34], pd = 0.822
# Magnitude probabilities:
#   P(|effect| > 0.5 dB) = 49.9%
# P(|effect| > 1.0 dB) = 14.8%
# P(|effect| > 1.5 dB) = 2.0%
# 
# Psychoticism - Recovery moderation:
#   median = 0.88 dB, 89% CrI [0.07, 1.68], pd = 0.959
# Magnitude probabilities:
#   P(|effect| > 0.5 dB) = 77.7%
# P(|effect| > 1.0 dB) = 40.8%
# P(|effect| > 1.5 dB) = 10.8%

# ==============================================================================
# PART 4: THEORETICAL ALIGNMENT (direction only, no pd threshold)
# ==============================================================================

cat("\n=== PART 4: THEORETICAL ALIGNMENT (NNE) ===\n\n")

# Predizioni teoriche per NNE
# NNE più negativo = voce più pulita (meno rumore)
# NNE meno negativo = voce più soffiata (più rumore)
theoretical_predictions <- tribble(
  ~domain,
  ~parameter,
  ~predicted_direction,
  ~rationale,
  "Negative Affectivity",
  "Stress (gamma1)",
  "negative",
  "High NA -> more tension -> cleaner voice (more negative NNE)",
  "Negative Affectivity",
  "Recovery (gamma2)",
  "positive",
  "High NA -> slow recovery -> noisier voice (less negative NNE)",
  "Detachment",
  "Stress (gamma1)",
  "ambiguous",
  "Detachment may reduce tension or expression",
  "Detachment",
  "Recovery (gamma2)",
  "positive",
  "High Det -> impaired recovery",
  "Antagonism",
  "Stress (gamma1)",
  "ambiguous",
  "Complex - could suppress or amplify",
  "Antagonism",
  "Recovery (gamma2)",
  "positive",
  "High Ant -> slower recovery",
  "Disinhibition",
  "Stress (gamma1)",
  "positive",
  "High Dis -> less control -> noisier (less negative)",
  "Disinhibition",
  "Recovery (gamma2)",
  "positive",
  "High Dis -> impaired recovery",
  "Psychoticism",
  "Stress (gamma1)",
  "ambiguous",
  "Unclear prediction",
  "Psychoticism",
  "Recovery (gamma2)",
  "positive",
  "High Psy -> impaired recovery"
)

# Alignment is judged on direction only; pd is reported alongside but is not
# used to gate "aligned vs not".
alignment_table <- direction_summary %>%
  left_join(theoretical_predictions, by = c("domain", "parameter")) %>%
  mutate(
    observed_direction = ifelse(median > 0, "positive", "negative"),
    alignment = case_when(
      predicted_direction == "ambiguous" ~ "No clear prediction",
      observed_direction == predicted_direction ~
        "Same direction as prediction",
      observed_direction != predicted_direction ~
        "Opposite direction from prediction",
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
  here(out_dir, "tables", "theoretical_alignment_table_descriptive_89cri.csv")
)

# ==============================================================================
# VISUALIZATION 1: Posterior medians and 89% intervals (no threshold colouring)
# ==============================================================================

direction_plot_data <- direction_summary %>%
  mutate(domain = factor(domain, levels = rev(pid5_labels)))

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
    linewidth = 1.5,
    color = "gray40",
    alpha = 0.9
  ) +
  geom_point(size = 4, color = "black") +
  geom_text(
    aes(
      label = sprintf("pd=%.2f", pd),
      x = ifelse(median > 0, ci_upper + 0.3, ci_lower - 0.3),
      hjust = ifelse(median > 0, 0, 1)
    ),
    size = 3,
    color = "gray30"
  ) +
  facet_wrap(~parameter) +
  labs(
    x = "Moderation Effect (dB per SD)",
    y = NULL,
    title = "NNE Moderation Effects",
    subtitle = paste0(
      "Posterior medians with ",
      cri_label,
      " credible intervals; pd shown descriptively"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

print(fig_direction)

ggsave(
  filename = here(out_dir, "figures", "figure_moderation_effects.png"),
  plot = fig_direction,
  width = 12,
  height = 6,
  dpi = 300
)

# ==============================================================================
# VISUALIZATION 2: SNR (descriptive; reference lines carry no decision meaning)
# ==============================================================================

snr_plot_data <- direction_summary %>%
  mutate(domain = factor(domain, levels = rev(pid5_labels)))

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
    title = "NNE: Effect Magnitude Relative to Posterior Uncertainty",
    subtitle = "Reference lines at SNR = 1.0 and 1.5 are descriptive only"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

print(fig_snr)

ggsave(
  filename = here(out_dir, "figures", "figure_snr_descriptive.png"),
  plot = fig_snr,
  width = 12,
  height = 6,
  dpi = 300
)

# ==============================================================================
# VISUALIZATION 3: Pairwise contrasts
# ==============================================================================

if (nrow(contrast_data) > 0) {
  contrast_summary <- contrast_data %>%
    group_by(contrast, parameter) %>%
    summarise(
      median_diff = median(difference),
      ci_lower = quantile(difference, cri_probs[1]),
      ci_upper = quantile(difference, cri_probs[2]),
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
      .width = cri_level,
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
      title = "Pairwise Contrasts: NNE Moderation Effects",
      subtitle = paste0("Intervals are ", cri_label, " credible intervals")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 11)
    )

  print(fig_contrasts)

  ggsave(
    filename = here(out_dir, "figures", "figure_pairwise_contrasts.png"),
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
cat("  1. results/NNE/tables/direction_posterior_summary_89cri.csv\n")
cat("  2. results/NNE/tables/theoretical_alignment_table_descriptive_89cri.csv\n")
cat("  3. results/NNE/figures/figure_moderation_effects.png\n")
cat("  4. results/NNE/figures/figure_snr_descriptive.png\n")
if (nrow(contrast_data) > 0) {
  cat("  5. results/NNE/figures/figure_pairwise_contrasts.png\n")
}

cat("\n=== KEY FINDINGS (NNE) ===\n\n")

cat("1. EFFECTS ORDERED BY PROBABILITY OF DIRECTION:\n")
top_dir <- direction_summary %>%
  arrange(desc(pd)) %>%
  slice_head(n = 5)
for (i in seq_len(nrow(top_dir))) {
  cat(sprintf(
    "   %s - %s: %.2f dB [%s CrI %.2f, %.2f], pd = %.3f\n",
    top_dir$domain[i],
    top_dir$parameter[i],
    top_dir$median[i],
    cri_label,
    top_dir$ci_lower[i],
    top_dir$ci_upper[i],
    top_dir$pd[i]
  ))
}
# Psychoticism - Recovery (gamma2): 0.88 dB [89% CrI 0.07, 1.68], pd = 0.959
# Negative Affectivity - Stress (gamma1): -0.46 dB [89% CrI -1.27, 0.34], pd = 0.822
# Antagonism - Recovery (gamma2): -0.43 dB [89% CrI -1.18, 0.33], pd = 0.819
# Negative Affectivity - Recovery (gamma2): -0.39 dB [89% CrI -1.19, 0.41], pd = 0.783
# Disinhibition - Recovery (gamma2): -0.40 dB [89% CrI -1.34, 0.56], pd = 0.750

cat("\n2. THEORETICAL ALIGNMENT (by direction):\n")
aligned <- alignment_table %>%
  filter(alignment == "Same direction as prediction")
if (nrow(aligned) > 0) {
  for (i in seq_len(nrow(aligned))) {
    cat(sprintf(
      "   Same direction: %s - %s (pd = %.2f)\n",
      aligned$domain[i],
      aligned$parameter[i],
      aligned$pd[i]
    ))
  }
}
# Same direction: Negative Affectivity - Stress (gamma1) (pd = 0.82)
# Same direction: Detachment - Recovery (gamma2) (pd = 0.66)
# Same direction: Disinhibition - Stress (gamma1) (pd = 0.70)
# Same direction: Psychoticism - Recovery (gamma2) (pd = 0.96)
contrary <- alignment_table %>%
  filter(alignment == "Opposite direction from prediction")
if (nrow(contrary) > 0) {
  for (i in seq_len(nrow(contrary))) {
    cat(sprintf(
      "   Opposite direction: %s - %s (predicted %s, observed %s, pd = %.2f)\n",
      contrary$domain[i],
      contrary$parameter[i],
      contrary$predicted_direction[i],
      contrary$observed_direction[i],
      contrary$pd[i]
    ))
  }
}
# Opposite direction: Negative Affectivity - Recovery (gamma2) (predicted positive, observed negative, pd = 0.78)
# Opposite direction: Antagonism - Recovery (gamma2) (predicted positive, observed negative, pd = 0.82)
# Opposite direction: Disinhibition - Recovery (gamma2) (predicted positive, observed negative, pd = 0.75)

cat("\n3. INTERPRETATION:\n")
cat("   NNE (Normalized Noise Energy):\n")
cat("     - More negative = less glottal noise = 'cleaner' voice\n")
cat("     - Less negative = more glottal noise = 'breathier' voice\n")
cat("   Read the pattern of directional evidence across domains and phases,\n")
cat("   not single estimates; intervals index magnitude uncertainty only.\n")

cat("\n=== NEXT STEPS ===\n")
cat("  1. Compare with F0 results (are moderation patterns similar?)\n")
cat("  2. If NNE shows weak directional evidence -> discuss as dissociation\n")
cat("  3. Create manuscript table combining F0 + NNE (update 06 to read the\n")
cat("     new *_89cri.csv files)\n")

# eof ---
