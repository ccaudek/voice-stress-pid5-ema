# ==============================================================================
# 05_populate_results.R
# Estrae i valori numerici dai modelli fittati e genera la sezione Risultati
# completa con tutti i placeholder popolati
# ==============================================================================

library(tidyverse)
library(rstan)
library(glue)
library(here)

# Load fitted models and analysis bundle
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
bundle <- readRDS(here("results", "stress", "analysis_bundle.rds"))

# ==============================================================================
# Helper Functions
# ==============================================================================

format_est <- function(x, digits = 2) {
  sprintf(paste0("%.", digits, "f"), x)
}

format_prob <- function(x, digits = 3) {
  sprintf(paste0("%.", digits, "f"), x)
}

extract_summary <- function(fit, param) {
  # Estrae i draw del parametro come vettore numerico
  draws_mat <- fit$draws(variables = param, format = "matrix")
  post <- as.numeric(draws_mat[, 1])

  list(
    median = median(post),
    mad = mad(post),
    lower = unname(quantile(post, 0.055)),
    upper = unname(quantile(post, 0.945)),
    prob_pos = mean(post > 0),
    prob_neg = mean(post < 0)
  )
}

# ==============================================================================
# Extract Model Estimates
# ==============================================================================

# F0 Fixed effects
f0_alpha <- extract_summary(fit_f0, "alpha")
f0_b1 <- extract_summary(fit_f0, "b1")
f0_b2 <- extract_summary(fit_f0, "b2")

# F0 Random effects SDs
f0_tau1 <- extract_summary(fit_f0, "tau[1]")
f0_tau2 <- extract_summary(fit_f0, "tau[2]")
f0_tau3 <- extract_summary(fit_f0, "tau[3]")

# F0 Residual SD
f0_sigma <- extract_summary(fit_f0, "sigma_y")

# NNE Fixed effects
nne_alpha <- extract_summary(fit_nne, "alpha")
nne_b1 <- extract_summary(fit_nne, "b1")
nne_b2 <- extract_summary(fit_nne, "b2")

# NNE Random effects SDs
nne_tau1 <- extract_summary(fit_nne, "tau[1]")
nne_tau2 <- extract_summary(fit_nne, "tau[2]")
nne_tau3 <- extract_summary(fit_nne, "tau[3]")

# NNE Residual SD
nne_sigma <- extract_summary(fit_nne, "sigma_y")

# ==============================================================================
# Descriptive Statistics (from bundle)
# ==============================================================================

desc_stats <- bundle$desc_stats

baseline_f0_mean <- desc_stats %>%
  dplyr::filter(timepoint == "baseline") %>%
  pull(f0_mean)
pre_f0_mean <- desc_stats %>%
  dplyr::filter(timepoint == "pre") %>%
  pull(f0_mean)
post_f0_mean <- desc_stats %>%
  dplyr::filter(timepoint == "post") %>%
  pull(f0_mean)

baseline_f0_sd <- desc_stats %>%
  dplyr::filter(timepoint == "baseline") %>%
  pull(f0_sd)
pre_f0_sd <- desc_stats %>% dplyr::filter(timepoint == "pre") %>% pull(f0_sd)
post_f0_sd <- desc_stats %>% dplyr::filter(timepoint == "post") %>% pull(f0_sd)

baseline_nne_mean <- desc_stats %>%
  filter(timepoint == "baseline") %>%
  pull(nne_mean)
pre_nne_mean <- desc_stats %>%
  dplyr::filter(timepoint == "pre") %>%
  pull(nne_mean)
post_nne_mean <- desc_stats %>%
  dplyr::filter(timepoint == "post") %>%
  pull(nne_mean)

baseline_nne_sd <- desc_stats %>%
  dplyr::filter(timepoint == "baseline") %>%
  pull(nne_sd)
pre_nne_sd <- desc_stats %>% dplyr::filter(timepoint == "pre") %>% pull(nne_sd)
post_nne_sd <- desc_stats %>%
  dplyr::filter(timepoint == "post") %>%
  pull(nne_sd)

# Calculate raw differences
diff_pre_baseline_f0 <- pre_f0_mean - baseline_f0_mean
diff_post_pre_f0 <- post_f0_mean - pre_f0_mean

diff_pre_baseline_nne <- pre_nne_mean - baseline_nne_mean
diff_post_pre_nne <- post_nne_mean - pre_nne_mean

# ==============================================================================
# Interpret β₂ Direction
# ==============================================================================

interpret_b2_f0 <- function(b2_summary) {
  if (b2_summary$prob_pos > 0.95) {
    "This suggests that F0 remained elevated even after exam completion, indicating sustained physiological arousal during the immediate post-exam period."
  } else if (
    abs(b2_summary$median) < 1 &&
      b2_summary$prob_pos < 0.75 &&
      b2_summary$prob_pos > 0.25
  ) {
    "This indicates that F0 plateaued after the exam, with minimal further change during the recovery phase."
  } else if (b2_summary$prob_neg > 0.75) {
    "This suggests a partial normalization of F0 following exam completion, though levels remained above baseline."
  } else {
    "The recovery effect was ambiguous, with considerable uncertainty in the direction of change."
  }
}

interpret_b2_nne <- function(b2_summary) {
  if (b2_summary$prob_pos > 0.95) {
    "This indicates a partial reversal of the stress-induced reduction, with NNE beginning to normalize following exam completion."
  } else if (
    abs(b2_summary$median) < 0.5 &&
      b2_summary$prob_pos < 0.75 &&
      b2_summary$prob_pos > 0.25
  ) {
    "This suggests that the stress-induced reduction in NNE persisted through the immediate post-exam period."
  } else if (b2_summary$prob_neg > 0.75) {
    "This indicates a continued reduction in glottal noise even after the exam."
  } else {
    "The recovery effect showed considerable uncertainty."
  }
}

b2_f0_interp <- interpret_b2_f0(f0_b2)
b2_nne_interp <- interpret_b2_nne(nne_b2)

b2_direction_f0 <- if (f0_b2$median > 0) "positive" else if (f0_b2$median < 0)
  "negative" else "near zero"
b2_direction_nne <- if (nne_b2$median > 0) "positive" else if (
  nne_b2$median < 0
)
  "negative" else "near zero"

post_nne_change_desc <- if (diff_post_pre_nne > 0.5) {
  glue(
    "an increase of {format_est(diff_post_pre_nne, 2)} dB from the pre-exam level"
  )
} else if (diff_post_pre_nne < -0.5) {
  glue(
    "a decrease of {format_est(abs(diff_post_pre_nne), 2)} dB from the pre-exam level"
  )
} else {
  "minimal change from the pre-exam level"
}

# ==============================================================================
# Generate Complete Results Section
# ==============================================================================

results_text <- glue(
  "
# Results: Main Effects of Exam Stress on Vocal Production

## Analytic Approach

We examined the effects of acute exam-related stress on two core acoustic parameters: fundamental frequency (F0 mean, averaged across vowels /a/, /i/, and /u/) and normalized noise energy (NNE, similarly averaged). Hierarchical Bayesian models were specified to account for the nested structure of repeated acoustic measurements within participants. Two orthogonal contrasts captured distinct phases of the stress response: a *stress contrast* (c₁) comparing pre-exam to baseline recordings, and a *recovery contrast* (c₂) comparing post-exam to pre-exam recordings. Models included random intercepts and random slopes for both contrasts, allowing individual variability in baseline vocal parameters and stress reactivity.

All models were implemented in Stan via rstan and estimated using Hamiltonian Monte Carlo with four chains of 4,000 iterations each (2,000 warmup). Convergence was assessed using R-hat statistics (all < 1.01) and visual inspection of trace plots. Following the inferential framework described in the Supplement, we report posterior medians together with the probability of direction (pd) as the primary index of evidence, and 89% equal-tailed credible intervals (CrIs) as summaries of magnitude uncertainty; the intervals are not interpreted as significance tests, and we read the pd as a continuous index of directional certainty rather than against a threshold.

---

## Fundamental Frequency (F0 Mean)

### Descriptive Findings

Fundamental frequency exhibited a progressive increase across assessment timepoints. At baseline, mean F0 was {format_est(baseline_f0_mean, 1)} Hz (SD = {format_est(baseline_f0_sd, 1)}). This increased to {format_est(pre_f0_mean, 1)} Hz (SD = {format_est(pre_f0_sd, 1)}) immediately before the exam, representing a raw increase of {format_est(diff_pre_baseline_f0, 1)} Hz. Following the exam, F0 was {format_est(post_f0_mean, 1)} Hz (SD = {format_est(post_f0_sd, 1)}), showing an additional change of {format_est(diff_post_pre_f0, 1)} Hz from the pre-exam assessment.

### Model Results

The hierarchical Bayesian model confirmed robust stress-related elevation in fundamental frequency. The intercept parameter (α), representing estimated F0 at baseline, had a posterior median of {format_est(f0_alpha$median, 1)} Hz (MAD = {format_est(f0_alpha$mad, 1)}, 89% CrI [{format_est(f0_alpha$lower, 1)}, {format_est(f0_alpha$upper, 1)}]). 

The stress contrast (β₁) revealed a clear positive effect: F0 increased by {format_est(f0_b1$median, 2)} Hz (MAD = {format_est(f0_b1$mad, 2)}, 89% CrI [{format_est(f0_b1$lower, 2)}, {format_est(f0_b1$upper, 2)}]) when comparing pre-exam to baseline recordings. This effect was highly consistent across posterior samples, with P(β₁ > 0) = {format_prob(f0_b1$prob_pos)}. This finding indicates that acute academic stress reliably elevates vocal pitch, consistent with increased laryngeal tension and autonomic arousal.

The recovery contrast (β₂) was {b2_direction_f0}, with a median estimate of {format_est(f0_b2$median, 2)} Hz (MAD = {format_est(f0_b2$mad, 2)}, 89% CrI [{format_est(f0_b2$lower, 2)}, {format_est(f0_b2$upper, 2)}], P(β₂ > 0) = {format_prob(f0_b2$prob_pos)}). {b2_f0_interp}

Between-person variability was substantial, as evidenced by the standard deviation of random intercepts (τ₁ = {format_est(f0_tau1$median, 2)}, 89% CrI [{format_est(f0_tau1$lower, 2)}, {format_est(f0_tau1$upper, 2)}]) and random slopes for the stress contrast (τ₂ = {format_est(f0_tau2$median, 2)}, 89% CrI [{format_est(f0_tau2$lower, 2)}, {format_est(f0_tau2$upper, 2)}]). The residual standard deviation was σ = {format_est(f0_sigma$median, 2)} Hz (89% CrI [{format_est(f0_sigma$lower, 2)}, {format_est(f0_sigma$upper, 2)}]), reflecting within-person measurement variability.

---

## Normalized Noise Energy (NNE)

### Descriptive Findings

In contrast to F0, NNE exhibited a pattern consistent with reduced glottal noise under stress. At baseline, mean NNE was {format_est(baseline_nne_mean, 2)} dB (SD = {format_est(baseline_nne_sd, 2)}). This decreased to {format_est(pre_nne_mean, 2)} dB (SD = {format_est(pre_nne_sd, 2)}) at the pre-exam assessment, reflecting a reduction of {format_est(abs(diff_pre_baseline_nne), 2)} dB. Post-exam values were {format_est(post_nne_mean, 2)} dB (SD = {format_est(post_nne_sd, 2)}), showing {post_nne_change_desc}.

### Model Results

The hierarchical model for NNE confirmed a systematic reduction in glottal noise under acute stress. The intercept parameter (α) had a posterior median of {format_est(nne_alpha$median, 2)} dB (MAD = {format_est(nne_alpha$mad, 2)}, 89% CrI [{format_est(nne_alpha$lower, 2)}, {format_est(nne_alpha$upper, 2)}]).

Critically, the stress contrast (β₁) showed a robust negative effect: NNE decreased by {format_est(abs(nne_b1$median), 2)} dB (MAD = {format_est(nne_b1$mad, 2)}, 89% CrI [{format_est(nne_b1$lower, 2)}, {format_est(nne_b1$upper, 2)}]) when comparing pre-exam to baseline recordings. The posterior probability that this effect was negative was P(β₁ < 0) = {format_prob(nne_b1$prob_neg)}, providing strong evidence for stress-induced reduction in glottal noise. More negative NNE values indicate a more periodic, harmonically stable signal, suggesting that acute stress does not destabilize phonation but instead promotes a 'cleaner,' albeit potentially more effortful, vocal quality.

The recovery contrast (β₂) had a median estimate of {format_est(nne_b2$median, 2)} dB (MAD = {format_est(nne_b2$mad, 2)}, 89% CrI [{format_est(nne_b2$lower, 2)}, {format_est(nne_b2$upper, 2)}], P(β₂ > 0) = {format_prob(nne_b2$prob_pos)}). {b2_nne_interp}

Random effects estimates revealed considerable between-person heterogeneity in baseline NNE (τ₁ = {format_est(nne_tau1$median, 2)}, 89% CrI [{format_est(nne_tau1$lower, 2)}, {format_est(nne_tau1$upper, 2)}]) and in stress-related change (τ₂ = {format_est(nne_tau2$median, 2)}, 89% CrI [{format_est(nne_tau2$lower, 2)}, {format_est(nne_tau2$upper, 2)}]). Residual variability was σ = {format_est(nne_sigma$median, 2)} dB (89% CrI [{format_est(nne_sigma$lower, 2)}, {format_est(nne_sigma$upper, 2)}]).

---

## Summary of Main Effects

Exam-related stress produced dissociable changes in vocal production. Fundamental frequency increased robustly under stress, reflecting heightened autonomic arousal and laryngeal tension. In contrast, NNE decreased, indicating reduced glottal noise and a more controlled, periodic phonatory signal. These patterns suggest that acute stress does not simply destabilize the voice but instead induces simultaneous increases in physiological arousal (indexed by F0) and compensatory control (indexed by reduced noise). The consistent directionality and strong directional evidence (pd) for both parameters underscore the reliability of these vocal signatures of stress, which were observed across individuals despite substantial between-person variability in baseline vocal characteristics and stress reactivity.
"
)

# ==============================================================================
# Save Complete Results Section
# ==============================================================================

write_file(results_text, here("manuscript", "results_main_effects_complete.md"))


cat("\n=== Complete Results section generated ===\n")
cat("File saved to: manuscript/results_main_effects_complete.md\n")

# ==============================================================================
# Generate Quick Summary Table
# ==============================================================================

summary_table <- tibble(
  Parameter = c(
    "F0: Intercept (α)",
    "F0: Stress (β₁)",
    "F0: Recovery (β₂)",
    "F0: Residual SD (σ)",
    "NNE: Intercept (α)",
    "NNE: Stress (β₁)",
    "NNE: Recovery (β₂)",
    "NNE: Residual SD (σ)"
  ),
  Median = c(
    format_est(f0_alpha$median, 2),
    format_est(f0_b1$median, 2),
    format_est(f0_b2$median, 2),
    format_est(f0_sigma$median, 2),
    format_est(nne_alpha$median, 2),
    format_est(nne_b1$median, 2),
    format_est(nne_b2$median, 2),
    format_est(nne_sigma$median, 2)
  ),
  CrI_89 = c(
    glue("[{format_est(f0_alpha$lower, 2)}, {format_est(f0_alpha$upper, 2)}]"),
    glue("[{format_est(f0_b1$lower, 2)}, {format_est(f0_b1$upper, 2)}]"),
    glue("[{format_est(f0_b2$lower, 2)}, {format_est(f0_b2$upper, 2)}]"),
    glue("[{format_est(f0_sigma$lower, 2)}, {format_est(f0_sigma$upper, 2)}]"),
    glue(
      "[{format_est(nne_alpha$lower, 2)}, {format_est(nne_alpha$upper, 2)}]"
    ),
    glue("[{format_est(nne_b1$lower, 2)}, {format_est(nne_b1$upper, 2)}]"),
    glue("[{format_est(nne_b2$lower, 2)}, {format_est(nne_b2$upper, 2)}]"),
    glue("[{format_est(nne_sigma$lower, 2)}, {format_est(nne_sigma$upper, 2)}]")
  ),
  P_direction = c(
    format_prob(max(f0_alpha$prob_pos, f0_alpha$prob_neg)),
    format_prob(f0_b1$prob_pos),
    format_prob(max(f0_b2$prob_pos, f0_b2$prob_neg)),
    "—",
    format_prob(max(nne_alpha$prob_pos, nne_alpha$prob_neg)),
    format_prob(nne_b1$prob_neg),
    format_prob(max(nne_b2$prob_pos, nne_b2$prob_neg)),
    "—"
  )
)

write_csv(summary_table, here("manuscript", "parameter_estimates_summary.csv"))

print(summary_table)

cat("\n=== Parameter estimates summary saved ===\n")
cat("File saved to: manuscript/parameter_estimates_summary.csv\n")


# ==============================================================================
# Quantify Between-Person Variability
# ==============================================================================

# F0 - Variance components
f0_icc_baseline <- f0_tau1$median^2 / (f0_tau1$median^2 + f0_sigma$median^2)
f0_cv_baseline <- (f0_tau1$median / abs(f0_alpha$median)) * 100

# F0 - Stress reactivity heterogeneity
f0_tau2_beta1_ratio <- f0_tau2$median / abs(f0_b1$median)

# Individual prediction intervals (89%) for stress effect
f0_stress_pi_lower <- f0_b1$median - 1.6449 * f0_tau2$median
f0_stress_pi_upper <- f0_b1$median + 1.6449 * f0_tau2$median

cat("\n=== F0 BETWEEN-PERSON VARIABILITY ===\n")
cat("ICC (baseline):", format_est(f0_icc_baseline * 100, 1), "%\n")
cat("CV (baseline):", format_est(f0_cv_baseline, 1), "%\n")
cat("τ₂/β₁ ratio:", format_est(f0_tau2_beta1_ratio, 2), "\n")
cat(
  "Individual stress effect 89% PI: [",
  format_est(f0_stress_pi_lower, 1),
  ",",
  format_est(f0_stress_pi_upper, 1),
  "] Hz\n"
)

# NNE - Variance components
nne_icc_baseline <- nne_tau1$median^2 / (nne_tau1$median^2 + nne_sigma$median^2)
nne_cv_baseline <- (nne_tau1$median / abs(nne_alpha$median)) * 100

# NNE - Stress reactivity heterogeneity
nne_tau2_beta1_ratio <- nne_tau2$median / abs(nne_b1$median)

# Individual prediction intervals (89%) for stress effect
nne_stress_pi_lower <- nne_b1$median - 1.6449 * nne_tau2$median
nne_stress_pi_upper <- nne_b1$median + 1.6449 * nne_tau2$median

cat("\n=== NNE BETWEEN-PERSON VARIABILITY ===\n")
cat("ICC (baseline):", format_est(nne_icc_baseline * 100, 1), "%\n")
cat("CV (baseline):", format_est(nne_cv_baseline, 1), "%\n")
cat("τ₂/β₁ ratio:", format_est(nne_tau2_beta1_ratio, 2), "\n")
cat(
  "Individual stress effect 89% PI: [",
  format_est(nne_stress_pi_lower, 2),
  ",",
  format_est(nne_stress_pi_upper, 2),
  "] dB\n"
)

# Assumendo distribuzione normale dei random slopes
# β₁ = -0.65, τ₂ = 0.83
# P(individual effect > 0) = P(β₁ + u₂ > 0) = P(u₂ > -β₁)
prob_opposite <- pnorm(
  0,
  mean = nne_b1$median,
  sd = nne_tau2$median,
  lower.tail = FALSE
)
cat(
  "% individuals with opposite NNE effect:",
  round(prob_opposite * 100, 1),
  "%\n"
)

# eof ---
