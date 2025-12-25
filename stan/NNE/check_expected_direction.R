# ==============================================================================
# check_expected_direction_nne.R  (CORRETTO per il tuo df_voice)
# Usa: ID, timepoint, y_nne, c1_stress, c2_recovery, subj
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

# ------------------------------------------------------------
# 0) df_voice deve essere già in environment
#    (oppure caricalo dal bundle)
# ------------------------------------------------------------
if (!exists("df_voice")) {
  bundle_path <- "results/stan_bundle_nne_pid5.rds"
  stopifnot(file.exists(bundle_path))
  bundle <- readRDS(bundle_path)
  df_voice <- bundle$df_voice
}

# Sanity: nomi attesi
stopifnot(all(
  c("ID", "timepoint", "y_nne", "c1_stress", "c2_recovery", "subj") %in%
    names(df_voice)
))

# Ordina livelli timepoint
df_voice <- df_voice %>%
  mutate(timepoint = factor(timepoint, levels = c("baseline", "pre", "post")))

cat("\n=== CONTRAST CODING CHECK ===\n")
print(
  df_voice %>%
    distinct(timepoint, c1_stress, c2_recovery) %>%
    arrange(timepoint)
)

cat("\nNota importante:\n")
cat("Nel modello y = alpha + b1*c1_stress + b2*c2_recovery + ...\n")
cat(
  "b1 NON è automaticamente (pre-baseline) e b2 NON è automaticamente (post-pre).\n"
)
cat("Le differenze tra timepoint sono combinazioni lineari di b1 e b2.\n\n")

# ------------------------------------------------------------
# 1) Derivazione analitica: differenze come combinazioni di b1/b2
# ------------------------------------------------------------
cat("=== DIFFERENZE IMPLICATE DALLA CODIFICA (in termini di b1, b2) ===\n")
cat("baseline: c1=-0.5, c2=0\n")
cat("pre:      c1=+0.5, c2=-0.5\n")
cat("post:     c1= 0,   c2=+0.5\n\n")

cat("Quindi:\n")
cat(
  "Δ_stress = E[pre]-E[baseline] = b1*(0.5-(-0.5)) + b2*(-0.5-0) = 1*b1 - 0.5*b2\n"
)
cat(
  "Δ_recovery = E[post]-E[pre]   = b1*(0-0.5)      + b2*(0.5-(-0.5)) = -0.5*b1 + 1*b2\n"
)
cat(
  "Δ_post_vs_base = E[post]-E[baseline] = b1*(0-(-0.5)) + b2*(0.5-0) = 0.5*b1 + 0.5*b2\n\n"
)

# ------------------------------------------------------------
# 2) Controllo empirico: medie per timepoint e differenze within-subject
# ------------------------------------------------------------
cat("=== DESCRITTIVE y_nne PER TIMEPOINT ===\n")
by_tp <- df_voice %>%
  group_by(timepoint) %>%
  summarise(
    n = n(),
    mean = mean(y_nne, na.rm = TRUE),
    sd = sd(y_nne, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(timepoint)
print(by_tp)

# Wide per ID (within-subject)
wide <- df_voice %>%
  select(ID, timepoint, y_nne) %>%
  pivot_wider(names_from = timepoint, values_from = y_nne)

# differenze within-subject
diffs <- wide %>%
  transmute(
    ID,
    d_stress = pre - baseline, # pre - baseline
    d_recovery = post - pre, # post - pre
    d_post_vs_base = post - baseline # post - baseline
  )

cat("\n=== DIFFERENZE WITHIN-SUBJECT (NNE) ===\n")
diff_summary <- diffs %>%
  summarise(
    n = n(),
    mean_d_stress = mean(d_stress, na.rm = TRUE),
    q05_d_stress = quantile(d_stress, 0.05, na.rm = TRUE),
    q95_d_stress = quantile(d_stress, 0.95, na.rm = TRUE),
    p_d_stress_gt0 = mean(d_stress > 0, na.rm = TRUE),
    p_d_stress_lt0 = mean(d_stress < 0, na.rm = TRUE),

    mean_d_recovery = mean(d_recovery, na.rm = TRUE),
    q05_d_recovery = quantile(d_recovery, 0.05, na.rm = TRUE),
    q95_d_recovery = quantile(d_recovery, 0.95, na.rm = TRUE),
    p_d_recovery_gt0 = mean(d_recovery > 0, na.rm = TRUE),
    p_d_recovery_lt0 = mean(d_recovery < 0, na.rm = TRUE)
  )
print(diff_summary)

cat("\nInterpretazione segni (NNE è spesso negativa):\n")
cat(" - d_stress > 0  => NNE aumenta (meno negativa, verso 0)\n")
cat(" - d_stress < 0  => NNE diminuisce (più negativa)\n\n")

# ------------------------------------------------------------
# 3) “Direzione attesa”: dipende da cosa intendi per NNE
# ------------------------------------------------------------
assumption_tbl <- tibble(
  assumption = c(
    "Peggior qualità sotto stress => NNE aumenta (meno negativa, verso 0) => d_stress > 0",
    "Peggior qualità sotto stress => NNE diminuisce (più negativa) => d_stress < 0"
  ),
  expected_sign_d_stress = c("gt0", "lt0"),
  p_data_matches = c(
    mean(diffs$d_stress > 0, na.rm = TRUE),
    mean(diffs$d_stress < 0, na.rm = TRUE)
  ),
  mean_d_stress = mean(diffs$d_stress, na.rm = TRUE)
)

cat("=== CHECK: quale assunzione è coerente con i tuoi dati? ===\n")
print(assumption_tbl)

# ------------------------------------------------------------
# 4) Visual (opzionale)
# ------------------------------------------------------------
if (interactive()) {
  print(
    ggplot(diffs, aes(x = d_stress)) +
      geom_histogram(bins = 30, na.rm = TRUE) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      labs(title = "NNE: Δ stress = PRE - BASELINE", x = "Δ (dB)", y = "count")
  )

  print(
    ggplot(diffs, aes(x = d_recovery)) +
      geom_histogram(bins = 30, na.rm = TRUE) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      labs(title = "NNE: Δ recovery = POST - PRE", x = "Δ (dB)", y = "count")
  )
}

# ------------------------------------------------------------
# 5) Sanity check: regressione OLS solo per vedere segni (non inferenza)
# ------------------------------------------------------------
lm_fit <- lm(y_nne ~ c1_stress + c2_recovery, data = df_voice)
cat("\n=== OLS sanity check (solo segni/scala) ===\n")
print(summary(lm_fit)$coefficients)

cat("\nFINE.\n")

#' Discussion: Vocal Stress Reactivity as Arousal and Control
#'
#' The present findings suggest that exam-related stress modulates vocal
#' production along two partially dissociable dimensions, reflecting increased
#' physiological arousal and enhanced phonatory control. Specifically, stress
#' was associated with a robust increase in fundamental frequency (F0 mean),
#' accompanied by a concurrent reduction in glottal noise, indexed by more
#' negative Noise-to-Harmonics Energy (NNE) values. Taken together, these
#' patterns indicate that acute academic stress does not merely destabilize
#' vocal production, but instead induces a more controlled and tension-driven
#' phonatory state.
#'
#' The elevation of F0 under stress is consistent with extensive evidence
#' linking psychological stress and autonomic arousal to increased laryngeal
#' muscle activation and subglottal pressure. Importantly, the current study
#' extends this literature by showing that this pitch increase is not uniform
#' across individuals: Negative Affectivity reliably amplified stress-related
#' F0 elevation, suggesting that individuals characterized by heightened
#' emotional reactivity exhibit stronger vocal arousal responses. This finding
#' aligns with theoretical models positing Negative Affectivity as a core
#' dimension of stress sensitivity and autonomic responsivity.
#'
#' In contrast, changes in NNE followed a different pattern. Rather than
#' increasing under stress—as might be expected if stress primarily induced
#' vocal instability—NNE decreased, indicating reduced glottal noise and a more
#' periodic signal. This effect was consistent across descriptive analyses and
#' hierarchical Bayesian modeling, and showed little evidence of modulation by
#' personality traits. From a physiological perspective, this pattern is
#' compatible with stress-induced hyperadduction or increased laryngeal tension,
#' leading to a “pressed” voice quality that is acoustically cleaner but less
#' flexible. Thus, while F0 captures the arousal-driven component of the stress
#' response, NNE appears to index a control-related component, reflecting
#' compensatory or regulatory adjustments in phonatory behavior.
#'
#' Crucially, the dissociation between F0 and NNE moderation highlights that not
#' all aspects of vocal stress reactivity are equally shaped by personality traits.
#' Whereas Negative Affectivity selectively modulated arousal-related pitch
#' responses, noise-related measures of vocal quality were largely trait-independent,
#' suggesting a more uniform physiological mechanism. This pattern underscores
#' the importance of conceptualizing vocal stress responses as multidimensional,
#' rather than assuming a single pathway of vocal “degradation” under stress.
#'
#' Methodologically, these conclusions were enabled by modeling personality
#' traits as latent variables with explicit measurement error correction,
#' integrating intensive EMA assessments with hierarchical Bayesian models.
#' This approach revealed trait-level moderation effects that were not apparent
#' in conventional mixed-effects analyses, while also clarifying the limits of
#' personality modulation for certain acoustic parameters. Together, the
#' findings support a model in which acute stress elicits simultaneous increases
#' in arousal and control, with personality differences shaping the former more
#' strongly than the latter.
