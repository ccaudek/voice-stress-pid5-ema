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
  bundle_path <- here::here("results", "NNE", "stan_bundle_nne_pid5.rds")
  stopifnot(file.exists(bundle_path))
  bundle <- readRDS(bundle_path)
  df_voice <- bundle$df_voice
}

# Sanity: nomi attesi
stopifnot(all(
  c("ID", "timepoint", "y_nne", "c1_stress", "c2_recovery") %in%
    names(df_voice)
))

# Ordina livelli timepoint
df_voice <- df_voice |>
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
by_tp <- df_voice |>
  group_by(timepoint) |>
  summarise(
    n = n(),
    mean = mean(y_nne, na.rm = TRUE),
    sd = sd(y_nne, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(timepoint)
print(by_tp)

# Wide per ID (within-subject)
wide <- df_voice |>
  dplyr::select(ID, timepoint, y_nne) |>
  pivot_wider(names_from = timepoint, values_from = y_nne)

# differenze within-subject
diffs <- wide |>
  transmute(
    ID,
    d_stress = pre - baseline, # pre - baseline
    d_recovery = post - pre, # post - pre
    d_post_vs_base = post - baseline # post - baseline
  )

cat("\n=== DIFFERENZE WITHIN-SUBJECT (NNE) ===\n")
diff_summary <- diffs |>
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
data.frame(diff_summary)

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

# Aggiungi questo nel tuo script di interpretazione:
cat("\n=== INTERPRETAZIONE CHIAVE ===\n")
cat("NNE = Noise-to-Harmonics Energy (in dB, valori tipici negativi)\n")
cat("NNE più negativo = MENO rumore glottico = voce più 'pulita'\n")
cat("NNE meno negativo = PIÙ rumore glottico = voce più 'soffiata'\n")
cat("Stress → NNE più negativo di -0.541 dB → voce più 'pulita' sotto stress\n")

cat("\nFINE.\n")
