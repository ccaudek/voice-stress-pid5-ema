# ==============================================================================
# 10_f2_disinhibition_moderation.R
# Analisi moderazione Disinhibition su F2 / articulatory precision
#
# OBIETTIVO:
# Testare se Disinhibition modera gli effetti dello stress su caratteristiche
# articolatorie (F2, Vowel Space Area).
#
# IPOTESI:
# Disinhibition (ridotto controllo motorio) × Stress → maggiore centralizzazione
# vocalica / ridotta precisione articolatoria
#
# STRATEGIA:
# 1. Calcola metriche robuste: VSA, F2 range normalized, F2 mean
# 2. Testa quale metrica risponde meglio allo stress
# 3. Modello Stan con measurement error e latent Disinhibition
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
  library(rio)
  library(jsonlite)
  library(ggplot2)
  library(patchwork)
})

# ==============================================================================
# 0) CONFIGURAZIONE
# ==============================================================================

# Percorsi
voice_path <- here::here(
  "data",
  "raw",
  "acustic_features",
  "datiacustici",
  "AUDIO.xlsx"
)
ema_path <- here::here("data", "processed", "ema_plus_scales_cleaned.csv")

# Output
output_dir <- "results/f2_disinhibition"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Domini PID-5 EMA
pid5_ema_vars <- c(
  "pid5_negative_affectivity",
  "pid5_detachment",
  "pid5_antagonism",
  "pid5_disinhibition",
  "pid5_psychoticism"
)

# ==============================================================================
# 1) CARICAMENTO DATI VOCALI CON F1 E F2
# ==============================================================================

cat("=== CARICAMENTO DATI VOCALI ===\n")

# Carica 3 sheet
baseline <- read_excel(voice_path, sheet = "BASELINE") %>%
  mutate(timepoint = "baseline")
pre <- read_excel(voice_path, sheet = "PRE") %>%
  mutate(timepoint = "pre")
post <- read_excel(voice_path, sheet = "POST") %>%
  mutate(timepoint = "post")

df_voice <- bind_rows(baseline, pre, post)
names(df_voice) <- str_trim(names(df_voice))

# Correzione ID
df_voice <- df_voice |>
  mutate(
    ID = case_when(
      ID == "am_bo_1988_08_24_166" ~ "an_bo_1988_08_24_166",
      ID == "as_li_2005_04_26_447" ~ "as_si_2005_04_26_447",
      ID == "cl_bo_1987_10_16_628" ~ "ca_bo_1987_10_16_628",
      ID == "hi_na_2005_03_08_339" ~ "gi_na_2005_03_08_339",
      ID == "ma_si_2003_10_31_940" ~ "si_ma_2003_10_31_940",
      TRUE ~ ID
    )
  ) |>
  dplyr::rename(user_id = ID)

# Estrai F1 e F2 per tutte le vocali
df_voice <- df_voice |>
  transmute(
    user_id,
    timepoint = factor(timepoint, levels = c("baseline", "pre", "post")),
    # F1 per vocale
    f1_a = `F1 mean Hz /a/`,
    f1_i = `F1 mean Hz /i/`,
    f1_u = `F1 mean Hz /u/`,
    # F2 per vocale
    f2_a = `F2 mean Hz /a/`,
    f2_i = `F2 mean Hz /i/`,
    f2_u = `F2 mean Hz /u/`,
    # F0 (per confronto)
    f0_mean = (`F0 mean Hz /a/` + `F0 mean Hz /i/` + `F0 mean Hz /u/`) / 3
  )

cat(sprintf(
  "Dati vocali: %d osservazioni, %d soggetti\n",
  nrow(df_voice),
  n_distinct(df_voice$user_id)
))

# ==============================================================================
# 2) CALCOLO METRICHE ARTICOLATORIE ROBUSTE
# ==============================================================================

cat("\n=== CALCOLO METRICHE ARTICOLATORIE ===\n")

df_voice <- df_voice %>%
  mutate(
    # === OPZIONE A: VOWEL SPACE AREA (VSA) ===
    # Area triangolo /a-i-u/ nel piano F1-F2 (formula di Shoelace)
    vsa = 0.5 *
      abs(
        f1_a * (f2_i - f2_u) + f1_i * (f2_u - f2_a) + f1_u * (f2_a - f2_i)
      ),

    # Log-trasformato per ridurre skewness
    vsa_log = log(vsa),

    # === OPZIONE B: F2 RANGE (NORMALIZED) ===
    # Quanto è "esteso" lo spazio F2 (i-u continuum)
    f2_range = f2_i - f2_u,

    # Normalizzato per dimensioni vocali (F1 medio)
    f1_mean = (f1_a + f1_i + f1_u) / 3,
    f2_range_norm = f2_range / f1_mean,

    # === OPZIONE C: F2 MEAN (SEMPLICE) ===
    f2_mean = (f2_a + f2_i + f2_u) / 3,

    # === OPZIONE D: F2 SLOPE (i-u articulation) ===
    # Slope linea i→u nel piano F1-F2
    f2_slope = (f2_i - f2_u) / (f1_i - f1_u),

    # === OPZIONE E: CENTRALIZATION INDEX ===
    # Distanza euclidea media dal centroide vocalico
    f1_centroid = (f1_a + f1_i + f1_u) / 3,
    f2_centroid = (f2_a + f2_i + f2_u) / 3,

    centralization = sqrt(
      ((f1_a - f1_centroid)^2 +
        (f2_a - f2_centroid)^2 +
        (f1_i - f1_centroid)^2 +
        (f2_i - f2_centroid)^2 +
        (f1_u - f1_centroid)^2 +
        (f2_u - f2_centroid)^2) /
        3
    )
  )

# Report NA
metrics <- c(
  "vsa_log",
  "f2_range_norm",
  "f2_mean",
  "f2_slope",
  "centralization"
)
for (m in metrics) {
  n_na <- sum(is.na(df_voice[[m]]))
  cat(sprintf("  %s: %d NA (%.1f%%)\n", m, n_na, 100 * n_na / nrow(df_voice)))
}

# ==============================================================================
# 3) CONTRASTI STRESS
# ==============================================================================

df_voice <- df_voice %>%
  mutate(
    c1_stress = case_when(
      timepoint == "baseline" ~ -0.5,
      timepoint == "pre" ~ 0.5,
      timepoint == "post" ~ 0.0
    ),
    c2_recovery = case_when(
      timepoint == "baseline" ~ 0.0,
      timepoint == "pre" ~ -0.5,
      timepoint == "post" ~ 0.5
    )
  ) %>%
  filter(
    !is.na(user_id),
    !is.na(c1_stress),
    !is.na(c2_recovery)
  )

# ==============================================================================
# 4) CARICAMENTO EMA E MERGE
# ==============================================================================

cat("\n=== CARICAMENTO EMA ===\n")

ema <- rio::import(ema_path) %>% as_tibble()

# PID-5 EMA
df_ema <- ema %>%
  transmute(
    ID = user_id,
    across(all_of(pid5_ema_vars), as.numeric)
  ) %>%
  filter(!is.na(ID)) %>%
  filter(ID %in% unique(df_voice$user_id)) %>%
  filter(if_any(all_of(pid5_ema_vars), ~ !is.na(.x)))

# Imputazione within-subject
df_ema_imp <- df_ema %>%
  group_by(ID) %>%
  mutate(across(
    all_of(pid5_ema_vars),
    ~ if_else(is.na(.x), mean(.x, na.rm = TRUE), .x)
  )) %>%
  ungroup() %>%
  filter(if_all(all_of(pid5_ema_vars), ~ is.finite(.x)))

cat(sprintf(
  "EMA: %d righe, %d soggetti\n",
  nrow(df_ema_imp),
  n_distinct(df_ema_imp$ID)
))

# ==============================================================================
# 5) ANALISI ESPLORATIVA: QUALE METRICA FUNZIONA MEGLIO?
# ==============================================================================

cat("\n=== ANALISI ESPLORATIVA: EFFETTI STRESS ===\n")

# Test: quale metrica risponde meglio allo stress?
# Modello semplice: metric ~ c1_stress + (1|ID)

test_stress_effects <- function(df, metric_name) {
  # Rimuovi NA
  df_complete <- df %>%
    select(user_id, timepoint, c1_stress, all_of(metric_name)) %>%
    na.omit()

  if (nrow(df_complete) < 50) {
    return(tibble(
      metric = metric_name,
      n = nrow(df_complete),
      mean_baseline = NA,
      mean_pre = NA,
      mean_post = NA,
      effect_stress = NA,
      cor_with_f0 = NA
    ))
  }

  # Medie per timepoint
  means <- df_complete %>%
    group_by(timepoint) %>%
    summarise(m = mean(.data[[metric_name]], na.rm = TRUE), .groups = "drop")

  # Effetto stress (pre - baseline)
  effect <- means$m[means$timepoint == "pre"] -
    means$m[means$timepoint == "baseline"]

  # Correlazione con F0
  df_cor <- df %>%
    select(all_of(metric_name), f0_mean) %>%
    na.omit()

  cor_f0 <- if (nrow(df_cor) > 10)
    cor(df_cor[[metric_name]], df_cor$f0_mean) else NA

  tibble(
    metric = metric_name,
    n = nrow(df_complete),
    mean_baseline = means$m[means$timepoint == "baseline"],
    mean_pre = means$m[means$timepoint == "pre"],
    mean_post = means$m[means$timepoint == "post"],
    effect_stress = effect,
    cor_with_f0 = cor_f0
  )
}

# Test tutte le metriche
results_exploratory <- map_dfr(
  metrics,
  ~ test_stress_effects(df_voice, .x)
)

cat("\nRisultati preliminari (effetto stress = pre - baseline):\n")
print(results_exploratory)

write_csv(
  results_exploratory,
  file.path(output_dir, "exploratory_stress_effects.csv")
)

# Identifica metrica migliore (maggior effetto stress)
best_metric <- results_exploratory %>%
  filter(!is.na(effect_stress)) %>%
  arrange(desc(abs(effect_stress))) %>%
  slice(1) %>%
  pull(metric)

cat(sprintf("\n✓ Metrica con effetto stress più forte: %s\n", best_metric))

# ==============================================================================
# 6) VISUALIZZAZIONI ESPLORATIVE
# ==============================================================================

cat("\n=== CREAZIONE GRAFICI ESPLORATIVI ===\n")

# Plot: tutte le metriche per timepoint
p_metrics <- df_voice %>%
  select(user_id, timepoint, all_of(metrics)) %>%
  pivot_longer(
    cols = all_of(metrics),
    names_to = "metric",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = timepoint, y = value, fill = timepoint)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~metric, scales = "free_y", ncol = 2) +
  labs(
    title = "Articulatory Metrics Across Stress Conditions",
    x = "Timepoint",
    y = "Metric Value"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  file.path(output_dir, "metrics_by_timepoint.pdf"),
  p_metrics,
  width = 12,
  height = 10
)

cat("✓ Grafici esplorativi salvati\n")

# ==============================================================================
# 7) PREPARAZIONE DATI STAN: FOCUS SU METRICA MIGLIORE
# ==============================================================================

cat("\n=== PREPARAZIONE DATI STAN ===\n")

# Usa metrica migliore (o VSA se disponibile)
outcome_metric <- if (
  "vsa_log" %in% metrics && sum(!is.na(df_voice$vsa_log)) > 100
) {
  "vsa_log"
} else {
  best_metric
}

cat(sprintf("Outcome selezionato: %s\n", outcome_metric))

# Indici soggetti
subj_ids <- sort(unique(intersect(df_voice$user_id, df_ema_imp$ID)))
N_subj <- length(subj_ids)
id_map <- tibble(ID = subj_ids, subj = seq_len(N_subj))

# Voice data
df_voice_stan <- df_voice %>%
  inner_join(id_map, by = c("user_id" = "ID")) %>%
  filter(!is.na(.data[[outcome_metric]])) %>%
  arrange(subj, timepoint)

# EMA data
df_ema_stan <- df_ema_imp %>%
  inner_join(id_map, by = "ID") %>%
  arrange(subj)

cat(sprintf(
  "FINAL: N_subj = %d | voice obs = %d | ema obs = %d\n",
  N_subj,
  nrow(df_voice_stan),
  nrow(df_ema_stan)
))

# Standardizza PID-5 EMA
X <- df_ema_stan %>%
  select(all_of(pid5_ema_vars)) %>%
  as.matrix()

stopifnot(!anyNA(X))
X_scaled <- scale(X)

pid5_center <- attr(X_scaled, "scaled:center")
pid5_scale <- attr(X_scaled, "scaled:scale")

# ==============================================================================
# 8) CREA LISTA DATI STAN
# ==============================================================================

stan_data <- list(
  N_subj = N_subj,

  # Voice part
  N_voice = nrow(df_voice_stan),
  subj_voice = as.integer(df_voice_stan$subj),
  y = as.numeric(df_voice_stan[[outcome_metric]]),
  c1 = as.numeric(df_voice_stan$c1_stress),
  c2 = as.numeric(df_voice_stan$c2_recovery),

  # EMA measurement part
  N_ema = nrow(df_ema_stan),
  subj_ema = as.integer(df_ema_stan$subj),
  D = length(pid5_ema_vars),
  X = X_scaled
)

# Verifica
stopifnot(length(stan_data$y) == stan_data$N_voice)
stopifnot(ncol(stan_data$X) == stan_data$D)
stopifnot(!anyNA(stan_data$X))
stopifnot(!anyNA(stan_data$y))

# Export
write_json(
  stan_data,
  path = file.path(output_dir, "stan_data_f2_disinhibition.json"),
  digits = 16,
  auto_unbox = TRUE
)

saveRDS(
  list(
    stan_data = stan_data,
    df_voice = df_voice_stan,
    df_ema = df_ema_stan,
    pid5_center = pid5_center,
    pid5_scale = pid5_scale,
    pid5_vars = pid5_ema_vars,
    outcome_metric = outcome_metric,
    metrics_tested = results_exploratory
  ),
  file = file.path(output_dir, "stan_bundle_f2_disinhibition.rds")
)

cat("\nDati Stan salvati.\n")

# ==============================================================================
# 9) SUGGERIMENTI PROSSIMI PASSI
# ==============================================================================

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("ANALISI ESPLORATIVA COMPLETATA\n")
cat(rep("=", 80), "\n", sep = "")

cat("\nOUTPUT:\n")
cat("  - exploratory_stress_effects.csv\n")
cat("  - metrics_by_timepoint.pdf\n")
cat("  - stan_data_f2_disinhibition.json\n")
cat("  - stan_bundle_f2_disinhibition.rds\n")

cat("\nMETRICA SELEZIONATA:", outcome_metric, "\n")

cat("\nPROSSIMI PASSI:\n")
cat("1. Esamina exploratory_stress_effects.csv:\n")
cat("   - Quale metrica ha effetto stress più forte?\n")
cat("   - VSA mostra centralizzazione (riduzione) sotto stress?\n")
cat("   - F2 range mostra compressione sotto stress?\n\n")

cat("2. Se effetti stress deboli/inconsistenti:\n")
cat("   a) Potrebbe essere che F2 è troppo rumoroso\n")
cat("   b) Considera pooling vocali in modo diverso\n")
cat("   c) Prova normalizzazione Bark/Mel\n")
cat("   d) Outlier removal più aggressivo\n\n")

cat("3. Se effetti stress presenti:\n")
cat("   → Usa modello Stan f2_disinhibition_moderation.stan\n")
cat("   → Focus su interazione Disinhibition × Stress\n\n")

cat("4. IDEE ALTERNATIVE se troppo rumore:\n")
cat("   a) Formant dispersion (F2-F1) come misura precisione\n")
cat("   b) Spectral tilt (misura generale tension)\n")
cat("   c) Speaking rate (se disponibile)\n")
cat("   d) Duration vocali (se disponibile)\n\n")

cat(rep("=", 80), "\n\n", sep = "")

# eof ---
