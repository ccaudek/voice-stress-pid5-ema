# ==============================================================================
# 01_prepare_within_person_data.R
# Preparazione dati per analisi covariazione within-person
#
# INPUT:
#   - AUDIO.xlsx (dati vocali: baseline, pre, post)
#   - ema_plus_scales_cleaned.csv (EMA giornalieri)
#   - all_combined_sex_NEW_1.xlsx (metadati)
#
# OUTPUT:
#   - stan_data_within_person.rds (dati pronti per Stan)
#   - metadata.rds (informazioni ausiliarie)
#   - df_within_full.rds (dataset completo per analisi esplorative)
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
  library(rio)
  library(lubridate)
})

cat("=== PREPARAZIONE DATI WITHIN-PERSON ===\n\n")

# ==============================================================================
# CONFIGURAZIONE
# ==============================================================================

# Percorsi file
voice_path <- here::here("data/raw/acustic_features/datiacustici/AUDIO.xlsx")
ema_path <- here::here("data/processed/ema_plus_scales_cleaned.csv")
meta_path <- here::here("data/raw/meta/all_combined_sex_NEW_1.xlsx")

# Output
output_dir <- here::here("results/within_person_final/data")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Domini PID-5 EMA
pid5_vars <- c(
  "pid5_negative_affectivity",
  "pid5_detachment",
  "pid5_antagonism",
  "pid5_disinhibition",
  "pid5_psychoticism"
)

# ==============================================================================
# 1) CARICA DATI VOCALI
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

# Correzione ID (errori noti nel dataset)
df_voice <- df_voice %>%
  mutate(
    ID = case_when(
      ID == "am_bo_1988_08_24_166" ~ "an_bo_1988_08_24_166",
      ID == "as_li_2005_04_26_447" ~ "as_si_2005_04_26_447",
      ID == "cl_bo_1987_10_16_628" ~ "ca_bo_1987_10_16_628",
      ID == "hi_na_2005_03_08_339" ~ "gi_na_2005_03_08_339",
      ID == "ma_si_2003_10_31_940" ~ "si_ma_2003_10_31_940",
      TRUE ~ ID
    )
  ) %>%
  rename(user_id = ID)

# Estrai caratteristiche vocali
has_nne <- "NNE (mean) dB" %in% names(df_voice)

df_voice <- df_voice %>%
  transmute(
    user_id,
    timepoint = factor(timepoint, levels = c("baseline", "pre", "post")),
    # F0 mean per vocale
    f0_mean_a = `F0 mean Hz /a/`,
    f0_mean_i = `F0 mean Hz /i/`,
    f0_mean_u = `F0 mean Hz /u/`,
    # F0 aggregato (media sulle vocali)
    f0_mean = (`F0 mean Hz /a/` + `F0 mean Hz /i/` + `F0 mean Hz /u/`) / 3,
    # NNE (se disponibile)
    nne = if (has_nne) `NNE (mean) dB` else NA_real_
  ) %>%
  # Gestisci NA
  mutate(
    f0_mean = if_else(
      is.na(f0_mean_a) & is.na(f0_mean_i) & is.na(f0_mean_u),
      NA_real_,
      f0_mean
    )
  )

cat(sprintf(
  "  Dati vocali: %d osservazioni, %d soggetti\n",
  nrow(df_voice),
  n_distinct(df_voice$user_id)
))

if (has_nne) {
  cat("  ✓ NNE disponibile\n")
} else {
  cat("  ⚠ NNE non disponibile\n")
}

# ==============================================================================
# 2) CARICA DATI EMA
# ==============================================================================

cat("\n=== CARICAMENTO DATI EMA ===\n")

ema <- rio::import(ema_path) %>%
  as_tibble() %>%
  mutate(
    day = as.Date(day),
    exam_period = factor(
      exam_period,
      levels = c("baseline", "pre_exam", "post_exam")
    )
  )

# Verifica presenza variabili
stopifnot(all(c("user_id", "day", "exam_period", pid5_vars) %in% names(ema)))

cat(sprintf(
  "  Dati EMA: %d osservazioni, %d soggetti\n",
  nrow(ema),
  n_distinct(ema$user_id)
))

# ==============================================================================
# 3) MATCHING TEMPORALE EMA-VOCE
# ==============================================================================

cat("\n=== MATCHING TEMPORALE EMA-VOCE ===\n")

# Mappa timepoint vocale -> exam_period EMA
timepoint_map <- tibble(
  timepoint = c("baseline", "pre", "post"),
  exam_period = factor(
    c("baseline", "pre_exam", "post_exam"),
    levels = c("baseline", "pre_exam", "post_exam")
  )
)

# Prepara EMA con PID-5
ema_pid5 <- ema %>%
  select(user_id, day, exam_period, all_of(pid5_vars)) %>%
  filter(if_any(all_of(pid5_vars), ~ !is.na(.x)))

# Join voce con EMA
df_voice_matched <- df_voice %>%
  left_join(timepoint_map, by = "timepoint")

ema_matched <- df_voice_matched %>%
  select(user_id, timepoint, exam_period) %>%
  left_join(
    ema_pid5,
    by = c("user_id", "exam_period"),
    relationship = "many-to-many"
  )

cat(sprintf(
  "  %d misurazioni EMA abbinate a %d osservazioni vocali\n",
  nrow(ema_matched),
  nrow(df_voice)
))

# ==============================================================================
# 4) AGGREGAZIONE EMA PER TIMEPOINT
# ==============================================================================

cat("\n=== AGGREGAZIONE EMA PER TIMEPOINT ===\n")

# Media EMA per ogni soggetto-timepoint
ema_aggregated <- ema_matched %>%
  group_by(user_id, timepoint) %>%
  summarise(
    across(
      all_of(pid5_vars),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_mean"
    ),
    n_ema = n(),
    .groups = "drop"
  )

cat(sprintf(
  "  %d soggetti × timepoint con EMA aggregato\n",
  nrow(ema_aggregated)
))

# ==============================================================================
# 5) MERGE VOCE + EMA
# ==============================================================================

cat("\n=== MERGE VOCE + EMA ===\n")

df_combined <- df_voice %>%
  left_join(ema_aggregated, by = c("user_id", "timepoint")) %>%
  dplyr::filter(if_any(ends_with("_mean"), ~ !is.nan(.x) & !is.na(.x)))

cat(sprintf(
  "  Dataset combinato: %d osservazioni, %d soggetti\n",
  nrow(df_combined),
  n_distinct(df_combined$user_id)
))

# ==============================================================================
# 6) CALCOLO DEVIAZIONI WITHIN-PERSON
# ==============================================================================

cat("\n=== CALCOLO DEVIAZIONI WITHIN-PERSON ===\n")

# Person means
person_means <- df_combined %>%
  group_by(user_id) %>%
  summarise(
    across(
      ends_with("_mean"),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_pm"
    ),
    f0_mean_pm = mean(f0_mean, na.rm = TRUE),
    nne_pm = mean(nne, na.rm = TRUE),
    .groups = "drop"
  )

# Calcola deviazioni
pid5_mean_vars <- paste0(pid5_vars, "_mean")

df_within <- df_combined %>%
  left_join(person_means, by = "user_id") %>%
  mutate(
    # Deviazioni PID-5 (within-person)
    across(
      all_of(pid5_mean_vars),
      ~ .x - get(paste0(cur_column(), "_pm")),
      .names = "{str_remove(.col, '_mean')}_wp"
    ),
    # Deviazioni voce
    f0_wp = f0_mean - f0_mean_pm,
    nne_wp = nne - nne_pm
  )

cat(sprintf(
  "  Dataset within-person: %d osservazioni, %d soggetti\n",
  nrow(df_within),
  n_distinct(df_within$user_id)
))

# Statistiche descrittive
cat("\n  Statistiche within-person deviations:\n")
wp_vars <- paste0(pid5_vars, "_wp")
for (var in wp_vars) {
  sd_val <- sd(df_within[[var]], na.rm = TRUE)
  cat(sprintf("    %s: SD = %.3f\n", var, sd_val))
}
cat(sprintf("    f0_wp: SD = %.3f Hz\n", sd(df_within$f0_wp, na.rm = TRUE)))

# ==============================================================================
# 7) PREPARA DATI PER STAN
# ==============================================================================

cat("\n=== PREPARAZIONE DATI STAN ===\n")

# Filtra osservazioni complete
df_model <- df_within %>%
  dplyr::filter(
    !is.na(f0_wp),
    if_all(all_of(wp_vars), ~ !is.na(.x))
  ) %>%
  arrange(user_id, timepoint) %>%
  mutate(subj = as.integer(factor(user_id)))

# Matrice predictori
X_wp <- df_model %>%
  dplyr::select(all_of(wp_vars)) %>%
  as.matrix()

# Outcome
y_wp <- df_model$f0_wp

# Stan data list
stan_data <- list(
  N = nrow(df_model),
  N_subj = n_distinct(df_model$subj),
  D = length(pid5_vars),
  subj = df_model$subj,
  X_wp = X_wp,
  y_wp = y_wp
)

# Verifica
stopifnot(!anyNA(stan_data$X_wp))
stopifnot(!anyNA(stan_data$y_wp))

cat(sprintf("  N = %d osservazioni\n", stan_data$N))
cat(sprintf("  N_subj = %d soggetti\n", stan_data$N_subj))
cat(sprintf("  D = %d domini PID-5\n", stan_data$D))
cat(sprintf("  Media obs per soggetto: %.1f\n", stan_data$N / stan_data$N_subj))

# ==============================================================================
# 8) SALVATAGGIO
# ==============================================================================

cat("\n=== SALVATAGGIO ===\n")

# Dati Stan
saveRDS(stan_data, file.path(output_dir, "stan_data_within_person.rds"))
cat("  ✓ stan_data_within_person.rds\n")

# Metadata per analisi successive
metadata <- list(
  df_model = df_model,
  pid5_vars = pid5_vars,
  N_obs_per_subj = df_model %>%
    group_by(subj, user_id) %>%
    summarise(n_obs = n(), .groups = "drop"),
  person_means = person_means
)

saveRDS(metadata, file.path(output_dir, "metadata.rds"))
cat("  ✓ metadata.rds\n")

# Dataset completo per eventuali analisi esplorative
saveRDS(df_within, file.path(output_dir, "df_within_full.rds"))
cat("  ✓ df_within_full.rds\n")

# ==============================================================================
# SUMMARY FINALE
# ==============================================================================

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("✓ PREPARAZIONE DATI COMPLETATA\n")
cat(rep("=", 80), "\n\n")

cat("DATASET FINALE:\n")
cat(sprintf("  - %d soggetti\n", stan_data$N_subj))
cat(sprintf("  - %d osservazioni totali\n", stan_data$N))
cat(sprintf(
  "  - ~%.1f osservazioni per soggetto\n",
  stan_data$N / stan_data$N_subj
))
cat(sprintf("  - %d domini PID-5\n", stan_data$D))

cat("\nFILE SALVATI IN:\n")
cat(sprintf("  %s/\n", output_dir))

cat("\n✓ Prossimo step: source('02_fit_models.R')\n\n")

# eof ---
