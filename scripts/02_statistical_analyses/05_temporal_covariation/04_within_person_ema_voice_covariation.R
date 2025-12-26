# ==============================================================================
# 04_within_person_ema_voice_covariation.R
# Analisi covariazione within-person tra PID-5 EMA momentaneo e voce
#
# OBIETTIVO:
# Esplorare se le fluttuazioni momentanee del PID-5 EMA (deviazioni dal proprio
# livello medio) covariino con le caratteristiche vocali nei 3 timepoint in cui
# entrambe le misure sono disponibili.
#
# STRATEGIA:
# 1. Identificare le misurazioni EMA più vicine ai 3 timepoint vocali
# 2. Calcolare deviazioni within-person (centrate sulla media individuale)
# 3. Analizzare correlazioni within-person tra PID-5 e voce
# 4. (Opzionale) Modello Stan per inferenza robusta
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
  library(rio)
  library(lubridate)
  library(ggplot2)
  library(corrplot)
  library(jsonlite)
})

# ==============================================================================
# 0) CONFIGURAZIONE
# ==============================================================================

# Percorsi file
voice_path <- here::here(
  "data",
  "raw",
  "acustic_features",
  "datiacustici",
  "AUDIO.xlsx"
)
ema_path <- here::here("data", "processed", "ema_plus_scales_cleaned.csv")
meta_path <- here::here("data", "raw", "meta", "all_combined_sex_NEW_1.xlsx")

# Output
output_dir <- "results/within_person_covariation"
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
# 1) CARICAMENTO DATI VOCALI
# ==============================================================================

cat("=== CARICAMENTO DATI VOCALI ===\n")

# Carica 3 sheet (baseline, pre, post)
baseline <- read_excel(voice_path, sheet = "BASELINE") %>%
  mutate(timepoint = "baseline")
pre <- read_excel(voice_path, sheet = "PRE") %>%
  mutate(timepoint = "pre")
post <- read_excel(voice_path, sheet = "POST") %>%
  mutate(timepoint = "post")

df_voice <- bind_rows(baseline, pre, post)
names(df_voice) <- str_trim(names(df_voice))

# Correzione ID (come negli script precedenti)
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

# Seleziona caratteristiche acustiche di interesse
# Prima controlla se NNE esiste
has_nne <- "NNE (mean) dB" %in% names(df_voice)

# Crea dataframe con caratteristiche vocali
df_voice <- df_voice |>
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
  ) |>
  # Gestisci NA in modo esplicito per f0_mean
  mutate(
    f0_mean = if_else(
      is.na(f0_mean_a) & is.na(f0_mean_i) & is.na(f0_mean_u),
      NA_real_,
      f0_mean
    )
  )

cat(sprintf(
  "Dati vocali: %d osservazioni, %d soggetti\n",
  nrow(df_voice),
  n_distinct(df_voice$user_id)
))

if (has_nne) {
  cat("  ✓ NNE disponibile\n")
} else {
  cat("  ⚠ NNE non disponibile in questo dataset\n")
}

# ==============================================================================
# 2) CARICAMENTO DATI EMA
# ==============================================================================

cat("\n=== CARICAMENTO DATI EMA ===\n")

ema <- rio::import(ema_path) %>%
  as_tibble() %>%
  mutate(
    # Assicura che day sia Date
    day = as.Date(day),
    # Converti exam_period in factor se non lo è già
    exam_period = factor(
      exam_period,
      levels = c("baseline", "pre_exam", "post_exam")
    )
  )

# Verifica presenza variabili necessarie
stopifnot(all(
  c("user_id", "day", "exam_period", pid5_ema_vars) %in% names(ema)
))

cat(sprintf(
  "Dati EMA: %d osservazioni, %d soggetti\n",
  nrow(ema),
  n_distinct(ema$user_id)
))

# ==============================================================================
# 3) CARICAMENTO METADATI (per date esami)
# ==============================================================================

cat("\n=== CARICAMENTO METADATI ===\n")

meta <- rio::import(meta_path) %>%
  rename(
    user_id = any_of(c("subj_code", "user_id", "id_anon")),
    date_meta = any_of(c("date", "Date", "data")),
    course = any_of(c("course", "Corso", "insegnamento"))
  ) %>%
  mutate(
    date_meta = as.Date(date_meta, format = "%d_%m_%Y"),
    course = case_when(
      str_detect(course, regex("^psico", ignore_case = TRUE)) ~ "Psicometria",
      str_detect(course, regex("^test", ignore_case = TRUE)) ~ "Testing",
      str_detect(course, regex("^inter", ignore_case = TRUE)) ~ "Interventi",
      TRUE ~ course
    )
  ) %>%
  dplyr::select(user_id, course, date_meta)

# ==============================================================================
# 4) IDENTIFICAZIONE MISURAZIONI EMA VICINE AI TIMEPOINT VOCALI
# ==============================================================================

cat("\n=== MATCHING TEMPORALE EMA-VOCE ===\n")

# Strategia: per ogni soggetto e timepoint vocale, trova le misurazioni EMA
# che cadono nello stesso exam_period

# Crea mappa timepoint vocale -> exam_period EMA
timepoint_map <- tibble(
  timepoint = c("baseline", "pre", "post"),
  exam_period = factor(
    c("baseline", "pre_exam", "post_exam"),
    levels = c("baseline", "pre_exam", "post_exam")
  )
)

# Prepara dati EMA con PID-5 completo
ema_pid5 <- ema %>%
  dplyr::select(user_id, day, exam_period, all_of(pid5_ema_vars)) %>%
  # Rimuovi righe con tutti NA sui domini PID-5
  dplyr::filter(if_any(all_of(pid5_ema_vars), ~ !is.na(.x)))

# Join voce con timepoint_map
df_voice_matched <- df_voice %>%
  left_join(timepoint_map, by = "timepoint")

# Per ogni combinazione soggetto-timepoint vocale, trova misurazioni EMA
ema_matched <- df_voice_matched %>%
  dplyr::select(user_id, timepoint, exam_period) %>%
  left_join(
    ema_pid5,
    by = c("user_id", "exam_period"),
    relationship = "many-to-many"
  )

cat(sprintf(
  "Matching EMA-voce: %d misurazioni EMA abbinate a %d osservazioni vocali\n",
  nrow(ema_matched),
  nrow(df_voice)
))

# Conta quante misurazioni EMA per timepoint vocale
n_ema_per_timepoint <- ema_matched %>%
  group_by(user_id, timepoint) %>%
  summarise(n_ema = n(), .groups = "drop")

cat("\nDistribuzione misurazioni EMA per timepoint vocale:\n")
print(summary(n_ema_per_timepoint$n_ema))

# ==============================================================================
# 5) AGGREGAZIONE: MEDIA EMA PER TIMEPOINT VOCALE
# ==============================================================================

cat("\n=== AGGREGAZIONE EMA PER TIMEPOINT ===\n")

# Strategia: per ogni soggetto-timepoint, calcola la media delle misurazioni
# EMA disponibili in quel periodo

ema_aggregated <- ema_matched %>%
  group_by(user_id, timepoint) %>%
  summarise(
    across(
      all_of(pid5_ema_vars),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_mean"
    ),
    n_ema = n(),
    .groups = "drop"
  )

# ==============================================================================
# 6) MERGE VOCE + EMA AGGREGATO
# ==============================================================================

cat("\n=== MERGE VOCE + EMA ===\n")

df_combined <- df_voice %>%
  left_join(
    ema_aggregated,
    by = c("user_id", "timepoint")
  ) %>%
  # Rimuovi casi con PID-5 mancante
  dplyr::filter(if_any(ends_with("_mean"), ~ !is.nan(.x) & !is.na(.x)))

cat(sprintf(
  "Dataset combinato: %d osservazioni, %d soggetti\n",
  nrow(df_combined),
  n_distinct(df_combined$user_id)
))

# ==============================================================================
# 7) CALCOLO DEVIAZIONI WITHIN-PERSON
# ==============================================================================

cat("\n=== CALCOLO DEVIAZIONI WITHIN-PERSON ===\n")

# Calcola person means
person_means <- df_combined %>%
  group_by(user_id) %>%
  summarise(
    across(
      ends_with("_mean"),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_pm"
    ),
    # Media voce
    f0_mean_pm = mean(f0_mean, na.rm = TRUE),
    nne_pm = mean(nne, na.rm = TRUE),
    .groups = "drop"
  )

# Merge person means e calcola deviazioni
pid5_mean_vars <- paste0(pid5_ema_vars, "_mean")

df_within <- df_combined %>%
  left_join(person_means, by = "user_id") %>%
  mutate(
    # Deviazioni PID-5 (within-person)
    across(
      all_of(pid5_mean_vars),
      ~ .x - get(paste0(cur_column(), "_pm")),
      .names = "{.col}_wp"
    ),
    # Deviazioni voce (within-person)
    f0_mean_wp = f0_mean - f0_mean_pm,
    nne_wp = nne - nne_pm
  ) %>%
  # Rinomina le deviazioni WP per coerenza (rimuove _mean intermedio)
  rename_with(
    ~ str_replace(.x, "_mean_wp$", "_wp"),
    ends_with("_mean_wp")
  ) %>%
  # FILTRO: rimuovi righe con NaN nei domini PID-5
  filter(
    if_all(
      all_of(paste0(pid5_ema_vars, "_wp")),
      ~ is.finite(.x)
    )
  ) %>%
  # Seleziona colonne rilevanti
  select(
    user_id,
    timepoint,
    # Caratteristiche vocali
    f0_mean,
    f0_wp, # ← Dopo rename_with diventa f0_wp (non f0_mean_wp)
    f0_mean_pm,
    nne,
    nne_wp,
    nne_pm,
    # PID-5 aggregato per timepoint
    all_of(pid5_mean_vars),
    # PID-5 within-person deviations
    all_of(paste0(pid5_ema_vars, "_wp")),
    # Person means PID-5
    all_of(paste0(pid5_mean_vars, "_pm")),
    # N misurazioni EMA
    n_ema
  )

cat(sprintf(
  "Dataset with within-person deviations: %d righe\n",
  nrow(df_within)
))

# Report soggetti esclusi
n_subj_total <- n_distinct(df_combined$user_id)
n_subj_kept <- n_distinct(df_within$user_id)

cat(sprintf(
  "Soggetti con dati validi: %d/%d (%.1f%%)\n",
  n_subj_kept,
  n_subj_total,
  100 * n_subj_kept / n_subj_total
))

# ==============================================================================
# 8) ANALISI DESCRITTIVE
# ==============================================================================

cat("\n=== STATISTICHE DESCRITTIVE ===\n")

# Variabilità within vs between
var_decomp <- df_within %>%
  summarise(
    # F0
    var_f0_total = var(f0_mean, na.rm = TRUE),
    var_f0_between = var(f0_mean_pm, na.rm = TRUE),
    var_f0_within = mean((f0_wp)^2, na.rm = TRUE),
    icc_f0 = var_f0_between / var_f0_total,

    # PID-5 domains (esempio: Negative Affectivity)
    var_na_total = var(pid5_negative_affectivity_mean, na.rm = TRUE),
    var_na_between = var(pid5_negative_affectivity_mean_pm, na.rm = TRUE),
    var_na_within = mean((pid5_negative_affectivity_wp)^2, na.rm = TRUE),
    icc_na = var_na_between / var_na_total
  )

cat("\nVariance decomposition:\n")
print(var_decomp)

# Salva
write_csv(
  var_decomp,
  file.path(output_dir, "variance_decomposition.csv")
)

# ==============================================================================
# 9) CORRELAZIONI WITHIN-PERSON
# ==============================================================================

cat("\n=== CORRELAZIONI WITHIN-PERSON ===\n")

# Verifica disponibilità NNE
nne_available <- sum(!is.na(df_within$nne)) > 0

if (!nne_available) {
  cat("⚠ NNE non disponibile in questo dataset - escluso dall'analisi\n\n")
}

# Crea matrice di deviazioni within-person
# Includi NNE solo se disponibile
within_vars <- c(
  "f0_wp",
  paste0(pid5_ema_vars, "_wp")
)

if (nne_available) {
  within_vars <- c(within_vars, "nne_wp")
}

# Rimuovi NA
df_corr <- df_within %>%
  dplyr::select(all_of(within_vars)) %>%
  na.omit()

cat(sprintf("Osservazioni valide per correlazioni: %d\n", nrow(df_corr)))

if (nrow(df_corr) == 0) {
  stop("Nessuna osservazione valida per le correlazioni!")
}

# Correlazioni
cor_within <- cor(df_corr, use = "pairwise.complete.obs")

cat("\nMatrice correlazioni within-person:\n")
print(round(cor_within, 3))

# Salva
write_csv(
  as_tibble(cor_within, rownames = "variable"),
  file.path(output_dir, "within_person_correlations.csv")
)

# Plot correlazioni
pdf(
  file.path(output_dir, "within_person_correlations.pdf"),
  width = 10,
  height = 8
)
corrplot(
  cor_within,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.7,
  tl.cex = 0.8,
  tl.col = "black",
  title = "Within-Person Correlations: Voice & PID-5 EMA",
  mar = c(0, 0, 2, 0)
)
dev.off()

cat("✓ Grafico correlazioni salvato\n")

# ==============================================================================
# 10) ANALISI PER DOMINIO PID-5
# ==============================================================================

cat("\n=== CORRELAZIONI PER DOMINIO PID-5 ===\n")

# Focus su F0 mean
cor_results <- tibble(
  domain = pid5_ema_vars,
  cor_f0_within = NA_real_,
  p_value = NA_real_,
  n_obs = NA_real_
)

for (i in seq_along(pid5_ema_vars)) {
  domain <- pid5_ema_vars[i]
  wp_var <- paste0(domain, "_wp")

  # Rimuovi NA
  tmp <- df_within %>%
    dplyr::select(f0_wp, all_of(wp_var)) %>%
    na.omit()

  if (nrow(tmp) > 3) {
    test <- cor.test(tmp$f0_wp, tmp[[wp_var]])
    cor_results$cor_f0_within[i] <- test$estimate
    cor_results$p_value[i] <- test$p.value
    cor_results$n_obs[i] <- nrow(tmp)
  }
}

cat("\nCorrelazioni within-person: F0 mean ~ PID-5 domains\n")
print(cor_results)

# Salva
write_csv(
  cor_results,
  file.path(output_dir, "f0_pid5_within_correlations.csv")
)

# Se NNE disponibile, fai analisi separata
if (nne_available) {
  cat("\n=== CORRELAZIONI NNE (se disponibile) ===\n")

  cor_results_nne <- tibble(
    domain = pid5_ema_vars,
    cor_nne_within = NA_real_,
    p_value = NA_real_,
    n_obs = NA_real_
  )

  for (i in seq_along(pid5_ema_vars)) {
    domain <- pid5_ema_vars[i]
    wp_var <- paste0(domain, "_wp")

    # Rimuovi NA
    tmp <- df_within %>%
      dplyr::select(nne_wp, all_of(wp_var)) %>%
      na.omit()

    if (nrow(tmp) > 3) {
      test <- cor.test(tmp$nne_wp, tmp[[wp_var]])
      cor_results_nne$cor_nne_within[i] <- test$estimate
      cor_results_nne$p_value[i] <- test$p.value
      cor_results_nne$n_obs[i] <- nrow(tmp)
    }
  }

  cat("\nCorrelazioni within-person: NNE ~ PID-5 domains\n")
  print(cor_results_nne)

  write_csv(
    cor_results_nne,
    file.path(output_dir, "nne_pid5_within_correlations.csv")
  )
}

# ==============================================================================
# 11) VISUALIZZAZIONI
# ==============================================================================

cat("\n=== CREAZIONE GRAFICI ===\n")

# Plot: deviazioni within-person per dominio
for (domain in pid5_ema_vars) {
  wp_var <- paste0(domain, "_wp")

  p <- ggplot(df_within, aes(x = .data[[wp_var]], y = f0_wp)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = paste("Within-Person Covariation:", domain),
      x = paste0(domain, " (deviation from person mean)"),
      y = "F0 mean (deviation from person mean)",
      caption = sprintf(
        "r = %.3f, p = %.3f",
        cor_results$cor_f0_within[which(pid5_ema_vars == domain)],
        cor_results$p_value[which(pid5_ema_vars == domain)]
      )
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  ggsave(
    file.path(output_dir, paste0("wp_scatter_", domain, ".pdf")),
    p,
    width = 8,
    height = 6
  )
}

cat("✓ Grafici scatter per dominio salvati (5 file)\n")

# Plot: distribuzione deviazioni per timepoint
p_dist <- df_within %>%
  dplyr::select(timepoint, all_of(paste0(pid5_ema_vars, "_wp"))) %>%
  pivot_longer(
    cols = ends_with("_wp"),
    names_to = "domain",
    values_to = "deviation"
  ) %>%
  mutate(domain = str_remove(domain, "_wp")) %>%
  ggplot(aes(x = timepoint, y = deviation, fill = timepoint)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~domain, scales = "free_y", ncol = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Within-Person Deviations by Timepoint",
    y = "Deviation from person mean",
    x = "Timepoint"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave(
  file.path(output_dir, "wp_deviations_by_timepoint.pdf"),
  p_dist,
  width = 10,
  height = 12
)

cat("✓ Grafico distribuzione deviazioni salvato\n")

# ==============================================================================
# 12) PREPARAZIONE DATI PER STAN (OPZIONALE)
# ==============================================================================

cat("\n=== PREPARAZIONE DATI STAN ===\n")

# Crea dataset completo per Stan
df_stan <- df_within %>%
  # Rimuovi righe con NA su F0 o PID-5 (dovrebbero essere già stati rimossi)
  filter(
    !is.na(f0_wp),
    if_all(all_of(paste0(pid5_ema_vars, "_wp")), ~ !is.na(.x))
  ) %>%
  arrange(user_id, timepoint) %>%
  mutate(
    subj = as.integer(factor(user_id)),
    time = as.integer(factor(timepoint, levels = c("baseline", "pre", "post")))
  )

cat(sprintf(
  "Dati Stan: %d osservazioni, %d soggetti\n",
  nrow(df_stan),
  n_distinct(df_stan$subj)
))

# Soggetti unici
subj_ids <- df_stan %>%
  distinct(user_id, subj) %>%
  arrange(subj)

N_subj <- nrow(subj_ids)

# Matrice PID-5 within-person deviations
X_wp <- df_stan %>%
  select(all_of(paste0(pid5_ema_vars, "_wp"))) %>%
  as.matrix()

# Outcome: F0 within-person
# CORREZIONE: usa f0_wp (non f0_mean_wp)
y_wp <- df_stan$f0_wp

# Verifica no NA
stopifnot(!anyNA(X_wp))
stopifnot(!anyNA(y_wp))

# Stan data list
stan_data_wp <- list(
  N = nrow(df_stan),
  N_subj = N_subj,
  D = length(pid5_ema_vars),
  subj = df_stan$subj,
  time = df_stan$time,
  X_wp = X_wp,
  y_wp = y_wp
)

# Salva
write_json(
  stan_data_wp,
  path = file.path(output_dir, "stan_data_within_person.json"),
  digits = 16,
  auto_unbox = TRUE
)

saveRDS(
  list(
    stan_data = stan_data_wp,
    df_within = df_within,
    df_stan = df_stan,
    subj_ids = subj_ids,
    pid5_vars = pid5_ema_vars,
    nne_available = nne_available
  ),
  file = file.path(output_dir, "within_person_bundle.rds")
)

cat(
  "\nDati Stan salvati in:",
  file.path(output_dir, "stan_data_within_person.json"),
  "\n"
)

# ==============================================================================
# 13) REPORT FINALE
# ==============================================================================

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("ANALISI COMPLETATA\n")
cat(rep("=", 80), "\n", sep = "")
cat("\nOUTPUT SALVATI IN:", output_dir, "\n")
cat("\nFile creati:\n")
cat("  - variance_decomposition.csv\n")
cat("  - within_person_correlations.csv\n")
cat("  - f0_pid5_within_correlations.csv\n")
cat("  - within_person_correlations.pdf\n")
cat("  - wp_scatter_[domain].pdf (5 grafici)\n")
cat("  - wp_deviations_by_timepoint.pdf\n")
cat("  - stan_data_within_person.json\n")
cat("  - within_person_bundle.rds\n")

if (nne_available) {
  cat("  - nne_pid5_within_correlations.csv\n")
}

cat(rep("=", 80), "\n\n", sep = "")

cat("✓ Tutto completato con successo!\n")
cat("\nProssimi passi:\n")
cat("  1. Esamina i risultati in:", output_dir, "\n")
cat("  2. Controlla correlazioni in: f0_pid5_within_correlations.csv\n")
cat(
  "  3. (Opzionale) Esegui modello Stan: source('05_fit_stan_within_person_model.R')\n\n"
)

# eof ---
