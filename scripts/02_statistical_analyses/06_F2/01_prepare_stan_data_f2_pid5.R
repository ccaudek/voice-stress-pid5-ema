# ==============================================================================
# 01_prepare_stan_data_f2_pid5.R
# Prepara dati per Stan:
# - Outcome: metriche F2/articolatorie derivate da F1-F2 sulle vocali /a i u/
# - Moderatori: 5 domini PID-5 EMA (latent trait con errore di misura)
# - Contrasti: c1_stress (PRE vs BASELINE), c2_recovery (POST vs PRE)
# - IMPUTAZIONE: within-subject sui 5 domini PID-5 EMA (per rimuovere NA in X)
# - OUTCOME: standardizzato in R; y_center e y_scale salvati nel bundle
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
  library(rio)
  library(jsonlite)
})

# ----------------------------
# 0) PATHS
# ----------------------------
voice_path <- here(
  "data",
  "raw",
  "acustic_features",
  "datiacustici",
  "AUDIO.xlsx"
)
ema_path <- here("data", "processed", "ema_plus_scales_cleaned.csv")

stopifnot(file.exists(voice_path))
stopifnot(file.exists(ema_path))

stan_f2_dir <- here("stan", "F2")
results_f2_dir <- here("results", "F2")
results_f2_data_dir <- here("results", "F2", "data")

dir.create(stan_f2_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_f2_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_f2_data_dir, recursive = TRUE, showWarnings = FALSE)

# Metriche da esportare. f2_range_norm e centralization sono le candidate piu robuste.
# f2_slope e utile come controllo, ma puo essere instabile se F1_i ~= F1_u.
f2_outcomes <- c(
  "f2_mean",
  "f2_range",
  "f2_range_norm",
  "vsa_log",
  "centralization",
  "f2_slope"
)

# ----------------------------
# 1) LOAD VOICE (3 timepoint)
# ----------------------------
baseline <- read_excel(voice_path, sheet = "BASELINE") %>%
  mutate(timepoint = "baseline")
pre <- read_excel(voice_path, sheet = "PRE") %>%
  mutate(timepoint = "pre")
post <- read_excel(voice_path, sheet = "POST") %>%
  mutate(timepoint = "post")

df_voice <- bind_rows(baseline, pre, post)
names(df_voice) <- stringr::str_trim(names(df_voice))

# Correzione ID, mantenuta parallela allo script F0
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
  )

# Estrai F1 e F2 per tutte le vocali
df_voice <- df_voice |>
  transmute(
    ID,
    timepoint = factor(timepoint, levels = c("baseline", "pre", "post")),
    f1_a = as.numeric(`F1 mean Hz /a/`),
    f1_i = as.numeric(`F1 mean Hz /i/`),
    f1_u = as.numeric(`F1 mean Hz /u/`),
    f2_a = as.numeric(`F2 mean Hz /a/`),
    f2_i = as.numeric(`F2 mean Hz /i/`),
    f2_u = as.numeric(`F2 mean Hz /u/`)
  )

cat("\n=== CALCOLO METRICHE F2 / ARTICOLATORIE ===\n")

df_voice <- df_voice |>
  mutate(
    # Media F2 sulle tre vocali; se almeno una vocale e disponibile, usa quella/e disponibile/i.
    f2_mean = rowMeans(across(c(f2_a, f2_i, f2_u)), na.rm = TRUE),
    f2_mean = if_else(is.nan(f2_mean), NA_real_, f2_mean),

    # Range F2 /i-u/: avanzamento-riduzione lungo il continuum anteriore-posteriore.
    f2_range = f2_i - f2_u,

    # F1 medio, usato come normalizzatore di scala anatomico/acustica.
    f1_mean = rowMeans(across(c(f1_a, f1_i, f1_u)), na.rm = TRUE),
    f1_mean = if_else(is.nan(f1_mean), NA_real_, f1_mean),
    f2_range_norm = if_else(
      is.finite(f2_range) & is.finite(f1_mean) & f1_mean > 0,
      f2_range / f1_mean,
      NA_real_
    ),

    # Vowel Space Area nel piano F1-F2, formula Shoelace.
    vsa = 0.5 *
      abs(
        f1_a * (f2_i - f2_u) + f1_i * (f2_u - f2_a) + f1_u * (f2_a - f2_i)
      ),
    vsa_log = if_else(is.finite(vsa) & vsa > 0, log(vsa), NA_real_),

    # Slope i -> u nel piano F1-F2. Guard rail per denominatori quasi nulli.
    f1_i_minus_u = f1_i - f1_u,
    f2_slope = if_else(
      is.finite(f2_range) & is.finite(f1_i_minus_u) & abs(f1_i_minus_u) > 10,
      f2_range / f1_i_minus_u,
      NA_real_
    ),

    # Centralization: distanza euclidea media delle tre vocali dal centroide.
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

metric_report <- tibble(metric = f2_outcomes) |>
  mutate(
    n_na = map_int(metric, ~ sum(!is.finite(df_voice[[.x]]))),
    pct_na = 100 * n_na / nrow(df_voice),
    n_valid = nrow(df_voice) - n_na
  )

print(metric_report)

# Contrasti
df_voice <- df_voice |>
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
  ) |>
  filter(!is.na(ID), !is.na(c1_stress), !is.na(c2_recovery))

cat(
  "VOICE: N obs =",
  nrow(df_voice),
  "| N subj =",
  n_distinct(df_voice$ID),
  "\n"
)

# ----------------------------
# 2) LOAD EMA CLEANED
# ----------------------------
ema <- rio::import(ema_path) |>
  as_tibble()

pid5_ema_vars <- c(
  "pid5_negative_affectivity",
  "pid5_detachment",
  "pid5_antagonism",
  "pid5_disinhibition",
  "pid5_psychoticism"
)

stopifnot(all(c("user_id", pid5_ema_vars) %in% names(ema)))

df_ema <- ema |>
  transmute(
    ID = user_id,
    across(all_of(pid5_ema_vars), as.numeric)
  ) |>
  filter(!is.na(ID)) |>
  filter(ID %in% unique(df_voice$ID)) |>
  # tieni righe con almeno 1 dominio osservato
  filter(if_any(all_of(pid5_ema_vars), ~ !is.na(.x)))

cat(
  "EMA (raw): N rows =",
  nrow(df_ema),
  "| N subj =",
  n_distinct(df_ema$ID),
  "\n"
)

# ----------------------------
# 2b) IMPUTAZIONE within-subject (media soggetto per dominio)
# ----------------------------
na_before <- df_ema |>
  summarise(across(all_of(pid5_ema_vars), ~ sum(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "var", values_to = "n_na_before")

df_ema_imp <- df_ema |>
  group_by(ID) |>
  mutate(across(
    all_of(pid5_ema_vars),
    ~ if_else(is.na(.x), mean(.x, na.rm = TRUE), .x)
  )) |>
  ungroup()

# Elimina eventuali casi patologici (dominio sempre NA per quel soggetto -> mean = NaN)
df_ema_imp <- df_ema_imp |>
  filter(if_all(all_of(pid5_ema_vars), ~ is.finite(.x)))

na_after <- df_ema_imp |>
  summarise(across(all_of(pid5_ema_vars), ~ sum(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "var", values_to = "n_na_after")

na_report <- na_before |>
  left_join(na_after, by = "var") |>
  mutate(n_imputed = n_na_before - n_na_after)

cat("\n=== IMPUTATION REPORT (within-subject means) ===\n")
print(na_report)

cat(
  "\nEMA (imputed): N rows =",
  nrow(df_ema_imp),
  "| N subj =",
  n_distinct(df_ema_imp$ID),
  "\n"
)

# ----------------------------
# 3) FUNZIONE PER ESPORTARE UN OUTCOME ALLA VOLTA
# ----------------------------
make_stan_bundle <- function(outcome_name) {
  stopifnot(outcome_name %in% names(df_voice))

  df_voice_out <- df_voice |>
    transmute(
      ID,
      timepoint,
      y_raw = .data[[outcome_name]],
      c1_stress,
      c2_recovery,
      f1_a,
      f1_i,
      f1_u,
      f2_a,
      f2_i,
      f2_u,
      f2_mean,
      f2_range,
      f2_range_norm,
      vsa,
      vsa_log,
      f2_slope,
      centralization
    ) |>
    filter(is.finite(y_raw))

  subj_ids <- sort(unique(intersect(df_voice_out$ID, df_ema_imp$ID)))
  N_subj <- length(subj_ids)

  if (N_subj < 2) {
    stop("Troppi pochi soggetti per outcome: ", outcome_name)
  }

  id_map <- tibble(ID = subj_ids, subj = seq_len(N_subj))

  df_voice_stan <- df_voice_out |>
    inner_join(id_map, by = "ID") |>
    arrange(subj, timepoint)

  df_ema_stan <- df_ema_imp |>
    inner_join(id_map, by = "ID") |>
    arrange(subj)

  y_center <- mean(df_voice_stan$y_raw)
  y_scale <- sd(df_voice_stan$y_raw)

  if (!is.finite(y_scale) || y_scale <= 0) {
    stop("Outcome con sd nulla/non finita: ", outcome_name)
  }

  df_voice_stan <- df_voice_stan |>
    mutate(y = (y_raw - y_center) / y_scale)

  X <- df_ema_stan |>
    select(all_of(pid5_ema_vars)) |>
    as.matrix()

  stopifnot(!anyNA(X), all(is.finite(X)))

  X_scaled <- scale(X)
  pid5_center <- attr(X_scaled, "scaled:center")
  pid5_scale <- attr(X_scaled, "scaled:scale")

  if (any(!is.finite(X_scaled))) {
    stop("Standardizzazione PID-5 non finita per outcome: ", outcome_name)
  }

  stan_data <- list(
    N_subj = N_subj,

    # voice/articulatory part
    N_voice = nrow(df_voice_stan),
    subj_voice = as.integer(df_voice_stan$subj),
    y = as.numeric(df_voice_stan$y),
    c1 = as.numeric(df_voice_stan$c1_stress),
    c2 = as.numeric(df_voice_stan$c2_recovery),

    # ema measurement part
    N_ema = nrow(df_ema_stan),
    subj_ema = as.integer(df_ema_stan$subj),
    D = length(pid5_ema_vars),
    X = X_scaled
  )

  stopifnot(length(stan_data$y) == stan_data$N_voice)
  stopifnot(ncol(stan_data$X) == stan_data$D)
  stopifnot(!anyNA(stan_data$X), all(is.finite(stan_data$X)))

  stan_json_path <- here(
    "stan",
    "F2",
    paste0("stan_data_", outcome_name, "_pid5.json")
  )

  stan_bundle_path <- here(
    "results",
    "F2",
    "data",
    paste0("stan_bundle_", outcome_name, "_pid5.rds")
  )

  write_json(
    stan_data,
    path = stan_json_path,
    digits = 16,
    auto_unbox = TRUE
  )

  saveRDS(
    list(
      stan_data = stan_data,
      df_voice = df_voice_stan,
      df_ema = df_ema_stan,
      outcome_name = outcome_name,
      y_center = y_center,
      y_scale = y_scale,
      pid5_center = pid5_center,
      pid5_scale = pid5_scale,
      pid5_vars = pid5_ema_vars,
      metric_report = metric_report,
      na_report = na_report
    ),
    file = stan_bundle_path
  )

  tibble(
    outcome = outcome_name,
    N_subj = N_subj,
    N_voice = nrow(df_voice_stan),
    N_ema = nrow(df_ema_stan),
    y_center = y_center,
    y_scale = y_scale,
    stan_json_path = stan_json_path,
    stan_bundle_path = stan_bundle_path
  )
}

# ----------------------------
# 4) EXPORT DI TUTTE LE METRICHE F2
# ----------------------------
export_report <- map_dfr(f2_outcomes, make_stan_bundle)

write.csv(
  export_report,
  file = here("results", "F2", "data", "f2_export_report.csv"),
  row.names = FALSE
)

cat("\n=== EXPORT REPORT ===\n")
print(export_report)

cat("\nSaved bundles in: ", results_f2_data_dir, "\n", sep = "")
cat("Saved Stan JSON files in: ", stan_f2_dir, "\n", sep = "")

# eof ---
