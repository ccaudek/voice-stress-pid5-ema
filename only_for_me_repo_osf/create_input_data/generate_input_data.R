# =============================================================================
# create_analysis_datasets.R
#
# Crea i due file di dati analisi-ready da depositare nel repository:
#   data/processed/pid5_required_columns.csv
#   data/processed/audio_required_columns.csv
#
# Input (in data/processed/):
#   ema_plus_scales_cleaned.csv   — dataset EMA completo (N = 219, post-cleaning)
#   AUDIO.xlsx                    — features acustiche (3 sheet: BASELINE, PRE, POST)
#
# Logica:
#   - Campione finale N = 119 = intersezione tra partecipanti in AUDIO.xlsx
#     (N = 141 reclutati) e partecipanti con dati EMA validi
#   - pid5_required_columns.csv: 12 colonne inclusa exam_period (necessaria
#     per lo script 06_temporal_covariation.R)
#   - audio_required_columns.csv: long-format, solo colonne usate nelle analisi
#
# Run da project root.
# =============================================================================

library(readr)
library(dplyr)
library(readxl)
library(here)

audio_xlsx <- here("data", "processed", "AUDIO.xlsx")
ema_csv <- here("data", "processed", "ema_plus_scales_cleaned.csv")

stopifnot(file.exists(audio_xlsx), file.exists(ema_csv))

# =============================================================================
# 1. Carica dataset EMA e seleziona colonne necessarie
# =============================================================================
df <- read_csv(ema_csv, show_col_types = FALSE)

required_cols <- c(
  "user_id",
  "exam_period", # necessaria per script 06
  "pid5_negative_affectivity",
  "pid5_detachment",
  "pid5_antagonism",
  "pid5_disinhibition",
  "pid5_psychoticism",
  "domain_negative_affect_baseline",
  "domain_detachment_baseline",
  "domain_antagonism_baseline",
  "domain_disinhibition_baseline",
  "domain_psychoticism_baseline"
)

missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop(
    "Colonne mancanti nel dataset EMA: ",
    paste(missing_cols, collapse = ", ")
  )
}

pid5_selected <- df |> dplyr::select(all_of(required_cols))

# =============================================================================
# 2. Identifica gli N = 119 finali (intersezione voce × EMA)
# =============================================================================
id_corrections <- c(
  "am_bo_1988_08_24_166" = "an_bo_1988_08_24_166",
  "as_li_2005_04_26_447" = "as_si_2005_04_26_447",
  "cl_bo_1987_10_16_628" = "ca_bo_1987_10_16_628",
  "hi_na_2005_03_08_339" = "gi_na_2005_03_08_339",
  "ma_si_2003_10_31_940" = "si_ma_2003_10_31_940"
)

voice_ids_raw <- bind_rows(
  read_excel(audio_xlsx, sheet = "BASELINE"),
  read_excel(audio_xlsx, sheet = "PRE"),
  read_excel(audio_xlsx, sheet = "POST")
) |>
  pull(ID) |>
  unique()

voice_ids <- dplyr::recode(voice_ids_raw, !!!id_corrections)
ema_ids <- pid5_selected |> pull(user_id) |> unique()
n_final_ids <- intersect(voice_ids, ema_ids)

cat("In AUDIO.xlsx (reclutati):      N =", length(voice_ids), "\n")
cat("In ema_plus_scales_cleaned.csv: N =", length(ema_ids), "\n")
cat("Intersezione (campione finale): N =", length(n_final_ids), "\n")

stopifnot(length(n_final_ids) == 119)

# =============================================================================
# 3. Verifica NA baseline
# =============================================================================
n_missing_baseline <- pid5_selected |>
  filter(user_id %in% n_final_ids) |>
  distinct(user_id, .keep_all = TRUE) |>
  pull(domain_negative_affect_baseline) |>
  is.na() |>
  sum()

cat(
  "Partecipanti senza dati baseline:",
  n_missing_baseline,
  "(LOO comparison su N =",
  119 - n_missing_baseline,
  ")\n"
)

# =============================================================================
# 4. Salva pid5_required_columns.csv (12 colonne, N = 119)
# =============================================================================
pid5_final <- pid5_selected |>
  filter(user_id %in% n_final_ids)

cat("\npid5_required_columns.csv:\n")
cat(
  "  Righe:",
  nrow(pid5_final),
  "| Partecipanti:",
  n_distinct(pid5_final$user_id),
  "| Colonne:",
  ncol(pid5_final),
  "\n"
)
cat(
  "  exam_period values:",
  paste(
    sort(unique(pid5_final$exam_period[!is.na(pid5_final$exam_period)])),
    collapse = ", "
  ),
  "\n"
)

write_csv(pid5_final, here("data", "processed", "pid5_required_columns.csv"))
cat("Salvato: data/processed/pid5_required_columns.csv\n")

# =============================================================================
# 5. Salva audio_required_columns.csv (long-format, N = 119 × 3 timepoint)
# =============================================================================
audio_cols_needed <- c(
  "ID",
  "F0 mean Hz /a/",
  "F0 mean Hz /i/",
  "F0 mean Hz /u/",
  "NNE /a/",
  "NNE /i/",
  "NNE /u/"
)

read_audio_sheet <- function(sheet) {
  read_excel(audio_xlsx, sheet = sheet) |>
    mutate(ID = dplyr::recode(ID, !!!id_corrections), timepoint = sheet) |>
    filter(ID %in% n_final_ids) |>
    dplyr::select(timepoint, all_of(audio_cols_needed))
}

audio_final <- bind_rows(
  read_audio_sheet("BASELINE"),
  read_audio_sheet("PRE"),
  read_audio_sheet("POST")
) |>
  mutate(
    timepoint = factor(timepoint, levels = c("BASELINE", "PRE", "POST"))
  ) |>
  arrange(timepoint, ID)

cat("\naudio_required_columns.csv:\n")
cat(
  "  Righe:",
  nrow(audio_final),
  "(attese",
  length(n_final_ids) * 3,
  "= 119 × 3)\n"
)
cat("  Colonne:", ncol(audio_final), "\n")

obs_check <- audio_final |> count(ID)
if (any(obs_check$n != 3)) {
  warning("Alcuni partecipanti non hanno esattamente 3 timepoint!")
  print(filter(obs_check, n != 3))
} else {
  cat("  OK: tutti i 119 hanno esattamente 3 osservazioni.\n")
}

write_csv(audio_final, here("data", "processed", "audio_required_columns.csv"))
cat("Salvato: data/processed/audio_required_columns.csv\n")

cat("\n=== Dataset creation complete ===\n")
