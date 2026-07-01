# ==============================================================================
# 02_ema_compliance_for_f0mean_pid5_sample.R
#
# Compliance EMA calcolata SOLO sui 119 soggetti inclusi nello script:
# 10_prepare_stan_data_f0mean_pid5.R
#
# Correzione:
# - periodo EMA: 20 marzo 2025 - 31 maggio 2025
# - notifiche possibili: 21 programmate + 6 aggiuntive = 27
# - nella condizione baseline: max 2 notifiche per soggetto per settimana
# - nelle settimane con esame: max 2 baseline, ma pre_exam/post_exam si mantengono
#
# Reviewer request:
# "State the number of possible assessments over the 2.5 months and report
# compliance (mean, median, min, max)."
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
  library(rio)
  library(lubridate)
  library(readr)
})

# ------------------------------------------------------------------------------
# 0) PARAMETRI
# ------------------------------------------------------------------------------

STUDY_START <- as.Date("2025-03-20")
STUDY_END <- as.Date("2025-05-31")

SCHEDULED_NOTIFICATIONS <- 21L
ADDITIONAL_NOTIFICATIONS <- 6L
POSSIBLE_ASSESSMENTS <- SCHEDULED_NOTIFICATIONS + ADDITIONAL_NOTIFICATIONS

EXPECTED_N_ANALYTIC <- 119L

# Settimana calendario.
# week_start = 1 significa lunedì.
# Se vuoi settimane domenica-sabato, cambia in week_start = 7.
WEEK_START <- 1L

# Input coerenti con lo script F0 mean + PID-5
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

# Output
out_dir <- here("results", "F0", "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

OUT_PER_PARTICIPANT <- here(
  "results",
  "F0",
  "tables",
  "ema_compliance_f0mean_pid5_sample_by_participant.csv"
)

OUT_SUMMARY <- here(
  "results",
  "F0",
  "tables",
  "ema_compliance_f0mean_pid5_sample_summary.csv"
)

OUT_REMOVED_BASELINE <- here(
  "results",
  "F0",
  "tables",
  "ema_compliance_removed_extra_baseline_rows.csv"
)

OUT_BASELINE_WEEK_QC <- here(
  "results",
  "F0",
  "tables",
  "ema_compliance_baseline_week_qc.csv"
)

OUT_REVIEWER_TEXT <- here(
  "results",
  "F0",
  "tables",
  "ema_compliance_f0mean_pid5_sample_reviewer_text.txt"
)

# ------------------------------------------------------------------------------
# 1) FUNZIONI DI SUPPORTO
# ------------------------------------------------------------------------------

parse_any_datetime <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(x)
  }

  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = "UTC"))
  }

  if (is.numeric(x)) {
    # Gestione di eventuali date Excel
    return(as.POSIXct(as.Date(x, origin = "1899-12-30"), tz = "UTC"))
  }

  suppressWarnings(
    lubridate::parse_date_time(
      as.character(x),
      orders = c(
        "ymd HMS",
        "ymd HM",
        "ymd",
        "dmy HMS",
        "dmy HM",
        "dmy",
        "mdy HMS",
        "mdy HM",
        "mdy",
        "Ymd HMS",
        "Ymd HM",
        "Ymd"
      ),
      tz = "UTC"
    )
  )
}

normalize_exam_period <- function(x) {
  x |>
    as.character() |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[ -]", "_")
}

# ------------------------------------------------------------------------------
# 2) CARICA DATI VOCALI E RICOSTRUISCI GLI ID ANALIZZATI
# ------------------------------------------------------------------------------

baseline <- read_excel(voice_path, sheet = "BASELINE") |>
  mutate(timepoint = "baseline")

pre <- read_excel(voice_path, sheet = "PRE") |>
  mutate(timepoint = "pre")

post <- read_excel(voice_path, sheet = "POST") |>
  mutate(timepoint = "post")

df_voice <- bind_rows(baseline, pre, post)
names(df_voice) <- stringr::str_trim(names(df_voice))

# Correzione ID identica allo script F0 mean + PID-5
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

df_voice <- df_voice |>
  transmute(
    ID,
    timepoint = factor(timepoint, levels = c("baseline", "pre", "post")),
    f0_mean_a = `F0 mean Hz /a/`,
    f0_mean_i = `F0 mean Hz /i/`,
    f0_mean_u = `F0 mean Hz /u/`
  ) |>
  mutate(
    y_f0 = rowMeans(
      across(c(f0_mean_a, f0_mean_i, f0_mean_u)),
      na.rm = TRUE
    ),
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
  filter(
    !is.na(ID),
    !is.na(y_f0),
    !is.na(c1_stress),
    !is.na(c2_recovery)
  ) |>
  select(ID, timepoint, y_f0, c1_stress, c2_recovery)

cat(
  "VOICE: N obs =",
  nrow(df_voice),
  "| N subj =",
  n_distinct(df_voice$ID),
  "\n"
)

# ------------------------------------------------------------------------------
# 3) CARICA EMA CLEANED E PREPARA PID-5 COME NELLO SCRIPT F0 MEAN + PID-5
# ------------------------------------------------------------------------------

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
  # tieni righe con almeno 1 dominio PID-5 osservato
  filter(if_any(all_of(pid5_ema_vars), ~ !is.na(.x)))

cat(
  "EMA before imputation: N rows =",
  nrow(df_ema),
  "| N subj =",
  n_distinct(df_ema$ID),
  "\n"
)

# Imputazione within-subject identica allo script F0 mean + PID-5
df_ema_imp <- df_ema |>
  group_by(ID) |>
  mutate(across(
    all_of(pid5_ema_vars),
    ~ if_else(is.na(.x), mean(.x, na.rm = TRUE), .x)
  )) |>
  ungroup()

# Elimina eventuali casi in cui un dominio resta non finito
df_ema_imp <- df_ema_imp |>
  filter(if_all(all_of(pid5_ema_vars), ~ is.finite(.x)))

cat(
  "EMA after imputation: N rows =",
  nrow(df_ema_imp),
  "| N subj =",
  n_distinct(df_ema_imp$ID),
  "\n"
)

# ID finali analizzati nello script Stan
analytic_ids <- sort(unique(intersect(df_voice$ID, df_ema_imp$ID)))
N_analytic <- length(analytic_ids)

cat(
  "FINAL ANALYTIC SAMPLE: N subj =",
  N_analytic,
  "\n"
)

if (N_analytic != EXPECTED_N_ANALYTIC) {
  stop(
    "Il numero di soggetti analitici non è 119. N trovato = ",
    N_analytic,
    ". Controlla che i file input siano gli stessi usati nello script F0 mean + PID-5."
  )
}

# ------------------------------------------------------------------------------
# 4) PREPARA DATE E CONDIZIONE EMA
# ------------------------------------------------------------------------------

date_candidates <- c("day", "date", "Date", "baseline_date")
date_col <- date_candidates[date_candidates %in% names(ema)][1]

if (is.na(date_col)) {
  stop(
    "Non trovo una colonna data nel dataset EMA. ",
    "Mi aspettavo una tra: day, date, Date, baseline_date."
  )
}

datetime_candidates <- c("ema_time", "timestamp", "time", "datetime")
datetime_col <- datetime_candidates[datetime_candidates %in% names(ema)][1]

ema <- ema |>
  mutate(
    .row_id = row_number(),
    .ema_date = as.Date(parse_any_datetime(.data[[date_col]]))
  )

if (all(is.na(ema$.ema_date))) {
  stop(
    "La colonna data è stata trovata, ma non riesco a convertirla in Date. ",
    "Colonna usata: ",
    date_col
  )
}

if (!is.na(datetime_col)) {
  ema <- ema |>
    mutate(
      .ema_order_datetime = parse_any_datetime(.data[[datetime_col]])
    )

  # Se il timestamp non è parsabile o è solo parziale, uso la data + ordine riga.
  ema <- ema |>
    mutate(
      .ema_order_datetime = if_else(
        is.na(.ema_order_datetime),
        as.POSIXct(.ema_date, tz = "UTC") + lubridate::seconds(.row_id),
        .ema_order_datetime
      )
    )
} else {
  ema <- ema |>
    mutate(
      .ema_order_datetime = as.POSIXct(.ema_date, tz = "UTC") +
        lubridate::seconds(.row_id)
    )
}

# Se exam_period non fosse presente, lo ricostruisco con le stesse finestre
# usate nello script di cleaning.
if (!"exam_period" %in% names(ema)) {
  if (!"course" %in% names(ema)) {
    stop(
      "Nel dataset EMA mancano sia exam_period sia course. ",
      "Non posso distinguere baseline, pre_exam e post_exam."
    )
  }

  psico_pre <- as.Date(c("2025-04-14", "2025-05-21"))
  psico_post <- as.Date(c("2025-04-15", "2025-05-22"))

  test_pre <- as.Date(c("2025-04-14", "2025-05-25"))
  test_post <- as.Date(c("2025-04-15", "2025-05-26"))

  interv_pre <- as.Date("2025-05-12")
  interv_post <- as.Date("2025-05-13")

  ema <- ema |>
    mutate(
      course_norm = case_when(
        stringr::str_detect(course, regex("^psico", ignore_case = TRUE)) ~
          "Psicometria",
        stringr::str_detect(course, regex("^test", ignore_case = TRUE)) ~
          "Testing",
        stringr::str_detect(course, regex("^inter", ignore_case = TRUE)) ~
          "Interventi",
        stringr::str_detect(course, regex("^clin", ignore_case = TRUE)) ~
          "Clinica",
        TRUE ~ as.character(course)
      ),
      exam_period = case_when(
        course_norm == "Clinica" ~ "baseline",
        course_norm == "Psicometria" & .ema_date %in% psico_pre ~ "pre_exam",
        course_norm == "Psicometria" & .ema_date %in% psico_post ~ "post_exam",
        course_norm == "Testing" & .ema_date %in% test_pre ~ "pre_exam",
        course_norm == "Testing" & .ema_date %in% test_post ~ "post_exam",
        course_norm == "Interventi" & .ema_date %in% interv_pre ~ "pre_exam",
        course_norm == "Interventi" & .ema_date %in% interv_post ~ "post_exam",
        TRUE ~ "baseline"
      )
    )
}

ema <- ema |>
  mutate(
    .exam_period = normalize_exam_period(exam_period),
    .is_baseline = .exam_period == "baseline",
    .is_prepost = .exam_period %in% c("pre_exam", "post_exam"),
    .week_start = lubridate::floor_date(
      .ema_date,
      unit = "week",
      week_start = WEEK_START
    )
  )

# ------------------------------------------------------------------------------
# 5) APPLICA FILTRO: MAX 2 BASELINE PER SOGGETTO PER SETTIMANA
# ------------------------------------------------------------------------------

ema_analytic_period <- ema |>
  filter(user_id %in% analytic_ids) |>
  filter(.ema_date >= STUDY_START, .ema_date <= STUDY_END)

cat(
  "EMA analytic rows before baseline-week filter: N rows =",
  nrow(ema_analytic_period),
  "| N subj =",
  n_distinct(ema_analytic_period$user_id),
  "\n"
)

# QC: quante baseline per soggetto-settimana prima del filtro?
baseline_week_qc <- ema_analytic_period |>
  filter(.is_baseline) |>
  count(user_id, .week_start, name = "n_baseline_before") |>
  mutate(
    n_baseline_kept = pmin(n_baseline_before, 2L),
    n_baseline_removed = pmax(n_baseline_before - 2L, 0L),
    exceeds_two_baseline = n_baseline_before > 2L
  ) |>
  arrange(desc(n_baseline_removed), user_id, .week_start)

# Ranking solo delle baseline entro soggetto-settimana
baseline_rows <- ema_analytic_period |>
  filter(.is_baseline) |>
  group_by(user_id, .week_start) |>
  arrange(.ema_order_datetime, .row_id, .by_group = TRUE) |>
  mutate(
    .baseline_rank_in_week = row_number(),
    .keep_for_compliance = .baseline_rank_in_week <= 2L
  ) |>
  ungroup()

# Le righe pre_exam/post_exam, e qualunque riga non-baseline, vengono mantenute
nonbaseline_rows <- ema_analytic_period |>
  filter(!.is_baseline) |>
  mutate(
    .baseline_rank_in_week = NA_integer_,
    .keep_for_compliance = TRUE
  )

ema_ranked <- bind_rows(baseline_rows, nonbaseline_rows) |>
  arrange(.row_id)

removed_baseline_extra <- ema_ranked |>
  filter(.is_baseline, !.keep_for_compliance) |>
  arrange(user_id, .week_start, .ema_order_datetime, .row_id)

ema_compliance_rows <- ema_ranked |>
  filter(.keep_for_compliance)

cat(
  "Extra baseline rows removed: N rows =",
  nrow(removed_baseline_extra),
  "| N subj affected =",
  n_distinct(removed_baseline_extra$user_id),
  "\n"
)

cat(
  "EMA analytic rows after baseline-week filter: N rows =",
  nrow(ema_compliance_rows),
  "| N subj =",
  n_distinct(ema_compliance_rows$user_id),
  "\n"
)

# Safety check: dopo il filtro, nessun soggetto deve avere >2 baseline/settimana
baseline_week_after <- ema_compliance_rows |>
  filter(.is_baseline) |>
  count(user_id, .week_start, name = "n_baseline_after")

if (any(baseline_week_after$n_baseline_after > 2L)) {
  stop(
    "Errore: ci sono ancora soggetti con più di 2 baseline nella stessa settimana."
  )
}

# ------------------------------------------------------------------------------
# 6) CALCOLA COMPLIANCE SOLO SUI 119 SOGGETTI ANALIZZATI
# ------------------------------------------------------------------------------

# Non usare n_ema, perché n_ema non tiene conto del filtro sulle baseline extra.
completed_counts <- ema_compliance_rows |>
  count(user_id, name = "completed_assessments")

compliance_by_participant <- tibble(user_id = analytic_ids) |>
  left_join(completed_counts, by = "user_id") |>
  mutate(
    completed_assessments = replace_na(completed_assessments, 0L),
    possible_assessments = POSSIBLE_ASSESSMENTS,
    compliance_prop = completed_assessments / possible_assessments,
    compliance_percent = 100 * compliance_prop
  ) |>
  select(
    user_id,
    possible_assessments,
    completed_assessments,
    compliance_prop,
    compliance_percent
  ) |>
  arrange(user_id)

stopifnot(n_distinct(compliance_by_participant$user_id) == EXPECTED_N_ANALYTIC)

if (
  max(compliance_by_participant$completed_assessments, na.rm = TRUE) >
    POSSIBLE_ASSESSMENTS
) {
  warning(
    "Almeno un soggetto ha più assessment completati del numero massimo possibile. ",
    "Controlla eventuali duplicati o ulteriori notifiche extra non codificate."
  )
}

# ------------------------------------------------------------------------------
# 7) STATISTICHE RICHIESTE DAL REVIEWER
# ------------------------------------------------------------------------------

compliance_summary <- compliance_by_participant |>
  summarise(
    n_participants = n(),

    study_start = STUDY_START,
    study_end = STUDY_END,
    study_days = as.integer(STUDY_END - STUDY_START + 1),

    scheduled_notifications = SCHEDULED_NOTIFICATIONS,
    additional_notifications = ADDITIONAL_NOTIFICATIONS,
    possible_assessments = POSSIBLE_ASSESSMENTS,

    removed_extra_baseline_rows = nrow(removed_baseline_extra),
    n_subjects_with_extra_baseline_removed = n_distinct(
      removed_baseline_extra$user_id
    ),

    completed_mean = mean(completed_assessments, na.rm = TRUE),
    completed_median = median(completed_assessments, na.rm = TRUE),
    completed_min = min(completed_assessments, na.rm = TRUE),
    completed_max = max(completed_assessments, na.rm = TRUE),

    compliance_mean_percent = mean(compliance_percent, na.rm = TRUE),
    compliance_median_percent = median(compliance_percent, na.rm = TRUE),
    compliance_min_percent = min(compliance_percent, na.rm = TRUE),
    compliance_max_percent = max(compliance_percent, na.rm = TRUE)
  )

print(compliance_summary)

# ------------------------------------------------------------------------------
# 8) TESTO PRONTO PER REVIEWER / MANOSCRITTO
# ------------------------------------------------------------------------------

s <- compliance_summary

reviewer_text <- sprintf(
  paste0(
    "Across the EMA period from %s to %s, participants could receive up to %d ",
    "notifications, comprising %d scheduled notifications plus %d additional ",
    "exam-related notifications. Compliance was calculated in the analytic sample ",
    "used for the F0 mean PID-5 model (N = %d). For the baseline condition, we ",
    "allowed a maximum of two baseline assessments per participant per calendar ",
    "week; pre-exam and post-exam assessments were retained in addition to these ",
    "baseline assessments. Participants completed a mean of %.2f assessments ",
    "(median = %.0f, range = %d-%d), corresponding to a mean compliance of %.1f%% ",
    "(median = %.1f%%, range = %.1f%%-%.1f%%)."
  ),
  format(s$study_start, "%B %d, %Y"),
  format(s$study_end, "%B %d, %Y"),
  s$possible_assessments,
  s$scheduled_notifications,
  s$additional_notifications,
  s$n_participants,
  s$completed_mean,
  s$completed_median,
  s$completed_min,
  s$completed_max,
  s$compliance_mean_percent,
  s$compliance_median_percent,
  s$compliance_min_percent,
  s$compliance_max_percent
)

cat("\nText for reviewer/manuscript:\n")
cat(reviewer_text, "\n")

# ------------------------------------------------------------------------------
# 9) EXPORT
# ------------------------------------------------------------------------------

write_csv(compliance_by_participant, OUT_PER_PARTICIPANT)
write_csv(compliance_summary, OUT_SUMMARY)
write_csv(removed_baseline_extra, OUT_REMOVED_BASELINE)
write_csv(baseline_week_qc, OUT_BASELINE_WEEK_QC)
writeLines(reviewer_text, OUT_REVIEWER_TEXT)

message(
  "\nEMA compliance completed for F0 mean PID-5 analytic sample.\n",
  "- N analytic sample: ",
  N_analytic,
  "\n",
  "- Possible assessments: ",
  POSSIBLE_ASSESSMENTS,
  "\n",
  "- Extra baseline rows removed: ",
  nrow(removed_baseline_extra),
  "\n",
  "- Per-participant table: ",
  OUT_PER_PARTICIPANT,
  "\n",
  "- Summary table: ",
  OUT_SUMMARY,
  "\n",
  "- Removed baseline rows: ",
  OUT_REMOVED_BASELINE,
  "\n",
  "- Baseline week QC: ",
  OUT_BASELINE_WEEK_QC,
  "\n",
  "- Reviewer text: ",
  OUT_REVIEWER_TEXT,
  "\n"
)

# ------------------------------------------------------------------------------
# 6c) NUMERO DI ASSESSMENT PER SOGGETTO E CONDIZIONE
# ------------------------------------------------------------------------------

compliance_by_condition <- ema_compliance_rows |>
  count(user_id, .exam_period, name = "n_assessments") |>
  pivot_wider(
    names_from = .exam_period,
    values_from = n_assessments,
    values_fill = 0
  ) |>
  right_join(tibble(user_id = analytic_ids), by = "user_id") |>
  mutate(
    across(
      c(baseline, pre_exam, post_exam),
      ~ replace_na(.x, 0L)
    ),
    completed_assessments = baseline + pre_exam + post_exam,
    possible_assessments = POSSIBLE_ASSESSMENTS,
    compliance_percent = round(
      100 * completed_assessments / possible_assessments,
      1
    )
  ) |>
  select(
    user_id,
    baseline,
    pre_exam,
    post_exam,
    completed_assessments,
    possible_assessments,
    compliance_percent
  ) |>
  arrange(user_id)

print(compliance_by_condition, n = Inf)

write_csv(
  compliance_by_condition,
  here(
    "results",
    "F0",
    "tables",
    "ema_compliance_per_subject_by_condition.csv"
  )
)

# eof
