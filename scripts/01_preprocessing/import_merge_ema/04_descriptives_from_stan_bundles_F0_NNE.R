# ==============================================================================
# 02_descriptives_from_stan_bundles_F0_NNE_FIXED.R
# Descriptives for MAIN TEXT + SUPPLEMENT using the SAME datasets used for Stan.
# FIX: avoid !!!desc_num() inside summarise (was causing y_f0 not found / cascade)
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(rio)
})

set.seed(42)

# ------------------------------------------------------------------------------
# 0) PATHS
# ------------------------------------------------------------------------------
BUNDLE_F0 <- here("results", "stan_bundle_f0mean_pid5.rds")
BUNDLE_NNE <- here("results", "NNE", "stan_bundle_nne_pid5.rds")
EMA_CLEAN <- here("data", "processed", "ema_plus_scales_cleaned.csv")

OUT_DIR <- here("results", "descriptives_stan")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

OUT_MAIN <- file.path(OUT_DIR, "Table_MainText_Descriptives_F0_NNE.csv")
OUT_S1 <- file.path(OUT_DIR, "S1_Design_and_Coverage.csv")
OUT_S2A <- file.path(OUT_DIR, "S2_Voice_ObsLevel_by_Period.csv")
OUT_S2B <- file.path(OUT_DIR, "S2_Voice_PersonMean_by_Period.csv")
OUT_S2C <- file.path(OUT_DIR, "S2_Voice_WithinPerson_SD.csv")
OUT_S3A <- file.path(OUT_DIR, "S3_EMA_BetweenPerson.csv")
OUT_S3B <- file.path(OUT_DIR, "S3_EMA_WithinPerson.csv")

stopifnot(file.exists(BUNDLE_F0))
stopifnot(file.exists(BUNDLE_NNE))

# ------------------------------------------------------------------------------
# 1) LOAD BUNDLES
# ------------------------------------------------------------------------------
b_f0 <- readRDS(BUNDLE_F0)
b_nne <- readRDS(BUNDLE_NNE)

df_voice_f0 <- b_f0$df_voice
df_voice_nne <- b_nne$df_voice

# sanity checks
stopifnot(all(
  c("ID", "timepoint", "y_f0", "c1_stress", "c2_recovery", "subj") %in%
    names(df_voice_f0)
))
stopifnot(all(
  c("ID", "timepoint", "y_nne", "c1_stress", "c2_recovery", "subj") %in%
    names(df_voice_nne)
))

# IDs included in each analysis dataset
ids_all <- sort(unique(intersect(
  unique(df_voice_f0$ID),
  unique(df_voice_nne$ID)
)))
if (length(ids_all) == 0) stop("No overlapping IDs between F0 and NNE bundles.")

df_voice_f0 <- df_voice_f0 %>% filter(ID %in% ids_all)
df_voice_nne <- df_voice_nne %>% filter(ID %in% ids_all)

# Harmonize timepoint labels/order
tp_levels <- c("baseline", "pre", "post")
tp_labels <- c("Baseline", "Pre-exam", "Post-exam")

df_voice_f0 <- df_voice_f0 %>%
  mutate(
    period = factor(
      as.character(timepoint),
      levels = tp_levels,
      labels = tp_labels
    )
  )

df_voice_nne <- df_voice_nne %>%
  mutate(
    period = factor(
      as.character(timepoint),
      levels = tp_levels,
      labels = tp_labels
    )
  )

# ------------------------------------------------------------------------------
# 2) OPTIONAL: LOAD EMA CLEANED (for n_ema + EMA PID-5 descriptives)
# ------------------------------------------------------------------------------
pid5_vars <- c(
  "pid5_negative_affectivity",
  "pid5_detachment",
  "pid5_antagonism",
  "pid5_disinhibition",
  "pid5_psychoticism"
)

ema_ok <- file.exists(EMA_CLEAN)
if (ema_ok) {
  ema <- rio::import(EMA_CLEAN) %>% as_tibble()
  stopifnot("user_id" %in% names(ema))
  stopifnot(all(pid5_vars %in% names(ema)))

  ema <- ema %>%
    mutate(user_id = as.character(user_id)) %>%
    filter(user_id %in% ids_all)

  nema_tbl <- ema %>% count(user_id, name = "n_ema")
} else {
  message(
    "NOTE: ema_plus_scales_cleaned.csv not found -> S3 skipped, n_ema not available."
  )
  nema_tbl <- tibble(user_id = ids_all, n_ema = NA_integer_)
}

# ------------------------------------------------------------------------------
# 3) Helper: numeric descriptives (computed INSIDE summarise)
# ------------------------------------------------------------------------------
summ_desc <- function(x) {
  x <- x[is.finite(x)]
  tibble(
    n = length(x),
    mean = mean(x),
    sd = sd(x),
    median = median(x),
    min = min(x),
    max = max(x)
  )
}

fmt_m_sd <- function(m, sd, digits = 2)
  paste0(round(m, digits), " (", round(sd, digits), ")")

# ------------------------------------------------------------------------------
# 4) SUPPLEMENT S1: DESIGN / COVERAGE
# ------------------------------------------------------------------------------
N_subj <- length(ids_all)

ema_summary <- nema_tbl %>%
  summarise(
    N_subj = n_distinct(user_id),
    ema_mean = mean(n_ema, na.rm = TRUE),
    ema_sd = sd(n_ema, na.rm = TRUE),
    ema_median = median(n_ema, na.rm = TRUE),
    ema_min = min(n_ema, na.rm = TRUE),
    ema_max = max(n_ema, na.rm = TRUE)
  )

voice_counts <- df_voice_f0 %>%
  count(period, name = "n_voice_obs") %>%
  arrange(period)

S1 <- ema_summary %>%
  mutate(N_subj_voice = N_subj) %>%
  bind_cols(
    voice_counts %>%
      pivot_wider(
        names_from = period,
        values_from = n_voice_obs,
        values_fill = 0
      ) %>%
      rename_with(~ paste0("voice_obs_", .x))
  )

write_csv(S1, OUT_S1)

# ------------------------------------------------------------------------------
# 5) SUPPLEMENT S2: VOICE DESCRIPTIVES (ROBUST)
# ------------------------------------------------------------------------------
# A) Observation-level by period
S2_f0_obs <- df_voice_f0 %>%
  group_by(period) %>%
  summarise(
    outcome = "F0 (Hz)",
    n = sum(is.finite(y_f0)),
    mean = mean(y_f0, na.rm = TRUE),
    sd = sd(y_f0, na.rm = TRUE),
    median = median(y_f0, na.rm = TRUE),
    min = min(y_f0, na.rm = TRUE),
    max = max(y_f0, na.rm = TRUE),
    .groups = "drop"
  )

S2_nne_obs <- df_voice_nne %>%
  group_by(period) %>%
  summarise(
    outcome = "NNE",
    n = sum(is.finite(y_nne)),
    mean = mean(y_nne, na.rm = TRUE),
    sd = sd(y_nne, na.rm = TRUE),
    median = median(y_nne, na.rm = TRUE),
    min = min(y_nne, na.rm = TRUE),
    max = max(y_nne, na.rm = TRUE),
    .groups = "drop"
  )

S2_obs <- bind_rows(S2_f0_obs, S2_nne_obs)

# B) Person means by period
pm_f0 <- df_voice_f0 %>%
  group_by(ID, period) %>%
  summarise(f0_pm = mean(y_f0, na.rm = TRUE), .groups = "drop")

pm_nne <- df_voice_nne %>%
  group_by(ID, period) %>%
  summarise(nne_pm = mean(y_nne, na.rm = TRUE), .groups = "drop")

S2_f0_pm <- pm_f0 %>%
  group_by(period) %>%
  summarise(
    outcome = "F0 person-mean (Hz)",
    n = sum(is.finite(f0_pm)),
    mean = mean(f0_pm, na.rm = TRUE),
    sd = sd(f0_pm, na.rm = TRUE),
    median = median(f0_pm, na.rm = TRUE),
    min = min(f0_pm, na.rm = TRUE),
    max = max(f0_pm, na.rm = TRUE),
    .groups = "drop"
  )

S2_nne_pm <- pm_nne %>%
  group_by(period) %>%
  summarise(
    outcome = "NNE person-mean",
    n = sum(is.finite(nne_pm)),
    mean = mean(nne_pm, na.rm = TRUE),
    sd = sd(nne_pm, na.rm = TRUE),
    median = median(nne_pm, na.rm = TRUE),
    min = min(nne_pm, na.rm = TRUE),
    max = max(nne_pm, na.rm = TRUE),
    .groups = "drop"
  )

S2_pm <- bind_rows(S2_f0_pm, S2_nne_pm)

# C) Within-person SD across periods (3 timepoints)
sd_f0 <- df_voice_f0 %>%
  group_by(ID) %>%
  summarise(f0_sd_within = sd(y_f0, na.rm = TRUE), .groups = "drop")

sd_nne <- df_voice_nne %>%
  group_by(ID) %>%
  summarise(nne_sd_within = sd(y_nne, na.rm = TRUE), .groups = "drop")

S2_within <- bind_rows(
  sd_f0 %>%
    summarise(
      outcome = "F0 within-person SD (Hz)",
      n = sum(is.finite(f0_sd_within)),
      mean = mean(f0_sd_within, na.rm = TRUE),
      sd = sd(f0_sd_within, na.rm = TRUE),
      median = median(f0_sd_within, na.rm = TRUE),
      min = min(f0_sd_within, na.rm = TRUE),
      max = max(f0_sd_within, na.rm = TRUE),
      .groups = "drop"
    ),
  sd_nne %>%
    summarise(
      outcome = "NNE within-person SD",
      n = sum(is.finite(nne_sd_within)),
      mean = mean(nne_sd_within, na.rm = TRUE),
      sd = sd(nne_sd_within, na.rm = TRUE),
      median = median(nne_sd_within, na.rm = TRUE),
      min = min(nne_sd_within, na.rm = TRUE),
      max = max(nne_sd_within, na.rm = TRUE),
      .groups = "drop"
    )
)

write_csv(S2_obs, OUT_S2A)
write_csv(S2_pm, OUT_S2B)
write_csv(S2_within, OUT_S2C)

# ------------------------------------------------------------------------------
# 6) SUPPLEMENT S3: EMA PID-5 DESCRIPTIVES (if available)
# ------------------------------------------------------------------------------
if (ema_ok) {
  ema_pid5 <- ema %>%
    transmute(
      ID = as.character(user_id),
      across(all_of(pid5_vars), ~ suppressWarnings(as.numeric(.x)))
    ) %>%
    filter(ID %in% ids_all) %>%
    filter(if_any(all_of(pid5_vars), ~ is.finite(.x)))

  pid5_pm <- ema_pid5 %>%
    group_by(ID) %>%
    summarise(
      across(all_of(pid5_vars), ~ mean(.x, na.rm = TRUE), .names = "{.col}_pm"),
      .groups = "drop"
    )

  pid5_wsd <- ema_pid5 %>%
    group_by(ID) %>%
    summarise(
      across(all_of(pid5_vars), ~ sd(.x, na.rm = TRUE), .names = "{.col}_wsd"),
      .groups = "drop"
    )

  desc_num <- function(x) {
    x <- x[is.finite(x)]
    tibble(
      n = length(x),
      mean = mean(x),
      sd = sd(x),
      median = median(x),
      min = min(x),
      max = max(x)
    )
  }

  S3_between <- map_dfr(pid5_vars, function(v) {
    d <- desc_num(pid5_pm[[paste0(v, "_pm")]])
    d$variable <- v
    d$level <- "Between-person (person means)"
    d
  }) %>%
    select(level, variable, everything())

  S3_within <- map_dfr(pid5_vars, function(v) {
    d <- desc_num(pid5_wsd[[paste0(v, "_wsd")]])
    d$variable <- v
    d$level <- "Within-person (subject SDs)"
    d
  }) %>%
    select(level, variable, everything())

  write_csv(S3_between, OUT_S3A)
  write_csv(S3_within, OUT_S3B)
}

# ------------------------------------------------------------------------------
# 7) MAIN TEXT TABLE (BRIEF)
# ------------------------------------------------------------------------------
ema_str <- if (ema_ok) {
  nema_tbl %>%
    summarise(
      m = mean(n_ema),
      sd = sd(n_ema),
      mn = min(n_ema),
      mx = max(n_ema)
    ) %>%
    transmute(
      s = paste0(round(m, 1), " (SD ", round(sd, 1), "), range ", mn, "-", mx)
    ) %>%
    pull(s)
} else NA_character_

voice_obs_counts <- df_voice_f0 %>%
  count(period, name = "n_obs") %>%
  pivot_wider(names_from = period, values_from = n_obs, values_fill = 0) %>%
  rename_with(~ paste0("Voice_obs_", .x))

f0_pm_summary <- pm_f0 %>%
  group_by(period) %>%
  summarise(m = mean(f0_pm), sd = sd(f0_pm), .groups = "drop") %>%
  mutate(val = fmt_m_sd(m, sd, digits = 2)) %>%
  select(period, val) %>%
  pivot_wider(names_from = period, values_from = val, names_prefix = "F0_")

nne_pm_summary <- pm_nne %>%
  group_by(period) %>%
  summarise(m = mean(nne_pm), sd = sd(nne_pm), .groups = "drop") %>%
  mutate(val = fmt_m_sd(m, sd, digits = 2)) %>%
  select(period, val) %>%
  pivot_wider(names_from = period, values_from = val, names_prefix = "NNE_")

Table_Main <- tibble(
  N_participants = N_subj,
  EMA_per_subject = ema_str
) %>%
  bind_cols(voice_obs_counts) %>%
  bind_cols(f0_pm_summary) %>%
  bind_cols(nne_pm_summary)

write_csv(Table_Main, OUT_MAIN)

cat("\n=== DONE (fixed) ===\n")
cat("Saved outputs to: ", OUT_DIR, "\n", sep = "")
cat(" - ", basename(OUT_MAIN), "\n", sep = "")
cat(" - ", basename(OUT_S1), "\n", sep = "")
cat(" - ", basename(OUT_S2A), "\n", sep = "")
cat(" - ", basename(OUT_S2B), "\n", sep = "")
cat(" - ", basename(OUT_S2C), "\n", sep = "")
if (ema_ok) {
  cat(" - ", basename(OUT_S3A), "\n", sep = "")
  cat(" - ", basename(OUT_S3B), "\n", sep = "")
}
