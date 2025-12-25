# ==============================================================================
# 10_prepare_stan_data_f0mean_pid5.R
# Prepara dati per Stan:
# - Outcome: F0 mean aggregato su /a i u/ (più robusto)
# - Moderatori: 5 domini PID-5 EMA (latent trait con errore di misura)
# - Contrasti: c1_stress (PRE vs BASELINE), c2_recovery (POST vs PRE)
# - IMPUTAZIONE: within-subject sui 5 domini PID-5 EMA (per rimuovere NA in X)
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
  library(rio)
  library(lubridate)
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

dir.create("stan", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)

# ----------------------------
# 1) LOAD VOICE (3 timepoint)
# ----------------------------
baseline <- read_excel(voice_path, sheet = "BASELINE") %>%
  mutate(timepoint = "baseline")
pre <- read_excel(voice_path, sheet = "PRE") %>% mutate(timepoint = "pre")
post <- read_excel(voice_path, sheet = "POST") %>% mutate(timepoint = "post")

df_voice <- bind_rows(baseline, pre, post)
names(df_voice) <- stringr::str_trim(names(df_voice))

# Correzione ID (come nel tuo script)
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
  )

# Seleziona SOLO F0 mean sulle 3 vocali
df_voice <- df_voice %>%
  transmute(
    ID,
    timepoint = factor(timepoint, levels = c("baseline", "pre", "post")),
    f0_mean_a = `F0 mean Hz /a/`,
    f0_mean_i = `F0 mean Hz /i/`,
    f0_mean_u = `F0 mean Hz /u/`
  ) %>%
  mutate(
    # outcome robusto: media sulle vocali (riduce rumore vocale-specifico)
    y_f0 = rowMeans(across(c(f0_mean_a, f0_mean_i, f0_mean_u)), na.rm = TRUE)
  ) %>%
  select(ID, timepoint, y_f0)

# Contrasti
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
  filter(!is.na(ID), !is.na(y_f0), !is.na(c1_stress), !is.na(c2_recovery))

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
ema <- rio::import(ema_path) %>% as_tibble()

pid5_ema_vars <- c(
  "pid5_negative_affectivity",
  "pid5_detachment",
  "pid5_antagonism",
  "pid5_disinhibition",
  "pid5_psychoticism"
)

stopifnot(all(c("user_id", pid5_ema_vars) %in% names(ema)))

df_ema <- ema %>%
  transmute(
    ID = user_id,
    across(all_of(pid5_ema_vars), as.numeric)
  ) %>%
  filter(!is.na(ID)) %>%
  filter(ID %in% unique(df_voice$ID)) %>%
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

# Conteggio NA prima
na_before <- df_ema %>%
  summarise(across(all_of(pid5_ema_vars), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "n_na_before")

# Imputa: NA -> mean(ID, dominio)
df_ema_imp <- df_ema %>%
  group_by(ID) %>%
  mutate(across(
    all_of(pid5_ema_vars),
    ~ if_else(is.na(.x), mean(.x, na.rm = TRUE), .x)
  )) %>%
  ungroup()

# Elimina eventuali casi patologici (dominio sempre NA per quel soggetto -> mean = NaN)
df_ema_imp <- df_ema_imp %>%
  filter(if_all(all_of(pid5_ema_vars), ~ is.finite(.x)))

# Conteggio NA dopo (dovrebbe essere 0)
na_after <- df_ema_imp %>%
  summarise(across(all_of(pid5_ema_vars), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "n_na_after")

na_report <- na_before %>%
  left_join(na_after, by = "var") %>%
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
# 3) INDICI SOGGETTI CONSISTENTI TRA VOCE & EMA
# ----------------------------
subj_ids <- sort(unique(intersect(df_voice$ID, df_ema_imp$ID)))
N_subj <- length(subj_ids)

id_map <- tibble(ID = subj_ids, subj = seq_len(N_subj))

df_voice_stan <- df_voice %>%
  inner_join(id_map, by = "ID") %>%
  arrange(subj, timepoint)

df_ema_stan <- df_ema_imp %>%
  inner_join(id_map, by = "ID") %>%
  arrange(subj)

cat(
  "FINAL: N_subj =",
  N_subj,
  "| voice obs =",
  nrow(df_voice_stan),
  "| ema obs =",
  nrow(df_ema_stan),
  "\n"
)

# ----------------------------
# 4) STANDARDIZZA PID5 EMA (consigliato)
# ----------------------------
X <- df_ema_stan %>% select(all_of(pid5_ema_vars)) %>% as.matrix()

# Safety: Stan non accetta NA
stopifnot(!anyNA(X))

X_scaled <- scale(X)

pid5_center <- attr(X_scaled, "scaled:center")
pid5_scale <- attr(X_scaled, "scaled:scale")

# ----------------------------
# 5) CREA LISTA DATI PER STAN
# ----------------------------
stan_data <- list(
  N_subj = N_subj,

  # voice part
  N_voice = nrow(df_voice_stan),
  subj_voice = as.integer(df_voice_stan$subj),
  y = as.numeric(df_voice_stan$y_f0),
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
stopifnot(!anyNA(stan_data$X))

# ----------------------------
# 6) EXPORT
# ----------------------------
write_json(
  stan_data,
  path = "stan/stan_data_f0mean_pid5.json",
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
    na_report = na_report
  ),
  file = "results/stan_bundle_f0mean_pid5.rds"
)

cat("\nSaved:\n")
cat(" - stan/stan_data_f0mean_pid5.json\n")
cat(" - results/stan_bundle_f0mean_pid5.rds\n")
