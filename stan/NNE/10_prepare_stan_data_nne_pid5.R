# ==============================================================================
# 10_prepare_stan_data_nne_pid5.R
# Prepara dati per Stan (NNE):
# - Outcome: NNE aggregato su /a i u/ (più robusto)
# - Moderatori: 5 domini PID-5 EMA (latent trait con errore di misura)
# - Contrasti: c1_stress (PRE vs BASELINE), c2_recovery (POST vs PRE)
# - Imputazione: risolve NA in X prima di passare a Stan
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
  library(rio)
  library(lubridate)
  library(jsonlite)
  library(missRanger)
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

# Correzione ID (coerente col tuo script)
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

# Seleziona NNE sulle 3 vocali e crea outcome robusto
df_voice <- df_voice %>%
  transmute(
    ID,
    timepoint = factor(timepoint, levels = c("baseline", "pre", "post")),
    nne_a = `NNE /a/`,
    nne_i = `NNE /i/`,
    nne_u = `NNE /u/`
  ) %>%
  mutate(
    y_nne = rowMeans(across(c(nne_a, nne_i, nne_u)), na.rm = TRUE)
  ) %>%
  select(ID, timepoint, y_nne)

# Contrasti (come già usi)
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
  filter(!is.na(ID), !is.na(y_nne), !is.na(c1_stress), !is.na(c2_recovery))

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
    across(all_of(pid5_ema_vars), ~ suppressWarnings(as.numeric(.x)))
  ) %>%
  filter(!is.na(ID)) %>%
  filter(ID %in% unique(df_voice$ID)) %>%
  filter(if_any(all_of(pid5_ema_vars), ~ !is.na(.x)))

cat(
  "EMA (raw): N rows =",
  nrow(df_ema),
  "| N subj =",
  n_distinct(df_ema$ID),
  "\n"
)

# ----------------------------
# 3) CONSISTENT SUBJECT INDICES
# ----------------------------
subj_ids <- sort(unique(intersect(df_voice$ID, df_ema$ID)))
N_subj <- length(subj_ids)
id_map <- tibble(ID = subj_ids, subj = seq_len(N_subj))

df_voice_stan <- df_voice %>%
  inner_join(id_map, by = "ID") %>%
  arrange(subj, timepoint)

df_ema_stan <- df_ema %>%
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
# 4) IMPUTATION FOR X (avoid NA in Stan)
#    - Imputazione sulle righe EMA (molte) usando missRanger
#    - Inclusi: subj come fattore + 5 variabili PID5
# ----------------------------
df_imp_in <- df_ema_stan %>%
  select(subj, all_of(pid5_ema_vars)) %>%
  mutate(subj = factor(subj))

n_miss <- sum(is.na(df_imp_in[, pid5_ema_vars]))
cat("Missing in EMA PID5 before imputation:", n_miss, "\n")

if (n_miss > 0) {
  set.seed(123)
  df_imp_out <- missRanger(
    df_imp_in,
    pmm.k = 3,
    num.trees = 200,
    verbose = 0
  )
} else {
  df_imp_out <- df_imp_in
}

stopifnot(sum(is.na(df_imp_out[, pid5_ema_vars])) == 0)

# ----------------------------
# 5) STANDARDIZE PID5 EMA (global z on imputed)
# ----------------------------
X <- df_imp_out %>% select(all_of(pid5_ema_vars)) %>% as.matrix()

X_scaled <- scale(X)
pid5_center <- attr(X_scaled, "scaled:center")
pid5_scale <- attr(X_scaled, "scaled:scale")

# ----------------------------
# 6) STAN DATA LIST
# ----------------------------
stan_data <- list(
  N_subj = N_subj,

  # voice part
  N_voice = nrow(df_voice_stan),
  subj_voice = as.integer(df_voice_stan$subj),
  y = as.numeric(df_voice_stan$y_nne),
  c1 = as.numeric(df_voice_stan$c1_stress),
  c2 = as.numeric(df_voice_stan$c2_recovery),

  # ema measurement part
  N_ema = nrow(df_ema_stan),
  subj_ema = as.integer(df_ema_stan$subj),
  D = length(pid5_ema_vars),
  X = X_scaled
)

# sanity
stopifnot(length(stan_data$y) == stan_data$N_voice)
stopifnot(nrow(stan_data$X) == stan_data$N_ema)
stopifnot(ncol(stan_data$X) == stan_data$D)
stopifnot(!any(is.na(stan_data$X)))

# ----------------------------
# 7) EXPORT
# ----------------------------
write_json(
  stan_data,
  path = "stan/stan_data_nne_pid5.json",
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
    pid5_vars = pid5_ema_vars
  ),
  file = "results/stan_bundle_nne_pid5.rds"
)

cat("\nSaved:\n")
cat(" - stan/stan_data_nne_pid5.json\n")
cat(" - results/stan_bundle_nne_pid5.rds\n")
