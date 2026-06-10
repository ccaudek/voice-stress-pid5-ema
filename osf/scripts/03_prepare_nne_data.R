# =============================================================================
# 03_prepare_nne_data.R
#
# Prepares the Stan data list for the NNE × PID-5 moderation model.
# Workflow is identical to 01_prepare_f0_data.R; only the outcome variable
# (NNE averaged across /a/, /i/, /u/) differs.
#
# Inputs:
#   data/processed/audio_required_columns.csv   (long format, 3 timepoints)
#   data/processed/pid5_required_columns.csv
#
# Outputs:
#   results/NNE/stan_bundle_nne.rds
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

voice_path <- here("data", "processed", "audio_required_columns.csv")
ema_path   <- here("data", "processed", "pid5_required_columns.csv")

stopifnot(file.exists(voice_path), file.exists(ema_path))

dir.create(here("results", "NNE"), recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. Voice data
# -----------------------------------------------------------------------------
# IDs and timepoints are already cleaned in audio_required_columns.csv
df_voice <- read_csv(voice_path, show_col_types = FALSE) |>
  transmute(
    ID,
    timepoint = factor(tolower(timepoint), levels = c("baseline", "pre", "post")),
    # NNE averaged across three vowels (more negative = more periodic/controlled)
    y_nne = rowMeans(
      across(c(`NNE /a/`, `NNE /i/`, `NNE /u/`)),
      na.rm = TRUE
    )
  ) |>
  mutate(
    c1_stress   = case_when(timepoint == "baseline" ~ -0.5,
                            timepoint == "pre"      ~  0.5,
                            timepoint == "post"     ~  0.0),
    c2_recovery = case_when(timepoint == "baseline" ~  0.0,
                            timepoint == "pre"      ~ -0.5,
                            timepoint == "post"     ~  0.5)
  ) |>
  filter(!is.na(ID), !is.na(y_nne))

cat("Voice (NNE): N =", nrow(df_voice), "| participants =",
    n_distinct(df_voice$ID), "\n")

# -----------------------------------------------------------------------------
# 2. EMA data
# -----------------------------------------------------------------------------
pid5_ema_vars <- c(
  "pid5_negative_affectivity", "pid5_detachment", "pid5_antagonism",
  "pid5_disinhibition", "pid5_psychoticism"
)

ema <- read_csv(ema_path, show_col_types = FALSE)
stopifnot(all(c("user_id", pid5_ema_vars) %in% names(ema)))

df_ema <- ema |>
  transmute(ID = user_id, across(all_of(pid5_ema_vars), as.numeric)) |>
  filter(!is.na(ID), ID %in% unique(df_voice$ID)) |>
  filter(if_any(all_of(pid5_ema_vars), ~ !is.na(.x)))

# Within-subject imputation
df_ema <- df_ema |>
  group_by(ID) |>
  mutate(across(
    all_of(pid5_ema_vars),
    ~ if_else(is.na(.x), mean(.x, na.rm = TRUE), .x)
  )) |>
  ungroup() |>
  filter(if_all(all_of(pid5_ema_vars), is.finite))

# -----------------------------------------------------------------------------
# 3. Align indices, standardise, build Stan list
# -----------------------------------------------------------------------------
subj_ids <- sort(intersect(unique(df_voice$ID), unique(df_ema$ID)))
N_subj   <- length(subj_ids)
id_map   <- tibble(ID = subj_ids, subj = seq_len(N_subj))

df_voice_stan <- df_voice |> inner_join(id_map, by = "ID") |> arrange(subj, timepoint)
df_ema_stan   <- df_ema   |> inner_join(id_map, by = "ID") |> arrange(subj)

X        <- as.matrix(df_ema_stan[, pid5_ema_vars])
X_scaled <- scale(X)
stopifnot(!anyNA(X_scaled))

stan_data <- list(
  N_subj     = N_subj,
  N_voice    = nrow(df_voice_stan),
  subj_voice = as.integer(df_voice_stan$subj),
  y          = as.numeric(df_voice_stan$y_nne),
  c1         = as.numeric(df_voice_stan$c1_stress),
  c2         = as.numeric(df_voice_stan$c2_recovery),
  N_ema      = nrow(df_ema_stan),
  subj_ema   = as.integer(df_ema_stan$subj),
  D          = length(pid5_ema_vars),
  X          = X_scaled
)

stopifnot(!anyNA(stan_data$y), !anyNA(stan_data$X))

saveRDS(
  list(stan_data = stan_data, df_voice = df_voice_stan,
       df_ema = df_ema_stan, pid5_vars = pid5_ema_vars,
       pid5_center = attr(X_scaled, "scaled:center"),
       pid5_scale  = attr(X_scaled, "scaled:scale"),
       subj_ids    = subj_ids),
  file = here("results", "NNE", "stan_bundle_nne.rds")
)

cat("Saved: results/NNE/stan_bundle_nne.rds\n")
cat("Final: N_subj =", N_subj, "| voice obs =", nrow(df_voice_stan),
    "| EMA obs =", nrow(df_ema_stan), "\n")
