# =============================================================================
# 01_prepare_f0_data.R
#
# Prepares the Stan data list for the F0 × PID-5 moderation model.
#
# Inputs:
#   data/processed/audio_required_columns.csv   (long format, 3 timepoints)
#   data/processed/pid5_required_columns.csv
#
# Outputs:
#   results/F0/stan_bundle_f0.rds   (Stan data list + auxiliary objects)
#
# Run from the project root.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

# -----------------------------------------------------------------------------
# 0. Paths
# -----------------------------------------------------------------------------
voice_path <- here("data", "processed", "audio_required_columns.csv")
ema_path <- here("data", "processed", "pid5_required_columns.csv")

stopifnot(file.exists(voice_path), file.exists(ema_path))

dir.create(here("results", "F0"), recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. Load voice data (three assessment timepoints)
# -----------------------------------------------------------------------------
# IDs and timepoints are already cleaned in audio_required_columns.csv
df_voice <- read_csv(voice_path, show_col_types = FALSE)

# Compute outcome: F0 mean averaged across three cardinal vowels
df_voice <- df_voice |>
  transmute(
    ID,
    timepoint = factor(
      tolower(timepoint),
      levels = c("baseline", "pre", "post")
    ),
    y_f0 = rowMeans(
      across(c(`F0 mean Hz /a/`, `F0 mean Hz /i/`, `F0 mean Hz /u/`)),
      na.rm = TRUE
    )
  ) |>
  # Orthogonal contrasts (see Supplementary Materials, Statistical Models)
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
  filter(!is.na(ID), !is.na(y_f0))

cat(
  "Voice data: N =",
  nrow(df_voice),
  "| participants =",
  n_distinct(df_voice$ID),
  "\n"
)

# -----------------------------------------------------------------------------
# 2. Load EMA data
# -----------------------------------------------------------------------------
pid5_ema_vars <- c(
  "pid5_negative_affectivity",
  "pid5_detachment",
  "pid5_antagonism",
  "pid5_disinhibition",
  "pid5_psychoticism"
)

ema <- read_csv(ema_path, show_col_types = FALSE)

df_ema <- ema |>
  transmute(ID = user_id, across(all_of(pid5_ema_vars), as.numeric)) |>
  filter(!is.na(ID), ID %in% unique(df_voice$ID)) |>
  filter(if_any(all_of(pid5_ema_vars), ~ !is.na(.x)))

cat("EMA rows:", nrow(df_ema), "| participants:", n_distinct(df_ema$ID), "\n")

# -----------------------------------------------------------------------------
# 3. Within-subject imputation of sporadic missing EMA values
# -----------------------------------------------------------------------------
df_ema <- df_ema |>
  group_by(ID) |>
  mutate(across(
    all_of(pid5_ema_vars),
    ~ if_else(is.na(.x), mean(.x, na.rm = TRUE), .x)
  )) |>
  ungroup() |>
  filter(if_all(all_of(pid5_ema_vars), is.finite))

# -----------------------------------------------------------------------------
# 4. Align participant indices across voice and EMA
# -----------------------------------------------------------------------------
subj_ids <- sort(intersect(unique(df_voice$ID), unique(df_ema$ID)))
N_subj <- length(subj_ids)
id_map <- tibble(ID = subj_ids, subj = seq_len(N_subj))

df_voice_stan <- df_voice |>
  inner_join(id_map, by = "ID") |>
  arrange(subj, timepoint)
df_ema_stan <- df_ema |> inner_join(id_map, by = "ID") |> arrange(subj)

cat(
  "Final: N_subj =",
  N_subj,
  "| voice obs =",
  nrow(df_voice_stan),
  "| EMA obs =",
  nrow(df_ema_stan),
  "\n"
)

# -----------------------------------------------------------------------------
# 5. Standardise EMA domain scores (mean 0, SD 1)
# -----------------------------------------------------------------------------
X <- as.matrix(df_ema_stan[, pid5_ema_vars])
X_scaled <- scale(X)
stopifnot(!anyNA(X_scaled))

# -----------------------------------------------------------------------------
# 6. Assemble Stan data list
# -----------------------------------------------------------------------------
stan_data <- list(
  N_subj = N_subj,

  N_voice = nrow(df_voice_stan),
  subj_voice = as.integer(df_voice_stan$subj),
  y = as.numeric(df_voice_stan$y_f0),
  c1 = as.numeric(df_voice_stan$c1_stress),
  c2 = as.numeric(df_voice_stan$c2_recovery),

  N_ema = nrow(df_ema_stan),
  subj_ema = as.integer(df_ema_stan$subj),
  D = length(pid5_ema_vars),
  X = X_scaled
)

# Sanity checks
stopifnot(
  length(stan_data$y) == stan_data$N_voice,
  ncol(stan_data$X) == stan_data$D,
  !anyNA(stan_data$X),
  !anyNA(stan_data$y)
)

# -----------------------------------------------------------------------------
# 7. Save bundle
# -----------------------------------------------------------------------------
saveRDS(
  list(
    stan_data = stan_data,
    df_voice = df_voice_stan,
    df_ema = df_ema_stan,
    pid5_vars = pid5_ema_vars,
    pid5_center = attr(X_scaled, "scaled:center"),
    pid5_scale = attr(X_scaled, "scaled:scale"),
    subj_ids = subj_ids
  ),
  file = here("results", "F0", "stan_bundle_f0.rds")
)

cat("\nSaved: results/F0/stan_bundle_f0.rds\n")
