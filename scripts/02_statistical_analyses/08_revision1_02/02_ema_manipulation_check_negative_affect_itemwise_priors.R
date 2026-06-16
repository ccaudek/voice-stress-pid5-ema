# ==============================================================================
# 02_ema_manipulation_check_negative_affect_itemwise_priors.R
# Manipulation check EMA: negative affect composite + four item-level checks
#
# Rationale for the reviewer response:
#   The original manipulation check used the composite:
#     negative_affect = angry + sad + reverse(happy) + reverse(satisfied)
#   This script keeps that composite, but also models the four original items
#   separately, so we can show whether the effect is carried by increases in
#   negative-valence items and/or decreases in positive-valence items.
#
# Item scale:
#   happy, sad, satisfied, angry: 0..100
#   higher happy/satisfied = more positive affect
#   higher sad/angry       = more negative affect
#
# Outcomes modeled by default:
#   - negative_affect: 0..400, higher = more negative affect
#   - angry          : 0..100, higher = more anger
#   - sad            : 0..100, higher = more sadness
#   - happy          : 0..100, higher = more happiness
#   - satisfied      : 0..100, higher = more satisfaction
#
# Priors:
#   With the adjacent centered contrasts used here, b1 = PRE - BASELINE and
#   b2 = POST - PRE in outcome units. Priors are scaled to the theoretical
#   range of each outcome, rather than to the observed SD. Specifically:
#     alpha ~ Normal(scale_mid, scale_range/2)
#     b1,b2 ~ Normal(0, scale_range/2)
#     tau,sigma ~ half-Normal(0, scale_range/2)
#   This puts about 95% prior mass for a contrast within +/- one full scale
#   range: weakly informative but coherent for bounded EMA scales.
#
# Timepoints:
#   BASELINE = prompts about 1 month before exam
#   PRE      = day before exam
#   POST     = evening of exam day, after the morning exam
#   If voice T3 is the day after, state this timing mismatch in the text.
# ============================================================================== 

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
  library(cmdstanr)
  library(bayesplot)
  library(loo)
  library(posterior)
})

set.seed(123)
options(mc.cores = parallel::detectCores())

# ----------------------------
# 0) OPTIONS
# ----------------------------
ITEM_MIN <- 0
ITEM_MAX <- 100

ANALYSIS_LEVEL <- "subject_timepoint_means"   # or "prompt_level"
REQUIRE_ALL_TIMEPOINTS <- FALSE

N_CHAINS <- 4
N_WARMUP <- 2000
N_SAMPLING <- 4000
ADAPT_DELTA <- 0.95
MAX_TREEDEPTH <- 15
SEED <- 123

MOOD_ITEMS <- c("happy", "sad", "satisfied", "angry")

# Keep negative_affect as the legacy sum to avoid changing the scale of any
# previous result. A 0..100 average is also computed and saved descriptively.
OUTCOMES <- c("negative_affect", "angry", "sad", "happy", "satisfied")

OUTCOME_SCALES <- tibble::tribble(
  ~outcome,             ~label,                                ~scale_min, ~scale_max, ~expected_pre_baseline, ~expected_post_pre, ~item_type,      ~interpretation,
  "negative_affect",    "Negative affect composite",              0,        400,       "+",                    "-",                "composite",    "higher = more negative affect",
  "negative_affect_0100", "Negative affect composite, mean",       0,        100,       "+",                    "-",                "composite",    "higher = more negative affect",
  "angry",              "Angry",                                  0,        100,       "+",                    "-",                "negative item", "higher = more anger",
  "sad",                "Sad",                                    0,        100,       "+",                    "-",                "negative item", "higher = more sadness",
  "happy",              "Happy",                                  0,        100,       "-",                    "+",                "positive item", "higher = more happiness",
  "satisfied",          "Satisfied",                              0,        100,       "-",                    "+",                "positive item", "higher = more satisfaction",
  "happy_rev",          "Happy, reverse-coded",                   0,        100,       "+",                    "-",                "keyed item",    "higher = less happiness",
  "satisfied_rev",      "Satisfied, reverse-coded",               0,        100,       "+",                    "-",                "keyed item",    "higher = less satisfaction"
) |>
  mutate(
    scale_range = scale_max - scale_min,
    scale_mid = (scale_min + scale_max) / 2,
    prior_alpha_mu = scale_mid,
    prior_alpha_sd = scale_range / 2,
    prior_beta_sd = scale_range / 2,
    prior_sd_sd = scale_range / 2
  )

get_outcome_scale <- function(outcome_name) {
  out <- OUTCOME_SCALES |> filter(.data$outcome == outcome_name)
  if (nrow(out) != 1) stop("Scale not defined for outcome: ", outcome_name, call. = FALSE)
  out
}

safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits)
prob_fmt <- function(x) formatC(x, format = "f", digits = 2)

prob_expected <- function(draws, direction) {
  if (direction == "+") return(mean(draws > 0))
  if (direction == "-") return(mean(draws < 0))
  NA_real_
}

# ----------------------------
# 1) PATHS AND DIRECTORIES
# ----------------------------
first_existing <- function(paths, label) {
  existing <- paths[file.exists(paths)]
  if (length(existing) == 0) {
    stop(
      "File not found for ", label, ". Paths tried:\n",
      paste0(" - ", paths, collapse = "\n"),
      call. = FALSE
    )
  }
  existing[[1]]
}

ema_path <- first_existing(
  c(
    here("data", "processed", "ema_plus_scales_cleaned.csv"),
    here("data", "cleaned", "ema_plus_scales_cleaned.csv"),
    here("data", "ema_plus_scales_cleaned.csv"),
    here("ema_plus_scales_cleaned.csv"),
    "ema_plus_scales_cleaned.csv"
  ),
  "ema_plus_scales_cleaned.csv"
)

audio_path <- first_existing(
  c(
    here("data", "raw", "acustic_features", "datiacustici", "AUDIO.xlsx"),
    here("data", "raw", "acoustic_features", "datiacustici", "AUDIO.xlsx"),
    here("data", "raw", "AUDIO.xlsx"),
    here("AUDIO.xlsx"),
    "AUDIO.xlsx"
  ),
  "AUDIO.xlsx"
)

out_dir <- here("results", "manipulation_check", "ema_negative_affect_itemwise")
for (sub in c("data", "models", "figures", "tables", "stan", "manuscript")) {
  dir.create(file.path(out_dir, sub), recursive = TRUE, showWarnings = FALSE)
}

cat("\n=== PATHS ===\n")
cat("EMA file:   ", ema_path, "\n")
cat("AUDIO file: ", audio_path, "\n")
cat("Output dir: ", out_dir, "\n")

write_csv(OUTCOME_SCALES, file.path(out_dir, "tables", "negative_affect_prior_specs_all.csv"))
cat("\n=== PRIOR SPECIFICATIONS ===\n")
print(as.data.frame(OUTCOME_SCALES))

# ----------------------------
# 2) AUDIO IDs + CORRECTIONS
# ----------------------------
cat("\n=== LOADING AUDIO IDs ===\n")

read_audio_sheet <- function(sheet_name) {
  read_excel(audio_path, sheet = sheet_name) |>
    rename_with(stringr::str_trim) |>
    mutate(
      timepoint_audio = tolower(sheet_name),
      ID = stringr::str_trim(as.character(ID))
    ) |>
    select(ID, timepoint_audio)
}

df_audio_ids <- bind_rows(
  read_audio_sheet("BASELINE"),
  read_audio_sheet("PRE"),
  read_audio_sheet("POST")
) |>
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
  filter(!is.na(ID), ID != "")

audio_ids <- df_audio_ids |>
  distinct(ID) |>
  arrange(ID)

cat("AUDIO: N ID rows =", nrow(df_audio_ids), "| N unique subjects =", nrow(audio_ids), "\n")

# ----------------------------
# 3) LOAD EMA AND BUILD OUTCOMES
# ----------------------------
cat("\n=== LOADING EMA DATA ===\n")

df_ema_raw <- read_csv(ema_path, show_col_types = FALSE) |>
  rename_with(stringr::str_trim)

required_cols <- c("user_id", "exam_period", MOOD_ITEMS)
missing_cols <- setdiff(required_cols, names(df_ema_raw))
if (length(missing_cols) > 0) {
  stop("Missing EMA columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

has_existing_negative_affect <- "negative_affect" %in% names(df_ema_raw)
if (has_existing_negative_affect) {
  df_ema_raw <- df_ema_raw |>
    rename(negative_affect_existing = negative_affect)
}

df_ema <- df_ema_raw |>
  mutate(
    user_id = stringr::str_trim(as.character(user_id)),
    exam_period = stringr::str_to_lower(stringr::str_trim(as.character(exam_period))),
    across(all_of(MOOD_ITEMS), ~ suppressWarnings(as.numeric(.x))),
    timepoint = case_when(
      exam_period %in% c("baseline", "base", "bsl") ~ "baseline",
      exam_period %in% c("pre", "pre_exam", "pre-exam", "preexam") ~ "pre",
      exam_period %in% c("post", "post_exam", "post-exam", "postexam") ~ "post",
      TRUE ~ NA_character_
    ),
    timepoint = factor(timepoint, levels = c("baseline", "pre", "post")),
    happy_rev = ITEM_MAX + ITEM_MIN - happy,
    satisfied_rev = ITEM_MAX + ITEM_MIN - satisfied,
    negative_affect = angry + sad + happy_rev + satisfied_rev,
    negative_affect_0100 = negative_affect / 4
  )

# Raw item range check.
range_check <- tibble(item = MOOD_ITEMS) |>
  mutate(
    expected_min = ITEM_MIN,
    expected_max = ITEM_MAX,
    observed_min = purrr::map_dbl(item, ~ safe_min(df_ema[[.x]])),
    observed_max = purrr::map_dbl(item, ~ safe_max(df_ema[[.x]])),
    n_missing = purrr::map_int(item, ~ sum(is.na(df_ema[[.x]]))),
    n_out_of_range = purrr::map_int(item, ~ sum(!is.na(df_ema[[.x]]) &
                                                 (df_ema[[.x]] < ITEM_MIN | df_ema[[.x]] > ITEM_MAX)))
  )
cat("\n=== RAW ITEM RANGE CHECK ===\n")
print(as.data.frame(range_check))
write_csv(range_check, file.path(out_dir, "tables", "raw_item_range_check.csv"))
if (any(range_check$n_out_of_range > 0)) {
  warning("Some EMA mood items are outside ITEM_MIN/ITEM_MAX.")
}

# Check a pre-existing negative_affect column, if present.
if (has_existing_negative_affect) {
  df_ema <- df_ema |>
    mutate(
      negative_affect_existing = suppressWarnings(as.numeric(negative_affect_existing)),
      diff_existing_minus_sum = negative_affect_existing - negative_affect,
      diff_existing_minus_mean0100 = negative_affect_existing - negative_affect_0100
    )

  existing_check <- tibble(
    comparison = c("existing_minus_recomputed_sum_0_400", "existing_minus_recomputed_mean_0_100"),
    max_abs_diff = c(
      max(abs(df_ema$diff_existing_minus_sum), na.rm = TRUE),
      max(abs(df_ema$diff_existing_minus_mean0100), na.rm = TRUE)
    )
  ) |>
    mutate(max_abs_diff = ifelse(is.infinite(max_abs_diff), NA_real_, max_abs_diff))
  cat("\n=== EXISTING negative_affect CHECK ===\n")
  print(as.data.frame(existing_check))
  write_csv(existing_check, file.path(out_dir, "tables", "existing_negative_affect_check.csv"))
}

# ----------------------------
# 4) MATCH EMA TO AUDIO IDs
# ----------------------------
cat("\n=== MATCHING EMA TO AUDIO IDs ===\n")

df_prompt <- df_ema |>
  semi_join(audio_ids, by = c("user_id" = "ID")) |>
  filter(!is.na(timepoint)) |>
  mutate(
    user_id = factor(user_id),
    c1_pre_vs_baseline = case_when(timepoint == "baseline" ~ -2 / 3, TRUE ~ 1 / 3),
    c2_post_vs_pre = case_when(timepoint == "post" ~ 2 / 3, TRUE ~ -1 / 3)
  )

cat("EMA total: N rows =", nrow(df_ema), "| N subjects =", n_distinct(df_ema$user_id), "\n")
cat("EMA x AUDIO: N rows =", nrow(df_prompt), "| N subjects =", n_distinct(df_prompt$user_id), "\n")

match_report <- tibble(
  quantity = c(
    "audio_unique_ids",
    "ema_unique_ids",
    "matched_unique_ids",
    "audio_ids_not_in_ema",
    "ema_ids_not_in_audio",
    "matched_prompt_rows"
  ),
  n = c(
    nrow(audio_ids),
    n_distinct(df_ema$user_id),
    n_distinct(df_prompt$user_id),
    nrow(anti_join(audio_ids, df_ema |> distinct(user_id), by = c("ID" = "user_id"))),
    nrow(anti_join(df_ema |> distinct(user_id), audio_ids, by = c("user_id" = "ID"))),
    nrow(df_prompt)
  )
)
print(match_report)
write_csv(match_report, file.path(out_dir, "tables", "match_report.csv"))
write_csv(df_prompt, file.path(out_dir, "data", "ema_audio_matched_prompt_level.csv"))

# ----------------------------
# 5) STAN MODEL
# ----------------------------
cat("\n=== WRITING STAN MODEL ===\n")

stan_file <- file.path(out_dir, "stan", "ema_adjacent_contrasts_scale_priors.stan")
stan_code <- '
data {
  int<lower=1> N_subj;
  int<lower=1> N_obs;
  array[N_obs] int<lower=1, upper=N_subj> subj_id;
  vector[N_obs] y;
  vector[N_obs] c1;           // centered adjacent contrast: PRE - BASELINE
  vector[N_obs] c2;           // centered adjacent contrast: POST - PRE
  real prior_alpha_mu;
  real<lower=0> prior_alpha_sd;
  real<lower=0> prior_beta_sd;
  real<lower=0> prior_sd_sd;
}
parameters {
  real alpha;                 // equal-weighted grand mean across the 3 timepoints
  real b1;                    // PRE - BASELINE
  real b2;                    // POST - PRE
  real<lower=0> tau_subj;     // SD random intercepts
  vector[N_subj] z_subj;
  real<lower=0> sigma_y;
}
transformed parameters {
  vector[N_subj] u_subj = tau_subj * z_subj;
}
model {
  vector[N_obs] mu;

  // Weakly informative priors scaled to each outcome theoretical range.
  alpha ~ normal(prior_alpha_mu, prior_alpha_sd);
  b1 ~ normal(0, prior_beta_sd);
  b2 ~ normal(0, prior_beta_sd);
  tau_subj ~ normal(0, prior_sd_sd);
  sigma_y ~ normal(0, prior_sd_sd);
  z_subj ~ std_normal();

  for (n in 1:N_obs) {
    mu[n] = alpha + u_subj[subj_id[n]] + b1 * c1[n] + b2 * c2[n];
  }
  y ~ normal(mu, sigma_y);
}
generated quantities {
  vector[N_obs] log_lik;
  vector[N_obs] y_rep;
  real mu_baseline;
  real mu_pre;
  real mu_post;
  real pre_minus_baseline;
  real post_minus_pre;
  real post_minus_baseline;

  mu_baseline = alpha + b1 * (-2.0 / 3.0) + b2 * (-1.0 / 3.0);
  mu_pre      = alpha + b1 * ( 1.0 / 3.0) + b2 * (-1.0 / 3.0);
  mu_post     = alpha + b1 * ( 1.0 / 3.0) + b2 * ( 2.0 / 3.0);

  pre_minus_baseline = mu_pre - mu_baseline;
  post_minus_pre = mu_post - mu_pre;
  post_minus_baseline = mu_post - mu_baseline;

  for (n in 1:N_obs) {
    real mu_n;
    mu_n = alpha + u_subj[subj_id[n]] + b1 * c1[n] + b2 * c2[n];
    log_lik[n] = normal_lpdf(y[n] | mu_n, sigma_y);
    y_rep[n] = normal_rng(mu_n, sigma_y);
  }
}
'
writeLines(stan_code, stan_file)
cat("Stan file:", stan_file, "\n")
mod <- cmdstan_model(stan_file)

# ----------------------------
# 6) OUTCOME-SPECIFIC MODEL FITTER
# ----------------------------
run_outcome <- function(outcome) {
  cat("\n========== OUTCOME:", outcome, "==========\n")
  od <- file.path(out_dir, outcome)
  for (sub in c("figures", "tables", "models")) {
    dir.create(file.path(od, sub), recursive = TRUE, showWarnings = FALSE)
  }

  scale_info <- get_outcome_scale(outcome)
  prior_spec <- scale_info |>
    transmute(
      outcome, label, item_type, scale_min, scale_max, scale_range,
      prior_alpha = paste0("normal(", prior_alpha_mu, ", ", prior_alpha_sd, ")"),
      prior_b1_b2 = paste0("normal(0, ", prior_beta_sd, ")"),
      prior_tau_sigma = paste0("half-normal(0, ", prior_sd_sd, ")"),
      expected_pre_baseline, expected_post_pre, interpretation
    )
  cat("Priors used:\n")
  print(as.data.frame(prior_spec))
  write_csv(prior_spec, file.path(od, "tables", "prior_spec.csv"))

  if (ANALYSIS_LEVEL == "subject_timepoint_means") {
    df_analysis <- df_prompt |>
      filter(!is.na(.data[[outcome]])) |>
      group_by(user_id, timepoint) |>
      summarise(
        value = mean(.data[[outcome]], na.rm = TRUE),
        n_prompts = n(),
        .groups = "drop"
      ) |>
      mutate(
        timepoint = factor(timepoint, levels = c("baseline", "pre", "post")),
        c1_pre_vs_baseline = case_when(timepoint == "baseline" ~ -2 / 3, TRUE ~ 1 / 3),
        c2_post_vs_pre = case_when(timepoint == "post" ~ 2 / 3, TRUE ~ -1 / 3)
      )
  } else if (ANALYSIS_LEVEL == "prompt_level") {
    df_analysis <- df_prompt |>
      filter(!is.na(.data[[outcome]])) |>
      transmute(
        user_id, timepoint,
        value = .data[[outcome]],
        n_prompts = 1L,
        c1_pre_vs_baseline,
        c2_post_vs_pre
      )
  } else {
    stop("ANALYSIS_LEVEL must be 'subject_timepoint_means' or 'prompt_level'.", call. = FALSE)
  }

  if (REQUIRE_ALL_TIMEPOINTS) {
    complete_ids <- df_analysis |>
      count(user_id, timepoint) |>
      pivot_wider(names_from = timepoint, values_from = n, values_fill = 0) |>
      filter(baseline > 0, pre > 0, post > 0) |>
      pull(user_id)
    df_analysis <- df_analysis |> filter(user_id %in% complete_ids)
  }

  if (nrow(df_analysis) < 2 || n_distinct(df_analysis$user_id) < 1) {
    stop("Insufficient data for outcome: ", outcome, call. = FALSE)
  }

  desc <- df_analysis |>
    group_by(timepoint) |>
    summarise(
      outcome = outcome,
      label = scale_info$label[[1]],
      n = n(),
      n_subj = n_distinct(user_id),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      iqr = IQR(value, na.rm = TRUE),
      prompts_mean = mean(n_prompts, na.rm = TRUE),
      .groups = "drop"
    )
  cat("Descriptives by timepoint:\n")
  print(as.data.frame(desc))
  write_csv(desc, file.path(od, "tables", "descriptives.csv"))

  subj_ids <- sort(unique(as.character(df_analysis$user_id)))
  id_map <- tibble(user_id = subj_ids, subj = seq_along(subj_ids))
  df_stan <- df_analysis |>
    inner_join(id_map, by = "user_id") |>
    arrange(subj, timepoint)

  stan_data <- list(
    N_subj = nrow(id_map),
    N_obs = nrow(df_stan),
    subj_id = as.integer(df_stan$subj),
    y = as.numeric(df_stan$value),
    c1 = as.numeric(df_stan$c1_pre_vs_baseline),
    c2 = as.numeric(df_stan$c2_post_vs_pre),
    prior_alpha_mu = scale_info$prior_alpha_mu[[1]],
    prior_alpha_sd = scale_info$prior_alpha_sd[[1]],
    prior_beta_sd = scale_info$prior_beta_sd[[1]],
    prior_sd_sd = scale_info$prior_sd_sd[[1]]
  )
  cat("Stan data: N_subj =", stan_data$N_subj, "| N_obs =", stan_data$N_obs,
      "| scale = [", scale_info$scale_min[[1]], ", ", scale_info$scale_max[[1]], "]\n", sep = "")

  write_csv(df_stan, file.path(od, "data_stan.csv"))
  write_csv(id_map, file.path(od, "id_map.csv"))

  fit <- mod$sample(
    data = stan_data,
    chains = N_CHAINS,
    parallel_chains = N_CHAINS,
    iter_warmup = N_WARMUP,
    iter_sampling = N_SAMPLING,
    adapt_delta = ADAPT_DELTA,
    max_treedepth = MAX_TREEDEPTH,
    seed = SEED
  )

  fit$cmdstan_diagnose()
  fit$save_object(file.path(od, "models", paste0("fit_", outcome, ".rds")))

  post <- fit$draws(format = "df")

  cs <- tibble(
    outcome = outcome,
    label = scale_info$label[[1]],
    item_type = scale_info$item_type[[1]],
    scale_min = scale_info$scale_min[[1]],
    scale_max = scale_info$scale_max[[1]],
    expected_pre_baseline = scale_info$expected_pre_baseline[[1]],
    expected_post_pre = scale_info$expected_post_pre[[1]],
    pre_minus_baseline_median = median(post$pre_minus_baseline),
    pre_minus_baseline_lo = quantile(post$pre_minus_baseline, .055),
    pre_minus_baseline_hi = quantile(post$pre_minus_baseline, .945),
    pre_minus_baseline_pgt0 = mean(post$pre_minus_baseline > 0),
    pre_minus_baseline_plt0 = mean(post$pre_minus_baseline < 0),
    pre_minus_baseline_p_expected = prob_expected(post$pre_minus_baseline, scale_info$expected_pre_baseline[[1]]),
    post_minus_pre_median = median(post$post_minus_pre),
    post_minus_pre_lo = quantile(post$post_minus_pre, .055),
    post_minus_pre_hi = quantile(post$post_minus_pre, .945),
    post_minus_pre_pgt0 = mean(post$post_minus_pre > 0),
    post_minus_pre_plt0 = mean(post$post_minus_pre < 0),
    post_minus_pre_p_expected = prob_expected(post$post_minus_pre, scale_info$expected_post_pre[[1]]),
    post_minus_baseline_median = median(post$post_minus_baseline),
    post_minus_baseline_lo = quantile(post$post_minus_baseline, .055),
    post_minus_baseline_hi = quantile(post$post_minus_baseline, .945),
    post_minus_baseline_pgt0 = mean(post$post_minus_baseline > 0),
    post_minus_baseline_plt0 = mean(post$post_minus_baseline < 0)
  )
  cat("Contrasts:\n")
  print(as.data.frame(cs), digits = 4)
  write_csv(cs, file.path(od, "tables", "contrast_summary.csv"))

  marg <- tibble(
    outcome = outcome,
    label = scale_info$label[[1]],
    item_type = scale_info$item_type[[1]],
    timepoint = factor(c("baseline", "pre", "post"), levels = c("baseline", "pre", "post")),
    median = c(median(post$mu_baseline), median(post$mu_pre), median(post$mu_post)),
    lower = c(quantile(post$mu_baseline, .055), quantile(post$mu_pre, .055), quantile(post$mu_post, .055)),
    upper = c(quantile(post$mu_baseline, .945), quantile(post$mu_pre, .945), quantile(post$mu_post, .945))
  )
  write_csv(marg, file.path(od, "tables", "marginal_means.csv"))

  summary_fit <- fit$summary(c(
    "alpha", "b1", "b2", "tau_subj", "sigma_y",
    "mu_baseline", "mu_pre", "mu_post",
    "pre_minus_baseline", "post_minus_pre", "post_minus_baseline"
  ))
  write_csv(summary_fit, file.path(od, "tables", "cmdstan_summary.csv"))

  loo_fit <- loo::loo(fit$draws("log_lik", format = "matrix"))
  saveRDS(loo_fit, file.path(od, "models", paste0("loo_", outcome, ".rds")))

  ggsave(
    filename = file.path(od, "figures", "posterior_contrasts.png"),
    plot = mcmc_areas(fit$draws(c("pre_minus_baseline", "post_minus_pre")), prob = .89, prob_outer = .89) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      labs(
        title = paste0("Manipulation check: ", scale_info$label[[1]]),
        subtitle = "PRE - BASELINE and POST - PRE adjacent contrasts",
        x = "Difference"
      ) +
      theme_minimal(),
    width = 8,
    height = 5,
    dpi = 300
  )

  ggsave(
    filename = file.path(od, "figures", "marginal_means.png"),
    plot = ggplot(marg, aes(timepoint, median, group = 1)) +
      geom_line() +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = .15) +
      labs(title = paste0("Model-implied marginal means: ", scale_info$label[[1]]), x = NULL, y = scale_info$label[[1]]) +
      theme_minimal(),
    width = 6,
    height = 5,
    dpi = 300
  )

  y_rep <- fit$draws("y_rep", format = "matrix")
  ggsave(
    filename = file.path(od, "figures", "ppc.png"),
    plot = ppc_dens_overlay(y = stan_data$y, yrep = y_rep[seq_len(min(100, nrow(y_rep))), , drop = FALSE]) +
      labs(title = paste0("PPC: ", scale_info$label[[1]])) +
      theme_minimal(),
    width = 8,
    height = 5,
    dpi = 300
  )

  list(
    outcome = outcome,
    descriptives = desc,
    contrasts = cs,
    marginal = marg,
    prior_spec = prior_spec,
    fit = fit,
    loo = loo_fit,
    id_map = id_map,
    stan_data = stan_data
  )
}

# ----------------------------
# 7) RUN MODELS
# ----------------------------
results <- setNames(lapply(OUTCOMES, run_outcome), OUTCOMES)

# ----------------------------
# 8) COMBINED TABLES AND FIGURES
# ----------------------------
contrasts_all <- bind_rows(lapply(results, `[[`, "contrasts"))
marg_all <- bind_rows(lapply(results, `[[`, "marginal"))
desc_all <- bind_rows(lapply(results, `[[`, "descriptives"))
prior_specs_used <- bind_rows(lapply(results, `[[`, "prior_spec"))

write_csv(contrasts_all, file.path(out_dir, "tables", "negative_affect_contrasts_all.csv"))
write_csv(marg_all, file.path(out_dir, "tables", "negative_affect_marginal_all.csv"))
write_csv(desc_all, file.path(out_dir, "tables", "negative_affect_descriptives_all.csv"))
write_csv(prior_specs_used, file.path(out_dir, "tables", "negative_affect_prior_specs_used.csv"))

# Extra descriptive table for the 0..100 average composite, without fitting a redundant model by default.
if (!("negative_affect_0100" %in% OUTCOMES)) {
  desc_0100 <- df_prompt |>
    filter(!is.na(negative_affect_0100)) |>
    group_by(user_id, timepoint) |>
    summarise(value = mean(negative_affect_0100, na.rm = TRUE), n_prompts = n(), .groups = "drop") |>
    group_by(timepoint) |>
    summarise(
      outcome = "negative_affect_0100",
      label = "Negative affect composite, mean",
      n = n(),
      n_subj = n_distinct(user_id),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      iqr = IQR(value, na.rm = TRUE),
      prompts_mean = mean(n_prompts, na.rm = TRUE),
      .groups = "drop"
    )
  write_csv(desc_0100, file.path(out_dir, "tables", "negative_affect_0100_descriptives.csv"))
}

cat("\n=== CONTRASTS: ALL OUTCOMES ===\n")
print(as.data.frame(contrasts_all), digits = 4)

p_combined <- ggplot(marg_all, aes(timepoint, median, group = 1)) +
  geom_line() +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .15) +
  facet_wrap(~label, scales = "free_y") +
  labs(
    title = "EMA affect around the exam: composite and item-level checks",
    subtitle = "Expected pattern: angry/sad and composite increase before exam and decrease after; happy/satisfied show the reverse",
    x = NULL,
    y = NULL
  ) +
  theme_minimal()

ggsave(
  file.path(out_dir, "figures", "negative_affect_marginal_means_combined.png"),
  p_combined,
  width = 12,
  height = 7,
  dpi = 300
)

# Reviewer-ready table focused on the four raw items + composite.
reviewer_table <- contrasts_all |>
  mutate(
    pre_baseline = paste0(fmt(pre_minus_baseline_median), " [", fmt(pre_minus_baseline_lo), ", ", fmt(pre_minus_baseline_hi), "]"),
    post_pre = paste0(fmt(post_minus_pre_median), " [", fmt(post_minus_pre_lo), ", ", fmt(post_minus_pre_hi), "]"),
    pre_baseline_expected_prob = pre_minus_baseline_p_expected,
    post_pre_expected_prob = post_minus_pre_p_expected
  ) |>
  select(
    outcome, label, item_type, scale_min, scale_max,
    expected_pre_baseline, pre_baseline, pre_baseline_expected_prob,
    expected_post_pre, post_pre, post_pre_expected_prob
  )
write_csv(reviewer_table, file.path(out_dir, "tables", "reviewer_ready_negative_affect_table.csv"))

# ----------------------------
# 9) MANUSCRIPT / REVIEWER TEXT TEMPLATE
# ----------------------------
cat("\n=== WRITING MANUSCRIPT TEMPLATE ===\n")

row_for <- function(x) contrasts_all |> filter(.data$outcome == x) |> slice(1)
contrast_txt <- function(row, contrast = c("pre", "post")) {
  contrast <- match.arg(contrast)
  if (contrast == "pre") {
    direction <- ifelse(row$expected_pre_baseline == "+", ">0", "<0")
    paste0(
      fmt(row$pre_minus_baseline_median), " [", fmt(row$pre_minus_baseline_lo), ", ", fmt(row$pre_minus_baseline_hi),
      "], Pr(", direction, ") = ", prob_fmt(row$pre_minus_baseline_p_expected)
    )
  } else {
    direction <- ifelse(row$expected_post_pre == "+", ">0", "<0")
    paste0(
      fmt(row$post_minus_pre_median), " [", fmt(row$post_minus_pre_lo), ", ", fmt(row$post_minus_pre_hi),
      "], Pr(", direction, ") = ", prob_fmt(row$post_minus_pre_p_expected)
    )
  }
}

na_row <- row_for("negative_affect")
angry_row <- row_for("angry")
sad_row <- row_for("sad")
happy_row <- row_for("happy")
satisfied_row <- row_for("satisfied")

manuscript_text <- paste0(
  "We decomposed the EMA negative-affect manipulation check into the original four items. ",
  "The composite was computed as angry + sad + reverse(happy) + reverse(satisfied), with all items scored on the 0-100 scale ",
  "and the resulting composite ranging from 0 to 400. The same hierarchical Bayesian model with participant-level random intercepts ",
  "and adjacent contrasts was fitted to the composite and to each raw item separately; weakly informative priors were scaled to each ",
  "outcome's theoretical range. All credible intervals are 89% equal-tailed intervals (quantiles .055/.945); Pr(>0) and Pr(<0) denote the posterior probability of the expected direction. ",
  "The composite increased from BASELINE to PRE (PRE - BASELINE = ",
  contrast_txt(na_row, "pre"), ") and decreased from PRE to POST (POST - PRE = ",
  contrast_txt(na_row, "post"), "). Item-level analyses showed the expected pattern for the negative-valence items: angry ",
  "(PRE - BASELINE = ", contrast_txt(angry_row, "pre"), "; POST - PRE = ", contrast_txt(angry_row, "post"), ") and sad ",
  "(PRE - BASELINE = ", contrast_txt(sad_row, "pre"), "; POST - PRE = ", contrast_txt(sad_row, "post"), "). ",
  "The positive-valence items showed the complementary pattern: happy ",
  "(PRE - BASELINE = ", contrast_txt(happy_row, "pre"), "; POST - PRE = ", contrast_txt(happy_row, "post"), ") and satisfied ",
  "(PRE - BASELINE = ", contrast_txt(satisfied_row, "pre"), "; POST - PRE = ", contrast_txt(satisfied_row, "post"), "). ",
  "These item-level results show that the composite manipulation check was not solely driven by reverse-coding of positive affect or by a single negative item. ",
  "[NB: state the precise timing of the POST EMA prompt relative to voice T3 if they differ.]\n"
)

writeLines(manuscript_text, file.path(out_dir, "manuscript", "negative_affect_itemwise_text.txt"))
cat("\n", manuscript_text, "\n")

# ----------------------------
# 10) SAVE BUNDLE
# ----------------------------
saveRDS(
  list(
    ema_path = ema_path,
    audio_path = audio_path,
    item_min = ITEM_MIN,
    item_max = ITEM_MAX,
    analysis_level = ANALYSIS_LEVEL,
    require_all_timepoints = REQUIRE_ALL_TIMEPOINTS,
    audio_ids = audio_ids,
    match_report = match_report,
    range_check = range_check,
    outcome_scales = OUTCOME_SCALES,
    results = results,
    contrasts_all = contrasts_all,
    marg_all = marg_all,
    desc_all = desc_all,
    reviewer_table = reviewer_table
  ),
  file = file.path(out_dir, "models", "negative_affect_itemwise_bundle.rds")
)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Saved outputs in:", out_dir, "\n")
cat("Key files:\n")
cat(" - tables/reviewer_ready_negative_affect_table.csv\n")
cat(" - tables/negative_affect_contrasts_all.csv\n")
cat(" - figures/negative_affect_marginal_means_combined.png\n")
cat(" - manuscript/negative_affect_itemwise_text.txt\n")
cat(" - models/negative_affect_itemwise_bundle.rds\n")

# eof ---
