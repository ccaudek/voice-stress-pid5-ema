# ==============================================================================
# 20_followup_analyses_pid5_voice.R
#
# Goal: run the *additional* analyses needed to strengthen the central claim:
#       EMA PID-5 domains moderate stress-related acoustic change.
#
# What this script does (end-to-end):
#   (A) Re-load the same VOICE + EMA cleaned sources you already used
#   (B) Build 3 moderator variants for each outcome (F0mean, NNE):
#       1) EMA-only (your current Stan model)
#       2) Baseline-only (PID-5 full at baseline, single measure)
#       3) Combined (EMA + baseline entered together; incremental value)
#   (C) Fit models (cmdstanr) and compare with LOO (expected log predictive density)
#   (D) Produce a compact “manuscript-ready” table:
#       - main contrasts (Δstress, Δrecovery computed correctly from b1/b2)
#       - moderation PD and P(direction) for each domain
#       - model comparison (EMA vs baseline vs combined)
#   (E) Optional: “temporal covariation” sanity checks (EMA near recording date)
#
# Notes:
# - Uses your contrast coding: baseline(-0.5,0), pre(+0.5,-0.5), post(0,+0.5)
#   so Δstress = b1 - 0.5*b2 and Δrecovery = b2 - 0.5*b1.
# - Baseline PID-5 variable names differ across datasets; the script tries to detect them.
# - Requires: cmdstanr, posterior, loo, tidyverse, readxl, here, rio, jsonlite
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
  library(rio)
  library(jsonlite)
  library(cmdstanr)
  library(posterior)
  library(loo)
})

# ------------------------------------------------------------------------------
# 0) PATHS (edit if needed)
# ------------------------------------------------------------------------------

voice_path <- here("data", "raw", "acustic_features", "datiacustici", "AUDIO.xlsx")
ema_path   <- here("data", "processed", "ema_plus_scales_cleaned.csv")

# Stan files you already have (EMA measurement model version)
stan_file_f0_ema  <- here("stan", "F0", "f0mean_pid5_moderation.stan")
stan_file_nne_ema <- here("stan", "NNE", "nne_pid5_moderation.stan")

# NEW Stan files (baseline-only and combined)
# (this script will WRITE them to stan/ if they don't exist)
stan_file_baseline <- here("stan", "pid5_baseline_moderation.stan")
stan_file_combined <- here("stan", "pid5_ema_plus_baseline_moderation.stan")

# Output folders
dir.create(here("results", "followup"), recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(voice_path), file.exists(ema_path))
stopifnot(file.exists(stan_file_f0_ema), file.exists(stan_file_nne_ema))

# ------------------------------------------------------------------------------
# 1) LOAD VOICE and construct outcomes (F0mean, NNE mean across vowels)
# ------------------------------------------------------------------------------

load_voice <- function(voice_path) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(readxl)
    library(stringr)
    library(tidyr)
  })
  
  stopifnot(file.exists(voice_path))
  
  # ---------- helper: robust column finder ----------
  find_col <- function(nm, prefix, vowel) {
    # matches e.g. "NNE /a/" or "F0 mean Hz /a/" with flexible spaces/case
    pat <- paste0("^\\s*", prefix, "\\s*/\\s*", vowel, "\\s*/\\s*$")
    hit <- nm[grepl(pat, nm, ignore.case = TRUE)]
    if (length(hit) == 0) return(NA_character_)
    hit[1]
  }
  
  # ---------- read 3 sheets ----------
  baseline <- read_excel(voice_path, sheet = "BASELINE") %>% mutate(timepoint = "baseline")
  pre      <- read_excel(voice_path, sheet = "PRE")      %>% mutate(timepoint = "pre")
  post     <- read_excel(voice_path, sheet = "POST")     %>% mutate(timepoint = "post")
  
  df <- bind_rows(baseline, pre, post)
  names(df) <- str_trim(names(df))
  
  # ---------- ID corrections (as in your pipeline) ----------
  df <- df %>%
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
  
  # ---------- detect needed NNE columns ----------
  nm <- names(df)
  nne_a_nm <- find_col(nm, "NNE", "a")
  nne_i_nm <- find_col(nm, "NNE", "i")
  nne_u_nm <- find_col(nm, "NNE", "u")
  
  if (anyNA(c(nne_a_nm, nne_i_nm, nne_u_nm))) {
    cand <- nm[grepl("NNE", nm, ignore.case = TRUE)]
    stop(
      "Could not find NNE columns for all vowels.\n",
      "Found candidates containing 'NNE':\n- ",
      paste(cand, collapse = "\n- "),
      "\n\nExpected columns like: 'NNE /a/', 'NNE /i/', 'NNE /u/'."
    )
  }
  
  # ---------- build tidy voice df with outcome + contrasts ----------
  df_voice <- df %>%
    transmute(
      ID = as.character(ID),
      timepoint = factor(timepoint, levels = c("baseline", "pre", "post")),
      nne_a = as.numeric(.data[[nne_a_nm]]),
      nne_i = as.numeric(.data[[nne_i_nm]]),
      nne_u = as.numeric(.data[[nne_u_nm]]),
      y_nne = rowMeans(across(c(nne_a, nne_i, nne_u)), na.rm = TRUE)
    ) %>%
    mutate(
      c1_stress = case_when(
        timepoint == "baseline" ~ -0.5,
        timepoint == "pre"      ~  0.5,
        timepoint == "post"     ~  0.0,
        TRUE ~ NA_real_
      ),
      c2_recovery = case_when(
        timepoint == "baseline" ~  0.0,
        timepoint == "pre"      ~ -0.5,
        timepoint == "post"     ~  0.5,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(
      !is.na(ID),
      !is.na(timepoint),
      !is.na(y_nne),
      !is.na(c1_stress),
      !is.na(c2_recovery)
    ) %>%
    arrange(ID, timepoint)
  
  # quick sanity info
  message(
    "VOICE loaded: N obs = ", nrow(df_voice),
    " | N subj = ", dplyr::n_distinct(df_voice$ID),
    " | N per timepoint = ",
    paste(with(df_voice, table(timepoint)), collapse = ", ")
  )
  
  df_voice
}

df_voice <- load_voice(voice_path)
cat("VOICE loaded. N rows:", nrow(df_voice), " | N subj:", n_distinct(df_voice$ID), "\n")

# ------------------------------------------------------------------------------
# 2) LOAD EMA cleaned, extract EMA-PID5 domains + baseline PID-5 domains if present
# ------------------------------------------------------------------------------

ema <- rio::import(ema_path) %>% as_tibble()
stopifnot("user_id" %in% names(ema))

pid5_ema_vars <- c(
  "pid5_negative_affectivity",
  "pid5_detachment",
  "pid5_antagonism",
  "pid5_disinhibition",
  "pid5_psychoticism"
)

stopifnot(all(pid5_ema_vars %in% names(ema)))

# Attempt to detect baseline full PID-5 domain scores in cleaned file.
# From your descriptives you had names like:
# domain_negative_affect_baseline, domain_detachment_baseline, etc.
# We'll accept multiple naming conventions.
baseline_map <- tribble(
  ~domain, ~candidates,
  "negative_affectivity", c("domain_negative_affect_baseline", "pid5_negative_affectivity_baseline", "negative_affect_baseline"),
  "detachment",          c("domain_detachment_baseline",       "pid5_detachment_baseline",          "detachment_baseline"),
  "antagonism",          c("domain_antagonism_baseline",       "pid5_antagonism_baseline",          "antagonism_baseline"),
  "disinhibition",       c("domain_disinhibition_baseline",    "pid5_disinhibition_baseline",       "disinhibition_baseline"),
  "psychoticism",        c("domain_psychoticism_baseline",     "pid5_psychoticism_baseline",        "psychoticism_baseline")
)

pick_existing <- function(cands, nm) {
  hit <- cands[cands %in% nm]
  if (length(hit) == 0) NA_character_ else hit[1]
}

baseline_vars_found <- baseline_map %>%
  mutate(var = map_chr(candidates, pick_existing, nm = names(ema))) %>%
  select(domain, var)

if (any(is.na(baseline_vars_found$var))) {
  cat("\n[WARN] Not all baseline PID-5 domains were found in ema_plus_scales_cleaned.\n")
  print(baseline_vars_found)
  cat(
    "\nIf baseline PID-5 is stored elsewhere (e.g., pid5_scores_noEMA.csv) and joined earlier,\n",
    "either re-join it into ema_plus_scales_cleaned OR point this script to that file and merge by user_id.\n\n",
    sep = ""
  )
}

baseline_pid5_vars <- baseline_vars_found$var[!is.na(baseline_vars_found$var)]
baseline_pid5_vars

# Minimal EMA dataset for Stan measurement model
df_ema_long <- ema %>%
  transmute(ID = tolower(user_id), across(all_of(pid5_ema_vars), as.numeric)) %>%
  filter(!is.na(ID)) %>%
  filter(ID %in% unique(df_voice$ID)) %>%
  filter(if_any(all_of(pid5_ema_vars), ~ !is.na(.x)))

# Baseline subject-level dataset (one row per subject)
df_baseline <- ema %>%
  transmute(ID = tolower(user_id), across(all_of(baseline_pid5_vars), as.numeric)) %>%
  distinct() %>%
  filter(!is.na(ID)) %>%
  filter(ID %in% unique(df_voice$ID))

# ------------------------------------------------------------------------------
# 3) Build consistent subject index + outcome-specific voice frames
# ------------------------------------------------------------------------------

subj_ids <- sort(unique(intersect(df_voice$ID, df_ema_long$ID)))
N_subj <- length(subj_ids)

id_map <- tibble(ID = subj_ids, subj = seq_len(N_subj))

voice_stan <- df_voice %>%
  mutate(ID = tolower(ID)) %>%
  inner_join(id_map, by = "ID") %>%
  arrange(subj, timepoint)

ema_stan <- df_ema_long %>%
  inner_join(id_map, by = "ID") %>%
  arrange(subj)

baseline_stan <- df_baseline %>%
  inner_join(id_map, by = "ID") %>%
  arrange(subj)

cat("FINAL indexing: N_subj =", N_subj,
    "| voice rows =", nrow(voice_stan),
    "| ema rows =", nrow(ema_stan),
    "| baseline rows =", nrow(baseline_stan), "\n")

# ------------------------------------------------------------------------------
# 4) Utility: within-subject mean imputation for EMA domains (needed for Stan X)
# ------------------------------------------------------------------------------

impute_within_subject_mean <- function(df, id_col, vars) {
  df %>%
    group_by(.data[[id_col]]) %>%
    mutate(across(all_of(vars), ~ if_else(is.na(.x), mean(.x, na.rm = TRUE), .x))) %>%
    ungroup() %>%
    filter(if_all(all_of(vars), ~ is.finite(.x)))
}

ema_stan_imp <- impute_within_subject_mean(ema_stan, "ID", pid5_ema_vars)

# Scale EMA and baseline predictors (so coefficients are per +1 SD)
X_ema <- ema_stan_imp %>% select(all_of(pid5_ema_vars)) %>% as.matrix()
stopifnot(!anyNA(X_ema))
X_ema_z <- scale(X_ema)
ema_center <- attr(X_ema_z, "scaled:center")
ema_scale  <- attr(X_ema_z, "scaled:scale")

Z_base <- NULL
base_center <- base_scale <- NULL
if (length(baseline_pid5_vars) > 0) {
  Z_base <- baseline_stan %>% select(all_of(baseline_pid5_vars)) %>% as.matrix()
  # If baseline has missingness, do a simple subject-level mean imputation (across subjects)
  # (better: multiple imputation; but for robustness checks this is acceptable)
  if (anyNA(Z_base)) {
    col_means <- apply(Z_base, 2, function(v) mean(v, na.rm = TRUE))
    for (j in seq_len(ncol(Z_base))) Z_base[is.na(Z_base[, j]), j] <- col_means[j]
  }
  Z_base_z <- scale(Z_base)
  base_center <- attr(Z_base_z, "scaled:center")
  base_scale  <- attr(Z_base_z, "scaled:scale")
}

# ------------------------------------------------------------------------------
# 5) Write Stan code for baseline-only and combined models (if not present)
# ------------------------------------------------------------------------------

# Baseline-only moderation model: y ~ alpha + b1*c1 + b2*c2 + (u0 + u1*c1 + u2*c2)
# plus interactions with baseline domains: (b1 + g1'Z)*c1 + (b2 + g2'Z)*c2
# (no EMA measurement model here)
write_stan_baseline <- function(path) {
  stan_code <- '
data {
  int<lower=1> N_subj;
  int<lower=1> N_voice;
  array[N_voice] int<lower=1, upper=N_subj> subj_voice;
  vector[N_voice] y;
  vector[N_voice] c1;
  vector[N_voice] c2;

  int<lower=1> D_base;
  matrix[N_subj, D_base] Z; // baseline domains, standardized
}

parameters {
  real alpha;

  real b1;
  real b2;

  vector[D_base] g1;  // moderation on stress contrast
  vector[D_base] g2;  // moderation on recovery contrast

  vector[N_subj] u0;
  vector[N_subj] u1;
  vector[N_subj] u2;

  real<lower=0> tau0;
  real<lower=0> tau1;
  real<lower=0> tau2;

  real<lower=0> sigma_y;
}

model {
  // priors (weakly informative; adjust if you want)
  alpha ~ normal(0, 50);
  b1 ~ normal(0, 10);
  b2 ~ normal(0, 10);
  g1 ~ normal(0, 10);
  g2 ~ normal(0, 10);

  tau0 ~ normal(0, 20);
  tau1 ~ normal(0, 10);
  tau2 ~ normal(0, 10);

  u0 ~ normal(0, tau0);
  u1 ~ normal(0, tau1);
  u2 ~ normal(0, tau2);

  sigma_y ~ normal(0, 20);

  // likelihood
  for (n in 1:N_voice) {
    int s = subj_voice[n];
    real b1s = b1 + dot_product(g1, Z[s]');
real b2s = b2 + dot_product(g2, Z[s]');
    y[n] ~ normal(alpha + u0[s] + (b1s + u1[s]) * c1[n] + (b2s + u2[s]) * c2[n], sigma_y);
  }
}

generated quantities{
  // implied contrasts under your coding (baseline(-.5,0), pre(.5,-.5), post(0,.5))
  real delta_stress = b1 - 0.5*b2;   // E[pre]-E[baseline]
  real delta_recovery = b2 - 0.5*b1; // E[post]-E[pre]
}
'
writeLines(stan_code, con = path)
}

# Combined model: keep the EMA measurement model for theta (latent true EMA trait)
# AND include baseline Z as additional *fixed* moderators in the voice part.
write_stan_combined <- function(path) {
  stan_code <- '
data {
  int<lower=1> N_subj;

  // voice
  int<lower=1> N_voice;
  array[N_voice] int<lower=1, upper=N_subj> subj_voice;
  vector[N_voice] y;
  vector[N_voice] c1;
  vector[N_voice] c2;

  // EMA measurement model
  int<lower=1> N_ema;
  array[N_ema] int<lower=1, upper=N_subj> subj_ema;
  int<lower=1> D_ema;
  matrix[N_ema, D_ema] X; // z-scored EMA observed domain scores

  // Baseline domains (subject-level)
  int<lower=1> D_base;
  matrix[N_subj, D_base] Z; // z-scored baseline domains
}

parameters {
  // voice fixed effects
  real alpha;
  real b1;
  real b2;

  vector[D_ema] g1_ema;
  vector[D_ema] g2_ema;

  vector[D_base] g1_base;
  vector[D_base] g2_base;

  // subject random effects
  vector[N_subj] u0;
  vector[N_subj] u1;
  vector[N_subj] u2;
  real<lower=0> tau0;
  real<lower=0> tau1;
  real<lower=0> tau2;
  real<lower=0> sigma_y;

  // latent EMA trait (true score for each domain)
  matrix[N_subj, D_ema] theta;
  vector<lower=0>[D_ema] sigma_ema;
}

model {
  // priors
  alpha ~ normal(0, 50);
  b1 ~ normal(0, 10);
  b2 ~ normal(0, 10);

  g1_ema ~ normal(0, 10);
  g2_ema ~ normal(0, 10);
  g1_base ~ normal(0, 10);
  g2_base ~ normal(0, 10);

  tau0 ~ normal(0, 20);
  tau1 ~ normal(0, 10);
  tau2 ~ normal(0, 10);

  u0 ~ normal(0, tau0);
  u1 ~ normal(0, tau1);
  u2 ~ normal(0, tau2);

  sigma_y ~ normal(0, 20);

  to_vector(theta) ~ normal(0, 1);
  sigma_ema ~ normal(0, 1);

  // measurement model
  for (n in 1:N_ema) {
    int s = subj_ema[n];
    for (d in 1:D_ema) {
      X[n, d] ~ normal(theta[s, d], sigma_ema[d]);
    }
  }

  // voice model
  for (n in 1:N_voice) {
    int s = subj_voice[n];

    real b1s = b1
      + dot_product(g1_ema, theta[s]')
+ dot_product(g1_base, Z[s]');

    real b2s = b2
      + dot_product(g2_ema, theta[s]')
+ dot_product(g2_base, Z[s]');

    y[n] ~ normal(alpha + u0[s] + (b1s + u1[s]) * c1[n] + (b2s + u2[s]) * c2[n], sigma_y);
  }
}

generated quantities{
  real delta_stress = b1 - 0.5*b2;
  real delta_recovery = b2 - 0.5*b1;
}
'
writeLines(stan_code, con = path)
}

if (!file.exists(stan_file_baseline)) write_stan_baseline(stan_file_baseline)
if (!file.exists(stan_file_combined)) write_stan_combined(stan_file_combined)

# ------------------------------------------------------------------------------
# 6) Build Stan data lists for the three model variants
# ------------------------------------------------------------------------------

make_stan_data_ema <- function(y_vec) {
  list(
    N_subj = N_subj,
    N_voice = nrow(voice_stan),
    subj_voice = as.integer(voice_stan$subj),
    y = as.numeric(y_vec),
    c1 = as.numeric(voice_stan$c1_stress),
    c2 = as.numeric(voice_stan$c2_recovery),
    N_ema = nrow(ema_stan_imp),
    subj_ema = as.integer(ema_stan_imp$subj),
    D = length(pid5_ema_vars),
    X = X_ema_z
  )
}

make_stan_data_baseline <- function(y_vec) {
  stopifnot(!is.null(Z_base))
  list(
    N_subj = N_subj,
    N_voice = nrow(voice_stan),
    subj_voice = as.integer(voice_stan$subj),
    y = as.numeric(y_vec),
    c1 = as.numeric(voice_stan$c1_stress),
    c2 = as.numeric(voice_stan$c2_recovery),
    D_base = ncol(Z_base_z),
    Z = Z_base_z
  )
}

make_stan_data_combined <- function(y_vec) {
  stopifnot(!is.null(Z_base))
  list(
    N_subj = N_subj,
    N_voice = nrow(voice_stan),
    subj_voice = as.integer(voice_stan$subj),
    y = as.numeric(y_vec),
    c1 = as.numeric(voice_stan$c1_stress),
    c2 = as.numeric(voice_stan$c2_recovery),
    N_ema = nrow(ema_stan_imp),
    subj_ema = as.integer(ema_stan_imp$subj),
    D_ema = length(pid5_ema_vars),
    X = X_ema_z,
    D_base = ncol(Z_base_z),
    Z = Z_base_z
  )
}

# Outcomes
y_f0  <- voice_stan$y_f0
y_nne <- voice_stan$y_nne

stopifnot(!anyNA(y_f0), !anyNA(y_nne))

stan_f0_ema  <- make_stan_data_ema(y_f0)
stan_nne_ema <- make_stan_data_ema(y_nne)

stan_f0_base <- if (!is.null(Z_base)) make_stan_data_baseline(y_f0) else NULL
stan_nne_base <- if (!is.null(Z_base)) make_stan_data_baseline(y_nne) else NULL

stan_f0_comb <- if (!is.null(Z_base)) make_stan_data_combined(y_f0) else NULL
stan_nne_comb <- if (!is.null(Z_base)) make_stan_data_combined(y_nne) else NULL

# ------------------------------------------------------------------------------
# 7) Fit helper + LOO helper
# ------------------------------------------------------------------------------

fit_cmdstan <- function(stan_file, data_list,
                        seed = 123,
                        chains = 4, parallel_chains = 4,
                        iter_warmup = 2000, iter_sampling = 4000,
                        adapt_delta = 0.99, max_treedepth = 15) {
  
  mod <- cmdstan_model(stan_file)
  mod$sample(
    data = data_list,
    seed = seed,
    chains = chains,
    parallel_chains = parallel_chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth
  )
}

to_loo <- function(fit) {
  # cmdstanr: compute pointwise log-lik if available; otherwise use loo::loo on log_lik draws
  # Here: we rely on cmdstanr built-in method if generated quantities include log_lik.
  # If your Stan models do NOT output log_lik yet, you should add it in generated quantities.
  # For now we use the "approx_loo" fallback via loo::loo(fit$draws("lp__")) is NOT valid.
  # So: we compute LOO only if log_lik exists.
  vars <- fit$metadata()$stan_variables
  if (!"log_lik" %in% vars) {
    warning("No log_lik found in Stan output. Add log_lik in generated quantities for LOO.")
    return(NULL)
  }
  ll <- fit$draws("log_lik")
  loo::loo(ll)
}

# ------------------------------------------------------------------------------
# 8) Extract posterior summaries in a way consistent with your contrast coding
# ------------------------------------------------------------------------------

pd <- function(x) max(mean(x > 0), 1 - mean(x > 0))

summ1 <- function(x) {
  tibble(
    mean = mean(x),
    median = median(x),
    q05 = unname(quantile(x, 0.05)),
    q95 = unname(quantile(x, 0.95)),
    pd = pd(x),
    p_gt0 = mean(x > 0),
    p_lt0 = mean(x < 0)
  )
}

# implied contrasts from b1/b2 coding
compute_implied_deltas <- function(df_draws) {
  b1 <- df_draws$b1
  b2 <- df_draws$b2
  tibble(
    delta_stress = b1 - 0.5*b2,     # pre - baseline
    delta_recovery = b2 - 0.5*b1,   # post - pre
    delta_post_vs_base = 0.5*b1 + 0.5*b2
  )
}

extract_moderation_tbl_ema <- function(df_draws, pid5_vars, prefix_g1 = "g1", prefix_g2 = "g2") {
  D <- length(pid5_vars)
  get <- function(pref, d) df_draws[[sprintf("%s[%d]", pref, d)]]
  map_dfr(seq_len(D), function(d) {
    g1d <- get(prefix_g1, d)
    g2d <- get(prefix_g2, d)
    tibble(
      trait = pid5_vars[d],
      g1_mean = mean(g1d), g1_q05 = quantile(g1d, 0.05), g1_q95 = quantile(g1d, 0.95), g1_pd = pd(g1d),
      g2_mean = mean(g2d), g2_q05 = quantile(g2d, 0.05), g2_q95 = quantile(g2d, 0.95), g2_pd = pd(g2d)
    )
  })
}

# Baseline and combined have different parameter names:
extract_moderation_tbl_baseline <- function(df_draws, base_names) {
  D <- length(base_names)
  get <- function(pref, d) df_draws[[sprintf("%s[%d]", pref, d)]]
  map_dfr(seq_len(D), function(d) {
    g1d <- get("g1", d)
    g2d <- get("g2", d)
    tibble(
      trait = base_names[d],
      g1_mean = mean(g1d), g1_q05 = quantile(g1d, 0.05), g1_q95 = quantile(g1d, 0.95), g1_pd = pd(g1d),
      g2_mean = mean(g2d), g2_q05 = quantile(g2d, 0.05), g2_q95 = quantile(g2d, 0.95), g2_pd = pd(g2d)
    )
  })
}

extract_moderation_tbl_combined <- function(df_draws, pid5_vars, base_names) {
  D1 <- length(pid5_vars)
  D2 <- length(base_names)
  get <- function(pref, d) df_draws[[sprintf("%s[%d]", pref, d)]]
  
  ema_tbl <- map_dfr(seq_len(D1), function(d) {
    g1d <- get("g1_ema", d)
    g2d <- get("g2_ema", d)
    tibble(
      source = "EMA",
      trait = pid5_vars[d],
      g1_mean = mean(g1d), g1_q05 = quantile(g1d, 0.05), g1_q95 = quantile(g1d, 0.95), g1_pd = pd(g1d),
      g2_mean = mean(g2d), g2_q05 = quantile(g2d, 0.05), g2_q95 = quantile(g2d, 0.95), g2_pd = pd(g2d)
    )
  })
  
  base_tbl <- map_dfr(seq_len(D2), function(d) {
    g1d <- get("g1_base", d)
    g2d <- get("g2_base", d)
    tibble(
      source = "Baseline",
      trait = base_names[d],
      g1_mean = mean(g1d), g1_q05 = quantile(g1d, 0.05), g1_q95 = quantile(g1d, 0.95), g1_pd = pd(g1d),
      g2_mean = mean(g2d), g2_q05 = quantile(g2d, 0.05), g2_q95 = quantile(g2d, 0.95), g2_pd = pd(g2d)
    )
  })
  
  bind_rows(ema_tbl, base_tbl)
}

# ------------------------------------------------------------------------------
# 9) RUN MODELS (toggle these flags)
# ------------------------------------------------------------------------------

RUN_EMA      <- TRUE
RUN_BASELINE <- !is.null(Z_base)   # only if baseline domains exist
RUN_COMBINED <- !is.null(Z_base)

# F0
fit_f0_ema <- fit_f0_base <- fit_f0_comb <- NULL
# NNE
fit_nne_ema <- fit_nne_base <- fit_nne_comb <- NULL

if (RUN_EMA) {
  cat("\n--- FIT: F0 (EMA-only) ---\n")
  fit_f0_ema <- fit_cmdstan(stan_file_f0_ema, stan_f0_ema)
  fit_f0_ema$save_object(file = here("results", "followup", "fit_f0_ema.rds"))
  
  cat("\n--- FIT: NNE (EMA-only) ---\n")
  fit_nne_ema <- fit_cmdstan(stan_file_nne_ema, stan_nne_ema)
  fit_nne_ema$save_object(file = here("results", "followup", "fit_nne_ema.rds"))
}

if (RUN_BASELINE) {
  cat("\n--- FIT: F0 (baseline-only) ---\n")
  fit_f0_base <- fit_cmdstan(stan_file_baseline, stan_f0_base)
  fit_f0_base$save_object(file = here("results", "followup", "fit_f0_baseline.rds"))
  
  cat("\n--- FIT: NNE (baseline-only) ---\n")
  fit_nne_base <- fit_cmdstan(stan_file_baseline, stan_nne_base)
  fit_nne_base$save_object(file = here("results", "followup", "fit_nne_baseline.rds"))
}

if (RUN_COMBINED) {
  cat("\n--- FIT: F0 (EMA + baseline) ---\n")
  fit_f0_comb <- fit_cmdstan(stan_file_combined, stan_f0_comb)
  fit_f0_comb$save_object(file = here("results", "followup", "fit_f0_combined.rds"))
  
  cat("\n--- FIT: NNE (EMA + baseline) ---\n")
  fit_nne_comb <- fit_cmdstan(stan_file_combined, stan_nne_comb)
  fit_nne_comb$save_object(file = here("results", "followup", "fit_nne_combined.rds"))
}

# ------------------------------------------------------------------------------
# 10) SUMMARIZE: main implied contrasts + moderation tables
# ------------------------------------------------------------------------------

summarize_fit <- function(fit, model_label, moderator_type = c("EMA","BASELINE","COMBINED"),
                          pid5_vars = NULL, base_vars = NULL) {
  moderator_type <- match.arg(moderator_type)
  df <- as_draws_df(fit$draws())
  
  deltas <- compute_implied_deltas(df)
  delta_tbl <- bind_rows(
    summ1(deltas$delta_stress)   %>% mutate(effect = "Δstress (pre-baseline)"),
    summ1(deltas$delta_recovery) %>% mutate(effect = "Δrecovery (post-pre)"),
    summ1(deltas$delta_post_vs_base) %>% mutate(effect = "Δpost-baseline")
  ) %>%
    mutate(model = model_label) %>%
    select(model, effect, everything())
  
  mod_tbl <- NULL
  if (moderator_type == "EMA") {
    mod_tbl <- extract_moderation_tbl_ema(df, pid5_vars) %>%
      mutate(model = model_label, moderator_type = "EMA")
  } else if (moderator_type == "BASELINE") {
    mod_tbl <- extract_moderation_tbl_baseline(df, base_vars) %>%
      mutate(model = model_label, moderator_type = "Baseline")
  } else if (moderator_type == "COMBINED") {
    mod_tbl <- extract_moderation_tbl_combined(df, pid5_vars, base_vars) %>%
      mutate(model = model_label)
  }
  
  list(delta_tbl = delta_tbl, mod_tbl = mod_tbl)
}

all_delta <- list()
all_mod   <- list()

if (!is.null(fit_f0_ema)) {
  out <- summarize_fit(fit_f0_ema, "F0_EMA", "EMA", pid5_vars = pid5_ema_vars)
  all_delta <- append(all_delta, list(out$delta_tbl))
  all_mod   <- append(all_mod, list(out$mod_tbl))
}
if (!is.null(fit_nne_ema)) {
  out <- summarize_fit(fit_nne_ema, "NNE_EMA", "EMA", pid5_vars = pid5_ema_vars)
  all_delta <- append(all_delta, list(out$delta_tbl))
  all_mod   <- append(all_mod, list(out$mod_tbl))
}

if (!is.null(fit_f0_base)) {
  out <- summarize_fit(fit_f0_base, "F0_BASELINE", "BASELINE",
                       base_vars = baseline_pid5_vars)
  all_delta <- append(all_delta, list(out$delta_tbl))
  all_mod   <- append(all_mod, list(out$mod_tbl))
}
if (!is.null(fit_nne_base)) {
  out <- summarize_fit(fit_nne_base, "NNE_BASELINE", "BASELINE",
                       base_vars = baseline_pid5_vars)
  all_delta <- append(all_delta, list(out$delta_tbl))
  all_mod   <- append(all_mod, list(out$mod_tbl))
}

if (!is.null(fit_f0_comb)) {
  out <- summarize_fit(fit_f0_comb, "F0_COMBINED", "COMBINED",
                       pid5_vars = pid5_ema_vars, base_vars = baseline_pid5_vars)
  all_delta <- append(all_delta, list(out$delta_tbl))
  all_mod   <- append(all_mod, list(out$mod_tbl))
}
if (!is.null(fit_nne_comb)) {
  out <- summarize_fit(fit_nne_comb, "NNE_COMBINED", "COMBINED",
                       pid5_vars = pid5_ema_vars, base_vars = baseline_pid5_vars)
  all_delta <- append(all_delta, list(out$delta_tbl))
  all_mod   <- append(all_mod, list(out$mod_tbl))
}

delta_tbl <- bind_rows(all_delta)
mod_tbl   <- bind_rows(all_mod)

write_csv(delta_tbl, here("results", "followup", "posterior_contrasts.csv"))
write_csv(mod_tbl,   here("results", "followup", "posterior_moderation.csv"))

cat("\nSaved:\n",
    "- results/followup/posterior_contrasts.csv\n",
    "- results/followup/posterior_moderation.csv\n")

# ------------------------------------------------------------------------------
# 11) MODEL COMPARISON (LOO) — only if log_lik exists in generated quantities
# ------------------------------------------------------------------------------

compare_models <- function(...) {
  fits <- list(...)
  fits <- fits[!vapply(fits, is.null, logical(1))]
  if (length(fits) < 2) return(NULL)
  
  loos <- lapply(fits, to_loo)
  if (any(vapply(loos, is.null, logical(1)))) {
    cat("\n[INFO] LOO not computed (missing log_lik). If you want LOO, add log_lik to Stan.\n")
    return(NULL)
  }
  names(loos) <- names(fits)
  loo::loo_compare(loos)
}

loo_f0 <- compare_models(
  F0_EMA = fit_f0_ema,
  F0_BASELINE = fit_f0_base,
  F0_COMBINED = fit_f0_comb
)

loo_nne <- compare_models(
  NNE_EMA = fit_nne_ema,
  NNE_BASELINE = fit_nne_base,
  NNE_COMBINED = fit_nne_comb
)

if (!is.null(loo_f0)) {
  print(loo_f0)
  saveRDS(loo_f0, here("results", "followup", "loo_compare_f0.rds"))
}
if (!is.null(loo_nne)) {
  print(loo_nne)
  saveRDS(loo_nne, here("results", "followup", "loo_compare_nne.rds"))
}

# ------------------------------------------------------------------------------
# 12) OPTIONAL: “temporal covariation” sanity checks (EMA near recording date)
# ------------------------------------------------------------------------------

# This is not the main inferential model (because EMA timestamps may be sparse around voice days),
# but it helps you write a credible supplementary statement:
# “EMA domain scores nearer to exam-related recording days co-varied with acoustic changes.”

RUN_TEMPORAL_CHECKS <- TRUE

if (RUN_TEMPORAL_CHECKS) {
  
  # Try to find a date column in voice sheets; if not present, skip
  # (your descriptives showed a 'date' field in voice summaries)
  # If AUDIO.xlsx contains 'date', we can merge it; otherwise you can provide a map.
  #
  # Here we just demonstrate the framework:
  if (!("date" %in% names(df_voice))) {
    cat("\n[INFO] No 'date' column detected in df_voice (voice). Temporal checks skipped.\n")
  } else {
    # Detect EMA day column
    # from your import script you had 'day' derived from datetime
    day_col <- intersect(c("day", "date", "calendar_day"), names(ema))
    if (length(day_col) == 0) {
      cat("\n[INFO] No day/date column detected in EMA cleaned file. Temporal checks skipped.\n")
    } else {
      day_col <- day_col[1]
      
      ema_day <- ema %>%
        transmute(
          ID = tolower(user_id),
          day = as.Date(.data[[day_col]]),
          across(all_of(pid5_ema_vars), as.numeric)
        ) %>%
        filter(!is.na(ID), !is.na(day))
      
      voice_day <- df_voice %>%
        transmute(
          ID = tolower(ID),
          timepoint,
          day = as.Date(date),
          y_f0, y_nne
        ) %>%
        filter(!is.na(ID), !is.na(day))
      
      # For each voice day, take EMA within +/- 7 days and average (can tighten to +/-3)
      window_days <- 7
      
      ema_near_voice <- voice_day %>%
        left_join(ema_day, by = "ID") %>%
        filter(abs(as.integer(day.y - day.x)) <= window_days) %>%
        group_by(ID, timepoint, day = day.x) %>%
        summarise(
          across(all_of(pid5_ema_vars), ~ mean(.x, na.rm = TRUE)),
          y_f0 = first(y_f0),
          y_nne = first(y_nne),
          n_ema_in_window = n(),
          .groups = "drop"
        ) %>%
        filter(n_ema_in_window >= 1)
      
      # crude correlations (descriptive)
      cor_tbl <- expand_grid(
        outcome = c("y_f0", "y_nne"),
        trait = pid5_ema_vars
      ) %>%
        mutate(
          cor = map2_dbl(outcome, trait, ~ suppressWarnings(
            cor(ema_near_voice[[.x]], ema_near_voice[[.y]], use = "pairwise.complete.obs")
          )),
          n = nrow(ema_near_voice)
        )
      
      write_csv(ema_near_voice, here("results", "followup", "ema_near_voice_window.csv"))
      write_csv(cor_tbl, here("results", "followup", "temporal_covariation_correlations.csv"))
      
      cat("\nSaved temporal check outputs:\n",
          "- results/followup/ema_near_voice_window.csv\n",
          "- results/followup/temporal_covariation_correlations.csv\n")
    }
  }
}

cat("\nDONE.\n")
