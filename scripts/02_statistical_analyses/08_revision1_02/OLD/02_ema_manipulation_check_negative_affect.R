# ==============================================================================
# 02_ema_manipulation_check_negative_affect.R
# Manipulation check EMA: impatto dell'esame sull'umore istantaneo
# Outcome: negative_affect = angry + sad + reverse(happy) + reverse(satisfied)
# Campione: solo user_id presenti negli ID corretti di AUDIO.xlsx
# Contrasti: b1 = PRE - BASELINE; b2 = POST - PRE
#
# Nota metodologica:
# - PRE = prompt del giorno prima dell'esame.
# - POST = prompt della sera del giorno dell'esame, dopo l'esame mattutino.
# - BASELINE = prompt raccolti circa un mese prima.
# - Per default il modello usa le medie soggetto x timepoint, per evitare che i
#   molti prompt baseline pesino piu' dei prompt pre/post exam-linked.
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

# ----------------------------
# 0) OPZIONI
# ----------------------------
set.seed(123)
options(mc.cores = parallel::detectCores())

# Scala degli item EMA. Nei file attuali gli item vanno da 0 a 100.
ITEM_MIN <- 0
ITEM_MAX <- 100

# Default raccomandato: una media per soggetto per timepoint.
# Alternative: "prompt_level" per usare tutti i singoli prompt EMA.
ANALYSIS_LEVEL <- "subject_timepoint_means"

# Se TRUE, conserva solo soggetti con baseline, pre e post disponibili.
# Se FALSE, il modello gerarchico usa tutti i dati disponibili.
REQUIRE_ALL_TIMEPOINTS <- FALSE

# Sampling MCMC
N_CHAINS <- 4
N_WARMUP <- 2000
N_SAMPLING <- 4000
ADAPT_DELTA <- 0.95
MAX_TREEDEPTH <- 15
SEED <- 123

# ----------------------------
# 1) PATHS E DIRECTORIES
# ----------------------------
first_existing <- function(paths, label) {
  existing <- paths[file.exists(paths)]
  if (length(existing) == 0) {
    stop(
      "File non trovato per ", label, ". Percorsi cercati:\n",
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

out_dir <- here("results", "manipulation_check", "ema_negative_affect")
dir.create(file.path(out_dir, "data"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "models"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "stan"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "manuscript"), recursive = TRUE, showWarnings = FALSE)

cat("\n=== PATHS ===\n")
cat("EMA file:   ", ema_path, "\n")
cat("AUDIO file: ", audio_path, "\n")
cat("Output dir: ", out_dir, "\n")

# ----------------------------
# 2) CARICA AUDIO.xlsx E CORREGGI ID
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
    # Correzione ID: identica agli altri script, per allineare i soggetti
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

cat("AUDIO: N righe ID =", nrow(df_audio_ids), "| N soggetti unici =", nrow(audio_ids), "\n")

# ----------------------------
# 3) CARICA EMA E COSTRUISCI negative_affect
# ----------------------------
cat("\n=== LOADING EMA DATA ===\n")

df_ema_raw <- read_csv(ema_path, show_col_types = FALSE) |>
  rename_with(stringr::str_trim)

required_cols <- c("user_id", "exam_period", "happy", "sad", "satisfied", "angry")
missing_cols <- setdiff(required_cols, names(df_ema_raw))
if (length(missing_cols) > 0) {
  stop("Colonne mancanti in EMA: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

has_existing_negative_affect <- "negative_affect" %in% names(df_ema_raw)
if (has_existing_negative_affect) {
  df_ema_raw <- df_ema_raw |>
    rename(negative_affect_existing = negative_affect)
}

mood_items <- c("happy", "sad", "satisfied", "angry")

df_ema <- df_ema_raw |>
  mutate(
    user_id = stringr::str_trim(as.character(user_id)),
    exam_period = stringr::str_to_lower(stringr::str_trim(as.character(exam_period))),
    across(all_of(mood_items), ~ suppressWarnings(as.numeric(.x))),
    timepoint = case_when(
      exam_period %in% c("baseline", "base", "bsl") ~ "baseline",
      exam_period %in% c("pre", "pre_exam", "pre-exam", "preexam") ~ "pre",
      exam_period %in% c("post", "post_exam", "post-exam", "postexam") ~ "post",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    timepoint = factor(timepoint, levels = c("baseline", "pre", "post")),
    happy_rev = ITEM_MAX + ITEM_MIN - happy,
    satisfied_rev = ITEM_MAX + ITEM_MIN - satisfied,
    negative_affect = angry + sad + happy_rev + satisfied_rev
  )

# Controllo range item
range_check <- df_ema |>
  summarise(
    across(
      all_of(mood_items),
      list(
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        n_out_of_range = ~ sum(!is.na(.x) & (.x < ITEM_MIN | .x > ITEM_MAX))
      ),
      .names = "{.col}_{.fn}"
    )
  )
print(range_check)

if (any(range_check |> select(ends_with("n_out_of_range")) |> unlist() > 0)) {
  warning("Alcuni item EMA sono fuori dal range definito da ITEM_MIN/ITEM_MAX.")
}

# Se esisteva gia' una colonna negative_affect, controlla che coincida.
if (has_existing_negative_affect) {
  df_ema <- df_ema |>
    mutate(
      negative_affect_existing = suppressWarnings(as.numeric(negative_affect_existing)),
      negative_affect_diff = negative_affect_existing - negative_affect
    )

  max_abs_diff <- max(abs(df_ema$negative_affect_diff), na.rm = TRUE)
  if (!is.finite(max_abs_diff)) max_abs_diff <- NA_real_

  cat("\nControllo negative_affect esistente:\n")
  cat("Max |existing - recalculated| =", max_abs_diff, "\n")

  if (isTRUE(max_abs_diff > 1e-6)) {
    warning(
      "La colonna negative_affect esistente non coincide con angry + sad + reverse(happy) + reverse(satisfied). ",
      "Nel modello uso la versione ricalcolata."
    )
  }
}

# ----------------------------
# 4) SELEZIONA SOLO EMA CON user_id PRESENTE IN AUDIO.xlsx
# ----------------------------
cat("\n=== MATCHING EMA TO AUDIO IDs ===\n")

df_mood_prompt <- df_ema |>
  semi_join(audio_ids, by = c("user_id" = "ID")) |>
  filter(!is.na(timepoint)) |>
  filter(if_all(all_of(mood_items), ~ !is.na(.x))) |>
  mutate(
    user_id = factor(user_id),
    timepoint = factor(timepoint, levels = c("baseline", "pre", "post")),
    # Contrasti adjacent centrati.
    # Con questa codifica:
    #   b1 = media PRE - media BASELINE
    #   b2 = media POST - media PRE
    c1_pre_vs_baseline = case_when(
      timepoint == "baseline" ~ -2 / 3,
      timepoint == "pre" ~ 1 / 3,
      timepoint == "post" ~ 1 / 3
    ),
    c2_post_vs_pre = case_when(
      timepoint == "baseline" ~ -1 / 3,
      timepoint == "pre" ~ -1 / 3,
      timepoint == "post" ~ 2 / 3
    )
  )

cat("EMA totale: N obs =", nrow(df_ema), "| N soggetti =", n_distinct(df_ema$user_id), "\n")
cat("EMA x AUDIO: N obs =", nrow(df_mood_prompt), "| N soggetti =", n_distinct(df_mood_prompt$user_id), "\n")

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
    n_distinct(df_mood_prompt$user_id),
    nrow(anti_join(audio_ids, df_ema |> distinct(user_id), by = c("ID" = "user_id"))),
    nrow(anti_join(df_ema |> distinct(user_id), audio_ids, by = c("user_id" = "ID"))),
    nrow(df_mood_prompt)
  )
)
print(match_report)

write_csv(match_report, file.path(out_dir, "tables", "match_report.csv"))
write_csv(
  anti_join(audio_ids, df_ema |> distinct(user_id), by = c("ID" = "user_id")),
  file.path(out_dir, "tables", "audio_ids_not_in_ema.csv")
)
write_csv(
  anti_join(df_ema |> distinct(user_id), audio_ids, by = c("user_id" = "ID")),
  file.path(out_dir, "tables", "ema_ids_not_in_audio.csv")
)
write_csv(df_mood_prompt, file.path(out_dir, "data", "ema_audio_matched_prompt_level.csv"))

# ----------------------------
# 5) AGGREGAZIONE SOGGETTO x TIMEPOINT E DESCRITTIVE
# ----------------------------
cat("\n=== DESCRIPTIVE STATISTICS ===\n")

prompt_desc <- df_mood_prompt |>
  group_by(timepoint) |>
  summarise(
    n_obs = n(),
    n_subj = n_distinct(user_id),
    negative_affect_mean = mean(negative_affect, na.rm = TRUE),
    negative_affect_sd = sd(negative_affect, na.rm = TRUE),
    negative_affect_median = median(negative_affect, na.rm = TRUE),
    negative_affect_iqr = IQR(negative_affect, na.rm = TRUE),
    .groups = "drop"
  )

print(prompt_desc)
write_csv(prompt_desc, file.path(out_dir, "tables", "prompt_level_descriptives.csv"))

# Media soggetto x timepoint: raccomandata per un confronto coerente con AUDIO.xlsx
# e per non sovra-pesare il periodo baseline, che contiene molti piu' prompt.
df_mood_subject_tp <- df_mood_prompt |>
  group_by(user_id, timepoint) |>
  summarise(
    negative_affect = mean(negative_affect, na.rm = TRUE),
    n_prompts = n(),
    happy = mean(happy, na.rm = TRUE),
    sad = mean(sad, na.rm = TRUE),
    satisfied = mean(satisfied, na.rm = TRUE),
    angry = mean(angry, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    timepoint = factor(timepoint, levels = c("baseline", "pre", "post")),
    c1_pre_vs_baseline = case_when(
      timepoint == "baseline" ~ -2 / 3,
      timepoint == "pre" ~ 1 / 3,
      timepoint == "post" ~ 1 / 3
    ),
    c2_post_vs_pre = case_when(
      timepoint == "baseline" ~ -1 / 3,
      timepoint == "pre" ~ -1 / 3,
      timepoint == "post" ~ 2 / 3
    )
  )

availability <- df_mood_subject_tp |>
  count(user_id, timepoint) |>
  tidyr::pivot_wider(
    names_from = timepoint,
    values_from = n,
    values_fill = 0
  ) |>
  mutate(
    has_baseline = baseline > 0,
    has_pre = pre > 0,
    has_post = post > 0,
    complete_all_3 = has_baseline & has_pre & has_post
  )

cat("Soggetti con tutti e 3 i timepoint =", sum(availability$complete_all_3), "\n")
write_csv(availability, file.path(out_dir, "tables", "timepoint_availability.csv"))

if (REQUIRE_ALL_TIMEPOINTS) {
  complete_ids <- availability |>
    filter(complete_all_3) |>
    pull(user_id)
  df_mood_subject_tp <- df_mood_subject_tp |>
    filter(user_id %in% complete_ids)
  df_mood_prompt <- df_mood_prompt |>
    filter(user_id %in% complete_ids)
}

subject_tp_desc <- df_mood_subject_tp |>
  group_by(timepoint) |>
  summarise(
    n_obs = n(),
    n_subj = n_distinct(user_id),
    negative_affect_mean = mean(negative_affect, na.rm = TRUE),
    negative_affect_sd = sd(negative_affect, na.rm = TRUE),
    negative_affect_median = median(negative_affect, na.rm = TRUE),
    negative_affect_iqr = IQR(negative_affect, na.rm = TRUE),
    prompts_mean = mean(n_prompts, na.rm = TRUE),
    prompts_min = min(n_prompts, na.rm = TRUE),
    prompts_max = max(n_prompts, na.rm = TRUE),
    .groups = "drop"
  )

print(subject_tp_desc)
write_csv(subject_tp_desc, file.path(out_dir, "tables", "subject_timepoint_descriptives.csv"))
write_csv(df_mood_subject_tp, file.path(out_dir, "data", "ema_audio_matched_subject_timepoint.csv"))

# Differenze grezze entro soggetto, quando disponibili.
wide_subject <- df_mood_subject_tp |>
  select(user_id, timepoint, negative_affect) |>
  tidyr::pivot_wider(names_from = timepoint, values_from = negative_affect)

raw_differences <- wide_subject |>
  summarise(
    n_pre_baseline = sum(!is.na(pre) & !is.na(baseline)),
    pre_minus_baseline_mean = mean(pre - baseline, na.rm = TRUE),
    pre_minus_baseline_sd = sd(pre - baseline, na.rm = TRUE),
    n_post_pre = sum(!is.na(post) & !is.na(pre)),
    post_minus_pre_mean = mean(post - pre, na.rm = TRUE),
    post_minus_pre_sd = sd(post - pre, na.rm = TRUE),
    n_post_baseline = sum(!is.na(post) & !is.na(baseline)),
    post_minus_baseline_mean = mean(post - baseline, na.rm = TRUE),
    post_minus_baseline_sd = sd(post - baseline, na.rm = TRUE)
  )

print(raw_differences)
write_csv(raw_differences, file.path(out_dir, "tables", "raw_within_subject_differences.csv"))

# ----------------------------
# 6) PREPARA DATI PER STAN
# ----------------------------
cat("\n=== PREPARING STAN DATA ===\n")

if (ANALYSIS_LEVEL == "subject_timepoint_means") {
  df_analysis <- df_mood_subject_tp
} else if (ANALYSIS_LEVEL == "prompt_level") {
  df_analysis <- df_mood_prompt
} else {
  stop("ANALYSIS_LEVEL deve essere 'subject_timepoint_means' oppure 'prompt_level'.")
}

df_analysis <- df_analysis |>
  filter(!is.na(negative_affect)) |>
  mutate(user_id = factor(user_id))

subj_ids <- sort(unique(as.character(df_analysis$user_id)))
id_map <- tibble(user_id = subj_ids, subj = seq_along(subj_ids))

df_stan <- df_analysis |>
  inner_join(id_map, by = "user_id") |>
  arrange(subj, timepoint)

N_subj <- nrow(id_map)
N_obs <- nrow(df_stan)
y_mean <- mean(df_stan$negative_affect)
y_sd <- sd(df_stan$negative_affect)
if (!is.finite(y_sd) || y_sd <= 0) y_sd <- 1

stan_data <- list(
  N_subj = N_subj,
  N_obs = N_obs,
  subj_id = as.integer(df_stan$subj),
  y = as.numeric(df_stan$negative_affect),
  c1 = as.numeric(df_stan$c1_pre_vs_baseline),
  c2 = as.numeric(df_stan$c2_post_vs_pre),
  y_mean = as.numeric(y_mean),
  y_sd = as.numeric(y_sd)
)

cat("Analysis level:", ANALYSIS_LEVEL, "\n")
cat("Stan data: N_subj =", N_subj, "| N_obs =", N_obs, "\n")

write_csv(df_stan, file.path(out_dir, "data", "stan_analysis_dataset.csv"))
write_csv(id_map, file.path(out_dir, "data", "stan_id_map.csv"))

# ----------------------------
# 7) STAN MODEL
# ----------------------------
cat("\n=== WRITING STAN MODEL ===\n")

stan_file <- file.path(out_dir, "stan", "ema_negative_affect_adjacent_contrasts.stan")

stan_code <- '
data {
  int<lower=1> N_subj;
  int<lower=1> N_obs;
  array[N_obs] int<lower=1, upper=N_subj> subj_id;
  vector[N_obs] y;
  vector[N_obs] c1;           // centered adjacent contrast: PRE - BASELINE
  vector[N_obs] c2;           // centered adjacent contrast: POST - PRE
  real y_mean;
  real<lower=0> y_sd;
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

  // Weakly informative priors on the observed negative_affect scale.
  alpha ~ normal(y_mean, 2 * y_sd);
  b1 ~ normal(0, y_sd);
  b2 ~ normal(0, y_sd);
  tau_subj ~ normal(0, y_sd);
  sigma_y ~ normal(0, y_sd);
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

  // Population-level marginal means, excluding subject-specific deviations.
  mu_baseline = alpha + b1 * (-2.0 / 3.0) + b2 * (-1.0 / 3.0);
  mu_pre      = alpha + b1 * ( 1.0 / 3.0) + b2 * (-1.0 / 3.0);
  mu_post     = alpha + b1 * ( 1.0 / 3.0) + b2 * ( 2.0 / 3.0);

  pre_minus_baseline = mu_pre - mu_baseline;    // equals b1
  post_minus_pre = mu_post - mu_pre;            // equals b2
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

# ----------------------------
# 8) FIT MODEL
# ----------------------------
cat("\n=== FITTING HIERARCHICAL BAYESIAN MODEL ===\n")

mod_mood <- cmdstan_model(stan_file)

fit_mood <- mod_mood$sample(
  data = stan_data,
  chains = N_CHAINS,
  parallel_chains = N_CHAINS,
  iter_warmup = N_WARMUP,
  iter_sampling = N_SAMPLING,
  adapt_delta = ADAPT_DELTA,
  max_treedepth = MAX_TREEDEPTH,
  seed = SEED
)

fit_mood$cmdstan_diagnose()
fit_mood$save_object(file = file.path(out_dir, "models", "fit_ema_negative_affect.rds"))

summary_mood <- fit_mood$summary(
  c(
    "alpha", "b1", "b2", "tau_subj", "sigma_y",
    "mu_baseline", "mu_pre", "mu_post",
    "pre_minus_baseline", "post_minus_pre", "post_minus_baseline"
  )
)
print(data.frame(summary_mood))
write_csv(summary_mood, file.path(out_dir, "tables", "cmdstan_summary.csv"))

post_mood <- fit_mood$draws(format = "df")
write_csv(post_mood, file.path(out_dir, "models", "posterior_samples.csv"))

# ----------------------------
# 9) LOO E DIAGNOSTICA PREDITTIVA
# ----------------------------
cat("\n=== LOO CROSS-VALIDATION ===\n")

loo_mood <- loo::loo(fit_mood$draws("log_lik", format = "matrix"))
print(loo_mood)
saveRDS(loo_mood, file.path(out_dir, "models", "loo_ema_negative_affect.rds"))

# ----------------------------
# 10) POSTERIOR SUMMARIES DEI CONTRASTI
# ----------------------------
cat("\n=== POSTERIOR CONTRAST SUMMARY ===\n")

contrast_summary <- post_mood |>
  summarise(
    # b1: manipulation check principale. Valori positivi = piu' NA pre-esame.
    pre_minus_baseline_median = median(pre_minus_baseline),
    pre_minus_baseline_mad = mad(pre_minus_baseline),
    pre_minus_baseline_ci_lower = quantile(pre_minus_baseline, 0.025),
    pre_minus_baseline_ci_upper = quantile(pre_minus_baseline, 0.975),
    pre_minus_baseline_prob_gt_0 = mean(pre_minus_baseline > 0),

    # b2: recupero/normalizzazione. Valori negativi = meno NA post rispetto a pre.
    post_minus_pre_median = median(post_minus_pre),
    post_minus_pre_mad = mad(post_minus_pre),
    post_minus_pre_ci_lower = quantile(post_minus_pre, 0.025),
    post_minus_pre_ci_upper = quantile(post_minus_pre, 0.975),
    post_minus_pre_prob_lt_0 = mean(post_minus_pre < 0),
    post_minus_pre_prob_gt_0 = mean(post_minus_pre > 0),

    post_minus_baseline_median = median(post_minus_baseline),
    post_minus_baseline_mad = mad(post_minus_baseline),
    post_minus_baseline_ci_lower = quantile(post_minus_baseline, 0.025),
    post_minus_baseline_ci_upper = quantile(post_minus_baseline, 0.975),
    post_minus_baseline_prob_gt_0 = mean(post_minus_baseline > 0)
  )

print(data.frame(contrast_summary))
write_csv(contrast_summary, file.path(out_dir, "tables", "contrast_summary.csv"))

marginal_summary <- post_mood |>
  summarise(
    baseline_median = median(mu_baseline),
    baseline_lower = quantile(mu_baseline, 0.025),
    baseline_upper = quantile(mu_baseline, 0.975),
    pre_median = median(mu_pre),
    pre_lower = quantile(mu_pre, 0.025),
    pre_upper = quantile(mu_pre, 0.975),
    post_median = median(mu_post),
    post_lower = quantile(mu_post, 0.025),
    post_upper = quantile(mu_post, 0.975)
  )
write_csv(marginal_summary, file.path(out_dir, "tables", "marginal_means_summary.csv"))

# ----------------------------
# 11) FIGURE
# ----------------------------
cat("\n=== CREATING FIGURES ===\n")

# 11.1 Descrittive: distribuzione soggetto x timepoint
median_qi <- function(x) {
  data.frame(
    y = median(x, na.rm = TRUE),
    ymin = quantile(x, 0.025, na.rm = TRUE),
    ymax = quantile(x, 0.975, na.rm = TRUE)
  )
}

p_subject <- ggplot(df_mood_subject_tp, aes(x = timepoint, y = negative_affect)) +
  geom_point(alpha = 0.30, position = position_jitter(width = 0.08, height = 0)) +
  stat_summary(fun = median, geom = "point", size = 3) +
  stat_summary(fun.data = median_qi, geom = "errorbar", width = 0.15) +
  labs(
    title = "EMA negative affect around the exam",
    subtitle = "Subject x timepoint means; positive items reversed",
    x = "Timepoint",
    y = "Negative affect = angry + sad + reverse(happy) + reverse(satisfied)"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(out_dir, "figures", "negative_affect_subject_timepoint.png"),
  plot = p_subject,
  width = 7,
  height = 5,
  dpi = 300
)

# 11.2 Posterior dei due contrasti
contrast_draws <- fit_mood$draws(variables = c("pre_minus_baseline", "post_minus_pre"))

p_post <- mcmc_areas(
  contrast_draws,
  prob = 0.95,
  prob_outer = 0.99
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Manipulation check: posterior contrasts",
    subtitle = "pre_minus_baseline > 0 indicates higher negative affect before the exam; post_minus_pre < 0 indicates recovery",
    x = "Difference in negative affect"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(out_dir, "figures", "posterior_contrasts.png"),
  plot = p_post,
  width = 8,
  height = 5,
  dpi = 300
)

# 11.3 Posterior predictive check
y_rep <- fit_mood$draws("y_rep", format = "matrix")
max_ppc_draws <- min(100, nrow(y_rep))

p_ppc <- ppc_dens_overlay(y = stan_data$y, yrep = y_rep[seq_len(max_ppc_draws), , drop = FALSE]) +
  labs(title = "EMA negative affect: posterior predictive check") +
  theme_minimal()

ggsave(
  filename = file.path(out_dir, "figures", "ppc_negative_affect.png"),
  plot = p_ppc,
  width = 8,
  height = 5,
  dpi = 300
)

# 11.4 Model-implied marginal means
marginal_long <- tibble(
  timepoint = factor(c("baseline", "pre", "post"), levels = c("baseline", "pre", "post")),
  median = c(median(post_mood$mu_baseline), median(post_mood$mu_pre), median(post_mood$mu_post)),
  lower = c(quantile(post_mood$mu_baseline, 0.025), quantile(post_mood$mu_pre, 0.025), quantile(post_mood$mu_post, 0.025)),
  upper = c(quantile(post_mood$mu_baseline, 0.975), quantile(post_mood$mu_pre, 0.975), quantile(post_mood$mu_post, 0.975))
)
write_csv(marginal_long, file.path(out_dir, "tables", "marginal_means_long.csv"))

p_marginal <- ggplot(marginal_long, aes(x = timepoint, y = median, group = 1)) +
  geom_line() +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  labs(
    title = "Model-implied marginal means",
    x = "Timepoint",
    y = "Negative affect"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(out_dir, "figures", "model_implied_marginal_means.png"),
  plot = p_marginal,
  width = 6,
  height = 5,
  dpi = 300
)

# ----------------------------
# 12) TESTO PER MANOSCRITTO / RISPOSTA REVIEWERS
# ----------------------------
cat("\n=== WRITING MANUSCRIPT TEMPLATE ===\n")

cs <- contrast_summary
ms <- marginal_long

manuscript_text <- paste0(
  "Manipulation check. We tested whether the exam-linked EMA prompts captured a change in instantaneous affect around the exam. ",
  "Negative affect was computed at each prompt as angry + sad + reverse(happy) + reverse(satisfied), with happy and satisfied reverse-coded on the original ",
  ITEM_MIN, "-", ITEM_MAX, " scale. Analyses were restricted to participants whose corrected AUDIO.xlsx ID matched an EMA user_id. ",
  "A hierarchical Bayesian model with participant-level random intercepts estimated adjacent contrasts for PRE vs BASELINE and POST vs PRE. ",
  "For PRE vs BASELINE, the posterior median difference was ",
  round(cs$pre_minus_baseline_median, 2),
  " [95% CrI ", round(cs$pre_minus_baseline_ci_lower, 2), ", ", round(cs$pre_minus_baseline_ci_upper, 2),
  "], Pr(diff > 0) = ", round(cs$pre_minus_baseline_prob_gt_0, 3), ". ",
  "For POST vs PRE, the posterior median difference was ",
  round(cs$post_minus_pre_median, 2),
  " [95% CrI ", round(cs$post_minus_pre_ci_lower, 2), ", ", round(cs$post_minus_pre_ci_upper, 2),
  "], Pr(diff < 0) = ", round(cs$post_minus_pre_prob_lt_0, 3), ".\n"
)

writeLines(manuscript_text, file.path(out_dir, "manuscript", "manipulation_check_text.txt"))
cat(manuscript_text, "\n")

# ----------------------------
# 13) SAVE BUNDLE
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
    prompt_desc = prompt_desc,
    subject_tp_desc = subject_tp_desc,
    raw_differences = raw_differences,
    availability = availability,
    df_mood_prompt = df_mood_prompt,
    df_mood_subject_tp = df_mood_subject_tp,
    df_stan = df_stan,
    id_map = id_map,
    stan_data = stan_data,
    fit_mood = fit_mood,
    loo_mood = loo_mood,
    post_mood = post_mood,
    contrast_summary = contrast_summary,
    marginal_summary = marginal_summary
  ),
  file = file.path(out_dir, "models", "analysis_bundle.rds")
)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Saved outputs in:", out_dir, "\n")
cat("Key files:\n")
cat(" - data/ema_audio_matched_prompt_level.csv\n")
cat(" - data/ema_audio_matched_subject_timepoint.csv\n")
cat(" - tables/contrast_summary.csv\n")
cat(" - figures/posterior_contrasts.png\n")
cat(" - manuscript/manipulation_check_text.txt\n")
cat(" - models/fit_ema_negative_affect.rds\n")

# eof ---
