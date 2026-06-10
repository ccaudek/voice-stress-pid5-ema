# ==============================================================================
# 02b_ema_manipulation_check_appraisal.R
# Manipulation check EMA — APPRAISAL della situazione presente (rinforza il NA)
# Outcome:
#   - context_threat   (atteso: piu' alto sotto stress, b1 > 0; sollievo post, b2 < 0)
#   - context_quality  (piacevolezza; atteso: piu' BASSA sotto stress, b1 < 0; b2 > 0)
#   - negative_appraisal = context_threat + reverse(context_quality)  [composito opzionale]
#
# Stesso schema di 02_ema_manipulation_check_negative_affect.R:
#   stesso match con gli ID corretti di AUDIO.xlsx, stesse medie soggetto x timepoint,
#   stessi contrasti adjacent centrati, stesso modello gerarchico bayesiano
#   (priors sulla scala osservata via y_mean/y_sd, quindi scale-adaptive).
#
# Timepoint (come nello script NA):
#   BASELINE = prompt ~1 mese prima ; PRE = giorno prima ; POST = sera del giorno dell'esame.
#   NB: la voce POST (T3) e' il giorno DOPO -> dichiara il disallineamento nel testo.
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
# 0) OPZIONI
# ----------------------------
ITEM_MIN <- 0                      # [PLACEHOLDER: verifica la scala degli item appraisal]
ITEM_MAX <- 100                    # [PLACEHOLDER: verifica la scala degli item appraisal]
ANALYSIS_LEVEL <- "subject_timepoint_means"   # oppure "prompt_level"
REQUIRE_ALL_TIMEPOINTS <- FALSE

N_CHAINS <- 4; N_WARMUP <- 2000; N_SAMPLING <- 4000
ADAPT_DELTA <- 0.95; MAX_TREEDEPTH <- 15; SEED <- 123

# [PLACEHOLDER: conferma i nomi esatti delle colonne nel file EMA]
APPRAISAL_ITEMS <- c("context_threat", "context_quality")

# ----------------------------
# 1) PATHS E DIRECTORIES
# ----------------------------
first_existing <- function(paths, label) {
  ex <- paths[file.exists(paths)]
  if (length(ex) == 0) stop("File non trovato per ", label, ":\n",
                            paste0(" - ", paths, collapse = "\n"), call. = FALSE)
  ex[[1]]
}

ema_path <- first_existing(
  c(here("data", "processed", "ema_plus_scales_cleaned.csv"),
    here("data", "cleaned", "ema_plus_scales_cleaned.csv"),
    here("data", "ema_plus_scales_cleaned.csv"),
    "ema_plus_scales_cleaned.csv"),
  "ema_plus_scales_cleaned.csv"
)
audio_path <- first_existing(
  c(here("data", "raw", "acustic_features", "datiacustici", "AUDIO.xlsx"),
    here("data", "raw", "acoustic_features", "datiacustici", "AUDIO.xlsx"),
    here("data", "raw", "AUDIO.xlsx"),
    "AUDIO.xlsx"),
  "AUDIO.xlsx"
)

out_dir <- here("results", "manipulation_check", "ema_appraisal")
for (sub in c("data", "models", "figures", "tables", "stan", "manuscript"))
  dir.create(file.path(out_dir, sub), recursive = TRUE, showWarnings = FALSE)

cat("\n=== PATHS ===\nEMA:", ema_path, "\nAUDIO:", audio_path, "\nOut:", out_dir, "\n")

# ----------------------------
# 2) AUDIO IDs + CORREZIONE (identica agli altri script)
# ----------------------------
read_audio_sheet <- function(sheet_name) {
  read_excel(audio_path, sheet = sheet_name) |>
    rename_with(stringr::str_trim) |>
    mutate(ID = stringr::str_trim(as.character(ID))) |>
    select(ID)
}
audio_ids <- bind_rows(read_audio_sheet("BASELINE"),
                       read_audio_sheet("PRE"),
                       read_audio_sheet("POST")) |>
  mutate(ID = case_when(
    ID == "am_bo_1988_08_24_166" ~ "an_bo_1988_08_24_166",
    ID == "as_li_2005_04_26_447" ~ "as_si_2005_04_26_447",
    ID == "cl_bo_1987_10_16_628" ~ "ca_bo_1987_10_16_628",
    ID == "hi_na_2005_03_08_339" ~ "gi_na_2005_03_08_339",
    ID == "ma_si_2003_10_31_940" ~ "si_ma_2003_10_31_940",
    TRUE ~ ID)) |>
  filter(!is.na(ID), ID != "") |>
  distinct(ID) |>
  arrange(ID)
cat("AUDIO: N soggetti unici =", nrow(audio_ids), "\n")

# ----------------------------
# 3) CARICA EMA E COSTRUISCI GLI OUTCOME APPRAISAL
# ----------------------------
df_ema_raw <- read_csv(ema_path, show_col_types = FALSE) |> rename_with(stringr::str_trim)

required_cols <- c("user_id", "exam_period", APPRAISAL_ITEMS)
missing_cols <- setdiff(required_cols, names(df_ema_raw))
if (length(missing_cols) > 0)
  stop("Colonne mancanti in EMA: ", paste(missing_cols, collapse = ", "),
       "\n[PLACEHOLDER: correggi i nomi in APPRAISAL_ITEMS].", call. = FALSE)

df_ema <- df_ema_raw |>
  mutate(
    user_id = stringr::str_trim(as.character(user_id)),
    exam_period = stringr::str_to_lower(stringr::str_trim(as.character(exam_period))),
    across(all_of(APPRAISAL_ITEMS), ~ suppressWarnings(as.numeric(.x))),
    timepoint = case_when(
      exam_period %in% c("baseline", "base", "bsl") ~ "baseline",
      exam_period %in% c("pre", "pre_exam", "pre-exam", "preexam") ~ "pre",
      exam_period %in% c("post", "post_exam", "post-exam", "postexam") ~ "post",
      TRUE ~ NA_character_),
    timepoint = factor(timepoint, levels = c("baseline", "pre", "post")),
    # composito: alto = situazione minacciosa + sgradevole
    context_quality_rev = ITEM_MAX + ITEM_MIN - context_quality,
    negative_appraisal  = context_threat + context_quality_rev
  )

# Controllo range
range_check <- df_ema |>
  summarise(across(all_of(APPRAISAL_ITEMS),
                   list(min = ~min(.x, na.rm = TRUE), max = ~max(.x, na.rm = TRUE),
                        n_oor = ~sum(!is.na(.x) & (.x < ITEM_MIN | .x > ITEM_MAX))),
                   .names = "{.col}_{.fn}"))
print(range_check)
if (any(range_check |> select(ends_with("n_oor")) |> unlist() > 0))
  warning("Alcuni item appraisal sono fuori dal range ITEM_MIN/ITEM_MAX.")

OUTCOMES <- c(APPRAISAL_ITEMS, "negative_appraisal")

# ----------------------------
# 4) MATCH EMA -> AUDIO IDs
# ----------------------------
df_prompt <- df_ema |>
  semi_join(audio_ids, by = c("user_id" = "ID")) |>
  filter(!is.na(timepoint))
cat("EMA x AUDIO: N prompt =", nrow(df_prompt),
    "| N soggetti =", n_distinct(df_prompt$user_id), "\n")

# ----------------------------
# 5) MODELLO STAN (identico allo script NA: scale-adaptive)
# ----------------------------
stan_file <- file.path(out_dir, "stan", "ema_adjacent_contrasts.stan")
stan_code <- '
data {
  int<lower=1> N_subj;
  int<lower=1> N_obs;
  array[N_obs] int<lower=1, upper=N_subj> subj_id;
  vector[N_obs] y;
  vector[N_obs] c1;           // adjacent contrast PRE - BASELINE
  vector[N_obs] c2;           // adjacent contrast POST - PRE
  real y_mean;
  real<lower=0> y_sd;
}
parameters {
  real alpha; real b1; real b2;
  real<lower=0> tau_subj; vector[N_subj] z_subj;
  real<lower=0> sigma_y;
}
transformed parameters { vector[N_subj] u_subj = tau_subj * z_subj; }
model {
  vector[N_obs] mu;
  alpha ~ normal(y_mean, 2 * y_sd);
  b1 ~ normal(0, y_sd); b2 ~ normal(0, y_sd);
  tau_subj ~ normal(0, y_sd); sigma_y ~ normal(0, y_sd);
  z_subj ~ std_normal();
  for (n in 1:N_obs) mu[n] = alpha + u_subj[subj_id[n]] + b1 * c1[n] + b2 * c2[n];
  y ~ normal(mu, sigma_y);
}
generated quantities {
  vector[N_obs] log_lik; vector[N_obs] y_rep;
  real mu_baseline; real mu_pre; real mu_post;
  real pre_minus_baseline; real post_minus_pre; real post_minus_baseline;
  mu_baseline = alpha + b1 * (-2.0/3.0) + b2 * (-1.0/3.0);
  mu_pre      = alpha + b1 * ( 1.0/3.0) + b2 * (-1.0/3.0);
  mu_post     = alpha + b1 * ( 1.0/3.0) + b2 * ( 2.0/3.0);
  pre_minus_baseline = mu_pre - mu_baseline;
  post_minus_pre     = mu_post - mu_pre;
  post_minus_baseline = mu_post - mu_baseline;
  for (n in 1:N_obs) {
    real mu_n = alpha + u_subj[subj_id[n]] + b1 * c1[n] + b2 * c2[n];
    log_lik[n] = normal_lpdf(y[n] | mu_n, sigma_y);
    y_rep[n]   = normal_rng(mu_n, sigma_y);
  }
}
'
writeLines(stan_code, stan_file)
mod <- cmdstan_model(stan_file)

# ----------------------------
# 6) FUNZIONE: manipulation check per un singolo outcome
# ----------------------------
run_outcome <- function(outcome) {
  cat("\n========== OUTCOME:", outcome, "==========\n")
  od <- file.path(out_dir, outcome)
  for (sub in c("figures", "tables", "models")) dir.create(file.path(od, sub), recursive = TRUE, showWarnings = FALSE)

  # 6.1 medie soggetto x timepoint (non sovra-pesa i molti prompt baseline)
  df_tp <- df_prompt |>
    filter(!is.na(.data[[outcome]])) |>
    group_by(user_id, timepoint) |>
    summarise(value = mean(.data[[outcome]], na.rm = TRUE), n_prompts = n(), .groups = "drop") |>
    mutate(
      timepoint = factor(timepoint, levels = c("baseline", "pre", "post")),
      c1_pre_vs_baseline = case_when(timepoint == "baseline" ~ -2/3, TRUE ~ 1/3),
      c2_post_vs_pre     = case_when(timepoint == "post" ~ 2/3, TRUE ~ -1/3)
    )

  if (REQUIRE_ALL_TIMEPOINTS) {
    keep <- df_tp |> count(user_id, timepoint) |>
      pivot_wider(names_from = timepoint, values_from = n, values_fill = 0) |>
      filter(baseline > 0, pre > 0, post > 0) |> pull(user_id)
    df_tp <- df_tp |> filter(user_id %in% keep)
  }

  # 6.2 descrittive (per VEDERE il pattern, dato il sorpresone sul NA)
  desc <- df_tp |> group_by(timepoint) |>
    summarise(n = n(), mean = mean(value), sd = sd(value),
              median = median(value), iqr = IQR(value), .groups = "drop")
  cat("Descrittive per timepoint:\n"); print(as.data.frame(desc))
  write_csv(desc, file.path(od, "tables", "descriptives.csv"))

  # 6.3 dati per Stan
  subj_ids <- sort(unique(as.character(df_tp$user_id)))
  id_map <- tibble(user_id = subj_ids, subj = seq_along(subj_ids))
  df_stan <- df_tp |> inner_join(id_map, by = "user_id") |> arrange(subj, timepoint)
  y_sd <- sd(df_stan$value); if (!is.finite(y_sd) || y_sd <= 0) y_sd <- 1
  stan_data <- list(
    N_subj = nrow(id_map), N_obs = nrow(df_stan),
    subj_id = as.integer(df_stan$subj), y = as.numeric(df_stan$value),
    c1 = as.numeric(df_stan$c1_pre_vs_baseline), c2 = as.numeric(df_stan$c2_post_vs_pre),
    y_mean = mean(df_stan$value), y_sd = y_sd
  )
  cat("Stan data: N_subj =", stan_data$N_subj, "| N_obs =", stan_data$N_obs, "\n")

  # 6.4 fit
  fit <- mod$sample(data = stan_data, chains = N_CHAINS, parallel_chains = N_CHAINS,
                    iter_warmup = N_WARMUP, iter_sampling = N_SAMPLING,
                    adapt_delta = ADAPT_DELTA, max_treedepth = MAX_TREEDEPTH, seed = SEED)
  fit$cmdstan_diagnose()
  fit$save_object(file.path(od, "models", paste0("fit_", outcome, ".rds")))

  post <- fit$draws(format = "df")

  # 6.5 contrasti
  cs <- tibble(
    outcome = outcome,
    pre_minus_baseline_median = median(post$pre_minus_baseline),
    pre_minus_baseline_lo = quantile(post$pre_minus_baseline, .025),
    pre_minus_baseline_hi = quantile(post$pre_minus_baseline, .975),
    pre_minus_baseline_pgt0 = mean(post$pre_minus_baseline > 0),
    post_minus_pre_median = median(post$post_minus_pre),
    post_minus_pre_lo = quantile(post$post_minus_pre, .025),
    post_minus_pre_hi = quantile(post$post_minus_pre, .975),
    post_minus_pre_plt0 = mean(post$post_minus_pre < 0),
    post_minus_baseline_median = median(post$post_minus_baseline),
    post_minus_baseline_lo = quantile(post$post_minus_baseline, .025),
    post_minus_baseline_hi = quantile(post$post_minus_baseline, .975)
  )
  cat("Contrasti:\n"); print(as.data.frame(cs), digits = 4)
  write_csv(cs, file.path(od, "tables", "contrast_summary.csv"))

  # 6.6 marginal means model-implied
  marg <- tibble(
    outcome = outcome,
    timepoint = factor(c("baseline", "pre", "post"), levels = c("baseline", "pre", "post")),
    median = c(median(post$mu_baseline), median(post$mu_pre), median(post$mu_post)),
    lower  = c(quantile(post$mu_baseline, .025), quantile(post$mu_pre, .025), quantile(post$mu_post, .025)),
    upper  = c(quantile(post$mu_baseline, .975), quantile(post$mu_pre, .975), quantile(post$mu_post, .975))
  )
  write_csv(marg, file.path(od, "tables", "marginal_means.csv"))

  # 6.7 figure
  ggsave(file.path(od, "figures", "posterior_contrasts.png"),
         mcmc_areas(fit$draws(c("pre_minus_baseline", "post_minus_pre")), prob = .95, prob_outer = .99) +
           geom_vline(xintercept = 0, linetype = "dashed") +
           labs(title = paste0("Manipulation check: ", outcome),
                subtitle = "pre-baseline (anticipatory) and post-pre (recovery) contrasts",
                x = "Difference") + theme_minimal(),
         width = 8, height = 5, dpi = 300)

  ggsave(file.path(od, "figures", "marginal_means.png"),
         ggplot(marg, aes(timepoint, median, group = 1)) +
           geom_line() + geom_point(size = 3) +
           geom_errorbar(aes(ymin = lower, ymax = upper), width = .15) +
           labs(title = paste0("Model-implied marginal means: ", outcome), x = NULL, y = outcome) +
           theme_minimal(),
         width = 6, height = 5, dpi = 300)

  y_rep <- fit$draws("y_rep", format = "matrix")
  ggsave(file.path(od, "figures", "ppc.png"),
         ppc_dens_overlay(y = stan_data$y, yrep = y_rep[seq_len(min(100, nrow(y_rep))), , drop = FALSE]) +
           labs(title = paste0("PPC: ", outcome)) + theme_minimal(),
         width = 8, height = 5, dpi = 300)

  list(outcome = outcome, descriptives = desc, contrasts = cs,
       marginal = marg, fit = fit, id_map = id_map, stan_data = stan_data)
}

# ----------------------------
# 7) ESEGUI PER I DUE ITEM (+ composito)
# ----------------------------
results <- setNames(lapply(OUTCOMES, run_outcome), OUTCOMES)

# ----------------------------
# 8) TABELLE E FIGURA COMBINATE
# ----------------------------
contrasts_all <- bind_rows(lapply(results, `[[`, "contrasts"))
marg_all      <- bind_rows(lapply(results, `[[`, "marginal"))
write_csv(contrasts_all, file.path(out_dir, "tables", "appraisal_contrasts_all.csv"))
write_csv(marg_all,      file.path(out_dir, "tables", "appraisal_marginal_all.csv"))

cat("\n=== CONTRASTI (tutti gli outcome) ===\n")
print(as.data.frame(contrasts_all), digits = 4)

p_combined <- ggplot(marg_all, aes(timepoint, median, group = 1)) +
  geom_line() + geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .15) +
  facet_wrap(~outcome, scales = "free_y") +
  labs(title = "EMA appraisal around the exam (model-implied marginal means)",
       subtitle = "Atteso se è stress+sollievo: threat su pre / giù post; quality giù pre / su post",
       x = NULL, y = NULL) +
  theme_minimal()
ggsave(file.path(out_dir, "figures", "appraisal_marginal_means_combined.png"),
       p_combined, width = 11, height = 4.5, dpi = 300)

# ----------------------------
# 9) TESTO PER MANOSCRITTO (template)
# ----------------------------
fmt <- function(x) formatC(x, format = "f", digits = 2)
ct <- results[["context_threat"]]$contrasts
cq <- results[["context_quality"]]$contrasts
manuscript_text <- paste0(
  "Two single items assessed momentary appraisal of the current situation (perceived threat and pleasantness), ",
  "analyzed with the same hierarchical Bayesian model and adjacent contrasts as negative affect. ",
  "Perceived threat was higher before the exam than at baseline (PRE - BASELINE = ",
  fmt(ct$pre_minus_baseline_median), " [", fmt(ct$pre_minus_baseline_lo), ", ", fmt(ct$pre_minus_baseline_hi),
  "], Pr(>0) = ", fmt(ct$pre_minus_baseline_pgt0), ") and decreased afterwards (POST - PRE = ",
  fmt(ct$post_minus_pre_median), " [", fmt(ct$post_minus_pre_lo), ", ", fmt(ct$post_minus_pre_hi),
  "]). Situational pleasantness showed the complementary pattern (PRE - BASELINE = ",
  fmt(cq$pre_minus_baseline_median), " [", fmt(cq$pre_minus_baseline_lo), ", ", fmt(cq$pre_minus_baseline_hi),
  "]; POST - PRE = ", fmt(cq$post_minus_pre_median), " [", fmt(cq$post_minus_pre_lo), ", ", fmt(cq$post_minus_pre_hi),
  "]). [NB: report reliability/caution for 2 single items; state timing of POST prompt vs voice T3.]\n"
)
writeLines(manuscript_text, file.path(out_dir, "manuscript", "appraisal_text.txt"))
cat("\n", manuscript_text, "\n")

# ----------------------------
# 10) SAVE BUNDLE
# ----------------------------
saveRDS(list(results = results, contrasts_all = contrasts_all, marg_all = marg_all,
             audio_ids = audio_ids, item_min = ITEM_MIN, item_max = ITEM_MAX,
             appraisal_items = APPRAISAL_ITEMS),
        file.path(out_dir, "models", "appraisal_bundle.rds"))

cat("\n=== DONE ===\nOutput in:", out_dir, "\n")
# eof ---
