# ==============================================================================
# 01_main_effects_analysis_robust_student_t_corr.R
# Analisi robusta degli effetti principali dello stress da esame su caratteristiche vocali
# Outcome: F0 mean, NNE (aggregati su vocali /a i u/)
# Contrasti: c1_stress (PRE vs BASELINE), c2_recovery (POST vs PRE)
# Modello robusto: Student-t likelihood + random effects correlati
# Salvataggi separati con suffisso: robust_student_t_corr
# ===============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
  library(bayesplot)
  library(loo)
  library(posterior)
  library(cmdstanr)
})

# Stan options
options(mc.cores = parallel::detectCores())

# ----------------------------
# 0) PATHS E DIRECTORIES
# ----------------------------
model_tag <- "robust_student_t_corr"

voice_path <- here(
  "data",
  "raw",
  "acustic_features",
  "datiacustici",
  "AUDIO.xlsx"
)

stan_f0_path <- here("stan", "stress", "f0_main_effects_student_t_corr.stan")
stan_nne_path <- here("stan", "stress", "nne_main_effects_student_t_corr.stan")

stopifnot(file.exists(voice_path))
stopifnot(file.exists(stan_f0_path))
stopifnot(file.exists(stan_nne_path))

# Create output directories without overwriting existing files
output_dirs <- c(
  here("results"),
  here("results", "stress"),
  here("results", "stress", "models"),
  here("results", "stress", "figures"),
  here("results", "stress", "tables"),
  here("figures"),
  here("tables"),
  here("manuscript")
)

walk(output_dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# 1) CARICA DATI VOCALI (3 timepoint)
# ----------------------------
cat("\n=== LOADING VOICE DATA ===\n")

baseline <- read_excel(voice_path, sheet = "BASELINE") %>%
  mutate(timepoint = "baseline")
pre <- read_excel(voice_path, sheet = "PRE") %>%
  mutate(timepoint = "pre")
post <- read_excel(voice_path, sheet = "POST") %>%
  mutate(timepoint = "post")

df_voice <- bind_rows(baseline, pre, post)
names(df_voice) <- stringr::str_trim(names(df_voice))

# Correzione ID (consistency con altri script)
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

# ----------------------------
# 2) PREPARA OUTCOME F0 e NNE
# ----------------------------
cat("\n=== PREPARING ACOUSTIC FEATURES ===\n")

df_voice <- df_voice |>
  transmute(
    ID,
    timepoint = factor(timepoint, levels = c("baseline", "pre", "post")),

    # F0 mean per vocale
    f0_mean_a = `F0 mean Hz /a/`,
    f0_mean_i = `F0 mean Hz /i/`,
    f0_mean_u = `F0 mean Hz /u/`,

    # NNE per vocale (Normalized Noise Energy)
    nne_a = `NNE /a/`,
    nne_i = `NNE /i/`,
    nne_u = `NNE /u/`
  ) %>%
  mutate(
    # Outcome aggregato sulle vocali
    y_f0 = rowMeans(across(c(f0_mean_a, f0_mean_i, f0_mean_u)), na.rm = TRUE),
    y_nne = rowMeans(across(c(nne_a, nne_i, nne_u)), na.rm = TRUE)
  ) |>
  dplyr::select(ID, timepoint, y_f0, y_nne)

# Contrasti ortogonali
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
  ) |>
  dplyr::filter(
    !is.na(ID),
    !is.na(c1_stress),
    !is.na(c2_recovery)
  )

cat(
  "VOICE DATA: N obs =",
  nrow(df_voice),
  "| N subj =",
  n_distinct(df_voice$ID),
  "\n"
)

# ----------------------------
# 3) CREA INDICI SOGGETTI
# ----------------------------
subj_ids <- sort(unique(df_voice$ID))
N_subj <- length(subj_ids)

id_map <- tibble(ID = subj_ids, subj = seq_len(N_subj))

df_voice_stan <- df_voice |>
  inner_join(id_map, by = "ID") |>
  arrange(subj, timepoint)

cat("FINAL: N_subj =", N_subj, "| N obs =", nrow(df_voice_stan), "\n")

# ----------------------------
# 4) STATISTICHE DESCRITTIVE
# ----------------------------
cat("\n=== DESCRIPTIVE STATISTICS ===\n")

desc_stats_robust <- df_voice_stan %>%
  group_by(timepoint) %>%
  summarise(
    n_obs = n(),
    f0_mean = mean(y_f0, na.rm = TRUE),
    f0_sd = sd(y_f0, na.rm = TRUE),
    nne_mean = mean(y_nne, na.rm = TRUE),
    nne_sd = sd(y_nne, na.rm = TRUE),
    .groups = "drop"
  )

print(desc_stats_robust)

# Raw differences
cat("\n--- F0 Mean Differences ---\n")
f0_baseline <- df_voice_stan %>%
  dplyr::filter(timepoint == "baseline") %>%
  pull(y_f0)
f0_pre <- df_voice_stan %>% dplyr::filter(timepoint == "pre") %>% pull(y_f0)
f0_post <- df_voice_stan %>% dplyr::filter(timepoint == "post") %>% pull(y_f0)

cat("PRE - BASELINE:", mean(f0_pre - f0_baseline, na.rm = TRUE), "Hz\n")
cat("POST - PRE:", mean(f0_post - f0_pre, na.rm = TRUE), "Hz\n")

cat("\n--- NNE Differences ---\n")
nne_baseline <- df_voice_stan %>%
  dplyr::filter(timepoint == "baseline") %>%
  pull(y_nne)
nne_pre <- df_voice_stan %>% dplyr::filter(timepoint == "pre") %>% pull(y_nne)
nne_post <- df_voice_stan %>% dplyr::filter(timepoint == "post") %>% pull(y_nne)

cat("PRE - BASELINE:", mean(nne_pre - nne_baseline, na.rm = TRUE), "dB\n")
cat("POST - PRE:", mean(nne_post - nne_pre, na.rm = TRUE), "dB\n")

# ----------------------------
# 5) PREPARA DATI PER cmdSTAN - F0
# ----------------------------
cat("\n=== PREPARING STAN DATA: F0 ROBUST ===\n")

df_f0 <- df_voice_stan %>%
  dplyr::filter(!is.na(y_f0))

stan_data_f0_robust <- list(
  N_subj = N_subj,
  N_obs = nrow(df_f0),
  subj_id = as.integer(df_f0$subj),
  y = as.numeric(df_f0$y_f0),
  c1 = as.numeric(df_f0$c1_stress),
  c2 = as.numeric(df_f0$c2_recovery)
)

cat("F0 data: N_obs =", stan_data_f0_robust$N_obs, "\n")

# ----------------------------
# 6) FIT F0 ROBUST MODEL
# ----------------------------
cat("\n=== FITTING F0 ROBUST STUDENT-T + CORRELATED RANDOM EFFECTS MODEL ===\n")

mod_f0_robust <- cmdstan_model(stan_f0_path)

fit_f0_robust <- mod_f0_robust$sample(
  data = stan_data_f0_robust,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 4000,
  adapt_delta = 0.99,
  max_treedepth = 15,
  seed = 123
)

# Diagnostics
fit_f0_robust$cmdstan_diagnose()

# Summary
summary_f0_robust <- fit_f0_robust$summary(
  c("alpha", "b1", "b2", "tau", "Omega", "sigma_y")
)
print(data.frame(summary_f0_robust))

# Extract posterior as data.frame
post_f0_robust <- fit_f0_robust$draws(format = "df")

# Save immediately with robust-specific name
fit_f0_robust$save_object(
  file = here(
    "results",
    "stress",
    "models",
    "fit_f0_main_effects_robust_student_t_corr.rds"
  )
)

# ----------------------------
# 7) PREPARA DATI PER STAN - NNE
# ----------------------------
cat("\n=== PREPARING STAN DATA: NNE ROBUST ===\n")

df_nne <- df_voice_stan %>%
  filter(!is.na(y_nne))

stan_data_nne_robust <- list(
  N_subj = N_subj,
  N_obs = nrow(df_nne),
  subj_id = as.integer(df_nne$subj),
  y = as.numeric(df_nne$y_nne),
  c1 = as.numeric(df_nne$c1_stress),
  c2 = as.numeric(df_nne$c2_recovery)
)

cat("NNE data: N_obs =", stan_data_nne_robust$N_obs, "\n")

# ----------------------------
# 8) FIT NNE ROBUST MODEL
# ----------------------------
cat(
  "\n=== FITTING NNE ROBUST STUDENT-T + CORRELATED RANDOM EFFECTS MODEL ===\n"
)

mod_nne_robust <- cmdstan_model(stan_nne_path)

fit_nne_robust <- mod_nne_robust$sample(
  data = stan_data_nne_robust,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 4000,
  adapt_delta = 0.99,
  max_treedepth = 15,
  seed = 123
)

# Diagnostics
fit_nne_robust$cmdstan_diagnose()

# Summary
summary_nne_robust <- fit_nne_robust$summary(
  c("alpha", "b1", "b2", "tau", "Omega", "sigma_y")
)
print(data.frame(summary_nne_robust))

# Extract posterior as data.frame
post_nne_robust <- fit_nne_robust$draws(format = "df")

# Save immediately with robust-specific name
fit_nne_robust$save_object(
  file = here(
    "results",
    "stress",
    "models",
    "fit_nne_main_effects_robust_student_t_corr.rds"
  )
)

# ----------------------------
# 9) LOO CROSS-VALIDATION
# ----------------------------
cat("\n=== ROBUST MODEL LOO DIAGNOSTICS ===\n")

compute_loo_from_draws <- function(fit, variable) {
  log_lik_array <- as.array(fit$draws(variables = variable))
  r_eff <- loo::relative_eff(exp(log_lik_array))
  loo::loo(log_lik_array, r_eff = r_eff)
}

# Observation-level LOO, comparable to the original diagnostic report
loo_f0_robust <- compute_loo_from_draws(fit_f0_robust, "log_lik")
loo_nne_robust <- compute_loo_from_draws(fit_nne_robust, "log_lik")

cat("\n--- F0 robust observation-level LOO ---\n")
print(loo_f0_robust)

cat("\n--- NNE robust observation-level LOO ---\n")
print(loo_nne_robust)

# Subject-level LOO diagnostic using log_lik_subj generated in the robust Stan files
loo_f0_robust_subj <- compute_loo_from_draws(fit_f0_robust, "log_lik_subj")
loo_nne_robust_subj <- compute_loo_from_draws(fit_nne_robust, "log_lik_subj")

cat("\n--- F0 robust subject-level LOO diagnostic ---\n")
print(loo_f0_robust_subj)

cat("\n--- NNE robust subject-level LOO diagnostic ---\n")
print(loo_nne_robust_subj)

# Save LOO objects separately
saveRDS(
  loo_f0_robust,
  here("results", "stress", "models", "loo_f0_robust_student_t_corr.rds")
)
saveRDS(
  loo_nne_robust,
  here("results", "stress", "models", "loo_nne_robust_student_t_corr.rds")
)
saveRDS(
  loo_f0_robust_subj,
  here(
    "results",
    "stress",
    "models",
    "loo_f0_robust_student_t_corr_subject_level.rds"
  )
)
saveRDS(
  loo_nne_robust_subj,
  here(
    "results",
    "stress",
    "models",
    "loo_nne_robust_student_t_corr_subject_level.rds"
  )
)

# ----------------------------
# 10) POSTERIOR SUMMARIES
# ----------------------------
cat("\n=== F0 ROBUST MAIN EFFECTS SUMMARY ===\n")

f0_summary_robust <- post_f0_robust %>%
  summarise(
    alpha_median = median(alpha),
    alpha_mad = mad(alpha),
    alpha_ci_lower = quantile(alpha, 0.055),
    alpha_ci_upper = quantile(alpha, 0.945),

    b1_median = median(b1),
    b1_mad = mad(b1),
    b1_ci_lower = quantile(b1, 0.055),
    b1_ci_upper = quantile(b1, 0.945),
    b1_prob_positive = mean(b1 > 0),

    b2_median = median(b2),
    b2_mad = mad(b2),
    b2_ci_lower = quantile(b2, 0.055),
    b2_ci_upper = quantile(b2, 0.945),
    b2_prob_positive = mean(b2 > 0),

    sigma_y_median = median(sigma_y),
    sigma_y_mad = mad(sigma_y),
    sigma_y_ci_lower = quantile(sigma_y, 0.055),
    sigma_y_ci_upper = quantile(sigma_y, 0.945)
  )

print(data.frame(f0_summary_robust))
# alpha_median alpha_mad alpha_ci_lower alpha_ci_upper b1_median   b1_mad b1_ci_lower b1_ci_upper
# 1     192.4951  1.649926       189.7445       195.1044  2.302817 1.140555   0.4723249    4.160976
# b1_prob_positive  b2_median   b2_mad b2_ci_lower b2_ci_upper b2_prob_positive sigma_y_median
# 1        0.9785625 -0.3939892 1.141232   -2.205839     1.40503         0.368125       6.515633
# sigma_y_mad sigma_y_ci_lower sigma_y_ci_upper
# 1   0.3764172         5.932701         7.156086

cat("\n=== NNE ROBUST MAIN EFFECTS SUMMARY ===\n")

nne_summary_robust <- post_nne_robust %>%
  summarise(
    alpha_median = median(alpha),
    alpha_mad = mad(alpha),
    alpha_ci_lower = quantile(alpha, 0.055),
    alpha_ci_upper = quantile(alpha, 0.945),

    b1_median = median(b1),
    b1_mad = mad(b1),
    b1_ci_lower = quantile(b1, 0.055),
    b1_ci_upper = quantile(b1, 0.945),
    b1_prob_negative = mean(b1 < 0),

    b2_median = median(b2),
    b2_mad = mad(b2),
    b2_ci_lower = quantile(b2, 0.055),
    b2_ci_upper = quantile(b2, 0.945),
    b2_prob_positive = mean(b2 > 0),

    sigma_y_median = median(sigma_y),
    sigma_y_mad = mad(sigma_y),
    sigma_y_ci_lower = quantile(sigma_y, 0.055),
    sigma_y_ci_upper = quantile(sigma_y, 0.945)
  )

print(data.frame(nne_summary_robust))
# alpha_median alpha_mad alpha_ci_lower alpha_ci_upper  b1_median    b1_mad b1_ci_lower b1_ci_upper
# 1    -27.04427 0.2001873      -27.36486      -26.71895 -0.7867342 0.2660992   -1.214185  -0.3578437
# b1_prob_negative  b2_median    b2_mad b2_ci_lower b2_ci_upper b2_prob_positive sigma_y_median
# 1         0.998375 -0.3196874 0.2546215   -0.725931  0.09247086        0.1049375       1.441299
# sigma_y_mad sigma_y_ci_lower sigma_y_ci_upper
# 1   0.1009654         1.275019         1.600491

# ----------------------------
# 11) VISUALIZATIONS
# ----------------------------
cat("\n=== CREATING ROBUST FIGURES ===\n")

# Converti per bayesplot
robust_effect_vars <- c("b1", "b2")
draws_f0_robust <- fit_f0_robust$draws(variables = robust_effect_vars)
draws_nne_robust <- fit_nne_robust$draws(variables = robust_effect_vars)

# F0 posterior distributions
p_f0_post_robust <- mcmc_areas(
  draws_f0_robust,
  prob = 0.89,
  prob_outer = 0.89
) +
  labs(
    title = "F0 Mean: Robust Posterior Distributions of Main Effects",
    subtitle = "Student-t likelihood + correlated random effects; b1 = stress, b2 = recovery"
  ) +
  theme_minimal()

ggsave(
  here(
    "results",
    "stress",
    "figures",
    "f0_posterior_effects_robust_student_t_corr.png"
  ),
  p_f0_post_robust,
  width = 8,
  height = 5,
  dpi = 300
)

# NNE posterior distributions
p_nne_post_robust <- mcmc_areas(
  draws_nne_robust,
  prob = 0.89,
  prob_outer = 0.89
) +
  labs(
    title = "NNE: Robust Posterior Distributions of Main Effects",
    subtitle = "Student-t likelihood + correlated random effects; b1 = stress, b2 = recovery"
  ) +
  theme_minimal()

ggsave(
  here(
    "results",
    "stress",
    "figures",
    "nne_posterior_effects_robust_student_t_corr.png"
  ),
  p_nne_post_robust,
  width = 8,
  height = 5,
  dpi = 300
)

# Posterior predictive checks
sample_ppc_rows <- function(yrep, n = 100) {
  yrep[seq_len(min(n, nrow(yrep))), , drop = FALSE]
}

y_rep_f0_robust <- fit_f0_robust$draws("y_rep", format = "matrix")
stopifnot(length(stan_data_f0_robust$y) == ncol(y_rep_f0_robust))

ppc_f0_robust <- ppc_dens_overlay(
  y = stan_data_f0_robust$y,
  yrep = sample_ppc_rows(y_rep_f0_robust, 100)
) +
  labs(title = "F0: Robust Posterior Predictive Check") +
  theme_minimal()

ggsave(
  here("results", "stress", "figures", "f0_ppc_robust_student_t_corr.png"),
  ppc_f0_robust,
  width = 8,
  height = 5,
  dpi = 300
)

y_rep_nne_robust <- fit_nne_robust$draws("y_rep", format = "matrix")
stopifnot(length(stan_data_nne_robust$y) == ncol(y_rep_nne_robust))

ppc_nne_robust <- ppc_dens_overlay(
  y = stan_data_nne_robust$y,
  yrep = sample_ppc_rows(y_rep_nne_robust, 100)
) +
  labs(title = "NNE: Robust Posterior Predictive Check") +
  theme_minimal()

ggsave(
  here("results", "stress", "figures", "nne_ppc_robust_student_t_corr.png"),
  ppc_nne_robust,
  width = 8,
  height = 5,
  dpi = 300
)

# Model-implied marginal means
pred_baseline_f0_robust <- post_f0_robust$alpha - 0.5 * post_f0_robust$b1
pred_pre_f0_robust <- post_f0_robust$alpha +
  0.5 * post_f0_robust$b1 -
  0.5 * post_f0_robust$b2
pred_post_f0_robust <- post_f0_robust$alpha + 0.5 * post_f0_robust$b2

marginal_f0_robust <- tibble(
  timepoint = factor(
    c("BASELINE", "PRE", "POST"),
    levels = c("BASELINE", "PRE", "POST")
  ),
  median = c(
    median(pred_baseline_f0_robust),
    median(pred_pre_f0_robust),
    median(pred_post_f0_robust)
  ),
  lower = c(
    quantile(pred_baseline_f0_robust, 0.055),
    quantile(pred_pre_f0_robust, 0.055),
    quantile(pred_post_f0_robust, 0.055)
  ),
  upper = c(
    quantile(pred_baseline_f0_robust, 0.945),
    quantile(pred_pre_f0_robust, 0.945),
    quantile(pred_post_f0_robust, 0.945)
  )
)

p_f0_marginal_robust <- ggplot(
  marginal_f0_robust,
  aes(x = timepoint, y = median)
) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_line(aes(group = 1)) +
  labs(
    title = "F0 Mean: Robust Model-Implied Marginal Means",
    x = "Timepoint",
    y = "F0 (Hz)"
  ) +
  theme_minimal()

ggsave(
  here(
    "results",
    "stress",
    "figures",
    "f0_marginal_means_robust_student_t_corr.png"
  ),
  p_f0_marginal_robust,
  width = 6,
  height = 5,
  dpi = 300
)

pred_baseline_nne_robust <- post_nne_robust$alpha - 0.5 * post_nne_robust$b1
pred_pre_nne_robust <- post_nne_robust$alpha +
  0.5 * post_nne_robust$b1 -
  0.5 * post_nne_robust$b2
pred_post_nne_robust <- post_nne_robust$alpha + 0.5 * post_nne_robust$b2

marginal_nne_robust <- tibble(
  timepoint = factor(
    c("BASELINE", "PRE", "POST"),
    levels = c("BASELINE", "PRE", "POST")
  ),
  median = c(
    median(pred_baseline_nne_robust),
    median(pred_pre_nne_robust),
    median(pred_post_nne_robust)
  ),
  lower = c(
    quantile(pred_baseline_nne_robust, 0.055),
    quantile(pred_pre_nne_robust, 0.055),
    quantile(pred_post_nne_robust, 0.055)
  ),
  upper = c(
    quantile(pred_baseline_nne_robust, 0.945),
    quantile(pred_pre_nne_robust, 0.945),
    quantile(pred_post_nne_robust, 0.945)
  )
)

p_nne_marginal_robust <- ggplot(
  marginal_nne_robust,
  aes(x = timepoint, y = median)
) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_line(aes(group = 1)) +
  labs(
    title = "NNE: Robust Model-Implied Marginal Means",
    x = "Timepoint",
    y = "NNE (dB)"
  ) +
  theme_minimal()

ggsave(
  here(
    "results",
    "stress",
    "figures",
    "nne_marginal_means_robust_student_t_corr.png"
  ),
  p_nne_marginal_robust,
  width = 6,
  height = 5,
  dpi = 300
)

# ----------------------------
# 12) SAVE ROBUST RESULTS
# ----------------------------
cat("\n=== SAVING ROBUST RESULTS ===\n")

# Models already saved after sampling; save again here only to make the final output block explicit
fit_f0_robust$save_object(
  file = here(
    "results",
    "stress",
    "models",
    "fit_f0_main_effects_robust_student_t_corr.rds"
  )
)
fit_nne_robust$save_object(
  file = here(
    "results",
    "stress",
    "models",
    "fit_nne_main_effects_robust_student_t_corr.rds"
  )
)

# Summaries
write_csv(
  f0_summary_robust,
  here(
    "results",
    "stress",
    "tables",
    "f0_main_effects_summary_robust_student_t_corr.csv"
  )
)
write_csv(
  nne_summary_robust,
  here(
    "results",
    "stress",
    "tables",
    "nne_main_effects_summary_robust_student_t_corr.csv"
  )
)

# Also save CmdStan summary tables
write_csv(
  summary_f0_robust,
  here(
    "results",
    "stress",
    "tables",
    "f0_cmdstan_summary_robust_student_t_corr.csv"
  )
)
write_csv(
  summary_nne_robust,
  here(
    "results",
    "stress",
    "tables",
    "nne_cmdstan_summary_robust_student_t_corr.csv"
  )
)

# Posterior samples
write_csv(
  post_f0_robust,
  here(
    "results",
    "stress",
    "models",
    "f0_posterior_samples_robust_student_t_corr.csv"
  )
)
write_csv(
  post_nne_robust,
  here(
    "results",
    "stress",
    "models",
    "nne_posterior_samples_robust_student_t_corr.csv"
  )
)

# Descriptive stats, saved with robust suffix to avoid overwriting old file
write_csv(
  desc_stats_robust,
  here(
    "results",
    "stress",
    "descriptive_statistics_robust_student_t_corr_input.csv"
  )
)

# Bundle for downstream robust analyses
analysis_bundle_robust_student_t_corr <- list(
  model_tag = model_tag,
  stan_f0_path = stan_f0_path,
  stan_nne_path = stan_nne_path,
  stan_data_f0_robust = stan_data_f0_robust,
  stan_data_nne_robust = stan_data_nne_robust,
  df_voice = df_voice_stan,
  df_f0 = df_f0,
  df_nne = df_nne,
  id_map = id_map,
  desc_stats_robust = desc_stats_robust,
  fit_f0_robust = fit_f0_robust,
  fit_nne_robust = fit_nne_robust,
  post_f0_robust = post_f0_robust,
  post_nne_robust = post_nne_robust,
  summary_f0_robust = summary_f0_robust,
  summary_nne_robust = summary_nne_robust,
  f0_summary_robust = f0_summary_robust,
  nne_summary_robust = nne_summary_robust,
  loo_f0_robust = loo_f0_robust,
  loo_nne_robust = loo_nne_robust,
  loo_f0_robust_subj = loo_f0_robust_subj,
  loo_nne_robust_subj = loo_nne_robust_subj,
  marginal_f0_robust = marginal_f0_robust,
  marginal_nne_robust = marginal_nne_robust
)

saveRDS(
  analysis_bundle_robust_student_t_corr,
  file = here("results", "stress", "analysis_bundle_robust_student_t_corr.rds")
)

cat("\n=== ROBUST ANALYSIS COMPLETE ===\n")
cat("Saved robust outputs:\n")
cat(" - results/stress/models/fit_f0_main_effects_robust_student_t_corr.rds\n")
cat(" - results/stress/models/fit_nne_main_effects_robust_student_t_corr.rds\n")
cat(" - results/stress/models/loo_*_robust_student_t_corr*.rds\n")
cat(" - results/stress/models/*_posterior_samples_robust_student_t_corr.csv\n")
cat(" - results/stress/tables/*_robust_student_t_corr.csv\n")
cat(" - results/stress/figures/*_robust_student_t_corr.png\n")
cat(" - results/stress/analysis_bundle_robust_student_t_corr.rds\n")

loo::loo_compare(loo_f0, loo_f0_robust)
#      elpd_diff se_diff
# model2   0.0       0.0
# model1 -16.3       5.9

loo::loo_compare(loo_nne, loo_nne_robust)
#      elpd_diff se_diff
# model2   0.0       0.0
# model1 -12.9       6.9

# eof ---
