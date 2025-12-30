# ==============================================================================
# 01_main_effects_analysis.R
# Analisi effetti principali dello stress da esame su caratteristiche vocali
# Outcome: F0 mean, NNE (aggregati su vocali /a i u/)
# Contrasti: c1_stress (PRE vs BASELINE), c2_recovery (POST vs PRE)
# NO moderazione PID-5 (solo effetti principali)
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
  library(rstan)
  library(bayesplot)
  library(loo)
  library(posterior)
})

# Stan options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# ----------------------------
# 0) PATHS E DIRECTORIES
# ----------------------------
voice_path <- here(
  "data",
  "raw",
  "acustic_features",
  "datiacustici",
  "AUDIO.xlsx"
)

stopifnot(file.exists(voice_path))

# Create output directories
dir.create("models", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)
dir.create("manuscript", showWarnings = FALSE)

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
    # Outcome robusto: media sulle vocali (riduce rumore vocale-specifico)
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

desc_stats <- df_voice_stan %>%
  group_by(timepoint) %>%
  summarise(
    n_obs = n(),
    f0_mean = mean(y_f0, na.rm = TRUE),
    f0_sd = sd(y_f0, na.rm = TRUE),
    nne_mean = mean(y_nne, na.rm = TRUE),
    nne_sd = sd(y_nne, na.rm = TRUE),
    .groups = "drop"
  )

print(desc_stats)

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
cat("\n=== PREPARING STAN DATA: F0 ===\n")

# Filtra NA in F0
df_f0 <- df_voice_stan %>%
  dplyr::filter(!is.na(y_f0))

stan_data_f0 <- list(
  N_subj = N_subj,
  N_obs = nrow(df_f0),
  subj_id = as.integer(df_f0$subj),
  y = as.numeric(df_f0$y_f0),
  c1 = as.numeric(df_f0$c1_stress),
  c2 = as.numeric(df_f0$c2_recovery)
)

cat("F0 data: N_obs =", stan_data_f0$N_obs, "\n")

# ----------------------------
# 6) FIT F0 MODEL
# ----------------------------
cat("\n=== FITTING F0 MAIN EFFECTS MODEL ===\n")

mod_f0 <- cmdstan_model("stan/stress/f0_main_effects.stan")

fit_f0 <- mod_f0$sample(
  data = stan_data_f0,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 4000,
  adapt_delta = 0.95,
  max_treedepth = 15,
  seed = 123
)

# Diagnostics
fit_f0$cmdstan_diagnose()

# Summary
summary_f0 <- fit_f0$summary(c("alpha", "b1", "b2", "tau", "sigma_y"))
print(data.frame(summary_f0))

# Extract posterior as data.frame
post_f0 <- fit_f0$draws(format = "df")

fit_f0$save_object(file = "results/stress/models/fit_f0_main_effects.RDS")

# ----------------------------
# 7) PREPARA DATI PER STAN - NNE
# ----------------------------
cat("\n=== PREPARING STAN DATA: NNE ===\n")

df_nne <- df_voice_stan %>%
  filter(!is.na(y_nne))

stan_data_nne <- list(
  N_subj = N_subj,
  N_obs = nrow(df_nne),
  subj_id = as.integer(df_nne$subj),
  y = as.numeric(df_nne$y_nne),
  c1 = as.numeric(df_nne$c1_stress),
  c2 = as.numeric(df_nne$c2_recovery)
)

cat("NNE data: N_obs =", stan_data_nne$N_obs, "\n")

# ----------------------------
# 8) FIT NNE MODEL
# ----------------------------
cat("\n=== FITTING NNE MAIN EFFECTS MODEL ===\n")

mod_nne <- cmdstan_model("stan/stress/nne_main_effects.stan")

fit_nne <- mod_nne$sample(
  data = stan_data_nne,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 4000,
  adapt_delta = 0.95,
  max_treedepth = 15,
  seed = 123
)

# Diagnostics
fit_nne$cmdstan_diagnose()

# Summary
summary_nne <- fit_nne$summary(c("alpha", "b1", "b2", "tau", "sigma_y"))
print(data.frame(summary_nne))

# Extract posterior as data.frame
post_nne <- fit_nne$draws(format = "df")

fit_nne$save_object(file = "results/stress/models/fit_nne_main_effects.RDS")

# ----------------------------
# 9) LOO CROSS-VALIDATION
# ----------------------------
cat("\n=== MODEL COMPARISON (LOO) ===\n")

# cmdstan ha metodo diretto per LOO
loo_f0 <- fit_f0$loo()
loo_nne <- fit_nne$loo()

print(loo_f0)
print(loo_nne)

# ----------------------------
# 10) POSTERIOR SUMMARIES
# ----------------------------
cat("\n=== F0 MAIN EFFECTS SUMMARY ===\n")

f0_summary <- post_f0 %>%
  summarise(
    alpha_median = median(alpha),
    alpha_mad = mad(alpha),
    alpha_ci_lower = quantile(alpha, 0.025),
    alpha_ci_upper = quantile(alpha, 0.975),

    b1_median = median(b1),
    b1_mad = mad(b1),
    b1_ci_lower = quantile(b1, 0.025),
    b1_ci_upper = quantile(b1, 0.975),
    b1_prob_positive = mean(b1 > 0),

    b2_median = median(b2),
    b2_mad = mad(b2),
    b2_ci_lower = quantile(b2, 0.025),
    b2_ci_upper = quantile(b2, 0.975),
    b2_prob_positive = mean(b2 > 0)
  )

data.frame(f0_summary)
# alpha_median alpha_mad alpha_ci_lower alpha_ci_upper b1_median  b1_mad b1_ci_lower
# 1     192.4769   1.73356       189.0708       195.9646  3.271918 1.25095   0.8127971
# b1_ci_upper b1_prob_positive b2_median   b2_mad b2_ci_lower b2_ci_upper
# 1    5.708922          0.99525 0.1404935 1.241791   -2.340882    2.593917
# b2_prob_positive
# 1         0.542375

cat("\n=== NNE MAIN EFFECTS SUMMARY ===\n")

nne_summary <- post_nne %>%
  summarise(
    alpha_median = median(alpha),
    alpha_mad = mad(alpha),
    alpha_ci_lower = quantile(alpha, 0.025),
    alpha_ci_upper = quantile(alpha, 0.975),

    b1_median = median(b1),
    b1_mad = mad(b1),
    b1_ci_lower = quantile(b1, 0.025),
    b1_ci_upper = quantile(b1, 0.975),
    b1_prob_negative = mean(b1 < 0),

    b2_median = median(b2),
    b2_mad = mad(b2),
    b2_ci_lower = quantile(b2, 0.025),
    b2_ci_upper = quantile(b2, 0.975),
    b2_prob_positive = mean(b2 > 0)
  )

data.frame(nne_summary)
# alpha_median alpha_mad alpha_ci_lower alpha_ci_upper  b1_median   b1_mad b1_ci_lower
# 1    -26.86714 0.2020028      -27.27755      -26.46701 -0.6511491 0.284773   -1.203382
# b1_ci_upper b1_prob_negative  b2_median   b2_mad b2_ci_lower b2_ci_upper
# 1  -0.1077927          0.98975 -0.2207684 0.284177  -0.7677878    0.330555
# b2_prob_positive
# 1        0.2194375

# ----------------------------
# 11) VISUALIZATIONS
# ----------------------------
cat("\n=== CREATING FIGURES ===\n")

# Converti per bayesplot (rimuovi metadati)
draws_f0 <- fit_f0$draws(variables = c("b1", "b2"))
draws_nne <- fit_nne$draws(variables = c("b1", "b2"))

# F0 posterior distributions
p_f0_post <- mcmc_areas(
  draws_f0,
  prob = 0.95,
  prob_outer = 0.99
) +
  labs(
    title = "F0 Mean: Posterior Distributions of Main Effects",
    subtitle = "b1 = stress (PRE vs BASELINE), b2 = recovery (POST vs PRE)"
  ) +
  theme_minimal()

ggsave(
  here("results", "stress", "figures", "f0_posterior_effects.png"),
  p_f0_post,
  width = 8,
  height = 5,
  dpi = 300
)

# NNE posterior distributions
p_nne_post <- mcmc_areas(
  draws_nne,
  prob = 0.95,
  prob_outer = 0.99
) +
  labs(
    title = "NNE: Posterior Distributions of Main Effects",
    subtitle = "b1 = stress (PRE vs BASELINE), b2 = recovery (POST vs PRE)"
  ) +
  theme_minimal()

ggsave(
  here("results", "stress", "figures", "nne_posterior_effects.png"),
  p_nne_post,
  width = 8,
  height = 5,
  dpi = 300
)

# Posterior predictive checks
y_rep_f0 <- fit_f0$draws("y_rep", format = "matrix")
ppc_f0 <- ppc_dens_overlay(y = stan_data_f0$y, yrep = y_rep_f0[1:100, ]) +
  labs(title = "F0: Posterior Predictive Check") +
  theme_minimal()
ggsave(
  here("results", "stress", "figures", "f0_ppc.png"),
  ppc_f0,
  width = 8,
  height = 5,
  dpi = 300
)

y_rep_nne <- fit_nne$draws("y_rep", format = "matrix")
ppc_nne <- ppc_dens_overlay(y = stan_data_nne$y, yrep = y_rep_nne[1:100, ]) +
  labs(title = "NNE: Posterior Predictive Check") +
  theme_minimal()
ggsave(
  here("results", "stress", "figures", "nne_ppc.png"),
  ppc_nne,
  width = 8,
  height = 5,
  dpi = 300
)

# Model-implied marginal means (usando post_f0 già come tibble)
pred_baseline_f0 <- post_f0$alpha - 0.5 * post_f0$b1
pred_pre_f0 <- post_f0$alpha + 0.5 * post_f0$b1 - 0.5 * post_f0$b2
pred_post_f0 <- post_f0$alpha + 0.5 * post_f0$b2

marginal_f0 <- tibble(
  timepoint = factor(
    c("BASELINE", "PRE", "POST"),
    levels = c("BASELINE", "PRE", "POST")
  ),
  median = c(
    median(pred_baseline_f0),
    median(pred_pre_f0),
    median(pred_post_f0)
  ),
  lower = c(
    quantile(pred_baseline_f0, 0.025),
    quantile(pred_pre_f0, 0.025),
    quantile(pred_post_f0, 0.025)
  ),
  upper = c(
    quantile(pred_baseline_f0, 0.975),
    quantile(pred_pre_f0, 0.975),
    quantile(pred_post_f0, 0.975)
  )
)

p_f0_marginal <- ggplot(marginal_f0, aes(x = timepoint, y = median)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_line(aes(group = 1)) +
  labs(
    title = "F0 Mean: Model-Implied Marginal Means",
    x = "Timepoint",
    y = "F0 (Hz)"
  ) +
  theme_minimal()

ggsave(
  here("results", "stress", "figures", "f0_marginal_means.png"),
  p_f0_marginal,
  width = 6,
  height = 5,
  dpi = 300
)

# NNE
pred_baseline_nne <- post_nne$alpha - 0.5 * post_nne$b1
pred_pre_nne <- post_nne$alpha + 0.5 * post_nne$b1 - 0.5 * post_nne$b2
pred_post_nne <- post_nne$alpha + 0.5 * post_nne$b2

marginal_nne <- tibble(
  timepoint = factor(
    c("BASELINE", "PRE", "POST"),
    levels = c("BASELINE", "PRE", "POST")
  ),
  median = c(
    median(pred_baseline_nne),
    median(pred_pre_nne),
    median(pred_post_nne)
  ),
  lower = c(
    quantile(pred_baseline_nne, 0.025),
    quantile(pred_pre_nne, 0.025),
    quantile(pred_post_nne, 0.025)
  ),
  upper = c(
    quantile(pred_baseline_nne, 0.975),
    quantile(pred_pre_nne, 0.975),
    quantile(pred_post_nne, 0.975)
  )
)

p_nne_marginal <- ggplot(marginal_nne, aes(x = timepoint, y = median)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_line(aes(group = 1)) +
  labs(
    title = "NNE: Model-Implied Marginal Means",
    x = "Timepoint",
    y = "NNE (dB)"
  ) +
  theme_minimal()

ggsave(
  here("results", "stress", "figures", "nne_marginal_means.png"),
  p_nne_marginal,
  width = 6,
  height = 5,
  dpi = 300
)

# ----------------------------
# 12) SAVE RESULTS
# ----------------------------
cat("\n=== SAVING RESULTS ===\n")

# Models (cmdstan objects)
fit_f0$save_object(
  file = here("results", "stress", "models", "fit_f0_main_effects.rds")
)
fit_nne$save_object(
  file = here("results", "stress", "models", "fit_nne_main_effects.rds")
)

# Summaries
write_csv(f0_summary, here("results", "f0_main_effects_summary.csv"))
write_csv(nne_summary, here("results", "nne_main_effects_summary.csv"))

# Posterior samples
write_csv(
  post_f0,
  here("results", "stress", "models", "f0_posterior_samples.csv")
)
write_csv(
  post_nne,
  here("results", "stress", "models", "nne_posterior_samples.csv")
)

# Descriptive stats
write_csv(desc_stats, here("results", "stress", "descriptive_statistics.csv"))

# Bundle for downstream analyses
saveRDS(
  list(
    stan_data_f0 = stan_data_f0,
    stan_data_nne = stan_data_nne,
    df_voice = df_voice_stan,
    id_map = id_map,
    desc_stats = desc_stats,
    fit_f0 = fit_f0,
    fit_nne = fit_nne,
    post_f0 = post_f0,
    post_nne = post_nne
  ),
  file = here("results", "stress", "analysis_bundle.rds")
)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Saved:\n")
cat(" - models/fit_f0_main_effects.rds\n")
cat(" - models/fit_nne_main_effects.rds\n")
cat(" - results/*.csv\n")
cat(" - figures/*.png\n")

# eof ---
