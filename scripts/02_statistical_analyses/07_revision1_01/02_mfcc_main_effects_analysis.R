# ==============================================================================
# 02_mfcc_main_effects_analysis.R
# Effetti principali dello stress da esame sul profilo MFCC della FRASE (parlato connesso)
# Outcome: vettore delle 13 medie MFCC (MFCC1..MFCC13 "- mean")
# Contrasti: c1_stress (PRE vs BASELINE), c2_recovery (POST vs PRE)  [identici a F0/NNE]
# Modello: gerarchico MULTIVARIATO (multi_normal), intercetta random K-dim per soggetto
# Omnibus: D_stress / D_recovery (Mahalanobis) + confronto LOO (modello con vs senza effetti)
# NO moderazione PID-5 (solo effetti principali)  -> file: mfcc_main_effects.stan
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
  library(cmdstanr) # NB: usiamo il backend cmdstanr (come nei tuoi $sample/$loo)
  library(posterior)
  library(bayesplot)
  library(loo)
})

options(mc.cores = parallel::detectCores())

# ----------------------------
# 0) PATHS E DIRECTORIES
# ----------------------------
# NB: input DIVERSO rispetto a 01 -> file MFCC della frase
mfcc_path <- here(
  "data",
  "raw",
  "acustic_features",
  "datiacustici",
  "AUDIO_sent.xlsx"
)
stopifnot(file.exists(mfcc_path))

# modello Stan (stesso layout dei modelli f0/nne)
stan_file <- here("stan", "mfcc", "mfcc_main_effects.stan")
stopifnot(file.exists(stan_file))

dir.create(
  here("results", "mfcc", "models"),
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  here("results", "mfcc", "figures"),
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  here("results", "mfcc", "tables"),
  recursive = TRUE,
  showWarnings = FALSE
)

# ----------------------------
# 1) CARICA DATI MFCC (3 timepoint)
# ----------------------------
cat("\n=== LOADING MFCC (SENTENCE) DATA ===\n")

baseline <- read_excel(mfcc_path, sheet = "Baseline") %>%
  mutate(timepoint = "baseline")
pre <- read_excel(mfcc_path, sheet = "Pre") %>% mutate(timepoint = "pre")
post <- read_excel(mfcc_path, sheet = "Post") %>% mutate(timepoint = "post")

df_mfcc <- bind_rows(baseline, pre, post)
names(df_mfcc) <- stringr::str_trim(names(df_mfcc))

# Correzione ID (IDENTICA agli altri script, per allineare i soggetti)
df_mfcc <- df_mfcc |>
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
# 2) PREPARA OUTCOME MULTIVARIATO (13 medie MFCC) + STANDARDIZZAZIONE
# ----------------------------
cat("\n=== PREPARING MFCC FEATURES ===\n")

K <- 13L
mfcc_cols <- paste0("MFCC", 1:K, " - mean") # nomi esatti nel file
mfcc_labels <- paste0("MFCC", 1:K)
stopifnot(all(mfcc_cols %in% names(df_mfcc)))

df_mfcc <- df_mfcc |>
  dplyr::select(ID, timepoint, all_of(mfcc_cols)) |>
  mutate(
    timepoint = factor(timepoint, levels = c("baseline", "pre", "post"))
  ) |>
  # tieni solo righe con vettore MFCC COMPLETO (multi_normal richiede vettori completi)
  tidyr::drop_na(all_of(mfcc_cols)) |>
  dplyr::filter(!is.na(ID))

# --- OPZIONALE: allinea al campione analitico canonico (es. lo stesso di F0/NNE) ---
# Per coerenza con il resto del paper puoi restringere qui a una lista master di ID.
# master_ids <- readRDS(here("results","mfcc","analysis_bundle.rds"))$id_map$ID
# df_mfcc <- df_mfcc |> dplyr::filter(ID %in% master_ids)

# Standardizzazione per coefficiente (z-score sull'intero dataset pooled).
# Salviamo center/scale per poter ri-trasformare le marginali in unita' MFCC grezze.
mfcc_center <- df_mfcc |>
  summarise(across(all_of(mfcc_cols), \(x) mean(x))) |>
  unlist()
mfcc_scale <- df_mfcc |>
  summarise(across(all_of(mfcc_cols), \(x) sd(x))) |>
  unlist()

df_z <- df_mfcc |>
  mutate(across(all_of(mfcc_cols), \(x) as.numeric(scale(x)))) # z per coefficiente

# Contrasti ortogonali (IDENTICI a 01_main_effects_analysis.R)
df_z <- df_z |>
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
  )

# ----------------------------
# 3) CREA INDICI SOGGETTI
# ----------------------------
subj_ids <- sort(unique(df_z$ID))
N_subj <- length(subj_ids)
id_map <- tibble(ID = subj_ids, subj = seq_len(N_subj))

df_stan <- df_z |>
  inner_join(id_map, by = "ID") |>
  arrange(subj, timepoint)

cat("MFCC DATA: N obs =", nrow(df_stan), "| N subj =", N_subj, "| K =", K, "\n")

# ----------------------------
# 4) STATISTICHE DESCRITTIVE (su scala grezza)
# ----------------------------
cat("\n=== DESCRIPTIVE STATISTICS (raw MFCC means per timepoint) ===\n")

desc_stats <- df_mfcc |>
  group_by(timepoint) |>
  summarise(
    across(all_of(mfcc_cols), \(x) mean(x)),
    n_obs = n(),
    .groups = "drop"
  )
print(as.data.frame(desc_stats))

# Vettori di differenza grezzi (per riferimento all'omnibus frequentista)
raw_wide <- df_mfcc |>
  pivot_longer(all_of(mfcc_cols), names_to = "coef", values_to = "val") |>
  group_by(timepoint, coef) |>
  summarise(m = mean(val), .groups = "drop") |>
  pivot_wider(names_from = timepoint, values_from = m)
cat("\n--- Raw mean-shift vectors ---\n")
print(
  raw_wide |>
    transmute(
      coef,
      stress_PRE_minus_BASE = pre - baseline,
      recov_POST_minus_PRE = post - pre
    )
)

# ----------------------------
# 5) DATI PER STAN (outcome multivariato)
# ----------------------------
Y <- as.matrix(df_stan[, mfcc_cols]) # N_obs x K (standardizzata)
storage.mode(Y) <- "double"

stan_data <- list(
  N_obs = nrow(df_stan),
  N_subj = N_subj,
  K = K,
  subj_id = as.integer(df_stan$subj),
  Y = Y,
  c1 = as.numeric(df_stan$c1_stress),
  c2 = as.numeric(df_stan$c2_recovery)
)

# ----------------------------
# 6) FIT MODELLO PIENO
# ----------------------------
cat("\n=== FITTING MFCC MULTIVARIATE MAIN-EFFECTS MODEL ===\n")

mod <- cmdstan_model(stan_file)

fit_full <- mod$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 4000,
  adapt_delta = 0.95, # modello multivariato + LKJ: alza a 0.99 se divergenze
  max_treedepth = 15,
  seed = 123
)

fit_full$cmdstan_diagnose()

summary_full <- fit_full$summary(
  c("alpha", "beta1", "beta2", "tau_u", "sigma", "D_stress", "D_recovery")
)
print(as.data.frame(summary_full), digits = 3)

fit_full$save_object(here(
  "results",
  "mfcc",
  "models",
  "fit_mfcc_main_effects.rds"
))

# ----------------------------
# 7) FIT MODELLO NULLO (per omnibus via LOO)
#    Stesso modello con contrasti azzerati: beta1/beta2 vengono moltiplicati per 0
#    => mu = alpha + intercetta random (modello "solo intercetta"). I beta campionano
#    dalla prior ma non influenzano mu/log_lik. ELPD vs pieno = test omnibus.
# ----------------------------
# cat("\n=== FITTING NULL MODEL (zeroed contrasts) FOR OMNIBUS LOO ===\n")
#
# stan_data_null <- stan_data
# stan_data_null$c1 <- rep(0, stan_data$N_obs)
# stan_data_null$c2 <- rep(0, stan_data$N_obs)
#
# fit_null <- mod$sample(
#   data            = stan_data_null,
#   chains          = 4,
#   parallel_chains = 4,
#   iter_warmup     = 2000,
#   iter_sampling   = 4000,
#   adapt_delta     = 0.95,
#   max_treedepth   = 15,
#   seed            = 123
# )

# ----------------------------
# 8) OMNIBUS: LOO (pieno vs nullo)
# ----------------------------
# cat("\n=== OMNIBUS MODEL COMPARISON (LOO) ===\n")
loo_full <- fit_full$loo()
# loo_null <- fit_null$loo()
# print(loo_full)
# loo_cmp <- loo::loo_compare(list(full = loo_full, null = loo_null))
# print(loo_cmp)
# CAVEAT: per questo modello multivariato la PSIS-LOO ha Pareto-k inaffidabili
# (p_loo elevato, molte k > 0.7), quindi questo confronto NON va usato come omnibus
# ne' citato come evidenza nel manoscritto. L'omnibus di riferimento e' la distanza
# di Mahalanobis a posteriori (sezione 9, con CrI 89%); il Wald a posteriori in
# 03_mfcc_omnibus_bayesian.R serve solo da calibrazione frequentista descrittiva.
# (Si puo' rimuovere l'intero fit nullo + loo_compare senza perdere nulla.)

# ----------------------------
# 9) OMNIBUS: effect-size di Mahalanobis (analogo bayesiano di Hotelling T^2)
# ----------------------------
cat("\n=== OMNIBUS MULTIVARIATE EFFECT SIZE (Mahalanobis) ===\n")
post <- fit_full$draws(format = "df")

omnibus <- tibble(
  contrast = c("stress (PRE-BASE)", "recovery (POST-PRE)"),
  D_median = c(median(post$D_stress), median(post$D_recovery)),
  D_lo = c(quantile(post$D_stress, .055), quantile(post$D_recovery, .055)),
  D_hi = c(quantile(post$D_stress, .945), quantile(post$D_recovery, .945))
)
print(omnibus)
# Riferimento dal primo passo (frequentista, Hotelling su 13 medie):
#   stress  D ~ 0.48 (omnibus p ~ .024) ; recovery D ~ 0.42 (omnibus p ~ .11)
# contrast            D_median  D_lo  D_hi
# <chr>                  <dbl> <dbl> <dbl>
#   1 stress (PRE-BASE)    0.867 0.653 1.09
# 2 recovery (POST-PRE)    0.696 0.500 0.897

# ----------------------------
# 10) PER-COEFFICIENTE: beta1 / beta2 con CrI e PD (linguaggio del manoscritto)
# ----------------------------
summarise_effect <- function(draws_df, par, labels) {
  purrr::map_dfr(seq_along(labels), function(k) {
    x <- draws_df[[sprintf("%s[%d]", par, k)]]
    tibble(
      coef = labels[k],
      median = median(x),
      ci_low = quantile(x, .055),
      ci_high = quantile(x, .945),
      PD = max(mean(x > 0), mean(x < 0)) # probability of direction
    )
  })
}

eff_stress <- summarise_effect(post, "beta1", mfcc_labels) |>
  mutate(contrast = "stress")
eff_recovery <- summarise_effect(post, "beta2", mfcc_labels) |>
  mutate(contrast = "recovery")
eff_all <- bind_rows(eff_stress, eff_recovery)

cat("\n--- Per-coefficient stress effects (SD units) ---\n")
print(as.data.frame(eff_stress), digits = 3)
cat("\n--- Per-coefficient recovery effects (SD units) ---\n")
print(as.data.frame(eff_recovery), digits = 3)

# Salva tabelle
write_csv(
  omnibus,
  here("results", "mfcc", "tables", "mfcc_omnibus_mahalanobis.csv")
)
write_csv(
  eff_all,
  here("results", "mfcc", "tables", "mfcc_per_coefficient_effects.csv")
)

# ----------------------------
# 11) FIGURE
# ----------------------------
cat("\n=== CREATING FIGURES ===\n")

# 11a) Omnibus: posterior di D_stress e D_recovery
p_omni <- fit_full$draws(c("D_stress", "D_recovery")) |>
  mcmc_areas(prob = 0.89, prob_outer = 0.89) +
  labs(
    title = "MFCC: omnibus multivariate effect size",
    subtitle = "Mahalanobis length of the stress / recovery shift vectors"
  ) +
  theme_minimal()
ggsave(
  here("results", "mfcc", "figures", "mfcc_omnibus_D.png"),
  p_omni,
  width = 8,
  height = 4,
  dpi = 300
)

# 11b) Per-coefficiente: intervalli di beta1 (stress) e beta2 (recovery)
p_b1 <- fit_full$draws(paste0("beta1[", 1:K, "]")) |>
  mcmc_intervals(prob = 0.5, prob_outer = 0.89) +
  labs(title = "Stress effect per MFCC coefficient (beta1, SD units)") +
  theme_minimal()
p_b2 <- fit_full$draws(paste0("beta2[", 1:K, "]")) |>
  mcmc_intervals(prob = 0.5, prob_outer = 0.89) +
  labs(title = "Recovery effect per MFCC coefficient (beta2, SD units)") +
  theme_minimal()
ggsave(
  here("results", "stress", "figures", "mfcc_beta1_intervals.png"),
  p_b1,
  width = 7,
  height = 5,
  dpi = 300
)
ggsave(
  here("results", "stress", "figures", "mfcc_beta2_intervals.png"),
  p_b2,
  width = 7,
  height = 5,
  dpi = 300
)

# 11c) Marginal means model-implied per coefficiente (ri-trasformate in unita' grezze)
inv_z <- function(z, k) mfcc_center[k] + mfcc_scale[k] * z
marg <- purrr::map_dfr(1:K, function(k) {
  a <- post[[sprintf("alpha[%d]", k)]]
  b1 <- post[[sprintf("beta1[%d]", k)]]
  b2 <- post[[sprintf("beta2[%d]", k)]]
  m_base <- inv_z(a - 0.5 * b1, k)
  m_pre <- inv_z(a + 0.5 * b1 - 0.5 * b2, k)
  m_post <- inv_z(a + 0.5 * b2, k)
  tibble(
    coef = mfcc_labels[k],
    timepoint = factor(
      c("BASELINE", "PRE", "POST"),
      levels = c("BASELINE", "PRE", "POST")
    ),
    median = c(median(m_base), median(m_pre), median(m_post)),
    lower = c(
      quantile(m_base, .055),
      quantile(m_pre, .055),
      quantile(m_post, .055)
    ),
    upper = c(
      quantile(m_base, .945),
      quantile(m_pre, .945),
      quantile(m_post, .945)
    )
  )
})
p_marg <- ggplot(marg, aes(timepoint, median, group = 1)) +
  geom_line() +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .15) +
  facet_wrap(~coef, scales = "free_y") +
  labs(
    title = "MFCC: model-implied marginal means by timepoint",
    y = "MFCC (raw units)",
    x = NULL
  ) +
  theme_minimal()
ggsave(
  here("results", "stress", "figures", "mfcc_marginal_means.png"),
  p_marg,
  width = 11,
  height = 8,
  dpi = 300
)

# 11d) Posterior predictive check faceted (per coefficiente, standardizzato)
#      NB: passo piu' pesante. Estrae Y_rep coefficiente-per-coefficiente e assottiglia.
#      Riduci n_show o commenta questo blocco se serve.
n_show <- 60L
yrep_long <- purrr::map_dfr(1:K, function(k) {
  vars <- sprintf("Y_rep[%d,%d]", seq_len(stan_data$N_obs), k)
  m <- fit_full$draws(variables = vars, format = "matrix")
  idx <- sample.int(nrow(m), n_show)
  as_tibble(m[idx, , drop = FALSE], .name_repair = "minimal") |>
    setNames(seq_len(stan_data$N_obs)) |>
    mutate(.draw = row_number()) |>
    pivot_longer(-.draw, names_to = "obs", values_to = "value") |>
    mutate(coef = mfcc_labels[k])
})
obs_long <- tibble(
  value = as.vector(Y), # column-major: coef1 (tutte le righe), coef2, ...
  coef = rep(mfcc_labels, each = stan_data$N_obs)
)
p_ppc <- ggplot() +
  geom_density(
    data = yrep_long,
    aes(value, group = .draw),
    colour = "lightblue",
    linewidth = .2,
    alpha = .4
  ) +
  geom_density(data = obs_long, aes(value), colour = "navy", linewidth = .8) +
  facet_wrap(~coef, scales = "free") +
  labs(
    title = "MFCC: posterior predictive check (per coefficient, standardized)"
  ) +
  theme_minimal()
ggsave(
  here("results", "mfcc", "figures", "mfcc_ppc.png"),
  p_ppc,
  width = 11,
  height = 8,
  dpi = 300
)

# ----------------------------
# 12) SAVE RESULTS
# ----------------------------
cat("\n=== SAVING RESULTS ===\n")

write_csv(
  as.data.frame(summary_full),
  here("results", "mfcc", "tables", "mfcc_posterior_summary.csv")
)
write_csv(
  desc_stats,
  here("results", "mfcc", "tables", "mfcc_descriptives.csv")
)

saveRDS(
  list(
    stan_data = stan_data,
    df_stan = df_stan,
    id_map = id_map,
    mfcc_center = mfcc_center,
    mfcc_scale = mfcc_scale,
    fit_full = fit_full,
    # fit_null    = fit_null,
    loo_full = loo_full,
    # loo_null    = loo_null,
    # loo_compare = loo_cmp,
    omnibus = omnibus,
    eff_all = eff_all
  ),
  file = here("results", "mfcc", "mfcc_analysis_bundle.rds")
)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Saved:\n")
cat(" - results/mfcc/models/fit_mfcc_main_effects.rds\n")
cat(
  " - results/mfcc/tables/*.csv  (omnibus, per-coefficient, summary, descriptives)\n"
)
cat(
  " - results/mfcc/figures/*.png (omnibus D, beta intervals, marginal means, PPC)\n"
)
cat(" - results/mfcc/mfcc_analysis_bundle.rds\n")

# eof ---
