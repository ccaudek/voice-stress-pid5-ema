# ==============================================================================
# 1a_f0_cross_vowel_concordance.R
#
# Analisi di concordanza tra le tre vocali per F0 mean:
# - valori assoluti: /a/, /i/, /u/ a baseline, pre, post
# - cambiamenti: PRE - BASELINE e POST - PRE
# - ICC/reliability del composito medio
# - sensitivity model: timepoint x vowel
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
  library(psych)
  library(lme4)
  library(broom.mixed)
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

stopifnot(file.exists(voice_path))

out_dir <- here("results", "F0", "cross_vowel")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# 1) LOAD VOICE DATA
#    Stessa logica del primo script, ma NON eliminiamo le vocali singole.
# ----------------------------

baseline <- read_excel(voice_path, sheet = "BASELINE") %>%
  mutate(timepoint = "baseline")

pre <- read_excel(voice_path, sheet = "PRE") %>%
  mutate(timepoint = "pre")

post <- read_excel(voice_path, sheet = "POST") %>%
  mutate(timepoint = "post")

df_voice_raw <- bind_rows(baseline, pre, post)
names(df_voice_raw) <- stringr::str_trim(names(df_voice_raw))

df_voice <- df_voice_raw |>
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
  transmute(
    ID,
    timepoint = factor(timepoint, levels = c("baseline", "pre", "post")),

    f0_mean_a = as.numeric(`F0 mean Hz /a/`),
    f0_mean_i = as.numeric(`F0 mean Hz /i/`),
    f0_mean_u = as.numeric(`F0 mean Hz /u/`)
  ) |>
  filter(!is.na(ID)) |>
  mutate(
    n_vowels_observed = rowSums(
      !is.na(across(c(f0_mean_a, f0_mean_i, f0_mean_u)))
    ),
    y_f0 = if_else(
      n_vowels_observed > 0,
      rowMeans(across(c(f0_mean_a, f0_mean_i, f0_mean_u)), na.rm = TRUE),
      NA_real_
    ),
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
  filter(n_vowels_observed > 0)

# Opzionale ma consigliato:
# se esiste il bundle Stan del modello principale, restringiamo l'analisi
# agli stessi soggetti usati nell'analisi F0 principale.

bundle_path <- here("results", "F0", "data", "stan_bundle_f0mean_pid5.rds")

if (file.exists(bundle_path)) {
  bundle <- readRDS(bundle_path)
  main_ids <- unique(bundle$df_voice$ID)

  df_voice <- df_voice |>
    filter(ID %in% main_ids)

  message(
    "Analisi ristretta agli ID del modello F0 principale: ",
    n_distinct(df_voice$ID),
    " soggetti."
  )
} else {
  message(
    "Bundle Stan non trovato. Uso tutti i soggetti disponibili in AUDIO.xlsx: ",
    n_distinct(df_voice$ID),
    " soggetti."
  )
}

write.csv(
  df_voice,
  file = file.path(out_dir, "f0_cross_vowel_input_data.csv"),
  row.names = FALSE
)

cat(
  "VOICE DATA: N obs = ",
  nrow(df_voice),
  " | N subj = ",
  n_distinct(df_voice$ID),
  "\n",
  sep = ""
)

# ----------------------------
# 2) HELPER FUNCTIONS
# ----------------------------

boot_ci_cor <- function(x, y, method = "spearman", B = 2000) {
  d <- tibble(x = x, y = y) |>
    drop_na()

  n <- nrow(d)

  if (n < 8) {
    return(c(low = NA_real_, high = NA_real_))
  }

  vals <- replicate(B, {
    idx <- sample.int(n, size = n, replace = TRUE)
    suppressWarnings(
      cor(d$x[idx], d$y[idx], method = method, use = "complete.obs")
    )
  })

  vals <- vals[is.finite(vals)]

  if (length(vals) < 10) {
    return(c(low = NA_real_, high = NA_real_))
  }

  as.numeric(quantile(vals, probs = c(.025, .975), na.rm = TRUE)) |>
    setNames(c("low", "high"))
}

pairwise_cor_table <- function(data, vars, analysis_label, B = 2000) {
  pairs <- combn(vars, 2, simplify = FALSE)

  map_dfr(pairs, function(pair) {
    d <- data |>
      select(all_of(pair)) |>
      drop_na()

    n <- nrow(d)

    if (n < 4) {
      return(tibble(
        analysis = analysis_label,
        var1 = pair[1],
        var2 = pair[2],
        n = n,
        pearson_r = NA_real_,
        pearson_low = NA_real_,
        pearson_high = NA_real_,
        spearman_rho = NA_real_,
        spearman_low = NA_real_,
        spearman_high = NA_real_
      ))
    }

    pear <- suppressWarnings(
      cor.test(d[[pair[1]]], d[[pair[2]]], method = "pearson")
    )

    rho <- suppressWarnings(
      cor(d[[pair[1]]], d[[pair[2]]], method = "spearman")
    )

    rho_ci <- boot_ci_cor(
      x = d[[pair[1]]],
      y = d[[pair[2]]],
      method = "spearman",
      B = B
    )

    tibble(
      analysis = analysis_label,
      var1 = pair[1],
      var2 = pair[2],
      n = n,
      pearson_r = unname(pear$estimate),
      pearson_low = pear$conf.int[1],
      pearson_high = pear$conf.int[2],
      spearman_rho = rho,
      spearman_low = rho_ci["low"],
      spearman_high = rho_ci["high"]
    )
  })
}

icc_table <- function(data, vars, analysis_label) {
  mat <- data |>
    select(all_of(vars)) |>
    drop_na()

  if (nrow(mat) < 4) {
    return(tibble(
      analysis = analysis_label,
      n = nrow(mat),
      type = NA_character_,
      ICC = NA_real_,
      lower = NA_real_,
      upper = NA_real_,
      note = "Too few complete rows"
    ))
  }

  icc_res <- psych::ICC(as.data.frame(mat), lmer = FALSE)$results |>
    as_tibble()

  icc_res |>
    filter(type %in% c("ICC2", "ICC2k", "ICC3", "ICC3k")) |>
    transmute(
      analysis = analysis_label,
      n = nrow(mat),
      type,
      ICC,
      lower = `lower bound`,
      upper = `upper bound`,
      note = case_when(
        type == "ICC2" ~ "single vowel, absolute agreement",
        type == "ICC2k" ~ "mean of 3 vowels, absolute agreement",
        type == "ICC3" ~ "single vowel, consistency",
        type == "ICC3k" ~
          "mean of 3 vowels, consistency; primary reliability estimate for the composite",
        TRUE ~ NA_character_
      )
    )
}

# ----------------------------
# 3) DESCRIPTIVES
# ----------------------------

df_long <- df_voice |>
  pivot_longer(
    cols = c(f0_mean_a, f0_mean_i, f0_mean_u),
    names_to = "vowel",
    values_to = "f0"
  ) |>
  mutate(
    vowel = recode(
      vowel,
      f0_mean_a = "/a/",
      f0_mean_i = "/i/",
      f0_mean_u = "/u/"
    ),
    vowel = factor(vowel, levels = c("/a/", "/i/", "/u/")),
    obs_id = interaction(ID, timepoint, drop = TRUE)
  ) |>
  filter(!is.na(f0))

f0_descriptives <- df_long |>
  group_by(timepoint, vowel) |>
  summarise(
    n = n(),
    n_subj = n_distinct(ID),
    mean = mean(f0, na.rm = TRUE),
    sd = sd(f0, na.rm = TRUE),
    median = median(f0, na.rm = TRUE),
    q25 = quantile(f0, .25, na.rm = TRUE),
    q75 = quantile(f0, .75, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(
  f0_descriptives,
  file = file.path(out_dir, "f0_cross_vowel_descriptives.csv"),
  row.names = FALSE
)

# ----------------------------
# 4) PAIRWISE CORRELATIONS: ABSOLUTE F0 VALUES
# ----------------------------

set.seed(123)

f0_vars <- c("f0_mean_a", "f0_mean_i", "f0_mean_u")

abs_cor_by_time <- df_voice |>
  group_by(timepoint) |>
  group_modify(
    ~ pairwise_cor_table(
      data = .x,
      vars = f0_vars,
      analysis_label = paste0("absolute_F0_", unique(.x$timepoint)),
      B = 2000
    )
  ) |>
  ungroup()

abs_cor_all <- pairwise_cor_table(
  data = df_voice,
  vars = f0_vars,
  analysis_label = "absolute_F0_all_timepoints_pooled",
  B = 2000
)

abs_cor_results <- bind_rows(abs_cor_by_time, abs_cor_all)

write.csv(
  abs_cor_results,
  file = file.path(out_dir, "f0_cross_vowel_absolute_correlations.csv"),
  row.names = FALSE
)

# ----------------------------
# 5) CHANGE SCORES: STRESS AND RECOVERY
# ----------------------------

df_delta <- df_voice |>
  select(ID, timepoint, f0_mean_a, f0_mean_i, f0_mean_u) |>
  pivot_wider(
    names_from = timepoint,
    values_from = c(f0_mean_a, f0_mean_i, f0_mean_u)
  ) |>
  mutate(
    d_stress_a = f0_mean_a_pre - f0_mean_a_baseline,
    d_stress_i = f0_mean_i_pre - f0_mean_i_baseline,
    d_stress_u = f0_mean_u_pre - f0_mean_u_baseline,

    d_recovery_a = f0_mean_a_post - f0_mean_a_pre,
    d_recovery_i = f0_mean_i_post - f0_mean_i_pre,
    d_recovery_u = f0_mean_u_post - f0_mean_u_pre,

    d_total_a = f0_mean_a_post - f0_mean_a_baseline,
    d_total_i = f0_mean_i_post - f0_mean_i_baseline,
    d_total_u = f0_mean_u_post - f0_mean_u_baseline
  )

write.csv(
  df_delta,
  file = file.path(out_dir, "f0_cross_vowel_change_scores.csv"),
  row.names = FALSE
)

stress_cor <- pairwise_cor_table(
  data = df_delta,
  vars = c("d_stress_a", "d_stress_i", "d_stress_u"),
  analysis_label = "delta_stress_PRE_minus_BASELINE",
  B = 2000
)

recovery_cor <- pairwise_cor_table(
  data = df_delta,
  vars = c("d_recovery_a", "d_recovery_i", "d_recovery_u"),
  analysis_label = "delta_recovery_POST_minus_PRE",
  B = 2000
)

total_cor <- pairwise_cor_table(
  data = df_delta,
  vars = c("d_total_a", "d_total_i", "d_total_u"),
  analysis_label = "delta_total_POST_minus_BASELINE",
  B = 2000
)

change_cor_results <- bind_rows(stress_cor, recovery_cor, total_cor)

write.csv(
  change_cor_results,
  file = file.path(out_dir, "f0_cross_vowel_change_correlations.csv"),
  row.names = FALSE
)

# ----------------------------
# 6) ICC / RELIABILITY OF THE CROSS-VOWEL COMPOSITE
# ----------------------------

icc_all_absolute <- icc_table(
  data = df_voice,
  vars = f0_vars,
  analysis_label = "absolute_F0_all_timepoints"
)

icc_by_time <- df_voice |>
  group_by(timepoint) |>
  group_modify(
    ~ icc_table(
      data = .x,
      vars = f0_vars,
      analysis_label = paste0("absolute_F0_", unique(.x$timepoint))
    )
  ) |>
  ungroup()

icc_stress <- icc_table(
  data = df_delta,
  vars = c("d_stress_a", "d_stress_i", "d_stress_u"),
  analysis_label = "delta_stress_PRE_minus_BASELINE"
)

icc_recovery <- icc_table(
  data = df_delta,
  vars = c("d_recovery_a", "d_recovery_i", "d_recovery_u"),
  analysis_label = "delta_recovery_POST_minus_PRE"
)

icc_total <- icc_table(
  data = df_delta,
  vars = c("d_total_a", "d_total_i", "d_total_u"),
  analysis_label = "delta_total_POST_minus_BASELINE"
)

icc_results <- bind_rows(
  icc_all_absolute,
  icc_by_time,
  icc_stress,
  icc_recovery,
  icc_total
)

write.csv(
  icc_results,
  file = file.path(out_dir, "f0_cross_vowel_icc.csv"),
  row.names = FALSE
)

# ----------------------------
# 7) MIXED MODEL: DOES THE TIME EFFECT DIFFER BY VOWEL?
# ----------------------------
# Questo è un controllo omnibus.
# Se l'interazione c1_stress:vowel e c2_recovery:vowel è debole/non necessaria,
# allora il pattern temporale F0 è coerente tra vocali.

m0 <- lmer(
  f0 ~ c1_stress + c2_recovery + vowel + (1 | ID) + (1 | obs_id),
  data = df_long,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa")
)

m1 <- lmer(
  f0 ~ (c1_stress + c2_recovery) * vowel + (1 | ID) + (1 | obs_id),
  data = df_long,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa")
)

model_lrt <- anova(m0, m1) |>
  as.data.frame() |>
  rownames_to_column("model")

write.csv(
  model_lrt,
  file = file.path(out_dir, "f0_vowel_time_interaction_lrt.csv"),
  row.names = FALSE
)

fixed_effects_m1 <- broom.mixed::tidy(
  m1,
  effects = "fixed",
  conf.int = TRUE
)

write.csv(
  fixed_effects_m1,
  file = file.path(out_dir, "f0_vowel_time_interaction_fixed_effects.csv"),
  row.names = FALSE
)

# Optional: se emmeans è installato, stimiamo gli effetti stress/recovery
# separatamente per ciascuna vocale.

if (requireNamespace("emmeans", quietly = TRUE)) {
  stress_slopes <- emmeans::emtrends(
    m1,
    specs = ~vowel,
    var = "c1_stress"
  ) |>
    as.data.frame() |>
    mutate(effect = "stress_PRE_minus_BASELINE")

  recovery_slopes <- emmeans::emtrends(
    m1,
    specs = ~vowel,
    var = "c2_recovery"
  ) |>
    as.data.frame() |>
    mutate(effect = "recovery_POST_minus_PRE")

  vowel_specific_slopes <- bind_rows(stress_slopes, recovery_slopes)

  write.csv(
    vowel_specific_slopes,
    file = file.path(out_dir, "f0_vowel_specific_stress_recovery_slopes.csv"),
    row.names = FALSE
  )
}

# ----------------------------
# 8) SUMMARY TABLES FOR MANUSCRIPT/RESPONSE LETTER
# ----------------------------

# Sintesi compatta delle correlazioni assolute
abs_cor_summary <- abs_cor_results |>
  group_by(analysis) |>
  summarise(
    n_min = min(n, na.rm = TRUE),
    n_max = max(n, na.rm = TRUE),
    mean_spearman_rho = mean(spearman_rho, na.rm = TRUE),
    min_spearman_rho = min(spearman_rho, na.rm = TRUE),
    max_spearman_rho = max(spearman_rho, na.rm = TRUE),
    .groups = "drop"
  )

# Sintesi compatta delle correlazioni dei cambiamenti
change_cor_summary <- change_cor_results |>
  group_by(analysis) |>
  summarise(
    n_min = min(n, na.rm = TRUE),
    n_max = max(n, na.rm = TRUE),
    mean_spearman_rho = mean(spearman_rho, na.rm = TRUE),
    min_spearman_rho = min(spearman_rho, na.rm = TRUE),
    max_spearman_rho = max(spearman_rho, na.rm = TRUE),
    .groups = "drop"
  )

# ICC primario: ICC3k, cioè consistency del composito medio
icc_primary <- icc_results |>
  filter(type == "ICC3k") |>
  select(analysis, n, ICC, lower, upper, note)

write.csv(
  abs_cor_summary,
  file = file.path(out_dir, "f0_absolute_correlation_summary.csv"),
  row.names = FALSE
)

write.csv(
  change_cor_summary,
  file = file.path(out_dir, "f0_change_correlation_summary.csv"),
  row.names = FALSE
)

write.csv(
  icc_primary,
  file = file.path(out_dir, "f0_primary_icc3k_summary.csv"),
  row.names = FALSE
)

# ----------------------------
# 9) PLOT: MEAN F0 BY VOWEL AND TIMEPOINT
# ----------------------------

plot_data <- df_long |>
  group_by(timepoint, vowel) |>
  summarise(
    n = n(),
    mean_f0 = mean(f0, na.rm = TRUE),
    se_f0 = sd(f0, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

p <- ggplot(
  plot_data,
  aes(
    x = timepoint,
    y = mean_f0,
    group = vowel,
    linetype = vowel,
    shape = vowel
  )
) +
  geom_line() +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(ymin = mean_f0 - se_f0, ymax = mean_f0 + se_f0),
    width = 0.08
  ) +
  labs(
    x = "Timepoint",
    y = "Mean F0 (Hz)",
    linetype = "Vowel",
    shape = "Vowel",
    title = "Cross-vowel F0 pattern across timepoints"
  ) +
  theme_classic()

ggsave(
  filename = file.path(out_dir, "f0_cross_vowel_timepoint_plot.png"),
  plot = p,
  width = 7,
  height = 5,
  dpi = 300
)

# ----------------------------
# 10) CONSOLE OUTPUT
# ----------------------------

cat("\n=== ABSOLUTE F0 CORRELATION SUMMARY ===\n")
print(abs_cor_summary)

cat("\n=== CHANGE-SCORE F0 CORRELATION SUMMARY ===\n")
print(change_cor_summary)

cat("\n=== PRIMARY ICC3k SUMMARY ===\n")
print(icc_primary)

cat("\n=== TIMEPOINT x VOWEL MODEL COMPARISON ===\n")
print(model_lrt)

cat("\nSaved outputs in:\n", out_dir, "\n", sep = "")

# eof
