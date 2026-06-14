# ==============================================================================
# 01b_nne_cross_vowel_concordance.R
#
# Analisi di concordanza tra le tre vocali per NNE:
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

out_dir <- here("results", "NNE", "cross_vowel")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# 1) LOAD VOICE DATA
# ----------------------------

baseline <- read_excel(voice_path, sheet = "BASELINE") %>%
  mutate(timepoint = "baseline")

pre <- read_excel(voice_path, sheet = "PRE") %>%
  mutate(timepoint = "pre")

post <- read_excel(voice_path, sheet = "POST") %>%
  mutate(timepoint = "post")

df_voice_raw <- bind_rows(baseline, pre, post)
names(df_voice_raw) <- stringr::str_trim(names(df_voice_raw))

# ----------------------------
# 1b) IDENTIFY NNE COLUMNS
# ----------------------------
# Questo blocco cerca colonne che contengano "NNE" e il marker della vocale.
# Se fallisce, stampa i candidati disponibili.

find_acoustic_col <- function(data, feature = "NNE", vowel = "/a/") {
  nm <- names(data)

  candidates <- nm[
    stringr::str_detect(
      stringr::str_to_lower(nm),
      stringr::str_to_lower(feature)
    ) &
      stringr::str_detect(nm, stringr::fixed(vowel))
  ]

  if (length(candidates) != 1) {
    message(
      "\nCould not uniquely identify column for feature = ",
      feature,
      ", vowel = ",
      vowel
    )
    message("\nCandidate NNE columns found in the file:")
    print(nm[stringr::str_detect(stringr::str_to_lower(nm), "nne")])

    stop(
      "Expected exactly one column for ",
      feature,
      " ",
      vowel,
      ", but found ",
      length(candidates),
      "."
    )
  }

  candidates
}

nne_col_a <- find_acoustic_col(df_voice_raw, feature = "NNE", vowel = "/a/")
nne_col_i <- find_acoustic_col(df_voice_raw, feature = "NNE", vowel = "/i/")
nne_col_u <- find_acoustic_col(df_voice_raw, feature = "NNE", vowel = "/u/")

message("\nUsing NNE columns:")
message(" /a/: ", nne_col_a)
message(" /i/: ", nne_col_i)
message(" /u/: ", nne_col_u)

# ----------------------------
# 2) CLEAN AND KEEP SINGLE-VOWEL NNE VALUES
# ----------------------------

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

    nne_a = as.numeric(.data[[nne_col_a]]),
    nne_i = as.numeric(.data[[nne_col_i]]),
    nne_u = as.numeric(.data[[nne_col_u]])
  ) |>
  filter(!is.na(ID)) |>
  mutate(
    n_vowels_observed = rowSums(!is.na(across(c(nne_a, nne_i, nne_u)))),
    y_nne = if_else(
      n_vowels_observed > 0,
      rowMeans(across(c(nne_a, nne_i, nne_u)), na.rm = TRUE),
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
# se esiste il bundle Stan del modello principale NNE, restringiamo l'analisi
# agli stessi soggetti usati nell'analisi principale.

bundle_path <- here("results", "NNE", "data", "stan_bundle_nnemean_pid5.rds")

if (file.exists(bundle_path)) {
  bundle <- readRDS(bundle_path)
  main_ids <- unique(bundle$df_voice$ID)

  df_voice <- df_voice |>
    filter(ID %in% main_ids)

  message(
    "Analisi ristretta agli ID del modello NNE principale: ",
    n_distinct(df_voice$ID),
    " soggetti."
  )
} else {
  message(
    "Bundle Stan NNE non trovato. Uso tutti i soggetti disponibili in AUDIO.xlsx: ",
    n_distinct(df_voice$ID),
    " soggetti."
  )
}

write.csv(
  df_voice,
  file = file.path(out_dir, "nne_cross_vowel_input_data.csv"),
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
# 3) HELPER FUNCTIONS
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
# 4) DESCRIPTIVES
# ----------------------------

nne_vars <- c("nne_a", "nne_i", "nne_u")

df_long <- df_voice |>
  pivot_longer(
    cols = all_of(nne_vars),
    names_to = "vowel",
    values_to = "nne"
  ) |>
  mutate(
    vowel = recode(
      vowel,
      nne_a = "/a/",
      nne_i = "/i/",
      nne_u = "/u/"
    ),
    vowel = factor(vowel, levels = c("/a/", "/i/", "/u/")),
    obs_id = interaction(ID, timepoint, drop = TRUE)
  ) |>
  filter(!is.na(nne))

nne_descriptives <- df_long |>
  group_by(timepoint, vowel) |>
  summarise(
    n = n(),
    n_subj = n_distinct(ID),
    mean = mean(nne, na.rm = TRUE),
    sd = sd(nne, na.rm = TRUE),
    median = median(nne, na.rm = TRUE),
    q25 = quantile(nne, .25, na.rm = TRUE),
    q75 = quantile(nne, .75, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(
  nne_descriptives,
  file = file.path(out_dir, "nne_cross_vowel_descriptives.csv"),
  row.names = FALSE
)

# ----------------------------
# 5) PAIRWISE CORRELATIONS: ABSOLUTE NNE VALUES
# ----------------------------

set.seed(123)

abs_cor_by_time <- df_voice |>
  group_by(timepoint) |>
  group_modify(
    ~ pairwise_cor_table(
      data = .x,
      vars = nne_vars,
      analysis_label = paste0("absolute_NNE_", as.character(.y$timepoint)),
      B = 2000
    )
  ) |>
  ungroup()

abs_cor_all <- pairwise_cor_table(
  data = df_voice,
  vars = nne_vars,
  analysis_label = "absolute_NNE_all_timepoints_pooled",
  B = 2000
)

abs_cor_results <- bind_rows(abs_cor_by_time, abs_cor_all)

write.csv(
  abs_cor_results,
  file = file.path(out_dir, "nne_cross_vowel_absolute_correlations.csv"),
  row.names = FALSE
)

# ----------------------------
# 6) CHANGE SCORES: STRESS AND RECOVERY
# ----------------------------

df_delta <- df_voice |>
  select(ID, timepoint, nne_a, nne_i, nne_u) |>
  pivot_wider(
    names_from = timepoint,
    values_from = c(nne_a, nne_i, nne_u)
  ) |>
  mutate(
    d_stress_a = nne_a_pre - nne_a_baseline,
    d_stress_i = nne_i_pre - nne_i_baseline,
    d_stress_u = nne_u_pre - nne_u_baseline,

    d_recovery_a = nne_a_post - nne_a_pre,
    d_recovery_i = nne_i_post - nne_i_pre,
    d_recovery_u = nne_u_post - nne_u_pre,

    d_total_a = nne_a_post - nne_a_baseline,
    d_total_i = nne_i_post - nne_i_baseline,
    d_total_u = nne_u_post - nne_u_baseline
  )

write.csv(
  df_delta,
  file = file.path(out_dir, "nne_cross_vowel_change_scores.csv"),
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
  file = file.path(out_dir, "nne_cross_vowel_change_correlations.csv"),
  row.names = FALSE
)

# ----------------------------
# 7) ICC / RELIABILITY OF THE CROSS-VOWEL COMPOSITE
# ----------------------------

icc_all_absolute <- icc_table(
  data = df_voice,
  vars = nne_vars,
  analysis_label = "absolute_NNE_all_timepoints"
)

icc_by_time <- df_voice |>
  group_by(timepoint) |>
  group_modify(
    ~ icc_table(
      data = .x,
      vars = nne_vars,
      analysis_label = paste0("absolute_NNE_", as.character(.y$timepoint))
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
  file = file.path(out_dir, "nne_cross_vowel_icc.csv"),
  row.names = FALSE
)

# ----------------------------
# 8) MIXED MODEL: DOES THE TIME EFFECT DIFFER BY VOWEL?
# ----------------------------
# Questo è un controllo omnibus.
# Se l'interazione c1_stress:vowel e c2_recovery:vowel è debole/non necessaria,
# allora il pattern temporale NNE è coerente tra vocali.

m0 <- lmer(
  nne ~ c1_stress + c2_recovery + vowel + (1 | ID) + (1 | obs_id),
  data = df_long,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa")
)

m1 <- lmer(
  nne ~ (c1_stress + c2_recovery) * vowel + (1 | ID) + (1 | obs_id),
  data = df_long,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa")
)

model_lrt <- anova(m0, m1) |>
  as.data.frame() |>
  rownames_to_column("model")

write.csv(
  model_lrt,
  file = file.path(out_dir, "nne_vowel_time_interaction_lrt.csv"),
  row.names = FALSE
)

fixed_effects_m1 <- broom.mixed::tidy(
  m1,
  effects = "fixed",
  conf.int = TRUE
)

write.csv(
  fixed_effects_m1,
  file = file.path(out_dir, "nne_vowel_time_interaction_fixed_effects.csv"),
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
    file = file.path(out_dir, "nne_vowel_specific_stress_recovery_slopes.csv"),
    row.names = FALSE
  )
}

# ----------------------------
# 9) SUMMARY TABLES FOR MANUSCRIPT/RESPONSE LETTER
# ----------------------------

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

icc_primary <- icc_results |>
  filter(type == "ICC3k") |>
  select(analysis, n, ICC, lower, upper, note)

write.csv(
  abs_cor_summary,
  file = file.path(out_dir, "nne_absolute_correlation_summary.csv"),
  row.names = FALSE
)

write.csv(
  change_cor_summary,
  file = file.path(out_dir, "nne_change_correlation_summary.csv"),
  row.names = FALSE
)

write.csv(
  icc_primary,
  file = file.path(out_dir, "nne_primary_icc3k_summary.csv"),
  row.names = FALSE
)

# ----------------------------
# 10) PLOT: MEAN NNE BY VOWEL AND TIMEPOINT
# ----------------------------

plot_data <- df_long |>
  group_by(timepoint, vowel) |>
  summarise(
    n = n(),
    mean_nne = mean(nne, na.rm = TRUE),
    se_nne = sd(nne, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

p <- ggplot(
  plot_data,
  aes(
    x = timepoint,
    y = mean_nne,
    group = vowel,
    linetype = vowel,
    shape = vowel
  )
) +
  geom_line() +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(ymin = mean_nne - se_nne, ymax = mean_nne + se_nne),
    width = 0.08
  ) +
  labs(
    x = "Timepoint",
    y = "Mean NNE",
    linetype = "Vowel",
    shape = "Vowel",
    title = "Cross-vowel NNE pattern across timepoints"
  ) +
  theme_classic()

ggsave(
  filename = file.path(out_dir, "nne_cross_vowel_timepoint_plot.png"),
  plot = p,
  width = 7,
  height = 5,
  dpi = 300
)

# ----------------------------
# 11) CONSOLE OUTPUT
# ----------------------------

cat("\n=== ABSOLUTE NNE CORRELATION SUMMARY ===\n")
print(abs_cor_summary)

cat("\n=== CHANGE-SCORE NNE CORRELATION SUMMARY ===\n")
print(change_cor_summary)

cat("\n=== PRIMARY ICC3k SUMMARY ===\n")
print(icc_primary)

cat("\n=== TIMEPOINT x VOWEL MODEL COMPARISON ===\n")
print(model_lrt)

cat("\nSaved outputs in:\n", out_dir, "\n", sep = "")

# eof
