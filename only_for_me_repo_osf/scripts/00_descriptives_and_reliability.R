# =============================================================================
# 00_descriptives_and_reliability.R
#
# Produces:
#   (A) Baseline PID-5 reliability (alpha, omega per domain) — Table S1
#   (B) EMA multilevel reliability (alpha_2L, alpha_B, alpha_W,
#       omega_2L, omega_B, omega_W) — Table S2
#   (C) Convergent validity: EMA domain means vs baseline domain scores
#   (D) Voice outcome descriptives by assessment period — Tables S5/S6
#   (E) EMA PID-5 domain descriptives (between/within person) — Tables S7/S8
#
# Inputs:
#   data/processed/pid5_required_columns.csv
#   data/processed/audio_required_columns.csv
#
# Note on Section A: baseline item-level data (220-item PID-5) are not in the
#   public repository. The expected output is hard-coded; 
#
# Note on Section B: the live computation requires the 15 individual EMA item
#   scores (pid5_1--pid5_15). If absent from pid5_required_columns.csv the
#   pre-computed estimates from the manuscript are printed instead.
#
# Outputs (results/descriptives/):
#   table_s1_baseline_reliability.csv
#   table_s2_ema_multilevel_reliability.csv
#   table_s3_convergent_validity.csv
#   table_s5_voice_descriptives.csv
#   table_s6_voice_wp_variability.csv
#   table_s7_ema_between_descriptives.csv
#   table_s8_ema_within_descriptives.csv
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(psych)
  library(lavaan)
  library(semTools)
  library(here)
})
# MASS is called as MASS::mvrnorm() to avoid masking dplyr::select()

dir.create(
  here("results", "descriptives"),
  recursive = TRUE,
  showWarnings = FALSE
)

ema_path <- here("data", "processed", "pid5_required_columns.csv")
voice_path <- here("data", "processed", "audio_required_columns.csv")
stopifnot(file.exists(ema_path), file.exists(voice_path))

ema <- read_csv(ema_path, show_col_types = FALSE)
voice <- read_csv(voice_path, show_col_types = FALSE)

pid5_ema_vars <- c(
  "pid5_negative_affectivity",
  "pid5_detachment",
  "pid5_antagonism",
  "pid5_disinhibition",
  "pid5_psychoticism"
)
pid5_bl_vars <- c(
  "domain_negative_affect_baseline",
  "domain_detachment_baseline",
  "domain_antagonism_baseline",
  "domain_disinhibition_baseline",
  "domain_psychoticism_baseline"
)
domain_labels <- c(
  "Negative Affectivity",
  "Detachment",
  "Antagonism",
  "Disinhibition",
  "Psychoticism"
)

# =============================================================================
# A. Baseline PID-5 reliability (Table S1)
# =============================================================================
# These values are hard-coded because the computation requires the 220-item
# PID-5 questionnaire item-level responses, which are not included in the
# public repository. The estimates below are the values reported in Table S1
# of the manuscript. 
table_s1 <- tibble(
  domain = c(domain_labels, "Total PID-5"),
  n_items = c(23, 22, 21, 22, 33, 220),
  alpha_full = c(0.783, 0.754, 0.670, 0.747, 0.870, 0.965),
  omega_full = c(0.851, 0.844, 0.819, 0.843, 0.898, 0.979),
  alpha_clean = c(0.767, 0.741, 0.627, 0.725, 0.864, 0.964),
  omega_clean = c(0.843, 0.840, 0.796, 0.824, 0.892, 0.977)
)

print(table_s1)
write_csv(
  table_s1,
  here("results", "descriptives", "table_s1_baseline_reliability.csv")
)
cat("Saved: table_s1_baseline_reliability.csv\n\n")

# =============================================================================
# B. EMA multilevel reliability (Table S2)
# =============================================================================
pid5_items <- list(
  Negative_Affectivity = c("pid5_2", "pid5_11", "pid5_13"),
  Detachment = c("pid5_15", "pid5_3", "pid5_7"),
  Antagonism = c("pid5_14", "pid5_6", "pid5_4"),
  Disinhibition = c("pid5_12", "pid5_1", "pid5_9"),
  Psychoticism = c("pid5_5", "pid5_8", "pid5_10")
)
all_items <- unlist(pid5_items)
items_present <- all(all_items %in% names(ema))

if (!items_present) {
  cat(
    "NOTE: Item columns (pid5_1--pid5_15) not found in",
    "pid5_required_columns.csv.\n"
  )
  cat("      Using pre-computed estimates from the manuscript.\n\n")
  
  # These values are hard-coded because the computation requires the 15
  # individual EMA item scores (pid5_1--pid5_15), which are not included in
  # pid5_required_columns.csv (only the 5 domain composites are published).
  # The estimates below are the values reported in Table S2 of the manuscript.
  table_s2 <- tibble(
    index = c(
      "alpha_2L",
      "alpha_B",
      "alpha_W",
      "omega_2L",
      "omega_B",
      "omega_W"
    ),
    estimate = c(0.826, 0.866, 0.727, 0.814, 0.852, 0.724),
    ci_lower = c(0.802, 0.835, 0.687, NA, NA, NA),
    ci_upper = c(0.845, 0.889, 0.759, NA, NA, NA)
  )
} else {
  multilevel_alpha <- function(
    data,
    id,
    nsim = 5000,
    conf_level = .95,
    se = "robust.huber.white"
  ) {
    nitem <- ncol(data)
    ynames <- paste0("y", seq_len(nitem))
    colnames(data) <- ynames
    data <- cbind(data, id = id)
    tab_id <- table(id)
    hmean_cs <- 1 / mean(1 / tab_id[tab_id > 0])
    sat_syntax <- (function(y) {
      if (length(y) <= 1) return(NULL)
      paste(
        c(paste(y[1], "~~", paste(y[-1], collapse = " + ")), Recall(y[-1])),
        collapse = "\n  "
      )
    })(ynames)
    msat <- paste0("level: 1\n  ", sat_syntax, "\nlevel: 2\n  ", sat_syntax)
    msat_fit <- cfa(
      msat,
      data = data,
      cluster = "id",
      se = se,
      test = "none",
      h1 = FALSE,
      baseline = FALSE
    )
    coef_msat <- coef(msat_fit, type = "user")
    vcov_msat <- vcov(msat_fit)
    vw <- names(coef_msat)[with(
      msat_fit@ParTable,
      which(op == "~~" & lhs == rhs & level == 1)
    )]
    cvw <- names(coef_msat)[with(
      msat_fit@ParTable,
      which(op == "~~" & lhs != rhs & level == 1)
    )]
    vb <- names(coef_msat)[with(
      msat_fit@ParTable,
      which(op == "~~" & lhs == rhs & level == 2)
    )]
    cvb <- names(coef_msat)[with(
      msat_fit@ParTable,
      which(op == "~~" & lhs != rhs & level == 2)
    )]
    Sw <- sum(coef_msat[vw], 2 * coef_msat[cvw])
    Sb <- sum(coef_msat[vb], 2 * coef_msat[cvb])
    k <- nitem / (nitem - 1)
    alphaw <- k * sum(2 * coef_msat[cvw]) / Sw
    alpha2l <- k * sum(2 * coef_msat[c(cvw, cvb)]) / (Sb + Sw)
    alphab <- k * sum(2 * coef_msat[cvb]) / (Sb + Sw / hmean_cs)
    sim <- MASS::mvrnorm(
      nsim,
      mu = coef_msat[c(vw, vb, cvw, cvb)],
      Sigma = vcov_msat[c(vw, vb, cvw, cvb), c(vw, vb, cvw, cvb)]
    )
    sim_Sw <- rowSums(cbind(sim[, vw], 2 * sim[, cvw]))
    sim_Sb <- rowSums(cbind(sim[, vb], 2 * sim[, cvb]))
    sim_aw <- k * rowSums(2 * sim[, cvw]) / sim_Sw
    sim_a2l <- k * rowSums(2 * sim[, c(cvw, cvb)]) / (sim_Sb + sim_Sw)
    sim_ab <- k * rowSums(2 * sim[, cvb]) / (sim_Sb + sim_Sw / hmean_cs)
    ci <- lapply(
      list(alpha2l = sim_a2l, alphab = sim_ab, alphaw = sim_aw),
      quantile,
      probs = .5 + c(-conf_level, conf_level) / 2
    )
    list(
      alpha = c(alpha2l = alpha2l, alphab = alphab, alphaw = alphaw),
      alpha_ci = do.call(rbind, ci)
    )
  }
  # Participants who showed careless responding (insufficient within-subject
  # variability, atypical response patterns) were excluded during preprocessing;
  # they are not present in pid5_required_columns.csv. No further exclusion
  # is needed here.
  ema_items <- ema |>
    dplyr::select(user_id, all_of(all_items)) |>
    filter(if_all(all_of(all_items), ~ !is.na(.x)))
  
  colnames(ema_items)[2:16] <- paste0("y", 1:15)
  
  rel <- multilevel_alpha(
    data = ema_items[paste0("y", 1:15)],
    id = ema_items$user_id
  )
  
  table_s2 <- tibble(
    index = names(rel$alpha),
    estimate = round(rel$alpha, 3),
    ci_lower = round(rel$alpha_ci[, 1], 3),
    ci_upper = round(rel$alpha_ci[, 2], 3)
  )
  cat("EMA multilevel reliability computed from item-level data.\n")
}

print(table_s2)
write_csv(
  table_s2,
  here("results", "descriptives", "table_s2_ema_multilevel_reliability.csv")
)
cat("Saved: table_s2_ema_multilevel_reliability.csv\n\n")

# =============================================================================
# C. Convergent validity: EMA domain means vs baseline domain scores
# =============================================================================
ema_person <- ema |>
  group_by(user_id) |>
  summarise(
    across(all_of(pid5_ema_vars), \(x) mean(x, na.rm = TRUE)),
    .groups = "drop"
  )

bl_person <- ema |>
  dplyr::select(user_id, all_of(pid5_bl_vars)) |>
  distinct(user_id, .keep_all = TRUE) |>
  filter(!is.na(domain_negative_affect_baseline))

conv_data <- inner_join(ema_person, bl_person, by = "user_id")

conv_validity <- map_dfr(seq_along(pid5_ema_vars), function(d) {
  r <- cor(
    conv_data[[pid5_ema_vars[d]]],
    conv_data[[pid5_bl_vars[d]]],
    use = "complete.obs"
  )
  tibble(domain = domain_labels[d], r_ema_baseline = round(r, 3))
})

print(conv_validity)
write_csv(
  conv_validity,
  here("results", "descriptives", "table_s3_convergent_validity.csv")
)
cat("Saved: table_s3_convergent_validity.csv\n\n")

# =============================================================================
# D. Voice outcome descriptives by assessment period (Tables S5/S6)
# =============================================================================
voice_aug <- voice |>
  mutate(
    f0_mean = rowMeans(
      across(c(`F0 mean Hz /a/`, `F0 mean Hz /i/`, `F0 mean Hz /u/`)),
      na.rm = TRUE
    ),
    nne_mean = rowMeans(
      across(c(`NNE /a/`, `NNE /i/`, `NNE /u/`)),
      na.rm = TRUE
    )
  )

# Table S5: means and SDs by timepoint
voice_desc <- voice_aug |>
  group_by(timepoint) |>
  summarise(
    n = n(),
    f0_M = round(mean(f0_mean, na.rm = TRUE), 2),
    f0_SD = round(sd(f0_mean, na.rm = TRUE), 2),
    nne_M = round(mean(nne_mean, na.rm = TRUE), 2),
    nne_SD = round(sd(nne_mean, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  mutate(
    timepoint = factor(
      tolower(timepoint),
      levels = c("baseline", "pre", "post")
    )
  ) |>
  arrange(timepoint)

print(voice_desc)
write_csv(
  voice_desc,
  here("results", "descriptives", "table_s5_voice_descriptives.csv")
)
cat("Saved: table_s5_voice_descriptives.csv\n\n")

# Table S6: within-person variability across timepoints
voice_wp_sd <- voice_aug |>
  group_by(ID) |>
  summarise(
    f0_wp_sd = sd(f0_mean, na.rm = TRUE),
    nne_wp_sd = sd(nne_mean, na.rm = TRUE),
    .groups = "drop"
  )

voice_wp_summary <- tibble(
  variable = c("F0 within-person SD (Hz)", "NNE within-person SD (dB)"),
  n = nrow(voice_wp_sd),
  mean_SD = round(
    c(
      mean(voice_wp_sd$f0_wp_sd, na.rm = TRUE),
      mean(voice_wp_sd$nne_wp_sd, na.rm = TRUE)
    ),
    2
  ),
  sd_of_SD = round(
    c(
      sd(voice_wp_sd$f0_wp_sd, na.rm = TRUE),
      sd(voice_wp_sd$nne_wp_sd, na.rm = TRUE)
    ),
    2
  ),
  min_SD = round(
    c(
      min(voice_wp_sd$f0_wp_sd, na.rm = TRUE),
      min(voice_wp_sd$nne_wp_sd, na.rm = TRUE)
    ),
    2
  ),
  max_SD = round(
    c(
      max(voice_wp_sd$f0_wp_sd, na.rm = TRUE),
      max(voice_wp_sd$nne_wp_sd, na.rm = TRUE)
    ),
    2
  )
)

print(voice_wp_summary)
write_csv(
  voice_wp_summary,
  here("results", "descriptives", "table_s6_voice_wp_variability.csv")
)
cat("Saved: table_s6_voice_wp_variability.csv\n\n")

# =============================================================================
# E. EMA PID-5 domain descriptives (Tables S7/S8)
# =============================================================================

# Restrict to the 119 participants in the final analysis sample
ema_119 <- ema |> filter(user_id %in% unique(voice$ID))

# Table S7: between-person descriptives (person-level means)
person_means <- ema_119 |>
  group_by(user_id) |>
  summarise(
    across(all_of(pid5_ema_vars), \(x) mean(x, na.rm = TRUE)),
    .groups = "drop"
  )

pm_mat <- as.matrix(person_means[pid5_ema_vars])

table_s7 <- tibble(
  domain = domain_labels,
  n = colSums(!is.na(pm_mat)),
  M = round(colMeans(pm_mat, na.rm = TRUE), 2),
  SD = round(apply(pm_mat, 2, sd, na.rm = TRUE), 2),
  Min = round(apply(pm_mat, 2, min, na.rm = TRUE), 2),
  Max = round(apply(pm_mat, 2, max, na.rm = TRUE), 2)
)

print(table_s7)
write_csv(
  table_s7,
  here("results", "descriptives", "table_s7_ema_between_descriptives.csv")
)
cat("Saved: table_s7_ema_between_descriptives.csv\n")

# Table S8: within-person descriptives (average SD across EMA occasions)
within_sds <- ema_119 |>
  group_by(user_id) |>
  summarise(
    across(all_of(pid5_ema_vars), \(x) sd(x, na.rm = TRUE)),
    .groups = "drop"
  )

ws_mat <- as.matrix(within_sds[pid5_ema_vars])

table_s8 <- tibble(
  domain = domain_labels,
  M_SD = round(colMeans(ws_mat, na.rm = TRUE), 2),
  SD_of_SD = round(apply(ws_mat, 2, sd, na.rm = TRUE), 2),
  Min_SD = round(apply(ws_mat, 2, min, na.rm = TRUE), 2),
  Max_SD = round(apply(ws_mat, 2, max, na.rm = TRUE), 2)
)

print(table_s8)
write_csv(
  table_s8,
  here("results", "descriptives", "table_s8_ema_within_descriptives.csv")
)
cat("Saved: table_s8_ema_within_descriptives.csv\n")

cat("\n=== Descriptives and reliability complete ===\n")
