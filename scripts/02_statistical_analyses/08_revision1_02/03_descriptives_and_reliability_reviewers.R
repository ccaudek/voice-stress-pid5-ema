# ==============================================================================
# 03_descriptives_and_reliability_reviewers.R
# Reviewer-support descriptives and reliability for EMA manipulation checks
#
# Purpose:
#   Produce descriptive statistics and internal-consistency diagnostics for:
#     1) negative affect keyed components:
#        angry, sad, reverse(happy), reverse(satisfied)
#     2) appraisal keyed components:
#        context_threat_05, reverse(context_quality_05)
#
# Why this script exists:
#   The manipulation-check models should show item-level change. This script
#   complements those Bayesian models by documenting the item scales, keyed
#   composites, descriptives, inter-item correlations, and reliability indices.
#
# Caveat:
#   The two appraisal indicators are single items assessing different facets
#   of the present situation: threat and pleasantness. The two-item alpha / 
#   Spearman-Brown coefficient is reported only as a descriptive diagnostic,
#   not as evidence that the two items form a strongly reflective scale.
# ============================================================================== 

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
})

set.seed(123)

# ----------------------------
# 0) SCALES AND OPTIONS
# ----------------------------
MOOD_MIN <- 0
MOOD_MAX <- 100
THREAT_MIN <- 0
THREAT_MAX <- 5
QUALITY_MIN <- -2
QUALITY_MAX <- 2
COMMON_MIN <- 0
COMMON_MAX <- 5

MOOD_ITEMS <- c("happy", "sad", "satisfied", "angry")
APPRAISAL_ITEMS <- c("context_threat", "context_quality")

NEGATIVE_AFFECT_KEYED_ITEMS <- c("angry", "sad", "happy_rev", "satisfied_rev")
APPRAISAL_KEYED_ITEMS <- c("context_threat_05", "context_quality_rev_05")

safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

rescale_linear <- function(x, from_min, from_max, to_min = 0, to_max = 5) {
  ((x - from_min) / (from_max - from_min)) * (to_max - to_min) + to_min
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

out_dir <- here("results", "manipulation_check", "ema_descriptives_reliability")
for (sub in c("data", "figures", "tables", "manuscript")) {
  dir.create(file.path(out_dir, sub), recursive = TRUE, showWarnings = FALSE)
}

cat("\n=== PATHS ===\n")
cat("EMA file:   ", ema_path, "\n")
cat("AUDIO file: ", audio_path, "\n")
cat("Output dir: ", out_dir, "\n")

# ----------------------------
# 2) AUDIO IDs + CORRECTIONS
# ----------------------------
read_audio_sheet <- function(sheet_name) {
  read_excel(audio_path, sheet = sheet_name) |>
    rename_with(stringr::str_trim) |>
    mutate(ID = stringr::str_trim(as.character(ID))) |>
    select(ID)
}

audio_ids <- bind_rows(
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
  filter(!is.na(ID), ID != "") |>
  distinct(ID) |>
  arrange(ID)

cat("AUDIO: N unique subjects =", nrow(audio_ids), "\n")

# ----------------------------
# 3) LOAD EMA AND CREATE KEYED VARIABLES
# ----------------------------
df_ema_raw <- read_csv(ema_path, show_col_types = FALSE) |>
  rename_with(stringr::str_trim)

required_cols <- c("user_id", "exam_period", MOOD_ITEMS, APPRAISAL_ITEMS)
missing_cols <- setdiff(required_cols, names(df_ema_raw))
if (length(missing_cols) > 0) {
  stop("Missing EMA columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

df_ema <- df_ema_raw |>
  mutate(
    user_id = stringr::str_trim(as.character(user_id)),
    exam_period = stringr::str_to_lower(stringr::str_trim(as.character(exam_period))),
    across(all_of(c(MOOD_ITEMS, APPRAISAL_ITEMS)), ~ suppressWarnings(as.numeric(.x))),
    timepoint = case_when(
      exam_period %in% c("baseline", "base", "bsl") ~ "baseline",
      exam_period %in% c("pre", "pre_exam", "pre-exam", "preexam") ~ "pre",
      exam_period %in% c("post", "post_exam", "post-exam", "postexam") ~ "post",
      TRUE ~ NA_character_
    ),
    timepoint = factor(timepoint, levels = c("baseline", "pre", "post")),

    # Negative affect keyed components on 0..100.
    happy_rev = MOOD_MAX + MOOD_MIN - happy,
    satisfied_rev = MOOD_MAX + MOOD_MIN - satisfied,
    negative_affect = angry + sad + happy_rev + satisfied_rev,
    negative_affect_0100 = negative_affect / 4,

    # Appraisal components on common 0..5 scale before reverse coding.
    context_threat_05 = rescale_linear(context_threat, THREAT_MIN, THREAT_MAX, COMMON_MIN, COMMON_MAX),
    context_quality_05 = rescale_linear(context_quality, QUALITY_MIN, QUALITY_MAX, COMMON_MIN, COMMON_MAX),
    context_quality_rev_05 = COMMON_MAX + COMMON_MIN - context_quality_05,
    negative_appraisal = context_threat_05 + context_quality_rev_05
  )

df_prompt <- df_ema |>
  semi_join(audio_ids, by = c("user_id" = "ID")) |>
  filter(!is.na(timepoint))

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
cat("\n=== MATCH REPORT ===\n")
print(match_report)
write_csv(match_report, file.path(out_dir, "tables", "match_report.csv"))
write_csv(df_prompt, file.path(out_dir, "data", "ema_audio_matched_prompt_level.csv"))

# ----------------------------
# 4) RANGE CHECKS
# ----------------------------
range_specs <- tibble::tribble(
  ~variable,                ~expected_min, ~expected_max, ~domain,
  "happy",                  MOOD_MIN,      MOOD_MAX,      "negative_affect_raw_item",
  "sad",                    MOOD_MIN,      MOOD_MAX,      "negative_affect_raw_item",
  "satisfied",              MOOD_MIN,      MOOD_MAX,      "negative_affect_raw_item",
  "angry",                  MOOD_MIN,      MOOD_MAX,      "negative_affect_raw_item",
  "happy_rev",              MOOD_MIN,      MOOD_MAX,      "negative_affect_keyed_item",
  "satisfied_rev",          MOOD_MIN,      MOOD_MAX,      "negative_affect_keyed_item",
  "negative_affect",        0,             400,           "negative_affect_composite",
  "negative_affect_0100",   0,             100,           "negative_affect_composite",
  "context_threat",         THREAT_MIN,    THREAT_MAX,    "appraisal_raw_item",
  "context_quality",        QUALITY_MIN,   QUALITY_MAX,   "appraisal_raw_item",
  "context_threat_05",      COMMON_MIN,    COMMON_MAX,    "appraisal_keyed_item",
  "context_quality_05",     COMMON_MIN,    COMMON_MAX,    "appraisal_rescaled_item",
  "context_quality_rev_05", COMMON_MIN,    COMMON_MAX,    "appraisal_keyed_item",
  "negative_appraisal",     0,             10,            "appraisal_composite"
)

range_check <- range_specs |>
  mutate(
    observed_min = purrr::map_dbl(variable, ~ safe_min(df_prompt[[.x]])),
    observed_max = purrr::map_dbl(variable, ~ safe_max(df_prompt[[.x]])),
    n_missing = purrr::map_int(variable, ~ sum(is.na(df_prompt[[.x]]))),
    n_out_of_range = purrr::pmap_int(
      list(variable, expected_min, expected_max),
      function(v, mn, mx) sum(!is.na(df_prompt[[v]]) & (df_prompt[[v]] < mn | df_prompt[[v]] > mx))
    )
  )
cat("\n=== RANGE CHECK ===\n")
print(as.data.frame(range_check))
write_csv(range_check, file.path(out_dir, "tables", "range_check.csv"))

if (any(range_check$n_out_of_range > 0)) {
  warning("Some variables are outside their expected theoretical range.")
}

# ----------------------------
# 5) DESCRIPTIVES
# ----------------------------
vars_for_descriptives <- range_specs$variable

make_descriptives <- function(data, level_label) {
  data |>
    pivot_longer(
      cols = all_of(vars_for_descriptives),
      names_to = "variable",
      values_to = "value"
    ) |>
    left_join(range_specs, by = "variable") |>
    group_by(level = level_label, domain, variable, timepoint, expected_min, expected_max) |>
    summarise(
      n = sum(!is.na(value)),
      n_subj = n_distinct(user_id[!is.na(value)]),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      iqr = IQR(value, na.rm = TRUE),
      min = safe_min(value),
      max = safe_max(value),
      .groups = "drop"
    )
}

prompt_descriptives <- make_descriptives(df_prompt, "prompt_level")

subject_timepoint <- df_prompt |>
  group_by(user_id, timepoint) |>
  summarise(
    across(all_of(vars_for_descriptives), ~ mean(.x, na.rm = TRUE)),
    n_prompts = n(),
    .groups = "drop"
  ) |>
  mutate(across(all_of(vars_for_descriptives), ~ ifelse(is.nan(.x), NA_real_, .x)))

subject_timepoint_descriptives <- make_descriptives(subject_timepoint, "subject_timepoint_means")

descriptives_all <- bind_rows(prompt_descriptives, subject_timepoint_descriptives)
write_csv(prompt_descriptives, file.path(out_dir, "tables", "prompt_level_descriptives.csv"))
write_csv(subject_timepoint_descriptives, file.path(out_dir, "tables", "subject_timepoint_descriptives.csv"))
write_csv(descriptives_all, file.path(out_dir, "tables", "descriptives_all.csv"))
write_csv(subject_timepoint, file.path(out_dir, "data", "ema_audio_matched_subject_timepoint.csv"))

# ----------------------------
# 6) RELIABILITY / INTER-ITEM DIAGNOSTICS
# ----------------------------
cronbach_alpha_table <- function(data, items) {
  item_df <- data |>
    select(all_of(items)) |>
    as.data.frame()
  item_df <- item_df[complete.cases(item_df), , drop = FALSE]
  k <- length(items)
  n_complete <- nrow(item_df)

  if (k < 2 || n_complete < 3) {
    return(tibble(
      k = k, n_complete = n_complete,
      alpha = NA_real_, mean_interitem_r = NA_real_, spearman_brown = NA_real_
    ))
  }

  item_vars <- purrr::map_dbl(item_df, var)
  total_score <- rowSums(item_df)
  total_var <- var(total_score)
  alpha <- if (is.finite(total_var) && total_var > 0) {
    k / (k - 1) * (1 - sum(item_vars, na.rm = TRUE) / total_var)
  } else {
    NA_real_
  }

  cors <- suppressWarnings(cor(item_df, use = "pairwise.complete.obs"))
  mean_r <- if (k > 1) mean(cors[upper.tri(cors)], na.rm = TRUE) else NA_real_
  if (!is.finite(mean_r)) mean_r <- NA_real_
  spearman_brown <- if (k == 2 && is.finite(mean_r) && mean_r > -1) {
    2 * mean_r / (1 + mean_r)
  } else {
    NA_real_
  }

  tibble(
    k = k,
    n_complete = n_complete,
    alpha = alpha,
    mean_interitem_r = mean_r,
    spearman_brown = spearman_brown
  )
}

item_rest_correlations <- function(data, items) {
  item_df <- data |>
    select(all_of(items)) |>
    as.data.frame()
  item_df <- item_df[complete.cases(item_df), , drop = FALSE]

  if (nrow(item_df) < 3 || length(items) < 2) {
    return(tibble(item = items, item_rest_correlation = NA_real_, n_complete = nrow(item_df)))
  }

  purrr::map_dfr(items, function(item) {
    rest <- setdiff(items, item)
    rest_sum <- rowSums(item_df[, rest, drop = FALSE])
    r <- suppressWarnings(cor(item_df[[item]], rest_sum, use = "complete.obs"))
    tibble(item = item, item_rest_correlation = r, n_complete = nrow(item_df))
  })
}

reliability_by_timepoint <- function(data, items, scale_name, level_label) {
  by_tp <- data |>
    group_by(timepoint) |>
    group_modify(~ cronbach_alpha_table(.x, items)) |>
    ungroup()

  total <- data |>
    mutate(timepoint = factor("all", levels = c("baseline", "pre", "post", "all"))) |>
    group_by(timepoint) |>
    group_modify(~ cronbach_alpha_table(.x, items)) |>
    ungroup()

  bind_rows(by_tp, total) |>
    mutate(scale = scale_name, level = level_label, .before = 1)
}

item_rest_by_timepoint <- function(data, items, scale_name, level_label) {
  by_tp <- data |>
    group_by(timepoint) |>
    group_modify(~ item_rest_correlations(.x, items)) |>
    ungroup()

  total <- data |>
    mutate(timepoint = factor("all", levels = c("baseline", "pre", "post", "all"))) |>
    group_by(timepoint) |>
    group_modify(~ item_rest_correlations(.x, items)) |>
    ungroup()

  bind_rows(by_tp, total) |>
    mutate(scale = scale_name, level = level_label, .before = 1)
}

reliability_all <- bind_rows(
  reliability_by_timepoint(df_prompt, NEGATIVE_AFFECT_KEYED_ITEMS, "negative_affect_keyed_4items", "prompt_level"),
  reliability_by_timepoint(subject_timepoint, NEGATIVE_AFFECT_KEYED_ITEMS, "negative_affect_keyed_4items", "subject_timepoint_means"),
  reliability_by_timepoint(df_prompt, APPRAISAL_KEYED_ITEMS, "negative_appraisal_keyed_2items", "prompt_level"),
  reliability_by_timepoint(subject_timepoint, APPRAISAL_KEYED_ITEMS, "negative_appraisal_keyed_2items", "subject_timepoint_means")
)

item_rest_all <- bind_rows(
  item_rest_by_timepoint(df_prompt, NEGATIVE_AFFECT_KEYED_ITEMS, "negative_affect_keyed_4items", "prompt_level"),
  item_rest_by_timepoint(subject_timepoint, NEGATIVE_AFFECT_KEYED_ITEMS, "negative_affect_keyed_4items", "subject_timepoint_means"),
  item_rest_by_timepoint(df_prompt, APPRAISAL_KEYED_ITEMS, "negative_appraisal_keyed_2items", "prompt_level"),
  item_rest_by_timepoint(subject_timepoint, APPRAISAL_KEYED_ITEMS, "negative_appraisal_keyed_2items", "subject_timepoint_means")
)

cat("\n=== RELIABILITY DIAGNOSTICS ===\n")
print(as.data.frame(reliability_all))
write_csv(reliability_all, file.path(out_dir, "tables", "reliability_diagnostics.csv"))
write_csv(item_rest_all, file.path(out_dir, "tables", "item_rest_correlations.csv"))

# Pairwise correlations, useful for transparent reporting.
pairwise_correlations <- function(data, items, scale_name, level_label) {
  purrr::map_dfr(levels(droplevels(data$timepoint)), function(tp) {
    d <- data |> filter(timepoint == tp) |> select(all_of(items))
    cors <- suppressWarnings(cor(d, use = "pairwise.complete.obs"))
    as.data.frame(as.table(cors)) |>
      as_tibble() |>
      rename(item_1 = Var1, item_2 = Var2, correlation = Freq) |>
      filter(as.character(item_1) < as.character(item_2)) |>
      mutate(scale = scale_name, level = level_label, timepoint = tp, .before = 1)
  })
}

pairwise_all <- bind_rows(
  pairwise_correlations(df_prompt, NEGATIVE_AFFECT_KEYED_ITEMS, "negative_affect_keyed_4items", "prompt_level"),
  pairwise_correlations(subject_timepoint, NEGATIVE_AFFECT_KEYED_ITEMS, "negative_affect_keyed_4items", "subject_timepoint_means"),
  pairwise_correlations(df_prompt, APPRAISAL_KEYED_ITEMS, "negative_appraisal_keyed_2items", "prompt_level"),
  pairwise_correlations(subject_timepoint, APPRAISAL_KEYED_ITEMS, "negative_appraisal_keyed_2items", "subject_timepoint_means")
)
write_csv(pairwise_all, file.path(out_dir, "tables", "pairwise_item_correlations.csv"))

# ----------------------------
# 7) FIGURES
# ----------------------------
plot_vars <- c("angry", "sad", "happy", "satisfied", "context_threat", "context_quality")
plot_data <- subject_timepoint |>
  select(user_id, timepoint, all_of(plot_vars)) |>
  pivot_longer(cols = all_of(plot_vars), names_to = "variable", values_to = "value")

p_items <- ggplot(plot_data, aes(timepoint, value, group = user_id)) +
  geom_line(alpha = 0.15) +
  geom_point(alpha = 0.25, position = position_jitter(width = 0.05, height = 0)) +
  stat_summary(aes(group = 1), fun = median, geom = "point", size = 2.5) +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Subject-timepoint means for reviewer-relevant EMA items",
    subtitle = "Negative affect: angry, sad, happy, satisfied; appraisal: threat, pleasantness",
    x = NULL,
    y = NULL
  ) +
  theme_minimal()

ggsave(
  file.path(out_dir, "figures", "reviewer_relevant_item_descriptives.png"),
  p_items,
  width = 12,
  height = 7,
  dpi = 300
)

# ----------------------------
# 8) MANUSCRIPT NOTE TEMPLATE
# ----------------------------
rel_subject <- reliability_all |>
  filter(level == "subject_timepoint_means", timepoint == "all")

na_rel <- rel_subject |> filter(scale == "negative_affect_keyed_4items") |> slice(1)
app_rel <- rel_subject |> filter(scale == "negative_appraisal_keyed_2items") |> slice(1)
fmt <- function(x) ifelse(is.na(x), "NA", formatC(x, format = "f", digits = 2))

manuscript_note <- paste0(
  "Descriptive reliability diagnostics were computed after keying the items in the direction of negative affect/appraisal. ",
  "For the four negative-affect components (angry, sad, reverse-coded happy, reverse-coded satisfied), Cronbach's alpha based on ",
  "subject-by-timepoint means was ", fmt(na_rel$alpha), " (mean inter-item r = ", fmt(na_rel$mean_interitem_r), "). ",
  "For the two appraisal components (threat rescaled to 0-5 and reverse-coded pleasantness rescaled to 0-5), the two-item alpha/Spearman-Brown ",
  "coefficient was ", fmt(app_rel$alpha), " / ", fmt(app_rel$spearman_brown), " (inter-item r = ", fmt(app_rel$mean_interitem_r), "). ",
  "Because the appraisal score combines two conceptually distinct single-item facets, this coefficient should be reported only as a descriptive diagnostic.\n"
)
writeLines(manuscript_note, file.path(out_dir, "manuscript", "descriptives_reliability_note.txt"))
cat("\n", manuscript_note, "\n")

# ----------------------------
# 9) SAVE BUNDLE
# ----------------------------
saveRDS(
  list(
    ema_path = ema_path,
    audio_path = audio_path,
    match_report = match_report,
    range_check = range_check,
    df_prompt = df_prompt,
    subject_timepoint = subject_timepoint,
    descriptives_all = descriptives_all,
    reliability_all = reliability_all,
    item_rest_all = item_rest_all,
    pairwise_all = pairwise_all
  ),
  file.path(out_dir, "data", "descriptives_reliability_bundle.rds")
)

cat("\n=== DONE ===\n")
cat("Output in:", out_dir, "\n")
cat("Key files:\n")
cat(" - tables/descriptives_all.csv\n")
cat(" - tables/reliability_diagnostics.csv\n")
cat(" - tables/item_rest_correlations.csv\n")
cat(" - figures/reviewer_relevant_item_descriptives.png\n")
cat(" - manuscript/descriptives_reliability_note.txt\n")

# eof ---
