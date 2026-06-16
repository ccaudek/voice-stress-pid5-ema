# PID-5 trait and state distributions for reviewer R2.14
# -----------------------------------------------------
# Purpose:
#   1. Put baseline PID-5 trait scores and EMA PID-5 state scores on the same
#      interpretable 0-3 item-mean scale.
#   2. Produce main-text histograms for trait and state distributions.
#   3. Export descriptives and draft text for the manuscript / reviewer response.
#
# Notes:
#   - EMA PID-5 domain variables in the processed data are sums of 3 items
#     (range 0-9). They are divided by 3 here.
#   - Baseline PID-5 domain variables, when read from the merged processed file,
#     are raw item sums. They are divided by the number of scored items in the
#     domain. If the raw PID-5 Excel file is available, scores are recomputed
#     directly as item means.
#   - Rescaling changes only descriptive reporting. Moderation models based on
#     standardized latent trait scores do not need to be refit.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(ggplot2)
  library(purrr)
  library(glue)
})

# Optional packages used only if available.
has_rio <- requireNamespace("rio", quietly = TRUE)
has_here <- requireNamespace("here", quietly = TRUE)

project_path <- function(...) {
  if (has_here) here::here(...) else file.path(...)
}

first_existing <- function(paths) {
  hits <- paths[file.exists(paths)]
  if (length(hits) == 0) return(NA_character_)
  hits[[1]]
}

out_dir <- project_path("outputs", "pid5_distributions")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1) Read processed EMA / merged data
# -----------------------------------------------------------------------------
ema_path <- first_existing(c(
  project_path("data", "processed", "ema_plus_scales_cleaned.csv"),
  project_path("data", "processed", "ema_audio_matched_prompt_level.csv"),
  "ema_audio_matched_prompt_level.csv",
  "/mnt/data/ema_audio_matched_prompt_level.csv"
))

if (is.na(ema_path)) {
  stop("Could not find the EMA / merged processed data file. Update ema_path.")
}

ema <- readr::read_csv(ema_path, show_col_types = FALSE)
if (!"user_id" %in% names(ema)) stop("EMA data must contain a user_id column.")
analysis_ids <- unique(ema$user_id)

# -----------------------------------------------------------------------------
# 2) EMA state scores, item-mean metric 0-3
# -----------------------------------------------------------------------------
# Prefer the domain-sum variables already created in the processed file because
# these reproduce the descriptives currently in the Supplement (e.g., the 4.53
# Negative Affectivity mean on the 0-9 sum scale). If they are absent, compute
# them from the sequential 3-item domain keys.
ema_domain_sum_vars <- c(
  "Negative Affectivity" = "pid5_negative_affectivity",
  "Detachment" = "pid5_detachment",
  "Antagonism" = "pid5_antagonism",
  "Disinhibition" = "pid5_disinhibition",
  "Psychoticism" = "pid5_psychoticism"
)

ema_item_keys <- list(
  "Negative Affectivity" = c("pid5_1", "pid5_2", "pid5_3"),
  "Detachment" = c("pid5_4", "pid5_5", "pid5_6"),
  "Antagonism" = c("pid5_7", "pid5_8", "pid5_9"),
  "Disinhibition" = c("pid5_10", "pid5_11", "pid5_12"),
  "Psychoticism" = c("pid5_13", "pid5_14", "pid5_15")
)

if (all(unname(ema_domain_sum_vars) %in% names(ema))) {
  ema_state <- ema %>%
    select(user_id, all_of(unname(ema_domain_sum_vars))) %>%
    rename(
      !!!setNames(unname(ema_domain_sum_vars), names(ema_domain_sum_vars))
    ) %>%
    pivot_longer(-user_id, names_to = "domain", values_to = "score_sum") %>%
    mutate(
      score = score_sum / 3,
      distribution = "EMA state scores\n(prompt-level)"
    ) %>%
    filter(!is.na(score))
} else {
  missing_items <- setdiff(unique(unlist(ema_item_keys)), names(ema))
  if (length(missing_items) > 0) {
    stop("Missing EMA PID-5 items: ", paste(missing_items, collapse = ", "))
  }
  ema_state <- purrr::map_dfr(names(ema_item_keys), function(dom) {
    ema %>%
      transmute(
        user_id,
        domain = dom,
        score = rowMeans(across(all_of(ema_item_keys[[dom]])), na.rm = TRUE),
        distribution = "EMA state scores\n(prompt-level)"
      )
  }) %>%
    filter(!is.na(score))
}

# Person-level means of the momentary state scores, useful for the table and for
# correcting the old Supplement table that reported 0-9 sums as if they were 0-3.
ema_person_means <- ema_state %>%
  group_by(user_id, domain) %>%
  summarise(score = mean(score, na.rm = TRUE), .groups = "drop") %>%
  mutate(distribution = "EMA person means\n(across prompts)")

# -----------------------------------------------------------------------------
# 3) Baseline full PID-5 trait scores, item-mean metric 0-3
# -----------------------------------------------------------------------------
# Full scoring keys copied from reliability_pid5.R. When raw PID-5 data are
# available, we recompute baseline domains directly as item means.
rev_items <- c(
  7,
  30,
  35,
  58,
  87,
  90,
  96,
  97,
  98,
  131,
  142,
  155,
  164,
  177,
  210,
  215
)
careless_qpos <- c(68, 161)

facet_items <- list(
  anhedonia = c(1, 23, 26, 30, 124, 155, 157, 189),
  anxiousness = c(79, 93, 95, 96, 109, 110, 130, 141, 174),
  attention_seeking = c(14, 43, 74, 111, 113, 173, 191, 211),
  callousness = c(
    11,
    13,
    19,
    54,
    72,
    73,
    90,
    153,
    166,
    183,
    198,
    200,
    207,
    208
  ),
  deceitfulness = c(41, 53, 56, 76, 126, 134, 142, 206, 214, 218),
  depressivity = c(
    27,
    61,
    66,
    81,
    86,
    104,
    119,
    148,
    151,
    163,
    168,
    169,
    178,
    212
  ),
  distractibility = c(6, 29, 47, 68, 88, 118, 132, 144, 199),
  eccentricity = c(5, 21, 24, 25, 33, 52, 55, 70, 71, 152, 172, 185, 205),
  emotional_lability = c(18, 62, 102, 122, 138, 165, 181),
  grandiosity = c(40, 65, 114, 179, 187, 197),
  hostility = c(28, 32, 38, 85, 92, 116, 158, 170, 188, 216),
  impulsivity = c(4, 16, 17, 22, 58, 204),
  intimacy_avoidance = c(89, 97, 108, 120, 145, 203),
  irresponsibility = c(31, 129, 156, 160, 171, 201, 210),
  manipulativeness = c(107, 125, 162, 180, 219),
  perceptual_dysregulation = c(
    36,
    37,
    42,
    44,
    59,
    77,
    83,
    154,
    192,
    193,
    213,
    217
  ),
  perseveration = c(46, 51, 60, 78, 80, 100, 121, 128, 137),
  restricted_affectivity = c(8, 45, 84, 91, 101, 167, 184),
  rigid_perfectionism = c(34, 49, 105, 115, 123, 135, 140, 176, 196, 220),
  risk_taking = c(3, 7, 35, 39, 48, 67, 69, 87, 98, 112, 159, 164, 195, 215),
  separation_insecurity = c(12, 50, 57, 64, 127, 149, 175),
  submissiveness = c(9, 15, 63, 202),
  suspiciousness = c(2, 103, 117, 131, 133, 177, 190),
  unusual_beliefs_exp = c(94, 99, 106, 139, 143, 150, 194, 209),
  withdrawal = c(10, 20, 75, 82, 136, 146, 147, 161, 182, 186)
)

domain_facets <- list(
  "Negative Affectivity" = c(
    "emotional_lability",
    "anxiousness",
    "separation_insecurity"
  ),
  "Detachment" = c("withdrawal", "anhedonia", "intimacy_avoidance"),
  "Antagonism" = c("manipulativeness", "deceitfulness", "grandiosity"),
  "Disinhibition" = c("irresponsibility", "impulsivity", "distractibility"),
  "Psychoticism" = c(
    "unusual_beliefs_exp",
    "eccentricity",
    "perceptual_dysregulation"
  )
)

domain_items <- lapply(domain_facets, function(ff) {
  sort(unique(setdiff(unlist(facet_items[ff]), careless_qpos)))
})

domain_n_items <- vapply(domain_items, length, integer(1))

raw_pid5_path <- first_existing(c(
  project_path("data", "raw", "quest", "PID-5_2025_EMA.xlsx"),
  "PID-5_2025_EMA.xlsx",
  "/mnt/data/PID-5_2025_EMA.xlsx"
))

baseline_sum_vars <- c(
  "Negative Affectivity" = "domain_negative_affect_baseline",
  "Detachment" = "domain_detachment_baseline",
  "Antagonism" = "domain_antagonism_baseline",
  "Disinhibition" = "domain_disinhibition_baseline",
  "Psychoticism" = "domain_psychoticism_baseline"
)

if (!is.na(raw_pid5_path) && has_rio) {
  raw <- rio::import(raw_pid5_path) %>% as_tibble()

  # Column positions follow reliability_pid5.R. Update these if the raw file
  # structure changes.
  item_idx <- 12:233
  date_col <- names(raw)[1]
  user_col <- names(raw)[3]
  age_col <- names(raw)[4]
  sex_col <- names(raw)[5]

  pid <- raw %>%
    transmute(
      date = .data[[date_col]],
      user_id = .data[[user_col]] %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all("__+", "_") %>%
        stringr::str_replace("_$", "") %>%
        stringr::str_replace("^fla_me_", "fl_me_"),
      age = .data[[age_col]],
      sex = .data[[sex_col]],
      !!!setNames(raw[item_idx], paste0("i_", seq_along(item_idx)))
    ) %>%
    mutate(across(starts_with("i_"), ~ suppressWarnings(as.numeric(.x)))) %>%
    mutate(across(
      any_of(paste0("i_", rev_items)),
      ~ ifelse(is.na(.x), NA_real_, 3 - .x)
    )) %>%
    filter(user_id %in% analysis_ids)

  baseline_trait <- purrr::map_dfr(names(domain_items), function(dom) {
    cols <- paste0("i_", domain_items[[dom]])
    pid %>%
      transmute(
        user_id,
        domain = dom,
        score = rowMeans(across(all_of(cols)), na.rm = TRUE),
        n_valid_items = rowSums(!is.na(across(all_of(cols))))
      )
  }) %>%
    filter(!is.na(score)) %>%
    mutate(distribution = "Baseline PID-5 traits\n(full questionnaire)")
} else if (all(unname(baseline_sum_vars) %in% names(ema))) {
  # Fallback for merged data in which baseline domains were already scored as sums.
  baseline_trait <- ema %>%
    select(user_id, all_of(unname(baseline_sum_vars))) %>%
    distinct(user_id, .keep_all = TRUE) %>%
    rename(!!!setNames(unname(baseline_sum_vars), names(baseline_sum_vars))) %>%
    pivot_longer(-user_id, names_to = "domain", values_to = "score_sum") %>%
    mutate(
      score = score_sum / domain_n_items[domain],
      distribution = "Baseline PID-5 traits\n(full questionnaire)"
    ) %>%
    filter(!is.na(score))
} else {
  stop(
    "No baseline PID-5 raw file or precomputed baseline domain variables found."
  )
}

# -----------------------------------------------------------------------------
# 4) Combine, describe, and plot
# -----------------------------------------------------------------------------
domain_order <- c(
  "Negative Affectivity",
  "Detachment",
  "Antagonism",
  "Disinhibition",
  "Psychoticism"
)
plot_data <- bind_rows(baseline_trait, ema_state) %>%
  mutate(
    domain = factor(domain, levels = domain_order),
    distribution = factor(
      distribution,
      levels = c(
        "Baseline PID-5 traits\n(full questionnaire)",
        "EMA state scores\n(prompt-level)"
      )
    )
  )

# Descriptives for main text and supplement.
descriptives <- bind_rows(
  baseline_trait,
  ema_state,
  ema_person_means
) %>%
  mutate(domain = factor(domain, levels = domain_order)) %>%
  group_by(distribution, domain) %>%
  summarise(
    n_observations = n(),
    n_participants = n_distinct(user_id),
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    median = median(score, na.rm = TRUE),
    iqr = IQR(score, na.rm = TRUE),
    min = min(score, na.rm = TRUE),
    max = max(score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(distribution, domain)

readr::write_csv(
  descriptives,
  file.path(out_dir, "pid5_trait_state_descriptives_0_3.csv")
)

scale_check <- tibble(
  domain = names(domain_n_items),
  baseline_scored_items = as.integer(domain_n_items),
  baseline_score_metric = "item mean, 0-3",
  ema_items_per_domain = 3L,
  ema_score_metric = "item mean, 0-3; processed sum variables divided by 3"
)
readr::write_csv(scale_check, file.path(out_dir, "pid5_scale_check.csv"))

# Main-text histogram figure.
p <- ggplot(plot_data, aes(x = score)) +
  geom_histogram(
    binwidth = 0.25,
    boundary = 0,
    closed = "left",
    fill = "grey70",
    color = "white"
  ) +
  facet_grid(distribution ~ domain) +
  coord_cartesian(xlim = c(0, 3)) +
  scale_x_continuous(breaks = 0:3) +
  labs(
    x = "PID-5 domain score (item mean, 0-3)",
    y = "Frequency",
    title = "PID-5 trait and state distributions",
    subtitle = "Baseline full PID-5 traits and EMA momentary PID-5 states on a common 0-3 metric"
  ) +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 9),
    plot.title.position = "plot"
  )

ggsave(
  file.path(out_dir, "pid5_trait_state_histograms.png"),
  p,
  width = 10.5,
  height = 5.8,
  dpi = 300
)
ggsave(
  file.path(out_dir, "pid5_trait_state_histograms.pdf"),
  p,
  width = 10.5,
  height = 5.8,
  device = "pdf"
)

# Optional compact version: person-level only. This is useful if the journal wants
# each histogram to reflect persons rather than prompts.
p_person <- bind_rows(baseline_trait, ema_person_means) %>%
  mutate(
    domain = factor(domain, levels = domain_order),
    distribution = factor(
      distribution,
      levels = c(
        "Baseline PID-5 traits\n(full questionnaire)",
        "EMA person means\n(across prompts)"
      )
    )
  ) %>%
  ggplot(aes(x = score)) +
  geom_histogram(
    binwidth = 0.25,
    boundary = 0,
    closed = "left",
    fill = "grey70",
    color = "white"
  ) +
  facet_grid(distribution ~ domain) +
  coord_cartesian(xlim = c(0, 3)) +
  scale_x_continuous(breaks = 0:3) +
  labs(
    x = "PID-5 domain score (item mean, 0-3)",
    y = "Number of participants",
    title = "PID-5 person-level trait distributions",
    subtitle = "Baseline full PID-5 and person-level EMA means on a common 0-3 metric"
  ) +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 9),
    plot.title.position = "plot"
  )

ggsave(
  file.path(out_dir, "pid5_person_level_histograms.png"),
  p_person,
  width = 10.5,
  height = 5.8,
  dpi = 300
)
ggsave(
  file.path(out_dir, "pid5_person_level_histograms.pdf"),
  p_person,
  width = 10.5,
  height = 5.8,
  device = "pdf"
)

# -----------------------------------------------------------------------------
# 5) Draft text for the manuscript and response letter
# -----------------------------------------------------------------------------
get_mean <- function(tbl, dist, dom) {
  tbl %>%
    filter(distribution == dist, domain == dom) %>%
    pull(mean) %>%
    round(2)
}
get_sd <- function(tbl, dist, dom) {
  tbl %>% filter(distribution == dist, domain == dom) %>% pull(sd) %>% round(2)
}
get_n <- function(tbl, dist) {
  tbl %>%
    filter(distribution == dist) %>%
    pull(n_participants) %>%
    max(na.rm = TRUE)
}
get_obs <- function(tbl, dist) {
  tbl %>%
    filter(distribution == dist) %>%
    pull(n_observations) %>%
    max(na.rm = TRUE)
}

old_neg_aff_sum_mean <- ema_state %>%
  filter(domain == "Negative Affectivity") %>%
  group_by(user_id) %>%
  summarise(x = mean(score, na.rm = TRUE) * 3, .groups = "drop") %>%
  summarise(x = mean(x, na.rm = TRUE)) %>%
  pull(x)

neg_aff_rescaled <- old_neg_aff_sum_mean / 3

manuscript_text <- glue(
  "PID-5 trait and state distributions. In response to the reviewer, we inspected the distribution of PID-5 scores on a common item-mean metric (0-3; Figure X). Full baseline PID-5 trait scores were available for {get_n(descriptives, 'Baseline PID-5 traits\n(full questionnaire)')} participants in the analytic data file. EMA state scores were based on {get_obs(descriptives, 'EMA state scores\n(prompt-level)')} prompt-level observations from {get_n(descriptives, 'EMA state scores\n(prompt-level)')} participants. The EMA domain variables had previously been summarized as three-item sums (0-9); here they are divided by 3 so that values are directly comparable with the full PID-5 item-mean scores. On this scale, the earlier Negative Affectivity person-mean value of {round(old_neg_aff_sum_mean, 2)} corresponds to {round(neg_aff_rescaled, 2)}, not to a value above the 0-3 response range. Distributions were concentrated in the lower-to-mid range, as expected in a non-clinical student sample, with particularly low and right-skewed scores for Antagonism and Psychoticism. This restricted range is now noted as a limitation for detecting moderation effects in these domains; it does not change the moderation models, which used standardized latent trait estimates rather than the descriptive raw-score metric.\n"
)

response_text <- glue(
  "R2.14. Report the PID-5 trait distributions (trait and state) in the main text with histograms.\n\nWe thank the reviewer for noting this issue. We have added histograms of the PID-5 distributions to the main text, showing both full baseline PID-5 trait scores and EMA momentary PID-5 state scores on a common 0-3 item-mean metric. This also corrected a scaling ambiguity in the Supplement: the EMA domain descriptives had been reported as three-item sums (range 0-9), although the text referred to the original 0-3 response scale. For example, the previously reported Negative Affectivity between-person mean of {round(old_neg_aff_sum_mean, 2)} corresponds to {round(neg_aff_rescaled, 2)} after division by the three EMA items. We now state the metric explicitly and report the corrected descriptives. The histograms show that several domains, especially Antagonism and Psychoticism, are low and right-skewed in this non-clinical student sample. We have added this as a sample-suitability limitation for testing trait moderation. The inferential moderation models were not refit, because they use standardized latent trait scores and are unaffected by this descriptive rescaling.\n"
)

writeLines(manuscript_text, file.path(out_dir, "pid5_main_text_draft.txt"))
writeLines(response_text, file.path(out_dir, "response_R2_14_draft.txt"))

message("Done. Outputs written to: ", out_dir)
message("Key files:")
message(" - pid5_trait_state_histograms.png / .pdf")
message(" - pid5_person_level_histograms.png / .pdf")
message(" - pid5_trait_state_descriptives_0_3.csv")
message(" - pid5_scale_check.csv")
message(" - pid5_main_text_draft.txt")
message(" - response_R2_14_draft.txt")

# R2.14 — PID-5 trait and state distributions

## Main-text insertion (draft)

# **PID-5 trait and state distributions.** To characterize the range of personality-pathology scores in the analytic sample, we inspected the distributions of the five PID-5 domains on a common item-mean metric (0–3; Figure X). Full baseline PID-5 trait scores were computed from the 220-item questionnaire, whereas EMA PID-5 state scores were computed at the prompt level from the three momentary items per domain and divided by three. Thus, the EMA descriptives are no longer reported as three-item sums (range 0–9), which was the source of the apparent out-of-range value noted by the reviewer. The distributions were concentrated in the lower-to-mid range, as expected in a non-clinical student sample, with especially low and right-skewed scores for Antagonism and Psychoticism. This restricted range should be kept in mind when interpreting moderation effects in these domains. The rescaling is descriptive only and does not affect the Bayesian moderation models, which used standardized latent trait estimates.

# **Figure caption (draft).** Distributions of PID-5 trait and state scores in the analytic sample. Baseline trait scores are full PID-5 item-mean domain scores from the baseline questionnaire. EMA state scores are prompt-level item-mean domain scores from the three momentary PID-5 items per domain. All scores are shown on the original 0–3 response metric. Histograms are descriptive; prompt-level EMA observations are repeated within participants.

# ## Response to reviewer (draft)

# Thank you for noting this issue. We added histograms of the PID-5 distributions to the main text, showing both full baseline PID-5 trait scores and EMA momentary PID-5 state scores on a common 0–3 item-mean metric. This also corrected a scaling ambiguity in the Supplement: the EMA domain descriptives had been reported as three-item sums (range 0–9), although the text referred to the original 0–3 response scale. For example, the previously reported Negative Affectivity between-person mean of 4.53 corresponds to 1.51 after division by the three EMA items. We now state the metric explicitly and report the corrected descriptives. The histograms show that several domains, especially Antagonism and Psychoticism, are low and right-skewed in this non-clinical student sample. We have added this as a sample-suitability limitation for testing trait moderation. The inferential moderation models were not refit, because they use standardized latent trait scores and are unaffected by this descriptive rescaling.
