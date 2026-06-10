# ==============================================================================
# 05_compile_manipulation_check_reviewer_tables.R
# Compile reviewer-ready manipulation-check tables after running:
#   02_ema_manipulation_check_negative_affect_itemwise_priors.R
#   04_ema_manipulation_check_appraisal_reviewer_ready.R
#
# Output:
#   results/manipulation_check/reviewer_response/manipulation_check_items_and_composites.csv
#   results/manipulation_check/reviewer_response/manipulation_check_response_skeleton.txt
# ============================================================================== 

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

fmt <- function(x, digits = 2) formatC(as.numeric(x), format = "f", digits = digits)

first_existing <- function(paths, label) {
  existing <- paths[file.exists(paths)]
  if (length(existing) == 0) {
    stop(
      "File not found for ", label, ". Run scripts 02 and 04 first. Paths tried:\n",
      paste0(" - ", paths, collapse = "\n"),
      call. = FALSE
    )
  }
  existing[[1]]
}

na_table_path <- first_existing(
  c(
    here("results", "manipulation_check", "ema_negative_affect_itemwise", "tables", "reviewer_ready_negative_affect_table.csv"),
    "results/manipulation_check/ema_negative_affect_itemwise/tables/reviewer_ready_negative_affect_table.csv"
  ),
  "reviewer_ready_negative_affect_table.csv"
)

app_table_path <- first_existing(
  c(
    here("results", "manipulation_check", "ema_appraisal", "tables", "reviewer_ready_appraisal_table.csv"),
    "results/manipulation_check/ema_appraisal/tables/reviewer_ready_appraisal_table.csv"
  ),
  "reviewer_ready_appraisal_table.csv"
)

out_dir <- here("results", "manipulation_check", "reviewer_response")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

na_tbl <- read_csv(na_table_path, show_col_types = FALSE) |>
  mutate(
    domain = "negative_affect",
    construct = label,
    pre_minus_baseline = pre_baseline,
    post_minus_pre = post_pre,
    pre_minus_baseline_p_expected = pre_baseline_expected_prob,
    post_minus_pre_p_expected = post_pre_expected_prob
  ) |>
  select(
    domain, outcome, construct, item_type, scale_min, scale_max,
    expected_pre_baseline, pre_minus_baseline, pre_minus_baseline_p_expected,
    expected_post_pre, post_minus_pre, post_minus_pre_p_expected
  )

app_label <- c(
  context_threat = "Perceived threat",
  context_quality = "Situational pleasantness",
  negative_appraisal = "Negative appraisal composite"
)

app_tbl <- read_csv(app_table_path, show_col_types = FALSE) |>
  mutate(
    construct = recode(outcome, !!!app_label),
    scale_min = case_when(
      outcome == "context_threat" ~ 0,
      outcome == "context_quality" ~ -2,
      outcome == "negative_appraisal" ~ 0,
      TRUE ~ NA_real_
    ),
    scale_max = case_when(
      outcome == "context_threat" ~ 5,
      outcome == "context_quality" ~ 2,
      outcome == "negative_appraisal" ~ 10,
      TRUE ~ NA_real_
    ),
    pre_minus_baseline = paste0(fmt(pre_minus_baseline_median), " [", fmt(pre_minus_baseline_lo), ", ", fmt(pre_minus_baseline_hi), "]"),
    post_minus_pre = paste0(fmt(post_minus_pre_median), " [", fmt(post_minus_pre_lo), ", ", fmt(post_minus_pre_hi), "]")
  ) |>
  select(
    domain, outcome, construct, item_type, scale_min, scale_max,
    expected_pre_baseline, pre_minus_baseline, pre_minus_baseline_p_expected,
    expected_post_pre, post_minus_pre, post_minus_pre_p_expected
  )

combined <- bind_rows(na_tbl, app_tbl) |>
  mutate(
    expected_pre_baseline = recode(expected_pre_baseline, "+" = "increase", "-" = "decrease"),
    expected_post_pre = recode(expected_post_pre, "+" = "increase", "-" = "decrease")
  )

write_csv(combined, file.path(out_dir, "manipulation_check_items_and_composites.csv"))

response_skeleton <- paste0(
  "Reviewer response skeleton\n\n",
  "We thank the reviewer for the suggestion. We repeated the EMA manipulation check at the item level, rather than relying only on the composites. ",
  "For negative affect, we modeled the two negative-valence items (angry, sad) and the two positive-valence items (happy, satisfied) separately. ",
  "For appraisal of the current situation, we modeled perceived threat and situational pleasantness separately; pleasantness was additionally rescaled and reverse-coded only for the composite. ",
  "All models used the same hierarchical Bayesian structure with participant-level random intercepts and adjacent contrasts. Priors were weakly informative and scaled to the theoretical range of each outcome, so b1 and b2 are directly interpretable as PRE - BASELINE and POST - PRE differences on the original outcome scale.\n\n",
  "The combined item-level and composite results are saved in manipulation_check_items_and_composites.csv.\n"
)
writeLines(response_skeleton, file.path(out_dir, "manipulation_check_response_skeleton.txt"))

cat("\n=== DONE ===\n")
cat("Combined table:", file.path(out_dir, "manipulation_check_items_and_composites.csv"), "\n")
cat("Response skeleton:", file.path(out_dir, "manipulation_check_response_skeleton.txt"), "\n")

# eof ---
