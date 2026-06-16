library(here)

script_dir <- here(
  "scripts",
  "02_statistical_analyses",
  "08_revision1_02"
)

scripts <- c(
  negative_affect = "02_ema_manipulation_check_negative_affect_itemwise_priors.R",
  appraisal = "04_ema_manipulation_check_appraisal_reviewer_ready.R",
  descriptives = "03_descriptives_and_reliability_reviewers.R",
  tables = "05_compile_manipulation_check_reviewer_tables.R"
)

source(file.path(script_dir, scripts["negative_affect"]))
source(file.path(script_dir, scripts["appraisal"]))
source(file.path(script_dir, scripts["descriptives"]))
source(file.path(script_dir, scripts["tables"]))

# eof ---
