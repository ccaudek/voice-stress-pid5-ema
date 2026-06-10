# =============================================================================
# anonymise_participant_ids.R
#
# Replaces participant IDs in both data files with fully anonymous codes
# (P001, P002, ..., P119) before depositing in the public repository.
#
# The original user_id format ("ad_pa_2006_01_27_630") encodes initials and
# date of birth and should not be published. This script creates a consistent
# anonymous mapping across both files so within-participant joins remain valid.
#
# Inputs (data/processed/):
#   pid5_required_columns.csv   — EMA + baseline PID-5 (column: user_id)
#   audio_required_columns.csv  — acoustic features (column: ID)
#
# Outputs:
#   data/processed/pid5_required_columns.csv    — overwritten with anon IDs
#   data/processed/audio_required_columns.csv   — overwritten with anon IDs
#   data/processed/id_mapping_PRIVATE.csv       — mapping original→anonymous
#                                                  DO NOT include in repository
#
# The id_mapping_PRIVATE.csv file must be kept secure and NOT uploaded to OSF
# or any public repository. It is needed only if individual-level linkage to
# other study data is required for future analyses.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

pid5_path  <- here("data", "processed", "pid5_required_columns.csv")
audio_path <- here("data", "processed", "audio_required_columns.csv")

stopifnot(file.exists(pid5_path), file.exists(audio_path))

pid5  <- read_csv(pid5_path,  show_col_types = FALSE)
audio <- read_csv(audio_path, show_col_types = FALSE)

# =============================================================================
# 1. Build consistent anonymous ID mapping from the union of both files
# =============================================================================
all_ids <- sort(union(unique(pid5$user_id), unique(audio$ID)))

stopifnot(
  length(all_ids) == n_distinct(pid5$user_id),
  length(all_ids) == n_distinct(audio$ID),
  setequal(pid5$user_id, audio$ID)
)

id_map <- tibble(
  original_id  = all_ids,
  anonymous_id = sprintf("P%03d", seq_along(all_ids))
)

cat("Participant mapping created:\n")
cat("  N participants:", nrow(id_map), "\n")
cat("  ID format: P001 –", tail(id_map$anonymous_id, 1), "\n\n")

# =============================================================================
# 2. Apply mapping to pid5_required_columns.csv
# =============================================================================
pid5_anon <- pid5 |>
  left_join(id_map, by = c("user_id" = "original_id")) |>
  mutate(user_id = anonymous_id) |>
  dplyr::select(-anonymous_id)

stopifnot(
  nrow(pid5_anon)       == nrow(pid5),
  !anyNA(pid5_anon$user_id),
  n_distinct(pid5_anon$user_id) == n_distinct(pid5$user_id)
)

write_csv(pid5_anon, pid5_path)
cat("Saved (anonymised): data/processed/pid5_required_columns.csv\n")
cat("  Rows:", nrow(pid5_anon), "| Participants:", n_distinct(pid5_anon$user_id), "\n\n")

# =============================================================================
# 3. Apply mapping to audio_required_columns.csv
# =============================================================================
audio_anon <- audio |>
  left_join(id_map, by = c("ID" = "original_id")) |>
  mutate(ID = anonymous_id) |>
  dplyr::select(-anonymous_id)

stopifnot(
  nrow(audio_anon)    == nrow(audio),
  !anyNA(audio_anon$ID),
  n_distinct(audio_anon$ID) == n_distinct(audio$ID)
)

write_csv(audio_anon, audio_path)
cat("Saved (anonymised): data/processed/audio_required_columns.csv\n")
cat("  Rows:", nrow(audio_anon), "| Participants:", n_distinct(audio_anon$ID), "\n\n")

# =============================================================================
# 4. Save private mapping file (DO NOT publish)
# =============================================================================
mapping_path <- here("data", "processed", "id_mapping_PRIVATE.csv")
write_csv(id_map, mapping_path)

cat("Saved (PRIVATE — do not upload): data/processed/id_mapping_PRIVATE.csv\n\n")

# =============================================================================
# 5. Verification
# =============================================================================
# Reload and confirm join still works
pid5_check  <- read_csv(pid5_path,  show_col_types = FALSE)
audio_check <- read_csv(audio_path, show_col_types = FALSE)

joined <- inner_join(
  distinct(pid5_check, user_id),
  distinct(audio_check, ID),
  by = c("user_id" = "ID")
)

stopifnot(nrow(joined) == nrow(id_map))

cat("=== Verification ===\n")
cat("  pid5 IDs:   ", n_distinct(pid5_check$user_id), "\n")
cat("  audio IDs:  ", n_distinct(audio_check$ID), "\n")
cat("  Join result:", nrow(joined), "(all match)\n")
cat("  Sample IDs: ", paste(head(sort(unique(pid5_check$user_id)), 5),
                             collapse = ", "), "\n\n")
cat("=== Anonymisation complete ===\n")
cat("REMINDER: keep id_mapping_PRIVATE.csv out of the repository.\n")
cat("Add it to .gitignore:\n")
cat("  echo 'data/processed/id_mapping_PRIVATE.csv' >> .gitignore\n")
