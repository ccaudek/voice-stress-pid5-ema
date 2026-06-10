# =============================================================================
# 05_loo_comparison.R
#
# LOO-CV model comparison: three PID-5 measurement approaches for F0.
#
#   Model A (EMA-only):   latent traits from repeated EMA assessments
#                         Stan: stan/LOO/f0mean_pid5_moderation.stan
#   Model B (Baseline):   baseline PID-5 scores as fixed predictors
#                         Stan: stan/LOO/pid5_baseline_moderation.stan
#   Model C (Combined):   EMA latent traits + baseline fixed predictors
#                         Stan: stan/LOO/pid5_ema_plus_baseline_moderation.stan
#
# All three models run on the same N = 109 participants (10 of 119 excluded
# for missing baseline PID-5 questionnaire).
#
# Requires:
#   results/F0/stan_bundle_f0.rds   (run 01_prepare_f0_data.R first)
#   data/processed/pid5_required_columns.csv
#
# Outputs (results/LOO/):
#   loo_comparison_table.csv              ELPD / LOOIC (Table 4)
#   negaffect_moderation_comparison.csv   precision comparison (Table 5)
# =============================================================================

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(loo)
  library(tidyverse)
  library(here)
})

dir.create(here("results", "LOO"), recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# Helper: compile, fit, and compute LOO for one model
# =============================================================================
fit_and_loo <- function(stan_file, stan_data_arg, label, seed = 1) {
  fit_path <- here("results", "LOO", paste0("fit_", label, ".RDS"))

  if (file.exists(fit_path)) {
    message(label, ": loading cached fit.")
    fit <- readRDS(fit_path)
  } else {
    message(label, ": sampling...")
    mod <- cmdstan_model(here("stan", "LOO", stan_file))
    fit <- mod$sample(
      data = stan_data_arg,
      chains = 4,
      parallel_chains = 4,
      iter_warmup = 2000,
      iter_sampling = 6000,
      adapt_delta = 0.99,
      max_treedepth = 15,
      seed = seed,
      refresh = 500
    )
    fit$save_object(file = fit_path)
  }

  ll <- fit$draws("log_lik", format = "matrix")
  loo_obj <- loo(ll, cores = 4)

  list(fit = fit, loo = loo_obj, label = label)
}

# =============================================================================
# 1. Load F0 bundle (N = 119)
# =============================================================================
bundle <- readRDS(here("results", "F0", "stan_bundle_f0.rds"))
stan_full <- bundle$stan_data # based on all 119 participants
pid5_vars <- bundle$pid5_vars
subj_ids <- bundle$subj_ids # length 119, sorted alphabetically

# =============================================================================
# 2. Load baseline scores and restrict to N = 109
# =============================================================================
pid5_baseline_vars <- c(
  "domain_negative_affect_baseline",
  "domain_detachment_baseline",
  "domain_antagonism_baseline",
  "domain_disinhibition_baseline",
  "domain_psychoticism_baseline"
)

ema_raw <- read_csv(
  here("data", "processed", "pid5_required_columns.csv"),
  show_col_types = FALSE
)

stopifnot(all(pid5_baseline_vars %in% names(ema_raw)))

baseline_scores <- ema_raw |>
  transmute(ID = user_id, across(all_of(pid5_baseline_vars), as.numeric)) |>
  distinct(ID, .keep_all = TRUE) |>
  filter(ID %in% subj_ids, !is.na(domain_negative_affect_baseline))

cat(
  "Participants with complete baseline: N =",
  nrow(baseline_scores),
  "(excluded:",
  length(subj_ids) - nrow(baseline_scores),
  ")\n"
)

# =============================================================================
# 3. Build stan data restricted to N = 109
# =============================================================================
subj_ids_109 <- baseline_scores$ID
idx_109 <- which(subj_ids %in% subj_ids_109)
stopifnot(length(idx_109) == nrow(baseline_scores))

# Remap subject indices from 1:119 to 1:109
old_to_new <- integer(length(subj_ids))
old_to_new[idx_109] <- seq_along(idx_109)

keep_voice <- stan_full$subj_voice %in% idx_109
keep_ema <- stan_full$subj_ema %in% idx_109

# Shared voice component (same for all three models)
voice_109 <- list(
  N_subj = length(idx_109),
  N_voice = sum(keep_voice),
  subj_voice = old_to_new[stan_full$subj_voice[keep_voice]],
  y = stan_full$y[keep_voice],
  c1 = stan_full$c1[keep_voice],
  c2 = stan_full$c2[keep_voice]
)

stopifnot(!anyNA(voice_109$y))
cat("Voice observations (N=109):", voice_109$N_voice, "\n")

# EMA component (Models A and C)
ema_109 <- list(
  N_ema = sum(keep_ema),
  subj_ema = old_to_new[stan_full$subj_ema[keep_ema]],
  D = stan_full$D,
  X = stan_full$X[keep_ema, ]
)

stopifnot(!anyNA(ema_109$X))
cat("EMA observations (N=109):", ema_109$N_ema, "\n")

# Baseline component (Models B and C)
# Z is a subject-level matrix (N_subj x D_base), one row per participant
bl_ordered <- tibble(ID = subj_ids[idx_109]) |>
  left_join(baseline_scores, by = "ID")

Z <- as.matrix(bl_ordered[, pid5_baseline_vars])
Z_scaled <- scale(Z)
stopifnot(!anyNA(Z_scaled))
cat("Baseline matrix:", nrow(Z_scaled), "x", ncol(Z_scaled), "\n")

# =============================================================================
# 4. Assemble data lists for each model
# =============================================================================

# Model A: EMA-only (latent traits)
# Uses: f0mean_pid5_moderation_improved.stan
stan_A <- c(
  voice_109,
  list(
    N_ema = ema_109$N_ema,
    subj_ema = ema_109$subj_ema,
    D = ema_109$D,
    X = ema_109$X
  )
)

# Model B: Baseline-only (fixed predictors, no measurement model)
# Uses: pid5_baseline_moderation_improved.stan
# Data: Z[N_subj, D_base] — one row per subject, no EMA component
stan_B <- c(voice_109, list(D_base = ncol(Z_scaled), Z = Z_scaled))

# Model C: Combined (EMA latent traits + baseline fixed predictors)
# Uses: pid5_ema_plus_baseline_moderation_improved.stan
stan_C <- c(
  voice_109,
  list(
    N_ema = ema_109$N_ema,
    subj_ema = ema_109$subj_ema,
    D_ema = ema_109$D,
    X = ema_109$X,
    D_base = ncol(Z_scaled),
    Z = Z_scaled
  )
)

# =============================================================================
# 5. Fit all three models
# =============================================================================
res_A <- fit_and_loo("f0mean_pid5_moderation.stan", stan_A, "ema_109")
res_B <- fit_and_loo(
  "pid5_baseline_moderation.stan",
  stan_B,
  "baseline_109"
)
res_C <- fit_and_loo(
  "pid5_ema_plus_baseline_moderation.stan",
  stan_C,
  "combined_109"
)

# =============================================================================
# 6. LOO comparison table (Table 4)
# =============================================================================
loo_comp <- loo_compare(res_A$loo, res_B$loo, res_C$loo)

loo_table <- as.data.frame(loo_comp) |>
  rownames_to_column("rank") |>
  mutate(
    model = case_when(
      rank == "model1" ~ "EMA (N=109)",
      rank == "model2" ~ "Baseline (N=109)",
      rank == "model3" ~ "Combined (N=109)",
      TRUE ~ rank
    )
  ) |>
  select(model, elpd_diff, se_diff, elpd_loo, se_elpd_loo, looic, p_loo)

cat("\n=== LOO Comparison ===\n")
print(loo_table)
write_csv(loo_table, here("results", "LOO", "loo_comparison_table.csv"))

# =============================================================================
# 7. Precision comparison for Negative Affectivity x stress (Table 5)
# =============================================================================
extract_negaff <- function(res, param = "g1[1]") {
  g1 <- as.numeric(res$fit$draws()[,, param])
  tibble(
    model = res$label,
    mean = mean(g1),
    median = median(g1),
    lo90 = quantile(g1, 0.05),
    hi90 = quantile(g1, 0.95),
    lo95 = quantile(g1, 0.025),
    hi95 = quantile(g1, 0.975),
    pd = max(mean(g1 > 0), mean(g1 < 0)),
    cri_width = quantile(g1, 0.975) - quantile(g1, 0.025)
  )
}

# Note: Model C has g1_ema[1] and g1_base[1] separately
negaff_comp <- bind_rows(
  extract_negaff(res_A, "g1[1]"),
  extract_negaff(res_B, "g1[1]"),
  extract_negaff(res_C, "g1_ema[1]")
) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

cat("\n=== Negative Affectivity x stress: precision comparison ===\n")
print(negaff_comp)
write_csv(
  negaff_comp,
  here("results", "LOO", "negaffect_moderation_comparison.csv")
)

cat("\n=== LOO comparison complete ===\n")
