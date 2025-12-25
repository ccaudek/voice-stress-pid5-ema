# ==============================================================================
# 03_moderation_fit_and_save.R
# Fit & SAVE: PID-5 Traits × Stress Reactivity (only relevant outcomes)
# Output: models/mod_<outcome>_<vowel>.rds
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(brms)
  library(cmdstanr)
})

options(brms.backend = "cmdstanr")
options(max.print = 5000)

dir.create("models", showWarnings = FALSE)

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("FITTING + SAVING MODERATION MODELS: PID-5 × Stress/Recovery\n")
cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")

# ==============================================================================
# 0. LOAD DATA
# ==============================================================================
if (!exists("df_analysis")) {
  if (file.exists("results/df_analysis.rds")) {
    df_analysis <- readRDS("results/df_analysis.rds")
    message("Loaded df_analysis from results/df_analysis.rds")
  } else {
    stop(
      "Manca results/df_analysis.rds. Esegui prima 02_voice_personality_analysis_FINAL.R"
    )
  }
}

pid5_vars_c <- c(
  "pid5_negative_affectivity_c",
  "pid5_detachment_c",
  "pid5_antagonism_c",
  "pid5_disinhibition_c",
  "pid5_psychoticism_c"
)
stopifnot(all(pid5_vars_c %in% names(df_analysis)))

df_analysis <- df_analysis %>%
  mutate(timepoint = factor(timepoint, levels = c("baseline", "pre", "post")))

cat(
  "\nDati: N obs =",
  nrow(df_analysis),
  "| N soggetti =",
  n_distinct(df_analysis$ID),
  "\n"
)

# ==============================================================================
# 1. CONTRAST CODING
# ==============================================================================
df_analysis <- df_analysis %>%
  mutate(
    c1_stress = case_when(
      timepoint == "baseline" ~ -0.5,
      timepoint == "pre" ~ 0.5,
      timepoint == "post" ~ 0
    ),
    c2_recovery = case_when(
      timepoint == "baseline" ~ 0,
      timepoint == "pre" ~ -0.5,
      timepoint == "post" ~ 0.5
    )
  )

# Ensure strictly positive for lognormal variables
logpositive_vars <- c("f0_std", "jitter", "f2_std")
vowels_all <- c("a", "i", "u")

for (var in logpositive_vars) {
  for (v in vowels_all) {
    col <- paste0(var, "_", v)
    if (!col %in% names(df_analysis)) next
    bad <- which(df_analysis[[col]] <= 0)
    if (length(bad) > 0) {
      min_pos <- min(df_analysis[[col]][df_analysis[[col]] > 0], na.rm = TRUE)
      df_analysis[[col]][bad] <- min_pos
      cat("Fixed", length(bad), "values <=0 in", col, "\n")
    }
  }
}

# ==============================================================================
# 2. TARGETS: solo outcome/vocali con effetto osservato
# ==============================================================================
targets <- tribble(
  ~outcome,
  ~vowel,
  ~family_key,
  "f0_mean",
  "a",
  "gaussian",
  "f0_mean",
  "i",
  "gaussian",
  "f0_mean",
  "u",
  "gaussian",
  "nne",
  "a",
  "gaussian",
  "f2_mean",
  "a",
  "student",
  "f2_mean",
  "i",
  "student",
  "f0_std",
  "a",
  "lognormal",
  "jitter",
  "a",
  "lognormal"
)

family_map <- list(
  gaussian = gaussian(),
  lognormal = lognormal(),
  student = student()
)

traits <- paste(pid5_vars_c, collapse = " + ")

build_formula <- function(colname) {
  as.formula(paste0(
    colname,
    " ~ ",
    "c1_stress * (",
    traits,
    ") + ",
    "c2_recovery * (",
    traits,
    ") + ",
    "(1 + c1_stress + c2_recovery || ID)"
  ))
}

# ==============================================================================
# 3. PRIORS (stabili, coerenti con main effects)
# ==============================================================================
make_priors <- function(outcome) {
  if (outcome == "f0_mean") {
    c(
      prior(student_t(3, 220, 30), class = "Intercept"),
      prior(normal(0, 10), class = "b"),
      prior(exponential(0.3), class = "sigma"),
      prior(exponential(0.3), class = "sd")
    )
  } else if (outcome == "nne") {
    c(
      prior(student_t(3, -20, 5), class = "Intercept"),
      prior(normal(0, 5), class = "b"),
      prior(exponential(0.5), class = "sigma"),
      prior(exponential(0.3), class = "sd")
    )
  } else if (outcome == "f2_mean") {
    c(
      prior(student_t(3, 1500, 150), class = "Intercept"),
      prior(normal(0, 50), class = "b"),
      prior(exponential(0.01), class = "sigma"),
      prior(exponential(0.01), class = "sd"),
      prior(gamma(2, 0.1), class = "nu")
    )
  } else if (outcome %in% c("f0_std", "jitter", "f2_std")) {
    c(
      prior(student_t(3, 0, 1), class = "Intercept"),
      prior(normal(0, 0.5), class = "b"),
      prior(exponential(1), class = "sigma"),
      prior(exponential(0.5), class = "sd")
    )
  } else stop("Outcome non gestito: ", outcome)
}

# ==============================================================================
# 4. FIT + SAVE
# ==============================================================================
iter <- 6000
warmup <- 3000
chains <- 4
cores <- 4
seed <- 123
control <- list(adapt_delta = 0.995, max_treedepth = 18)

for (k in seq_len(nrow(targets))) {
  out <- targets$outcome[k]
  v <- targets$vowel[k]
  famk <- targets$family_key[k]

  colname <- paste0(out, "_", v)
  if (!colname %in% names(df_analysis)) {
    cat("Skipping (colonna mancante):", colname, "\n")
    next
  }

  model_name <- paste0("mod_", out, "_", v)
  file_path <- file.path("models", paste0(model_name, ".rds"))

  cat("\n", rep("-", 70), "\n", sep = "")
  cat("Fitting:", model_name, " (", colname, ")\n", sep = "")
  cat("Saving to:", file_path, "\n")
  cat(rep("-", 70), "\n", sep = "")

  fit <- brm(
    formula = bf(build_formula(colname)),
    data = df_analysis,
    family = family_map[[famk]],
    prior = make_priors(out),
    iter = iter,
    warmup = warmup,
    chains = chains,
    cores = cores,
    seed = seed,
    control = control
  )

  saveRDS(fit, file_path)

  # quick diagnostics
  rh <- rhat(fit)
  np <- nuts_params(fit)
  cat(
    "Convergence: max Rhat =",
    round(max(rh, na.rm = TRUE), 3),
    "| divergences =",
    sum(subset(np, Parameter == "divergent__")$Value),
    "\n"
  )
}

cat("\n=== DONE: modelli di moderazione salvati in models/mod_*.rds ===\n")
