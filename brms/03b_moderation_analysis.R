# ==============================================================================
# Bayesian Moderation Analysis: PID-5 × Stress Reactivity
# STAMPA SOLO EFFETTI SIGNIFICATIVI + TRASFORMAZIONI "NATURALI" (no save)
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(brms)
  library(cmdstanr)
  library(stringr)
})

options(brms.backend = "cmdstanr")
options(max.print = 5000)

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("MODERATION ANALYSIS: PID-5 × Stress/Recovery (SIGNIFICANT ONLY)\n")
cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")

# ==============================================================================
# 0. LOAD DATA
# ==============================================================================

if (!exists("df_analysis")) {
  if (file.exists("results/df_analysis.rds")) {
    df_analysis <- readRDS("results/df_analysis.rds")
    message("Loaded df_analysis from results/df_analysis.rds")
  } else {
    stop("Manca results/df_analysis.rds. Esegui prima lo script 02_*.R")
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

# Lognormal: assicura >0 solo per le colonne coinvolte
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
# 2. TARGETS: solo outcome/vocali con evidenza stress nei main effects
# (dalla tua tabella di effetti "significativi")
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

make_priors <- function(outcome, family_key) {
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
      prior(normal(0, 0.5), class = "b"), # shrinkage su log-scale
      prior(exponential(1), class = "sigma"),
      prior(exponential(0.5), class = "sd")
    )
  } else stop("Outcome non gestito: ", outcome)
}

# ==============================================================================
# 3. FIT (no save)
# ==============================================================================
iter <- 6000
warmup <- 3000
chains <- 4
cores <- 4
seed <- 123
control <- list(adapt_delta = 0.995, max_treedepth = 18)

fitted <- list()

for (k in seq_len(nrow(targets))) {
  out <- targets$outcome[k]
  v <- targets$vowel[k]
  famk <- targets$family_key[k]

  colname <- paste0(out, "_", v)
  if (!colname %in% names(df_analysis)) {
    cat("Skipping (colonna mancante):", colname, "\n")
    next
  }

  cat("\n", rep("-", 70), "\n", sep = "")
  cat("Fitting:", colname, "| family:", famk, "\n")
  cat(rep("-", 70), "\n", sep = "")

  fit <- brm(
    formula = bf(build_formula(colname)),
    data = df_analysis,
    family = family_map[[famk]],
    prior = make_priors(out, famk),
    iter = iter,
    warmup = warmup,
    chains = chains,
    cores = cores,
    seed = seed,
    control = control
  )

  mn <- paste0("mod_", colname)
  fitted[[mn]] <- fit

  # quick convergence
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

# ==============================================================================
# 4. ESTRAI SOLO EFFETTI SIGNIFICATIVI + TRASFORMAZIONI
# ==============================================================================
is_sig <- function(l, u) !is.na(l) && !is.na(u) && (l > 0 || u < 0)

# quali outcome sono lognormal (interpretazione ratio)
is_lognormal_outcome <- function(model_name) {
  str_detect(model_name, "mod_(f0_std|jitter|f2_std)_")
}

# parse outcome/vowel dal nome "mod_f0_mean_a"
parse_model <- function(mn) {
  x <- sub("^mod_", "", mn)
  parts <- str_split(x, "_", simplify = TRUE)
  vowel <- parts[ncol(parts)]
  outcome <- paste(parts[1:(ncol(parts) - 1)], collapse = "_")
  list(outcome = outcome, vowel = vowel)
}

extract_sig <- function(fit, mn) {
  info <- parse_model(mn)
  fe <- fixef(fit, summary = TRUE)

  tb <- tibble(
    model = mn,
    outcome = info$outcome,
    vowel = info$vowel,
    parameter = rownames(fe),
    estimate = fe[, "Estimate"],
    l = fe[, "Q2.5"],
    u = fe[, "Q97.5"]
  ) %>%
    # teniamo solo: stress/recovery + interazioni coi tratti
    filter(
      parameter %in%
        c("c1_stress", "c2_recovery") |
        str_detect(parameter, "^c[12]_(stress|recovery):pid5_")
    ) %>%
    mutate(
      effect = case_when(
        parameter == "c1_stress" ~ "Main: Stress (PRE vs BASELINE)",
        parameter == "c2_recovery" ~ "Main: Recovery (POST vs PRE)",
        str_detect(parameter, "^c1_stress:") ~ "Interaction: Trait × Stress",
        str_detect(parameter, "^c2_recovery:") ~
          "Interaction: Trait × Recovery",
        TRUE ~ "Other"
      ),
      significant = map2_lgl(l, u, is_sig),
      scale = if_else(
        is_lognormal_outcome(mn),
        "lognormal (log-scale)",
        "linear"
      )
    ) %>%
    filter(significant)

  if (nrow(tb) == 0) return(NULL)

  # trasformazioni per lognormal: ratio e %change
  tb <- tb %>%
    mutate(
      ratio = if_else(
        scale == "lognormal (log-scale)",
        exp(estimate),
        NA_real_
      ),
      ratio_l = if_else(scale == "lognormal (log-scale)", exp(l), NA_real_),
      ratio_u = if_else(scale == "lognormal (log-scale)", exp(u), NA_real_),
      pct = if_else(
        scale == "lognormal (log-scale)",
        (exp(estimate) - 1) * 100,
        NA_real_
      ),
      pct_l = if_else(
        scale == "lognormal (log-scale)",
        (exp(l) - 1) * 100,
        NA_real_
      ),
      pct_u = if_else(
        scale == "lognormal (log-scale)",
        (exp(u) - 1) * 100,
        NA_real_
      )
    )

  tb
}

sig_all <- imap_dfr(fitted, extract_sig)

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("SIGNIFICANT ONLY (95% CrI esclude 0)\n")
cat("Per lognormal: mostro anche ratio=exp(beta) e %change\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

if (nrow(sig_all) == 0) {
  cat(
    "Nessun effetto significativo (né main c1/c2 né interazioni) con CrI95% che esclude 0.\n"
  )
} else {
  # stampa “naturale”:
  # - per linear: Estimate [l,u]
  # - per lognormal: %change [l,u] e ratio [l,u]
  linear_tbl <- sig_all %>%
    filter(scale == "linear") %>%
    mutate(across(c(estimate, l, u), ~ round(.x, 3))) %>%
    select(outcome, vowel, effect, parameter, estimate, l, u) %>%
    arrange(outcome, vowel, effect, parameter)

  log_tbl <- sig_all %>%
    filter(scale != "linear") %>%
    mutate(
      across(c(estimate, l, u), ~ round(.x, 3)),
      across(c(ratio, ratio_l, ratio_u), ~ round(.x, 3)),
      across(c(pct, pct_l, pct_u), ~ round(.x, 1))
    ) %>%
    select(
      outcome,
      vowel,
      effect,
      parameter,
      estimate,
      l,
      u,
      ratio,
      ratio_l,
      ratio_u,
      pct,
      pct_l,
      pct_u
    ) %>%
    arrange(outcome, vowel, effect, parameter)

  if (nrow(linear_tbl) > 0) {
    cat("=== LINEAR outcomes (Hz / dB / Hz) ===\n")
    as.data.frame(linear_tbl, n = Inf)
    cat("\n")
  }

  if (nrow(log_tbl) > 0) {
    cat("=== LOGNORMAL outcomes (interpretazione moltiplicativa) ===\n")
    cat(
      "pct = variazione percentuale per +1 SD nel tratto (o per unità di c1/c2)\n\n"
    )
    as.data.frame(log_tbl, n = Inf)
    cat("\n")
  }

  cat("Interpretazione rapida:\n")
  cat(
    "• Interazione c1_stress:pid5_X_c > 0 su f0_mean => tratto X amplifica l'aumento di pitch PRE vs baseline.\n"
  )
  cat(
    "• Interazione c2_recovery:pid5_X_c < 0 => tratto X peggiora il recupero (POST resta più 'stress-like').\n"
  )
  cat(
    "• Per lognormal: pct>0 = aumento (%), pct<0 = riduzione (%) della misura.\n"
  )
}

cat("\n=== DONE ===\n")


library(tidyverse)
library(brms)
library(posterior)

# helper: carica modelli specifici
load_m <- function(path) readRDS(path)

# estrai draws di un parametro brms "b_timepointpre" etc.
get_draws <- function(fit, par) {
  d <- as_draws_df(fit)
  stopifnot(par %in% names(d))
  d[[par]]
}

p_dir <- function(draws, direction = c("gt0", "lt0")) {
  direction <- match.arg(direction)
  if (direction == "gt0") mean(draws > 0) else mean(draws < 0)
}

# --- esempio: F0 mean, PRE vs baseline (timepointpre) per a/i/u
m_a <- load_m("models/m1_f0_mean_a.rds")
m_i <- load_m("models/m1_f0_mean_i.rds")
m_u <- load_m("models/m1_f0_mean_u.rds")

da <- get_draws(m_a, "b_timepointpre")
di <- get_draws(m_i, "b_timepointpre")
du <- get_draws(m_u, "b_timepointpre")

# PD per direzione attesa (stress ↑ F0 => >0)
P_a <- p_dir(da, "gt0")
P_i <- p_dir(di, "gt0")
P_u <- p_dir(du, "gt0")

cat("P(pre>0) F0mean a/i/u:", P_a, P_i, P_u, "\n")

# sintesi: probabilità che l'effetto sia positivo in almeno 2 vocali su 3
# (allineiamo la lunghezza dei draw)
n <- min(length(da), length(di), length(du))
K <- (da[1:n] > 0) + (di[1:n] > 0) + (du[1:n] > 0)

cat("P(>=2 vocali positive):", mean(K >= 2), "\n")
cat("P(tutte e 3 positive):", mean(K == 3), "\n")
