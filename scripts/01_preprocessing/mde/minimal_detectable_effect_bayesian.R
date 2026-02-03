# ==============================================================================
# minimal_detectable_effect_bayesian.R
#
# Calcola il Minimum Detectable Effect (MDE) per le interazioni trait × stress
# nel framework Bayesiano del modello di moderazione F0 ~ PID-5.
#
# Approccio: dato il design (N_subj, N_ema per soggetto, 3 timepoint voce) e i
# parametri empirici stimati dal fit reale, simuliamo dati con effetti di
# moderazione di varia dimensione e determiniamo quale sia il minimo effetto
# per cui il 95% CrI posteriore esclude lo zero con alta probabilità (es. 80%).
#
# SELF-CONTAINED: non richiede altri script. Carica i dati grezzi direttamente.
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
  library(rio)
  library(cmdstanr)
  library(posterior)
})

# ==============================================================================
# 0) CONFIGURATION
# ==============================================================================

# Target power e soglia per "rilevazione"
power_target <- 0.80
ci_level <- 0.95 # 95% credible interval

# Numero di simulazioni per ogni dimensione di effetto
n_sim <- 100 # aumentare a 200-500 per risultati più stabili (richiede tempo)

# Range di effect sizes da testare (in Hz per SD del trait)
# Basato sui tuoi risultati: effetti osservati ~3 Hz, testiamo 1-6 Hz
effect_sizes <- seq(1, 6, by = 0.5)

# Seed per riproducibilità
set.seed(42)

# ==============================================================================
# 1) LOAD RAW DATA (copiato da 01_prepare_stan_data_f0mean_pid5.R)
# ==============================================================================

cat("=== LOADING RAW DATA ===\n")

# Paths - adatta se necessario
voice_path <- here(
  "data",
  "raw",
  "acustic_features",
  "datiacustici",
  "AUDIO.xlsx"
)
ema_path <- here("data", "processed", "ema_plus_scales_cleaned.csv")

if (!file.exists(voice_path)) {
  stop(
    "Voice file not found: ",
    voice_path,
    "\nModifica voice_path nel script."
  )
}
if (!file.exists(ema_path)) {
  stop("EMA file not found: ", ema_path, "\nModifica ema_path nel script.")
}

# --- Load voice data ---
baseline <- read_excel(voice_path, sheet = "BASELINE") %>%
  mutate(timepoint = "baseline")
pre <- read_excel(voice_path, sheet = "PRE") %>% mutate(timepoint = "pre")
post <- read_excel(voice_path, sheet = "POST") %>% mutate(timepoint = "post")

df_voice <- bind_rows(baseline, pre, post)
names(df_voice) <- stringr::str_trim(names(df_voice))

# ID corrections
df_voice <- df_voice |>
  mutate(
    ID = case_when(
      ID == "am_bo_1988_08_24_166" ~ "an_bo_1988_08_24_166",
      ID == "as_li_2005_04_26_447" ~ "as_si_2005_04_26_447",
      ID == "cl_bo_1987_10_16_628" ~ "ca_bo_1987_10_16_628",
      ID == "hi_na_2005_03_08_339" ~ "gi_na_2005_03_08_339",
      ID == "ma_si_2003_10_31_940" ~ "si_ma_2003_10_31_940",
      TRUE ~ ID
    )
  )

# Extract F0 mean (average across vowels)
df_voice <- df_voice |>
  transmute(
    ID,
    timepoint = factor(timepoint, levels = c("baseline", "pre", "post")),
    f0_mean_a = `F0 mean Hz /a/`,
    f0_mean_i = `F0 mean Hz /i/`,
    f0_mean_u = `F0 mean Hz /u/`
  ) %>%
  mutate(
    y_f0 = rowMeans(across(c(f0_mean_a, f0_mean_i, f0_mean_u)), na.rm = TRUE)
  ) |>
  dplyr::select(ID, timepoint, y_f0)

# Contrasts
df_voice <- df_voice %>%
  mutate(
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
  dplyr::filter(!is.na(ID), !is.na(y_f0))

# --- Load EMA data ---
ema <- rio::import(ema_path) |> as_tibble()

pid5_ema_vars <- c(
  "pid5_negative_affectivity",
  "pid5_detachment",
  "pid5_antagonism",
  "pid5_disinhibition",
  "pid5_psychoticism"
)

df_ema <- ema |>
  transmute(
    ID = user_id,
    across(all_of(pid5_ema_vars), as.numeric)
  ) %>%
  dplyr::filter(!is.na(ID)) %>%
  dplyr::filter(ID %in% unique(df_voice$ID)) %>%
  dplyr::filter(if_any(all_of(pid5_ema_vars), ~ !is.na(.x)))

# Imputation (within-subject mean)
df_ema_imp <- df_ema |>
  group_by(ID) |>
  mutate(across(
    all_of(pid5_ema_vars),
    ~ if_else(is.na(.x), mean(.x, na.rm = TRUE), .x)
  )) |>
  ungroup() |>
  dplyr::filter(if_all(all_of(pid5_ema_vars), ~ is.finite(.x)))

# --- Create consistent subject indices ---
subj_ids <- sort(unique(intersect(df_voice$ID, df_ema_imp$ID)))
N_subj <- length(subj_ids)

id_map <- tibble(ID = subj_ids, subj = seq_len(N_subj))

df_voice_stan <- df_voice |>
  inner_join(id_map, by = "ID") |>
  arrange(subj, timepoint)

df_ema_stan <- df_ema_imp |>
  inner_join(id_map, by = "ID") |>
  arrange(subj)

cat(
  "N_subj =",
  N_subj,
  "| voice obs =",
  nrow(df_voice_stan),
  "| ema obs =",
  nrow(df_ema_stan),
  "\n"
)

# --- Standardize EMA ---
X <- df_ema_stan |>
  select(all_of(pid5_ema_vars)) |>
  as.matrix()

X_scaled <- scale(X)
pid5_center <- attr(X_scaled, "scaled:center")
pid5_scale <- attr(X_scaled, "scaled:scale")

# ==============================================================================
# 2) EXTRACT EMPIRICAL PARAMETERS FROM REAL DATA
# ==============================================================================

cat("\n=== EXTRACTING EMPIRICAL PARAMETERS ===\n")

# Calcola statistiche empiriche per la simulazione
# Questi saranno i "true" parameters (tranne g1, g2 che varieremo)

# Baseline F0 statistics
alpha_emp <- mean(
  df_voice_stan$y_f0[df_voice_stan$timepoint == "baseline"],
  na.rm = TRUE
)
sigma_y_emp <- sd(df_voice_stan$y_f0, na.rm = TRUE) * 0.5 # roughly residual SD

# Between-subject SD (from baseline)
tau1_emp <- sd(
  df_voice_stan %>%
    filter(timepoint == "baseline") %>%
    group_by(subj) %>%
    summarise(m = mean(y_f0)) %>%
    pull(m),
  na.rm = TRUE
)

# Main stress effect (empirical from your results: ~3.3 Hz)
b1_emp <- 3.3
b2_emp <- 0.1 # recovery effect near zero

# EMA measurement error (empirical SD of within-person deviations)
sigma_ema_emp <- df_ema_stan %>%
  group_by(ID) %>%
  summarise(across(all_of(pid5_ema_vars), ~ sd(.x, na.rm = TRUE))) %>%
  summarise(across(-ID, ~ mean(.x, na.rm = TRUE))) %>%
  unlist() %>%
  mean()

# Random slope SDs (small, based on your results)
tau2_emp <- 2.0
tau3_emp <- 2.0

cat("Empirical parameters:\n")
cat("  alpha (baseline F0):", round(alpha_emp, 1), "Hz\n")
cat("  b1 (stress effect):", b1_emp, "Hz\n")
cat("  b2 (recovery effect):", b2_emp, "Hz\n")
cat("  sigma_y (residual):", round(sigma_y_emp, 2), "Hz\n")
cat("  tau1 (random intercept SD):", round(tau1_emp, 2), "Hz\n")
cat("  tau2, tau3 (random slope SDs):", tau2_emp, "Hz\n")
cat("  sigma_ema:", round(sigma_ema_emp, 3), "\n")

# ==============================================================================
# 3) DATA SIMULATION FUNCTION
# ==============================================================================

#' Simula dati per il modello joint measurement-outcome
#'
#' @param N_subj numero di soggetti
#' @param n_ema_per_subj numero medio di osservazioni EMA per soggetto
#' @param g1_true vero effetto di moderazione stress (vettore D)
#' @param g2_true vero effetto di moderazione recovery (vettore D)
#' @param params lista di altri parametri (alpha, b1, b2, tau, sigma_y, sigma_ema)
#' @return lista con dati simulati nel formato Stan

simulate_data <- function(
  N_subj,
  n_ema_per_subj,
  g1_true,
  g2_true,
  params
) {
  D <- length(g1_true) # 5 domains

  # --- Latent traits (true person-level scores) ---
  # theta ~ N(0, 1) for each subject and domain
  theta <- matrix(rnorm(N_subj * D), nrow = N_subj, ncol = D)

  # --- EMA observations ---
  # Simulate variable number of EMA per subject (range around n_ema_per_subj)
  n_ema_vec <- pmax(5, round(rnorm(N_subj, n_ema_per_subj, 5)))
  N_ema <- sum(n_ema_vec)

  subj_ema <- rep(1:N_subj, times = n_ema_vec)
  X <- matrix(NA, nrow = N_ema, ncol = D)

  for (n in 1:N_ema) {
    i <- subj_ema[n]
    for (d in 1:D) {
      X[n, d] <- rnorm(1, theta[i, d], params$sigma_ema[d])
    }
  }

  # --- Voice observations (3 per subject) ---
  N_voice <- N_subj * 3

  subj_voice <- rep(1:N_subj, each = 3)
  c1 <- rep(c(-0.5, 0.5, 0.0), N_subj) # stress contrast
  c2 <- rep(c(0.0, -0.5, 0.5), N_subj) # recovery contrast

  # Random effects (non-centered)
  u <- matrix(0, N_subj, 3)
  u[, 1] <- rnorm(N_subj, 0, params$tau[1])
  u[, 2] <- rnorm(N_subj, 0, params$tau[2])
  u[, 3] <- rnorm(N_subj, 0, params$tau[3])

  # Main effects of traits on baseline (set to 0 for simplicity)
  a_trait <- rep(0, D)

  # Generate y
  y <- numeric(N_voice)
  for (j in 1:N_voice) {
    i <- subj_voice[j]

    mu <- params$alpha +
      u[i, 1] +
      (params$b1 + u[i, 2]) * c1[j] +
      (params$b2 + u[i, 3]) * c2[j]

    for (d in 1:D) {
      mu <- mu +
        a_trait[d] * theta[i, d] +
        g1_true[d] * c1[j] * theta[i, d] +
        g2_true[d] * c2[j] * theta[i, d]
    }

    y[j] <- rnorm(1, mu, params$sigma_y)
  }

  list(
    N_subj = N_subj,
    N_voice = N_voice,
    subj_voice = subj_voice,
    y = y,
    c1 = c1,
    c2 = c2,
    N_ema = N_ema,
    subj_ema = subj_ema,
    D = D,
    X = X,
    # For verification
    theta_true = theta,
    g1_true = g1_true,
    g2_true = g2_true
  )
}

# ==============================================================================
# 4) SIMPLIFIED ESTIMATION (using lmer as proxy for speed)
# ==============================================================================

# Per velocità, usiamo lmer invece del full Bayesian model.
# Questo dà una buona approssimazione del SE per calcolare MDE.
# Per un'analisi più rigorosa, si può sostituire con cmdstanr.

library(lme4)

#' Fit semplificato usando lmer
#' Usa le medie EMA per soggetto come proxy dei latent traits
#'
#' @param sim_data dati simulati
#' @return lista con stime e SE per g1 e g2

fit_simplified <- function(sim_data) {
  # Calcola medie EMA per soggetto (proxy per theta)
  theta_hat <- matrix(NA, sim_data$N_subj, sim_data$D)
  for (i in 1:sim_data$N_subj) {
    idx <- which(sim_data$subj_ema == i)
    if (length(idx) > 0) {
      theta_hat[i, ] <- colMeans(sim_data$X[idx, , drop = FALSE])
    }
  }

  # Crea dataframe per lmer
  df <- data.frame(
    y = sim_data$y,
    subj = factor(sim_data$subj_voice),
    c1 = sim_data$c1,
    c2 = sim_data$c2
  )

  # Aggiungi theta per ogni dominio
  for (d in 1:sim_data$D) {
    df[[paste0("theta", d)]] <- theta_hat[sim_data$subj_voice, d]
    df[[paste0("c1_theta", d)]] <- sim_data$c1 *
      theta_hat[sim_data$subj_voice, d]
    df[[paste0("c2_theta", d)]] <- sim_data$c2 *
      theta_hat[sim_data$subj_voice, d]
  }

  # Fit con random intercept (per velocità, no random slopes)
  formula_str <- "y ~ c1 + c2 + "
  formula_str <- paste0(
    formula_str,
    paste(paste0("theta", 1:sim_data$D), collapse = " + "),
    " + "
  )
  formula_str <- paste0(
    formula_str,
    paste(paste0("c1_theta", 1:sim_data$D), collapse = " + "),
    " + "
  )
  formula_str <- paste0(
    formula_str,
    paste(paste0("c2_theta", 1:sim_data$D), collapse = " + ")
  )
  formula_str <- paste0(formula_str, " + (1|subj)")

  fit <- suppressWarnings(suppressMessages(
    lmer(
      as.formula(formula_str),
      data = df,
      REML = FALSE,
      control = lmerControl(
        optimizer = "bobyqa",
        check.conv.singular = "ignore"
      )
    )
  ))

  # Estrai coefficienti e SE
  coefs <- summary(fit)$coefficients

  g1_est <- numeric(sim_data$D)
  g1_se <- numeric(sim_data$D)
  g2_est <- numeric(sim_data$D)
  g2_se <- numeric(sim_data$D)

  for (d in 1:sim_data$D) {
    g1_name <- paste0("c1_theta", d)
    g2_name <- paste0("c2_theta", d)

    if (g1_name %in% rownames(coefs)) {
      g1_est[d] <- coefs[g1_name, "Estimate"]
      g1_se[d] <- coefs[g1_name, "Std. Error"]
    }
    if (g2_name %in% rownames(coefs)) {
      g2_est[d] <- coefs[g2_name, "Estimate"]
      g2_se[d] <- coefs[g2_name, "Std. Error"]
    }
  }

  list(
    g1_est = g1_est,
    g1_se = g1_se,
    g2_est = g2_est,
    g2_se = g2_se
  )
}

# ==============================================================================
# 5) MDE CALCULATION VIA SIMULATION
# ==============================================================================

cat("\n=== CALCULATING MDE VIA SIMULATION ===\n")
cat("Testing effect sizes:", effect_sizes, "Hz\n")
cat("Simulations per effect size:", n_sim, "\n\n")

# Parametri per la simulazione (basati sui dati empirici)
params <- list(
  alpha = alpha_emp,
  b1 = b1_emp,
  b2 = b2_emp,
  sigma_y = sigma_y_emp,
  tau = c(tau1_emp, tau2_emp, tau3_emp),
  sigma_ema = rep(sigma_ema_emp, 5)
)

# Numero medio di EMA per soggetto nel dataset reale
n_ema_per_subj <- round(nrow(df_ema_stan) / N_subj)

cat("Design parameters:\n")
cat("  N_subj:", N_subj, "\n")
cat("  n_ema_per_subj:", n_ema_per_subj, "\n\n")

# Per semplicità, testiamo l'effetto su UN dominio (domain 1 = Negative Affectivity)
# Gli altri domini hanno g = 0

results <- tibble()

for (eff_size in effect_sizes) {
  cat("Testing effect size:", eff_size, "Hz...")

  # Effetto solo sul dominio 1 (Negative Affectivity), stress contrast
  g1_true <- c(eff_size, 0, 0, 0, 0)
  g2_true <- c(0, 0, 0, 0, 0)

  detected <- 0

  for (sim in 1:n_sim) {
    # Simula dati
    sim_data <- simulate_data(
      N_subj = N_subj,
      n_ema_per_subj = n_ema_per_subj,
      g1_true = g1_true,
      g2_true = g2_true,
      params = params
    )

    # Fit
    fit_result <- tryCatch(
      fit_simplified(sim_data),
      error = function(e) NULL
    )

    if (!is.null(fit_result)) {
      # Check se 95% CI esclude zero per dominio 1
      # Usando z = 1.96 per approssimazione
      ci_lower <- fit_result$g1_est[1] - 1.96 * fit_result$g1_se[1]
      ci_upper <- fit_result$g1_est[1] + 1.96 * fit_result$g1_se[1]

      # "Detected" se CI esclude zero nella direzione corretta
      if (ci_lower > 0 || ci_upper < 0) {
        detected <- detected + 1
      }
    }
  }

  power_est <- detected / n_sim
  cat(" Power =", round(power_est, 3), "\n")

  results <- bind_rows(
    results,
    tibble(
      effect_size = eff_size,
      n_sim = n_sim,
      n_detected = detected,
      power = power_est
    )
  )
}

# ==============================================================================
# 6) DETERMINE MDE
# ==============================================================================

cat("\n=== RESULTS ===\n")
print(results)

# Interpola per trovare MDE (effect size per cui power = 0.80)
if (any(results$power >= power_target)) {
  # Trova il minimo effect size con power >= target
  mde_idx <- min(which(results$power >= power_target))
  mde <- results$effect_size[mde_idx]

  # Interpolazione lineare più precisa
  if (mde_idx > 1) {
    x1 <- results$effect_size[mde_idx - 1]
    x2 <- results$effect_size[mde_idx]
    y1 <- results$power[mde_idx - 1]
    y2 <- results$power[mde_idx]

    # Interpola per trovare x dove y = power_target
    mde_interp <- x1 + (power_target - y1) * (x2 - x1) / (y2 - y1)
    mde <- mde_interp
  }

  cat("\n*** MINIMUM DETECTABLE EFFECT (80% power) ***\n")
  cat(
    "MDE for stress moderation (g1, domain 1):",
    round(mde, 2),
    "Hz per SD trait\n"
  )
} else {
  cat(
    "\nWARNING: Power never reached",
    power_target,
    "in tested range. Consider testing larger effect sizes.\n"
  )
  mde <- NA
}

# ==============================================================================
# 7) COMPARISON WITH OBSERVED EFFECTS
# ==============================================================================

cat("\n=== COMPARISON WITH OBSERVED EFFECTS ===\n")
cat("From your results:\n")
cat("  Negative Affectivity × Stress: 3.14 Hz [0.37, 5.89], PD = 0.97\n")
cat("  Antagonism × Recovery: 3.16 Hz [0.51, 5.78], PD = 0.97\n")
cat("\nEstimated MDE:", round(mde, 2), "Hz\n")

if (!is.na(mde)) {
  if (mde <= 3.14) {
    cat("\nInterpretation: The design was adequately powered to detect\n")
    cat("the observed moderation effects (MDE < observed effect).\n")
  } else {
    cat("\nInterpretation: The observed effects are close to or below MDE,\n")
    cat("suggesting the design had marginal power for these effect sizes.\n")
  }
}

# ==============================================================================
# 8) SAVE RESULTS
# ==============================================================================

dir.create("results", showWarnings = FALSE)

write_csv(results, "results/mde_simulation_results.csv")

mde_summary <- tibble(
  analysis = "F0 moderation by PID-5 (stress contrast)",
  N_subj = N_subj,
  n_ema_per_subj = n_ema_per_subj,
  power_target = power_target,
  MDE_Hz = round(mde, 2),
  observed_NegAff_stress = 3.14,
  observed_Antag_recovery = 3.16
)

write_csv(mde_summary, "results/mde_summary.csv")

cat("\nResults saved to:\n")
cat("  results/mde_simulation_results.csv\n")
cat("  results/mde_summary.csv\n")

# ==============================================================================
# 9) PLOT
# ==============================================================================

library(ggplot2)

p <- ggplot(results, aes(x = effect_size, y = power)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = power_target, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 3.14, linetype = "dotted", color = "blue") +
  annotate(
    "text",
    x = 3.14,
    y = 0.1,
    label = "Observed\n(Neg. Aff.)",
    hjust = -0.1,
    size = 3,
    color = "blue"
  ) +
  annotate(
    "text",
    x = max(effect_sizes),
    y = power_target + 0.03,
    label = "80% power",
    hjust = 1,
    size = 3,
    color = "red"
  ) +
  labs(
    title = "Design Sensitivity: Minimum Detectable Effect",
    subtitle = paste0(
      "N = ",
      N_subj,
      " subjects, ~",
      n_ema_per_subj,
      " EMA observations each"
    ),
    x = "Effect size (Hz per SD of latent trait)",
    y = "Power (proportion detected)"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("results/mde_power_curve.png", p, width = 8, height = 6, dpi = 300)
cat("  results/mde_power_curve.png\n")

cat("\n=== DONE ===\n")
