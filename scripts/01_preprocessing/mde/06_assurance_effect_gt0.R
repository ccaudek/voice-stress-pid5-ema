# ==============================================================================
# 06_assurance_effect_gt0.R
#
# Bayesian assurance (posterior predictive) for replication success:
# Success = estimated moderation effect > 0
#
# Target parameter (default): g1[1] = Negative Affectivity × Stress (c1 * theta1)
#
# It:
#  - Loads existing Stan fit (from 02_f0mean_pid5_moderation.R pipeline)
#  - Draws parameters from posterior
#  - Simulates NEW datasets with same design
#  - Fits a FAST proxy model (lmer) to each simulated dataset
#  - Computes assurance = P(success) with uncertainty
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(cmdstanr)
  library(posterior)
  library(lme4)
})

# ==============================================================================
# 0) CONFIG
# ==============================================================================

set.seed(123)

# --- Inputs produced by your existing pipeline (02 script) ---
bundle_path <- "results/stan_bundle_f0mean_pid5.rds"
fit_path <- "stan/F0/f0mean_pid5_moderation.RDS"

# --- Which effect defines "success" ---
# g_kind: "g1" (stress moderation) or "g2" (recovery moderation)
g_kind <- "g1"
domain_index <- 1 # 1 = Negative Affectivity
success_rule <- "estimate_gt0" # currently only this

# --- Monte Carlo settings ---
M <- 1000 # number of posterior-predictive replications (try 2000+ for stability)
max_fail_frac <- 0.20 # if too many fits fail, warn

# --- lmer fitting options ---
use_random_slopes <- FALSE # TRUE = slower / may increase convergence issues

# --- Output ---
out_dir <- "results/assurance"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_csv <- file.path(out_dir, "assurance_gt0_results.csv")
out_rds <- file.path(out_dir, "assurance_gt0_results.rds")

# ==============================================================================
# 1) LOAD BUNDLE + FIT
# ==============================================================================

if (!file.exists(bundle_path)) {
  stop(
    "Bundle not found: ",
    bundle_path,
    "\nRun 01_prepare_stan_data_f0mean_pid5.R first (or your pipeline)."
  )
}
if (!file.exists(fit_path)) {
  stop(
    "Stan fit not found: ",
    fit_path,
    "\nRun 02_f0mean_pid5_moderation.R first (or save the fit in this path)."
  )
}

bundle <- readRDS(bundle_path)
stan_data <- bundle$stan_data
pid5_vars <- bundle$pid5_vars

fit <- readRDS(fit_path)

cat("=== LOADED ===\n")
cat("N_subj:", stan_data$N_subj, "\n")
cat("N_voice:", stan_data$N_voice, "\n")
cat("N_ema:", stan_data$N_ema, "\n")
cat("Domains:", stan_data$D, paste(pid5_vars, collapse = ", "), "\n\n")

# ==============================================================================
# 2) DEFINE "DESIGN" FOR REPLICATION
#    - same N_subj
#    - same contrasts (3 voice obs per subject)
#    - similar EMA intensity: use observed n_ema per subject distribution
# ==============================================================================

N_subj <- stan_data$N_subj
D <- stan_data$D

# contrasts for voice: same as in your Stan data (already built)
# keep the canonical 3 obs/subject pattern
c1 <- rep(c(-0.5, 0.5, 0.0), N_subj)
c2 <- rep(c(0.0, -0.5, 0.5), N_subj)
subj_voice <- rep(1:N_subj, each = 3)
N_voice <- length(subj_voice)

# empirical EMA counts per subject from the observed dataset
ema_counts <- tabulate(stan_data$subj_ema, nbins = N_subj)
if (any(ema_counts == 0)) {
  warning(
    "Some subjects have 0 EMA in observed data. This may break measurement model."
  )
}
# For a "new sample", we reuse the same count distribution but reassign to new IDs
ema_counts_rep <- sample(ema_counts, size = N_subj, replace = FALSE)
N_ema <- sum(ema_counts_rep)
subj_ema <- rep(1:N_subj, times = ema_counts_rep)

design <- list(
  N_subj = N_subj,
  D = D,
  N_voice = N_voice,
  subj_voice = subj_voice,
  c1 = c1,
  c2 = c2,
  N_ema = N_ema,
  subj_ema = subj_ema
)

# ==============================================================================
# 3) EXTRACT POSTERIOR DRAWS FOR PARAMETERS NEEDED TO SIMULATE
# ==============================================================================

# We do NOT need subject-level theta or u from posterior because a replication
# draws new subjects: theta ~ N(0,1), z_u ~ N(0,1).
# We DO need population-level parameters: alpha, b1, b2, a_trait[], g1[], g2[],
# tau[], sigma_y, sigma_ema[].

param_vars <- c(
  "alpha",
  "b1",
  "b2",
  "sigma_y",
  "tau[1]",
  "tau[2]",
  "tau[3]",
  paste0("a_trait[", 1:D, "]"),
  paste0("g1[", 1:D, "]"),
  paste0("g2[", 1:D, "]"),
  paste0("sigma_ema[", 1:D, "]")
)

post_df <- as_draws_df(fit$draws(param_vars))

cat("Posterior draws available:", nrow(post_df), "\n\n")

# ==============================================================================
# 4) SIMULATION FROM ONE POSTERIOR DRAW
#    This matches the generative structure of f0mean_pid5_moderation.stan
# ==============================================================================

simulate_from_draw <- function(draw_row, design) {
  N_subj <- design$N_subj
  D <- design$D

  # unpack parameters
  alpha <- draw_row[["alpha"]]
  b1 <- draw_row[["b1"]]
  b2 <- draw_row[["b2"]]
  sigma_y <- draw_row[["sigma_y"]]

  tau <- c(draw_row[["tau[1]"]], draw_row[["tau[2]"]], draw_row[["tau[3]"]])

  a_trait <- map_dbl(1:D, ~ draw_row[[paste0("a_trait[", .x, "]")]])
  g1 <- map_dbl(1:D, ~ draw_row[[paste0("g1[", .x, "]")]])
  g2 <- map_dbl(1:D, ~ draw_row[[paste0("g2[", .x, "]")]])
  sigma_ema <- map_dbl(1:D, ~ draw_row[[paste0("sigma_ema[", .x, "]")]])

  # new-subject latent traits and random effects
  theta <- matrix(rnorm(N_subj * D, 0, 1), nrow = N_subj, ncol = D)

  z_u <- matrix(rnorm(N_subj * 3, 0, 1), nrow = N_subj, ncol = 3)
  u <- sweep(z_u, 2, tau, `*`) # u[,k] = z_u[,k] * tau[k]

  # simulate EMA X (standardized scale)
  N_ema <- design$N_ema
  subj_ema <- design$subj_ema
  X <- matrix(NA_real_, nrow = N_ema, ncol = D)
  for (n in 1:N_ema) {
    i <- subj_ema[n]
    for (d in 1:D) {
      X[n, d] <- rnorm(1, mean = theta[i, d], sd = sigma_ema[d])
    }
  }

  # simulate voice outcome y
  N_voice <- design$N_voice
  subj_voice <- design$subj_voice
  c1 <- design$c1
  c2 <- design$c2
  y <- numeric(N_voice)

  for (j in 1:N_voice) {
    i <- subj_voice[j]
    mu <- alpha +
      u[i, 1] +
      (b1 + u[i, 2]) * c1[j] +
      (b2 + u[i, 3]) * c2[j]

    # trait main + interactions
    for (d in 1:D) {
      mu <- mu +
        a_trait[d] * theta[i, d] +
        g1[d] * c1[j] * theta[i, d] +
        g2[d] * c2[j] * theta[i, d]
    }

    y[j] <- rnorm(1, mu, sigma_y)
  }

  list(
    y = y,
    subj_voice = subj_voice,
    c1 = c1,
    c2 = c2,
    X = X,
    subj_ema = subj_ema,
    theta_true = theta
  )
}

# ==============================================================================
# 5) FAST ANALYSIS (PROXY): lmer using subject-level EMA means as theta-hat
#    Returns estimate for the target interaction term.
# ==============================================================================

fit_proxy_lmer <- function(sim) {
  # subject means of EMA are proxies for theta
  D <- ncol(sim$X)
  N_subj <- max(sim$subj_voice)

  theta_hat <- matrix(NA_real_, nrow = N_subj, ncol = D)
  for (i in 1:N_subj) {
    idx <- which(sim$subj_ema == i)
    theta_hat[i, ] <- colMeans(sim$X[idx, , drop = FALSE])
  }

  df <- data.frame(
    y = sim$y,
    subj = factor(sim$subj_voice),
    c1 = sim$c1,
    c2 = sim$c2
  )

  for (d in 1:D) {
    df[[paste0("theta", d)]] <- theta_hat[sim$subj_voice, d]
    df[[paste0("c1_theta", d)]] <- sim$c1 * theta_hat[sim$subj_voice, d]
    df[[paste0("c2_theta", d)]] <- sim$c2 * theta_hat[sim$subj_voice, d]
  }

  fixed_terms <- c(
    "c1",
    "c2",
    paste0("theta", 1:D),
    paste0("c1_theta", 1:D),
    paste0("c2_theta", 1:D)
  )

  fixed_part <- paste(fixed_terms, collapse = " + ")

  if (use_random_slopes) {
    # closer to the Stan structure but slower/more fragile
    rand_part <- "(1 + c1 + c2 | subj)"
  } else {
    rand_part <- "(1 | subj)"
  }

  fml <- as.formula(paste("y ~", fixed_part, "+", rand_part))

  fit <- suppressWarnings(suppressMessages(
    lmer(
      fml,
      data = df,
      REML = FALSE,
      control = lmerControl(
        optimizer = "bobyqa",
        check.conv.singular = "ignore"
      )
    )
  ))

  coefs <- summary(fit)$coefficients
  list(
    coefs = coefs,
    # return only the estimate for convenience
    target_est = unname(coefs[
      paste0(g_kind, "_theta", domain_index),
      "Estimate"
    ])
  )
}

# NOTE: in fit_proxy_lmer we used name "c1_theta{d}" etc.
# so the target term name depends on g_kind:
#  - g1 corresponds to c1_theta{d}
#  - g2 corresponds to c2_theta{d}
# We'll map that explicitly:
target_term_name <- function(g_kind, d) {
  if (g_kind == "g1") return(paste0("c1_theta", d))
  if (g_kind == "g2") return(paste0("c2_theta", d))
  stop("g_kind must be 'g1' or 'g2'")
}

# adjust proxy function to return correct term
fit_proxy_lmer <- function(sim) {
  D <- ncol(sim$X)
  N_subj <- max(sim$subj_voice)

  theta_hat <- matrix(NA_real_, nrow = N_subj, ncol = D)
  for (i in 1:N_subj) {
    idx <- which(sim$subj_ema == i)
    theta_hat[i, ] <- colMeans(sim$X[idx, , drop = FALSE])
  }

  df <- data.frame(
    y = sim$y,
    subj = factor(sim$subj_voice),
    c1 = sim$c1,
    c2 = sim$c2
  )

  for (d in 1:D) {
    df[[paste0("theta", d)]] <- theta_hat[sim$subj_voice, d]
    df[[paste0("c1_theta", d)]] <- sim$c1 * theta_hat[sim$subj_voice, d]
    df[[paste0("c2_theta", d)]] <- sim$c2 * theta_hat[sim$subj_voice, d]
  }

  fixed_terms <- c(
    "c1",
    "c2",
    paste0("theta", 1:D),
    paste0("c1_theta", 1:D),
    paste0("c2_theta", 1:D)
  )
  fixed_part <- paste(fixed_terms, collapse = " + ")

  rand_part <- if (use_random_slopes) "(1 + c1 + c2 | subj)" else "(1 | subj)"
  fml <- as.formula(paste("y ~", fixed_part, "+", rand_part))

  fit <- suppressWarnings(suppressMessages(
    lmer(
      fml,
      data = df,
      REML = FALSE,
      control = lmerControl(
        optimizer = "bobyqa",
        check.conv.singular = "ignore"
      )
    )
  ))

  coefs <- summary(fit)$coefficients
  term <- target_term_name(g_kind, domain_index)

  if (!(term %in% rownames(coefs))) {
    stop("Target term not found in lmer coefficients: ", term)
  }

  list(
    term = term,
    est = unname(coefs[term, "Estimate"]),
    se = unname(coefs[term, "Std. Error"])
  )
}

# ==============================================================================
# 6) MONTE CARLO: POSTERIOR PREDICTIVE REPLICATIONS
# ==============================================================================

cat("=== ASSURANCE SIMULATION ===\n")
cat(
  "Target effect:",
  g_kind,
  "[",
  domain_index,
  "]",
  "(",
  pid5_vars[domain_index],
  ")\n",
  sep = ""
)
cat("Success definition: estimate > 0\n")
cat("Replications M =", M, "\n\n")

success <- rep(NA, M)
estimates <- rep(NA_real_, M)
fail <- 0L

for (m in 1:M) {
  # sample one posterior draw row
  draw_row <- post_df[sample.int(nrow(post_df), 1), ]

  sim <- simulate_from_draw(draw_row, design)

  fit_res <- tryCatch(
    fit_proxy_lmer(sim),
    error = function(e) NULL
  )

  if (is.null(fit_res)) {
    fail <- fail + 1L
    next
  }

  estimates[m] <- fit_res$est
  success[m] <- as.integer(fit_res$est > 0)
}

fail_frac <- fail / M
if (fail_frac > max_fail_frac) {
  warning("High fit failure rate: ", round(100 * fail_frac, 1), "%")
}

assurance_hat <- mean(success, na.rm = TRUE)

# Uncertainty for assurance: Beta posterior on Bernoulli success
# (Jeffreys prior Beta(0.5, 0.5) is a common default)
s <- sum(success == 1, na.rm = TRUE)
f <- sum(success == 0, na.rm = TRUE)
assurance_ci <- qbeta(c(0.025, 0.5, 0.975), 0.5 + s, 0.5 + f)

cat("\n=== RESULTS ===\n")
cat("Successful replications:", s, "\n")
cat("Failed (est <= 0):", f, "\n")
cat("Fit failures:", fail, "\n")
cat("Assurance P(success):", round(assurance_hat, 3), "\n")
cat(
  "95% CrI for P(success): [",
  round(assurance_ci[1], 3),
  ", ",
  round(assurance_ci[3], 3),
  "]\n",
  sep = ""
)

# ==============================================================================
# 7) SAVE OUTPUT
# ==============================================================================

res <- tibble(
  M = M,
  target = paste0(g_kind, "[", domain_index, "]"),
  trait = pid5_vars[domain_index],
  success_rule = success_rule,
  use_random_slopes = use_random_slopes,
  assurance = assurance_hat,
  assurance_q025 = assurance_ci[1],
  assurance_q50 = assurance_ci[2],
  assurance_q975 = assurance_ci[3],
  n_success = s,
  n_fail = f,
  n_fit_fail = fail
)

write_csv(res, out_csv)
saveRDS(list(summary = res, success = success, estimates = estimates), out_rds)

cat("\nSaved:\n")
cat(" -", out_csv, "\n")
cat(" -", out_rds, "\n")

# Optional: quick histogram of estimates
png(file.path(out_dir, "target_estimates_hist.png"), width = 900, height = 600)
hist(
  estimates,
  breaks = 40,
  main = paste0(
    "Posterior-predictive estimates: ",
    g_kind,
    "[",
    domain_index,
    "] > 0 success"
  ),
  xlab = "Estimated interaction (Hz per SD trait)"
)
abline(v = 0, lty = 2)
dev.off()

cat(" -", file.path(out_dir, "target_estimates_hist.png"), "\n")
cat("\n=== DONE ===\n")

#' Versione breve (metodi/risultati)
#' Using posterior predictive simulations, we estimated the probability of
#' replicating a positive moderation effect with the same study design. Under
#' the fitted model, the probability that the estimated Negative Affectivity ×
#' Stress interaction would be positive in a new sample was 0.92 (95% credible
#' interval: 0.90–0.94).
#'
#' Versione interpretativa (discussion)
#' Posterior predictive analyses indicate a high probability that the direction
#' of the moderation effect would replicate under the same study design.
#' Specifically, simulations based on the fitted model suggest that a positive
#' interaction effect would be observed in the large majority of replications.
#' This indicates that the observed moderation effect is directionally robust,
#' even though the magnitude of the effect remains associated with substantial
#' uncertainty.
