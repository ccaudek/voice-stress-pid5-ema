# ==============================================================================
# 11_interpret_nne_pid5_fit.R
# Interpreta posterior: NNE × PID-5 latent (measurement error)
# ==============================================================================

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(tidyverse)
  library(glue)
})

# ----------------------------
# 0) Load bundle (names, scaling info)
# ----------------------------
bundle <- readRDS("results/stan_bundle_nne_pid5.rds")
pid5_vars <- bundle$pid5_vars
D <- length(pid5_vars)

cat("PID5 domains (order in Stan):\n")
print(pid5_vars)

# ----------------------------
# 1) Load fit (cmdstanr)
# ----------------------------
# Assumo che tu abbia salvato output in stanfit, oppure ricarichi da CSV:
# Se hai fatto: fit <- mod$sample(...), puoi poi salvare:
# fit$save_object(file="results/fit_nne_pid5.rds")
#
# Qui provo prima a caricare un oggetto RDS, altrimenti chiedo CSV.
fit_path_rds <- "stan/NNE/nne_mean_pid5_moderation.rds"
if (file.exists(fit_path_rds)) {
  fit <- readRDS(fit_path_rds)
} else {
  stop(
    "Salva l'oggetto fit in results/fit_nne_pid5.rds (fit$save_object oppure saveRDS(fit, ...))."
  )
}

draws <- fit$draws()
df <- as_draws_df(draws)

# ----------------------------
# 2) Helpers
# ----------------------------
p_dir <- function(x, direction = c("gt0", "lt0")) {
  direction <- match.arg(direction)
  if (direction == "gt0") mean(x > 0) else mean(x < 0)
}
pd <- function(x) mean(x > 0) %>% (\(p) max(p, 1 - p))()

summ <- function(x) {
  tibble(
    mean = mean(x),
    median = median(x),
    q05 = quantile(x, 0.05),
    q95 = quantile(x, 0.95),
    pd = pd(x),
    p_gt0 = mean(x > 0),
    p_lt0 = mean(x < 0)
  )
}

# ----------------------------
# 3) Decide "expected direction" (set once)
# ----------------------------
# Typical hypothesis:
# - Stress worsens voice quality -> more noise -> NNE increases (less negative) => b1 > 0
# - Recovery improves quality -> NNE decreases (more negative) => b2 < 0
expected_stress <- "gt0"
expected_recover <- "lt0"

cat("\nExpected directions:\n")
cat(" - Stress (b1):", expected_stress, "\n")
cat(" - Recovery (b2):", expected_recover, "\n\n")

# ----------------------------
# 4) Main effects
# ----------------------------
cat("=== MAIN EFFECTS (NNE) ===\n")
b1 <- df$b1
b2 <- df$b2

cat("\nStress main effect b1 (PRE vs BASELINE; NNE units dB):\n")
print(summ(b1))

cat("\nRecovery main effect b2 (POST vs PRE; NNE units dB):\n")
print(summ(b2))

cat("\nP(b1 in expected direction):", p_dir(b1, expected_stress), "\n")
cat("P(b2 in expected direction):", p_dir(b2, expected_recover), "\n")

# ----------------------------
# 5) Moderation effects g1/g2 (per trait)
# ----------------------------
get_g <- function(prefix, d) df[[sprintf("%s[%d]", prefix, d)]]

mod_tbl <- map_dfr(seq_len(D), function(d) {
  g1d <- get_g("g1", d)
  g2d <- get_g("g2", d)

  tibble(
    trait = pid5_vars[d],

    # moderation on stress (interaction c1*trait)
    g1_mean = mean(g1d),
    g1_q05 = quantile(g1d, 0.05),
    g1_q95 = quantile(g1d, 0.95),
    g1_pd = pd(g1d),
    g1_p_expected = p_dir(g1d, expected_stress), # if you expect "amplification" of b1 direction

    # moderation on recovery (interaction c2*trait)
    g2_mean = mean(g2d),
    g2_q05 = quantile(g2d, 0.05),
    g2_q95 = quantile(g2d, 0.95),
    g2_pd = pd(g2d),
    g2_p_expected = p_dir(g2d, expected_recover) # if you expect trait makes recovery more negative
  )
})

cat("\n=== MODERATION (g1 = stress moderation; g2 = recovery moderation) ===\n")
print(mod_tbl %>% arrange(desc(g1_pd)), n = 50)

# ----------------------------
# 6) Simple effects at -1/0/+1 SD for each trait separately
#     effect_stress(trait=t) = b1 + g1[t]*theta
#     with theta in {-1,0,+1} and other traits = 0
# ----------------------------
simple_effects <- map_dfr(seq_len(D), function(d) {
  g1d <- get_g("g1", d)
  g2d <- get_g("g2", d)

  tibble(
    trait = pid5_vars[d],
    stress_m1sd = mean(b1 - g1d),
    stress_0sd = mean(b1),
    stress_p1sd = mean(b1 + g1d),
    recov_m1sd = mean(b2 - g2d),
    recov_0sd = mean(b2),
    recov_p1sd = mean(b2 + g2d),

    P_stress_expected_at_p1sd = p_dir(b1 + g1d, expected_stress),
    P_recov_expected_at_p1sd = p_dir(b2 + g2d, expected_recover)
  )
})

cat("\n=== SIMPLE EFFECTS (mean) at trait -1/0/+1 SD ===\n")
as.data.frame(simple_effects, n = 50)

# ----------------------------
# 7) A compact “takeaway” table (Bayesian evidence)
# ----------------------------
takeaway <- mod_tbl %>%
  transmute(
    trait,
    stress_PD = g1_pd,
    stress_P_expected = g1_p_expected,
    recovery_PD = g2_pd,
    recovery_P_expected = g2_p_expected
  ) %>%
  arrange(desc(stress_PD))

cat("\n=== TAKEAWAY (PD + P in expected direction) ===\n")
print(takeaway, n = 50)

cat("\nNotes:\n")
cat("• g1: quanto il tratto (1 SD) amplifica/riduce l'effetto stress b1.\n")
cat("• g2: quanto il tratto (1 SD) amplifica/riduce l'effetto recovery b2.\n")
cat(
  "• Per cambiare ipotesi su NNE, modifica expected_stress/expected_recover.\n"
)
