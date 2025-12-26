# ==============================================================================
# 05_fit_stan_within_person_model.R
# Fitting del modello Stan per covariazione within-person
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(cmdstanr)
  library(posterior)
  library(bayesplot)
  library(loo)
  library(here)
})

# ==============================================================================
# 0) CONFIGURAZIONE
# ==============================================================================

# Percorsi
model_path <- "stan/within_person_covariation/within_person_covariation.stan"
data_path <- "results/within_person_covariation/within_person_bundle.rds"
output_dir <- "results/within_person_stan"

# Crea directory output
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Parametri MCMC
n_chains <- 4
n_iter_warmup <- 1000
n_iter_sampling <- 1000
n_thin <- 1
n_cores <- parallel::detectCores() - 1

# ==============================================================================
# 1) CARICA DATI
# ==============================================================================

cat("=== CARICAMENTO DATI ===\n")

bundle <- readRDS(data_path)
stan_data <- bundle$stan_data
df_within <- bundle$df_within
pid5_vars <- bundle$pid5_vars

cat(sprintf(
  "Dati: N = %d, N_subj = %d, D = %d domini PID-5\n",
  stan_data$N,
  stan_data$N_subj,
  stan_data$D
))

# Verifica assenza NA
stopifnot(!anyNA(stan_data$X_wp))
stopifnot(!anyNA(stan_data$y_wp))

# ==============================================================================
# 2) COMPILA MODELLO STAN
# ==============================================================================

cat("\n=== COMPILAZIONE MODELLO STAN ===\n")

if (!file.exists(model_path)) {
  stop("File modello Stan non trovato: ", model_path)
}

mod <- cmdstan_model(model_path)

cat("Modello compilato con successo.\n")

# ==============================================================================
# 3) FITTING
# ==============================================================================

cat("\n=== FITTING MODELLO ===\n")
cat(sprintf(
  "Chains: %d | Warmup: %d | Sampling: %d | Thin: %d\n",
  n_chains,
  n_iter_warmup,
  n_iter_sampling,
  n_thin
))

fit <- mod$sample(
  data = stan_data,
  chains = n_chains,
  parallel_chains = min(n_chains, n_cores),
  iter_warmup = n_iter_warmup,
  iter_sampling = n_iter_sampling,
  thin = n_thin,
  refresh = 100,
  adapt_delta = 0.95,
  max_treedepth = 12,
  seed = 12345,
  output_dir = output_dir,
  output_basename = "within_person_fit"
)

# ==============================================================================
# 4) DIAGNOSTICS
# ==============================================================================

cat("\n=== DIAGNOSTICS ===\n")

# Rhat e ESS
diag_summary <- fit$summary(
  variables = c("beta_wp", "alpha", "sigma_subj", "sigma_y", "r2_within")
)

cat("\nParametri principali:\n")
print(diag_summary)

# Controlla convergenza
max_rhat <- max(diag_summary$rhat, na.rm = TRUE)
min_ess <- min(c(diag_summary$ess_bulk, diag_summary$ess_tail), na.rm = TRUE)

cat(sprintf(
  "\nConvergenza: max(Rhat) = %.3f, min(ESS) = %.0f\n",
  max_rhat,
  min_ess
))

if (max_rhat > 1.01) {
  warning("Possibili problemi di convergenza: Rhat > 1.01")
}

# Salva summary
write_csv(
  diag_summary,
  file.path(output_dir, "parameter_summary.csv")
)

# ==============================================================================
# 5) ESTRAZIONE POSTERIOR
# ==============================================================================

cat("\n=== ESTRAZIONE POSTERIOR ===\n")

# Draws
draws <- fit$draws(format = "df")

# Beta within-person
beta_wp_draws <- fit$draws("beta_wp", format = "matrix")

# Summary beta_wp con nomi domini
beta_wp_summary <- tibble(
  domain = pid5_vars,
  mean = colMeans(beta_wp_draws),
  median = apply(beta_wp_draws, 2, median),
  sd = apply(beta_wp_draws, 2, sd),
  q025 = apply(beta_wp_draws, 2, quantile, 0.025),
  q975 = apply(beta_wp_draws, 2, quantile, 0.975),
  prob_pos = colMeans(beta_wp_draws > 0)
)

cat("\nCoefficienti within-person (beta_wp):\n")
print(beta_wp_summary)

write_csv(
  beta_wp_summary,
  file.path(output_dir, "beta_wp_summary.csv")
)

# R² within-person
r2_within <- fit$draws("r2_within", format = "matrix")
r2_summary <- tibble(
  parameter = "r2_within",
  mean = mean(r2_within),
  median = median(r2_within),
  sd = sd(r2_within),
  q025 = quantile(r2_within, 0.025),
  q975 = quantile(r2_within, 0.975)
)

cat("\nR² within-person:\n")
print(r2_summary)

write_csv(
  r2_summary,
  file.path(output_dir, "r2_within_summary.csv")
)

# ==============================================================================
# 6) VISUALIZZAZIONI POSTERIOR
# ==============================================================================

cat("\n=== CREAZIONE GRAFICI ===\n")

# Trace plots
pdf(file.path(output_dir, "trace_plots.pdf"), width = 12, height = 8)
mcmc_trace(draws, pars = c("alpha", "sigma_subj", "sigma_y", "r2_within"))
dev.off()

# Density plots beta_wp
pdf(file.path(output_dir, "beta_wp_densities.pdf"), width = 12, height = 8)
mcmc_dens(draws, pars = paste0("beta_wp[", 1:stan_data$D, "]")) +
  labs(title = "Posterior Distributions: Within-Person Coefficients (beta_wp)")
dev.off()

# Intervals plot beta_wp
pdf(file.path(output_dir, "beta_wp_intervals.pdf"), width = 10, height = 6)
mcmc_intervals(
  draws,
  pars = paste0("beta_wp[", 1:stan_data$D, "]"),
  prob = 0.5,
  prob_outer = 0.95
) +
  scale_y_discrete(labels = pid5_vars) +
  labs(
    title = "Within-Person Coefficients: 50% and 95% Credible Intervals",
    x = "Coefficient Value"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red")
dev.off()

# R² distribution
pdf(file.path(output_dir, "r2_within_distribution.pdf"), width = 8, height = 6)
mcmc_dens(draws, pars = "r2_within") +
  labs(
    title = "Posterior Distribution: Within-Person R²",
    x = "R² within",
    y = "Density"
  )
dev.off()

# ==============================================================================
# 7) POSTERIOR PREDICTIVE CHECK
# ==============================================================================

cat("\n=== POSTERIOR PREDICTIVE CHECK ===\n")

# Estrai y_rep
y_rep <- fit$draws("y_rep", format = "matrix")

# Campiona 100 repliche
y_rep_sample <- y_rep[sample(nrow(y_rep), 100), ]

# PPC plot
pdf(file.path(output_dir, "ppc_density.pdf"), width = 10, height = 6)
ppc_dens_overlay(stan_data$y_wp, y_rep_sample) +
  labs(
    title = "Posterior Predictive Check: Density Overlay",
    x = "F0 within-person deviation (Hz)",
    y = "Density"
  )
dev.off()

# PPC intervals
pdf(file.path(output_dir, "ppc_intervals.pdf"), width = 10, height = 6)
ppc_intervals(
  stan_data$y_wp,
  y_rep,
  prob = 0.5,
  prob_outer = 0.95
) +
  labs(
    title = "Posterior Predictive Check: 50% and 95% Intervals",
    x = "Observation index",
    y = "F0 within-person deviation (Hz)"
  )
dev.off()

# ==============================================================================
# 8) LOO-CV
# ==============================================================================

cat("\n=== LOO CROSS-VALIDATION ===\n")

# Estrai log-likelihood
log_lik <- fit$draws("log_lik", format = "matrix")

# Calcola LOO
loo_result <- loo(log_lik)

cat("\nLOO summary:\n")
print(loo_result)

# Salva
saveRDS(
  loo_result,
  file.path(output_dir, "loo_result.rds")
)

# Plot Pareto k
pdf(file.path(output_dir, "loo_pareto_k.pdf"), width = 10, height = 6)
plot(loo_result, label_points = TRUE) +
  labs(
    title = "LOO: Pareto k Diagnostic",
    subtitle = sprintf(
      "ELPD LOO: %.1f ± %.1f",
      loo_result$estimates["elpd_loo", "Estimate"],
      loo_result$estimates["elpd_loo", "SE"]
    )
  )
dev.off()

# ==============================================================================
# 9) SALVATAGGIO RISULTATI
# ==============================================================================

cat("\n=== SALVATAGGIO RISULTATI ===\n")

# Salva fit object
fit$save_object(file.path(output_dir, "fit_within_person.rds"))

# Salva draws
write_csv(
  as_tibble(draws),
  file.path(output_dir, "posterior_draws.csv.gz")
)

# Bundle completo
results_bundle <- list(
  fit = fit,
  stan_data = stan_data,
  beta_wp_summary = beta_wp_summary,
  r2_summary = r2_summary,
  loo_result = loo_result,
  diagnostics = diag_summary,
  pid5_vars = pid5_vars
)

saveRDS(
  results_bundle,
  file.path(output_dir, "results_bundle.rds")
)

# ==============================================================================
# 10) INTERPRETAZIONE RISULTATI
# ==============================================================================

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("INTERPRETAZIONE RISULTATI\n")
cat(rep("=", 80), "\n", sep = "")

cat("\n--- Coefficienti within-person (beta_wp) ---\n")
cat(
  "Interpretazione: per ogni aumento di 1 unità nella deviazione within-person\n"
)
cat("di un dominio PID-5, quanto cambia la deviazione F0 (in Hz)?\n\n")

for (i in 1:nrow(beta_wp_summary)) {
  cat(sprintf(
    "%s:\n  β = %.2f [%.2f, %.2f], P(β>0) = %.2f\n",
    beta_wp_summary$domain[i],
    beta_wp_summary$mean[i],
    beta_wp_summary$q025[i],
    beta_wp_summary$q975[i],
    beta_wp_summary$prob_pos[i]
  ))

  # Interpretazione pratica
  if (beta_wp_summary$q025[i] > 0) {
    cat("  → Evidenza robusta di associazione positiva\n")
  } else if (beta_wp_summary$q975[i] < 0) {
    cat("  → Evidenza robusta di associazione negativa\n")
  } else {
    cat("  → Evidenza debole o nulla di associazione\n")
  }
  cat("\n")
}

cat(sprintf(
  "\n--- Varianza spiegata within-person ---\nR² = %.3f [%.3f, %.3f]\n",
  r2_summary$mean,
  r2_summary$q025,
  r2_summary$q975
))

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("ANALISI COMPLETATA\n")
cat(rep("=", 80), "\n", sep = "")
cat("\nOUTPUT SALVATI IN:", output_dir, "\n")
cat("\nFile creati:\n")
cat("  - parameter_summary.csv\n")
cat("  - beta_wp_summary.csv\n")
cat("  - r2_within_summary.csv\n")
cat("  - trace_plots.pdf\n")
cat("  - beta_wp_densities.pdf\n")
cat("  - beta_wp_intervals.pdf\n")
cat("  - r2_within_distribution.pdf\n")
cat("  - ppc_density.pdf\n")
cat("  - ppc_intervals.pdf\n")
cat("  - loo_pareto_k.pdf\n")
cat("  - fit_within_person.rds\n")
cat("  - posterior_draws.csv.gz\n")
cat("  - results_bundle.rds\n")
cat(rep("=", 80), "\n\n", sep = "")

# eof ---

#' ABSTRACT
#'
#' Ecco una versione concisa per l'abstract:
#'
#' To examine whether personality-voice associations reflected stable trait
#' differences or moment-to-moment covariation, we decomposed variance into
#' between-person and within-person components. Bayesian multilevel models
#' revealed that momentary fluctuations in PID-5 domains explained minimal
#' within-person variance in F0 (R² = 2.5% [0.5%, 5.7%]). Only Negative
#' Affectivity showed a modest positive trend (β = 0.77, 90% CI [-0.37, 1.85]),
#' suggesting that the personality-voice relationship is primarily driven by
#' stable between-person differences (i.e., trait moderation of stress
#' reactivity) rather than by state-dependent fluctuations.
#'
#' RESULTS SECTION
#' #### Within-Person Covariation Between Momentary Personality and Voice
#'
#' To complement the between-person moderation analyses, we examined whether
#' momentary fluctuations in personality traits covaried with acoustic features
#' within individuals. For each participant and timepoint, we calculated
#' within-person deviations from their person mean on both PID-5 domains and
#' F0, creating measures of how much each observation deviated from that
#' individual's typical level.
#'
#' A Bayesian multilevel model regressed within-person F0 deviations on
#' within-person PID-5 deviations, controlling for random intercepts by subject.
#' The model showed excellent convergence (all Rhat < 1.01, ESS > 2,300).
#' Momentary fluctuations in the five PID-5 domains collectively explained
#' minimal within-person variance in F0 (R² = 2.5%, 95% CI [0.5%, 5.7%]).
#'
#' At the level of individual domains, Negative Affectivity showed a positive
#' trend (β = 0.77 Hz, 95% CI [-0.37, 1.85], P(β > 0) = 90%), suggesting that
#' when individuals experienced momentarily higher negative affect than their
#' typical level, F0 tended to increase. However, the 95% credible interval
#' included zero, indicating substantial uncertainty. Unexpectedly, Antagonism
#' showed a negative trend (β = -1.31 Hz, 95% CI [-2.92, 0.34], P(β > 0) = 6%),
#' though this effect was also uncertain. The remaining domains (Detachment,
#' Disinhibition, Psychoticism) showed no evidence of within-person associations
#' with F0 (all |β| < 0.4, CIs spanning zero).

#' DISCUSSION SECTION
#' Ecco come integrare questo nella discussione:
#'
#' #### Dissociation Between Trait-Level Moderation and State-Level Covariation
#'
#' Our findings reveal a critical dissociation between **between-person** and
#' **within-person** effects of personality on voice. While Negative Affectivity
#' reliably moderated stress-related pitch elevation at the between-person level
#' (i.e., individuals higher in NA showed stronger F0 increases under stress),
#' momentary fluctuations in NA showed only a weak and uncertain association
#' with F0 at the within-person level (R² = 2.5%).
#'
#' This pattern suggests that personality traits primarily function as **stable
#' moderators** of stress reactivity rather than as **state-dependent
#' fluctuations** that are reflected moment-to-moment in vocal production.
#' In other words, *who you are* (trait NA) shapes *how you react to stress*,
#' but transient increases in negative affect do not systematically alter vocal
#' pitch in the same way.
#'
#' This dissociation aligns with conceptual models distinguishing between
#' **trait modulation** (between-person vulnerability to stress) and
#' **state coupling** (within-person affective-vocal synchrony). The
#' between-person moderation likely reflects stable neurophysiological or behavioral
#' patterns—individuals high in NA may chronically maintain higher laryngeal
#' tension or exhibit stronger autonomic arousal responses. In contrast, the
#' weak within-person effects suggest that short-term affective fluctuations
#' do not directly translate into proportional vocal changes, perhaps because
#' voice production is more sensitive to *sustained* rather than *momentary*
#' affective states.
#'
#' The unexpected negative trend for Antagonism (β = -1.31, though uncertain)
#' warrants cautious interpretation. If robust, this could suggest that moments
#' of heightened interpersonal antagonism involve vocal suppression or reduced
#' arousal rather than heightened tension. However, given the wide credible
#' interval, replication in larger samples with finer temporal resolution is
#' needed.
#'
#' **Methodological considerations.** The low R² within (2.5%) should be
#' interpreted in context: with only three observations per person, power to
#' detect within-person effects is inherently limited. Moreover, EMA and voice
#' measurements were not perfectly time-locked—we aggregated EMA responses
#' within exam periods rather than capturing momentary personality at the
#' exact time of voice recording. Future studies with concurrent ambulatory
#' voice and affect assessment could provide stronger tests of state-level
#' covariation.

#' 🎯 Messaggio Take-Home
#'
#' Il messaggio chiave per il paper è:
#'
#' La relazione personalità-voce è principalmente BETWEEN-person,
#' non WITHIN-person.
#'
#' Questo significa:
#' ✅ Chi sei (trait) modera come reagisci allo stress → ROBUSTO
#' ❌ Come ti senti momento per momento (state) non si riflette sistematicamente nella voce → DEBOLE/ASSENTE
#'
#' Implicazioni teoriche:
#'
#' La personalità agisce come vulnerabilità disposizionale stabile, non come
#' fluttuazione momento-per-momento
#' La voce è sensibile agli stati affettivi sostenuti (come lo stress da esame),
#' non alle micro-fluttuazioni
#' I meccanismi sono probabilmente neurobiologici stabili (es. tono vagale
#' baseline, tensione laryngea cronica) piuttosto che accoppiamento diretto
#' affect-voce
#'
#' Forza del paper:
#'
#' Hai separato effetti between e within
#' Questo distingue trait moderation da state coupling
#' È una contribuzione metodologicamente sofisticata che pochi studi su
#' personalità e voce hanno fatto
#'
#'
