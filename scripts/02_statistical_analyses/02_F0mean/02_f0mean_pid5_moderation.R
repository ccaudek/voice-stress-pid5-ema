# ==============================================================================
# 02_f0mean_pid5_moderation.R
# Moderation analysis of PID-5 with stress effect on acustic features
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(cmdstanr)
  library(posterior)
  library(bayesplot)
})

bundle <- readRDS("results/stan_bundle_f0mean_pid5.rds")
stan_data <- bundle$stan_data
pid5_vars <- bundle$pid5_vars

mod <- cmdstan_model("stan/F0/f0mean_pid5_moderation.stan")

# ---- Fit cache: se esiste leggi, altrimenti campiona e salva ----
fit_rds <- "stan/F0/f0mean_pid5_moderation.RDS"

if (file.exists(fit_rds)) {
  message("Trovato fit già salvato: ", fit_rds, " -> lo leggo.")
  fit <- readRDS(fit_rds)
} else {
  message("Fit non trovato: eseguo cmdstanr::sample() e salvo in ", fit_rds)

  fit <- mod$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 2000,
    iter_sampling = 6000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123
  )

  # crea la cartella se non esiste
  dir.create(dirname(fit_rds), recursive = TRUE, showWarnings = FALSE)

  # salva il fitted model
  fit$save_object(file = fit_rds)
}

# ---- Posterior predictive checks ----
y_rep_draws <- fit$draws("y_rep", format = "matrix")

ppc_plot <- ppc_dens_overlay(
  y = stan_data$y,
  yrep = y_rep_draws[1:100, ]
)
print(ppc_plot)

# Salva il plot PPC
ggsave(
  filename = here::here("results", "f0mean", "ppc_f0mean_moderation.png"),
  plot = ppc_plot,
  width = 8,
  height = 6,
  dpi = 300
)

# ---- Summary ----
summary_results <- fit$summary(c(
  "alpha",
  "b1",
  "b2",
  "g1",
  "g2",
  "sigma_y",
  "tau",
  "sigma_ema"
))

# Stampa a schermo
data.frame(summary_results)

# Salva come CSV
write.csv(
  as.data.frame(summary_results),
  file = here::here(
    "results",
    "f0mean",
    "model_summary_f0mean_moderation.csv"
  ),
  row.names = FALSE
)

# ---- PD per moderazioni ----
draws <- fit$draws()

pd <- function(x) max(mean(x > 0), mean(x < 0))

pd_results <- data.frame(
  trait = character(),
  g1_pd = numeric(),
  g1_p_gt0 = numeric(),
  g2_pd = numeric(),
  g2_p_gt0 = numeric(),
  stringsAsFactors = FALSE
)

for (d in 1:stan_data$D) {
  g1d <- as.numeric(draws[,, paste0("g1[", d, "]")])
  g2d <- as.numeric(draws[,, paste0("g2[", d, "]")])

  pd_results <- rbind(
    pd_results,
    data.frame(
      trait = pid5_vars[d],
      g1_pd = pd(g1d),
      g1_p_gt0 = mean(g1d > 0),
      g2_pd = pd(g2d),
      g2_p_gt0 = mean(g2d > 0)
    )
  )
}

# Arrotonda per output leggibile
pd_results[, -1] <- round(pd_results[, -1], 3)

# Stampa a schermo (opzionale)
print(pd_results)
# trait g1_pd g1_p_gt0 g2_pd g2_p_gt0
# 1 pid5_negative_affectivity 0.970    0.970 0.602    0.398
# 2           pid5_detachment 0.585    0.415 0.880    0.120
# 3           pid5_antagonism 0.525    0.475 0.974    0.974
# 4        pid5_disinhibition 0.546    0.454 0.536    0.464
# 5         pid5_psychoticism 0.559    0.441 0.796    0.204

# ---- Salvataggio ----
write.csv(
  pd_results,
  file = here::here(
    "results",
    "f0mean",
    "pd_f0mean_moderation.csv"
  ),
  row.names = FALSE
)

saveRDS(
  pd_results,
  file = here::here(
    "results",
    "f0mean",
    "pd_f0mean_moderation.rds"
  )
)

# eof ---
