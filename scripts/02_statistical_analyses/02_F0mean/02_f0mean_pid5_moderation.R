# ==============================================================================
# 02_f0mean_pid5_moderation.R
# Moderation analysis of PID-5 with stress effect on acustic features
# ==============================================================================

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(bayesplot)
})

bundle <- readRDS("results/stan_bundle_f0mean_pid5.rds")
stan_data <- bundle$stan_data
pid5_vars <- bundle$pid5_vars

mod <- cmdstan_model("stan/F0/f0mean_pid5_moderation.stan")

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

# Save the fitted model
# fit$save_object(file = "stan/F0/f0mean_pid5_moderation.RDS")

fit <- readRDS(
  "stan/F0/f0mean_pid5_moderation.RDS"
)

# Extract the posterior predictive draws
# Replace "y_rep" with the name of your generated quantity variable
y_rep_draws <- fit$draws("y_rep", format = "matrix")

# Create the density overlay plot
# Replace `stan_data$y` with your actual observed outcome variable from the data list
ppc_plot <- ppc_dens_overlay(y = stan_data$y, yrep = y_rep_draws[1:100, ])
print(ppc_plot)

# Salva il plot PPC
ggsave(
  filename = here::here("results", "f0mean", "ppc_f0mean_moderation.png"),
  plot = ppc_plot,
  width = 8,
  height = 6,
  dpi = 300
)

# Estrai e salva il summary
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

draws <- fit$draws()

# PD per g1 (moderazione stress) e g2 (moderazione recovery)
# g1[d] è "stress × trait_d" per +1 SD di tratto (perché theta è z)
pd <- function(x) max(mean(x > 0), mean(x < 0))

for (d in 1:stan_data$D) {
  g1d <- as.numeric(draws[,, paste0("g1[", d, "]")])
  g2d <- as.numeric(draws[,, paste0("g2[", d, "]")])

  cat("\n", pid5_vars[d], "\n")
  cat(
    "  Stress moderation g1: PD =",
    round(pd(g1d), 3),
    "| P(>0) =",
    round(mean(g1d > 0), 3),
    "\n"
  )
  cat(
    "  Recovery moderation g2: PD =",
    round(pd(g2d), 3),
    "| P(>0) =",
    round(mean(g2d > 0), 3),
    "\n"
  )
}

# pid5_negative_affectivity
# Stress moderation g1: PD = 0.97 | P(>0) = 0.97
# Recovery moderation g2: PD = 0.602 | P(>0) = 0.398
#
# pid5_detachment
# Stress moderation g1: PD = 0.585 | P(>0) = 0.415
# Recovery moderation g2: PD = 0.88 | P(>0) = 0.12
#
# pid5_antagonism
# Stress moderation g1: PD = 0.525 | P(>0) = 0.475
# Recovery moderation g2: PD = 0.974 | P(>0) = 0.974
#
# pid5_disinhibition
# Stress moderation g1: PD = 0.546 | P(>0) = 0.454
# Recovery moderation g2: PD = 0.536 | P(>0) = 0.464
#
# pid5_psychoticism
# Stress moderation g1: PD = 0.559 | P(>0) = 0.441
# Recovery moderation g2: PD = 0.796 | P(>0) = 0.204

# eof ---
