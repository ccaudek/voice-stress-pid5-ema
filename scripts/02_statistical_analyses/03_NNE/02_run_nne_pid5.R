# ==============================================================================
# 02_run_nne_pid5.R
# ==============================================================================

suppressPackageStartupMessages({
  library(cmdstanr)
})

source(
  here::here(
    "scripts",
    "02_statistical_analyses",
    "03_NNE",
    "01_prepare_stan_data_nne_pid5.R"
  )
)

stan_data <- jsonlite::read_json(
  here::here(
    "stan",
    "NNE",
    "stan_data_nne_pid5.json"
  ),
  simplifyVector = TRUE
)

mod <- cmdstan_model(
  here::here(
    "stan",
    "NNE",
    "nne_pid5_moderation.stan"
  )
)

fit <- mod$sample(
  data = here::here(
    "stan",
    "NNE",
    "stan_data_nne_pid5.json"
  ),
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 6000,
  adapt_delta = 0.99,
  max_treedepth = 15,
  seed = 123
)

if (0) {
  saveRDS(
    fit,
    here::here(
      "stan",
      "NNE",
      "nne_mean_pid5_moderation.rds"
    )
  )
}

fit <- readRDS(
  here::here(
    "stan",
    "NNE",
    "nne_mean_pid5_moderation.rds"
  )
)

y_rep_draws <- fit$draws("y_rep", format = "matrix")

# Create the density overlay plot
# Replace `stan_data$y` with your actual observed outcome variable from the data list
ppc_plot <- ppc_dens_overlay(y = stan_data$y, yrep = y_rep_draws[1:100, ])
print(ppc_plot)

# Salva il plot PPC
ggsave(
  filename = here::here("results", "NNE", "ppc_nne_moderation.png"),
  plot = ppc_plot,
  width = 8,
  height = 6,
  dpi = 300
)

# eof ---
