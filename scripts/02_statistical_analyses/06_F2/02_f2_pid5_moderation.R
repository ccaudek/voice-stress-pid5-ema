# ==============================================================================
# 02_f2_pid5_moderation.R
# Moderation analysis of PID-5 with stress/recovery effects on F2/articulatory
# metrics. Uses standardized outcomes prepared by 01_prepare_stan_data_f2_pid5.R.
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(cmdstanr)
  library(posterior)
  library(bayesplot)
  library(here)
})

# Fit default: robust/interpretability-first metrics.
# Aggiungi "f2_slope" se vuoi analizzare anche la slope i-u, sapendo che e piu' instabile.
outcomes_to_fit <- c(
  "f2_range_norm",
  "f2_mean",
  "vsa_log",
  "centralization"
)

# Per fit di controllo su tutte le metriche esportate, usa:
# outcomes_to_fit <- c("f2_mean", "f2_range", "f2_range_norm", "vsa_log", "centralization", "f2_slope")

stan_file <- here("stan", "F2", "f2_pid5_moderation.stan")
stopifnot(file.exists(stan_file))

mod <- cmdstan_model(stan_file)

pd <- function(x) max(mean(x > 0), mean(x < 0))

summarise_draw <- function(x, parameter) {
  tibble(
    variable = parameter,
    mean = mean(x),
    median = median(x),
    sd = sd(x),
    mad = mad(x),
    q5.5 = posterior::quantile2(x, probs = 0.055),
    q94.5 = posterior::quantile2(x, probs = 0.945),
    pd = pd(x),
    p_gt0 = mean(x > 0)
  )
}

make_original_scale_summary <- function(fit, bundle) {
  draws_df <- posterior::as_draws_df(fit$draws())
  y_center <- bundle$y_center
  y_scale <- bundle$y_scale
  D <- bundle$stan_data$D
  pid5_vars <- bundle$pid5_vars

  out <- list(
    summarise_draw(draws_df$alpha * y_scale + y_center, "alpha_raw"),
    summarise_draw(draws_df$b1 * y_scale, "b1_raw"),
    summarise_draw(draws_df$b2 * y_scale, "b2_raw"),
    summarise_draw(draws_df$sigma_y * y_scale, "sigma_y_raw")
  )

  for (k in 1:3) {
    out[[length(out) + 1]] <- summarise_draw(
      draws_df[[paste0("tau[", k, "]")]] * y_scale,
      paste0("tau_raw[", k, "]")
    )
  }

  for (d in seq_len(D)) {
    trait <- pid5_vars[d]
    out[[length(out) + 1]] <- summarise_draw(
      draws_df[[paste0("a_trait[", d, "]")]] * y_scale,
      paste0("a_trait_raw[", trait, "]")
    )
    out[[length(out) + 1]] <- summarise_draw(
      draws_df[[paste0("g1[", d, "]")]] * y_scale,
      paste0("g1_raw[", trait, "]")
    )
    out[[length(out) + 1]] <- summarise_draw(
      draws_df[[paste0("g2[", d, "]")]] * y_scale,
      paste0("g2_raw[", trait, "]")
    )
  }

  bind_rows(out) |>
    mutate(outcome = bundle$outcome_name, .before = 1)
}

fit_one_outcome <- function(outcome_name) {
  bundle_path <- here(
    "results",
    "F2",
    "data",
    paste0("stan_bundle_", outcome_name, "_pid5.rds")
  )
  stopifnot(file.exists(bundle_path))

  bundle <- readRDS(bundle_path)
  stan_data <- bundle$stan_data
  pid5_vars <- bundle$pid5_vars

  fit_rds <- here("stan", "F2", paste0(outcome_name, "_pid5_moderation.RDS"))

  if (file.exists(fit_rds)) {
    message("Trovato fit gia salvato: ", fit_rds, " -> lo leggo.")
    fit <- readRDS(fit_rds)
  } else {
    message(
      "Fit non trovato per ",
      outcome_name,
      ": eseguo cmdstanr::sample() e salvo in ",
      fit_rds
    )

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

    dir.create(dirname(fit_rds), recursive = TRUE, showWarnings = FALSE)
    fit$save_object(file = fit_rds)
  }

  # ---- Posterior predictive checks: riportati su scala originale ----
  y_rep_std <- fit$draws("y_rep", format = "matrix")
  ppc_rows <- seq_len(min(100, nrow(y_rep_std)))
  y_rep_raw <- y_rep_std[ppc_rows, , drop = FALSE] *
    bundle$y_scale +
    bundle$y_center
  y_raw <- bundle$df_voice$y_raw

  ppc_plot <- ppc_dens_overlay(
    y = y_raw,
    yrep = y_rep_raw
  ) +
    ggtitle(paste0("PPC - ", outcome_name))

  print(ppc_plot)

  ggsave(
    filename = here(
      "results",
      "F2",
      paste0("ppc_", outcome_name, "_moderation.png")
    ),
    plot = ppc_plot,
    width = 8,
    height = 6,
    dpi = 300
  )

  # ---- Summary (89%) su scala standardizzata ----
  summary_results <- fit$summary(
    variables = c(
      "alpha",
      "b1",
      "b2",
      "a_trait",
      "g1",
      "g2",
      "sigma_y",
      "tau",
      "sigma_ema"
    ),
    mean,
    median,
    sd,
    mad,
    ~ posterior::quantile2(.x, probs = c(0.055, 0.945)),
    rhat,
    ess_bulk,
    ess_tail
  )

  summary_results <- as.data.frame(summary_results) |>
    mutate(outcome = outcome_name, .before = 1)

  write.csv(
    summary_results,
    file = here(
      "results",
      "F2",
      paste0("model_summary_", outcome_name, "_moderation.csv")
    ),
    row.names = FALSE
  )

  # ---- Summary su scala originale dell'outcome ----
  original_scale_summary <- make_original_scale_summary(fit, bundle)

  write.csv(
    original_scale_summary,
    file = here(
      "results",
      "F2",
      paste0("model_summary_", outcome_name, "_moderation_original_scale.csv")
    ),
    row.names = FALSE
  )

  # ---- PD per moderazioni ----
  draws <- fit$draws()

  pd_results <- data.frame(
    outcome = character(),
    trait = character(),
    g1_pd = numeric(),
    g1_p_gt0 = numeric(),
    g2_pd = numeric(),
    g2_p_gt0 = numeric(),
    stringsAsFactors = FALSE
  )

  for (d in seq_len(stan_data$D)) {
    g1d <- as.numeric(draws[,, paste0("g1[", d, "]")])
    g2d <- as.numeric(draws[,, paste0("g2[", d, "]")])

    pd_results <- rbind(
      pd_results,
      data.frame(
        outcome = outcome_name,
        trait = pid5_vars[d],
        g1_pd = pd(g1d),
        g1_p_gt0 = mean(g1d > 0),
        g2_pd = pd(g2d),
        g2_p_gt0 = mean(g2d > 0)
      )
    )
  }

  pd_results[, c("g1_pd", "g1_p_gt0", "g2_pd", "g2_p_gt0")] <-
    round(pd_results[, c("g1_pd", "g1_p_gt0", "g2_pd", "g2_p_gt0")], 3)

  print(pd_results)

  write.csv(
    pd_results,
    file = here(
      "results",
      "F2",
      paste0("pd_", outcome_name, "_moderation.csv")
    ),
    row.names = FALSE
  )

  saveRDS(
    pd_results,
    file = here("results", "F2", paste0("pd_", outcome_name, "_moderation.rds"))
  )

  pd_results
}

all_pd <- map_dfr(outcomes_to_fit, fit_one_outcome)

write.csv(
  all_pd,
  file = here("results", "F2", "pd_all_f2_moderation.csv"),
  row.names = FALSE
)

saveRDS(
  all_pd,
  file = here("results", "F2", "pd_all_f2_moderation.rds")
)

cat("\n=== PD SUMMARY - ALL F2 OUTCOMES ===\n")
print(all_pd)

# eof ---
