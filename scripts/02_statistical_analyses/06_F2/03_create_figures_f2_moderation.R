# ==============================================================================
# 03_create_figures_f2_moderation.R
# Genera figure per il manoscritto: PID-5 x stress/recovery su outcome F2 /
# metriche articolatorie. Compatibile con:
# - 01_prepare_stan_data_f2_pid5.R
# - 02_f2_pid5_moderation.R
#
# Nota: il modello Stan usa outcome standardizzati. Le figure interpretative
# riportano i coefficienti e le predizioni sulla scala originale dell'outcome,
# usando y_center e y_scale salvati nel bundle.
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(posterior)
  library(bayesplot)
  library(ggdist)
  library(patchwork)
  library(here)
})

# ==============================================================================
# Impostazioni generali
# ==============================================================================

cri_level <- 0.89
cri_probs <- c((1 - cri_level) / 2, 1 - (1 - cri_level) / 2)
cri_label <- paste0(round(cri_level * 100), "%")

# Deve corrispondere agli outcome fittati da 02_f2_pid5_moderation.R.
# f2_slope e' esportata da 01 ma non inclusa di default perche' piu' instabile.
outcomes_to_plot <- c(
  "f2_range_norm",
  "f2_mean",
  "vsa_log",
  "centralization"
)

pid5_labels <- c(
  "Negative\nAffectivity",
  "Detachment",
  "Antagonism",
  "Disinhibition",
  "Psychoticism"
)

pid5_labels_plain <- c(
  "Negative Affectivity",
  "Detachment",
  "Antagonism",
  "Disinhibition",
  "Psychoticism"
)

outcome_labels <- c(
  f2_mean = "Mean F2",
  f2_range = "F2 range (/i/ - /u/)",
  f2_range_norm = "Normalized F2 range",
  vsa_log = "Log vowel space area",
  centralization = "Vowel-space centralization",
  f2_slope = "F2 slope (/i/ to /u/)"
)

outcome_units <- c(
  f2_mean = "Hz",
  f2_range = "Hz",
  f2_range_norm = "F2/F1 ratio",
  vsa_log = "log Hz^2",
  centralization = "Hz",
  f2_slope = "Hz/Hz"
)

pretty_outcome <- function(outcome) {
  if (outcome %in% names(outcome_labels)) outcome_labels[[outcome]] else outcome
}

unit_outcome <- function(outcome) {
  if (outcome %in% names(outcome_units)) outcome_units[[outcome]] else "original units"
}

pd_fun <- function(x) {
  max(mean(x > 0), mean(x < 0))
}

get_fit_bundle <- function(outcome_name) {
  fit_rds <- here("stan", "F2", paste0(outcome_name, "_pid5_moderation.RDS"))
  bundle_rds <- here(
    "results", "F2", "data",
    paste0("stan_bundle_", outcome_name, "_pid5.rds")
  )

  if (!file.exists(fit_rds)) {
    stop("Fitted model not found: ", fit_rds,
         "\nRun 02_f2_pid5_moderation.R for this outcome first.")
  }
  if (!file.exists(bundle_rds)) {
    stop("Stan bundle not found: ", bundle_rds,
         "\nRun 01_prepare_stan_data_f2_pid5.R first.")
  }

  list(
    fit = readRDS(fit_rds),
    bundle = readRDS(bundle_rds),
    fit_rds = fit_rds,
    bundle_rds = bundle_rds
  )
}

as_draw_vec <- function(draws, par) {
  as.numeric(draws[, , par])
}

predict_outcome_raw <- function(timepoint, theta, draws, g1_vec, g2_vec, bundle) {
  c1 <- dplyr::case_when(
    timepoint == "Baseline" ~ -0.5,
    timepoint == "Pre-Stress" ~ 0.5,
    timepoint == "Post-Stress" ~ 0.0
  )

  c2 <- dplyr::case_when(
    timepoint == "Baseline" ~ 0.0,
    timepoint == "Pre-Stress" ~ -0.5,
    timepoint == "Post-Stress" ~ 0.5
  )

  alpha <- as_draw_vec(draws, "alpha")
  b1 <- as_draw_vec(draws, "b1")
  b2 <- as_draw_vec(draws, "b2")

  mu_std <- alpha +
    b1 * c1 +
    b2 * c2 +
    g1_vec * c1 * theta +
    g2_vec * c2 * theta

  mu_std * bundle$y_scale + bundle$y_center
}

# ==============================================================================
# Funzione principale per un outcome
# ==============================================================================

make_figures_one_outcome <- function(outcome_name) {
  cat("\n=== FIGURES FOR ", outcome_name, " ===\n", sep = "")

  loaded <- get_fit_bundle(outcome_name)
  fit <- loaded$fit
  bundle <- loaded$bundle
  stan_data <- bundle$stan_data

  if (stan_data$D != length(pid5_labels)) {
    stop("Expected ", length(pid5_labels), " PID-5 domains, got ", stan_data$D)
  }

  draws <- fit$draws()

  out_dir <- here("results", "F2", "figures", outcome_name)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  outcome_label <- pretty_outcome(outcome_name)
  outcome_unit <- unit_outcome(outcome_name)

  # ------------------------------------------------------------------------------
  # FIGURA 1: Coefficient plot per g1 e g2, su scala originale
  # ------------------------------------------------------------------------------

  moderation_data <- tibble()

  for (d in seq_len(stan_data$D)) {
    g1_draws_raw <- as_draw_vec(draws, paste0("g1[", d, "]")) * bundle$y_scale
    g2_draws_raw <- as_draw_vec(draws, paste0("g2[", d, "]")) * bundle$y_scale

    moderation_data <- moderation_data %>%
      bind_rows(
        tibble(
          outcome = outcome_name,
          domain = pid5_labels[d],
          parameter = "Stress Moderation (gamma1)",
          estimate = median(g1_draws_raw),
          lower = unname(quantile(g1_draws_raw, cri_probs[1])),
          upper = unname(quantile(g1_draws_raw, cri_probs[2])),
          pd = pd_fun(g1_draws_raw)
        ),
        tibble(
          outcome = outcome_name,
          domain = pid5_labels[d],
          parameter = "Recovery Moderation (gamma2)",
          estimate = median(g2_draws_raw),
          lower = unname(quantile(g2_draws_raw, cri_probs[1])),
          upper = unname(quantile(g2_draws_raw, cri_probs[2])),
          pd = pd_fun(g2_draws_raw)
        )
      )
  }

  moderation_data <- moderation_data %>%
    mutate(
      domain = factor(domain, levels = rev(pid5_labels)),
      pd_label = sprintf("PD = %.2f", pd)
    )

  write_csv(
    moderation_data,
    file.path(out_dir, paste0("figure1_moderation_coefficients_", outcome_name, "_89cri.csv"))
  )

  fig1 <- ggplot(moderation_data, aes(x = estimate, y = domain)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_linerange(
      aes(xmin = lower, xmax = upper),
      linewidth = 1.2,
      alpha = 0.8,
      color = "gray40"
    ) +
    geom_point(size = 3, shape = 21, fill = "white", color = "black") +
    facet_wrap(~parameter) +
    labs(
      x = paste0("Moderation effect (", outcome_unit, " per SD increase in trait)"),
      y = NULL,
      title = paste0("PID-5 Moderation of ", outcome_label),
      subtitle = paste0("Posterior medians with ", cri_label,
                        " credible intervals; PD is descriptive")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 11),
      axis.text.y = element_text(size = 10)
    )

  print(fig1)

  ggsave(
    filename = file.path(out_dir, paste0("figure1_moderation_coefficients_", outcome_name, ".png")),
    plot = fig1,
    width = 10,
    height = 6,
    dpi = 300
  )

  ggsave(
    filename = file.path(out_dir, paste0("figure1_moderation_coefficients_", outcome_name, ".pdf")),
    plot = fig1,
    width = 10,
    height = 6
  )

  # ------------------------------------------------------------------------------
  # FIGURA 2: Predicted trajectories per Negative Affectivity e Antagonism
  # ------------------------------------------------------------------------------

  g1_na <- as_draw_vec(draws, "g1[1]")
  g2_na <- as_draw_vec(draws, "g2[1]")
  g1_ant <- as_draw_vec(draws, "g1[3]")
  g2_ant <- as_draw_vec(draws, "g2[3]")

  timepoints <- c("Baseline", "Pre-Stress", "Post-Stress")
  trait_levels <- c(-1, 0, 1)

  make_predictions <- function(domain_label, g1_vec, g2_vec) {
    expand_grid(
      timepoint = factor(timepoints, levels = timepoints),
      trait_level = trait_levels
    ) %>%
      rowwise() %>%
      mutate(
        domain = domain_label,
        y_draws = list(predict_outcome_raw(
          as.character(timepoint),
          trait_level,
          draws,
          g1_vec,
          g2_vec,
          bundle
        )),
        y_estimate = median(y_draws),
        y_lower = unname(quantile(y_draws, cri_probs[1])),
        y_upper = unname(quantile(y_draws, cri_probs[2]))
      ) %>%
      ungroup() %>%
      mutate(
        trait_label = factor(
          trait_level,
          levels = c(-1, 0, 1),
          labels = c("Low (-1 SD)", "Average", "High (+1 SD)")
        )
      )
  }

  predictions_all <- bind_rows(
    make_predictions("Negative Affectivity", g1_na, g2_na),
    make_predictions("Antagonism", g1_ant, g2_ant)
  )

  write_csv(
    predictions_all %>%
      select(domain, timepoint, trait_level, trait_label, y_estimate, y_lower, y_upper),
    file.path(out_dir, paste0("figure2_predicted_trajectories_", outcome_name, "_89cri.csv"))
  )

  fig2 <- ggplot(
    predictions_all,
    aes(x = timepoint, y = y_estimate, group = trait_label)
  ) +
    geom_ribbon(
      aes(ymin = y_lower, ymax = y_upper, fill = trait_label),
      alpha = 0.2
    ) +
    geom_line(aes(color = trait_label), linewidth = 1.2) +
    geom_point(aes(color = trait_label), size = 3) +
    facet_wrap(~domain) +
    scale_color_manual(
      values = c(
        "Low (-1 SD)" = "#1f77b4",
        "Average" = "gray60",
        "High (+1 SD)" = "#d62728"
      )
    ) +
    scale_fill_manual(
      values = c(
        "Low (-1 SD)" = "#1f77b4",
        "Average" = "gray60",
        "High (+1 SD)" = "#d62728"
      )
    ) +
    labs(
      x = "Timepoint",
      y = paste0("Predicted ", outcome_label, " (", outcome_unit, ")"),
      color = "Trait Level",
      fill = "Trait Level",
      title = paste0("Predicted ", outcome_label, " Trajectories by PID-5 Trait Level"),
      subtitle = paste0("Bands represent ", cri_label, " credible intervals")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  print(fig2)

  ggsave(
    filename = file.path(out_dir, paste0("figure2_predicted_trajectories_", outcome_name, ".png")),
    plot = fig2,
    width = 10,
    height = 5,
    dpi = 300
  )

  ggsave(
    filename = file.path(out_dir, paste0("figure2_predicted_trajectories_", outcome_name, ".pdf")),
    plot = fig2,
    width = 10,
    height = 5
  )

  # ------------------------------------------------------------------------------
  # FIGURA 3 SUPPLEMENTARE: Posterior distributions per g1/g2 su scala originale
  # ------------------------------------------------------------------------------

  g_draws <- tibble()

  for (d in seq_len(stan_data$D)) {
    g_draws <- g_draws %>%
      bind_rows(
        tibble(
          outcome = outcome_name,
          domain = pid5_labels[d],
          parameter = "gamma1 (Stress)",
          value = as_draw_vec(draws, paste0("g1[", d, "]")) * bundle$y_scale
        ),
        tibble(
          outcome = outcome_name,
          domain = pid5_labels[d],
          parameter = "gamma2 (Recovery)",
          value = as_draw_vec(draws, paste0("g2[", d, "]")) * bundle$y_scale
        )
      )
  }

  g_draws <- g_draws %>%
    mutate(domain = factor(domain, levels = rev(pid5_labels)))

  fig3 <- ggplot(g_draws, aes(x = value, y = domain)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    stat_halfeye(
      .width = cri_level,
      point_interval = "median_qi",
      normalize = "xy",
      fill = "gray70",
      alpha = 0.7
    ) +
    facet_wrap(~parameter) +
    labs(
      x = paste0("Moderation effect (", outcome_unit, " per SD)"),
      y = NULL,
      title = paste0("Posterior Distributions of Moderation Effects: ", outcome_label),
      subtitle = paste0("Points and intervals show posterior medians and ", cri_label,
                        " credible intervals")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 11)
    )

  print(fig3)

  ggsave(
    filename = file.path(out_dir, paste0("figureS1_posterior_distributions_", outcome_name, ".png")),
    plot = fig3,
    width = 10,
    height = 6,
    dpi = 300
  )

  ggsave(
    filename = file.path(out_dir, paste0("figureS1_posterior_distributions_", outcome_name, ".pdf")),
    plot = fig3,
    width = 10,
    height = 6
  )

  # ------------------------------------------------------------------------------
  # FIGURA 4 SUPPLEMENTARE: Trace plots, parametri standardizzati
  # ------------------------------------------------------------------------------

  params_to_plot <- c(
    "alpha",
    "b1",
    "b2",
    paste0("g1[", seq_len(stan_data$D), "]"),
    paste0("g2[", seq_len(stan_data$D), "]")
  )

  fig4 <- mcmc_trace(
    fit$draws(params_to_plot),
    facet_args = list(ncol = 3)
  ) +
    labs(title = paste0("MCMC Trace Plots for Key Parameters: ", outcome_label)) +
    theme_minimal(base_size = 10)

  print(fig4)

  ggsave(
    filename = file.path(out_dir, paste0("figureS2_trace_plots_", outcome_name, ".png")),
    plot = fig4,
    width = 12,
    height = 10,
    dpi = 300
  )

  ggsave(
    filename = file.path(out_dir, paste0("figureS2_trace_plots_", outcome_name, ".pdf")),
    plot = fig4,
    width = 12,
    height = 10
  )

  cat("Created figures for ", outcome_name, " in ", out_dir, "\n", sep = "")

  invisible(moderation_data)
}

# ==============================================================================
# Esecuzione
# ==============================================================================

all_moderation_data <- map_dfr(outcomes_to_plot, make_figures_one_outcome)

combined_dir <- here("results", "F2", "figures")
dir.create(combined_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(
  all_moderation_data,
  here("results", "F2", "figures", "figure1_moderation_coefficients_all_f2_outcomes_89cri.csv")
)

cat("\n=== FIGURE GENERATION COMPLETE FOR F2 OUTCOMES ===\n")
cat("Outcomes processed:\n")
cat(paste(" -", outcomes_to_plot), sep = "\n")
cat("\n")

# eof
