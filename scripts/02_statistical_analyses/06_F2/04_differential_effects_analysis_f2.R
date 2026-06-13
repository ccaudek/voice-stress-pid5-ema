# ==============================================================================
# 04_differential_effects_analysis_f2.R
# Differential moderation patterns: PID-5 domains x stress/recovery for F2 /
# articulatory outcomes.
#
# Compatibile con la pipeline F2:
# - 01_prepare_stan_data_f2_pid5.R
# - 02_f2_pid5_moderation.R
#
# Usa intervalli di credibilita' centrali all'89%.
# PD, SNR e probabilita' pairwise sono descrittivi, senza soglie decisionali.
# Coefficienti, contrasti e risposte individuali sono riportati sulla scala
# originale dell'outcome, usando y_center e y_scale nel bundle.
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
# SETUP
# ==============================================================================

cri_level <- 0.89
cri_probs <- c((1 - cri_level) / 2, 1 - (1 - cri_level) / 2)
cri_label <- paste0(round(cri_level * 100), "%")

outcomes_to_analyze <- c(
  "f2_range_norm",
  "f2_mean",
  "vsa_log",
  "centralization"
)

pid5_labels <- c(
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

as_draw_vec <- function(draws, par) {
  as.numeric(draws[, , par])
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

  list(fit = readRDS(fit_rds), bundle = readRDS(bundle_rds))
}

extract_metrics <- function(draws_param) {
  tibble(
    median = median(draws_param),
    mean = mean(draws_param),
    mad = mad(draws_param),
    ci_lower = unname(quantile(draws_param, cri_probs[1])),
    ci_upper = unname(quantile(draws_param, cri_probs[2])),
    pd = pd_fun(draws_param),
    p_positive = mean(draws_param > 0),
    p_negative = mean(draws_param < 0),
    snr = abs(median(draws_param)) / mad(draws_param)
  )
}

# Per F2/articolazione, le direzioni teoriche possono dipendere dall'outcome.
# Di default sono quindi marcate come "ambiguous". Modifica questa funzione se
# vuoi codificare predizioni direzionali specifiche per ogni metrica.
make_theoretical_predictions <- function(outcome_name) {
  expand_grid(
    domain = pid5_labels,
    parameter = c("Stress (gamma1)", "Recovery (gamma2)")
  ) %>%
    mutate(
      predicted_direction = "ambiguous",
      rationale = paste0(
        "Specify an a-priori directional prediction for ",
        pretty_outcome(outcome_name),
        " if theory supports it."
      )
    )
}

# ==============================================================================
# ANALISI PER UN OUTCOME
# ==============================================================================

analyze_one_outcome <- function(outcome_name) {
  cat("\n=== DIFFERENTIAL MODERATION EFFECTS ANALYSIS: ",
      outcome_name, " ===\n", sep = "")
  cat("Outcome label: ", pretty_outcome(outcome_name), "\n", sep = "")
  cat("Unit: ", unit_outcome(outcome_name), "\n", sep = "")
  cat("Credible intervals: ", cri_label, " central intervals\n", sep = "")

  loaded <- get_fit_bundle(outcome_name)
  fit <- loaded$fit
  bundle <- loaded$bundle
  stan_data <- bundle$stan_data
  draws <- fit$draws()

  if (stan_data$D != length(pid5_labels)) {
    stop("Expected ", length(pid5_labels), " PID-5 domains, got ", stan_data$D)
  }

  outcome_label <- pretty_outcome(outcome_name)
  outcome_unit <- unit_outcome(outcome_name)

  out_dir <- here("results", "F2")
  fig_dir <- here("results", "F2", "figures", outcome_name)
  tab_dir <- here("results", "F2", "tables")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

  sigma_y_raw <- median(as_draw_vec(draws, "sigma_y") * bundle$y_scale)
  cat("Residual SD (sigma_y) = ", round(sigma_y_raw, 3), " ",
      outcome_unit, "\n\n", sep = "")

  # ------------------------------------------------------------------------------
  # PART 1: DIRECTIONAL POSTERIOR SUMMARIES
  # ------------------------------------------------------------------------------

  cat("=== PART 1: DIRECTIONAL POSTERIOR SUMMARIES ===\n\n")

  direction_table <- tibble()

  for (d in seq_along(pid5_labels)) {
    g1_raw <- as_draw_vec(draws, paste0("g1[", d, "]")) * bundle$y_scale
    metrics_g1 <- extract_metrics(g1_raw)

    direction_table <- bind_rows(
      direction_table,
      metrics_g1 %>%
        mutate(
          outcome = outcome_name,
          outcome_label = outcome_label,
          outcome_unit = outcome_unit,
          domain = pid5_labels[d],
          parameter = "Stress (gamma1)"
        )
    )

    g2_raw <- as_draw_vec(draws, paste0("g2[", d, "]")) * bundle$y_scale
    metrics_g2 <- extract_metrics(g2_raw)

    direction_table <- bind_rows(
      direction_table,
      metrics_g2 %>%
        mutate(
          outcome = outcome_name,
          outcome_label = outcome_label,
          outcome_unit = outcome_unit,
          domain = pid5_labels[d],
          parameter = "Recovery (gamma2)"
        )
    )
  }

  direction_summary <- direction_table %>%
    mutate(
      direction = ifelse(median > 0, "positive", "negative"),
      pd_label = sprintf("PD = %.2f", pd),
      interval_label = sprintf("%.3f [%.3f, %.3f]", median, ci_lower, ci_upper)
    ) %>%
    select(
      outcome,
      outcome_label,
      outcome_unit,
      domain,
      parameter,
      median,
      ci_lower,
      ci_upper,
      pd,
      p_positive,
      p_negative,
      snr,
      direction,
      pd_label,
      interval_label
    )

  print(direction_summary, n = Inf)

  write_csv(
    direction_summary,
    here(tab_dir, paste0("direction_posterior_summary_", outcome_name, "_89cri.csv"))
  )

  top_directional_effects <- direction_summary %>%
    arrange(desc(pd)) %>%
    slice_head(n = 5)

  cat("\nTop moderation effects by posterior probability of direction:\n")
  print(
    top_directional_effects %>%
      select(outcome, domain, parameter, median, ci_lower, ci_upper, pd)
  )

  # ------------------------------------------------------------------------------
  # PART 2: DIFFERENTIAL EFFECTS - PAIRWISE PROBABILITIES
  # ------------------------------------------------------------------------------

  cat("\n=== PART 2: DIFFERENTIAL EFFECTS ANALYSIS ===\n\n")

  cat("--- Pairwise Contrasts (Stress moderation): NA versus other domains ---\n")
  g1_list <- vector("list", length(pid5_labels))
  for (d in seq_along(pid5_labels)) {
    g1_list[[d]] <- as_draw_vec(draws, paste0("g1[", d, "]")) * bundle$y_scale
  }

  for (d in 2:5) {
    p_greater <- mean(g1_list[[1]] > g1_list[[d]])
    cat(sprintf("  P(NA > %s) = %.1f%%\n", pid5_labels[d], p_greater * 100))
  }

  cat("\n--- Pairwise Contrasts (Recovery moderation): Antagonism versus other domains ---\n")
  g2_list <- vector("list", length(pid5_labels))
  for (d in seq_along(pid5_labels)) {
    g2_list[[d]] <- as_draw_vec(draws, paste0("g2[", d, "]")) * bundle$y_scale
  }

  for (d in c(1, 2, 4, 5)) {
    p_greater <- mean(g2_list[[3]] > g2_list[[d]])
    cat(sprintf("  P(Antagonism > %s) = %.1f%%\n", pid5_labels[d], p_greater * 100))
  }

  contrast_data <- tibble()

  for (d in 2:5) {
    diff <- g1_list[[1]] - g1_list[[d]]
    contrast_data <- bind_rows(
      contrast_data,
      tibble(
        outcome = outcome_name,
        outcome_label = outcome_label,
        outcome_unit = outcome_unit,
        contrast = paste0("NA - ", pid5_labels[d]),
        parameter = "Stress",
        difference = diff,
        p_positive = mean(diff > 0)
      )
    )
  }

  for (d in c(1, 2, 4, 5)) {
    diff <- g2_list[[3]] - g2_list[[d]]
    contrast_data <- bind_rows(
      contrast_data,
      tibble(
        outcome = outcome_name,
        outcome_label = outcome_label,
        outcome_unit = outcome_unit,
        contrast = paste0("Antagonism - ", pid5_labels[d]),
        parameter = "Recovery",
        difference = diff,
        p_positive = mean(diff > 0)
      )
    )
  }

  contrast_summary <- contrast_data %>%
    group_by(outcome, outcome_label, outcome_unit, contrast, parameter) %>%
    summarise(
      median_diff = median(difference),
      ci_lower = unname(quantile(difference, cri_probs[1])),
      ci_upper = unname(quantile(difference, cri_probs[2])),
      p_greater = unique(p_positive),
      .groups = "drop"
    )

  write_csv(
    contrast_summary,
    here(tab_dir, paste0("pairwise_contrasts_summary_", outcome_name, "_89cri.csv"))
  )

  # ------------------------------------------------------------------------------
  # PART 3: MAGNITUDE UNCERTAINTY AND DIRECTIONAL PROBABILITY
  # ------------------------------------------------------------------------------

  cat("\n=== PART 3: MAGNITUDE UNCERTAINTY AND DIRECTIONAL PROBABILITY ===\n\n")

  key_effects <- direction_summary %>%
    filter(domain %in% c("Negative Affectivity", "Antagonism", "Detachment")) %>%
    filter(
      (domain == "Negative Affectivity" & parameter == "Stress (gamma1)") |
        (domain == "Antagonism" & parameter == "Recovery (gamma2)") |
        (domain == "Detachment" & parameter == "Recovery (gamma2)")
    )

  # Magnitude cutoffs sono adattivi: 0.1, 0.25 e 0.5 SD dell'outcome originale.
  # Corrispondono rispettivamente a y_scale * c(.1, .25, .5).
  cutoffs <- bundle$y_scale * c(0.10, 0.25, 0.50)

  cat("Key posterior summaries:\n")
  for (i in seq_len(nrow(key_effects))) {
    cat(sprintf("\n%s - %s:\n", key_effects$domain[i], key_effects$parameter[i]))
    cat(sprintf(
      "  Median effect: %.3f %s [%s CrI: %.3f, %.3f]\n",
      key_effects$median[i],
      outcome_unit,
      cri_label,
      key_effects$ci_lower[i],
      key_effects$ci_upper[i]
    ))
    cat(sprintf(
      "  Probability of direction: %.1f%% (PD = %.3f)\n",
      key_effects$pd[i] * 100,
      key_effects$pd[i]
    ))
    cat(sprintf("  Signal-to-noise ratio: %.2f\n", key_effects$snr[i]))

    if (key_effects$domain[i] == "Negative Affectivity") {
      g <- g1_list[[1]]
    } else if (key_effects$domain[i] == "Antagonism") {
      g <- g2_list[[3]]
    } else {
      g <- g2_list[[2]]
    }

    cat("  Magnitude probabilities, adaptive to outcome scale:\n")
    for (cut in cutoffs) {
      cat(sprintf("    P(|effect| > %.3f %s) = %.1f%%\n",
                  cut, outcome_unit, mean(abs(g) > cut) * 100))
    }
  }

  # ------------------------------------------------------------------------------
  # VISUALIZATION 1: Directional posterior summaries
  # ------------------------------------------------------------------------------

  direction_plot_data <- direction_summary %>%
    mutate(domain = factor(domain, levels = rev(pid5_labels)))

  x_pad <- 0.08 * diff(range(c(direction_plot_data$ci_lower, direction_plot_data$ci_upper)))
  if (!is.finite(x_pad) || x_pad == 0) x_pad <- 0.1

  fig_direction <- ggplot(
    direction_plot_data,
    aes(x = median, y = domain)
  ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
    geom_linerange(
      aes(xmin = ci_lower, xmax = ci_upper),
      linewidth = 1.3,
      alpha = 0.8,
      color = "gray40"
    ) +
    geom_point(size = 4, shape = 21, stroke = 0.5, fill = "white", color = "black") +
    geom_text(
      aes(
        label = sprintf("PD=%.2f", pd),
        x = ifelse(median > 0, ci_upper + x_pad, ci_lower - x_pad),
        hjust = ifelse(median > 0, 0, 1)
      ),
      size = 3,
      color = "gray30"
    ) +
    facet_wrap(~parameter) +
    labs(
      x = paste0("Moderation effect (", outcome_unit, " per SD)"),
      y = NULL,
      title = paste0("Directional Posterior Summaries: ", outcome_label),
      subtitle = paste0("Posterior medians with ", cri_label,
                        " credible intervals; PD is descriptive")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 11)
    )

  print(fig_direction)

  ggsave(
    filename = file.path(fig_dir, paste0("figure_direction_posterior_summary_", outcome_name, "_89cri.png")),
    plot = fig_direction,
    width = 12,
    height = 6,
    dpi = 300
  )

  ggsave(
    filename = file.path(fig_dir, paste0("figure_direction_posterior_summary_", outcome_name, "_89cri.pdf")),
    plot = fig_direction,
    width = 12,
    height = 6
  )

  # ------------------------------------------------------------------------------
  # VISUALIZATION 2: Posterior contrasts for selected patterns
  # ------------------------------------------------------------------------------

  fig_contrasts <- contrast_data %>%
    mutate(contrast = factor(contrast, levels = unique(contrast_summary$contrast))) %>%
    ggplot(aes(x = difference, y = contrast)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    stat_halfeye(
      .width = cri_level,
      point_interval = "median_qi",
      fill = "gray70",
      alpha = 0.7,
      normalize = "xy"
    ) +
    geom_text(
      data = contrast_summary,
      aes(
        x = ci_upper + x_pad,
        y = contrast,
        label = sprintf("P>0: %.1f%%", p_greater * 100)
      ),
      inherit.aes = FALSE,
      hjust = 0,
      size = 3
    ) +
    facet_wrap(~parameter, scales = "free_y") +
    labs(
      x = paste0("Difference in moderation effect (", outcome_unit, ")"),
      y = NULL,
      title = paste0("Pairwise Posterior Contrasts: ", outcome_label),
      subtitle = paste0("Distributions show posterior differences; intervals are ",
                        cri_label, " credible intervals")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 11)
    )

  print(fig_contrasts)

  ggsave(
    filename = file.path(fig_dir, paste0("figure_pairwise_contrasts_", outcome_name, "_89cri.png")),
    plot = fig_contrasts,
    width = 12,
    height = 8,
    dpi = 300
  )

  ggsave(
    filename = file.path(fig_dir, paste0("figure_pairwise_contrasts_", outcome_name, "_89cri.pdf")),
    plot = fig_contrasts,
    width = 12,
    height = 8
  )

  # ------------------------------------------------------------------------------
  # VISUALIZATION 3: Signal-to-noise ratio
  # ------------------------------------------------------------------------------

  snr_plot_data <- direction_summary %>%
    mutate(domain = factor(domain, levels = rev(pid5_labels)))

  fig_snr <- ggplot(snr_plot_data, aes(x = snr, y = domain)) +
    geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = 1.5, linetype = "dotted", alpha = 0.5) +
    geom_segment(
      aes(x = 0, xend = snr, y = domain, yend = domain),
      linewidth = 1.3,
      color = "gray50"
    ) +
    geom_point(size = 4, color = "black") +
    facet_wrap(~parameter) +
    labs(
      x = "Signal-to-noise ratio: |posterior median| / MAD",
      y = NULL,
      title = paste0("Effect Magnitude Relative to Posterior Uncertainty: ", outcome_label),
      subtitle = "Reference lines at SNR = 1.0 and 1.5 are descriptive only"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 11)
    )

  print(fig_snr)

  ggsave(
    filename = file.path(fig_dir, paste0("figure_snr_descriptive_", outcome_name, ".png")),
    plot = fig_snr,
    width = 12,
    height = 6,
    dpi = 300
  )

  ggsave(
    filename = file.path(fig_dir, paste0("figure_snr_descriptive_", outcome_name, ".pdf")),
    plot = fig_snr,
    width = 12,
    height = 6
  )

  # ------------------------------------------------------------------------------
  # PART 4: THEORETICAL ALIGNMENT
  # ------------------------------------------------------------------------------

  cat("\n=== PART 4: THEORETICAL ALIGNMENT ===\n\n")
  cat("Default F2 theoretical predictions are ambiguous; edit make_theoretical_predictions() if needed.\n")

  theoretical_predictions <- make_theoretical_predictions(outcome_name)

  alignment_table <- direction_summary %>%
    left_join(theoretical_predictions, by = c("domain", "parameter")) %>%
    mutate(
      observed_direction = ifelse(median > 0, "positive", "negative"),
      alignment = case_when(
        predicted_direction == "ambiguous" ~ "No clear prediction",
        observed_direction == predicted_direction ~ "Same direction as prediction",
        observed_direction != predicted_direction ~ "Opposite direction from prediction",
        TRUE ~ "Unclear"
      )
    ) %>%
    select(
      outcome,
      outcome_label,
      domain,
      parameter,
      predicted_direction,
      observed_direction,
      median,
      ci_lower,
      ci_upper,
      pd,
      alignment,
      rationale
    )

  print(alignment_table, n = Inf)

  write_csv(
    alignment_table,
    here(tab_dir, paste0("theoretical_alignment_table_", outcome_name, "_descriptive_89cri.csv"))
  )

  # ------------------------------------------------------------------------------
  # PART 5: INDIVIDUAL-LEVEL HETEROGENEITY
  # ------------------------------------------------------------------------------

  cat("\n=== PART 5: INDIVIDUAL-LEVEL HETEROGENEITY ===\n\n")

  theta_df <- posterior::as_draws_df(fit$draws(variables = "theta"))
  u_df <- posterior::as_draws_df(fit$draws(variables = "u"))

  b1_raw <- as_draw_vec(draws, "b1") * bundle$y_scale
  g1_na_raw <- as_draw_vec(draws, "g1[1]") * bundle$y_scale

  individual_responses <- tibble()

  for (i in seq_len(stan_data$N_subj)) {
    theta_i <- mean(as.numeric(theta_df[[paste0("theta[", i, ",1]")]]))
    u_i_slope_raw <- mean(as.numeric(u_df[[paste0("u[", i, ",2]")]]) * bundle$y_scale)

    # Stress response = change from baseline to pre-stress; Delta c1 = 1.
    stress_response <- mean(b1_raw) + u_i_slope_raw + mean(g1_na_raw) * theta_i

    individual_responses <- bind_rows(
      individual_responses,
      tibble(
        outcome = outcome_name,
        subject = i,
        theta_na = theta_i,
        stress_response = stress_response
      )
    )
  }

  cor_theta_stress <- cor(
    individual_responses$theta_na,
    individual_responses$stress_response
  )

  cat("Individual-level heterogeneity (Negative Affectivity):\n")
  cat(sprintf("  Correlation (theta_NA, stress_response): r = %.3f\n", cor_theta_stress))
  cat(sprintf("  Explained variance: r^2 = %.1f%%\n", cor_theta_stress^2 * 100))
  cat(sprintf(
    "  Unexplained variance: %.1f%% (random effects + other factors)\n",
    (1 - cor_theta_stress^2) * 100
  ))

  write_csv(
    individual_responses,
    here(tab_dir, paste0("individual_predicted_stress_responses_", outcome_name, ".csv"))
  )

  fig_individual <- ggplot(
    individual_responses,
    aes(x = theta_na, y = stress_response)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(alpha = 0.6, size = 3, color = "gray40") +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = sprintf("r = %.3f\nr^2 = %.1f%%", cor_theta_stress, cor_theta_stress^2 * 100),
      hjust = 1.1,
      vjust = 1.5,
      size = 5
    ) +
    labs(
      x = "Negative Affectivity (latent trait theta)",
      y = paste0("Predicted stress response in ", outcome_label, " (", outcome_unit, ")"),
      title = paste0("Individual Variation in Predicted Stress Response: ", outcome_label),
      subtitle = paste0("N = ", stan_data$N_subj, " subjects; smooth line is descriptive")
    ) +
    theme_minimal(base_size = 12)

  print(fig_individual)

  ggsave(
    filename = file.path(fig_dir, paste0("figure_individual_heterogeneity_", outcome_name, "_descriptive.png")),
    plot = fig_individual,
    width = 8,
    height = 6,
    dpi = 300
  )

  ggsave(
    filename = file.path(fig_dir, paste0("figure_individual_heterogeneity_", outcome_name, "_descriptive.pdf")),
    plot = fig_individual,
    width = 8,
    height = 6
  )

  cat("\n=== ANALYSIS COMPLETE FOR ", outcome_name, " ===\n", sep = "")
  cat("Tables saved to: ", tab_dir, "\n", sep = "")
  cat("Figures saved to: ", fig_dir, "\n", sep = "")

  list(
    direction_summary = direction_summary,
    contrast_summary = contrast_summary,
    alignment_table = alignment_table,
    individual_responses = individual_responses
  )
}

# ==============================================================================
# Esecuzione multi-outcome
# ==============================================================================

results <- map(outcomes_to_analyze, analyze_one_outcome)
names(results) <- outcomes_to_analyze

all_direction <- map_dfr(results, "direction_summary")
all_contrasts <- map_dfr(results, "contrast_summary")
all_alignment <- map_dfr(results, "alignment_table")
all_individual <- map_dfr(results, "individual_responses")

tab_dir <- here("results", "F2", "tables")
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(all_direction, here(tab_dir, "direction_posterior_summary_all_f2_outcomes_89cri.csv"))
write_csv(all_contrasts, here(tab_dir, "pairwise_contrasts_summary_all_f2_outcomes_89cri.csv"))
write_csv(all_alignment, here(tab_dir, "theoretical_alignment_table_all_f2_outcomes_descriptive_89cri.csv"))
write_csv(all_individual, here(tab_dir, "individual_predicted_stress_responses_all_f2_outcomes.csv"))

cat("\n=== ALL F2 DIFFERENTIAL EFFECTS ANALYSES COMPLETE ===\n")
cat("Outcomes processed:\n")
cat(paste(" -", outcomes_to_analyze), sep = "\n")
cat("\n")
cat("Combined tables saved in: ", tab_dir, "\n", sep = "")

# eof
