# ==============================================================================
# 05_create_table1_f2_moderation.R
# Genera tabelle LaTeX/CSV per la moderation analysis su outcome F2/articolatori.
# Compatibile con:
# - 01_prepare_stan_data_f2_pid5.R
# - 02_f2_pid5_moderation.R
# - 04_differential_effects_analysis_f2.R
#
# Versione con intervalli di credibilita' all'89%.
# Le stime sono sulla scala originale dell'outcome, perche' lo script 04
# converte i coefficienti dal modello standardizzato usando y_scale.
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(knitr)
  library(kableExtra)
})

# ==============================================================================
# SETTINGS
# ==============================================================================

cri_level <- 0.89
cri_label <- paste0(round(cri_level * 100), "\\%")

# Deve corrispondere agli outcome analizzati in 04_differential_effects_analysis_f2.R.
outcomes_to_table <- c(
  "f2_range_norm",
  "f2_mean",
  "vsa_log",
  "centralization"
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

escape_latex <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("_", "\\\\_", x, fixed = TRUE)
  x <- gsub("%", "\\\\%", x, fixed = TRUE)
  x <- gsub("&", "\\\\&", x, fixed = TRUE)
  x <- gsub("#", "\\\\#", x, fixed = TRUE)
  x
}

# ==============================================================================
# Funzione per una tabella outcome-specifica
# ==============================================================================

make_table_one_outcome <- function(outcome_name) {
  tab_dir <- here("results", "F2", "tables")
  dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

  input_csv <- here(
    tab_dir,
    paste0("direction_posterior_summary_", outcome_name, "_89cri.csv")
  )

  if (!file.exists(input_csv)) {
    stop(
      "Missing input table: ", input_csv,
      "\nRun 04_differential_effects_analysis_f2.R first."
    )
  }

  f2_direction <- read_csv(input_csv, show_col_types = FALSE)

  outcome_label <- pretty_outcome(outcome_name)
  outcome_unit <- unit_outcome(outcome_name)

  table1_data <- f2_direction %>%
    mutate(
      estimate = sprintf("%.3f [%.3f, %.3f]", median, ci_lower, ci_upper),
      pd_fmt = sprintf("%.2f", pd)
    ) %>%
    select(domain, parameter, estimate, pd_fmt)

  table1_wide <- table1_data %>%
    pivot_wider(
      names_from = parameter,
      values_from = c(estimate, pd_fmt),
      names_sep = "_"
    ) %>%
    select(
      domain,
      `estimate_Stress (gamma1)`,
      `pd_fmt_Stress (gamma1)`,
      `estimate_Recovery (gamma2)`,
      `pd_fmt_Recovery (gamma2)`
    ) %>%
    rename(
      Domain = domain,
      Stress_Est = `estimate_Stress (gamma1)`,
      Stress_PD = `pd_fmt_Stress (gamma1)`,
      Recovery_Est = `estimate_Recovery (gamma2)`,
      Recovery_PD = `pd_fmt_Recovery (gamma2)`
    )

  latex_caption <- paste0(
    "Personality Moderation of ",
    escape_latex(outcome_label)
  )

  latex_label <- paste0("tab:f2-", gsub("_", "-", outcome_name), "-moderation")

  latex_table <- c(
    "\\begin{table}[h]",
    "\\centering",
    paste0("\\caption{", latex_caption, "}"),
    paste0("\\label{", latex_label, "}"),
    "\\small",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcccc}",
    "\\hline",
    "\\multirow{2}{*}{Domain} & \\multicolumn{2}{c}{Stress Moderation ($\\gamma_1$)} & \\multicolumn{2}{c}{Recovery Moderation ($\\gamma_2$)} \\\\",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
    paste0(
      " & Median [",
      cri_label,
      " CrI] & PD & Median [",
      cri_label,
      " CrI] & PD \\\\") ,
    "\\hline"
  )

  for (i in seq_len(nrow(table1_wide))) {
    row <- table1_wide[i, ]
    table_row <- sprintf(
      "%s & %s & %s & %s & %s \\\\",
      escape_latex(row$Domain),
      row$Stress_Est,
      row$Stress_PD,
      row$Recovery_Est,
      row$Recovery_PD
    )
    latex_table <- c(latex_table, table_row)
  }

  note <- paste0(
    "\\item \\textit{Note.} Moderation effects represent the change in ",
    escape_latex(outcome_label),
    " (", escape_latex(outcome_unit), ") associated with a one-standard-deviation ",
    "increase in the PID-5 trait. Values are posterior medians with 89\\% ",
    "credible intervals. The Stan model used a standardized outcome; estimates ",
    "shown here are back-transformed to the original outcome scale using the ",
    "bundle-specific y_scale. PD = Probability of Direction, computed as the ",
    "maximum of P($\\gamma$ > 0) and P($\\gamma$ < 0). PD is reported ",
    "descriptively and is not used as a threshold for statistical significance. ",
    "CrI = Credible Interval."
  )

  latex_table <- c(
    latex_table,
    "\\hline",
    "\\end{tabular}",
    "\\begin{tablenotes}",
    "\\small",
    note,
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  tex_path <- here(tab_dir, paste0("table1_", outcome_name, "_moderation_89cri.tex"))
  csv_path <- here(tab_dir, paste0("table1_", outcome_name, "_moderation_89cri.csv"))

  writeLines(latex_table, tex_path)

  table1_csv <- table1_wide %>%
    mutate(outcome = outcome_name, outcome_label = outcome_label, .before = 1) %>%
    select(
      outcome,
      outcome_label,
      Domain,
      Stress_Est,
      Stress_PD,
      Recovery_Est,
      Recovery_PD
    )

  write_csv(table1_csv, csv_path)

  cat("\n=== TABLE CREATED FOR ", outcome_name, " ===\n", sep = "")
  cat("Saved LaTeX to: ", tex_path, "\n", sep = "")
  cat("Saved CSV to: ", csv_path, "\n", sep = "")

  cat("\nHighest PD values for ", outcome_name, ":\n", sep = "")
  top_pd <- f2_direction %>%
    arrange(desc(pd)) %>%
    select(domain, parameter, median, ci_lower, ci_upper, pd) %>%
    slice_head(n = 5)
  print(top_pd)

  table1_csv
}

# ==============================================================================
# Esecuzione multi-outcome
# ==============================================================================

all_tables <- map_dfr(outcomes_to_table, make_table_one_outcome)

tab_dir <- here("results", "F2", "tables")
write_csv(all_tables, here(tab_dir, "table1_all_f2_moderation_89cri.csv"))

combined_tex <- map(outcomes_to_table, function(outcome_name) {
  readLines(here(tab_dir, paste0("table1_", outcome_name, "_moderation_89cri.tex")))
}) %>%
  unlist()

writeLines(
  combined_tex,
  here(tab_dir, "table1_all_f2_moderation_89cri.tex")
)

cat("\n=== ALL F2 TABLES CREATED ===\n")
cat("Combined CSV: results/F2/tables/table1_all_f2_moderation_89cri.csv\n")
cat("Combined LaTeX: results/F2/tables/table1_all_f2_moderation_89cri.tex\n")
cat("\nTable reports posterior medians, 89% credible intervals, and PD values.\n")
cat("No PD threshold is used for bolding or classifying effects.\n")

# eof
