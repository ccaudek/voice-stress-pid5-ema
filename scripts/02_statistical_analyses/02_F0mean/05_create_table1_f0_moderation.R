# ==============================================================================
# create_table1_f0_moderation.R
# Genera Table 1: Personality Moderation of F0 per inserimento in .Rmd
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(knitr)
  library(kableExtra)
})

# ==============================================================================
# LOAD RESULTS
# ==============================================================================

# Carica direction certainty table (generata da 04_differential_effects_analysis.R)
f0_direction <- read_csv(
  here("results", "f0mean", "direction_certainty_table.csv"),
  show_col_types = FALSE
)

# ==============================================================================
# PREPARE TABLE 1 DATA
# ==============================================================================

# Filtra solo F0, formatta per tabella
table1_data <- f0_direction %>%
  mutate(
    # Formatta estimate come "median [CI]"
    estimate = sprintf("%.2f [%.2f, %.2f]", median, ci_lower, ci_upper),
    # Formatta PD
    pd_fmt = sprintf("%.2f", pd),
    # Evidenzia Strong effects (PD > 0.95)
    is_strong = pd > 0.95
  ) %>%
  select(domain, parameter, estimate, pd_fmt, is_strong)

# Pivot per avere Stress e Recovery come colonne
table1_wide <- table1_data %>%
  pivot_wider(
    names_from = parameter,
    values_from = c(estimate, pd_fmt, is_strong),
    names_sep = "_"
  ) %>%
  # Riordina colonne
  select(
    domain,
    `estimate_Stress (γ₁)`,
    `pd_fmt_Stress (γ₁)`,
    `is_strong_Stress (γ₁)`,
    `estimate_Recovery (γ₂)`,
    `pd_fmt_Recovery (γ₂)`,
    `is_strong_Recovery (γ₂)`
  ) %>%
  # Rinomina colonne per chiarezza
  rename(
    Domain = domain,
    Stress_Est = `estimate_Stress (γ₁)`,
    Stress_PD = `pd_fmt_Stress (γ₁)`,
    Stress_Strong = `is_strong_Stress (γ₁)`,
    Recovery_Est = `estimate_Recovery (γ₂)`,
    Recovery_PD = `pd_fmt_Recovery (γ₂)`,
    Recovery_Strong = `is_strong_Recovery (γ₂)`
  )

# ==============================================================================
# CREATE LATEX TABLE
# ==============================================================================

# Crea versione LaTeX manuale (per controllo completo)
latex_table <- c(
  "\\begin{table}[h]",
  "\\caption{Personality Moderation of Fundamental Frequency (F0)}",
  "\\label{tab:f0-moderation}",
  "\\small",
  "\\begin{tabular}{lcccc}",
  "\\hline",
  "\\multirow{2}{*}{Domain} & \\multicolumn{2}{c}{Stress Moderation ($\\gamma_1$)} & \\multicolumn{2}{c}{Recovery Moderation ($\\gamma_2$)} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  " & Median [95\\% CrI] & PD & Median [95\\% CrI] & PD \\\\",
  "\\hline"
)

# Aggiungi righe dati
for (i in 1:nrow(table1_wide)) {
  row <- table1_wide[i, ]

  # Formatta estimate con bold se strong
  stress_est <- if (row$Stress_Strong) {
    paste0("\\textbf{", row$Stress_Est, "}")
  } else {
    row$Stress_Est
  }

  stress_pd <- if (row$Stress_Strong) {
    paste0("\\textbf{", row$Stress_PD, "}")
  } else {
    row$Stress_PD
  }

  recovery_est <- if (row$Recovery_Strong) {
    paste0("\\textbf{", row$Recovery_Est, "}")
  } else {
    row$Recovery_Est
  }

  recovery_pd <- if (row$Recovery_Strong) {
    paste0("\\textbf{", row$Recovery_PD, "}")
  } else {
    row$Recovery_PD
  }

  # Crea riga
  table_row <- sprintf(
    "%s & %s & %s & %s & %s \\\\",
    row$Domain,
    stress_est,
    stress_pd,
    recovery_est,
    recovery_pd
  )

  latex_table <- c(latex_table, table_row)
}

# Chiudi tabella
latex_table <- c(
  latex_table,
  "\\hline",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\small",
  "\\item \\textit{Note.} Moderation effects represent the change in F0 (Hz) associated with a one-standard-deviation increase in the trait. PD = Probability of Direction (maximum of P($\\gamma$ > 0) and P($\\gamma$ < 0)). Bold indicates strong directional certainty (PD > 0.95). CrI = Credible Interval.",
  "\\end{tablenotes}",
  "\\end{table}"
)

# ==============================================================================
# SAVE OUTPUT
# ==============================================================================

# Salva come file .tex
writeLines(latex_table, here("results", "table1_f0_moderation.tex"))

cat("\n=== TABLE 1 CREATED ===\n\n")
cat("Saved to: results/table1_f0_moderation.tex\n\n")

# Stampa preview
cat("LaTeX code to insert in .Rmd:\n")
cat("----------------------------------------\n")
cat(paste(latex_table, collapse = "\n"))
cat("\n----------------------------------------\n")

# Salva anche versione CSV per reference
table1_csv <- table1_wide %>%
  select(Domain, Stress_Est, Stress_PD, Recovery_Est, Recovery_PD)

write_csv(table1_csv, here("results", "table1_f0_moderation.csv"))
cat("\nAlso saved CSV version: results/table1_f0_moderation.csv\n")

# ==============================================================================
# PRINT SUMMARY
# ==============================================================================

cat("\n=== SUMMARY ===\n")
cat("Strong effects (PD > 0.95):\n")

strong_effects <- table1_wide %>%
  filter(Stress_Strong | Recovery_Strong) %>%
  select(Domain, Stress_PD, Recovery_PD)

if (nrow(strong_effects) > 0) {
  print(strong_effects)
} else {
  cat("  None\n")
}

cat("\n=== NEXT STEPS ===\n")
cat("1. Copy the LaTeX code above\n")
cat("2. Insert in .Rmd after line 283 (after Antagonism paragraph)\n")
cat("3. Verify formatting in compiled PDF\n")

# eof ---
