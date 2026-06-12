# ==============================================================================
# create_table1_f0_moderation.R
# Genera Table 1: Personality Moderation of F0 per inserimento in .Rmd
# Versione con intervalli di credibilità all'89%
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

# Questo script assume che direction_posterior_summary_89cri.csv sia stato
# generato dallo script 04_differential_effects_analysis.R corretto,
# cioè con intervalli di credibilità all'89%.
# NB: il vecchio file direction_certainty_table.csv (versione al 95%) va
# cancellato dal disco per evitare che venga letto per errore.
cri_level <- 0.89
cri_label <- paste0(round(cri_level * 100), "\\%")

# ==============================================================================
# LOAD RESULTS
# ==============================================================================

# Carica la tabella generata da 04_differential_effects_analysis.R
f0_direction <- read_csv(
  here("results", "F0", "direction_posterior_summary_89cri.csv"),
  show_col_types = FALSE
)

# ==============================================================================
# PREPARE TABLE 1 DATA
# ==============================================================================

# Formatta per tabella.
# Nota: PD viene riportata descrittivamente, senza soglie grafiche o bold.
table1_data <- f0_direction %>%
  mutate(
    estimate = sprintf("%.2f [%.2f, %.2f]", median, ci_lower, ci_upper),
    pd_fmt = sprintf("%.2f", pd)
  ) %>%
  select(domain, parameter, estimate, pd_fmt)

# Pivot per avere Stress e Recovery come colonne
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

# ==============================================================================
# CREATE LATEX TABLE
# ==============================================================================

latex_table <- c(
  "\\begin{table}[h]",
  "\\centering",
  "\\caption{Personality Moderation of Fundamental Frequency (F0)}",
  "\\label{tab:f0-moderation}",
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
    " CrI] & PD \\\\"
  ),
  "\\hline"
)

# Aggiungi righe dati
for (i in 1:nrow(table1_wide)) {
  row <- table1_wide[i, ]

  table_row <- sprintf(
    "%s & %s & %s & %s & %s \\\\",
    row$Domain,
    row$Stress_Est,
    row$Stress_PD,
    row$Recovery_Est,
    row$Recovery_PD
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
  "\\item \\textit{Note.} Moderation effects represent the change in F0 (Hz) associated with a one-standard-deviation increase in the trait. Values are posterior medians with 89\\% credible intervals. PD = Probability of Direction, computed as the maximum of P($\\gamma$ > 0) and P($\\gamma$ < 0). PD is reported descriptively and is not used as a threshold for statistical significance. CrI = Credible Interval.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

# ==============================================================================
# SAVE OUTPUT
# ==============================================================================

# Salva come file .tex
writeLines(
  latex_table,
  here("results", "F0", "tables", "table1_f0_moderation_89cri.tex")
)

cat("\n=== TABLE 1 CREATED ===\n\n")
cat("Saved to: results/F0/tables/table1_f0_moderation_89cri.tex\n\n")

# Stampa preview
cat("LaTeX code to insert in .Rmd:\n")
cat("----------------------------------------\n")
cat(paste(latex_table, collapse = "\n"))
cat("\n----------------------------------------\n")

# Salva anche versione CSV per reference
table1_csv <- table1_wide %>%
  select(
    Domain,
    Stress_Est,
    Stress_PD,
    Recovery_Est,
    Recovery_PD
  )

write_csv(
  table1_csv,
  here("results", "F0", "tables", "table1_f0_moderation_89cri.csv")
)

cat(
  "\nAlso saved CSV version: results/F0/tables/table1_f0_moderation_89cri.csv\n"
)

# ==============================================================================
# PRINT SUMMARY
# ==============================================================================

cat("\n=== SUMMARY ===\n")
cat("Table reports posterior medians, 89% credible intervals, and PD values.\n")
cat("No PD threshold is used for bolding or classifying effects.\n\n")

cat("Highest PD values:\n")

top_pd <- f0_direction %>%
  arrange(desc(pd)) %>%
  select(domain, parameter, median, ci_lower, ci_upper, pd) %>%
  slice_head(n = 5)

print(top_pd)
# domain               parameter         median ci_lower ci_upper    pd
# <chr>                <chr>              <dbl>    <dbl>    <dbl> <dbl>
# 1 Antagonism           Recovery (gamma2)  3.16     0.614    5.76  0.976
# 2 Negative Affectivity Stress (gamma1)    3.14     0.453    5.84  0.971
# 3 Detachment           Recovery (gamma2) -2.04    -4.76     0.727 0.880
# 4 Psychoticism         Recovery (gamma2) -1.42    -4.15     1.33  0.796
# 5 Negative Affectivity Recovery (gamma2) -0.452   -3.11     2.25  0.608

cat("\n=== NEXT STEPS ===\n")
cat("1. Use results/F0/tables/table1_f0_moderation_89cri.tex in the .Rmd\n")
cat(
  "2. Make sure 04_differential_effects_analysis.R was rerun with 89% CrI before this script\n"
)
cat("3. Verify formatting in the compiled PDF\n")

# eof
