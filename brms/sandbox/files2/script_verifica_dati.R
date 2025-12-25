# =============================================================================
# SCRIPT DI VERIFICA STRUTTURA DATI E CONTRASTI
# =============================================================================

library(tidyverse)

df_analysis <- readRDS("results/df_analysis.rds")

cat("=== STRUTTURA DATASET ===\n")
glimpse(df_analysis)

cat("\n=== DIMENSIONI ===\n")
cat("N righe:", nrow(df_analysis), "\n")
cat("N colonne:", ncol(df_analysis), "\n")

cat("\n=== TIMEPOINTS ===\n")
print(table(df_analysis$timepoint))

cat("\n=== VERIFICA ID UNICI ===\n")
n_unique_ids <- n_distinct(df_analysis$ID)
cat("N partecipanti unici:", n_unique_ids, "\n")
cat("N righe / N partecipanti:", nrow(df_analysis) / n_unique_ids, "\n")

cat("\n=== VERIFICA VARIABILI PID5 ===\n")
cat("Variabili PID5 centrate presenti:\n")
print(names(df_analysis)[str_detect(names(df_analysis), "pid5_.*_c$")])

cat("\n=== VERIFICA VARIABILI ACUSTICHE ===\n")
cat("\nF0 variables:\n")
print(names(df_analysis)[str_detect(names(df_analysis), "^f0_")])

cat("\nF2 variables:\n")
print(names(df_analysis)[str_detect(names(df_analysis), "^f2_")])

cat("\n=== STATISTICHE DESCRITTIVE F2 ===\n")
df_analysis %>%
  select(starts_with("f2_")) %>%
  summary()

cat("\n=== MISSING DATA ===\n")
missing_summary <- df_analysis %>%
  summarise(across(everything(), ~sum(is.na(.))))  %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  filter(n_missing > 0) %>%
  arrange(desc(n_missing))

print(missing_summary)

# Salva per riferimento
write_csv(missing_summary, "results/00_missing_data_summary.csv")

cat("\n=== VERIFICA CONTRASTI (se esistono già) ===\n")
if ("c1_stress" %in% names(df_analysis)) {
  cat("\nContrasti già presenti nel dataset!\n")
  df_analysis %>%
    group_by(timepoint) %>%
    summarise(
      c1_stress = mean(c1_stress, na.rm = TRUE),
      c2_recovery = mean(c2_recovery, na.rm = TRUE),
      n = n()
    ) %>%
    print()
} else {
  cat("\nContrasti NON presenti - devo crearli\n")
  cat("Livelli timepoint:", unique(df_analysis$timepoint), "\n")
}

cat("\n=== CONSIGLIO PER CONTRASTI ===\n")
cat("
Se i livelli di timepoint sono: baseline, pre-exam, post-exam
Allora i contrasti dovrebbero essere:

c1_stress (stress effect: baseline → pre-exam):
  baseline:  -0.5
  pre-exam:   0.5  
  post-exam:  0

c2_recovery (recovery effect: pre-exam → post-exam):
  baseline:   0
  pre-exam:  -0.5
  post-exam:  0.5

Verifica che questo corrisponda alla tua definizione teorica!
")

cat("\n=== SCRIPT COMPLETATO ===\n")
