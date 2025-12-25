#!/usr/bin/env Rscript
# =============================================================================
# MASTER SCRIPT - ANALISI FORMANT F2
# =============================================================================
#
# Questo script coordina l'intera pipeline di analisi formant.
# Esegue tutti gli step in sequenza con controlli di qualità.
#
# USAGE: source("00_master_script.R")
#
# =============================================================================

library(tidyverse)

cat("\n")
cat(
  "================================================================================\n"
)
cat("  SUITE ANALISI FORMANT F2 - MASTER SCRIPT\n")
cat(
  "================================================================================\n\n"
)

# Verifica che siamo nella directory corretta
if (!file.exists("results/df_analysis.rds")) {
  stop(
    "ERRORE: results/df_analysis.rds non trovato!\n",
    "Assicurati di essere nella directory corretta del progetto."
  )
}

# =============================================================================
# STEP 1: DIAGNOSTICA OUTLIERS
# =============================================================================

cat(">>> STEP 1: DIAGNOSTICA OUTLIERS\n\n")

if (
  !file.exists(
    here::here(
      "scripts",
      "ema_by_timepoint",
      "02_statistical_analyses",
      "formants",
      "01_diagnostica_outliers.R"
    )
  )
) {
  stop("ERRORE: Script 01_diagnostica_outliers.R non trovato!")
}

source(
  here::here(
    "scripts",
    "ema_by_timepoint",
    "02_statistical_analyses",
    "formants",
    "01_diagnostica_outliers.R"
  )
)

cat("\n")
cat(">>> STEP 1 COMPLETATO\n")
cat("    Controlla i file in figures/ per i plot diagnostici\n\n")

# =============================================================================
# STEP 2: DECISIONE WINSORIZZAZIONE
# =============================================================================

cat(">>> STEP 2: VERIFICA NECESSITÀ WINSORIZZAZIONE\n\n")

# Leggi summary outliers
outlier_summary <- read_csv(
  "results/f2_outlier_summary.csv",
  show_col_types = FALSE
)
mean_pct_outliers <- mean(outlier_summary$pct_outliers)

cat("    Percentuale media outliers:", round(mean_pct_outliers, 1), "%\n\n")

if (mean_pct_outliers > 2) {
  cat("    DECISIONE: Winsorizzazione NECESSARIA (>2% outliers)\n\n")

  if (
    !file.exists(
      here::here(
        "scripts",
        "ema_by_timepoint",
        "02_statistical_analyses",
        "formants",
        "02_winsorizzazione.R"
      )
    )
  ) {
    stop("ERRORE: Script 02_winsorizzazione.R non trovato!")
  }

  source(
    here::here(
      "scripts",
      "ema_by_timepoint",
      "02_statistical_analyses",
      "formants",
      "02_winsorizzazione.R"
    )
  )
  use_winsorized <- TRUE

  cat("\n>>> STEP 2 COMPLETATO: Dati winsorizzati\n\n")
} else {
  cat("    DECISIONE: Winsorizzazione NON necessaria (<2% outliers)\n")
  cat("    Procedo con dati originali\n\n")
  use_winsorized <- FALSE

  cat(">>> STEP 2 COMPLETATO: Skip winsorizzazione\n\n")
}

# =============================================================================
# STEP 3: PREPARAZIONE PRIOR
# =============================================================================

cat(">>> STEP 3: CALCOLO PRIOR INFORMATIVI\n\n")

if (
  !file.exists(
    here::here(
      "scripts",
      "ema_by_timepoint",
      "02_statistical_analyses",
      "formants",
      "03_prepara_prior.R"
    )
  )
) {
  stop("ERRORE: Script 03_prepara_prior.R non trovato!")
}

source(
  here::here(
    "scripts",
    "ema_by_timepoint",
    "02_statistical_analyses",
    "formants",
    "03_prepara_prior.R"
  )
)

cat("\n>>> STEP 3 COMPLETATO: Prior calcolati\n\n")

# =============================================================================
# STEP 4: TEST MODELLO SINGOLO
# =============================================================================

cat(">>> STEP 4: TEST MODELLO (F2 mean vowel /a/)\n\n")
cat("    Questo richiederà circa 10 minuti...\n\n")

if (
  !file.exists(
    here::here(
      "scripts",
      "ema_by_timepoint",
      "02_statistical_analyses",
      "formants",
      "04_test_modello.R"
    )
  )
) {
  stop("ERRORE: Script 04_test_modello.R non trovato!")
}

source(
  here::here(
    "scripts",
    "ema_by_timepoint",
    "02_statistical_analyses",
    "formants",
    "04_test_modello.R"
  )
)

# Verifica convergenza
if (!exists("test_converged") || !test_converged) {
  stop(
    "\nERRORE: Test modello non ha raggiunto la convergenza!\n",
    "Controlla diagnostica in figures/ e correggi problemi prima di procedere."
  )
}

cat("\n>>> STEP 4 COMPLETATO: Test model convergenza OK\n\n")

# =============================================================================
# STEP 5: ANALISI COMPLETA FORMANT
# =============================================================================

cat(">>> STEP 5: ANALISI COMPLETA FORMANT\n\n")
cat("    Questo richiederà 2-4 ore per 6 modelli...\n\n")

# Chiedi conferma
response <- readline(prompt = "Procedere con analisi completa? (y/n): ")

if (tolower(response) == "y") {
  if (!file.exists("05_analisi_formant_completa.R")) {
    stop("ERRORE: Script 05_analisi_formant_completa.R non trovato!")
  }

  source("05_analisi_formant_completa.R")

  cat("\n>>> STEP 5 COMPLETATO: Tutti i modelli fitted\n\n")
} else {
  cat("\n>>> STEP 5 SALTATO per richiesta utente\n\n")
  cat("Per eseguire più tardi: source('05_analisi_formant_completa.R')\n\n")
  stop("Analisi fermata dall'utente. Test completato con successo.")
}

# =============================================================================
# STEP 6: ESTRAZIONE RISULTATI
# =============================================================================

cat(">>> STEP 6: ESTRAZIONE E SINTESI RISULTATI\n\n")

if (!file.exists("06_estrai_risultati.R")) {
  stop("ERRORE: Script 06_estrai_risultati.R non trovato!")
}

source("06_estrai_risultati.R")

cat("\n>>> STEP 6 COMPLETATO: Risultati estratti\n\n")

# =============================================================================
# SUMMARY FINALE
# =============================================================================

cat("\n")
cat(
  "================================================================================\n"
)
cat("  ANALISI COMPLETATA CON SUCCESSO!\n")
cat(
  "================================================================================\n\n"
)

cat("Dataset usato:", ifelse(use_winsorized, "WINSORIZZATO", "ORIGINALE"), "\n")
cat("Outliers gestiti:", round(mean_pct_outliers, 1), "%\n\n")

cat("FILE PRODOTTI:\n")
cat("  results/\n")
cat("    - 05_formant_moderation_results.csv (tutti i risultati)\n")
cat("    - 05_formant_significant_effects.csv (solo significativi)\n")
cat("    - formant_models_summary.csv (summary tutti modelli)\n\n")
cat("  figures/\n")
cat("    - formant_interactions_*.pdf (plot interazioni)\n")
cat("    - convergence_diagnostics.pdf (diagnostica)\n\n")

cat("PROSSIMI PASSI:\n")
cat("  1. Verifica risultati in results/05_formant_significant_effects.csv\n")
cat("  2. Confronta con claims in abstract\n")
cat("  3. Aggiorna manoscritto con valori corretti\n\n")

cat(
  "================================================================================\n\n"
)
