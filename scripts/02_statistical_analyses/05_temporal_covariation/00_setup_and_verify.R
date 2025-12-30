# ==============================================================================
# 00_setup_and_verify.R
# Setup iniziale e verifica prerequisiti
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

cat("=== SETUP PIPELINE WITHIN-PERSON COVARIATION ===\n\n")

# ==============================================================================
# PREREQUISITI
# ==============================================================================

cat("PREREQUISITI - File necessari:\n\n")

required_files <- list(
  voice = "data/raw/acustic_features/datiacustici/AUDIO.xlsx",
  ema = "data/processed/ema_plus_scales_cleaned.csv",
  meta = "data/raw/meta/all_combined_sex_NEW_1.xlsx"
)

cat("File richiesti:\n")
all_present <- TRUE
for (name in names(required_files)) {
  path <- here::here(required_files[[name]])
  exists <- file.exists(path)
  status <- if (exists) "✓" else "✗"
  cat(sprintf("  [%s] %s: %s\n", status, name, required_files[[name]]))
  if (!exists) all_present <- FALSE
}

cat("\n")

if (!all_present) {
  cat("⚠️  ATTENZIONE: Alcuni file mancano!\n\n")
  cat("AZIONE RICHIESTA:\n")
  cat("1. Assicurati di avere i file raw del progetto voice-stress\n")
  cat("2. Posizionali nelle directory indicate sopra\n")
  cat("3. Riesegui questo script\n\n")
  cat("Oppure, se i file sono in posizioni diverse:\n")
  cat("  - Modifica le path in questo script (sezione required_files)\n\n")
  stop("File mancanti. Setup incompleto.")
}

# ==============================================================================
# CREA STRUTTURA DIRECTORY
# ==============================================================================

cat("Creazione struttura directory...\n")

dirs <- c(
  "results/within_person_final",
  "results/within_person_final/data",
  "results/within_person_final/stan_models",
  "results/within_person_final/fitted_models",
  "results/within_person_final/heterogeneity_analysis",
  "results/within_person_final/manuscript_materials",
  "results/within_person_final/figures"
)

for (dir in dirs) {
  dir.create(here::here(dir), showWarnings = FALSE, recursive = TRUE)
  cat(sprintf("  ✓ %s\n", dir))
}

cat("\n")

# ==============================================================================
# VERIFICA PACCHETTI
# ==============================================================================

cat("Verifica pacchetti R...\n")

required_packages <- c(
  "tidyverse",
  "readxl",
  "lubridate",
  "here",
  "cmdstanr",
  "posterior",
  "bayesplot",
  "loo",
  "ggplot2",
  "patchwork",
  "knitr",
  "kableExtra"
)

missing_packages <- c()
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
    cat(sprintf("  ✗ %s (mancante)\n", pkg))
  } else {
    cat(sprintf("  ✓ %s\n", pkg))
  }
}

if (length(missing_packages) > 0) {
  cat("\n⚠️  Pacchetti mancanti:\n")
  cat(paste("  -", missing_packages, collapse = "\n"))
  cat("\n\nInstallali con:\n")
  cat(sprintf(
    '  install.packages(c(%s))\n',
    paste0('"', missing_packages, '"', collapse = ", ")
  ))
  stop("Pacchetti mancanti.")
}

cat("\n")

# ==============================================================================
# VERIFICA CMDSTAN
# ==============================================================================

cat("Verifica CmdStan...\n")

if (requireNamespace("cmdstanr", quietly = TRUE)) {
  tryCatch(
    {
      cmdstan_path <- cmdstanr::cmdstan_path()
      cmdstan_version <- cmdstanr::cmdstan_version()
      cat(sprintf(
        "  ✓ CmdStan %s trovato in: %s\n",
        cmdstan_version,
        cmdstan_path
      ))
    },
    error = function(e) {
      cat("  ✗ CmdStan non configurato\n")
      cat("\n  Installa con:\n")
      cat('    cmdstanr::install_cmdstan()\n\n')
      stop("CmdStan non disponibile.")
    }
  )
} else {
  stop("cmdstanr non installato.")
}

cat("\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("✓ SETUP COMPLETATO CON SUCCESSO\n")
cat(rep("=", 80), "\n\n")

cat("PROSSIMI PASSI:\n")
cat("1. source('01_prepare_within_person_data.R')\n")
cat("2. source('02_fit_models.R')\n")
cat("3. source('03_analyze_heterogeneity.R')\n")
cat("4. source('04_create_manuscript_materials.R')\n\n")

cat("TEMPO STIMATO TOTALE: 3-5 ore (mostly fitting)\n\n")

# Salva info sessione
session_info <- sessionInfo()
saveRDS(
  session_info,
  here::here("results/within_person_final/session_info.rds")
)

cat("✓ Setup info salvate\n\n")

# eof ---
