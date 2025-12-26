# ==============================================================================
# 00_quick_test_setup.R
# Test rapido per verificare che dati e setup siano corretti
# ==============================================================================

cat("=== TEST SETUP ANALISI WITHIN-PERSON ===\n\n")

# ==============================================================================
# 1) VERIFICA PACCHETTI
# ==============================================================================

cat("1. Verifica pacchetti R...\n")

required_pkgs <- c(
  "tidyverse", "readxl", "here", "rio", "lubridate",
  "ggplot2", "corrplot", "jsonlite"
)

optional_pkgs <- c("cmdstanr", "posterior", "bayesplot", "loo")

missing_required <- setdiff(required_pkgs, rownames(installed.packages()))
missing_optional <- setdiff(optional_pkgs, rownames(installed.packages()))

if (length(missing_required) > 0) {
  cat("  ✗ ERRORE: Pacchetti mancanti (richiesti):\n")
  cat("   ", paste(missing_required, collapse = ", "), "\n")
  cat("    Installa con: install.packages(c('", 
      paste(missing_required, collapse = "', '"), "'))\n\n")
  stop("Pacchetti richiesti mancanti.")
} else {
  cat("  ✓ Tutti i pacchetti richiesti sono installati\n")
}

if (length(missing_optional) > 0) {
  cat("  ⚠ Pacchetti opzionali mancanti (per modello Stan):\n")
  cat("   ", paste(missing_optional, collapse = ", "), "\n")
  cat("    Analisi esplorativa comunque possibile\n")
} else {
  cat("  ✓ Tutti i pacchetti opzionali sono installati\n")
}

# ==============================================================================
# 2) VERIFICA FILE DATI
# ==============================================================================

cat("\n2. Verifica esistenza file dati...\n")

# Percorsi attesi
voice_path <- here::here(
  "data", "raw", "acustic_features", "datiacustici", "AUDIO.xlsx"
)
ema_path <- here::here("data", "processed", "ema_plus_scales_cleaned.csv")
meta_path <- here::here("data", "raw", "meta", "all_combined_sex_NEW_1.xlsx")

files_check <- list(
  "Dati vocali (AUDIO.xlsx)" = voice_path,
  "Dati EMA puliti (ema_plus_scales_cleaned.csv)" = ema_path,
  "Metadati (all_combined_sex_NEW_1.xlsx)" = meta_path
)

all_exist <- TRUE
for (label in names(files_check)) {
  path <- files_check[[label]]
  exists <- file.exists(path)
  
  if (exists) {
    cat("  ✓", label, "\n")
    cat("    ", path, "\n")
  } else {
    cat("  ✗", label, "- NON TROVATO\n")
    cat("    Cercato in:", path, "\n")
    all_exist <- FALSE
  }
}

if (!all_exist) {
  cat("\n  ⚠ ATTENZIONE: Alcuni file mancanti\n")
  cat("    Verifica i percorsi o modifica gli script\n\n")
}

# ==============================================================================
# 3) TEST CARICAMENTO DATI (se esistono)
# ==============================================================================

if (all_exist) {
  cat("\n3. Test caricamento dati...\n")
  
  suppressPackageStartupMessages({
    library(tidyverse)
    library(readxl)
    library(here)
    library(rio)
  })
  
  # Test voce
  tryCatch({
    baseline <- read_excel(voice_path, sheet = "BASELINE")
    pre <- read_excel(voice_path, sheet = "PRE")
    post <- read_excel(voice_path, sheet = "POST")
    
    df_voice <- bind_rows(
      baseline %>% mutate(timepoint = "baseline"),
      pre %>% mutate(timepoint = "pre"),
      post %>% mutate(timepoint = "post")
    )
    
    cat("  ✓ Dati vocali caricati:\n")
    cat("    ", nrow(df_voice), "osservazioni\n")
    cat("    ", n_distinct(df_voice$ID), "soggetti\n")
    cat("    ", sum(df_voice$timepoint == "baseline"), "baseline,",
        sum(df_voice$timepoint == "pre"), "pre,",
        sum(df_voice$timepoint == "post"), "post\n")
  }, error = function(e) {
    cat("  ✗ Errore caricamento dati vocali:\n")
    cat("    ", conditionMessage(e), "\n")
  })
  
  # Test EMA
  tryCatch({
    ema <- rio::import(ema_path)
    
    cat("  ✓ Dati EMA caricati:\n")
    cat("    ", nrow(ema), "osservazioni\n")
    cat("    ", n_distinct(ema$user_id), "soggetti\n")
    
    # Verifica PID-5 vars
    pid5_vars <- c(
      "pid5_negative_affectivity",
      "pid5_detachment",
      "pid5_antagonism",
      "pid5_disinhibition",
      "pid5_psychoticism"
    )
    
    if (all(pid5_vars %in% names(ema))) {
      cat("    ✓ Tutti i domini PID-5 presenti\n")
    } else {
      cat("    ✗ Domini PID-5 mancanti:\n")
      cat("      ", paste(setdiff(pid5_vars, names(ema)), collapse = ", "), "\n")
    }
    
    # Verifica exam_period
    if ("exam_period" %in% names(ema)) {
      cat("    ✓ Variabile exam_period presente\n")
      cat("      ", table(ema$exam_period), "\n")
    } else {
      cat("    ✗ Variabile exam_period mancante\n")
    }
    
  }, error = function(e) {
    cat("  ✗ Errore caricamento dati EMA:\n")
    cat("    ", conditionMessage(e), "\n")
  })
}

# ==============================================================================
# 4) VERIFICA CMDSTAN (se installato)
# ==============================================================================

if ("cmdstanr" %in% rownames(installed.packages())) {
  cat("\n4. Verifica cmdstanr e Stan...\n")
  
  tryCatch({
    library(cmdstanr)
    
    if (cmdstan_version() != "") {
      cat("  ✓ CmdStan installato:\n")
      cat("    Versione:", cmdstan_version(), "\n")
      cat("    Path:", cmdstan_path(), "\n")
    } else {
      cat("  ✗ CmdStan non trovato\n")
      cat("    Installa con: cmdstanr::install_cmdstan()\n")
    }
  }, error = function(e) {
    cat("  ✗ Errore verifica cmdstanr:\n")
    cat("    ", conditionMessage(e), "\n")
    cat("    Installa con: cmdstanr::install_cmdstan()\n")
  })
} else {
  cat("\n4. CmdStanR non installato (opzionale)\n")
  cat("    Installa per usare modello Stan:\n")
  cat("    install.packages('cmdstanr', repos = c('https://mc-stan.org/r-packages/', getOption('repos')))\n")
  cat("    cmdstanr::install_cmdstan()\n")
}

# ==============================================================================
# 5) SINTESI
# ==============================================================================

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("SINTESI TEST\n")
cat(rep("=", 80), "\n", sep = "")

if (length(missing_required) == 0 && all_exist) {
  cat("\n✓ TUTTO OK! Puoi procedere con:\n")
  cat("    source('04_within_person_ema_voice_covariation.R')\n")
  
  if (length(missing_optional) == 0 && "cmdstanr" %in% rownames(installed.packages())) {
    cat("\n  E poi (opzionale):\n")
    cat("    source('05_fit_stan_within_person_model.R')\n")
  }
} else {
  cat("\n⚠ PROBLEMI RILEVATI:\n")
  
  if (length(missing_required) > 0) {
    cat("  - Installa pacchetti richiesti\n")
  }
  
  if (!all_exist) {
    cat("  - Verifica percorsi file dati\n")
  }
  
  cat("\nConsulta README_WITHIN_PERSON.md per dettagli\n")
}

cat(rep("=", 80), "\n\n", sep = "")

# eof ---
