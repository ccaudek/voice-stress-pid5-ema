# =============================================================================
# 03 - PREPARAZIONE PRIOR INFORMATIVI
# =============================================================================

library(tidyverse)

cat("Caricamento dati...\n")

# Usa winsorizzato se disponibile
if (file.exists("results/df_analysis_winsorized.rds")) {
  cat("  Usando dataset WINSORIZZATO\n")
  df_analysis <- readRDS("results/df_analysis_winsorized.rds")
} else {
  cat("  Usando dataset ORIGINALE\n")
  df_analysis <- readRDS("results/df_analysis.rds")
}

# Calcola statistiche per prior
prior_params <- tibble()

for (vowel in c("a", "i", "u")) {
  for (outcome_type in c("mean", "std")) {
    outcome <- paste0("f2_", outcome_type, "_", vowel)
    
    M <- mean(df_analysis[[outcome]], na.rm = TRUE)
    SD <- sd(df_analysis[[outcome]], na.rm = TRUE)
    
    # Prior intercetta: centrato su media, SD permette ±2.5 SD
    intercept_mean <- round(M, 0)
    intercept_sd <- round(SD * 2.5, 0)
    
    # Prior sigma (log scale)
    sigma_log_mean <- round(log(SD), 2)
    
    prior_params <- bind_rows(
      prior_params,
      tibble(
        vowel = vowel,
        outcome = paste0("f2_", outcome_type),
        data_mean = round(M, 1),
        data_sd = round(SD, 1),
        prior_intercept_mean = intercept_mean,
        prior_intercept_sd = intercept_sd,
        prior_sigma_log = sigma_log_mean
      )
    )
    
    cat("F2 ", outcome_type, " /", vowel, "/: M=", round(M, 0), 
        " SD=", round(SD, 0), "\n", sep = "")
  }
}

# Salva
write_csv(prior_params, "results/03_prior_parameters.csv")

cat("\nParametri prior salvati in results/03_prior_parameters.csv\n")

# Crea anche oggetto R per uso negli script successivi
saveRDS(prior_params, "results/03_prior_parameters.rds")
