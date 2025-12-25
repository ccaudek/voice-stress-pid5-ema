# =============================================================================
# 02bis - WINSORIZZAZIONE AGGRESSIVA F2
# =============================================================================
# Usa percentili 1/99 invece di 2.5/97.5 per gestire outliers estremi

library(tidyverse)

cat("Caricamento dati originali...\n")
df_analysis <- readRDS("results/df_analysis.rds")

robust_quantile_trimmed <- function(x, probs, trim = 0.025) {
  x <- x[!is.na(x)]
  q_trim <- quantile(x, c(trim, 1 - trim))
  x_trim <- x[x >= q_trim[1] & x <= q_trim[2]]
  quantile(x_trim, probs)
}

# Funzione winsorizzazione AGGRESSIVA
winsorize_aggressive <- function(x, probs = c(0.01, 0.99)) {
  # ← 1/99 invece 2.5/97.5
  limits <- robust_quantile_trimmed(x, probs = probs, trim = 0.025)
  x_wins <- x
  x_wins[x < limits[1]] <- limits[1]
  x_wins[x > limits[2]] <- limits[2]

  n_wins <- sum(x < limits[1] | x > limits[2], na.rm = TRUE)
  pct_wins <- 100 * n_wins / sum(!is.na(x))

  cat("  Range:", round(limits[1], 0), "-", round(limits[2], 0), "Hz\n")
  cat("  Winsorizzati:", n_wins, "(", round(pct_wins, 1), "%)\n")

  x_wins
}

# Winsorizza
df_winsorized <- df_analysis

for (vowel in c("a", "i", "u")) {
  cat("\nVocale /", vowel, "/:\n", sep = "")

  # F2 mean
  outcome_mean <- paste0("f2_mean_", vowel)
  df_winsorized[[outcome_mean]] <- winsorize_aggressive(df_winsorized[[
    outcome_mean
  ]])

  # F2 std
  outcome_std <- paste0("f2_std_", vowel)
  df_winsorized[[outcome_std]] <- winsorize_aggressive(df_winsorized[[
    outcome_std
  ]])
}

# Salva
saveRDS(df_winsorized, "results/df_analysis_winsorized.rds")

cat("\nDataset winsorizzato (AGGRESSIVO) salvato:\n")
cat("  results/df_analysis_winsorized.rds\n")


foo <- readRDS("results/df_analysis_winsorized.rds")
hist(foo$f2_mean_a)
