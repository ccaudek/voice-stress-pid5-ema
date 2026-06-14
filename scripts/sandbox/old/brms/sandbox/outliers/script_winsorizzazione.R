# =============================================================================
# WINSORIZZAZIONE F2 OUTLIERS
# =============================================================================

library(tidyverse)

df_analysis <- readRDS("results/df_analysis.rds")

cat("=== WINSORIZZAZIONE F2 ===\n\n")

# =============================================================================
# FUNZIONE WINSORIZZAZIONE
# =============================================================================

winsorize <- function(x, probs = c(0.025, 0.975)) {
  # Calcola limiti
  limits <- quantile(x, probs = probs, na.rm = TRUE)
  
  # Winsorizza
  x_wins <- x
  x_wins[x < limits[1]] <- limits[1]
  x_wins[x > limits[2]] <- limits[2]
  
  # Report
  n_lower <- sum(x < limits[1], na.rm = TRUE)
  n_upper <- sum(x > limits[2], na.rm = TRUE)
  
  list(
    values = x_wins,
    limits = limits,
    n_winsorized_lower = n_lower,
    n_winsorized_upper = n_upper,
    n_winsorized_total = n_lower + n_upper,
    pct_winsorized = round(100 * (n_lower + n_upper) / sum(!is.na(x)), 2)
  )
}

# =============================================================================
# APPLICA WINSORIZZAZIONE
# =============================================================================

# Prepara dataset
df_winsorized <- df_analysis

# Statistics before winsorization
stats_before <- df_analysis %>%
  summarise(
    f2_mean_a_M = mean(f2_mean_a, na.rm = TRUE),
    f2_mean_a_SD = sd(f2_mean_a, na.rm = TRUE),
    f2_mean_i_M = mean(f2_mean_i, na.rm = TRUE),
    f2_mean_i_SD = sd(f2_mean_i, na.rm = TRUE),
    f2_mean_u_M = mean(f2_mean_u, na.rm = TRUE),
    f2_mean_u_SD = sd(f2_mean_u, na.rm = TRUE)
  )

cat("=== STATISTICHE PRIMA DELLA WINSORIZZAZIONE ===\n")
print(stats_before)

# Winsorizza ogni vocale
winsor_results <- list()

for (vowel in c("a", "i", "u")) {
  
  outcome_mean <- paste0("f2_mean_", vowel)
  outcome_std <- paste0("f2_std_", vowel)
  
  cat("\n=== WINSORIZZAZIONE VOCALE /", vowel, "/ ===\n", sep = "")
  
  # Winsorizza F2 mean
  wins_mean <- winsorize(df_winsorized[[outcome_mean]], probs = c(0.025, 0.975))
  df_winsorized[[outcome_mean]] <- wins_mean$values
  
  cat("\nF2 Mean:\n")
  cat("  Limite inferiore:", round(wins_mean$limits[1], 1), "Hz\n")
  cat("  Limite superiore:", round(wins_mean$limits[2], 1), "Hz\n")
  cat("  N winsorizzati:", wins_mean$n_winsorized_total, 
      "(", wins_mean$pct_winsorized, "%)\n")
  cat("    - Troppo bassi:", wins_mean$n_winsorized_lower, "\n")
  cat("    - Troppo alti:", wins_mean$n_winsorized_upper, "\n")
  
  # Winsorizza F2 std
  wins_std <- winsorize(df_winsorized[[outcome_std]], probs = c(0.025, 0.975))
  df_winsorized[[outcome_std]] <- wins_std$values
  
  cat("\nF2 SD:\n")
  cat("  Limite inferiore:", round(wins_std$limits[1], 1), "Hz\n")
  cat("  Limite superiore:", round(wins_std$limits[2], 1), "Hz\n")
  cat("  N winsorizzati:", wins_std$n_winsorized_total, 
      "(", wins_std$pct_winsorized, "%)\n")
  
  winsor_results[[vowel]] <- list(
    mean = wins_mean,
    std = wins_std
  )
}

# Statistics after winsorization
stats_after <- df_winsorized %>%
  summarise(
    f2_mean_a_M = mean(f2_mean_a, na.rm = TRUE),
    f2_mean_a_SD = sd(f2_mean_a, na.rm = TRUE),
    f2_mean_i_M = mean(f2_mean_i, na.rm = TRUE),
    f2_mean_i_SD = sd(f2_mean_i, na.rm = TRUE),
    f2_mean_u_M = mean(f2_mean_u, na.rm = TRUE),
    f2_mean_u_SD = sd(f2_mean_u, na.rm = TRUE)
  )

cat("\n=== STATISTICHE DOPO WINSORIZZAZIONE ===\n")
print(stats_after)

# Confronto
cat("\n=== CAMBIAMENTI ===\n")
comparison <- bind_rows(
  stats_before %>% mutate(when = "before"),
  stats_after %>% mutate(when = "after")
) %>%
  pivot_longer(-when, names_to = "stat", values_to = "value") %>%
  pivot_wider(names_from = when, values_from = value) %>%
  mutate(
    change = after - before,
    pct_change = round(100 * change / before, 2)
  )

print(comparison)

# =============================================================================
# VISUALIZZAZIONE BEFORE/AFTER
# =============================================================================

cat("\n=== CREAZIONE GRAFICI COMPARATIVI ===\n")

# Confronto distribuzioni
for (vowel in c("a", "i", "u")) {
  
  outcome <- paste0("f2_mean_", vowel)
  
  df_compare <- bind_rows(
    df_analysis %>% 
      select(ID, all_of(outcome)) %>%
      mutate(dataset = "Before Winsorization"),
    df_winsorized %>% 
      select(ID, all_of(outcome)) %>%
      mutate(dataset = "After Winsorization")
  )
  
  p <- ggplot(df_compare, aes(x = .data[[outcome]], fill = dataset)) +
    geom_density(alpha = 0.5) +
    geom_vline(data = . %>% group_by(dataset) %>% 
                 summarise(mean = mean(.data[[outcome]], na.rm = TRUE)),
               aes(xintercept = mean, color = dataset), 
               linetype = "dashed", linewidth = 1) +
    labs(
      title = paste0("F2 Mean - Vocale /", vowel, "/"),
      subtitle = "Confronto Before/After Winsorization",
      x = "F2 (Hz)",
      y = "Density",
      fill = "Dataset",
      color = "Mean"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  filename <- paste0("figures/winsorization_comparison_", vowel, ".pdf")
  ggsave(filename, p, width = 10, height = 6)
  
  cat("Salvato:", filename, "\n")
}

# Boxplot comparativo
df_boxplot <- bind_rows(
  df_analysis %>%
    select(starts_with("f2_mean_")) %>%
    pivot_longer(everything(), names_to = "vowel", values_to = "f2") %>%
    mutate(
      vowel = str_extract(vowel, "[aiu]$"),
      dataset = "Before"
    ),
  df_winsorized %>%
    select(starts_with("f2_mean_")) %>%
    pivot_longer(everything(), names_to = "vowel", values_to = "f2") %>%
    mutate(
      vowel = str_extract(vowel, "[aiu]$"),
      dataset = "After"
    )
)

p_boxplot <- ggplot(df_boxplot, aes(x = vowel, y = f2, fill = dataset)) +
  geom_boxplot(position = "dodge") +
  labs(
    title = "F2 Mean per Vocale - Before vs After Winsorization",
    subtitle = "Outliers ridotti mantenendo struttura dati",
    x = "Vocale",
    y = "F2 (Hz)",
    fill = "Dataset"
  ) +
  theme_minimal()

ggsave("figures/winsorization_boxplot_comparison.pdf", p_boxplot, 
       width = 10, height = 6)

# =============================================================================
# SALVA DATASET WINSORIZZATO
# =============================================================================

cat("\n=== SALVATAGGIO DATASET ===\n")

# Salva dataset winsorizzato
saveRDS(df_winsorized, "results/df_analysis_winsorized.rds")
write_csv(df_winsorized, "results/df_analysis_winsorized.csv")

# Salva summary winsorizzazione
winsor_summary <- bind_rows(
  tibble(
    vowel = "a",
    outcome = "f2_mean",
    lower_limit = winsor_results$a$mean$limits[1],
    upper_limit = winsor_results$a$mean$limits[2],
    n_winsorized = winsor_results$a$mean$n_winsorized_total,
    pct_winsorized = winsor_results$a$mean$pct_winsorized
  ),
  tibble(
    vowel = "i",
    outcome = "f2_mean",
    lower_limit = winsor_results$i$mean$limits[1],
    upper_limit = winsor_results$i$mean$limits[2],
    n_winsorized = winsor_results$i$mean$n_winsorized_total,
    pct_winsorized = winsor_results$i$mean$pct_winsorized
  ),
  tibble(
    vowel = "u",
    outcome = "f2_mean",
    lower_limit = winsor_results$u$mean$limits[1],
    upper_limit = winsor_results$u$mean$limits[2],
    n_winsorized = winsor_results$u$mean$n_winsorized_total,
    pct_winsorized = winsor_results$u$mean$pct_winsorized
  ),
  tibble(
    vowel = "a",
    outcome = "f2_std",
    lower_limit = winsor_results$a$std$limits[1],
    upper_limit = winsor_results$a$std$limits[2],
    n_winsorized = winsor_results$a$std$n_winsorized_total,
    pct_winsorized = winsor_results$a$std$pct_winsorized
  ),
  tibble(
    vowel = "i",
    outcome = "f2_std",
    lower_limit = winsor_results$i$std$limits[1],
    upper_limit = winsor_results$i$std$limits[2],
    n_winsorized = winsor_results$i$std$n_winsorized_total,
    pct_winsorized = winsor_results$i$std$pct_winsorized
  ),
  tibble(
    vowel = "u",
    outcome = "f2_std",
    lower_limit = winsor_results$u$std$limits[1],
    upper_limit = winsor_results$u$std$limits[2],
    n_winsorized = winsor_results$u$std$n_winsorized_total,
    pct_winsorized = winsor_results$u$std$pct_winsorized
  )
)

write_csv(winsor_summary, "results/winsorization_summary.csv")

cat("\nFile salvati:\n")
cat("- results/df_analysis_winsorized.rds\n")
cat("- results/df_analysis_winsorized.csv\n")
cat("- results/winsorization_summary.csv\n")
cat("- figures/winsorization_comparison_*.pdf\n")
cat("- figures/winsorization_boxplot_comparison.pdf\n")

# =============================================================================
# RACCOMANDAZIONI FINALI
# =============================================================================

cat("\n=== PROSSIMI PASSI ===\n\n")

total_wins_pct <- mean(c(
  winsor_results$a$mean$pct_winsorized,
  winsor_results$i$mean$pct_winsorized,
  winsor_results$u$mean$pct_winsorized
))

cat("In media,", round(total_wins_pct, 1), "% dei dati sono stati winsorizzati.\n\n")

if (total_wins_pct < 3) {
  cat("✓ Winsorizzazione conservativa applicata\n")
  cat("→ Procedi con analisi usando: df_analysis_winsorized.rds\n")
} else if (total_wins_pct < 5) {
  cat("ℹ Winsorizzazione moderata applicata\n")
  cat("→ Confronta risultati con/senza winsorizzazione\n")
  cat("→ Usa df_analysis_winsorized.rds per analisi principale\n")
} else {
  cat("⚠ Winsorizzazione sostanziale (>5%)\n")
  cat("→ Verifica manualmente outliers originali\n")
  cat("→ Considera anche modelli robusti (Student-t)\n")
  cat("→ Riporta nel Methods l'uso di winsorizzazione\n")
}

cat("\n=== COME USARE IL DATASET WINSORIZZATO ===\n\n")
cat("Negli script di analisi, sostituisci:\n")
cat("  df_analysis <- readRDS('results/df_analysis.rds')\n")
cat("con:\n")
cat("  df_analysis <- readRDS('results/df_analysis_winsorized.rds')\n\n")

cat("Nel Methods del manoscritto, aggiungi:\n")
cat("'F2 values were winsorized at the 2.5th and 97.5th percentiles\n")
cat("to reduce the influence of extreme outliers while maintaining\n")
cat("sample size (", round(total_wins_pct, 1), "% of observations affected).\n")
cat("This conservative approach preserves data structure while\n")
cat("limiting leverage of potentially erroneous formant tracking.'\n")
