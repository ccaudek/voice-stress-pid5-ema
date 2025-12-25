# =============================================================================
# 01 - DIAGNOSTICA OUTLIERS F2
# =============================================================================

library(tidyverse)

cat("Caricamento dati...\n")
df_analysis <- readRDS("results/df_analysis.rds")

# Funzione identificazione outliers
identify_outliers <- function(x, multiplier = 3) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - multiplier * iqr
  upper <- q3 + multiplier * iqr
  
  list(
    is_outlier = x < lower | x > upper,
    lower_fence = lower,
    upper_fence = upper,
    n_low = sum(x < lower, na.rm = TRUE),
    n_high = sum(x > upper, na.rm = TRUE)
  )
}

# Analizza ogni vocale
outlier_summary <- tibble()

for (vowel in c("a", "i", "u")) {
  outcome <- paste0("f2_mean_", vowel)
  
  outliers <- identify_outliers(df_analysis[[outcome]])
  n_total <- sum(!is.na(df_analysis[[outcome]]))
  n_outliers <- outliers$n_low + outliers$n_high
  
  outlier_summary <- bind_rows(
    outlier_summary,
    tibble(
      vowel = vowel,
      outcome = "f2_mean",
      n_total = n_total,
      n_outliers = n_outliers,
      pct_outliers = round(100 * n_outliers / n_total, 2),
      n_low = outliers$n_low,
      n_high = outliers$n_high,
      lower_fence = round(outliers$lower_fence, 1),
      upper_fence = round(outliers$upper_fence, 1)
    )
  )
  
  cat("Vocale /", vowel, "/: ", n_outliers, " outliers (", 
      round(100 * n_outliers / n_total, 1), "%)\n", sep = "")
}

# Salva summary
write_csv(outlier_summary, "results/f2_outlier_summary.csv")

# Plot diagnostico
p_boxplot <- df_analysis %>%
  select(starts_with("f2_mean_")) %>%
  pivot_longer(everything(), names_to = "vowel", values_to = "f2") %>%
  mutate(vowel = str_extract(vowel, "[aiu]$")) %>%
  ggplot(aes(x = vowel, y = f2, fill = vowel)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  labs(title = "F2 Mean - Boxplot con Outliers",
       subtitle = "Outliers oltre 3×IQR in rosso",
       x = "Vocale", y = "F2 (Hz)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/01_outliers_boxplot.pdf", p_boxplot, width = 10, height = 6)

cat("\nDiagnostica salvata:\n")
cat("  - results/f2_outlier_summary.csv\n")
cat("  - figures/01_outliers_boxplot.pdf\n")
