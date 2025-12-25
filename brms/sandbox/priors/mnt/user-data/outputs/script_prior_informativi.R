# =============================================================================
# STEP 1: ESTRAI STATISTICHE DESCRITTIVE PER PRIOR INFORMATIVI
# =============================================================================

library(tidyverse)

df_analysis <- readRDS("results/df_analysis.rds")

cat("=== STATISTICHE DESCRITTIVE F2 PER VOCALE ===\n\n")

# Statistiche per ogni vocale
stats_f2 <- df_analysis %>%
  summarise(
    # F2 mean per /a/
    f2_mean_a_M = mean(f2_mean_a, na.rm = TRUE),
    f2_mean_a_SD = sd(f2_mean_a, na.rm = TRUE),
    f2_std_a_M = mean(f2_std_a, na.rm = TRUE),
    f2_std_a_SD = sd(f2_std_a, na.rm = TRUE),
    # F2 mean per /i/
    f2_mean_i_M = mean(f2_mean_i, na.rm = TRUE),
    f2_mean_i_SD = sd(f2_mean_i, na.rm = TRUE),
    f2_std_i_M = mean(f2_std_i, na.rm = TRUE),
    f2_std_i_SD = sd(f2_std_i, na.rm = TRUE),
    # F2 mean per /u/
    f2_mean_u_M = mean(f2_mean_u, na.rm = TRUE),
    f2_mean_u_SD = sd(f2_mean_u, na.rm = TRUE),
    f2_std_u_M = mean(f2_std_u, na.rm = TRUE),
    f2_std_u_SD = sd(f2_std_u, na.rm = TRUE)
  )

print(stats_f2)

# Visualizza distribuzioni
p1 <- df_analysis %>%
  select(starts_with("f2_mean")) %>%
  pivot_longer(everything(), names_to = "vowel", values_to = "f2") %>%
  mutate(vowel = str_extract(vowel, "[aiu]$")) %>%
  ggplot(aes(x = f2, fill = vowel)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribuzione F2 mean per vocale",
       x = "F2 (Hz)", y = "Density") +
  theme_minimal()

print(p1)
ggsave("figures/f2_distributions_by_vowel.pdf", width = 10, height = 6)

# Range effetti plausibili
cat("\n=== RANGE EFFETTI PLAUSIBILI ===\n")
cat("Per stress/personality effects su F2:\n")
cat("- Small effect: ~20-50 Hz (< 5% della media)\n")
cat("- Medium effect: ~50-100 Hz (5-10% della media)\n")
cat("- Large effect: ~100-200 Hz (10-15% della media)\n\n")

# Calcola prior informativi
cat("=== PRIOR INFORMATIVI SUGGERITI ===\n\n")

# Per f2_mean_a
f2_mean_a_mean <- stats_f2$f2_mean_a_M
f2_mean_a_sd <- stats_f2$f2_mean_a_SD

cat("Per f2_mean vowel /a/:\n")
cat("Intercept: normal(", round(f2_mean_a_mean, 0), ", ", 
    round(f2_mean_a_sd * 2, 0), ")\n", sep="")
cat("  Rationale: Centrato sulla media osservata, SD permette +/-2SD dalla media\n\n")

cat("Coefficienti (b): normal(0, 100)\n")
cat("  Rationale: Effetti da -200 a +200 Hz (range plausibile per moderazione)\n\n")

cat("Intercept sigma: normal(", round(log(f2_mean_a_sd), 1), ", 0.5)\n", sep="")
cat("  Rationale: log(", round(f2_mean_a_sd, 0), ") = ", 
    round(log(f2_mean_a_sd), 1), " sulla scala log\n", sep="")
cat("  Questo permette SD da ~", round(exp(log(f2_mean_a_sd) - 1), 0), 
    " a ~", round(exp(log(f2_mean_a_sd) + 1), 0), " Hz\n\n", sep="")

cat("Coefficienti sigma (b, dpar='sigma'): normal(0, 0.3)\n")
cat("  Rationale: Piccoli cambiamenti in log-SD (±30% variazione in SD)\n\n")

cat("Random effects (sd): exponential(0.01)\n")
cat("  Rationale: Media a 100, permette ampia variabilità tra soggetti\n\n")

# Salva stats
write_csv(stats_f2, "results/f2_descriptive_stats.csv")

cat("\n=== FILE SALVATI ===\n")
cat("- results/f2_descriptive_stats.csv\n")
cat("- figures/f2_distributions_by_vowel.pdf\n")
