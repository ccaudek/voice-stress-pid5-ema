# =============================================================================
# DIAGNOSTICA OUTLIERS F2
# =============================================================================

library(tidyverse)
library(patchwork)

df_analysis <- readRDS("results/df_analysis.rds")

cat("=== DIAGNOSTICA OUTLIERS F2 ===\n\n")

# =============================================================================
# 1. IDENTIFICA OUTLIERS CON METODO IQR
# =============================================================================

identify_outliers <- function(data, outcome, multiplier = 3) {
  # Usa 3*IQR invece del classico 1.5*IQR per essere più conservativi
  q1 <- quantile(data[[outcome]], 0.25, na.rm = TRUE)
  q3 <- quantile(data[[outcome]], 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  lower_fence <- q1 - multiplier * iqr
  upper_fence <- q3 + multiplier * iqr
  
  data %>%
    mutate(
      is_outlier = .data[[outcome]] < lower_fence | .data[[outcome]] > upper_fence,
      outlier_type = case_when(
        .data[[outcome]] < lower_fence ~ "low",
        .data[[outcome]] > upper_fence ~ "high",
        TRUE ~ "normal"
      )
    )
}

# Analizza per ogni vocale
outlier_summary <- list()

for (vowel in c("a", "i", "u")) {
  
  outcome <- paste0("f2_mean_", vowel)
  
  cat("\n=== VOCALE /", vowel, "/ ===\n", sep = "")
  
  df_vowel <- identify_outliers(df_analysis, outcome, multiplier = 3)
  
  # Statistiche
  stats <- df_vowel %>%
    summarise(
      n_total = sum(!is.na(.data[[outcome]])),
      n_outliers = sum(is_outlier, na.rm = TRUE),
      pct_outliers = round(100 * n_outliers / n_total, 1),
      n_low = sum(outlier_type == "low", na.rm = TRUE),
      n_high = sum(outlier_type == "high", na.rm = TRUE),
      median = median(.data[[outcome]], na.rm = TRUE),
      iqr = IQR(.data[[outcome]], na.rm = TRUE),
      lower_fence = quantile(.data[[outcome]], 0.25, na.rm = TRUE) - 3 * iqr,
      upper_fence = quantile(.data[[outcome]], 0.75, na.rm = TRUE) + 3 * iqr
    )
  
  print(stats)
  
  # Identifica partecipanti con outliers
  outlier_ids <- df_vowel %>%
    filter(is_outlier) %>%
    select(ID, timepoint, value = all_of(outcome), outlier_type) %>%
    arrange(desc(abs(value - stats$median)))
  
  if (nrow(outlier_ids) > 0) {
    cat("\nOutliers identificati:\n")
    print(head(outlier_ids, 10))
  }
  
  outlier_summary[[vowel]] <- list(
    stats = stats,
    outliers = outlier_ids,
    data = df_vowel
  )
}

# =============================================================================
# 2. VISUALIZZAZIONE OUTLIERS
# =============================================================================

cat("\n=== CREAZIONE GRAFICI ===\n")

# Boxplot con outliers evidenziati
p_boxplot <- df_analysis %>%
  select(starts_with("f2_mean_")) %>%
  pivot_longer(everything(), names_to = "vowel", values_to = "f2") %>%
  mutate(vowel = str_extract(vowel, "[aiu]$")) %>%
  ggplot(aes(x = vowel, y = f2, fill = vowel)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2, outlier.alpha = 0.5) +
  labs(
    title = "F2 Mean per Vocale - Boxplot con Outliers",
    subtitle = "Outliers oltre 3*IQR evidenziati in rosso",
    x = "Vocale",
    y = "F2 (Hz)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/f2_outliers_boxplot.pdf", p_boxplot, width = 10, height = 6)

# Scatterplot per vedere pattern temporali
p_temporal <- df_analysis %>%
  select(ID, timepoint, starts_with("f2_mean_")) %>%
  pivot_longer(starts_with("f2_mean_"), names_to = "vowel", values_to = "f2") %>%
  mutate(vowel = str_extract(vowel, "[aiu]$")) %>%
  ggplot(aes(x = timepoint, y = f2, color = vowel, group = ID)) +
  geom_line(alpha = 0.2) +
  geom_point(alpha = 0.3, size = 0.5) +
  facet_wrap(~vowel, scales = "free_y") +
  labs(
    title = "F2 Mean per Timepoint - Traiettorie Individuali",
    subtitle = "Ogni linea = un partecipante",
    x = "Timepoint",
    y = "F2 (Hz)"
  ) +
  theme_minimal()

ggsave("figures/f2_temporal_trajectories.pdf", p_temporal, width = 12, height = 6)

# Histogram con soglie outlier
plots_hist <- list()
for (vowel in c("a", "i", "u")) {
  
  outcome <- paste0("f2_mean_", vowel)
  stats <- outlier_summary[[vowel]]$stats
  
  p <- df_analysis %>%
    ggplot(aes(x = .data[[outcome]])) +
    geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
    geom_vline(xintercept = stats$lower_fence, color = "red", linetype = "dashed") +
    geom_vline(xintercept = stats$upper_fence, color = "red", linetype = "dashed") +
    annotate("text", x = stats$lower_fence, y = Inf, 
             label = paste0("Lower fence\n", round(stats$lower_fence, 0)), 
             vjust = 1.5, hjust = -0.1, color = "red", size = 3) +
    annotate("text", x = stats$upper_fence, y = Inf, 
             label = paste0("Upper fence\n", round(stats$upper_fence, 0)), 
             vjust = 1.5, hjust = 1.1, color = "red", size = 3) +
    labs(
      title = paste0("F2 Mean - Vocale /", vowel, "/"),
      subtitle = paste0(stats$pct_outliers, "% outliers (", stats$n_outliers, "/", stats$n_total, ")"),
      x = "F2 (Hz)",
      y = "Count"
    ) +
    theme_minimal()
  
  plots_hist[[vowel]] <- p
}

p_combined <- plots_hist$a / plots_hist$i / plots_hist$u

ggsave("figures/f2_outliers_histograms.pdf", p_combined, width = 10, height = 12)

# =============================================================================
# 3. ANALISI PARTECIPANTI CON OUTLIERS
# =============================================================================

cat("\n=== ANALISI PARTECIPANTI ===\n")

# Identifica partecipanti con outliers multipli
participants_with_outliers <- bind_rows(
  outlier_summary$a$data %>% 
    filter(is_outlier) %>% 
    select(ID, timepoint, f2_a = f2_mean_a),
  outlier_summary$i$data %>% 
    filter(is_outlier) %>% 
    select(ID, timepoint, f2_i = f2_mean_i),
  outlier_summary$u$data %>% 
    filter(is_outlier) %>% 
    select(ID, timepoint, f2_u = f2_mean_u)
) %>%
  group_by(ID) %>%
  summarise(
    n_outliers = n(),
    vowels_affected = paste(unique(c(
      if (!all(is.na(f2_a))) "a" else NULL,
      if (!all(is.na(f2_i))) "i" else NULL,
      if (!all(is.na(f2_u))) "u" else NULL
    )), collapse = ","),
    .groups = "drop"
  ) %>%
  arrange(desc(n_outliers))

cat("\nPartecipanti con multiple osservazioni outlier:\n")
print(participants_with_outliers)

# Conta quanti partecipanti hanno almeno un outlier
n_participants_with_outliers <- n_distinct(participants_with_outliers$ID)
n_total_participants <- n_distinct(df_analysis$ID)
pct_participants <- round(100 * n_participants_with_outliers / n_total_participants, 1)

cat("\n", n_participants_with_outliers, " partecipanti (", 
    pct_participants, "%) hanno almeno un outlier\n", sep = "")

# =============================================================================
# 4. SALVA RISULTATI
# =============================================================================

# Summary table
outlier_table <- bind_rows(
  outlier_summary$a$stats %>% mutate(vowel = "a"),
  outlier_summary$i$stats %>% mutate(vowel = "i"),
  outlier_summary$u$stats %>% mutate(vowel = "u")
) %>%
  select(vowel, n_total, n_outliers, pct_outliers, n_low, n_high,
         median, iqr, lower_fence, upper_fence)

write_csv(outlier_table, "results/f2_outlier_summary.csv")

# Lista completa outliers
all_outliers <- bind_rows(
  outlier_summary$a$outliers %>% mutate(vowel = "a"),
  outlier_summary$i$outliers %>% mutate(vowel = "i"),
  outlier_summary$u$outliers %>% mutate(vowel = "u")
)

write_csv(all_outliers, "results/f2_outliers_identified.csv")

cat("\n=== FILE SALVATI ===\n")
cat("- results/f2_outlier_summary.csv\n")
cat("- results/f2_outliers_identified.csv\n")
cat("- figures/f2_outliers_boxplot.pdf\n")
cat("- figures/f2_temporal_trajectories.pdf\n")
cat("- figures/f2_outliers_histograms.pdf\n")

cat("\n=== RACCOMANDAZIONI ===\n\n")

total_pct <- mean(c(
  outlier_summary$a$stats$pct_outliers,
  outlier_summary$i$stats$pct_outliers,
  outlier_summary$u$stats$pct_outliers
))

if (total_pct > 5) {
  cat("⚠ ATTENZIONE: >5% di outliers trovati!\n")
  cat("Raccomandazioni:\n")
  cat("1. Verifica manualmente outliers (possibili errori tracking F2)\n")
  cat("2. Considera winsorizzazione al 95° percentile\n")
  cat("3. Oppure usa modelli robusti (Student-t family)\n")
  cat("\nEsegui: source('script_winsorizzazione.R')\n")
} else if (total_pct > 2) {
  cat("ℹ INFO: 2-5% di outliers trovati (ragionevole)\n")
  cat("Opzioni:\n")
  cat("1. Procedi con analisi (prior informativi gestiranno outliers)\n")
  cat("2. Oppure applica winsorizzazione conservativa\n")
  cat("\nPuoi procedere o eseguire: source('script_winsorizzazione.R')\n")
} else {
  cat("✓ OK: <2% di outliers (basso)\n")
  cat("Puoi procedere con analisi senza winsorizzazione\n")
}
