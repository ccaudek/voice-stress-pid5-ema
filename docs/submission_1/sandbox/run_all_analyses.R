# run_all_analyses.R
# Master script to reproduce all analyses

scripts <- c(
  "01_clean_after_merge_FINAL.R",
  "02_voice_personality_analysis_FINAL.R",
  "03_moderation_analysis_FINAL.R",
  "05_calculate_within_subject_variability.R",
  "06_moderation_with_variability_VERSION_A.R",
  "09_model_comparison_EMA_vs_Baseline.R",
  "12_stress_main_effects_summary.R",
  "13_generate_manuscript_tables.R",
  "14_generate_manuscript_figures.R"
)

for (script in scripts) {
  cat("\n", rep("=", 70), "\n")
  cat("Running:", script, "\n")
  cat(rep("=", 70), "\n\n")
  source(file.path("scripts", script))
}

cat("\n✓ ALL ANALYSES COMPLETE\n\n")
