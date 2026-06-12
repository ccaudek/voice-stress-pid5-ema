# ==============================================================================
# 99_collect_manuscript_values.R
# Single source of truth for the values to be transcribed into the manuscript.
#
# Reads the saved outputs in results/, and produces:
#   - results/manuscript_values/manuscript_values.csv  (tidy, one row per value)
#   - results/manuscript_values/manuscript_values.md   (human-readable, grouped)
#
# Design goals:
#   * ROBUST: never errors on a missing file or an unexpected column name;
#     it logs what it found and what it could not, so gaps are visible.
#   * TRACEABLE: every value carries its source file path.
#   * READY TO PASTE: for effect tables it adds a `value_89cri` column formatted
#     as "median [lo, hi], pd .NN"; it also gathers the auto-generated prose
#     snippets (.txt/.md) verbatim.
#
# HOW TO USE:
#   1. Adjust the paths in the `=== PATHS ===` block below to match your results/.
#      (Unknown paths are reported as MISSING; just fix and re-run.)
#   2. source("scripts/.../99_collect_manuscript_values.R")
#   3. Open results/manuscript_values/manuscript_values.md and transcribe into
#      the Word master with tracked changes (checklist R2.11).
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

OUT_DIR <- here("results", "manuscript_values")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# === PATHS ===  (edit here if your results/ layout differs)
# ==============================================================================
# Effect tables: each gets a formatted `value_89cri` column (median [lo,hi], pd).
EFFECT_FILES <- tibble::tribble(
  ~id,
  ~section,
  ~location,
  ~unit,
  ~path,
  "f0_moderation",
  "Main text",
  "Results, F0 moderation (par. 66-67); Table 1",
  "Hz",
  here("results", "F0", "tables", "table1_f0_moderation_89cri.csv"),
  "f0_dir_summary",
  "Main text / SM",
  "Results, F0 moderation (direction summary)",
  "Hz",
  here("results", "F0", "direction_posterior_summary_89cri.csv"),
  "nne_modsummary",
  "Main text",
  "Results, NNE moderation (par. 68); Table 2",
  "dB",
  here("results", "NNE", "tables", "model_summary_nne_moderation.csv"),
  "nne_dir_summary",
  "Main text / SM",
  "Results, NNE moderation (direction summary)",
  "dB",
  here("results", "NNE", "tables", "direction_posterior_summary_89cri.csv"),
  "main_effects_a",
  "Main text",
  "Results, F0/NNE main effects (par. 60-63)",
  "",
  here("results", "stress", "parameter_estimates_summary.csv"),
  "main_effects_b",
  "Main text",
  "Results, F0/NNE main effects (par. 60-63)",
  "",
  here("manuscript", "parameter_estimates_summary.csv"),
  "followup_width",
  "Main text",
  "EMA vs baseline (par. 73-75, 98, abstract); Table 4",
  "Hz",
  here("results", "followup", "precision_improvement_summary_improved.csv"),
  "followup_mod",
  "Main text",
  "EMA vs baseline, moderation comparison",
  "Hz",
  here("results", "followup", "moderation_comparison_improved.csv"),
  "mfcc_percoef",
  "Supplement",
  "MFCC connected speech, per-coefficient",
  "SD",
  here("results", "mfcc", "tables", "mfcc_per_coefficient_effects.csv"),
  "mfcc_maha",
  "Supplement",
  "MFCC omnibus (Mahalanobis, Bayesian)",
  "",
  here("results", "mfcc", "tables", "mfcc_omnibus_mahalanobis.csv"),
  "coupling",
  "Main text / SM",
  "Voice-affect coupling (R1.9)",
  "",
  here("results", "coupling", "voice_affect", "tables", "coupling_summary.csv"),
  "temporal_sigma",
  "Supplement",
  "Within-person temporal: between-subject SD (sigma_b)",
  "Hz",
  here(
    "results",
    "within_person",
    "heterogeneity_analysis",
    "sigma_beta_summary.csv"
  ),
  "prior_sens",
  "Supplement",
  "Prior sensitivity (headline params across priors)",
  "",
  here(
    "results",
    "prior_sensitivity",
    "tables",
    "prior_sensitivity_summary.csv"
  )
)

# Plain tables to embed verbatim (no formatted column; diagnostics/descriptives).
TABLE_FILES <- tibble::tribble(
  ~id,
  ~section,
  ~location,
  ~path,
  "mfcc_wald",
  "Supplement",
  "MFCC omnibus Wald reference (chi^2)",
  here("results", "mfcc", "tables", "mfcc_omnibus_bayes_wald.csv"),
  "followup_loo",
  "Main text",
  "EMA vs baseline, LOO comparison",
  here("results", "followup", "loo_comparison_f0_improved.csv"),
  "manip_negaffect",
  "Main text",
  "Manipulation check, negative affect (contrasts)",
  here(
    "results",
    "manipulation_check",
    "ema_negative_affect_itemwise",
    "tables",
    "reviewer_ready_negative_affect_table.csv"
  ),
  "manip_appraisal",
  "Main text",
  "Manipulation check, appraisal (contrasts)",
  here(
    "results",
    "manipulation_check",
    "ema_appraisal",
    "tables",
    "reviewer_ready_appraisal_table.csv"
  ),
  "temporal_modcomp",
  "Supplement",
  "Within-person temporal: model comparison",
  here(
    "results",
    "within_person",
    "fitted_models",
    "model_comparison.csv"
  ),
  "temporal_ciwidth",
  "Supplement",
  "Within-person temporal: mean CI width by domain",
  here(
    "results",
    "within_person",
    "heterogeneity_analysis",
    "ci_width_summary.csv"
  ),
  "prior_default",
  "Supplement",
  "Prior sensitivity: default-vs-published check",
  here(
    "results",
    "prior_sensitivity",
    "tables",
    "default_vs_published_check.csv"
  ),
  "conv_both",
  "Supplement",
  "Convergence summary F0 + NNE (Table S11/S12)",
  here("results", "NNE", "diagnostics", "convergence_summary_both_models.csv"),
  "conv_diag_f0",
  "Supplement",
  "F0 convergence diagnostics (key params)",
  here(
    "results",
    "NNE",
    "diagnostics",
    "supplementary_table_diagnostics_89cri.csv"
  )
)

# Auto-generated prose snippets to embed verbatim (ready-to-adapt text).
TEXT_FILES <- tibble::tribble(
  ~id,
  ~section,
  ~location,
  ~path,
  "txt_main_eff",
  "Main text",
  "Results, main effects (generated text)",
  here("manuscript", "results_main_effects_complete.md"),
  "txt_prior",
  "Supplement",
  "Prior sensitivity (generated text)",
  here("results", "prior_sensitivity", "prior_sensitivity_text.txt"),
  "txt_coupling",
  "Main text",
  "Voice-affect coupling (generated text)",
  here("results", "coupling", "voice_affect", "coupling_text.txt"),
  "txt_temp_res",
  "Supplement",
  "Within-person temporal (Results text)",
  here(
    "results",
    "within_person",
    "manuscript_materials",
    "results_text.txt"
  ),
  "txt_temp_disc",
  "Supplement",
  "Within-person temporal (Discussion text)",
  here(
    "results",
    "within_person",
    "manuscript_materials",
    "discussion_text.txt"
  )
)

# ==============================================================================
# HELPERS
# ==============================================================================
log_lines <- character(0)
log_msg <- function(...) {
  msg <- paste0(...)
  log_lines <<- c(log_lines, msg)
  cat(msg, "\n")
}

read_safe <- function(path) {
  if (!file.exists(path)) {
    log_msg("  [MISSING] ", path)
    return(NULL)
  }
  out <- tryCatch(
    suppressMessages(readr::read_csv(path, show_col_types = FALSE)),
    error = function(e) {
      log_msg("  [READ ERROR] ", path, " : ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(out)) log_msg("  [ok] ", path, "  (", nrow(out), " rows)")
  out
}

read_text_safe <- function(path) {
  if (!file.exists(path)) {
    log_msg("  [MISSING] ", path)
    return(NULL)
  }
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

# Find the first column name matching any of the regex patterns (case-insensitive)
find_col <- function(df, patterns) {
  nms <- names(df)
  for (p in patterns) {
    hit <- nms[stringr::str_detect(nms, stringr::regex(p, ignore_case = TRUE))]
    if (length(hit) > 0) return(hit[1])
  }
  NA_character_
}

fmt_num <- function(x, d = 2)
  ifelse(is.na(x), "NA", formatC(as.numeric(x), format = "f", digits = d))
fmt_pd <- function(x) {
  if (is.na(x)) return("NA")
  s <- formatC(as.numeric(x), format = "f", digits = 2)
  sub("^0", "", s) # .97 instead of 0.97
}

# Add a ready-to-paste `value_89cri` column to an effect table, autodetecting cols
add_formatted <- function(df, unit = "") {
  med <- find_col(
    df,
    c(
      "^median$",
      "^Median$",
      "_median$",
      "^median_",
      "D_median",
      "r_median",
      "^mean$",
      "^Mean$",
      "^est"
    )
  )
  lo <- find_col(
    df,
    c(
      "ci_low",
      "ci_lower",
      "CrI89_Lower",
      "q5\\.5",
      "q055",
      "q05$",
      "^lo$",
      "_lo$",
      "lower",
      "r_lo",
      "D_lo",
      "q_low"
    )
  )
  hi <- find_col(
    df,
    c(
      "ci_high",
      "ci_upper",
      "CrI89_Upper",
      "q94\\.5",
      "q945",
      "q95$",
      "^hi$",
      "_hi$",
      "upper",
      "r_hi",
      "D_hi",
      "q_high"
    )
  )
  pdc <- find_col(
    df,
    c(
      "^pd$",
      "^PD$",
      "prob_dir",
      "p_direction",
      "^Pr\\(",
      "P_dir",
      "^p_expected$"
    )
  )

  found <- c(median = med, lo = lo, hi = hi, pd = pdc)
  detected <<- c(detected, list(found))

  if (is.na(med)) {
    df$value_89cri <- NA_character_
    return(df)
  }
  u <- if (nzchar(unit)) paste0(" ", unit) else ""
  df$value_89cri <- mapply(
    function(m, l, h, p) {
      base <- paste0(fmt_num(m), u)
      if (!is.na(l) && !is.na(h))
        base <- paste0(base, " [", fmt_num(l), ", ", fmt_num(h), "]")
      if (!is.na(p)) base <- paste0(base, ", pd ", fmt_pd(p))
      base
    },
    m = df[[med]],
    l = if (!is.na(lo)) df[[lo]] else rep(NA_real_, nrow(df)),
    h = if (!is.na(hi)) df[[hi]] else rep(NA_real_, nrow(df)),
    p = if (!is.na(pdc)) df[[pdc]] else rep(NA_real_, nrow(df))
  )
  df
}

# Render a data frame as a GitHub-flavoured markdown table (no extra deps)
df_to_md <- function(df, max_rows = 60) {
  df <- utils::head(df, max_rows)
  df <- dplyr::mutate(
    df,
    dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3))
  )
  cells <- function(v) {
    v <- format(v, trim = TRUE, digits = 4)
    gsub("\\|", "\\\\|", as.character(v))
  }
  body <- apply(
    df,
    1,
    function(r) paste0("| ", paste(cells(r), collapse = " | "), " |")
  )
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |")
  paste(c(header, sep, body), collapse = "\n")
}

# ==============================================================================
# COLLECT
# ==============================================================================
detected <- list()
manifest <- tibble() # section, location, source_file, key, value_89cri, status
md_blocks <- c(
  "# Manuscript values (89% CrI, pd) — single source of truth\n",
  paste0("_Generated ", as.character(Sys.time()), "._\n"),
  "Each value is traceable to its source file. Effect tables include a ready-to-paste `value_89cri` column. Do not reintroduce 'CrI excludes zero' language; report pd as the primary index.\n"
)

cat("\n=== EFFECT TABLES ===\n")
for (i in seq_len(nrow(EFFECT_FILES))) {
  e <- EFFECT_FILES[i, ]
  df <- read_safe(e$path)
  md_blocks <- c(
    md_blocks,
    paste0("\n## [", e$section, "] ", e$location),
    paste0("_Source: `", e$path, "`_\n")
  )
  if (is.null(df)) {
    md_blocks <- c(
      md_blocks,
      "> **MISSING** — fix the path in the config and re-run.\n"
    )
    manifest <- bind_rows(
      manifest,
      tibble(
        section = e$section,
        location = e$location,
        source_file = e$path,
        key = NA,
        value_89cri = NA,
        status = "MISSING"
      )
    )
    next
  }
  df2 <- add_formatted(df, unit = e$unit)
  keycol <- find_col(
    df2,
    c(
      "term",
      "coef",
      "parameter",
      "domain",
      "variable",
      "contrast",
      "Parameter",
      "effect",
      "Domain"
    )
  )
  if (is.na(keycol)) keycol <- names(df2)[1]
  status <- if (all(is.na(df2$value_89cri)))
    "CHECK (no median col detected)" else "OK"
  manifest <- bind_rows(
    manifest,
    tibble(
      section = e$section,
      location = e$location,
      source_file = e$path,
      key = as.character(df2[[keycol]]),
      value_89cri = df2$value_89cri,
      status = status
    )
  )
  md_blocks <- c(md_blocks, df_to_md(df2), "")
}

cat("\n=== PLAIN TABLES ===\n")
for (i in seq_len(nrow(TABLE_FILES))) {
  e <- TABLE_FILES[i, ]
  df <- read_safe(e$path)
  md_blocks <- c(
    md_blocks,
    paste0("\n## [", e$section, "] ", e$location),
    paste0("_Source: `", e$path, "`_\n")
  )
  if (is.null(df)) {
    md_blocks <- c(
      md_blocks,
      "> **MISSING** — fix the path in the config and re-run.\n"
    )
    manifest <- bind_rows(
      manifest,
      tibble(
        section = e$section,
        location = e$location,
        source_file = e$path,
        key = NA,
        value_89cri = NA,
        status = "MISSING"
      )
    )
    next
  }
  md_blocks <- c(md_blocks, df_to_md(df), "")
  manifest <- bind_rows(
    manifest,
    tibble(
      section = e$section,
      location = e$location,
      source_file = e$path,
      key = "(see table)",
      value_89cri = NA,
      status = "OK"
    )
  )
}

cat("\n=== TEXT SNIPPETS ===\n")
for (i in seq_len(nrow(TEXT_FILES))) {
  e <- TEXT_FILES[i, ]
  txt <- read_text_safe(e$path)
  md_blocks <- c(
    md_blocks,
    paste0("\n## [", e$section, "] ", e$location),
    paste0("_Source: `", e$path, "`_\n")
  )
  if (is.null(txt)) {
    md_blocks <- c(
      md_blocks,
      "> **MISSING** — fix the path in the config and re-run.\n"
    )
    manifest <- bind_rows(
      manifest,
      tibble(
        section = e$section,
        location = e$location,
        source_file = e$path,
        key = NA,
        value_89cri = NA,
        status = "MISSING"
      )
    )
    next
  }
  log_msg("  [ok] ", e$path)
  md_blocks <- c(md_blocks, "```", txt, "```", "")
  manifest <- bind_rows(
    manifest,
    tibble(
      section = e$section,
      location = e$location,
      source_file = e$path,
      key = "(text)",
      value_89cri = NA,
      status = "OK"
    )
  )
}

# ==============================================================================
# WRITE OUTPUTS
# ==============================================================================
readr::write_csv(manifest, file.path(OUT_DIR, "manuscript_values.csv"))
writeLines(
  paste(md_blocks, collapse = "\n"),
  file.path(OUT_DIR, "manuscript_values.md")
)

# ==============================================================================
# REPORT
# ==============================================================================
n_missing <- sum(manifest$status == "MISSING")
n_check <- sum(stringr::str_detect(manifest$status, "CHECK"))
cat("\n", strrep("=", 70), "\n", sep = "")
cat("MANIFEST WRITTEN:\n")
cat("  - ", file.path(OUT_DIR, "manuscript_values.csv"), "\n")
cat("  - ", file.path(OUT_DIR, "manuscript_values.md"), "\n")
cat("Files MISSING (fix paths and re-run):", n_missing, "\n")
cat("Effect tables to CHECK (median col not auto-detected):", n_check, "\n")
if (n_missing > 0) {
  cat("\nMissing sources:\n")
  manifest %>%
    filter(status == "MISSING") %>%
    distinct(source_file) %>%
    pull() %>%
    paste0("  - ", .) %>%
    cat(sep = "\n")
  cat("\n")
}
cat(strrep("=", 70), "\n")

# eof ---
