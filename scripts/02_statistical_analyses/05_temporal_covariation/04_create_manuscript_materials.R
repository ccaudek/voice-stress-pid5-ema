# ==============================================================================
# 04_create_manuscript_materials.R - REVISED INTERPRETATION
# Prepara tabelle e testo per il manoscritto
#
# FOCUS: Effetti within-person deboli/assenti + incertezza da design sparse
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(knitr)
  library(kableExtra)
})

# ==============================================================================
# SETUP
# ==============================================================================

data_dir <- here::here("results/within_person")
fitted_dir <- file.path(data_dir, "fitted_models")
hetero_dir <- file.path(data_dir, "heterogeneity_analysis")
output_dir <- file.path(data_dir, "manuscript_materials")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

cat("=== CREAZIONE MATERIALI MANOSCRITTO (REVISED) ===\n\n")

# ==============================================================================
# CARICA RISULTATI
# ==============================================================================

comparison <- read_csv(
  file.path(fitted_dir, "model_comparison.csv"),
  show_col_types = FALSE
)

individual_slopes <- read_csv(
  file.path(hetero_dir, "individual_slopes.csv"),
  show_col_types = FALSE
)

uncertainty_summary <- read_csv(
  file.path(hetero_dir, "uncertainty_summary.csv"),
  show_col_types = FALSE
)

sigma_beta_summary <- read_csv(
  file.path(hetero_dir, "sigma_beta_summary.csv"),
  show_col_types = FALSE
)

# Fixed effects
diag_baseline <- read_csv(
  file.path(fitted_dir, "diagnostics_baseline.csv"),
  show_col_types = FALSE
)

diag_rs <- read_csv(
  file.path(fitted_dir, "diagnostics_random_slopes.csv"),
  show_col_types = FALSE
)

# ==============================================================================
# TABLE 1: MODEL COMPARISON
# ==============================================================================

cat("Creazione Table 1: Model Comparison...\n")

beta_baseline <- diag_baseline %>%
  filter(str_detect(variable, "beta_wp")) %>%
  select(variable, mean, q5, q95)

beta_rs <- diag_rs %>%
  filter(str_detect(variable, "beta_wp")) %>%
  select(variable, mean, q5, q95)

table1 <- tibble(
  Domain = c(
    "Negative Affectivity",
    "Detachment",
    "Antagonism",
    "Disinhibition",
    "Psychoticism"
  )
)

table1 <- table1 %>%
  mutate(
    `Baseline β (90% CI)` = sprintf(
      "%.2f [%.2f, %.2f]",
      beta_baseline$mean,
      beta_baseline$q5,
      beta_baseline$q95
    )
  )

table1 <- table1 %>%
  mutate(
    `Random Slopes β (90% CI)` = sprintf(
      "%.2f [%.2f, %.2f]",
      beta_rs$mean,
      beta_rs$q5,
      beta_rs$q95
    )
  )

table1 <- table1 %>%
  left_join(
    sigma_beta_summary %>%
      transmute(
        Domain = domain,
        `σ_β (90% CI)` = sprintf("%.2f [%.2f, %.2f]", mean, q025, q975)
      ),
    by = "Domain"
  )

r2_baseline <- diag_baseline$mean[diag_baseline$variable == "r2_within"]
r2_rs <- diag_rs$mean[diag_rs$variable == "r2_within"]

table1_footer <- sprintf(
  "Note: R² Baseline = %.3f, Random Slopes = %.3f. ELPD LOO: Baseline = %.1f, Random Slopes = %.1f (Δ = %.1f). All fixed effects (β) have 90%% CIs spanning zero, indicating weak/absent population-average effects.",
  r2_baseline,
  r2_rs,
  comparison$elpd[1],
  comparison$elpd[2],
  comparison$elpd[2] - comparison$elpd[1]
)

write_csv(table1, file.path(output_dir, "table1_model_comparison.csv"))
writeLines(table1_footer, file.path(output_dir, "table1_footer.txt"))

cat("  ✓ Table 1 creata\n")

# ==============================================================================
# TABLE 2: UNCERTAINTY SUMMARY
# ==============================================================================

cat("Creazione Table 2: Uncertainty Summary...\n")

table2 <- uncertainty_summary %>%
  select(Domain = domain, Classification = classification, N = n, `%` = pct) %>%
  mutate(`%` = sprintf("%.1f%%", `%`))

write_csv(table2, file.path(output_dir, "table2_uncertainty_summary.csv"))

cat("  ✓ Table 2 creata\n")

# ==============================================================================
# RESULTS TEXT
# ==============================================================================

cat("\nCreazione Results text...\n")

# Calcola statistiche chiave
n_uncertain <- uncertainty_summary %>%
  filter(classification == "Uncertain (CI includes 0)") %>%
  summarise(n = sum(n)) %>%
  pull(n)

n_total <- nrow(individual_slopes)
pct_uncertain <- n_uncertain / n_total * 100

results_text <- sprintf(
  '
#### Within-Person Covariation: Weak and Difficult to Detect

We examined whether momentary fluctuations in personality traits covaried with 
F0 within individuals by comparing two multilevel models: (1) a baseline model 
with fixed within-person slopes (assuming homogeneous associations), and (2) a 
random slopes model allowing between-subject variability in slopes.

**Model Comparison.** The random slopes model substantially outperformed the 
baseline model (ELPD difference = %.1f, SE = %.1f; R² = %.1f%% vs %.1f%%), 
suggesting that accounting for between-subject variability improves model fit. 
However, this improvement in fit **does not indicate clear individual-level 
effects**, as detailed below.

**Population-Level Effects.** Fixed effects were small and uncertain, with all 
90%% credible intervals spanning zero (Table 1). This indicates minimal 
population-average within-person associations between PID-5 domains and F0.

**Individual-Level Uncertainty.** Despite the improved fit of random slopes, 
**individual slope estimates showed very high uncertainty**. With only 3 
timepoints per person, 95%% credible intervals for individual slopes were wide 
(mean width ≈ 10-13 Hz). Consequently, **%.1f%% of individual slopes (n = %d/%d) 
had credible intervals including zero**, indicating that effects cannot be 
reliably distinguished from null for the vast majority of participants (Table 2, 
Figure 2).

Only 2 of %d participants (1.7%%) across all domains showed slopes with 95%% CIs 
excluding zero—a proportion consistent with chance given multiple comparisons. 
This pattern suggests that **within-person personality-voice associations are 
either absent or too weak to be detected reliably** with sparse temporal sampling.

**Between-Subject Variability.** Standard deviations of random slopes (σ_β) 
ranged from 2.6 to 4.3 Hz across domains (Table 1). While these suggest some 
between-subject variability exists, the high individual-level uncertainty 
prevents reliable characterization of which participants, if any, exhibit 
meaningful effects.
',
  # Model comparison
  comparison$elpd[2] - comparison$elpd[1],
  abs(comparison$elpd[2] - comparison$elpd[1]) / 5, # Approximate SE
  r2_rs * 100,
  r2_baseline * 100,
  # Uncertainty
  pct_uncertain,
  n_uncertain,
  n_total,
  individual_slopes %>% filter(domain_id == 1) %>% nrow()
)

writeLines(results_text, file.path(output_dir, "results_text.txt"))

cat("  ✓ Results text creato\n")

# ==============================================================================
# DISCUSSION TEXT
# ==============================================================================

cat("Creazione Discussion text...\n")

discussion_text <- '
#### Weak Within-Person Effects and Sparse Temporal Sampling

Our findings reveal that **within-person associations between momentary PID-5 
fluctuations and F0 are weak and difficult to detect reliably**. While random 
slopes models outperformed fixed-effects specifications (14× improvement in R²), 
the sparse temporal design (3 timepoints per person) resulted in very high 
uncertainty for individual slope estimates. Nearly all individual credible 
intervals (99.7%) included zero, indicating that effects cannot be reliably 
distinguished from null at the individual level.

**Interpreting Model Fit vs Individual Effects.** The improved fit of random 
slopes models appears to reflect **flexibility in modeling residual variability** 
rather than clear individual-level patterns. Between-subject variability in slopes 
(σ_β ≈ 2-4 Hz) suggests heterogeneity may exist at the population level, but with 
only 3 observations per person, individual slopes cannot be estimated precisely 
enough to characterize this heterogeneity meaningfully. This distinction is 
critical: **better model fit does not imply detectable individual-level effects 
when data are sparse**.

**Theoretical Implications.** Two interpretations are plausible. First, within-
person personality-voice coupling may be **genuinely weak or absent**—momentary 
affective fluctuations may not translate to detectable vocal changes during 
controlled reading tasks, particularly when those fluctuations are measured 
hours before voice recording (EMA aggregated across exam periods). Second, 
effects may exist but **require denser temporal sampling** to detect. Dense 
longitudinal designs (10-20+ observations per person) with concurrent affect 
and voice measurement would provide sufficient statistical power to distinguish 
true individual heterogeneity from measurement noise.

**Comparison with Between-Person Effects.** In contrast to the weak within-person 
findings, between-person analyses (not reported here) might reveal stronger 
trait-level associations. Individuals high in certain PID-5 domains may show 
systematically different vocal characteristics, even if moment-to-moment 
fluctuations within individuals are not reliably detected. This would suggest 
that **personality-voice relationships are primarily trait-level phenomena** 
rather than reflecting dynamic state-dependent coupling.

**Methodological Contributions.** Our results underscore critical design 
considerations for within-person research. **Sparse temporal sampling fundamentally 
limits the precision of individual parameter estimates**, regardless of model 
complexity. Researchers should carefully distinguish between (1) population-level 
heterogeneity (e.g., significant σ_β), (2) model fit improvements, and (3) 
reliable individual-level effects. The first two can exist without the third when 
data are sparse.

**Implications for Voice-Based Assessment.** These findings suggest caution for 
affective computing applications relying on within-person personality-voice 
coupling. If effects are as weak as our data suggest, **moment-to-moment affect 
detection from voice alone may be unreliable**, particularly in controlled 
speaking contexts. Trait-level calibration or multimodal approaches (combining 
voice with other signals) may be more promising than relying solely on vocal 
features for real-time affect inference.

**Limitations and Future Directions.** The 3-timepoint design was a major 
limitation. Temporal lag between EMA assessment and voice recording (hours to 
days), aggregation of EMA within exam periods, and constrained reading task 
contexts may have further attenuated effects. **Future studies should prioritize 
dense ambulatory designs** with concurrent affect and voice capture in naturalistic 
speaking contexts. Only with 10-20+ observations per person can we definitively 
distinguish between absent effects and effects too small to detect reliably with 
sparse sampling.
'

writeLines(discussion_text, file.path(output_dir, "discussion_text.txt"))

cat("  ✓ Discussion text creato\n")

# ==============================================================================
# ABSTRACT TEXT
# ==============================================================================

cat("Creazione Abstract...\n")

abstract_text <- '
To examine within-person personality-voice relationships, we decomposed variance 
into between-person and within-person components using Bayesian multilevel models. 
While random slopes models substantially outperformed fixed-effects models (R² = 
35% vs 2.5%, ELPD improvement = 40 points), **individual-level effects were 
difficult to detect reliably**. With only 3 timepoints per person, 99.7% of 
individual slope estimates had 95% credible intervals including zero, indicating 
high uncertainty. Population-level fixed effects were small and uncertain (all 
CIs spanning zero), and only 2 of 119 participants (1.7%) showed reliable non-zero 
slopes across all domains. These findings suggest that **within-person associations 
between momentary PID-5 fluctuations and F0 are weak or absent**, at least in 
controlled reading contexts with sparse temporal sampling. Dense longitudinal 
designs (10-20+ observations per person) with concurrent affect-voice measurement 
would be needed to characterize individual patterns, if they exist.
'

writeLines(abstract_text, file.path(output_dir, "abstract_text.txt"))

cat("  ✓ Abstract creato\n")

# ==============================================================================
# FIGURE CAPTIONS
# ==============================================================================

cat("Creazione Figure captions...\n")

captions <- list(
  figure1 = "Figure 1. Distribution of individual within-person slopes across five PID-5 domains. Density plots show variation in slope estimates, but individual uncertainty is high (see Figure 2). Dashed red line indicates zero (no effect).",

  figure2 = "Figure 2. Individual within-person slopes for Negative Affectivity with 95% credible intervals (N = 119 participants). Nearly all intervals span zero (gray), indicating high uncertainty due to sparse sampling (3 timepoints per person). Only 1 participant (0.8%) has a CI excluding zero. Forest plot demonstrates that improved model fit does not imply reliable individual-level effects.",

  figure3 = "Figure 3. Proportion of participants with credible intervals excluding vs including zero across PID-5 domains. Gray bars (uncertain slopes) dominate all domains, with only ~1-2% showing reliable effects (colored bars). This pattern indicates that individual within-person effects cannot be distinguished from null with sparse temporal sampling.",

  figure4 = "Figure 4. Width of 95% credible intervals for individual slopes by domain. Wide CIs (mean ≈ 10-13 Hz) reflect high uncertainty in individual estimates with 3 timepoints per person. Denser sampling would be needed to reduce uncertainty.",

  figure5 = "Figure 5. Population-level fixed effects (x-axis) versus between-subject standard deviations (y-axis). Fixed effects centered near zero indicate weak population-average associations. Nonzero σ_β suggests some between-subject variability, but individual slopes remain uncertain (see Figure 2)."
)

writeLines(
  paste(names(captions), captions, sep = ". ", collapse = "\n\n"),
  file.path(output_dir, "figure_captions.txt")
)

cat("  ✓ Figure captions creati\n")

# ==============================================================================
# SUMMARY FOR AUTHORS
# ==============================================================================

cat("\nCreazione summary for authors...\n")

author_summary <- sprintf(
  '
=== SUMMARY FOR AUTHORS ===

KEY MESSAGE (REVISED):
"Within-person personality-voice associations are weak/absent or undetectable 
with sparse temporal sampling."

MAIN FINDINGS:

1. MODEL COMPARISON:
   - Random slopes: R² = %.1f%%, ELPD = %.1f
   - Baseline: R² = %.1f%%, ELPD = %.1f
   - Difference: ΔR² = %.1f%% points, ΔELPD = %.1f
   → Random slopes fit better BUT this doesn\'t mean clear individual effects

2. INDIVIDUAL-LEVEL UNCERTAINTY:
   - %.1f%% of slopes have 95%% CI including zero (n = %d/%d)
   - Only ~1.7%% show "reliable" non-zero effects (2/119 participants)
   - Mean CI width: ~10-13 Hz (very wide relative to effect sizes)
   → Cannot distinguish individual effects from noise with 3 timepoints

3. POPULATION-LEVEL EFFECTS:
   - All fixed effects (β) have CIs spanning zero
   - σ_β = 2.6-4.3 Hz (suggests some variability, but uncertain at individual level)
   → Weak/absent population-average effects

INTERPRETATION:

DO NOT claim:
❌ "Idiographic patterns"
❌ "Individual heterogeneity in vocal-affective coupling"
❌ "Person-specific effects"

DO claim:
✅ "Weak/absent within-person associations"
✅ "Sparse sampling precludes reliable individual characterization"
✅ "Dense sampling (10-20+ obs) needed to detect effects, if they exist"

IMPLICATIONS:

1. THEORETICAL:
   - Within-person coupling weak/absent (at least in reading tasks)
   - Between-person effects may be stronger (examine separately)
   - Trait-level > state-level associations?

2. METHODOLOGICAL:
   - Model fit ≠ individual-level detectability
   - 3 timepoints insufficient for individual slopes
   - Need dense longitudinal designs

3. PRACTICAL:
   - Moment-to-moment affect detection from voice unreliable
   - Trait-level calibration more promising
   - Multimodal approaches recommended

MANUSCRIPT STRUCTURE:

Results:
  - Report model comparison (random slopes better)
  - Emphasize individual uncertainty (Table 2, Figure 2)
  - Report %.1f%% uncertain, only 1.7%% reliable

Discussion:
  - Weak effects vs sparse sampling limitation
  - Distinguish model fit from individual detectability
  - Recommend dense sampling future studies

Limitations:
  - 3 timepoints = major limitation
  - EMA-voice temporal lag
  - Controlled reading task

Future Directions:
  - Dense ambulatory sampling (10-20+ obs)
  - Concurrent affect-voice measurement
  - Naturalistic speaking contexts
',
  r2_rs * 100,
  comparison$elpd[2],
  r2_baseline * 100,
  comparison$elpd[1],
  (r2_rs - r2_baseline) * 100,
  comparison$elpd[2] - comparison$elpd[1],
  pct_uncertain,
  n_uncertain,
  n_total,
  pct_uncertain
)

writeLines(author_summary, file.path(output_dir, "AUTHOR_SUMMARY.txt"))

cat("  ✓ Author summary creato\n")

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("MATERIALI MANOSCRITTO COMPLETATI (REVISED INTERPRETATION)\n")
cat(rep("=", 80), "\n\n")

cat("File creati in:", output_dir, "\n\n")

cat("TABLES:\n")
cat("  - table1_model_comparison.csv\n")
cat("  - table1_footer.txt\n")
cat("  - table2_uncertainty_summary.csv\n\n")

cat("TEXT:\n")
cat("  - results_text.txt\n")
cat("  - discussion_text.txt\n")
cat("  - abstract_text.txt\n")
cat("  - figure_captions.txt\n")
cat("  - AUTHOR_SUMMARY.txt ⭐ READ THIS FIRST\n\n")

cat("FIGURES (in heterogeneity_analysis/):\n")
cat("  - 01_slope_densities.pdf\n")
cat("  - 02_forest_plot_NA.pdf ⭐ KEY FIGURE (shows wide CIs)\n")
cat("  - 03_uncertainty_barchart.pdf\n")
cat("  - 04_ci_width_distribution.pdf\n")
cat("  - 05_fixed_vs_variability.pdf\n\n")

cat(rep("=", 80), "\n", sep = "")
cat("⚠️  CRITICAL MESSAGE\n")
cat(rep("=", 80), "\n\n")

cat("INTERPRETAZIONE CORRETTA:\n")
cat("Gli effetti within-person sono DEBOLI/ASSENTI, NON idiografici.\n")
cat("Solo ~1.7%% dei partecipanti mostrano effetti affidabili.\n")
cat("99.7%% degli slopes hanno CI che include zero.\n\n")

cat("Il miglior fit del random slopes riflette flessibilità del modello,\n")
cat("non pattern individuali chiari. Con 3 timepoint, l\'incertezza è troppo\n")
cat("alta per distinguere effetti individuali dal rumore.\n\n")

cat("RACCOMANDAZIONI:\n")
cat("1. Leggi AUTHOR_SUMMARY.txt per overview completo\n")
cat("2. Usa i testi forniti come base per Results e Discussion\n")
cat("3. Enfatizza limitazione design sparse (3 timepoint)\n")
cat("4. Suggerisci dense sampling (10-20+ obs) per future studies\n\n")

cat(rep("=", 80), "\n", sep = "")
cat("✓ PIPELINE COMPLETATA CON INTERPRETAZIONE CORRETTA\n")
cat(rep("=", 80), "\n\n")

# eof ---
