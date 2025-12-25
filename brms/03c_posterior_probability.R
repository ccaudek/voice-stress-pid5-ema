suppressPackageStartupMessages({
  library(tidyverse)
  library(brms)
  library(posterior)
  library(stringr)
})

cat(
  "=== PD (probabilità direzionale) per MODERAZIONI PID-5 × Stress/Recovery ===\n\n"
)

# ------------------------------------------------------------
# 0) CONFIG: dove sono i modelli di moderazione?
# ------------------------------------------------------------
# Opzione A (consigliata): metti tutti i modelli di moderazione in models/ con prefisso "mod_"
# es: models/mod_f0_mean_a.rds, mod_nne_a.rds, ...
# Opzione B: se li salvi con altro nome, cambia il pattern.

model_files <- list.files(
  "models",
  pattern = "^mod_.*\\.rds$",
  full.names = TRUE
)

if (length(model_files) == 0) {
  stop(
    "Non trovo modelli di moderazione in models/ con pattern '^mod_.*\\.rds$'.\n",
    "Soluzione: salva i fit di moderazione come mod_*.rds oppure cambia pattern."
  )
}

models <- setNames(
  lapply(model_files, readRDS),
  tools::file_path_sans_ext(basename(model_files))
)

cat("Trovati", length(models), "modelli di moderazione.\n\n")

# ------------------------------------------------------------
# 1) Helper: parsing outcome/vowel dal nome modello
# ------------------------------------------------------------
parse_model <- function(mn) {
  # atteso: "mod_f0_mean_a" oppure "mod_jitter_a" ecc.
  x <- sub("^mod_", "", mn)
  parts <- str_split(x, "_", simplify = TRUE)
  vowel <- parts[ncol(parts)]
  outcome <- paste(parts[1:(ncol(parts) - 1)], collapse = "_")
  list(outcome = outcome, vowel = vowel)
}

is_lognormal_outcome <- function(outcome) {
  outcome %in% c("f0_std", "jitter", "f2_std")
}

# ------------------------------------------------------------
# 2) Helper: estrai draws e calcola probabilità direzionali
# ------------------------------------------------------------
get_draws <- function(fit, par) {
  d <- as_draws_df(fit)
  if (!par %in% names(d)) return(NULL)
  d[[par]]
}

pd_stats <- function(draws) {
  p_gt0 <- mean(draws > 0)
  p_lt0 <- mean(draws < 0)
  tibble(
    p_gt0 = p_gt0,
    p_lt0 = p_lt0,
    pd = pmax(p_gt0, p_lt0),
    direction_most = if_else(p_gt0 >= p_lt0, "gt0", "lt0")
  )
}

# ------------------------------------------------------------
# 3) Direzioni attese (EDITABILE)
# ------------------------------------------------------------
# Qui decidi la direzione "attesa" per le MODERAZIONI:
# Esempio: per f0_mean, se lo stress aumenta F0, "amplificazione" del tratto => coefficiente > 0
#          per recovery, se ci aspetti "recupero" (post-pre negativo su f0_mean) allora un tratto che peggiora il recupero
#          potrebbe rendere l'effetto meno negativo => coefficiente > 0 (dipende da come definisci "effetto atteso").
#
# Per evitare ambiguità, qui NON imposto aspettative su recovery (c2) di default.
# Imposto solo per c1_stress e solo su outcome dove la direzione del main effect è chiara.
#
# Puoi aggiungere righe a piacere.

expected <- tribble(
  ~outcome,
  ~contrast,
  ~expected_dir,
  ~note,
  "f0_mean",
  "c1_stress",
  "gt0",
  "tratto amplifica aumento pitch in PRE",
  "nne",
  "c1_stress",
  "lt0",
  "tratto amplifica riduzione NNE (più tensione/meno rumore) in PRE",
  "f2_mean",
  "c1_stress",
  "lt0",
  "tratto amplifica riduzione F2 (centralizzazione) in PRE",
  "f0_std",
  "c1_stress",
  "lt0",
  "tratto amplifica riduzione variabilità F0 in PRE (lognormal -> ratio<1)",
  "jitter",
  "c1_stress",
  "lt0",
  "tratto amplifica riduzione jitter in PRE (lognormal -> ratio<1)"
)
# NB: queste sono scelte teoriche. Se per jitter ti aspetti aumento sotto stress, cambia a "gt0".

# ------------------------------------------------------------
# 4) Estrai tutte le interazioni (c1/c2) con i 5 tratti
# ------------------------------------------------------------
pid5_traits <- c(
  "pid5_negative_affectivity_c",
  "pid5_detachment_c",
  "pid5_antagonism_c",
  "pid5_disinhibition_c",
  "pid5_psychoticism_c"
)

# Nomi parametri brms per interazioni: "b_c1_stress:pid5_negative_affectivity_c" ecc.
make_par <- function(contrast, trait) paste0("b_", contrast, ":", trait)

extract_all <- function(fit, mn) {
  info <- parse_model(mn)
  out <- info$outcome
  vwl <- info$vowel

  res <- map_dfr(c("c1_stress", "c2_recovery"), function(contrast) {
    map_dfr(pid5_traits, function(trait) {
      par <- make_par(contrast, trait)
      draws <- get_draws(fit, par)
      if (is.null(draws)) return(NULL)

      st <- pd_stats(draws)
      tibble(
        model = mn,
        outcome = out,
        vowel = vwl,
        contrast = contrast,
        trait = trait,
        par = par,
        estimate = mean(draws),
        q05 = unname(quantile(draws, 0.05)),
        q95 = unname(quantile(draws, 0.95))
      ) %>%
        bind_cols(st)
    })
  })

  res
}

raw_tbl <- imap_dfr(models, extract_all)

cat("Interazioni estratte:", nrow(raw_tbl), "\n\n")

# ------------------------------------------------------------
# 5) Trasformazioni “naturali” per lognormal
# ------------------------------------------------------------
raw_tbl <- raw_tbl %>%
  mutate(
    lognormal = is_lognormal_outcome(outcome),
    ratio = if_else(lognormal, exp(estimate), NA_real_),
    ratio_q05 = if_else(lognormal, exp(q05), NA_real_),
    ratio_q95 = if_else(lognormal, exp(q95), NA_real_),
    pct = if_else(lognormal, (exp(estimate) - 1) * 100, NA_real_),
    pct_q05 = if_else(lognormal, (exp(q05) - 1) * 100, NA_real_),
    pct_q95 = if_else(lognormal, (exp(q95) - 1) * 100, NA_real_)
  )

# ------------------------------------------------------------
# 6) Aggiungi “expected direction” e calcola P(dir attesa)
# ------------------------------------------------------------
tbl <- raw_tbl %>%
  left_join(expected, by = c("outcome", "contrast")) %>%
  mutate(
    p_expected = case_when(
      is.na(expected_dir) ~ NA_real_,
      expected_dir == "gt0" ~ p_gt0,
      expected_dir == "lt0" ~ p_lt0
    )
  )

# ------------------------------------------------------------
# 7) STAMPA: i più convincenti (per P(dir attesa) o PD)
# ------------------------------------------------------------
# A) Interazioni stress (c1) ordinate per P(dir attesa)
cat("=== TOP MODERAZIONI: c1_stress (ordinate per P(direzione attesa)) ===\n\n")
stress_tbl <- tbl %>%
  filter(contrast == "c1_stress") %>%
  arrange(desc(p_expected %||% pd))

print(
  stress_tbl %>%
    transmute(
      outcome,
      vowel,
      trait,
      expected_dir,
      p_expected = round(p_expected, 3),
      pd = round(pd, 3),
      est = round(estimate, 3),
      q05 = round(q05, 3),
      q95 = round(q95, 3),
      ratio = if_else(lognormal, round(ratio, 3), NA_real_),
      pct = if_else(lognormal, round(pct, 1), NA_real_)
    ),
  n = 50
)

# B) Recovery (c2): non imposto direzione attesa di default, stampo per PD
cat("\n=== TOP MODERAZIONI: c2_recovery (ordinate per PD) ===\n\n")
rec_tbl <- tbl %>%
  filter(contrast == "c2_recovery") %>%
  arrange(desc(pd))

print(
  rec_tbl %>%
    transmute(
      outcome,
      vowel,
      trait,
      pd = round(pd, 3),
      direction_most,
      est = round(estimate, 3),
      q05 = round(q05, 3),
      q95 = round(q95, 3),
      ratio = if_else(lognormal, round(ratio, 3), NA_real_),
      pct = if_else(lognormal, round(pct, 1), NA_real_)
    ),
  n = 50
)

# ------------------------------------------------------------
# 8) SINTESI ACROSS VOWELS: "≥2 su 3 nella direzione attesa"
# ------------------------------------------------------------
# Funziona quando hai più vocali per lo stesso (outcome, contrast, trait).
# Usa draw-level? Qui facciamo una sintesi pragmatica con i P_expected per vocale.
# Se vuoi la versione draw-level (come hai fatto con F0), serve estrarre i draw e allinearli per ogni gruppo.

cat("\n=== SINTESI ACROSS VOWELS (pragmatica) ===\n")
vowel_summary <- stress_tbl %>%
  filter(!is.na(p_expected)) %>%
  group_by(outcome, trait, expected_dir) %>%
  summarise(
    n_vowels = n(),
    mean_p_expected = mean(p_expected, na.rm = TRUE),
    min_p_expected = min(p_expected, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_p_expected))

print(vowel_summary, n = 50)

cat("\nNOTE:\n")
cat("• p_expected = P(beta nella direzione teorica attesa) per quel modello.\n")
cat("• Per lognormal: beta<0 equivale a ratio<1 (riduzione).\n")
cat(
  "• Per una vera 'posterior combinata' across vocali, la soluzione più corretta è un modello gerarchico unico.\n"
)
