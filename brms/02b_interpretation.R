suppressPackageStartupMessages({
  library(tidyverse)
  library(brms)
})

cat("=== LOAD + SUMMARY: tutti i modelli in models/ ===\n\n")

# 1) lista file
files <- list.files("models", pattern = "\\.rds$", full.names = TRUE)
stopifnot(length(files) > 0)

cat("Trovati", length(files), "file .rds in models/\n\n")

# 2) carica tutti i modelli
models <- setNames(
  lapply(files, readRDS),
  nm = tools::file_path_sans_ext(basename(files))
)

# 3) helper diagnostiche + effetti
extract_one <- function(mod, name) {
  # ---- diagnostiche
  rh <- tryCatch(rhat(mod), error = function(e) NA_real_)
  ess <- tryCatch(neff_ratio(mod), error = function(e) NA_real_)
  np <- tryCatch(nuts_params(mod), error = function(e) NULL)

  n_div <- NA_integer_
  n_td <- NA_integer_
  if (!is.null(np)) {
    n_div <- sum(subset(np, Parameter == "divergent__")$Value)
    n_td <- sum(subset(np, Parameter == "treedepth__")$Value >= 10)
  }

  # ---- fixed effects (timepointpre/post)
  fe <- tryCatch(fixef(mod), error = function(e) NULL)

  get_fe <- function(par) {
    if (is.null(fe) || !(par %in% rownames(fe))) return(c(NA, NA, NA))
    c(fe[par, "Estimate"], fe[par, "Q2.5"], fe[par, "Q97.5"])
  }

  pre <- get_fe("timepointpre")
  post <- get_fe("timepointpost")

  # ---- contrasto POST - PRE (su scala lineare del predittore)
  # Nota: per lognormal questo è su log-scale (differenza nei log), quindi interpretabile come ratio via exp().
  contr <- tryCatch(
    {
      h <- hypothesis(mod, "timepointpost - timepointpre = 0")
      c(h$hypothesis$Estimate, h$hypothesis$CI.Lower, h$hypothesis$CI.Upper)
    },
    error = function(e) c(NA, NA, NA)
  )

  tibble(
    model = name,
    max_rhat = suppressWarnings(max(rh, na.rm = TRUE)),
    min_ess_ratio = suppressWarnings(min(ess, na.rm = TRUE)),
    n_divergent = n_div,
    n_treedepth_hits = n_td,

    pre_est = pre[1],
    pre_l = pre[2],
    pre_u = pre[3],
    post_est = post[1],
    post_l = post[2],
    post_u = post[3],

    post_minus_pre_est = contr[1],
    post_minus_pre_l = contr[2],
    post_minus_pre_u = contr[3]
  )
}

# 4) tabella riassuntiva per tutti i modelli
summary_tbl <- imap_dfr(models, extract_one)

# 5) stampa
cat("=== SUMMARY TABLE (diagnostiche + effetti timepoint) ===\n\n")
print(summary_tbl %>% arrange(desc(max_rhat), min_ess_ratio), n = Inf) |>
  as.data.frame()

# 6) evidenzia modelli “attenzione”
cat("\n=== FLAG MODELS (attenzione) ===\n")
flags <- summary_tbl %>%
  mutate(
    flag_rhat = !is.na(max_rhat) & max_rhat > 1.01,
    flag_ess = !is.na(min_ess_ratio) & min_ess_ratio < 0.1,
    flag_div = !is.na(n_divergent) & n_divergent > 0,
    flag_td = !is.na(n_treedepth_hits) & n_treedepth_hits > 0
  ) %>%
  filter(flag_rhat | flag_ess | flag_div | flag_td) %>%
  arrange(desc(flag_div), desc(flag_rhat), flag_ess)

if (nrow(flags) == 0) {
  cat(
    "✓ Nessun modello supera le soglie (Rhat>1.01, ESS<0.1, divergences, treedepth hits)\n"
  )
} else {
  print(
    flags %>%
      select(model, max_rhat, min_ess_ratio, n_divergent, n_treedepth_hits),
    n = Inf
  )
}

# 7) (opzionale) Per lognormal: converti effetti in ratio (exp) per interpretazione più intuitiva
cat("\n=== NOTE: lognormal outcomes ===\n")
cat(
  "Se l'outcome è lognormal (f0_std, jitter, f2_std), gli effetti sono su log-scale.\n"
)
cat(
  "Puoi interpretare exp(beta) come moltiplicatore (ratio) rispetto a baseline.\n"
)


suppressPackageStartupMessages({
  library(tidyverse)
  library(brms)
})

cat("=== EFFETTI SIGNIFICATIVI (95% CrI esclude 0) ===\n\n")

# ------------------------------------------------------------------
# 1) carica tutti i modelli
# ------------------------------------------------------------------
files <- list.files("models", pattern = "\\.rds$", full.names = TRUE)
models <- setNames(
  lapply(files, readRDS),
  tools::file_path_sans_ext(basename(files))
)

# ------------------------------------------------------------------
# 2) helper: verifica se CI esclude 0
# ------------------------------------------------------------------
is_significant <- function(l, u) {
  !is.na(l) && !is.na(u) && (l > 0 || u < 0)
}

# ------------------------------------------------------------------
# 3) estrai SOLO effetti significativi
# ------------------------------------------------------------------
extract_sig_effects <- function(mod, name) {
  fe <- tryCatch(fixef(mod), error = function(e) NULL)
  if (is.null(fe)) return(NULL)

  out <- list()

  # --- PRE vs BASELINE
  if ("timepointpre" %in% rownames(fe)) {
    l <- fe["timepointpre", "Q2.5"]
    u <- fe["timepointpre", "Q97.5"]
    if (is_significant(l, u)) {
      out$pre <- tibble(
        model = name,
        contrast = "PRE vs BASELINE",
        estimate = fe["timepointpre", "Estimate"],
        l = l,
        u = u
      )
    }
  }

  # --- POST vs BASELINE
  if ("timepointpost" %in% rownames(fe)) {
    l <- fe["timepointpost", "Q2.5"]
    u <- fe["timepointpost", "Q97.5"]
    if (is_significant(l, u)) {
      out$post <- tibble(
        model = name,
        contrast = "POST vs BASELINE",
        estimate = fe["timepointpost", "Estimate"],
        l = l,
        u = u
      )
    }
  }

  # --- POST vs PRE
  h <- tryCatch(
    hypothesis(mod, "timepointpost - timepointpre = 0"),
    error = function(e) NULL
  )

  if (!is.null(h)) {
    l <- h$hypothesis$CI.Lower
    u <- h$hypothesis$CI.Upper
    if (is_significant(l, u)) {
      out$post_pre <- tibble(
        model = name,
        contrast = "POST vs PRE",
        estimate = h$hypothesis$Estimate,
        l = l,
        u = u
      )
    }
  }

  if (length(out) == 0) return(NULL)
  bind_rows(out)
}

# ------------------------------------------------------------------
# 4) applica a tutti i modelli
# ------------------------------------------------------------------
sig_tbl <- imap_dfr(models, extract_sig_effects)

# ------------------------------------------------------------------
# 5) stampa risultato
# ------------------------------------------------------------------
if (nrow(sig_tbl) == 0) {
  cat("⚠ Nessun effetto con CrI95% che esclude 0.\n")
} else {
  cat("✓ Effetti trovati:\n\n")
  print(
    sig_tbl %>%
      arrange(model, contrast) %>%
      mutate(
        estimate = round(estimate, 3),
        l = round(l, 3),
        u = round(u, 3)
      ),
    n = Inf
  )
}
# model        contrast         estimate        l       u
# <chr>        <chr>               <dbl>    <dbl>   <dbl>
# 1 m1_f0_mean_a POST vs BASELINE    2.32     0.105   4.52
# 2 m1_f0_mean_a PRE vs BASELINE     2.98     0.896   5.08
# 3 m1_f0_mean_i POST vs PRE        -3.24    -5.98   -0.56
# 4 m1_f0_mean_i PRE vs BASELINE     3.33     0.752   5.93
# 5 m1_f0_mean_u POST vs BASELINE    2.64     0.211   5.1
# 6 m1_f0_mean_u PRE vs BASELINE     3.25     0.983   5.6
# 7 m1_f0mean_a  POST vs BASELINE    2.32     0.105   4.52
# 8 m1_f0mean_a  PRE vs BASELINE     2.98     0.896   5.08
# 9 m2_f0_std_a  POST vs BASELINE   -0.182   -0.343  -0.02
# 10 m2_f0std_a   POST vs BASELINE   -0.182   -0.346  -0.017
# 11 m3_jitter_a  POST vs BASELINE   -0.131   -0.257  -0.005
# 12 m4_nne_a     POST vs BASELINE   -0.797   -1.47   -0.129
# 13 m4_nne_a     PRE vs BASELINE    -0.758   -1.46   -0.065
# 14 m5_f2_mean_a PRE vs BASELINE   -19.2    -37.2    -0.948
# 15 m5_f2_mean_i POST vs BASELINE  -61.8   -113.    -11.2
# 16 m5_f2mean_a  PRE vs BASELINE   -19.2    -37.2    -0.948

cat("\nLegenda:\n")
cat("• PRE vs BASELINE  = effetto dello stress anticipatorio\n")
cat("• POST vs BASELINE = recupero / persistenza post-esame\n")
cat("• POST vs PRE      = dinamica stress → recupero\n")

#' Il pattern complessivo è chiaro e teoricamente sensato
#' F0 mean (altezza tonale)
#' Risultato più solido dell’intero studio.
#' PRE vs BASELINE:
#' /a/: +2.98 Hz
#' /i/: +3.33 Hz
#' /u/: +3.25 Hz
#' POST vs BASELINE:
#' /a/: +2.32 Hz
#' /u/: +2.64 Hz
#' POST vs PRE (/i/): −3.24 Hz
#' 👉 Interpretazione
#' Il giorno prima dell’esame (PRE) la voce è sistematicamente più acuta
#' rispetto a un giorno neutro. Questo è perfettamente coerente con l’attivazione
#' simpatica da stress (↑ tensione laringea).
#'
#' Per /i/ si vede anche un chiaro recupero POST (POST < PRE).
#' Per /a/ e /u/ il POST resta ancora sopra baseline → possibile stress
#' residuo / incomplete recovery.
#'
#'  Conclusione F0
#'  Evidenza forte che lo stress da esame aumenta il pitch medio, in modo:
#'  coerente tra vocali
#'  replicato su più confronti
#'  con direzione teoricamente attesa
#'
#'  F0 variability (F0 std)
#'  POST vs BASELINE (/a/): −0.18 (log-scale)
#'  Interpretazione
#'  Riduzione della variabilità di F0 dopo l’esame.
#'  Suggerisce una voce più rigida / meno flessibile, compatibile con:
#'  affaticamento
#'  controllo motorio meno fine dopo stress acuto
#'
#'  Conclusione
#'  Effetto presente ma limitato (una vocale, un contrasto).
#'  Evidenza debole–moderata, ma coerente con F0 mean.
#'
#'  Jitter
#'  POST vs BASELINE (/a/): −0.13 (log-scale)
#'  Interpretazione
#'  Minore micro-instabilità dopo l’esame.
#'  Possibile:
#'  riduzione dell’arousal dopo il picco PRE
#'  oppure strategia di iper-controllo vocale post-stress
#'
#'  Conclusione
#'  Effetto piccolo, presente solo per /a/.
#'  Evidenza debole, ma nella direzione interpretabile.
#'
#'  NNE (rumore glottico)
#'  PRE vs BASELINE (/a/): −0.76
#'  POST vs BASELINE (/a/): −0.80
#'
#'  Interpretazione
#'  Meno rumore sia prima che dopo l’esame rispetto a baseline.
#'  Voce più “tesa/controllata”, meno breathy.
#'  Questo è molto coerente con uno stato di stress anticipatorio +
#'  mantenimento post-esame.
#'  Conclusione
#'  Effetto coerente e teoricamente chiaro, ma:
#'  confinato a una vocale non universale
#'  → Evidenza moderata
#'
#'  F2 mean (articolazione)
#'  PRE vs BASELINE
#'  /a/: −19 Hz
#'  POST vs BASELINE
#'  /i/: −62 Hz
#' Interpretazione
#'
#' Riduzione di F2 = articolazione meno estrema / più centralizzata.
#' Compatibile con:
#' carico cognitivo
#' ridotta precisione motoria sotto stress
#' Conclusione
#' Effetto interessante ma eterogeneo (diverse vocali, diversi momenti).
#' Evidenza debole–moderata, non ancora robusta.
#'
#'  Coerenza interna dei risultati
#'  ✔️ Molto alta per F0 mean
#'  ✔️ Buona per NNE
#'  ➖ Limitata per jitter e F0 std
#'  ➖ Frammentaria per F2
#'
#'  Non ci sono contraddizioni (nessun effetto va nella direzione opposta a quanto atteso), ma:
#'  alcuni effetti sono parziali, altri emergono solo in una vocale
#'  Questo è tipico di:
#'  effetti reali ma piccoli
#'  modulati da biomeccanica specifica delle vocali
#'
#'  3️⃣ Quanto è forte l’evidenza che lo stress modula la voce?
#'  🔵 Conclusione globale (onesta e difendibile)
#'  I dati forniscono evidenza moderata–forte che lo stress da esame modula
#'  le proprietà vocali, in particolare:
#'   aumentando l’altezza tonale (F0 mean) in modo consistente e replicato,
#'   e inducendo una voce più tesa e controllata (↓NNE, ↓variabilità).
#'   Non è un effetto massivo su tutte le misure, ma:
#'    è coerente
#'    è direzionalmente corretto
#'    emerge dove la teoria lo predice di più (pitch, tensione)
#'
#'    👉 Questo è esattamente il profilo di un effetto psicofisiologico reale
#'    ma di piccola–media ampiezza, non rumore.
#'
#'    4️⃣ Come lo scriverei in una frase da paper (se vuoi)
#'
#'    “Stress anticipatorio legato all’esame universitario era associato a un
#'    aumento consistente dell’altezza tonale (F0 mean) e a una riduzione del
#'    rumore vocale, suggerendo una maggiore tensione e controllo fonatorio.
#'    Altri parametri acustici hanno mostrato effetti più deboli e
#'    vocali-specifici.”
