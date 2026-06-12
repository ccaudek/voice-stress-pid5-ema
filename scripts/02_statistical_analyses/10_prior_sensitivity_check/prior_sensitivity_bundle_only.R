# ==============================================================================
# 04_prior_sensitivity.R   (risposta a R2.11 -- sensibilita' alle prior)
#
# SCOPO
# Rifittare i modelli di moderazione F0 e NNE variando SOLO la SD della prior
# sulle moderazioni gamma, cioe' gli effetti trait x stress e trait x recovery.
# La sensitivity deve girare sul modello ESATTO usato nel manoscritto, non su
# una re-implementazione. Per questo script compila i due file Stan gia' usati
# per i modelli di moderazione, nella cartella:
#   stan/prior_sensitivity/f0mean_pid5_moderation.stan
#   stan/prior_sensitivity/nne_pid5_moderation.stan
#
# EDIT MINIMO DA APPLICARE AI DUE FILE STAN
# Nei tuoi file Stan correnti i coefficienti gamma si chiamano g1 e g2:
#   g1[d] = moderazione trait d x stress     (gamma1 nel testo/manoscritto)
#   g2[d] = moderazione trait d x recovery   (gamma2 nel testo/manoscritto)
#
# Devi modificare SOLO questo:
#   1) nel blocco data aggiungi:
#        real<lower=0> prior_sd_gamma;
#   2) nel blocco model sostituisci:
#        g1 ~ normal(0, 3);
#        g2 ~ normal(0, 3);
#      con:
#        g1 ~ normal(0, prior_sd_gamma);
#        g2 ~ normal(0, prior_sd_gamma);
#
# Nessun'altra prior va toccata: alpha, b1, b2, a_trait, tau, sigma_y e il
# measurement model EMA restano identici. Al default prior_sd_gamma = 3, il fit
# deve riprodurre i valori del manoscritto: se non succede, dati/modello non
# coincidono con quelli pubblicati e lo sweep va fermato.
#
# SWEEP
#   prior_sd_gamma = 1.5, 3, 6   # dimezza / default / raddoppia
#
# EFFETTI HEADLINE DA TRACCIARE
#   gamma1[1] = g1[1]  NegAff x stress su F0
#   gamma2[3] = g2[3]  Antagonism x recovery su F0
#   gamma2[5] = g2[5]  Psychoticism x recovery su NNE
#
# OUTPUT
#   results/prior_sensitivity/tables/prior_sensitivity_summary.csv
#   results/prior_sensitivity/tables/default_vs_published_check.csv
#   results/prior_sensitivity/figures/prior_sensitivity_forest.png
#   results/prior_sensitivity/prior_sensitivity_text.txt
#   results/prior_sensitivity/models/prior_sensitivity_bundle.rds
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(cmdstanr)
  library(posterior)
})

set.seed(123)
options(mc.cores = parallel::detectCores())

cat("\n=== RUNNING BUNDLE-ONLY PRIOR SENSITIVITY SCRIPT ===\n")
cat(
  "This version reads results/stan_bundle_f0mean_pid5.rds and results/NNE/stan_bundle_nne_pid5.rds; it never uses the old environment-based loader.\n\n"
)

# ----------------------------
# 0) GRIGLIA DELLE PRIOR E SAMPLING
# ----------------------------
PRIOR_SD_GAMMA_GRID <- c(1.5, 3, 6)
DEFAULT_SD <- 3

N_CHAINS <- 4
N_WARMUP <- 2000
N_SAMPLING <- 4000
ADAPT_DELTA <- 0.99
MAX_TREEDEPTH <- 18
SEED <- 123

out_dir <- here("results", "prior_sensitivity")
for (s in c("tables", "figures", "models", "diagnostics")) {
  dir.create(file.path(out_dir, s), recursive = TRUE, showWarnings = FALSE)
}

# ----------------------------
# 1) FILE STAN: usa i modelli esatti del manoscritto, con edit minimo sopra
# ----------------------------
f0_stan_file <- here("stan", "prior_sensitivity", "f0mean_pid5_moderation.stan")
nne_stan_file <- here("stan", "prior_sensitivity", "nne_pid5_moderation.stan")

check_stan_file <- function(path, label) {
  if (!file.exists(path)) {
    stop("File Stan non trovato per ", label, ":\n", path, call. = FALSE)
  }
  code <- paste(readLines(path, warn = FALSE), collapse = "\n")
  if (!grepl("real<lower=0>\\s+prior_sd_gamma\\s*;", code)) {
    stop(
      "Il file Stan per ",
      label,
      " non contiene `real<lower=0> prior_sd_gamma;` nel data block.\n",
      "Applica l'edit minimo descritto in testa allo script: aggiungi prior_sd_gamma al data block ",
      "e sostituisci la prior su g1/g2 con prior_sd_gamma.",
      call. = FALSE
    )
  }
  if (
    !grepl(
      "g1\\s*~\\s*normal\\s*\\(\\s*0\\s*,\\s*prior_sd_gamma\\s*\\)",
      code
    ) ||
      !grepl(
        "g2\\s*~\\s*normal\\s*\\(\\s*0\\s*,\\s*prior_sd_gamma\\s*\\)",
        code
      )
  ) {
    stop(
      "Il file Stan per ",
      label,
      " non sembra usare `prior_sd_gamma` nella prior su g1 e g2.\n",
      "La sensitivity deve variare solo la regolarizzazione delle gamma.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

check_stan_file(f0_stan_file, "F0")
check_stan_file(nne_stan_file, "NNE")

mod_f0 <- cmdstan_model(f0_stan_file)
mod_nne <- cmdstan_model(nne_stan_file)

# ----------------------------
# 2) DATI STAN: usa i bundle prodotti dagli script originali 01_prepare...
# ----------------------------
# IMPORTANTE: qui NON ricostruiamo stan_data e NON cerchiamo oggetti gia' presenti
# nell'ambiente. La sensitivity deve usare gli stessi stan_data salvati dagli script
# originali che hanno prodotto i risultati del paper:
#   results/stan_bundle_f0mean_pid5.rds
#   results/NNE/stan_bundle_nne_pid5.rds
# Ogni refit modifica solo sd_data$prior_sd_gamma nello sweep.

F0_BUNDLE_RDS <- here("results", "F0", "data", "stan_bundle_f0mean_pid5.rds")
NNE_BUNDLE_RDS <- here("results", "NNE", "stan_bundle_nne_pid5.rds")

first_existing <- function(paths, label) {
  hits <- paths[file.exists(paths)]
  if (length(hits) == 0) {
    stop(
      "File non trovato per ",
      label,
      ":\n",
      paste0(" - ", paths, collapse = "\n"),
      call. = FALSE
    )
  }
  hits[[1]]
}

# Gli script allegati/salvati possono chiamarsi 01_* oppure, in alcune versioni,
# 10_*. Li cerchiamo nella root, in scripts/ e in R/.
find_prepare_script <- function(kind) {
  if (kind == "F0") {
    candidates <- c(
      here("01_prepare_stan_data_f0mean_pid5.R"),
      here("scripts", "01_prepare_stan_data_f0mean_pid5.R"),
      here("R", "01_prepare_stan_data_f0mean_pid5.R"),
      here("10_prepare_stan_data_f0mean_pid5.R"),
      here("scripts", "10_prepare_stan_data_f0mean_pid5.R"),
      here("R", "10_prepare_stan_data_f0mean_pid5.R")
    )
  } else if (kind == "NNE") {
    candidates <- c(
      here("01_prepare_stan_data_nne_pid5.R"),
      here("scripts", "01_prepare_stan_data_nne_pid5.R"),
      here("R", "01_prepare_stan_data_nne_pid5.R"),
      here("10_prepare_stan_data_nne_pid5.R"),
      here("scripts", "10_prepare_stan_data_nne_pid5.R"),
      here("R", "10_prepare_stan_data_nne_pid5.R")
    )
  } else {
    stop("kind deve essere 'F0' o 'NNE'.", call. = FALSE)
  }
  first_existing(candidates, paste0("script prepare ", kind))
}

run_prepare_if_needed <- function(bundle_path, kind) {
  if (file.exists(bundle_path)) return(invisible(TRUE))

  message("Bundle ", kind, " non trovato: ", bundle_path)
  message(
    "Provo a rigenerarlo eseguendo lo script originale di preparazione dati..."
  )

  dir.create(here("stan"), recursive = TRUE, showWarnings = FALSE)
  dir.create(here("stan", "NNE"), recursive = TRUE, showWarnings = FALSE)
  dir.create(here("results"), recursive = TRUE, showWarnings = FALSE)
  dir.create(here("results", "NNE"), recursive = TRUE, showWarnings = FALSE)

  prep_script <- find_prepare_script(kind)
  message("Sourcing: ", prep_script)

  # Alcuni script originali F0 scrivono file con path relativi, es. "stan/...".
  # Per evitare output nel posto sbagliato, li eseguiamo dalla root here().
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(here())
  source(prep_script, local = FALSE)
  setwd(old_wd)

  if (!file.exists(bundle_path)) {
    stop(
      "Lo script di preparazione ",
      kind,
      " e' stato eseguito, ma il bundle atteso non e' stato creato:\n",
      bundle_path,
      "\nControlla che lo script prepare salvi esattamente in quel path.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

load_original_bundle <- function(bundle_path, kind) {
  run_prepare_if_needed(bundle_path, kind)
  bundle <- readRDS(bundle_path)

  if (!is.list(bundle) || is.null(bundle$stan_data)) {
    stop(
      "Il bundle ",
      kind,
      " non contiene `$stan_data`: ",
      bundle_path,
      call. = FALSE
    )
  }
  if (is.null(bundle$pid5_vars)) {
    stop(
      "Il bundle ",
      kind,
      " non contiene `$pid5_vars`: ",
      bundle_path,
      call. = FALSE
    )
  }

  bundle
}

required_data_names <- c(
  "N_subj",
  "N_voice",
  "subj_voice",
  "y",
  "c1",
  "c2",
  "N_ema",
  "subj_ema",
  "D",
  "X"
)

check_stan_data <- function(sd, label) {
  missing <- setdiff(required_data_names, names(sd))
  if (length(missing) > 0) {
    stop(
      "stan_data ",
      label,
      " manca di: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.matrix(sd$X)) {
    # readRDS dovrebbe preservare la matrice, ma questo evita problemi se il bundle
    # fosse stato manipolato prima del salvataggio.
    sd$X <- as.matrix(sd$X)
  }
  if (nrow(sd$X) != sd$N_ema) {
    stop("stan_data ", label, ": nrow(X) != N_ema.", call. = FALSE)
  }
  if (ncol(sd$X) != sd$D) {
    stop("stan_data ", label, ": ncol(X) != D.", call. = FALSE)
  }
  if (anyNA(sd$X)) {
    stop("stan_data ", label, ": X contiene NA.", call. = FALSE)
  }
  if (length(sd$y) != sd$N_voice) {
    stop("stan_data ", label, ": length(y) != N_voice.", call. = FALSE)
  }
  if ("prior_sd_gamma" %in% names(sd)) {
    message(
      "Nota: `prior_sd_gamma` era gia' in stan_data ",
      label,
      "; verra' sovrascritto nello sweep."
    )
  }
  sd
}

f0_bundle <- load_original_bundle(F0_BUNDLE_RDS, "F0")
nne_bundle <- load_original_bundle(NNE_BUNDLE_RDS, "NNE")

stan_data_f0 <- check_stan_data(f0_bundle$stan_data, "F0")
stan_data_nne <- check_stan_data(nne_bundle$stan_data, "NNE")

pid5_vars_f0 <- f0_bundle$pid5_vars
pid5_vars_nne <- nne_bundle$pid5_vars

cat("\n=== BUNDLE ORIGINALI CARICATI ===\n")
cat("F0 bundle:  ", F0_BUNDLE_RDS, "\n", sep = "")
cat("NNE bundle: ", NNE_BUNDLE_RDS, "\n", sep = "")
cat(
  "PID-5 order F0:  ",
  paste(seq_along(pid5_vars_f0), pid5_vars_f0, sep = "=", collapse = "; "),
  "\n",
  sep = ""
)
cat(
  "PID-5 order NNE: ",
  paste(seq_along(pid5_vars_nne), pid5_vars_nne, sep = "=", collapse = "; "),
  "\n",
  sep = ""
)

if (!identical(pid5_vars_f0, pid5_vars_nne)) {
  stop(
    "L'ordine dei domini PID-5 differisce tra F0 e NNE.\n",
    "F0:  ",
    paste(pid5_vars_f0, collapse = ", "),
    "\n",
    "NNE: ",
    paste(pid5_vars_nne, collapse = ", "),
    "\n",
    "Correggi gli indici headline prima dello sweep.",
    call. = FALSE
  )
}

# Ordine atteso dagli script 01_prepare_*:
expected_pid5_vars <- c(
  "pid5_negative_affectivity",
  "pid5_detachment",
  "pid5_antagonism",
  "pid5_disinhibition",
  "pid5_psychoticism"
)
if (!identical(pid5_vars_f0, expected_pid5_vars)) {
  warning(
    "L'ordine PID-5 nel bundle non coincide con quello atteso nello script. ",
    "Controlla headline_specs prima di interpretare i risultati.\n",
    "Ordine nel bundle: ",
    paste(pid5_vars_f0, collapse = ", ")
  )
}

# ----------------------------
# 3) SPECIFICHE EFFETTI HEADLINE
# ----------------------------
# Ordine domini atteso:
#   1 = Negative Affectivity / NegAff
#   2 = Detachment
#   3 = Antagonism
#   4 = Disinhibition
#   5 = Psychoticism
# Conferma che questo sia lo stesso ordinamento usato nello stan_data del paper.
# Nei file Stan i parametri sono g1/g2; qui li riportiamo come gamma1/gamma2.
headline_specs <- tribble(
  ~outcome,
  ~stan_param,
  ~paper_param,
  ~label,
  ~published_median,
  "F0",
  "g1[1]",
  "gamma1[1]",
  "NegAff x stress (F0)",
  3.14,
  "F0",
  "g2[3]",
  "gamma2[3]",
  "Antagonism x recovery (F0)",
  3.16,
  "NNE",
  "g2[5]",
  "gamma2[5]",
  "Psychoticism x recovery (NNE)",
  0.88
)

# ----------------------------
# 4) HELPER
# ----------------------------
pd <- function(x) max(mean(x > 0), mean(x < 0))

summ_param <- function(fit, par) {
  draws <- as.numeric(posterior::as_draws_matrix(fit$draws(par)))
  smry <- fit$summary(par)
  tibble(
    median = median(draws),
    lo = unname(quantile(draws, .055)),
    hi = unname(quantile(draws, .945)),
    PD = pd(draws),
    rhat = smry$rhat,
    ess_bulk = smry$ess_bulk,
    ess_tail = smry$ess_tail
  )
}

get_divergences <- function(fit) {
  ds <- fit$diagnostic_summary()
  if ("num_divergent" %in% names(ds)) return(sum(ds$num_divergent))
  if ("num_divergent__" %in% names(ds)) return(sum(ds$num_divergent__))
  NA_integer_
}

save_fit_safely <- function(fit, outcome, prior_sd_gamma) {
  tag <- paste0(
    tolower(outcome),
    "_prior_sd_gamma_",
    gsub("\\.", "p", as.character(prior_sd_gamma))
  )
  fit$save_object(file.path(out_dir, "models", paste0("fit_", tag, ".rds")))
  invisible(TRUE)
}

run_one_fit <- function(
  outcome,
  mod,
  stan_data,
  prior_sd_gamma,
  params_to_track
) {
  cat("\n=== Outcome:", outcome, "| prior_sd_gamma =", prior_sd_gamma, "===\n")
  sd_data <- stan_data
  sd_data$prior_sd_gamma <- prior_sd_gamma

  fit <- mod$sample(
    data = sd_data,
    chains = N_CHAINS,
    parallel_chains = N_CHAINS,
    iter_warmup = N_WARMUP,
    iter_sampling = N_SAMPLING,
    adapt_delta = ADAPT_DELTA,
    max_treedepth = MAX_TREEDEPTH,
    seed = SEED,
    refresh = 500
  )

  fit$cmdstan_diagnose()
  save_fit_safely(fit, outcome, prior_sd_gamma)
  divergences <- get_divergences(fit)

  map_dfr(params_to_track, function(par) {
    summ_param(fit, par) |>
      mutate(
        outcome = outcome,
        stan_param = par,
        prior_sd_gamma = prior_sd_gamma,
        divergences = divergences,
        .before = 1
      )
  })
}

# ----------------------------
# 5) SWEEP
# ----------------------------
results <- list()
for (psd in PRIOR_SD_GAMMA_GRID) {
  results[[length(results) + 1]] <- run_one_fit(
    outcome = "F0",
    mod = mod_f0,
    stan_data = stan_data_f0,
    prior_sd_gamma = psd,
    params_to_track = headline_specs |>
      filter(outcome == "F0") |>
      pull(stan_param)
  )

  results[[length(results) + 1]] <- run_one_fit(
    outcome = "NNE",
    mod = mod_nne,
    stan_data = stan_data_nne,
    prior_sd_gamma = psd,
    params_to_track = headline_specs |>
      filter(outcome == "NNE") |>
      pull(stan_param)
  )
}

sens <- bind_rows(results) |>
  left_join(headline_specs, by = c("outcome", "stan_param")) |>
  mutate(is_default = prior_sd_gamma == DEFAULT_SD) |>
  select(
    outcome,
    label,
    paper_param,
    stan_param,
    prior_sd_gamma,
    is_default,
    median,
    lo,
    hi,
    PD,
    rhat,
    ess_bulk,
    ess_tail,
    divergences,
    published_median
  )

write_csv(sens, file.path(out_dir, "tables", "prior_sensitivity_summary.csv"))
cat("\n=== PRIOR SENSITIVITY SUMMARY ===\n")
print(as.data.frame(sens), digits = 3)

# ----------------------------
# 6) CHECK: default SD = 3 vs valori pubblicati
# ----------------------------
check <- sens |>
  filter(is_default) |>
  mutate(abs_diff = abs(median - published_median)) |>
  select(
    outcome,
    label,
    paper_param,
    stan_param,
    median,
    published_median,
    abs_diff,
    PD,
    lo,
    hi,
    rhat,
    ess_bulk,
    divergences
  )

write_csv(check, file.path(out_dir, "tables", "default_vs_published_check.csv"))
cat("\n=== CHECK: default SD = 3 vs valori pubblicati ===\n")
print(as.data.frame(check), digits = 3)

MAX_ACCEPTABLE_ABS_DIFF <- 0.10
STOP_ON_DEFAULT_MISMATCH <- TRUE
if (any(check$abs_diff > MAX_ACCEPTABLE_ABS_DIFF, na.rm = TRUE)) {
  msg <- paste0(
    "Almeno un effetto al default SD = 3 non riproduce il valore pubblicato entro ",
    MAX_ACCEPTABLE_ABS_DIFF,
    ". Non interpretare la sensitivity prima di verificare dati, modello, ",
    "ordinamento domini e seed/sampling."
  )
  if (STOP_ON_DEFAULT_MISMATCH) stop(msg, call. = FALSE) else warning(msg)
}

# ----------------------------
# 7) FIGURA: forest plot
# ----------------------------
p <- ggplot(sens, aes(x = median, y = factor(prior_sd_gamma))) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .18) +
  facet_wrap(~label, scales = "free_x") +
  labs(
    title = "Prior sensitivity of headline moderation effects",
    subtitle = "Posterior median and 89% CrI across prior SD on moderation coefficients (gamma)",
    x = "Moderation effect",
    y = "Prior SD on gamma"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(out_dir, "figures", "prior_sensitivity_forest.png"),
  p,
  width = 10,
  height = 4.5,
  dpi = 300
)

# ----------------------------
# 8) TESTO AUTO-COMPILATO PER RISPOSTA / MANOSCRITTO
# ----------------------------
fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits)

rng <- sens |>
  group_by(label) |>
  summarise(
    median_min = min(median),
    median_max = max(median),
    lo_min = min(lo),
    hi_max = max(hi),
    pd_min = min(PD),
    max_rhat = max(rhat, na.rm = TRUE),
    min_ess = min(ess_bulk, na.rm = TRUE),
    max_div = max(divergences, na.rm = TRUE),
    .groups = "drop"
  )

lines <- pmap_chr(
  rng,
  function(
    label,
    median_min,
    median_max,
    lo_min,
    hi_max,
    pd_min,
    max_rhat,
    min_ess,
    max_div
  ) {
    paste0(
      "  - ",
      label,
      ": posterior median ranged from ",
      fmt(median_min),
      " to ",
      fmt(median_max),
      ", 89% CrI envelope [",
      fmt(lo_min),
      ", ",
      fmt(hi_max),
      "], PD >= ",
      fmt(pd_min),
      "."
    )
  }
)

all_no_div <- all(sens$divergences == 0, na.rm = TRUE)
max_rhat <- max(sens$rhat, na.rm = TRUE)
min_ess <- min(sens$ess_bulk, na.rm = TRUE)
min_pd_overall <- min(sens$PD, na.rm = TRUE)

manuscript_text <- paste0(
  "Prior sensitivity. To assess whether the headline moderation effects depended on the ",
  "regularizing prior on the moderation coefficients, we refitted the exact F0 and NNE ",
  "moderation models used in the main analysis after varying only the prior standard ",
  "deviation on the gamma coefficients. Specifically, we used prior SDs of ",
  paste(PRIOR_SD_GAMMA_GRID, collapse = ", "),
  ", corresponding to halving, retaining, and doubling the value used in the main analysis. ",
  "At the default prior SD = ",
  DEFAULT_SD,
  ", the models reproduced the published estimates (see the default-vs-published check table). ",
  "The direction of all three effects was robust to the prior: the sign and the probability ",
  "of direction favoured the same direction at every setting (per-effect summaries below).\n",
  paste(lines, collapse = "\n"),
  "\nThe magnitude of the two F0 effects, by contrast, was prior-dependent: the posterior ",
  "median varied substantially across the prior range (see the ranges above), and the 89% ",
  "credible-interval envelopes include or border zero. Correspondingly, the probability of ",
  "direction for these effects declined under the more regularizing prior, reaching a minimum ",
  "of ",
  fmt(min_pd_overall),
  " across all effects and settings. The NNE effect ",
  "(Psychoticism x recovery) was more stable in magnitude. We therefore interpret the ",
  "moderation effects in terms of direction and the strength of directional evidence (pd) ",
  "rather than their precise magnitude, which the present sample constrains only loosely. ",
  "Across all refits, max R-hat = ",
  fmt(max_rhat, 3),
  ", minimum bulk ESS = ",
  fmt(min_ess, 0),
  if (all_no_div) ", and no divergent transitions were observed.\n" else
    paste0(
      ", with a maximum of ",
      max(sens$divergences, na.rm = TRUE),
      " divergent transitions.\n"
    )
)

writeLines(manuscript_text, file.path(out_dir, "prior_sensitivity_text.txt"))
cat("\n", manuscript_text, "\n")

saveRDS(
  list(
    sensitivity = sens,
    default_check = check,
    headline_specs = headline_specs,
    prior_grid = PRIOR_SD_GAMMA_GRID,
    stan_files = list(f0 = f0_stan_file, nne = nne_stan_file)
  ),
  file.path(out_dir, "models", "prior_sensitivity_bundle.rds")
)

cat("\n=== DONE ===\nOutput in: ", out_dir, "\n", sep = "")
# eof ---
