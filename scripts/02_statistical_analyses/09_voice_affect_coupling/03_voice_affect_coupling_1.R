# ==============================================================================
# 03_voice_affect_coupling.R   (risposta a R1.9)
# Accoppiamento between-person: il cambiamento di F0 indotto dallo stress traccia
# il cambiamento di affetto negativo auto-riferito?
#
# Operazionalizzazione (fase di stress, l'unica allineata tra voce ed EMA):
#   ΔF0_stress  = F0_pre  - F0_baseline   (Hz; media sulle 3 vocali /a/,/i/,/u/)
#   ΔNA_stress  = NA_pre  - NA_baseline   (NA = angry+sad+rev(happy)+rev(satisfied), 0-100)
#   Regressione bayesiana: z(ΔF0) ~ z(ΔNA)  -> la pendenza E' la correlazione (coupling).
#
# Recovery (ESPLORATIVO, disallineato): ΔF0_rec = F0_post-F0_pre vs ΔNA_rec = NA_post-NA_pre.
#   NB: POST affetto = sera dell'esame; POST voce (T3) = giorno dopo. Interpretare con cautela.
#
# Stesso ecosistema degli altri script: AUDIO.xlsx per F0, EMA per NA, stessa
# correzione ID, cmdstanr.
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse); library(readxl); library(here)
  library(cmdstanr); library(posterior); library(bayesplot); library(loo)
})
set.seed(123); options(mc.cores = parallel::detectCores())

ITEM_MIN <- 0; ITEM_MAX <- 100                      # scala item umore EMA
N_CHAINS <- 4; N_WARMUP <- 2000; N_SAMPLING <- 4000
ADAPT_DELTA <- 0.95; MAX_TREEDEPTH <- 15; SEED <- 123

# ----------------------------
# 0) PATHS
# ----------------------------
first_existing <- function(paths, label) {
  ex <- paths[file.exists(paths)]
  if (!length(ex)) stop("File non trovato per ", label, ":\n", paste0(" - ", paths, collapse = "\n"), call. = FALSE)
  ex[[1]]
}
audio_path <- first_existing(c(
  here("data","raw","acustic_features","datiacustici","AUDIO.xlsx"),
  here("data","raw","acoustic_features","datiacustici","AUDIO.xlsx"),
  here("data","raw","AUDIO.xlsx"), "AUDIO.xlsx"), "AUDIO.xlsx")
ema_path <- first_existing(c(
  here("data","processed","ema_plus_scales_cleaned.csv"),
  here("data","cleaned","ema_plus_scales_cleaned.csv"),
  here("data","ema_plus_scales_cleaned.csv"), "ema_plus_scales_cleaned.csv"), "ema_plus_scales_cleaned.csv")

out_dir <- here("results","coupling","voice_affect")
for (s in c("models","figures","tables","stan")) dir.create(file.path(out_dir, s), recursive = TRUE, showWarnings = FALSE)

fix_id <- function(x) dplyr::case_when(
  x == "am_bo_1988_08_24_166" ~ "an_bo_1988_08_24_166",
  x == "as_li_2005_04_26_447" ~ "as_si_2005_04_26_447",
  x == "cl_bo_1987_10_16_628" ~ "ca_bo_1987_10_16_628",
  x == "hi_na_2005_03_08_339" ~ "gi_na_2005_03_08_339",
  x == "ma_si_2003_10_31_940" ~ "si_ma_2003_10_31_940",
  TRUE ~ x)

# ----------------------------
# 1) F0 PER SOGGETTO x SESSIONE (media sulle 3 vocali)
# ----------------------------
read_f0 <- function(sheet, tp) {
  d <- read_excel(audio_path, sheet = sheet); names(d) <- stringr::str_trim(names(d))
  f0cols <- paste0("F0 mean Hz /", c("a","i","u"), "/")
  stopifnot(all(f0cols %in% names(d)))
  d |>
    mutate(ID = fix_id(stringr::str_trim(as.character(ID))),
           F0 = rowMeans(across(all_of(f0cols)), na.rm = TRUE),
           timepoint = tp) |>
    select(ID, timepoint, F0) |>
    filter(!is.na(ID), ID != "", !is.nan(F0))
}
df_f0 <- bind_rows(read_f0("BASELINE","baseline"), read_f0("PRE","pre"), read_f0("POST","post")) |>
  group_by(ID, timepoint) |> summarise(F0 = mean(F0, na.rm = TRUE), .groups = "drop")

# ----------------------------
# 2) NA EMA PER SOGGETTO x TIMEPOINT (come nel manipulation check)
# ----------------------------
df_ema <- read_csv(ema_path, show_col_types = FALSE) |> rename_with(stringr::str_trim)
req <- c("user_id","exam_period","happy","sad","satisfied","angry")
stopifnot(all(req %in% names(df_ema)))
df_na <- df_ema |>
  mutate(user_id = fix_id(stringr::str_trim(as.character(user_id))),
         exam_period = stringr::str_to_lower(stringr::str_trim(as.character(exam_period))),
         across(c(happy,sad,satisfied,angry), ~ suppressWarnings(as.numeric(.x))),
         timepoint = case_when(
           exam_period %in% c("baseline","base","bsl") ~ "baseline",
           exam_period %in% c("pre","pre_exam","pre-exam","preexam") ~ "pre",
           exam_period %in% c("post","post_exam","post-exam","postexam") ~ "post",
           TRUE ~ NA_character_),
         negative_affect = angry + sad + (ITEM_MAX+ITEM_MIN-happy) + (ITEM_MAX+ITEM_MIN-satisfied)) |>
  filter(!is.na(timepoint), !is.na(negative_affect)) |>
  group_by(user_id, timepoint) |>
  summarise(NA_score = mean(negative_affect, na.rm = TRUE), n_prompts = n(), .groups = "drop") |>
  rename(ID = user_id)

# ----------------------------
# 3) MERGE WIDE + CHANGE SCORES
# ----------------------------
wide <- df_f0 |> rename(F0v = F0) |>
  full_join(df_na |> select(ID, timepoint, NA_score), by = c("ID","timepoint")) |>
  pivot_wider(names_from = timepoint, values_from = c(F0v, NA_score))

coup <- wide |>
  transmute(
    ID,
    dF0_stress = F0v_pre  - F0v_baseline,
    dNA_stress = NA_score_pre  - NA_score_baseline,
    dF0_rec    = F0v_post - F0v_pre,
    dNA_rec    = NA_score_post - NA_score_pre
  )

cat("\n=== N disponibili ===\n")
cat("stress (F0 & NA, baseline+pre):", sum(complete.cases(coup[,c("dF0_stress","dNA_stress")])), "\n")
cat("recovery (F0 & NA, pre+post)  :", sum(complete.cases(coup[,c("dF0_rec","dNA_rec")])), "\n")
write_csv(coup, file.path(out_dir, "tables", "change_scores.csv"))

# ----------------------------
# 4) MODELLO STAN: regressione bayesiana su variabili standardizzate
#    pendenza = correlazione (coupling). log_lik per LOO.
# ----------------------------
stan_file <- file.path(out_dir, "stan", "coupling_regression.stan")
writeLines('
data { int<lower=1> N; vector[N] y; vector[N] x; }   // y,x gia standardizzati
parameters { real b0; real r; real<lower=0> sigma; }
model {
  b0 ~ normal(0, 1);
  r  ~ normal(0, 1);          // weakly informative; pendenza standardizzata = correlazione
  sigma ~ normal(0, 1);
  y ~ normal(b0 + r * x, sigma);
}
generated quantities {
  vector[N] log_lik;
  for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | b0 + r * x[n], sigma);
}
', stan_file)
mod <- cmdstan_model(stan_file)

run_coupling <- function(df, ycol, xcol, label) {
  d <- df |> select(all_of(c(ycol, xcol))) |> tidyr::drop_na()
  n <- nrow(d)
  sd_y_raw <- sd(d[[ycol]])           # per convertire r -> Hz per SD di ΔNA
  ystd <- as.numeric(scale(d[[ycol]])); xstd <- as.numeric(scale(d[[xcol]]))
  fit <- mod$sample(data = list(N = n, y = ystd, x = xstd),
                    chains = N_CHAINS, parallel_chains = N_CHAINS,
                    iter_warmup = N_WARMUP, iter_sampling = N_SAMPLING,
                    adapt_delta = ADAPT_DELTA, max_treedepth = MAX_TREEDEPTH, seed = SEED)
  fit$cmdstan_diagnose()
  rd <- fit$draws("r", format = "df")$r
  # cross-check frequentista
  ct <- suppressWarnings(cor.test(d[[ycol]], d[[xcol]]))
  res <- tibble(
    contrast = label, n = n,
    r_median = median(rd), r_lo = quantile(rd, .025), r_hi = quantile(rd, .975),
    PD = max(mean(rd > 0), mean(rd < 0)),
    slope_Hz_per_SD_dNA = median(rd) * sd_y_raw,   # interpretazione in Hz
    r_pearson_freq = unname(ct$estimate), p_freq = ct$p.value
  )
  fit$save_object(file.path(out_dir, "models", paste0("fit_", label, ".rds")))
  list(res = res, fit = fit, data = d, ycol = ycol, xcol = xcol)
}

cat("\n=== STRESS-PHASE COUPLING (primario) ===\n")
stress <- run_coupling(coup, "dF0_stress", "dNA_stress", "stress")
print(as.data.frame(stress$res), digits = 3)

cat("\n=== RECOVERY-PHASE COUPLING (esplorativo, disallineato) ===\n")
recovery <- run_coupling(coup, "dF0_rec", "dNA_rec", "recovery")
print(as.data.frame(recovery$res), digits = 3)

coupling_all <- bind_rows(stress$res, recovery$res)
write_csv(coupling_all, file.path(out_dir, "tables", "coupling_summary.csv"))

# ----------------------------
# 5) FIGURE: scatter ΔF0 vs ΔNA
# ----------------------------
mk_scatter <- function(obj, ttl) {
  ggplot(obj$data, aes(.data[[obj$xcol]], .data[[obj$ycol]])) +
    geom_point(alpha = .6) +
    geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
    labs(title = ttl, x = "Δ negative affect (EMA)", y = "Δ F0 (Hz)") +
    theme_minimal()
}
ggsave(file.path(out_dir,"figures","scatter_stress.png"),   mk_scatter(stress,   "Stress-phase coupling: ΔF0 vs ΔNA (pre - baseline)"),   width = 6, height = 5, dpi = 300)
ggsave(file.path(out_dir,"figures","scatter_recovery.png"), mk_scatter(recovery, "Recovery-phase coupling (exploratory; misaligned timing)"), width = 6, height = 5, dpi = 300)

ggsave(file.path(out_dir,"figures","posterior_r_stress.png"),
       mcmc_areas(stress$fit$draws("r"), prob = .95, prob_outer = .99) +
         geom_vline(xintercept = 0, linetype = "dashed") +
         labs(title = "Posterior of the stress-phase coupling (correlation r)", x = "r") + theme_minimal(),
       width = 7, height = 4, dpi = 300)

# ----------------------------
# 6) TESTO PER MANOSCRITTO (template)
# ----------------------------
s <- stress$res
fmt <- function(x) formatC(x, format = "f", digits = 2)
manuscript_text <- paste0(
  "Coupling between vocal and affective stress responses. To test whether the vocal stress response tracked ",
  "self-reported affect, we related the anticipatory change in F0 (pre minus baseline) to the anticipatory change ",
  "in EMA negative affect (pre minus baseline) across participants (n = ", s$n, "). ",
  "We focused on the anticipatory phase because the pre-exam voice and EMA assessments are temporally aligned, ",
  "whereas the post-exam EMA prompt (evening of the exam day) and post-exam voice recording (the following day) are not. ",
  "The between-person coupling was r = ", fmt(s$r_median), " [95% CrI ", fmt(s$r_lo), ", ", fmt(s$r_hi), "], ",
  "Pr(r > 0) = ", fmt(s$PD), " (equivalently, ", fmt(s$slope_Hz_per_SD_dNA), " Hz per SD of negative-affect change). ",
  "[INTERPRETARE secondo l'esito: se r e' positivo e CrI esclude 0 -> la risposta vocale traccia quella affettiva; ",
  "se r e' debole/nullo -> coerente con la dissociazione soggettivo/fisiologico, e attenuato dall'inaffidabilita' dei change-score.] ",
  "This analysis is exploratory: change scores from three waves are measured with limited reliability, which attenuates the coupling.\n"
)
writeLines(manuscript_text, file.path(out_dir, "coupling_text.txt"))
cat("\n", manuscript_text, "\n")

saveRDS(list(coup = coup, coupling_all = coupling_all, stress = stress, recovery = recovery),
        file.path(out_dir, "models", "coupling_bundle.rds"))
cat("\n=== DONE ===\nOutput in:", out_dir, "\n")
# eof ---
