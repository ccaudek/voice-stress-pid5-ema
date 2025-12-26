# Within-Person Covariation Analysis: Guida Rapida

## Panoramica

Questa analisi esplora la covariazione **within-person** tra fluttuazioni 
momentanee del PID-5 EMA e caratteristiche vocali (F0, NNE) nei 3 timepoint 
di registrazione (baseline, pre-esame, post-esame).

## File Forniti

### Script R
1. **04_within_person_ema_voice_covariation.R**  
   Script principale per analisi esplorativa (correlazioni, grafici)

2. **05_fit_stan_within_person_model.R**  
   Fitting modello Bayesiano gerarchico con Stan (opzionale ma consigliato)

### Modello Stan
3. **within_person_covariation.stan**  
   Modello gerarchico per inferenza robusta

### Documentazione
4. **METHODOLOGY_WITHIN_PERSON.md**  
   Spiegazione dettagliata del razionale metodologico

## Prerequisiti

### Pacchetti R richiesti

```r
# Analisi base
install.packages(c(
  "tidyverse", "readxl", "here", "rio", "lubridate",
  "ggplot2", "corrplot", "jsonlite"
))

# Per modello Stan (opzionale)
install.packages(c("cmdstanr", "posterior", "bayesplot", "loo"))

# Installazione cmdstanr (se necessario)
# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# cmdstanr::install_cmdstan()
```

### Struttura Dati Necessaria

I seguenti file devono esistere (usando i tuoi path attuali):

1. **Dati vocali:**  
   `data/raw/acustic_features/datiacustici/AUDIO.xlsx`
   - Sheet: "BASELINE", "PRE", "POST"

2. **Dati EMA puliti:**  
   `data/processed/ema_plus_scales_cleaned.csv`
   - Output dello script `01_clean_after_merge.R`

3. **Metadati:**  
   `data/raw/meta/all_combined_sex_NEW_1.xlsx`

## Workflow Passo-Passo

### Step 1: Analisi Esplorativa (obbligatorio)

```r
source("04_within_person_ema_voice_covariation.R")
```

**Cosa fa:**
1. Carica dati vocali ed EMA
2. Identifica misurazioni EMA contemporanee ai timepoint vocali
3. Aggrega PID-5 EMA per timepoint
4. Calcola deviazioni within-person per tutti
5. Stima correlazioni within-person
6. Genera grafici esplorativi

**Output directory:** `results/within_person_covariation/`

**Output chiave:**
- `variance_decomposition.csv`: ICC e scomposizione varianza
- `within_person_correlations.csv`: matrice correlazioni completa
- `f0_pid5_within_correlations.csv`: correlazioni per dominio PID-5
- `within_person_correlations.pdf`: heatmap correlazioni
- `wp_scatter_[domain].pdf`: scatter plot per dominio (5 file)
- `wp_deviations_by_timepoint.pdf`: distribuzione deviazioni
- `stan_data_within_person.json`: dati preparati per Stan
- `within_person_bundle.rds`: bundle completo R

**Tempo stimato:** 1-2 minuti

### Step 2: Modello Bayesiano (opzionale ma consigliato)

```r
source("05_fit_stan_within_person_model.R")
```

**Prerequisiti:**
- Aver eseguito Step 1 (genera i dati necessari)
- cmdstanr installato e funzionante

**Cosa fa:**
1. Carica dati preparati da Step 1
2. Compila modello Stan
3. Esegue MCMC (4 chains × 1000 warmup × 1000 sampling)
4. Diagnostici convergenza (Rhat, ESS)
5. Estrazione posterior (coefficienti WP, R²)
6. Posterior predictive checks
7. LOO cross-validation
8. Generazione grafici diagnostici

**Output directory:** `results/within_person_stan/`

**Output chiave:**
- `parameter_summary.csv`: tutti i parametri con diagnostici
- `beta_wp_summary.csv`: coefficienti WP con intervalli credibili
- `r2_within_summary.csv`: varianza spiegata WP
- `trace_plots.pdf`: convergenza chains
- `beta_wp_intervals.pdf`: intervalli credibili coefficienti
- `ppc_density.pdf`, `ppc_intervals.pdf`: predictive checks
- `loo_pareto_k.pdf`: diagnostici LOO-CV
- `fit_within_person.rds`: oggetto fit completo
- `results_bundle.rds`: bundle completo risultati

**Tempo stimato:** 5-15 minuti (dipende da CPU)

## Interpretazione Risultati

### Correlazioni Within-Person

File: `f0_pid5_within_correlations.csv`

```
domain                         cor_f0_within  p_value  n_obs
pid5_negative_affectivity      0.15           0.045    142
pid5_detachment               -0.03           0.714    142
...
```

**Interpretazione:**
- `cor_f0_within > 0`: quando la persona è più alta del suo solito su questo 
  dominio, anche la sua F0 tende ad essere più alta del suo solito
- `p_value < 0.05`: evidenza statistica di associazione
- Attenzione: con solo 3 timepoint per soggetto, la potenza è limitata

### Coefficienti Bayesiani (β_WP)

File: `beta_wp_summary.csv`

```
domain                         mean    q025   q975   prob_pos
pid5_negative_affectivity      5.2     2.1    8.3    0.998
pid5_detachment               -0.8    -3.5    1.9    0.276
...
```

**Interpretazione:**
- `mean`: effetto medio stimato (Hz per 1 SD di aumento WP nel PID-5)
- `q025, q975`: intervallo credibile 95%
- `prob_pos`: probabilità che β > 0

**Esempio pratico:**  
β[Negative Affectivity] = 5.2 [2.1, 8.3]
- In momenti in cui una persona è +1 SD più negativamente affettiva del suo 
  normale, la sua F0 aumenta di ~5 Hz rispetto al suo normale
- 95% CI non include 0 → evidenza robusta
- prob_pos = 0.998 → quasi certezza di effetto positivo

### Varianza Spiegata (R²_within)

File: `r2_within_summary.csv`

```
parameter   mean   median   q025   q975
r2_within   0.12   0.11     0.03   0.23
```

**Interpretazione:**
- ~12% della varianza within-person in F0 è spiegata dalle fluttuazioni WP 
  nei 5 domini PID-5
- Valore basso ma ragionevole dato:
  1. Solo 3 timepoint per soggetto
  2. Molti altri fattori influenzano la voce (situazione, stato fisiologico)
  3. Errore di misurazione

## Diagnostici Convergenza

### Rhat
- **OK:** Rhat < 1.01 per tutti i parametri
- **Warning:** Rhat > 1.01 → possibili problemi convergenza
- **Azione:** aumentare `n_iter_warmup` o `adapt_delta`

### ESS (Effective Sample Size)
- **OK:** ESS_bulk e ESS_tail > 400
- **Warning:** ESS < 100 → campionamento inefficiente
- **Azione:** aumentare `n_iter_sampling` o controllare parametrizzazione

### LOO Pareto k
- **OK:** tutti k < 0.7
- **Warning:** alcuni k > 0.7 → osservazioni influenti
- **Azione:** investigare outlier vocali o EMA

## Troubleshooting

### Errore: "File non trovato"
**Problema:** path dati non corretto  
**Soluzione:** verifica che i file esistano nei percorsi specificati, 
modifica i path in testa agli script se necessario

### Errore: "cmdstanr non trovato"
**Problema:** Stan non installato  
**Soluzione:**
```r
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan()
```

### Warning: "Divergent transitions"
**Problema:** geometria posterior difficile  
**Soluzione:** nello script Stan, aumenta `adapt_delta` a 0.99

### Warning: "Maximum treedepth exceeded"
**Problema:** parametri con correlazioni posteriori alte  
**Soluzione:** aumenta `max_treedepth` a 15

### Rhat > 1.01
**Problema:** chains non convergenti  
**Soluzione:** 
1. Aumenta `n_iter_warmup` a 2000
2. Controlla parametrizzazione del modello
3. Considera priors più informativi

## Personalizzazioni Possibili

### Cambiare outcome vocale

Nello script R base, modifica:

```r
# Invece di F0 mean, usa NNE
df_combined <- df_voice %>%
  left_join(ema_aggregated, by = c("user_id", "timepoint")) %>%
  mutate(
    nne_wp = nne - mean(nne, na.rm = TRUE)  # ← cambia qui
  )
```

E modifica corrispondentemente nel modello Stan.

### Aggiungere predittori

Nel modello Stan, aumenta `D` e fornisci più colonne in `X_wp`:

```r
# Esempio: aggiungi DASS stress
stan_data_wp <- list(
  # ...
  D = length(pid5_ema_vars) + 1,  # +1 per DASS
  X_wp = cbind(X_wp, dass_stress_wp)
)
```

### Cambiare parametri MCMC

Nello script fitting Stan:

```r
n_chains <- 4           # ← numero chains
n_iter_warmup <- 2000   # ← più warmup se non converge
n_iter_sampling <- 2000 # ← più campioni per posterior più stabile
adapt_delta <- 0.99     # ← più alto se divergenze
```

## Contatti e Supporto

Per domande metodologiche:
- Consulta `METHODOLOGY_WITHIN_PERSON.md`
- Riferimenti teorici in fondo al documento

Per problemi tecnici:
- Controlla diagnostici convergenza
- Verifica versioni pacchetti
- Controlla messaggi errore Stan

## Checklist Analisi Completa

- [ ] Eseguito script esplorativo (Step 1)
- [ ] Verificato correlazioni WP in `f0_pid5_within_correlations.csv`
- [ ] Esaminati grafici scatter WP per dominio
- [ ] Controllato ICC e decomposizione varianza
- [ ] (Opzionale) Eseguito modello Stan (Step 2)
- [ ] Verificato convergenza (Rhat < 1.01, ESS > 400)
- [ ] Esaminati coefficienti β_WP con intervalli credibili
- [ ] Controllato PPC plots (distribuzione predittiva)
- [ ] Verificato LOO (Pareto k < 0.7)
- [ ] Documentato risultati e interpretazione
