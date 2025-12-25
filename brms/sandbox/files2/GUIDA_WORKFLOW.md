# GUIDA WORKFLOW ANALISI FORMANT

## PROBLEMA IDENTIFICATO

Il tuo script originale assumeva un formato dati **long** con una colonna `vowel_type`, ma il tuo dataset è in formato **wide** con colonne separate per ogni vocale (es. `f2_mean_a`, `f2_mean_i`, `f2_mean_u`).

Ho creato script corretti che:
1. Gestiscono il formato wide del tuo dataset
2. Creano automaticamente i contrasti `c1_stress` e `c2_recovery` se mancano
3. Convertono i dati in formato long quando necessario
4. Eseguono i modelli Bayesian per tutte le vocali

---

## WORKFLOW RACCOMANDATO

### STEP 1: Verifica Struttura Dati (5 minuti)

**Script**: `script_verifica_dati.R`

**Cosa fa**:
- Verifica dimensioni dataset (423 righe = 141 partecipanti × 3 timepoints)
- Controlla presenza variabili PID5 centrate
- Identifica variabili acustiche disponibili (F0, F2, ecc.)
- Riporta missing data per ogni variabile
- Verifica se i contrasti c1_stress/c2_recovery esistono già

**Come lanciarlo**:
```r
source("script_verifica_dati.R")
```

**Output**:
- `results/00_missing_data_summary.csv`
- Console output con diagnostica completa

**IMPORTANTE**: Controlla che i livelli di `timepoint` siano come atteso:
- "baseline"
- "pre-exam" (o "pre_exam", "preexam")
- "post-exam" (o "post_exam", "postexam")

Se i nomi sono diversi, dovrai modificare lo script per creare i contrasti correttamente.

---

### STEP 2: Test Singolo Modello (10-15 minuti)

**Script**: `script_test_singolo_modello.R`

**Cosa fa**:
- Crea contrasti se non esistono
- Testa UN SOLO modello: `f2_mean` per vowel /a/
- Esegue prior predictive check
- Esegue posterior predictive check
- Diagnostica convergenza (Rhat, ESS)
- Estrae e visualizza risultati chiave

**Come lanciarlo**:
```r
# Assicurati di avere cmdstanr installato
library(cmdstanr)
check_cmdstan_toolchain()

# Se cmdstan non è installato:
# install_cmdstan()

source("script_test_singolo_modello.R")
```

**Output**:
- `models/test_f2_mean_a.rds` (modello fitted)
- `results/test_f2_mean_a_results.csv` (risultati)
- `figures/prior_predictive_f2_mean_a.pdf`
- `figures/posterior_predictive_test.pdf`
- `figures/trace_plot_test.pdf`

**CRITERI DI SUCCESSO**:
- Max Rhat < 1.01
- Min ESS ratio > 0.1
- Trace plots mostrano mixing buono
- Posterior predictive check ragionevole

Se tutto va bene, procedi allo Step 3. Se ci sono problemi di convergenza:
1. Aumenta `adapt_delta` (es. 0.99)
2. Aumenta `max_treedepth` (es. 15)
3. Verifica outliers nei dati
4. Considera prior più informativi se needed

---

### STEP 3: Analisi Completa Formant (2-4 ore)

**Script**: `script_formant_corretto.R`

**Cosa fa**:
- Esegue modelli per TUTTI gli outcomes formant: f2_mean, f2_std
- Per TUTTE le vocali: /a/, /i/, /u/
- Estrae risultati in formato tabellare
- Crea visualizzazioni
- Identifica effetti significativi

**Come lanciarlo**:
```r
# RACCOMANDAZIONE: Lancia in una sessione dedicata o su server
# Questo prenderà 2-4 ore per completare tutti i modelli

source("script_formant_corretto.R")
```

**Output**:
- `models/models_formant_separate.rds` (lista tutti i modelli)
- `models/m_f2_mean_a.rds`, `m_f2_mean_i.rds`, `m_f2_mean_u.rds`
- `models/m_f2_std_a.rds`, `m_f2_std_i.rds`, `m_f2_std_u.rds`
- `results/05_formant_moderation_results.csv` (risultati completi)
- `results/05_formant_significant_effects.csv` (solo significativi)
- `figures/formant_interactions_key.pdf`

**OTTIMIZZAZIONI**:
Se vuoi velocizzare, puoi:
1. Ridurre iter (da 4000 a 2000)
2. Usare solo 2 chains (invece di 4)
3. Lanciare in parallelo su più core/server

---

## CONTROLLI QUALITÀ POST-ANALISI

### 1. Convergenza
```r
# Carica i modelli
models <- readRDS("models/models_formant_separate.rds")

# Verifica convergenza per tutti
convergence_check <- map_dfr(names(models), function(model_name) {
  m <- models[[model_name]]
  tibble(
    model = model_name,
    max_rhat = max(rhat(m), na.rm=TRUE),
    min_ess = min(neff_ratio(m), na.rm=TRUE)
  )
})

# Identifica problemi
convergence_check %>%
  filter(max_rhat > 1.01 | min_ess < 0.1)
```

### 2. Confronto con F0
```r
# Carica risultati F0 e F2
results_f0 <- read_csv("results/02_moderation_results.csv")
results_f2 <- read_csv("results/05_formant_moderation_results.csv")

# Confronta pattern di moderazione
# Es: Negative Affectivity modera F0 ma non F2?
```

### 3. Effetti Attesi
Basandoti sull'abstract, verifica:
- **Psychoticism × Stress → F2 variability**: dovrebbe essere β ≈ -0.28 [-0.55, -0.00]
- **Negative Affectivity → F2 stability**: dovrebbe essere β ≈ -0.16 [-0.27, -0.04]

Se i valori sono molto diversi, potrebbero esserci problemi di:
- Definizione contrasti
- Scaling variabili
- Outliers
- Missing data handling

---

## TROUBLESHOOTING COMUNE

### Problema: "object 'c1_stress' not found"
**Soluzione**: Lo script test crea automaticamente i contrasti. Verifica che i livelli di `timepoint` siano corretti.

### Problema: Convergenza scarsa (Rhat > 1.01)
**Soluzione**:
```r
# Nel modello, aumenta adapt_delta e max_treedepth
control = list(adapt_delta = 0.99, max_treedepth = 15)
```

### Problema: "Chain X rejected Y proposals"
**Soluzione**: Normale se sporadico. Se frequente (>10%), aumenta adapt_delta.

### Problema: ESS molto basso (<100)
**Soluzione**: 
- Aumenta iter (es. 6000)
- Verifica correlazioni posterior tra parametri
- Considera reparametrizzazione (es. non-centered per random effects)

### Problema: Tempo di esecuzione troppo lungo
**Soluzione**:
- Usa `backend = "cmdstanr"` invece di rstan (più veloce)
- Parallelizza chains su più core
- Riduci iter (minimo 2000)
- Considera thinning se ESS è sufficiente

---

## PROSSIMI PASSI DOPO ANALISI

Una volta completate le analisi formant, dovrai:

### 1. Aggiornare Abstract
Sostituisci i valori placeholder con i risultati effettivi:
```
"Psychoticism moderated articulatory instability under stress 
(F2 variability: β=X.XX [X.XX, X.XX])"
```

### 2. Creare Tabelle per Manoscritto
```r
# Tabella S1: Effetti principali stress su formants
results_f2 %>%
  filter(type == "Main", parameter %in% c("b_c1_stress", "b_c2_recovery")) %>%
  select(outcome, vowel, parameter, estimate, ci_lower, ci_upper) %>%
  arrange(outcome, vowel)

# Tabella S2: Interazioni personality × stress su formants
results_f2 %>%
  filter(type == "Interaction", significant == TRUE) %>%
  select(outcome, vowel, parameter, estimate, ci_lower, ci_upper)
```

### 3. Figure per Risultati
- Forest plot interazioni significative
- Conditional effects plots per effetti chiave
- Comparison plot F0 vs F2 moderations

### 4. Temporal Covariation per F2
Se vuoi validare il claim su "within-person lability", devi fare between-within decomposition anche per F2:
```r
# Usa stesso approccio del file 03_temporal_covariation.csv
# Ma per f2_std invece di f0_mean
```

---

## CHECKLIST FINALE

Prima di submission, verifica:

- [ ] Tutti i modelli hanno Rhat < 1.01
- [ ] Tutti i modelli hanno ESS > 100 per parametri chiave
- [ ] Posterior predictive checks ragionevoli
- [ ] Valori in abstract corrispondono ai risultati
- [ ] Effetti "significativi" hanno CI che escludono zero
- [ ] Sample size consistente (n dopo drop_na)
- [ ] Confronto F0 vs F2 chiarisce pattern domain-specific
- [ ] Supplementary materials includono:
  - [ ] Tabelle complete tutti i modelli
  - [ ] Trace plots
  - [ ] Posterior predictive checks
  - [ ] Prior specifications

---

## CONTATTI E SUPPORTO

Se incontri problemi:
1. Verifica output console per messaggi di errore
2. Controlla file log in `models/`
3. Valuta prior più informativi se convergenza problematica
4. Considera simplified models (es. meno random effects) se necessario

Fammi sapere se hai domande o se emergono problemi specifici!
