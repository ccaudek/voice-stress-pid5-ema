# Workflow per Analisi F0-PID5 con Modelli Migliorati

## Panoramica

Questa directory contiene gli script aggiornati per l'analisi LOO comparison dei modelli F0-PID5, usando versioni **migliorate** dei modelli Stan con non-centered parametrization.

## Struttura File

### File Stan (in `stan/followup/`)
1. `pid5_baseline_moderation_improved.stan` - Modello Baseline
2. `f0mean_pid5_moderation_improved.stan` - Modello EMA
3. `pid5_ema_plus_baseline_moderation_improved.stan` - Modello Combined

### Script R (in ordine di esecuzione)
0. `00_diagnostic_precheck.R` - Test veloce pre-run
1. `01_loo_comparison_f0_pid5_improved.R` - Analisi principale
2. `02_create_loo_figures_improved.R` - Generazione figure

## Workflow Consigliato

### Step 0: Pre-check Diagnostico (OPZIONALE ma consigliato)

Prima di lanciare il run completo, testa i modelli con iterazioni minime:

```r
source("00_diagnostic_precheck.R")
```

**Cosa fa:**
- Compila i tre modelli
- Esegue fit velocissimi (100 warmup + 100 sampling)
- Controlla divergenze, Rhat, e problemi evidenti
- Ti dice se è sicuro procedere

**Output:**
- Tabella riassuntiva con status di ogni modello
- Raccomandazioni per settings del run finale

**Quando saltarlo:**
- Se hai già testato i modelli
- Se sei sicuro che i dati siano corretti

---

### Step 1: Analisi Principale

Questo è lo script principale che fa tutto il lavoro pesante:

```r
source("01_loo_comparison_f0_pid5_improved.R")
```

**Cosa fa:**
1. Carica e prepara i dati (voice, EMA, baseline PID-5)
2. Identifica soggetti comuni a tutti i dataset
3. Prepara stan_data per i tre modelli
4. Compila i modelli Stan improved
5. Fitta i modelli con settings appropriati
6. Calcola LOO-CV e confronta i modelli
7. Estrae effetti di moderazione
8. Salva tutti i risultati

**Settings di default:**
```r
n_chains <- 4
n_warmup <- 2000
n_sampling <- 6000
adapt_delta <- 0.995  # Ridotto da 0.999 (NCP needs less!)
max_treedepth <- 15   # Ridotto da 17
```

**Output salvati in `results/followup/`:**
- `fit_f0_ema_improved.rds`
- `fit_f0_baseline_improved.rds`
- `fit_f0_combined_improved.rds`
- `loo_f0_ema_improved.rds`
- `loo_f0_baseline_improved.rds`
- `loo_f0_combined_improved.rds`
- `loo_comparison_f0_improved.csv`
- `moderation_comparison_improved.csv`

**Tempo stimato:** 30-60 minuti (dipende dal computer)

---

### Step 2: Generazione Figure

Dopo che l'analisi principale è completa:

```r
source("02_create_loo_figures_improved.R")
```

**Cosa fa:**
1. Carica i risultati da Step 1
2. Crea 4 figure di alta qualità:
   - **Figure 1**: LOO comparison (ELPD plot)
   - **Figure 2**: Precision comparison (CrI widths)
   - **Figure 3**: Negative Affectivity posteriors
   - **Figure 4**: Pareto k diagnostics
3. Genera tabelle riassuntive
4. Confronta con risultati originali (se disponibili)

**Output salvati in `results/followup/`:**
- `figure1_loo_comparison_improved.png/pdf`
- `figure2_precision_comparison_improved.png/pdf`
- `figure3_na_posteriors_improved.png/pdf`
- `figure4_pareto_diagnostics_improved.png/pdf`
- `precision_improvement_summary_improved.csv`
- `pareto_k_summary_improved.csv`

---

## Modifiche Rispetto ai Modelli Originali

### 1. Non-Centered Parametrization (NCP)

**Prima (centered):**
```stan
parameters {
  vector[N_subj] u0;
  real<lower=0> tau0;
}
model {
  u0 ~ normal(0, tau0);
}
```

**Dopo (non-centered):**
```stan
parameters {
  matrix[N_subj, 3] z_u;
  vector<lower=0>[3] tau;
}
transformed parameters {
  matrix[N_subj, 3] u;
  for (i in 1:N_subj) {
    u[i, 1] = z_u[i, 1] * tau[1];  // u0 = z * tau
  }
}
model {
  to_vector(z_u) ~ std_normal();
  tau ~ exponential(0.5);
}
```

### 2. Prior Migliorati

- Scale parameters: `exponential()` invece di `half-normal()`
- Moderazioni: `normal(0, 5)` invece di `normal(0, 10)`

### 3. Likelihood Vettorizzato

Più efficiente, meno overhead.

---

## Risultati Attesi

Basandoci sui test preliminari, dovresti vedere:

### Convergenza
- ✓ **0 divergenze** (o quasi)
- ✓ **Rhat < 1.01** per tutti i parametri
- ✓ **ESS > 400** per catena

### Pareto k Diagnostics
- **Baseline**: 62% → **~77%** good
- **EMA**: 77.7% → **~78-85%** good (già era buono)
- **Combined**: 44% → **~67%** good (MIGLIORAMENTO ENORME)

### Model Comparison
```
EMA      = migliore (baseline)
Combined ≈ EMA (differenza < 2 SE)
Baseline = leggermente peggiore
```

---

## Troubleshooting

### Problema: "Stan file not found"
**Soluzione:** Assicurati che i file .stan siano in `stan/followup/`

### Problema: "Missing required data objects"
**Soluzione:** Se usi `00_diagnostic_precheck.R`, devi prima preparare i dati. Puoi:
1. Eseguire solo le sezioni 1-4 di `01_loo_comparison...R`
2. Oppure saltare il pre-check e andare direttamente a Step 1

### Problema: Ancora divergenze
**Soluzione:** 
1. Aumenta `adapt_delta` a 0.998
2. Aumenta `n_warmup` a 3000
3. Controlla per outlier nei dati

### Problema: Pareto k ancora problematici
**Opzioni:**
1. Accettare 3-4% di bad k (è normale con dati reali)
2. Usare k-fold CV invece di LOO:
   ```r
   kfold_result <- fit$kfold(K = 10)
   ```
3. Investigare osservazioni con k > 0.7

---

## Note Importanti

### File Naming Convention
Tutti i file di output hanno suffisso `_improved` per distinguerli dai risultati originali.

### Caching dei Fit
Gli script controllano se i fit esistono già su disco. Per re-fittare:
```r
file.remove("results/followup/fit_f0_ema_improved.rds")
# poi ri-esegui lo script
```

### Seed Consistency
Tutti i fit usano `seed = 123` per riproducibilità.

---

## Confronto con Modelli Originali

Se hai già risultati con i modelli originali in `results/followup/`, lo script 02 li confronterà automaticamente.

**Metriche confrontate:**
- Numero di osservazioni con good/bad Pareto k
- Miglioramento percentuale

---

## Prossimi Passi

Dopo aver eseguito questi script:

1. **Verifica convergenza**: Controlla che tutti i modelli abbiano Rhat < 1.01
2. **Esamina figure**: Le figure in `results/followup/` sono pronte per pubblicazione
3. **Scrivi risultati**: Usa le tabelle CSV per riportare risultati nel manoscritto
4. **Sensitivity analysis** (opzionale): Rimuovi outlier con k > 0.7 e ri-fitta

---

## Contatti / Support

Se hai problemi:
1. Controlla i messaggi di errore nella console
2. Verifica che tutti i path ai file siano corretti
3. Assicurati di avere tutte le librerie installate:
   ```r
   install.packages(c("tidyverse", "cmdstanr", "posterior", "loo", 
                      "ggdist", "patchwork", "bayesplot"))
   ```

---

**Buon lavoro!** 🎯
