# SUITE ANALISI FORMANT F2

## PANORAMICA

Questa suite esegue l'analisi completa dei formant F2 per il tuo manoscritto stress-voice-personality.

**Pipeline completa**:
1. Diagnostica outliers
2. Winsorizzazione (se necessaria)
3. Calcolo prior informativi
4. Test modello singolo
5. Analisi completa (6 modelli)
6. Estrazione risultati

---

## FILE DELLA SUITE

```
SUITE_FORMANT/
├── 00_master_script.R              ← ESEGUI QUESTO
├── 01_diagnostica_outliers.R
├── 02_winsorizzazione.R
├── 03_prepara_prior.R
├── 04_test_modello.R
├── 05_analisi_formant_completa.R
├── 06_estrai_risultati.R
└── README_SUITE.md                 ← QUESTO FILE
```

---

## COME USARE

### OPZIONE A: Automatica (Raccomandata)

Esegui tutto in sequenza con un comando:

```r
source("SUITE_FORMANT/00_master_script.R")
```

Lo script:
- Esegue automaticamente step 1-4
- Ti chiede conferma prima dello step 5 (analisi completa, 2-4 ore)
- Gestisce automaticamente decisioni (winsorizzazione sì/no)
- Verifica convergenza a ogni step

### OPZIONE B: Step-by-step (Debug)

Per debugging o controllo dettagliato:

```r
# Step 1: Diagnostica
source("SUITE_FORMANT/01_diagnostica_outliers.R")
# → Guarda figures/01_outliers_boxplot.pdf
# → Leggi results/f2_outlier_summary.csv

# Step 2: Winsorizzazione (solo se % > 2%)
source("SUITE_FORMANT/02_winsorizzazione.R")

# Step 3: Prior
source("SUITE_FORMANT/03_prepara_prior.R")

# Step 4: Test (10 min)
source("SUITE_FORMANT/04_test_modello.R")
# → VERIFICA figures/04_prior_check.pdf PRIMA di procedere!

# Step 5: Analisi completa (2-4 ore)
source("SUITE_FORMANT/05_analisi_formant_completa.R")

# Step 6: Risultati
source("SUITE_FORMANT/06_estrai_risultati.R")
```

---

## OUTPUT PRODOTTI

### Dopo Step 1-3:
```
results/
├── f2_outlier_summary.csv         # % outliers per vocale
├── df_analysis_winsorized.rds     # Dataset winsorizzato (se applicato)
└── 03_prior_parameters.csv        # Parametri prior calcolati

figures/
└── 01_outliers_boxplot.pdf        # Boxplot diagnostico
```

### Dopo Step 4:
```
results/
└── 04_test_results.csv            # Risultati test model

figures/
├── 04_prior_check.pdf             # ← CONTROLLA QUESTO!
└── 04_posterior_check.pdf

models/
└── 04_test_f2_mean_a.rds
```

### Dopo Step 5-6:
```
results/
├── 05_formant_moderation_results.csv      # Tutti i risultati
├── 05_formant_significant_effects.csv     # Solo significativi ← IMPORTANTE
└── 06_convergence_summary.csv             # Diagnostica

models/
├── 05_all_formant_models.rds              # Lista tutti modelli
├── m_f2_mean_a.rds
├── m_f2_mean_i.rds
├── m_f2_mean_u.rds
├── m_f2_std_a.rds
├── m_f2_std_i.rds
└── m_f2_std_u.rds
```

---

## CHECKPOINT CRITICI

### ⚠️ STOP 1: Dopo diagnostica outliers

**CONTROLLA**:
- `results/f2_outlier_summary.csv`
- `figures/01_outliers_boxplot.pdf`

**DOMANDA**: % outliers > 2%?
- **SÌ**: Winsorizzazione verrà applicata automaticamente
- **NO**: Si procede con dati originali

### ⚠️ STOP 2: Dopo prior predictive check

**CONTROLLA**:
- `figures/04_prior_check.pdf`

**DOMANDA**: yrep (colore) sovrapposto a y (nero)?
- **SÌ**: Prior OK, procedi
- **NO**: FERMA! Prior sbagliati, debug necessario

**Come dovrebbe essere**:
- ✓ yrep centrato ~1200 Hz per /a/
- ✓ Range yrep simile a y (800-1600 Hz)
- ✗ yrep da -2000 a +3000 Hz = ERRORE!

### ⚠️ STOP 3: Dopo test convergenza

**CONTROLLA**:
- Console output: "CONVERGENZA OK" o "PROBLEMI"
- `results/04_test_results.csv`

**DOMANDA**: Test convergito (Rhat < 1.01)?
- **SÌ**: Procedi con analisi completa
- **NO**: FERMA! Fix convergenza prima

---

## DEBUGGING

### Problema: Prior check mostra yrep lontano da y

**Causa**: Prior troppo vaghi o centrati male

**Fix**:
1. Apri `04_test_modello.R`
2. Trova la sezione prior:
   ```r
   priors <- c(
     prior(normal(1205, 585), class = "Intercept"),
     prior(normal(0, 100), class = "b"),  # ← Verifica questo
     ...
   )
   ```
3. Verifica che `normal(0, 100)` NON sia `normal(0, 1000)`
4. Ri-esegui step 4

### Problema: Test non converge (Rhat > 1.01)

**Fix**:
1. Apri `04_test_modello.R`
2. Aumenta `adapt_delta`:
   ```r
   control = list(adapt_delta = 0.99)  # era 0.95
   ```
3. Aumenta `iter`:
   ```r
   iter = 4000  # era 2000
   ```
4. Ri-esegui step 4

### Problema: Analisi completa troppo lenta

**Fix**:
- Usa `chains = 2` invece di `4` in `05_analisi_formant_completa.R`
- Riduci `iter = 2000` invece di `4000`
- Esegui solo alcuni modelli (commenta gli altri nel loop)

---

## STIMA TEMPI

| Step | Tempo | Note |
|------|-------|------|
| 1. Diagnostica | 1 min | Automatico |
| 2. Winsorizzazione | 1 min | Se necessario |
| 3. Prior | 30 sec | Automatico |
| 4. Test | 10 min | **VERIFICA OUTPUT** |
| 5. Analisi completa | 2-4 ore | 6 modelli × 20-40 min |
| 6. Estrai risultati | 1 min | Automatico |
| **TOTALE** | **2.5-4.5 ore** | |

---

## VERIFICA FINALE

Prima di usare i risultati per il manoscritto:

```r
# Carica risultati significativi
results <- read_csv("results/05_formant_significant_effects.csv")

# Filtra interazioni chiave
key_effects <- results %>%
  filter(str_detect(parameter, "psychoticism|negative_affectivity|detachment"))

print(key_effects)
```

**Confronta con abstract**:
- Psychoticism × Stress → F2 variability: β = ?
- Negative Affectivity → F2 stability: β = ?

Se valori molto diversi da abstract, abstract va aggiornato!

---

## MODIFICHE COMUNI

### Cambiare soglia winsorizzazione

Apri `02_winsorizzazione.R`, trova:
```r
winsorize <- function(x, probs = c(0.025, 0.975))
```

Cambia in:
```r
winsorize <- function(x, probs = c(0.01, 0.99))  # Più conservativo
```

### Usare modelli Student-t (robusti)

Apri `04_test_modello.R` e `05_analisi_formant_completa.R`, trova:
```r
family = gaussian()
```

Cambia in:
```r
family = student()
```

### Cambiare numero chains/iter

Apri `05_analisi_formant_completa.R`, trova:
```r
chains = 4,
iter = 4000,
```

Cambia secondo necessità.

---

## SUPPORTO

Se incontri problemi:
1. Controlla quale step ha fallito
2. Leggi console output per errori
3. Verifica file prodotti esistano
4. Esegui solo quello step in isolamento
5. Contatta con errore specifico

---

## WORKFLOW CONSIGLIATO

**Prima esecuzione**:
1. Esegui `source("SUITE_FORMANT/00_master_script.R")`
2. Quando chiede conferma per step 5, rispondi "n"
3. Verifica risultati step 1-4
4. Se tutto OK, esegui `source("SUITE_FORMANT/05_analisi_formant_completa.R")`
5. Poi `source("SUITE_FORMANT/06_estrai_risultati.R")`

**Debug**:
- Esegui step singoli con `source("SUITE_FORMANT/0X_script.R")`
- Modifica solo lo script che ha problemi
- Ri-esegui da quello step in poi

---

## NOTA IMPORTANTE

Tutti gli script usano **automaticamente** il dataset winsorizzato se esiste:
```r
if (file.exists("results/df_analysis_winsorized.rds")) {
  df_analysis <- readRDS("results/df_analysis_winsorized.rds")
} else {
  df_analysis <- readRDS("results/df_analysis.rds")
}
```

Non serve modificare manualmente!
