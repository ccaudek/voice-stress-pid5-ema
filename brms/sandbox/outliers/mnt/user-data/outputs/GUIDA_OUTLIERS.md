# GUIDA: GESTIONE OUTLIERS F2

## HAI RAGIONE! 🎯

La tua osservazione sulle code lunghe nelle distribuzioni F2 è ECCELLENTE. Quelle code potrebbero essere:

1. **Errori di tracking formant** (algoritmo ha preso F3 invece di F2)
2. **Coarticolazione estrema** (influenza fonemi adiacenti)
3. **Qualità voce compromessa** (voce breathy, creaky)
4. **Variabilità biologica reale** (ma improbabile se molto estrema)

## COSA SONO OUTLIERS PER FORMANT?

### Outliers tipici per F2:

| Vocale | Range normale | Outliers possibili |
|--------|---------------|-------------------|
| /a/ | 900-1500 Hz | <700 o >1800 Hz |
| /i/ | 1800-2600 Hz | <1400 o >2800 Hz |
| /u/ | 700-1300 Hz | <500 o >1600 Hz |

**Cause comuni**:
- Tracking F3 invece di F2 (salta una formante)
- Tracking F1 invece di F2 (scende una formante)
- Voice quality estrema (molto breathy o harsh)
- Errore segmentazione audio

## WORKFLOW RACCOMANDATO

### STEP 1: Diagnostica Outliers (5 min)

```r
source("script_diagnostica_outliers.R")
```

**Output**:
1. Console: Statistiche outliers per ogni vocale
2. `f2_outliers_boxplot.pdf` - Boxplot con outliers evidenziati
3. `f2_temporal_trajectories.pdf` - Traiettorie individuali
4. `f2_outliers_histograms.pdf` - Histogrammi con soglie
5. `f2_outlier_summary.csv` - Tabella riassuntiva
6. `f2_outliers_identified.csv` - Lista completa outliers

**Criteri outlier**: Usa **3×IQR** (più conservativo del classico 1.5×IQR)

### STEP 2: Verifica Visiva

Guarda attentamente i grafici:

**✓ OUTLIERS VERI**:
- Valori isolati molto lontani dalla massa
- Stesso partecipante ha outliers su più vocali
- Valori impossibili (es. F2 < 500 Hz per /i/)

**? OUTLIERS DUBBI**:
- Valori estremi ma in continuità con distribuzione
- Solo una vocale per quel partecipante
- Valori biologicamente plausibili ma rari

**✗ NON OUTLIERS**:
- Code lunghe ma continue
- Valori estremi ma in range fisiologico
- Molti partecipanti in quella regione

### STEP 3: Decisione

**OPZIONE A: Winsorizzazione (RACCOMANDATA)**

```r
source("script_winsorizzazione.R")
```

**Pro**:
- ✅ Mantiene dimensione campione (no perdita power)
- ✅ Riduce influenza outliers senza eliminarli
- ✅ Standard in analisi robuste
- ✅ Facilmente riportabile nel Methods

**Come funziona**:
- Sostituisce valori < 2.5° percentile con 2.5° percentile
- Sostituisce valori > 97.5° percentile con 97.5° percentile
- Es: Se 97.5° percentile è 1650 Hz, tutti i valori > 1650 diventano 1650

**Output**:
- `df_analysis_winsorized.rds` - Dataset da usare per analisi
- Grafici before/after comparativi
- Summary con % dati winsorizzati

**OPZIONE B: Modelli Robusti (ALTERNATIVA)**

Invece di winsorizzazione, usa famiglia **Student-t** invece di Gaussian:

```r
# Nei modelli brms, cambia:
family = gaussian()
# in:
family = student()  # Automaticamente più robusto a outliers
```

**Pro**:
- ✅ Non modifica dati
- ✅ Statisticamente robusto
- ✅ Heavier tails gestiscono outliers naturalmente

**Con**:
- ⚠ Stima extra parametro (df)
- ⚠ Convergenza può essere più lenta
- ⚠ Meno standard per formant analysis

**OPZIONE C: Rimozione Outliers (NON RACCOMANDATA)**

```r
# NON FARE QUESTO salvo casi estremi
df_clean <- df_analysis %>%
  filter(between(f2_mean_a, Q1_a - 3*IQR_a, Q3_a + 3*IQR_a))
```

**Contro**:
- ❌ Perde osservazioni (riduce power)
- ❌ Perde informazione temporale (se rimuovi un timepoint)
- ❌ Bias selection (rimuovi sistematicamente estremi)
- ❌ Problemi multilevel (sbilanciamento soggetti)

**Solo se**: Outlier è chiaramente errore tecnico verificato manualmente

## RACCOMANDAZIONE SPECIFICA

Basandomi sul tuo plot:

### 1. Prima esegui diagnostica:
```r
source("script_diagnostica_outliers.R")
```

Guarda:
- % outliers per vocale
- Se stesso ID ripetutamente outlier
- Pattern temporali (sempre baseline? sempre pre-exam?)

### 2. Se % outliers < 5%:
```r
source("script_winsorizzazione.R")
```

Winsorizza conservativamente e usa:
```r
df_analysis <- readRDS("results/df_analysis_winsorized.rds")
```

### 3. Se % outliers > 5%:
Prima verifica manualmente con spectrogrammi alcuni casi estremi. Potrebbero essere errori sistematici di tracking che richiedono correzione manuale.

## PER IL MANOSCRITTO

### Methods - Data Quality Control

**Se usi winsorizzazione**:

```
Acoustic Feature Processing and Outlier Management

F2 values were inspected for potential tracking errors resulting from 
algorithmic misidentification of formant peaks. To reduce the influence 
of extreme values while preserving sample size and temporal structure, 
we applied winsorization at the 2.5th and 97.5th percentiles for each 
vowel separately. This conservative threshold (compared to the standard 
1.5×IQR rule) resulted in winsorization of X.X% of observations 
(Table SX). This approach maintains data structure essential for 
multilevel modeling while limiting leverage of potentially erroneous 
formant estimates.
```

**Se usi Student-t**:

```
To account for potential outliers in formant estimation, we specified 
Student-t likelihood distributions in all models rather than Gaussian. 
This robust approach naturally downweights extreme observations through 
heavier distributional tails, avoiding arbitrary thresholding while 
maintaining statistical efficiency (Kruschke, 2015).
```

## QUANDO NON WINSORIZZARE

**Non winsorizzare** se:
- % outliers < 2% → Prior informativi gestiranno
- Outliers sono biologicamente plausibili
- Stai testando ipotesi sulla variabilità estrema

**Sempre winsorizzare** se:
- % outliers > 5%
- Valori chiaramente impossibili (es. F2 < 400 Hz)
- Stesso partecipante outlier ripetutamente
- Tracking errors evidenti

## SENSITIVITY ANALYSIS (OPZIONALE)

Per verificare robustezza risultati:

```r
# 1. Run analisi con dati originali
df_original <- readRDS("results/df_analysis.rds")
m1 <- brm(..., data = df_original)

# 2. Run analisi con dati winsorizzati
df_winsorized <- readRDS("results/df_analysis_winsorized.rds")
m2 <- brm(..., data = df_winsorized)

# 3. Confronta stime
compare_estimates <- bind_rows(
  fixef(m1) %>% as_tibble(rownames = "param") %>% mutate(data = "original"),
  fixef(m2) %>% as_tibble(rownames = "param") %>% mutate(data = "winsorized")
)

# 4. Verifica
# - Se stime molto simili → risultati robusti, outliers non influenti
# - Se stime diverse → outliers guidano risultati, DEVI winsorizzare o Student-t
```

Se fai sensitivity analysis, riporta nel manoscritto:
```
"Results were consistent across original and winsorized data 
(see Supplementary Table SX), indicating findings were not driven 
by extreme values."
```

## DOMANDE FREQUENTI

**Q: Winsorizzare è "truccare" i dati?**
A: No. Winsorizzazione è una tecnica statistica standard per ridurre influenza outliers mantenendo struttura dati. È diverso da rimuovere dati (che è problematico).

**Q: Devo rimuovere outliers prima o dopo centrare variabili?**
A: Prima! Outliers influenzano mean e SD usati per centrare.

**Q: Posso winsorizzare anche F0?**
A: Sì, ma F0 ha outliers diversi (octave jumps, creaky voice). Usa stesso approccio.

**Q: Quanti outliers sono "troppi"?**
A: 
- <2%: OK, procedi
- 2-5%: Winsorizza per sicurezza
- >5%: Verifica tracking errors, probabilmente sistematici

**Q: E se outliers sono tutti in una direzione (es. solo alti)?**
A: Usa winsorizzazione asimmetrica (es. solo 97.5° percentile) ma verifica prima se pattern è reale o artifact.

## ALTERNATIVE AVANZATE

### 1. Modelli Hierarchical con Random Slopes per Outliers
```r
# Permetti a ogni soggetto di avere diversa sensibilità a outliers
family = student(link = "identity", link_sigma = "log", link_nu = "logm1")
```

### 2. Mixture Models
```r
# Modella due popolazioni: dati normali + outliers
family = mixture(gaussian, gaussian)
```

### 3. Quantile Regression
```r
# Modella mediana invece che media (robusto a outliers)
family = asym_laplace()
```

Queste sono avanzate - usa winsorizzazione come approccio standard.

## CONCLUSIONE

1. ✅ La tua osservazione è corretta - ci sono outliers
2. ✅ Esegui `script_diagnostica_outliers.R` per quantificare
3. ✅ Usa `script_winsorizzazione.R` se % > 2%
4. ✅ Riporta trasparentemente nel Methods
5. ✅ Considera sensitivity analysis per robustezza

La winsorizzazione è la strategia più conservativa e facilmente difendibile per formant data con possibili tracking errors.

---

**PROSSIMI PASSI**:
```r
# 1. Diagnostica
source("script_diagnostica_outliers.R")

# 2. Guarda i plots e le statistiche

# 3. Se necessario, winsorizza
source("script_winsorizzazione.R")

# 4. Procedi con analisi usando dati winsorizzati
```
