# WORKFLOW FINALE AGGIORNATO - ANALISI FORMANT

## MODIFICHE IMPORTANTI

Grazie alla tua ottima osservazione sulle code lunghe nelle distribuzioni F2, ho aggiunto **2 nuovi step** per gestire outliers PRIMA delle analisi.

## WORKFLOW COMPLETO (AGGIORNATO)

### 📊 STEP 1: Verifica Struttura Dati (5 min)
```r
source("script_verifica_dati.R")
```
Controlla dimensioni, missing, variabili disponibili.

### 📈 STEP 2: Statistiche Descrittive (2 min) 
```r
source("script_prior_informativi.R")
```
**Output**: Distribuzioni F2 per vocale, statistiche per prior.

### 🔍 STEP 3: **NUOVO** - Diagnostica Outliers (5 min)
```r
source("script_diagnostica_outliers.R")
```

**Cosa fa**:
- Identifica outliers con metodo 3×IQR
- Crea boxplot e histogrammi
- Identifica partecipanti con outliers multipli
- Suggerisce strategia di gestione

**Output critico**:
- `f2_outliers_boxplot.pdf` ← **GUARDA QUESTO**
- `f2_outliers_histograms.pdf` ← **E QUESTO**
- Console: % outliers per vocale

**Interpreta**:
- **< 2% outliers**: Procedi senza winsorizzazione (STEP 5)
- **2-5% outliers**: Winsorizza (STEP 4)
- **> 5% outliers**: Winsorizza E verifica manualmente alcuni casi

### 🔧 STEP 4: **NUOVO** - Winsorizzazione (5 min) [SE NECESSARIO]
```r
source("script_winsorizzazione.R")
```

**Quando usare**:
- Se STEP 3 mostra > 2% outliers
- Se vedi valori chiaramente impossibili nei grafici
- Se stesso partecipante ha outliers ripetuti

**Output**:
- `df_analysis_winsorized.rds` ← **USA QUESTO per analisi**
- Grafici before/after
- Summary winsorizzazione

**Poi negli script successivi, usa**:
```r
# Invece di:
df_analysis <- readRDS("results/df_analysis.rds")

# Usa:
df_analysis <- readRDS("results/df_analysis_winsorized.rds")
```

### 🧪 STEP 5: Test con Prior Informativi (15 min)
```r
source("script_test_PRIOR_INFORMATIVI.R")
```

**IMPORTANTE**: Modifica lo script per usare dataset appropriato:
```r
# All'inizio dello script, scegli quale dataset usare:

# Se NON hai winsorizzato:
df_analysis <- readRDS("results/df_analysis.rds")

# Se HAI winsorizzato:
df_analysis <- readRDS("results/df_analysis_winsorized.rds")
```

**Prior predictive check**:
- Ora dovrebbe essere buono (yrep centrato su 1200 Hz per /a/)
- Se ancora problematico, fermati e contattami

**Verifica convergenza**:
- Max Rhat < 1.01 ✓
- Min ESS > 100 ✓
- Posterior predictive check buono ✓

### 🚀 STEP 6: Analisi Completa (2-4 ore)
```r
# Modifica script_formant_corretto.R per usare dataset appropriato
# Poi esegui:
source("script_formant_corretto.R")
```

**IMPORTANTE**: Prima di eseguire, apri `script_formant_corretto.R` e cambia la prima riga se hai winsorizzato:
```r
# Trova questa riga:
df_analysis <- readRDS("results/df_analysis.rds")

# Se hai winsorizzato, cambiala in:
df_analysis <- readRDS("results/df_analysis_winsorized.rds")
```

---

## DECISIONE TREE: WINSORIZZARE O NO?

```
Esegui script_diagnostica_outliers.R
         |
         v
    Guarda % outliers
         |
    _____|_____
   |           |
< 2%        > 2%
   |           |
   v           v
Procedi    Winsorizza
senza          |
              v
    Usa df_analysis_winsorized.rds
    per tutte le analisi successive
```

### Indicatori per WINSORIZZARE:

✅ **SÌ, winsorizza se**:
- % outliers > 2%
- Valori impossibili (es. F2 < 500 Hz per /i/)
- Stesso ID outlier su multiple vocali/timepoints
- Code lunghe asimmetriche nel plot distribuzione

❌ **NO, non serve se**:
- % outliers < 2%
- Valori estremi ma biologicamente plausibili
- Distribuzioni continuano smooth fino agli estremi
- Prior informativi + modelli robusti gestiranno

---

## CHECKLIST PRE-ANALISI

Prima di lanciare analisi formant complete:

- [ ] Verificato struttura dati (STEP 1)
- [ ] Estratte statistiche per prior (STEP 2)
- [ ] Diagnosticato outliers (STEP 3)
- [ ] Deciso su winsorizzazione (STEP 4 se necessario)
- [ ] Testato pipeline con prior informativi (STEP 5)
- [ ] Prior predictive check ragionevole
- [ ] Convergenza test OK (Rhat < 1.01)
- [ ] Modificato script finale per usare dataset corretto
- [ ] Preparato tè/caffè per 2-4 ore di fitting 😊

---

## PER IL MANOSCRITTO

### Se HAI winsorizzato:

Aggiungi nel **Methods - Data Quality**:

```
Outlier Management

To reduce the influence of extreme values resulting from potential 
formant tracking errors while preserving sample size and temporal 
structure, we applied winsorization at the 2.5th and 97.5th 
percentiles for each vowel separately. This conservative threshold 
affected X.X% of observations (see Supplementary Table SX), primarily 
reflecting implausibly extreme values that likely resulted from 
algorithmic misidentification of formant peaks. Winsorization replaces 
extreme values with the nearest threshold value, maintaining data 
structure essential for multilevel modeling while limiting leverage 
of potentially erroneous estimates.
```

### Se NON hai winsorizzato:

Non serve menzionare! Ma se reviewer chiede:

```
Visual inspection of formant distributions revealed minimal extreme 
values (<2% of observations), which were retained in analyses. 
Bayesian estimation with weakly informative priors is naturally 
robust to such modest outlier influence.
```

---

## TROUBLESHOOTING

### "Winsorizzazione ha cambiato poco le statistiche"
✓ **BUONO!** Significa outliers non influenti. Procedi con fiducia.

### "Winsorizzazione ha cambiato molto (>10%)"
⚠️ **ATTENZIONE**: Outliers erano molto influenti. Considera anche:
- Modelli Student-t (più robusti)
- Sensitivity analysis (confronta con/senza winsorizzazione)
- Verifica manuale alcuni casi estremi

### "Dopo winsorizzazione prior predictive check ancora male"
❌ **PROBLEMA**: Prior ancora sbagliati. Opzioni:
1. Ricalcola prior usando statistiche DOPO winsorizzazione
2. Prior ancora più informativi
3. Contattami per debugging

### "% outliers molto diverso tra vocali"
ℹ️ **NORMALE**: /i/ spesso ha più outliers (range più ampio). Winsorizza separatamente per vocale (script già fa questo).

---

## RIEPILOGO FILE PRODOTTI

Dopo STEP 1-5 avrai:

```
results/
├── df_analysis.rds                        # Originale
├── df_analysis_winsorized.rds            # Winsorizzato (se usato)
├── f2_descriptive_stats.csv              # Statistiche descrittive
├── f2_outlier_summary.csv                # Summary outliers
├── f2_outliers_identified.csv            # Lista outliers
├── winsorization_summary.csv             # Summary winsorizzazione
└── test_f2_mean_a_informative_results.csv # Test model results

figures/
├── f2_distributions_by_vowel.pdf         # Distribuzioni originali
├── f2_outliers_boxplot.pdf               # Outliers evidenziati
├── f2_outliers_histograms.pdf            # Histogrammi con soglie
├── f2_temporal_trajectories.pdf          # Traiettorie individuali
├── winsorization_comparison_*.pdf        # Before/after per vocale
├── winsorization_boxplot_comparison.pdf  # Before/after generale
├── prior_predictive_informative_f2.pdf   # Prior check
└── posterior_predictive_informative_f2.pdf # Posterior check
```

---

## STIMA TEMPI TOTALE

| Step | Tempo | Descrizione |
|------|-------|-------------|
| 1. Verifica dati | 5 min | Automatico |
| 2. Statistiche | 2 min | Automatico |
| 3. Diagnostica outliers | 5 min | Automatico |
| 4. Winsorizzazione | 5 min | Se necessario |
| 5. Test prior informativi | 15 min | Fitting |
| 6. Analisi completa | 2-4 ore | 6 modelli |
| **TOTALE** | **2.5-4.5 ore** | Incluso setup |

---

## DOMANDE FREQUENTI

**Q: Devo rifare anche i modelli F0?**
A: Se i modelli F0 hanno già convergito bene, non serve. Gli outliers F2 sono problema separato.

**Q: Posso winsorizzare dopo il test invece che prima?**
A: No, meglio prima. Altrimenti test non è rappresentativo dell'analisi finale.

**Q: E se non voglio winsorizzare ma voglio modelli robusti?**
A: Cambia `family = gaussian()` in `family = student()` negli script brms. Ma winsorizzazione è più semplice.

**Q: Devo riportare quali osservazioni ho winsorizzato?**
A: No, basta riportare % totale e range soglie nel Methods. Lista completa va in Supplementary se reviewer chiede.

---

**PRONTO PER INIZIARE**:

1. Conferma che hai eseguito `script_prior_informativi.R`
2. Esegui `script_diagnostica_outliers.R`
3. Guarda i plots
4. Decidi su winsorizzazione
5. Procedi con test
6. Lancia analisi completa

Fammi sapere:
- % outliers trovati
- Se winsorizzato o no
- Come va il prior predictive check con prior informativi
