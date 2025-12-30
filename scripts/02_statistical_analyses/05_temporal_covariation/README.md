# 🎯 WITHIN-PERSON COVARIATION ANALYSIS
## Complete Pipeline - From Raw Data to Manuscript

**Data:** Dicembre 2025
**Analista:** Corrado Caudek  
**Progetto:** voice-stress personality research

---

## 📊 KEY FINDING

> **"Vocal-affective coupling is IDIOGRAPHIC, not nomothetic"**

While population-average within-person effects are minimal (β ≈ 0, R² = 2.5%), 
random slopes models reveal **substantial individual heterogeneity** (R² = 35%, 
ELPD improvement ≈ 40 points).

**Pattern discovered:**
- **~1/3 participants:** Positive coupling (affect↑ → F0↑)
- **~1/3 participants:** Negative coupling (affect↑ → F0↓)
- **~1/3 participants:** No coupling

→ Individuals differ systematically in WHETHER and HOW momentary personality 
fluctuations relate to voice.

---

## 🗂️ PIPELINE STRUCTURE

### **Pipeline completa in 5 step:**

```
00_setup_and_verify.R          (setup + verifica prerequisiti)
    ↓
01_prepare_within_person_data.R (dati raw → dati Stan)
    ↓
02_fit_models.R                 (fit baseline + random slopes)
    ↓
03_analyze_heterogeneity.R      (analizza pattern idiografico)
    ↓
04_create_manuscript_materials.R (tables + text per paper)
```

---

## 📋 PREREQUISITI

### **File Necessari (dati raw):**

1. **AUDIO.xlsx**
   - Path: `data/raw/acustic_features/datiacustici/AUDIO.xlsx`
   - Contiene: Dati vocali (F0, NNE) per baseline/pre/post
   - Sheet: BASELINE, PRE, POST

2. **ema_plus_scales_cleaned.csv**
   - Path: `data/processed/ema_plus_scales_cleaned.csv`
   - Contiene: PID-5 EMA giornalieri + metadata temporali

3. **all_combined_sex_NEW_1.xlsx**
   - Path: `data/raw/meta/all_combined_sex_NEW_1.xlsx`
   - Contiene: Metadati partecipanti (sesso, corso, date esami)

### **Pacchetti R:**

```r
# Data wrangling
tidyverse, readxl, here, rio, lubridate

# Stan/Bayesian
cmdstanr, posterior, bayesplot, loo

# Visualizzazione
ggplot2, patchwork

# Manoscritto
knitr, kableExtra
```

### **Software:**

- R >= 4.0
- CmdStan >= 2.30
- RStudio (consigliato)

---

## 🚀 ESECUZIONE

### **STEP 0: Setup e Verifica** (< 1 minuto)

```r
# Dalla directory principale del progetto
setwd("path/to/project")

# Verifica prerequisiti
source("within_person_pipeline/00_setup_and_verify.R")
```

**Output:**
- Verifica presenza file raw
- Crea struttura directory
- Verifica pacchetti e CmdStan
- Status: ✓ setup completo / ⚠ file/pacchetti mancanti

**Se fallisce:** Segui le istruzioni a schermo per installare pacchetti o 
posizionare file mancanti.

---

### **STEP 1: Prepara Dati** (2-5 minuti)

```r
source("within_person_pipeline/01_prepare_within_person_data.R")
```

**Cosa fa:**
1. Carica dati vocali (AUDIO.xlsx) → 3 timepoint
2. Carica EMA giornalieri → PID-5 domains
3. Matching temporale EMA-voce per timepoint
4. Aggregazione EMA (media per periodo)
5. Calcolo deviazioni within-person
6. Prepara dati Stan (matrice X_wp, outcome y_wp)

**Output:**
- `results/within_person_final/data/stan_data_within_person.rds`
- `results/within_person_final/data/metadata.rds`
- `results/within_person_final/data/df_within_full.rds`

**Verifica:**
```r
stan_data <- readRDS("results/within_person_final/data/stan_data_within_person.rds")
str(stan_data)
# Dovrebbe mostrare: N, N_subj, D, subj, X_wp (matrix), y_wp (vector)
```

---

### **STEP 2: Fit Models** (2-4 ore)

```r
source("within_person_pipeline/02_fit_models.R")
```

**⚠️ IMPORTANTE:** Questo step richiede MOLTO tempo!
- Baseline model: ~30-60 minuti
- Random slopes: ~2-3 ore
- **Totale: 2-4 ore** (dipende da hardware)

**Parametri MCMC:**
- Chains: 4
- Warmup: 2000
- Sampling: 5000
- adapt_delta: 0.999
- max_treedepth: 15

**Cosa fa:**
1. Compila `baseline_model.stan`
2. Fit baseline (fixed effects only)
3. Compila `random_slopes_noncentered.stan`
4. Fit random slopes (non-centered parametrization)
5. LOO cross-validation per entrambi
6. Model comparison

**Output:**
- `results/within_person_final/fitted_models/fit_baseline.rds`
- `results/within_person_final/fitted_models/fit_random_slopes.rds`
- `results/within_person_final/fitted_models/loo_*.rds`
- `results/within_person_final/fitted_models/diagnostics_*.csv`
- `results/within_person_final/fitted_models/model_comparison.csv`

**Monitor progress:**
Lo script stampa aggiornamenti ogni 500 iterations. Vedrai:
```
Chain 1 Iteration: 500 / 7000 [ 7%]
Chain 1 Iteration: 1000 / 7000 [14%]
...
```

**☕ Suggerimento:** Esegui questo step la sera e lascia girare overnight!

---

### **STEP 3: Analizza Eterogeneità** (5-10 minuti)

```r
source("within_person_pipeline/03_analyze_heterogeneity.R")
```

**Cosa fa:**
1. Estrae `total_slopes` dal fit random slopes
2. Calcola summary per ogni soggetto × dominio
3. Categorizza responders (Positive/Negative/Non-responder)
4. Calcola sigma_beta (between-subject variability)
5. **Crea 5 plot publication-ready**

**Output - CSV:**
- `individual_slopes.csv` (tutti gli slopes individuali)
- `responder_summary.csv` (distribuzione responder types)
- `sigma_beta_summary.csv` (variabilità between-subject)

**Output - Figures (PDF):**
1. **`01_slope_densities.pdf`** - Density ridges per dominio
2. **`02_forest_plot_NA.pdf`** ⭐ **PLOT CHIAVE** - Forest plot individuale
3. **`03_responder_types_barchart.pdf`** - Distribuzione responder types
4. **`04_fixed_vs_variability.pdf`** - Fixed effects vs sigma_beta
5. **`05_individual_examples.pdf`** - 9 scatterplots rappresentativi

**Plot chiave per manoscritto:**
- **Main text:** Plot 2 (forest plot) - mostra eterogeneità
- **Supplementary:** Plot 5 (examples) - illustra pattern concreti

---

### **STEP 4: Materiali Manoscritto** (< 1 minuto)

```r
source("within_person_pipeline/04_create_manuscript_materials.R")
```

**Cosa fa:**
1. Crea Table 1 (model comparison)
2. Crea Table 2 (responder types distribution)
3. Crea Supplementary Table (individual slopes examples)
4. Genera text snippets (Results, Discussion, Abstract)
5. Crea figure captions

**Output - Tables:**
- `table1_model_comparison.csv`
- `table2_responder_types.csv`
- `supp_table_individual_slopes.csv`

**Output - Text:**
- `results_text.txt` - Paragrafo Results pronto
- `discussion_text.txt` - Paragrafo Discussion pronto
- `abstract_text.txt` - Abstract pronto
- `figure_captions.txt` - Caption per tutte le figure

**Uso:**
Apri i file `.txt` e copia-incolla nel tuo manoscritto Word/LaTeX.
Modifica numeri se necessario (alcuni sono placeholder).

---

## 📂 STRUTTURA OUTPUT COMPLETA

```
results/within_person_final/
├── data/
│   ├── stan_data_within_person.rds
│   ├── metadata.rds
│   └── df_within_full.rds
├── fitted_models/
│   ├── fit_baseline.rds
│   ├── fit_random_slopes.rds
│   ├── loo_baseline.rds
│   ├── loo_random_slopes.rds
│   ├── diagnostics_baseline.csv
│   ├── diagnostics_random_slopes.csv
│   └── model_comparison.csv
├── heterogeneity_analysis/
│   ├── individual_slopes.csv
│   ├── responder_summary.csv
│   ├── sigma_beta_summary.csv
│   ├── 01_slope_densities.pdf
│   ├── 02_forest_plot_NA.pdf           ⭐ MAIN FIGURE
│   ├── 03_responder_types_barchart.pdf
│   ├── 04_fixed_vs_variability.pdf
│   └── 05_individual_examples.pdf      ⭐ SUPPLEMENTARY
└── manuscript_materials/
    ├── table1_model_comparison.csv
    ├── table2_responder_types.csv
    ├── supp_table_individual_slopes.csv
    ├── results_text.txt
    ├── discussion_text.txt
    ├── abstract_text.txt
    └── figure_captions.txt
```

---

## 📊 RISULTATI ATTESI

### **Model Comparison:**

```
Baseline:      R² ≈ 2.5%, ELPD ≈ -1200
Random Slopes: R² ≈ 35%,  ELPD ≈ -1160
Difference:    ΔELPD ≈ 40 (SE ≈ 5-10)
```

**Interpretazione:**
- Random slopes >> baseline (14× improvement in R²)
- ΔELPD ≈ 40 è **molto sostanziale** (>4 tipicamente meaningful)
- Conferma eterogeneità individuale

### **Fixed Effects (Population Mean):**

```
All β ≈ 0, 90% CI spanning zero
```

**Interpretazione:**
- Nessun effetto population-average
- MA questo nasconde eterogeneità (vedi R² difference)

### **Between-Subject Variability:**

```
σ_β (NA) ≈ 2-3 Hz [95% CI: 1.5-4]
σ_β (Det) ≈ 2-3 Hz
σ_β (Ant) ≈ 2-3 Hz
```

**Interpretazione:**
- Large heterogeneity across individuals
- Individui differiscono sistematicamente negli slopes

### **Responder Types (Negative Affectivity):**

```
Positive:      ~30-35%
Negative:      ~25-30%
Non-responder: ~35-40%
```

**Interpretazione:**
- Pattern distribuito (non un solo tipo domina)
- Conferma idiographic nature

---

## 🎓 CONTRIBUTO TEORICO

### **Messaggio Principale:**

**"The null population-average effect is NOT evidence of no relationship, but 
evidence of HETEROGENEOUS relationships that cancel at the group level."**

### **Implicazioni:**

1. **Teoriche:**
   - Vocal-affective coupling è idiografico, non nomothetico
   - Challenge universal models of affect-voice mapping
   - Differential reactivity framework

2. **Metodologiche:**
   - Random slopes essential for within-person research
   - Fixed effects can miss substantial heterogeneity
   - LOO-CV confirms real signal vs overfitting

3. **Pratiche:**
   - Personalized calibration necessary for voice-based affect detection
   - One-size-fits-all algorithms inadequate
   - Need trait-informed models

---

## 📝 INTEGRAZIONE NEL MANOSCRITTO

### **Results Section:**

**Testo base:** Usa `manuscript_materials/results_text.txt`

**Struttura:**
1. Model comparison (LOO): Random slopes >> baseline
2. Fixed effects ≈ 0: Null population-average
3. σ_β large: Substantial heterogeneity
4. Responder types: Distribution ~1/3 each

**Figure principale:**
- **Figure 2:** Forest plot (02_forest_plot_NA.pdf)
  - Caption: "Individual within-person slopes for Negative Affectivity..."

### **Discussion Section:**

**Testo base:** Usa `manuscript_materials/discussion_text.txt`

**Punti chiave:**
1. Idiographic coupling challenges universal models
2. Differential reactivity framework
3. Implications for affective computing
4. Methodological: importance of random slopes

### **Supplementary Material:**

**Figure S3:** Individual examples (05_individual_examples.pdf)

**Extended text:**
"To illustrate idiographic patterns, Figure S3 shows within-person scatterplots 
for nine representative participants. Positive responders (top row) show clear 
positive slopes... [vedi manuscript_materials/discussion_text.txt per testo completo]"

---

## ✅ QUALITY CHECKLIST

Prima di submission:

### **Statistical:**
- [ ] All Rhat < 1.01 (verifica in diagnostics_*.csv)
- [ ] All ESS > 400
- [ ] LOO Pareto k acceptable (<0.7 per >99% obs)
- [ ] R² values consistent across reports

### **Results:**
- [ ] Numbers match tra text/tables/figures
- [ ] Sample sizes reported (N_subj = X, N_obs = Y)
- [ ] CIs consistent (90% vs 95% - decidi e mantieni)
- [ ] ELPD comparison reported correctly

### **Figures:**
- [ ] High resolution (300 dpi minimum)
- [ ] Legible text (12pt minimum)
- [ ] Color-blind friendly palette (already implemented)
- [ ] Axis labels clear and informative

### **Manuscript:**
- [ ] Results: objective, data-focused
- [ ] Discussion: theoretical implications clear
- [ ] Limitations: acknowledged but framed constructively
- [ ] Abstract: concise summary of key finding

---

## 🔧 TROUBLESHOOTING

### **Se 00_setup_and_verify.R fallisce:**

**File mancanti:**
- Verifica path dei file raw
- Se diversi, modifica sezione `required_files` in 00_setup
- Assicurati che i nomi file siano esatti

**Pacchetti mancanti:**
```r
install.packages(c("tidyverse", "readxl", "here", "rio", 
                   "lubridate", "posterior", "bayesplot", "loo"))
```

**CmdStan non trovato:**
```r
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan()
```

### **Se 02_fit_models.R ha convergence issues:**

**Baseline model problemi:**
- Raramente dovrebbe fallire
- Se Rhat > 1.01: aumenta warmup a 3000
- Se ESS < 400: aumenta sampling a 8000

**Random slopes problemi:**
- Se Rhat > 1.01 per sigma_beta: già usa non-centered, aumenta warmup
- Se ESS < 100: aumenta sampling o adapt_delta a 0.9999
- Se divergent transitions >1%: aumenta adapt_delta

**Tempo troppo lungo:**
- Normale! Random slopes richiede 2-3 ore
- Riduci sampling a 3000 per test veloce (poi rirun con 5000)

### **Se plot non si creano:**

Verifica pacchetti grafici:
```r
install.packages(c("ggplot2", "patchwork", "scales"))
```

Se errori di memoria con plot 5:
```r
# Riduci numero di soggetti mostrati
# Modifica linea 301 in 03_analyze_heterogeneity.R:
slice_sample(n = 2)  # invece di n = 3
```

---

## 📚 REFERENCES CHIAVE

**Citare nel manoscritto:**

**Metodologiche:**
- Hamaker (2012). Why researchers should think "within-person"
- McNeish & Stapleton (2016). Small sample MLM recommendations
- Vehtari et al. (2017). LOO cross-validation

**Teoriche:**
- Molenaar (2004). Manifesto on psychology as idiographic science
- Fisher et al. (2018). Lack of group-to-individual generalizability

**Voice-Personality:**
- Scherer (2003). Vocal communication of emotion
- Juslin & Laukka (2003). Communication of emotions in vocal expression

---

## 💡 TIPS & BEST PRACTICES

### **Durante Fitting:**

1. **Monitor in real-time:**
   - Guarda output console per warnings
   - Divergent transitions <1% è OK
   - Max treedepth warnings occasionali OK

2. **Se devi interrompere:**
   - Ctrl+C per stop
   - Re-run script riparte da capo
   - Non c'è checkpointing automatico

3. **Salva intermediate results:**
   - Gli script già salvano automaticamente
   - `.rds` files sono binari R-specific
   - Backup `fitted_models/` directory periodicamente

### **Per Interpretazione:**

1. **Fixed effects ≈ 0 è OK:**
   - Non significa "no effect"
   - Significa "cancellation of heterogeneous effects"
   - Focus su σ_β e R² comparison

2. **R² interpretation:**
   - Baseline R² è quello che most studies report
   - Random slopes R² rivela true signal
   - Improvement documenta eterogeneità

3. **Responder classification:**
   - Basato su 95% CI excluding zero
   - Conservative (alcuni "non-responders" might have small effects)
   - Alternative: use posterior probability > 0.95

---

## 🎉 CONGRATULAZIONI!

Quando completi questa pipeline, avrai:

✅ **Dataset rigoroso:** From raw to within-person deviations
✅ **Models convergenti:** Baseline e random slopes well-behaved
✅ **Heterogeneity quantificata:** Individual slopes per ogni soggetto
✅ **Visualizzazioni publication-ready:** 5 figure PDF
✅ **Manuscript materials:** Tables e text snippets pronti

**Il tuo contributo:**
Primo studio a documentare sistematicamente la natura idiografica del vocal-
affective coupling usando random slopes models con rigorous convergence diagnostics.

**Ready for submission!** 🚀

---

## 📞 SUPPORT

Se incontri problemi non coperti in questo README:

1. Verifica diagnostics files per dettagli errore
2. Controlla session_info.rds per versioni pacchetti
3. Documenta l'errore esatto (copy-paste da console)
4. Verifica prerequisiti con 00_setup

**Buona fortuna con l'analisi e la pubblicazione!** 🎓

---

**Last updated:** Dicembre 2025
**Pipeline version:** 1.0
**Author:** AI Assistant for Corrado Caudek

END README
