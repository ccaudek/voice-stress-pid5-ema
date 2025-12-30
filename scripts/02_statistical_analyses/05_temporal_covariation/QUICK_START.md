# 🚀 QUICK START
## Within-Person Covariation Analysis

---

## ✅ COSA HAI ORA

Pipeline completa in **5 script numerati**:

```
within_person_pipeline/
├── 00_setup_and_verify.R
├── 01_prepare_within_person_data.R
├── 02_fit_models.R
├── 03_analyze_heterogeneity.R
├── 04_create_manuscript_materials.R
├── stan_models/
│   ├── baseline_model.stan
│   └── random_slopes_noncentered.stan
└── README.md (guida completa)
```

---

## 🎯 PRIMA DI INIZIARE

### Assicurati di avere questi **3 file raw**:

1. **AUDIO.xlsx** → `data/raw/acustic_features/datiacustici/`
2. **ema_plus_scales_cleaned.csv** → `data/processed/`
3. **all_combined_sex_NEW_1.xlsx** → `data/raw/meta/`

**Se non li hai:** Devi recuperarli dal progetto voice-stress originale.

---

## 🚀 ESECUZIONE (4 COMANDI)

### Da R o RStudio:

```r
# 1. Setup (< 1 min)
setwd("path/to/your/project")
source("within_person_pipeline/00_setup_and_verify.R")

# 2. Prepara dati (2-5 min)
source("within_person_pipeline/01_prepare_within_person_data.R")

# 3. Fit models (2-4 ORE! - lascia girare)
source("within_person_pipeline/02_fit_models.R")

# 4. Analizza eterogeneità (5-10 min)
source("within_person_pipeline/03_analyze_heterogeneity.R")

# 5. Materiali manoscritto (< 1 min)
source("within_person_pipeline/04_create_manuscript_materials.R")
```

---

## ⏱️ TIMELINE

| Step | Tempo | Cosa fare |
|------|-------|-----------|
| 00 Setup | < 1 min | Esegui e verifica OK |
| 01 Dati | 2-5 min | Esegui |
| 02 Fitting | 2-4 ore | Esegui e vai a dormire 😴 |
| 03 Analisi | 5-10 min | Esegui |
| 04 Manuscript | < 1 min | Esegui |

**TOTALE:** ~3-5 ore (unattended)

---

## 📊 OUTPUT FINALE

Dopo esecuzione completa, troverai:

### **In `results/within_person_final/`:**

```
fitted_models/
  - fit_baseline.rds
  - fit_random_slopes.rds
  - model_comparison.csv ← R² e ELPD comparison

heterogeneity_analysis/
  - 02_forest_plot_NA.pdf ← MAIN FIGURE per paper
  - 05_individual_examples.pdf ← SUPPLEMENTARY
  - individual_slopes.csv
  - responder_summary.csv

manuscript_materials/
  - results_text.txt ← Copia nel paper
  - discussion_text.txt ← Copia nel paper
  - table1_model_comparison.csv
```

---

## 🎯 RISULTATO ATTESO

**R² comparison:**
- Baseline: ~2.5%
- Random Slopes: ~35%
- **14× improvement** → conferma eterogeneità

**ELPD difference:** ~40 punti → random slopes MOLTO migliore

**Pattern:** ~1/3 positive, ~1/3 negative, ~1/3 null

**Conclusione:** 
> "Vocal-affective coupling is IDIOGRAPHIC"

---

## 📝 TROUBLESHOOTING RAPIDO

**Se 00_setup fallisce:**
→ Installa pacchetti mancanti o posiziona file raw

**Se 02_fit è troppo lento:**
→ Normale! 2-3 ore per random slopes

**Se convergence problems:**
→ Script già usa parametri ottimali (adapt_delta=0.999)

**Per dettagli completi:**
→ Leggi `README.md` in `within_person_pipeline/`

---

## ✅ READY TO START!

```r
# Prima di tutto, verifica setup:
source("within_person_pipeline/00_setup_and_verify.R")
```

Se setup è OK (✓), procedi con gli altri script in ordine!

**Buona analisi!** 🎓

---
