# STRUTTURA PACKAGE: Main Effects Analysis

```
package_main_effects/
│
├── 📄 EXECUTIVE_SUMMARY.md          ⭐ INIZIA DA QUI - Quick Start
├── 📄 INDEX.md                      📚 Indice completo del package
│
├── 📁 models/                       🔬 Modelli Stan
│   ├── f0_main_effects.stan         • Modello gerarchico per F0
│   └── nne_main_effects.stan        • Modello gerarchico per NNE
│
├── 📁 scripts/                      💻 Script di Analisi R
│   ├── 01_main_effects_analysis.R   • SCRIPT PRINCIPALE - Fitting modelli
│   ├── 02_manuscript_tables_figures.R • Genera tabelle e figure
│   ├── 05_populate_results.R        • Popola sezione Risultati
│   └── 06_diagnostic_report.R       • Report diagnostico completo
│
├── 📁 manuscript/                   📝 Sezioni Manoscritto
│   ├── 03_results_main_effects.md   • Template sezione Risultati
│   └── 04_discussion_main_effects.md • Discussione COMPLETA ✅
│
└── 📁 documentation/                📖 Documentazione
    └── README.md                    • Guida completa all'uso

```

---

## 🎯 Workflow Essenziale

### Step 1: Leggi Executive Summary
→ `EXECUTIVE_SUMMARY.md`

### Step 2: Prepara i tuoi dati
Struttura CSV richiesta:
```
subj_id, timepoint, f0_mean, nne
1, BASELINE, 220.5, -26.8
1, PRE, 228.3, -28.4
1, POST, 226.1, -27.9
...
```

### Step 3: Esegui l'analisi
```r
source("scripts/01_main_effects_analysis.R")
```
⏱️ Tempo: 10-30 minuti

### Step 4: Genera materiali manoscritto
```r
source("scripts/02_manuscript_tables_figures.R")
source("scripts/05_populate_results.R")
source("scripts/06_diagnostic_report.R")
```
⏱️ Tempo: 5-10 minuti

### Step 5: Integra nel manoscritto
- Copia `manuscript/results_main_effects_complete.md` → Risultati
- Copia `manuscript/04_discussion_main_effects.md` → Discussione
- Aggiungi figure e tabelle generate

---

## 📦 Output Generati

Dopo aver eseguito gli script, avrai:

### Directory `models/`
- `fit_f0_main_effects.rds`
- `fit_nne_main_effects.rds`

### Directory `results/`
- `f0_main_effects_summary.csv`
- `nne_main_effects_summary.csv`
- `f0_posterior_samples.csv`
- `nne_posterior_samples.csv`
- `diagnostic_report.txt`

### Directory `figures/`
- `figure1_posterior_distributions.png` ⭐ Manoscritto
- `figure2_trajectories.png` ⭐ Manoscritto
- `figure3_effect_sizes.png` ⭐ Manoscritto
- `f0_posterior_effects.png`
- `nne_posterior_effects.png`
- `f0_ppc.png`, `nne_ppc.png`
- `f0_marginal_means.png`, `nne_marginal_means.png`
- Diagnostic plots (trace, ACF)

### Directory `tables/`
- `table1_main_effects.html`
- `table1_main_effects.docx` ⭐ Manoscritto

### Directory `manuscript/`
- `results_main_effects_complete.md` ⭐ Con valori numerici
- `parameter_estimates_summary.csv`

---

## 🔑 File Chiave

### Per iniziare
1. `EXECUTIVE_SUMMARY.md` - Quick start guide
2. `scripts/01_main_effects_analysis.R` - Script principale

### Per il manoscritto
1. `manuscript/04_discussion_main_effects.md` - PRONTO ✅
2. `manuscript/results_main_effects_complete.md` - Dopo script 05
3. Figure `figure1`, `figure2`, `figure3`
4. Tabella `table1_main_effects.docx`

### Per troubleshooting
1. `documentation/README.md` - Guida dettagliata
2. `results/diagnostic_report.txt` - Dopo script 06

---

## ✅ Checklist Completa

### Setup Iniziale
- [ ] Installare pacchetti R richiesti
- [ ] Verificare Stan funziona
- [ ] Preparare dati con struttura corretta

### Analisi
- [ ] Eseguire `01_main_effects_analysis.R`
- [ ] Verificare convergenza (no warnings)
- [ ] Controllare output in `models/` e `results/`

### Materiali Manoscritto
- [ ] Eseguire `02_manuscript_tables_figures.R`
- [ ] Eseguire `05_populate_results.R`
- [ ] Verificare figure in `figures/`
- [ ] Verificare tabelle in `tables/`

### Quality Control
- [ ] Eseguire `06_diagnostic_report.R`
- [ ] Leggere `diagnostic_report.txt`
- [ ] Verificare tutti i check passano ✅

### Integrazione
- [ ] Copiare `results_main_effects_complete.md` nel manoscritto
- [ ] Copiare `04_discussion_main_effects.md` nel manoscritto
- [ ] Inserire figure (3 principali + diagnostiche nei supplements)
- [ ] Inserire tabella 1

---

## 📊 Risultati Attesi

### F0 Mean
- **Stress effect (β₁):** +5 a +15 Hz, P(β₁ > 0) > 0.95
- **Interpretation:** Arousal-driven pitch elevation

### NNE
- **Stress effect (β₁):** -1 a -3 dB, P(β₁ < 0) > 0.95
- **Interpretation:** Control-driven noise reduction

### Significato
**Dissociazione = Dual-process model**
- F0 ↑ = Arousal component
- NNE ↓ = Control component

---

## 🆘 Supporto Rapido

### Problema: Modello non converge
```r
# Aumentare adapt_delta
control = list(adapt_delta = 0.99)
```

### Problema: Divergent transitions
```r
# Check outliers nei dati
summary(df)
```

### Problema: Script non trova file
```r
# Verificare working directory
getwd()
setwd("path/to/package_main_effects")
```

### Problema: Package mancante
```r
# Installare
install.packages("nome_package")
```

---

## 📧 Per Domande

Consulta nell'ordine:
1. `EXECUTIVE_SUMMARY.md` - Quick answers
2. `README.md` - Dettagli tecnici
3. `diagnostic_report.txt` - Problemi convergenza
4. Contattare: [la tua email]

---

**Tempo totale stimato: ~1 ora dalla preparazione dati al manoscritto completo** ⏱️

**Tutto è pronto. Inizia con EXECUTIVE_SUMMARY.md!** 🚀
