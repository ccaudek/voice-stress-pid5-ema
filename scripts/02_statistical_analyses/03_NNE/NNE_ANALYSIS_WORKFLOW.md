# NNE Analysis Pipeline - Complete Workflow

## 📋 Overview

This pipeline analyzes how PID-5 personality pathology domains moderate vocal stress reactivity for **NNE (Normalized Noise Energy)**, following the same framework successfully applied to F0.

---

## 🔄 **Complete Workflow (Riproducibile)**

### **Step 1: Data Preparation**
```r
source("scripts/02_statistical_analyses/03_NNE/01_prepare_stan_data_nne_pid5.R")
```

**What it does:**
- Loads voice data (NNE from 3 vowels: /a/, /i/, /u/)
- Aggregates NNE across vowels for robustness
- Loads EMA PID-5 data
- Imputes missing values (missRanger)
- Standardizes PID-5 scores
- Creates Stan data list
- Saves: `stan/NNE/stan_data_nne_pid5.json` and `results/NNE/stan_bundle_nne_pid5.rds`

**Checks:**
- ✓ ID consistency between voice and EMA
- ✓ No NA in Stan data
- ✓ Contrast coding correct

---

### **Step 2: Model Fitting** (REVISED)
```r
source("scripts/02_statistical_analyses/03_NNE/02_run_nne_pid5_REVISED.R")
```

**Changes from original:**
- ✅ Standalone (doesn't call script 01 with relative paths)
- ✅ Saves model summary to CSV
- ✅ Performs convergence checks
- ✅ Creates PPC plot

**What it does:**
- Loads prepared data bundle
- Compiles Stan model (or loads existing fit)
- Samples posterior (4 chains, 6000 iter, adapt_delta=0.99)
- Saves fit: `stan/NNE/nne_mean_pid5_moderation.rds`
- Saves summary: `results/NNE/model_summary_nne_moderation.csv` 
- Creates PPC: `results/NNE/ppc_nne_moderation.png`

**Outputs:**
- Convergence diagnostics (Rhat, ESS)
- Main effects (b1, b2) with PD
- Moderation effects (g1, g2) per domain

---

### **Step 3: Check Expected Direction** (Optional but Recommended)
```r
source("scripts/02_statistical_analyses/03_NNE/03_check_expected_direction.R")
```

**What it does:**
- Verifies contrast coding
- Computes within-subject differences
- Checks sign of effects
- Interprets NNE direction (more negative = cleaner voice)

**Why important:**
- NNE interpretation is less intuitive than F0
- More negative = less noise = "better" quality (under tension)
- Less negative = more noise = "breathier" quality

---

### **Step 4: Detailed Interpretation**
```r
source("scripts/02_statistical_analyses/03_NNE/04_interpret_nne_pid5_fit.R")
```

**What it does:**
- Extracts main effects and moderations
- Computes simple effects at -1/0/+1 SD
- Calculates PD and P(expected direction)

**Note:** This script prints to console but doesn't save tables. Use Step 5 for publication-ready tables.

---

### **Step 5: Differential Effects Analysis** 
```r
source("scripts/02_statistical_analyses/03_NNE/05_differential_effects_nne.R")
```

**What it does (same framework as F0):**
1. **Direction Certainty**: PD-based classification (Strong/Moderate/Weak)
2. **Signal-to-Noise Ratio**: SNR = |median| / MAD
3. **Pairwise Contrasts**: P(domain_A > domain_B)
4. **Theoretical Alignment**: Observed vs predicted patterns

**Outputs:**
- `results/NNE/direction_certainty_table.csv` ← **For manuscript!**
- `results/NNE/theoretical_alignment_table.csv`
- `results/NNE/figure_direction_certainty.png`
- `results/NNE/figure_snr.png`
- `results/NNE/figure_pairwise_contrasts.png` (if applicable)

---

### **Step 6: Create Manuscript Tables** (NEW!)
```r
source("scripts/02_statistical_analyses/06_create_manuscript_table.R")
```

**What it does:**
- Combines F0 and NNE results
- Creates publication-ready tables
- Generates comparison tables
- Produces LaTeX output (optional)

**Outputs:**
- `results/manuscript_table_combined.csv` ← **All effects**
- `results/manuscript_table_key_effects.csv` ← **Only Strong/Moderate**
- `results/comparison_f0_vs_nne.csv` ← **Side-by-side comparison**
- `results/manuscript_table.tex` (if kableExtra installed)
- `results/README_manuscript_tables.md` ← **Interpretation guide**

---

## 📊 **Key Output Files**

### **For Manuscript (.Rmd)**

#### **Main Analysis Tables:**
```r
# Load key effects table
key_effects <- read_csv("results/manuscript_table_key_effects.csv")

# Report in text:
# "Negative Affectivity showed strong directional certainty for stress 
#  moderation of F0 (γ₁ = 3.14 Hz [0.37, 5.89], PD = 0.97)..."
```

#### **Comparison Table:**
```r
# Load comparison
comparison <- read_csv("results/comparison_f0_vs_nne.csv")

# Use to discuss dissociation:
# "Trait moderation differed between F0 and NNE. While Negative Affectivity
#  showed strong moderation of F0 (PD = 0.97), NNE moderation was weak
#  (PD = 0.65), suggesting traits primarily affect arousal rather than
#  phonatory control."
```

### **For Verification:**
```r
# Check convergence
summary_nne <- read_csv("results/NNE/model_summary_nne_moderation.csv")
max(summary_nne$rhat)  # Should be < 1.01
min(summary_nne$ess_bulk)  # Should be > 1000
```

---

## 🔍 **Quality Checks**

### **Before Publication:**

1. **Convergence:**
   - [ ] All Rhat < 1.01 ✓
   - [ ] All ESS > 1000 ✓
   - [ ] Trace plots show good mixing

2. **Model Fit:**
   - [ ] PPC shows good overlap
   - [ ] No systematic deviations

3. **Direction Certainty:**
   - [ ] At least 1-2 effects with PD > 0.90
   - [ ] SNR > 1.0 for key effects
   - [ ] Theoretical alignment makes sense

4. **Reproducibility:**
   - [ ] All paths use `here::here()`
   - [ ] No hardcoded absolute paths
   - [ ] Scripts run in order without errors
   - [ ] Results match manuscript tables

---

## 📝 **Interpreting NNE Results**

### **NNE Physiology:**
- **NNE = Noise-to-Harmonics Energy (dB)**
- Typically negative values (-15 to -35 dB)
- **More negative** (-30 dB) = less glottal noise = "cleaner" voice
- **Less negative** (-15 dB) = more glottal noise = "breathier" voice

### **Stress Effects:**
- **b1 < 0** (more negative under stress):
  - Stress → increased tension → tighter glottal closure → cleaner voice
  - Interpretation: "Vocal control" or "hyperadduction"

- **b1 > 0** (less negative under stress):
  - Stress → decreased control → looser closure → breathier voice
  - Interpretation: "Vocal instability"

### **Moderation Effects:**
- **g1 < 0** (trait makes NNE more negative):
  - High trait → more tension under stress
  - Example: NA → hypercontrol

- **g1 > 0** (trait makes NNE less negative):
  - High trait → less control under stress
  - Example: Disinhibition → vocal instability

---

## 🎯 **Expected Patterns (Theoretical)**

### **Based on F0 Results:**
If **Negative Affectivity** amplifies F0 stress response, we might expect:
- **NNE moderation in same direction** (coupled arousal + control)
- **Or no NNE moderation** (dissociation: arousal ≠ control)

### **Based on Vocal Physiology:**
- **NA**: Increased tension → more negative NNE (g1 < 0)
- **Disinhibition**: Decreased control → less negative NNE (g1 > 0)
- **Antagonism**: Reduced engagement → less tension → less negative (g1 > 0)

---

## ⚠️ **Common Issues & Solutions**

### **Problem 1: NNE shows no moderation**
**Solution:**
- This is a finding! Report as dissociation from F0
- Interpretation: Traits affect arousal (F0) but not phonatory control (NNE)

### **Problem 2: Opposite direction from theory**
**Solution:**
- Check if main effect (b1) is strong
- Could be compensatory mechanism
- Report as contrary finding with proposed mechanism

### **Problem 3: Wide CIs and low PD**
**Solution:**
- Use Direction Certainty framework (not ROPE)
- Report SNR to show effect emerges from noise
- Focus on pairwise contrasts if some domains stand out

---

## 📁 **File Organization**

```
project/
├── scripts/
│   └── 02_statistical_analyses/
│       └── 03_NNE/
│           ├── 01_prepare_stan_data_nne_pid5.R
│           ├── 02_run_nne_pid5_REVISED.R ← Use this version!
│           ├── 03_check_expected_direction.R
│           ├── 04_interpret_nne_pid5_fit.R
│           ├── 05_differential_effects_nne.R ← NEW!
│           └── 06_create_manuscript_table.R ← NEW!
├── stan/
│   └── NNE/
│       ├── nne_pid5_moderation.stan
│       ├── stan_data_nne_pid5.json
│       └── nne_mean_pid5_moderation.rds
└── results/
    ├── NNE/
    │   ├── stan_bundle_nne_pid5.rds
    │   ├── model_summary_nne_moderation.csv ← NEW!
    │   ├── ppc_nne_moderation.png
    │   ├── direction_certainty_table.csv ← NEW!
    │   ├── theoretical_alignment_table.csv ← NEW!
    │   ├── figure_direction_certainty.png ← NEW!
    │   ├── figure_snr.png ← NEW!
    │   └── figure_pairwise_contrasts.png ← NEW!
    ├── manuscript_table_combined.csv ← For .Rmd!
    ├── manuscript_table_key_effects.csv ← For .Rmd!
    ├── comparison_f0_vs_nne.csv ← For Discussion!
    └── README_manuscript_tables.md ← Interpretation guide!
```

---

## ✅ **Checklist: Complete Analysis**

### **Data Preparation:**
- [ ] Run `01_prepare_stan_data_nne_pid5.R`
- [ ] Check: `stan_bundle_nne_pid5.rds` exists
- [ ] Check: No warnings about NA

### **Model Fitting:**
- [ ] Run `02_run_nne_pid5_REVISED.R` (use REVISED version!)
- [ ] Check: All Rhat < 1.01
- [ ] Check: All ESS > 1000
- [ ] Check: `model_summary_nne_moderation.csv` created

### **Interpretation:**
- [ ] Run `03_check_expected_direction.R` (understand NNE direction)
- [ ] Run `04_interpret_nne_pid5_fit.R` (detailed results)
- [ ] Run `05_differential_effects_nne.R` (publication analysis)
- [ ] Check: Figures created in `results/NNE/`

### **Manuscript Tables:**
- [ ] Run `06_create_manuscript_table.R`
- [ ] Check: `manuscript_table_key_effects.csv` created
- [ ] Check: `comparison_f0_vs_nne.csv` created
- [ ] Review: Are F0 and NNE patterns consistent or dissociated?

### **Final Verification:**
- [ ] Compare tables with manuscript text
- [ ] Verify all numbers match
- [ ] Check theoretical interpretation makes sense
- [ ] Ensure reproducibility (no hardcoded paths)

---

## 🚀 **Quick Start**

```r
# Run complete pipeline
setwd(here::here())

# Step 1: Prepare data
source("scripts/02_statistical_analyses/03_NNE/01_prepare_stan_data_nne_pid5.R")

# Step 2: Fit model (REVISED version)
source("scripts/02_statistical_analyses/03_NNE/02_run_nne_pid5_REVISED.R")

# Step 3: Check direction
source("scripts/02_statistical_analyses/03_NNE/03_check_expected_direction.R")

# Step 4: Interpret
source("scripts/02_statistical_analyses/03_NNE/04_interpret_nne_pid5_fit.R")

# Step 5: Differential effects
source("scripts/02_statistical_analyses/03_NNE/05_differential_effects_nne.R")

# Step 6: Create manuscript tables
source("scripts/02_statistical_analyses/06_create_manuscript_table.R")

# Done! Check results/manuscript_table_key_effects.csv
```

---

## 📖 **For Manuscript (.Rmd)**

### **Methods Section:**
```r
# Include in .Rmd:
# "We applied the same hierarchical Bayesian framework to NNE as to F0,
#  modeling personality domains as latent variables with measurement error.
#  Direction certainty was assessed using Probability of Direction (PD),
#  with effects classified as Strong (PD > 0.95), Moderate (PD > 0.90),
#  or Weak. Signal-to-noise ratios (SNR = |median| / MAD) quantified 
#  effect size relative to uncertainty."
```

### **Results Section:**
```r
# Load tables
key_effects <- read_csv("results/manuscript_table_key_effects.csv")

# Report findings
# Example if NNE shows no moderation:
# "Unlike F0, NNE showed weak evidence of personality moderation across
#  all domains (all PD < 0.80). This dissociation suggests that personality
#  traits primarily modulate arousal-related pitch changes rather than
#  phonatory control reflected in noise characteristics."
```

---

**End of Workflow Guide**
