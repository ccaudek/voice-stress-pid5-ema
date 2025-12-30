# Analysis Pipeline: Main Effects of Exam Stress on Vocal Production

This directory contains a complete analysis pipeline for examining the main effects of exam-related stress on vocal acoustic parameters (F0 and NNE), excluding personality trait moderations.

## Overview

The pipeline consists of:
1. **Stan models** for hierarchical Bayesian analysis
2. **R scripts** for data preparation, model fitting, and visualization
3. **Manuscript sections** (Results and Discussion) ready for integration

## Directory Structure

```
project/
├── data/
│   └── voice_stress_data.csv          # Your raw data (not included)
├── models/
│   ├── f0_main_effects.stan            # Stan model for F0
│   ├── nne_main_effects.stan           # Stan model for NNE
│   ├── fit_f0_main_effects.rds         # Fitted F0 model (generated)
│   └── fit_nne_main_effects.rds        # Fitted NNE model (generated)
├── scripts/
│   ├── 01_main_effects_analysis.R      # Main analysis script
│   ├── 02_manuscript_tables_figures.R  # Generate publication materials
│   └── 05_populate_results.R           # Auto-populate Results section
├── results/
│   ├── f0_main_effects_summary.csv     # F0 parameter summaries
│   ├── nne_main_effects_summary.csv    # NNE parameter summaries
│   ├── f0_posterior_samples.csv        # Full F0 posterior
│   └── nne_posterior_samples.csv       # Full NNE posterior
├── figures/
│   ├── f0_posterior_effects.png
│   ├── nne_posterior_effects.png
│   ├── f0_ppc.png
│   ├── nne_ppc.png
│   ├── f0_marginal_means.png
│   ├── nne_marginal_means.png
│   ├── figure1_posterior_distributions.png
│   ├── figure2_trajectories.png
│   └── figure3_effect_sizes.png
├── tables/
│   ├── table1_main_effects.html
│   └── table1_main_effects.docx
├── manuscript/
│   ├── results_main_effects_complete.md    # Auto-generated Results section
│   ├── discussion_main_effects.md          # Discussion section
│   └── parameter_estimates_summary.csv     # Quick reference table
└── README.md                               # This file
```

## Prerequisites

### Required R Packages

```r
install.packages(c(
  "tidyverse",
  "rstan",
  "bayesplot",
  "loo",
  "posterior",
  "gt",
  "patchwork",
  "here",
  "glue"
))
```

### Stan Installation

Ensure you have a working Stan installation. See: https://mc-stan.org/users/interfaces/rstan

## Data Requirements

Your dataset should have the following structure:

```
subj_id    timepoint    f0_mean    nne
-------    ---------    -------    ----
1          BASELINE     215.3      -26.8
1          PRE          223.1      -28.4
1          POST         220.5      -27.9
2          BASELINE     198.2      -25.3
...
```

### Required Variables

- `subj_id`: Unique participant identifier (numeric or character)
- `timepoint`: Factor with levels `BASELINE`, `PRE`, `POST`
- `f0_mean`: Fundamental frequency in Hz (numeric)
- `nne`: Normalized Noise Energy in dB (numeric)

## Analysis Pipeline

### Step 1: Prepare Your Data

Place your data file in `data/voice_stress_data.csv` or update the file path in the scripts.

### Step 2: Run Main Analysis

```r
source("scripts/01_main_effects_analysis.R")
```

This script will:
- Load and prepare your data
- Create contrast codes (c1: stress, c2: recovery)
- Fit hierarchical Bayesian models for F0 and NNE
- Generate diagnostic plots
- Save fitted models and posterior samples

**Expected runtime:** 10-30 minutes depending on your data size and computer

### Step 3: Generate Manuscript Materials

```r
source("scripts/02_manuscript_tables_figures.R")
```

This creates:
- Publication-ready tables (HTML and Word formats)
- Figures for main text and supplements
- Diagnostic visualizations

### Step 4: Populate Results Section

```r
source("scripts/05_populate_results.R")
```

This automatically fills in all numerical values in the Results section template, generating a complete, ready-to-use manuscript section.

## Model Specifications

### Contrast Coding

We use orthogonal contrast codes to decompose the stress response:

**Stress Contrast (c1):** PRE vs BASELINE
- BASELINE: -0.5
- PRE: +0.5  
- POST: 0

**Recovery Contrast (c2):** POST vs PRE
- BASELINE: 0
- PRE: -0.5
- POST: +0.5

### Hierarchical Structure

Both models include:
- **Fixed effects:** Intercept (α), stress effect (β₁), recovery effect (β₂)
- **Random effects:** Subject-specific intercepts and slopes for both contrasts
- **Non-centered parameterization** for computational efficiency

### Priors

**F0 Model:**
- α ~ Normal(220, 30) — typical adult pitch range
- β₁, β₂ ~ Normal(0, 10) — weakly informative
- τ ~ Exponential(0.5) — random effect SDs
- σ ~ Exponential(0.1) — residual SD

**NNE Model:**
- α ~ Normal(-28, 5) — typical NNE values
- β₁, β₂ ~ Normal(0, 5) — weakly informative
- τ ~ Exponential(0.2) — random effect SDs
- σ ~ Exponential(0.2) — residual SD

## Interpreting Results

### Key Parameters

- **α (Intercept):** Estimated baseline value (BASELINE timepoint)
- **β₁ (Stress effect):** Change from BASELINE to PRE (stress induction)
- **β₂ (Recovery effect):** Change from PRE to POST (recovery phase)
- **τ:** Between-person standard deviations
- **σ:** Residual (within-person) standard deviation

### Posterior Probabilities

- **P(β > 0):** Probability that effect is positive
- **P(β < 0):** Probability that effect is negative
- Values > 0.95 indicate strong directional evidence

### Effect Size Interpretation

**F0 Mean:**
- Small: 3-5 Hz
- Medium: 5-10 Hz  
- Large: >10 Hz

**NNE:**
- Small: 0.5-1.5 dB
- Medium: 1.5-3 dB
- Large: >3 dB

## Expected Results

Based on existing literature and preliminary findings:

### F0 Mean
- **Stress effect (β₁):** Positive, robust increase (~5-15 Hz)
- **Recovery effect (β₂):** Variable (may remain elevated, plateau, or partially normalize)

### NNE
- **Stress effect (β₁):** Negative, reduction in glottal noise (~1-3 dB decrease)
- **Recovery effect (β₂):** Variable (may begin to normalize or persist)

## Troubleshooting

### Convergence Issues

If models fail to converge (R-hat > 1.01):

1. Increase `adapt_delta`:
   ```r
   control = list(adapt_delta = 0.99)
   ```

2. Increase iterations:
   ```r
   iter = 6000, warmup = 3000
   ```

3. Check for data issues (outliers, missing values)

### Memory Issues

If you encounter memory errors:

1. Reduce posterior samples saved:
   ```r
   post_f0 <- as_draws_df(fit_f0) %>% slice_sample(n = 4000)
   ```

2. Process models separately (comment out one model fit at a time)

### Data Structure Errors

Common issues:
- Ensure `subj_id` is sequential integers (1, 2, 3, ...)
- Check for missing timepoints (each participant needs all 3)
- Verify contrast codes sum to zero

## Citation

If you use this pipeline, please cite:

```
[Your manuscript citation here]
```

## Contact

For questions or issues:
- Email: [your email]
- GitHub Issues: [repository link if applicable]

## License

[Specify your license here, e.g., MIT, CC-BY-4.0]

---

## Advanced Usage

### Customizing Priors

Edit the `.stan` files to modify prior specifications:

```stan
// In f0_main_effects.stan, line 58-61:
alpha ~ normal(220, 30);  // Change mean and SD as needed
b1 ~ normal(0, 10);       // Adjust informativeness
```

### Adding Covariates

To include additional predictors (e.g., age, gender):

1. Add to `data` block:
   ```stan
   vector[N_obs] age;
   ```

2. Add to `parameters`:
   ```stan
   real b_age;
   ```

3. Include in likelihood:
   ```stan
   real mu = alpha + u0[s] + b_age * age[n] + ...
   ```

### Posterior Predictive Checks

Generate additional PPCs:

```r
# Density overlay by timepoint
ppc_dens_overlay_grouped(
  y = stan_data_f0$y,
  yrep = y_rep_f0[1:100, ],
  group = df$timepoint
)
```

### Model Comparison

Compare nested models using LOO-CV:

```r
# Fit null model (no stress effects)
fit_null <- stan(...)

# Compare
loo_compare(loo_null, loo_f0)
```

## Version History

- **v1.0** (2025-01-XX): Initial release
  - Main effects models for F0 and NNE
  - Complete manuscript sections
  - Visualization pipeline

## Acknowledgments

This pipeline was developed for analyzing exam stress effects on vocal production using hierarchical Bayesian methods. Thanks to [acknowledge collaborators/funding here].
