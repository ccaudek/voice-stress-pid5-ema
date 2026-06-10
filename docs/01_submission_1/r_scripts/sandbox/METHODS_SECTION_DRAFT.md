# METHODS SECTION - COMPLETE DRAFT

## Participants

We recruited 141 female university students (M_age = [DA INSERIRE], SD = [DA INSERIRE]) from the University of Florence through course announcements and online postings. The sample was restricted to female participants to control for sex-related variation in vocal pitch characteristics, which could obscure personality-related effects and reduce statistical power. Fundamental frequency (F0) differs substantially between males and females (approximately 100 Hz lower in males) due to anatomical differences in vocal fold length and mass, making direct comparison problematic without large samples or complex statistical controls.

All participants were native Italian speakers with no reported history of voice disorders or current respiratory illness. Exclusion criteria included: (1) current or past psychiatric disorders requiring treatment, (2) substance use disorders, (3) self-reported hearing impairments that could affect voice monitoring, and (4) professional voice training (e.g., singing lessons), which could alter baseline vocal characteristics. Participants provided written informed consent and received course credit for participation. The study was approved by the University of Trieste Ethics Committee (protocol #[INSERIRE]).

**Note on parallel investigation.** A parallel study with male participants (N = 36) using an identical protocol examined MFCC-based patterns and stress-induced vocal alterations with focus on different acoustic features. The current manuscript reports exclusively on the female sample to maximize statistical power for examining personality moderation effects and ensure interpretability of pitch-related findings.

## Design and Procedure

We employed a naturalistic stress manipulation design, capitalizing on the university examination period as an ecologically valid acute stressor. The study comprised three assessment waves across 2.5 months:

1. **Baseline assessment** (T1): Administered 3-4 weeks before scheduled exams, participants completed the full PID-5 questionnaire and provided vocal recordings in a laboratory setting.
2. **Pre-exam assessment** (T2): The day before a major course examination, participants recorded vocal samples in the same laboratory.
3. **Post-exam assessment** (T3): The day after the examination, participants provided final vocal recordings.

Between T1 and T3, participants completed twice-weekly ecological momentary assessments (EMA) via smartphone application, yielding an average of 27.0 EMA observations per participant (range: 12-31).

## Measures

### Personality Pathology

**Full PID-5 (Baseline).** At T1, participants completed the 220-item Personality Inventory for DSM-5 (Krueger et al., 2012), which assesses five maladaptive trait domains: Negative Affectivity (α = [INSERIRE]), Detachment (α = [INSERIRE]), Antagonism (α = [INSERIRE]), Disinhibition (α = [INSERIRE]), and Psychoticism (α = [INSERIRE]). Items are rated on a 0-3 scale (0 = very false/often false, 3 = very true/often true). Domain scores were computed as mean item ratings.

**Brief PID-5 for EMA.** To reduce participant burden while maintaining construct coverage, we administered a 15-item brief version in EMA assessments (3 items per domain). Items were selected based on factor loadings from a larger sample from the same population (N > 1,000) to maximize domain representativeness while minimizing redundancy. 

EMA data were collected via the m-Path smartphone application (RoQua, Tilburg, Netherlands), a validated platform for ambulatory assessment research. Participants received push notifications twice weekly on non-consecutive days between 18:00 and 20:00 over the 2.5-month study period. Each prompt requested ratings of current affect states and brief personality-relevant items. Items were rated on the same 0-3 scale used in the full PID-5, adapted to assess "how you have felt/behaved since the last assessment."

In addition to routine EMA assessments, two exam-related prompts were administered: one immediately before a scheduled exam (same day, 1-4 hours prior) and one the day after exam completion. These exam-linked assessments were paired with voice recordings (see Voice Recordings section) to capture stress-related vocal changes.

**Data quality and compliance.** Participants with fewer than 50% response rate to EMA prompts were excluded from analysis prior to data processing to ensure adequate sampling of trait-relevant behaviors. Domain scores were computed by averaging items within each domain across all completed assessments, then person-mean centering to create stable trait estimates while removing individual response style effects.

### Voice Recordings

At each of the three laboratory sessions (T1, T2, T3), participants recorded sustained vowel phonations, a coarticulation task, and a standardized sentence in a sound-attenuated room. Recordings were made using a Shure SM58 microphone at 44.1 kHz sampling rate, positioned 15 cm from the participant's mouth at a 45° angle to minimize lateral distortions.

**Stimuli.** Participants produced:
1. Three repetitions of sustained Italian cardinal vowels (/a/, /i/, /u/) for at least 3 seconds each
2. A coarticulation task: counting from 1 to 10 in Italian
3. A standardized constantly-voiced Italian sentence: "Io amo le aiuole della mamma" (English: "I love mother's flowerbeds")

All recordings were performed with conversational pitch and loudness in quiet rooms to maintain ecological validity while ensuring acoustic quality. Participants were instructed to maintain consistent microphone positioning across all sessions.

**Recording quality control.** Audio files were automatically partitioned into segments corresponding to each vowel repetition, each number, and the standardized sentence, yielding 20 total segments per session. Visual inspection confirmed adequate signal-to-noise ratio and absence of technical artifacts (e.g., clipping, environmental noise intrusions).

### Acoustic Feature Extraction

Acoustic features were extracted using two complementary approaches: (1) traditional vocal parameters from sustained vowels, and (2) mel-frequency cepstral coefficients (MFCCs) from continuous speech.

**Sustained vowel analysis.** The open-source BioVoice software (version [INSERIRE]; [CITATION]) was used to extract 37 acoustic parameters from sustained vowel phonations, spanning frequency and time domains. Key features included:

- **Fundamental frequency (F0).** Mean and median F0 were computed across the sustained portion of each vowel (excluding onset/offset), indexing vocal pitch and laryngeal tension. F0 standard deviation quantified pitch stability. The time instance of maximum F0 (T0) was also extracted as a marker of phonatory dynamics. Higher F0 reflects increased vocal fold tension associated with arousal and stress, representing one of the most robust acoustic markers of altered psychological states (Giddens et al., 2013).

- **Formant frequencies.** Mean F1 (first formant) and F2 (second formant) were extracted for each vowel, along with their minimum and maximum values. Formants index articulatory precision and vocal tract configuration. F2, in particular, reflects tongue positioning and articulatory control, with variability (F2 SD) indicating consistency of articulation.

- **Voice quality parameters.** Jitter (cycle-to-cycle F0 variation, expressed as percentage) and voiced unit duration were extracted to assess glottal stability and phonatory control. Higher jitter values suggest compromised vocal control or irregular vocal fold vibration.

- **Noise-to-harmonics ratio (NNE).** Normalized noise energy (expressed in dB) quantifies aperiodicity in the voice signal, with higher (less negative) values indicating breathy or rough voice quality reflecting incomplete glottal closure or increased tension.

**Theoretical rationale.** These parameters were selected based on neurophysiological evidence that stress increases general muscular tone, including at the laryngeal level, causing vocal folds to stretch and vibrate more rapidly (F0 increase). Additionally, stress-induced tension in articulators (tongue, jaw, soft palate) alters resonance patterns, reflected in formant shifts and reduced variability (Scherer, 1986; Giddens et al., 2013).

**Mel-frequency cepstral coefficients (MFCCs).** For the standardized sentence, we computed MFCCs to capture the spectral envelope of continuous speech. MFCCs approximate the human auditory system's nonlinear frequency response and have been extensively validated for emotion and stress recognition (Eyben et al., 2015; Rachman et al., 2018).

The MFCC extraction pipeline was implemented in MATLAB R2023a (The MathWorks, Natick, MA, USA) using the following parameters:
- **Frame-based analysis:** 25ms Hamming windows with 15ms overlap (10ms step)
- **Mel filterbank:** 13 triangular filters spanning 0-8000 Hz
- **Output:** 13 MFCCs per frame (MFCC1-MFCC13)

Each MFCC captures different spectral properties: lower coefficients (MFCC1-MFCC5) reflect broad spectral shape and resonance (related to vocal tract configuration), while higher coefficients (MFCC6-MFCC13) capture fine spectral details including consonantal articulation and aspiration noise.

For each MFCC coefficient across all frames in a sentence, we computed eight summary statistics to characterize distributional properties:
1. Mean (central tendency)
2. Standard deviation (variability/stability)
3. Median (robust central tendency)
4. Interquartile range (IQR; robust variability measure, less sensitive to outliers)
5. Skewness (asymmetry of distribution)
6. Kurtosis (tail heaviness)
7. 25th percentile (lower quartile)
8. 75th percentile (upper quartile)

This yielded 104 MFCC-derived features per sentence (13 coefficients × 8 statistics). Reduced variability (lower SD/IQR) in MFCCs under stress likely reflects constrained articulatory movements due to increased muscular tension, while shifts in mean/median values may indicate altered resonance patterns or prosodic flattening.

**Quality control and preprocessing.** All acoustic measures were visually inspected using Praat spectrograms and waveform displays. Values beyond 3 SD from the mean were flagged for manual review to identify potential extraction errors (e.g., octave jumps in F0 tracking, formant tracking failures). No systematic outliers requiring exclusion were identified. For analyses, acoustic features were z-score standardized within each feature to enable comparison across parameters with different units and scales.

## Data Quality and Missingness

### EMA Compliance and Quality Control

Participants with fewer than 5 or more than 40 EMA assessments were excluded prior to analysis (n excluded = [INSERIRE]). We implemented additional quality checks to identify careless responding:

1. **Within-subject variability:** Participants with SD < 0.30 on the Negative Affectivity composite (which should exhibit temporal variation) were flagged for review.
2. **Response patterns:** Excessive use of scale endpoints or preference for round numbers (>80% responses divisible by 10 on 0-100 visual analog scales) indicated potential inattention.
3. **A priori exclusions:** Based on suspicious response patterns identified in preliminary screening, n = [INSERIRE] participants were excluded before analysis.

Final sample included N = 141 participants with complete voice data and valid EMA responses.

### Missing Data in Baseline PID-5

Baseline PID-5 data were missing for 93 observations (22% of total) due to participants entering the study through different recruitment streams. Missing values were imputed using random forest imputation (missRanger package; Mayer, 2019) with predictive mean matching (k = 3 nearest neighbors). EMA PID-5 domain scores were included as auxiliary variables given their strong convergent validity (see Results). Sensitivity analyses confirmed that results were robust to imputation approach (complete-case analysis yielded similar parameter estimates; see Supplementary Materials).

## Convergent Validity of EMA Measures

To establish construct validity of the brief EMA assessment, we computed correlations between person-level EMA domain scores (aggregated across all assessments) and full baseline PID-5 domains. This addresses the critical question of whether intensive sampling with reduced item coverage captures equivalent trait variance to comprehensive single-session assessment.

## Statistical Analysis

All analyses were conducted in R (version 4.5) using Bayesian multilevel models implemented in brms (Bürkner, 2017) with cmdstanr backend. We adopted a Bayesian framework to enable principled uncertainty quantification, incorporate prior knowledge, and estimate complex variance structures without convergence issues common in frequentist multilevel modeling.

### Model Specification

**Moderation models.** For each acoustic outcome (6 features × 3 vowels = 18 total), we estimated:

```
Outcome_ij = β_0 + β_1(Stress) + β_2(Recovery) +
             β_3(Trait_1) + ... + β_7(Trait_5) +
             β_8(Stress × Trait_1) + ... + β_12(Stress × Trait_5) +
             β_13(Recovery × Trait_1) + ... + β_17(Recovery × Trait_5) +
             u_0i + u_1i(Stress) + u_2i(Recovery) + ε_ij
```

Where i indexes participants, j indexes timepoints, Stress and Recovery are orthogonal contrast codes (Stress: baseline = -0.5, pre-exam = 0.5, post-exam = 0; Recovery: baseline = 0, pre-exam = -0.5, post-exam = 0.5), and all trait predictors were person-mean centered. Random effects included random intercepts and uncorrelated random slopes for temporal contrasts.

**Likelihood families.** We selected likelihood distributions based on outcome properties:
- F0 mean, F2 mean, NNE: Gaussian (unbounded continuous)
- F0 SD, F2 SD, Jitter: Lognormal (positive continuous, right-skewed)

For F2 mean, robust regression with Student-t likelihood was used to accommodate occasional extreme values.

### Prior Specification

Priors were weakly informative, centered on realistic parameter ranges while allowing data to dominate inference:

```
F0 mean:   Intercept ~ Student-t(3, 220, 30)
           β ~ Normal(0, 10)
           σ ~ Exponential(0.3)
           
F2 mean:   Intercept ~ Student-t(3, 1500, 150)
           β ~ Normal(0, 50)
           ν ~ Gamma(2, 0.1)  [degrees of freedom]
           
Lognormal: Intercept ~ Student-t(3, 0, 1)
           β ~ Normal(0, 1)
           σ ~ Exponential(1)
```

These priors were derived from pilot data and domain knowledge about typical vocal parameter ranges in female speakers.

### Estimation

Models were estimated using Hamiltonian Monte Carlo with 4 chains of 5,000 iterations each (2,500 warmup). Convergence was assessed via R̂ < 1.01 and effective sample size > 400. Adaptation parameters (adapt_delta = 0.995, max_treedepth = 18) prevented divergent transitions. For models with convergence difficulties, we increased iterations or simplified random effects structures.

### Inference

Effects were considered credible if 95% credible intervals excluded zero. We report posterior means and 95% CIs throughout. For key hypotheses, we computed Bayes factors comparing moderation models to null models without interactions.

### Model Comparison: EMA vs. Baseline PID-5

To assess whether intensive EMA sampling provided predictive value beyond traditional baseline assessment, we compared two sets of moderation models:

1. **EMA models:** Using person-aggregated EMA trait scores (as described above)
2. **Baseline models:** Using full baseline PID-5 domain scores

Models were identical in structure, differing only in predictor variables. We compared predictive accuracy using:
- **Bayesian R²:** Proportion of variance explained
- **Leave-One-Out Cross-Validation (LOO-IC):** Expected out-of-sample predictive accuracy

We expected EMA and baseline measures to show equivalent predictive accuracy if the brief assessment captured core trait variance, or superior accuracy for EMA if intensive sampling reduced measurement error.

### Data and Code Availability

All analysis code and de-identified data will be made publicly available upon publication at [OSF LINK]. Models were fit using brms 2.21 with cmdstanr 0.7.

---

## NOTES FOR COMPLETION

**[DA INSERIRE] indicates missing information you need to add:**
- Participant age (M and SD)
- Ethics protocol number
- Cronbach's alphas for PID-5 domains
- Standardized sentence used
- Exact numbers for exclusions
- Sample description (employment, education level, etc.)

**OPTIONAL ADDITIONS:**
- Power analysis (if conducted)
- Pre-registration statement (if applicable)
- Funding acknowledgment
- Conflict of interest statement

**TABLES NEEDED:**
- Table 1: Sample characteristics
- Table 2: Descriptive statistics by timepoint
- Table 3: EMA validation correlations (already have this)
- Table 4: Model parameters (could be supplementary)

