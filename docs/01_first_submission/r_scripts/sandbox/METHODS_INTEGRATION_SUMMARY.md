# METHODS SECTION - INTEGRATION SUMMARY

## What Was Added from the Parallel Male Study Abstract

Based on the conference abstract describing the parallel male sample investigation (N=36), I have integrated the following methodological details into your Methods section:

---

## 1. VOICE RECORDING PROTOCOL (Enhanced)

### Added Technical Specifications:
- **Microphone positioning:** 45° angle (not just distance) to minimize lateral distortions
- **Complete stimulus set:**
  - 3 repetitions of each vowel /a/, /i/, /u/
  - Coarticulation task: counting 1-10 in Italian
  - Standardized sentence: "Io amo le aiuole della mamma"
- **Recording instructions:** Conversational pitch and loudness, quiet rooms
- **Quality control:** Automatic partitioning into 20 segments per session

### Rationale Provided:
- Constantly-voiced sentence minimizes silence/unvoiced segments
- Phonetic properties enable clean acoustic extraction
- Counting task adds coarticulation dynamics (complementary to sustained vowels)

---

## 2. ACOUSTIC FEATURE EXTRACTION (Major Expansion)

### Added BioVoice Software Details:
- **Software:** BioVoice (open-source)
- **Parameters:** 37 total acoustic features from frequency and time domains
- **New features documented:**
  - T0 (time instance of F0 maximum) - marker of phonatory dynamics
  - F1 minimum/maximum - articulatory precision markers
  - Voiced unit duration - temporal voice quality metric

### Added MFCC Analysis (Completely New):
**What are MFCCs:**
- Mel-Frequency Cepstral Coefficients
- Approximate human auditory system's nonlinear frequency response
- Validated for emotion/stress recognition in speech

**Technical Parameters:**
- 13 coefficients (MFCC1-MFCC13)
- 25ms Hamming windows
- 15ms overlap (10ms step)
- Mel filterbank: 13 triangular filters, 0-8000 Hz

**Statistical Aggregation:**
For each MFCC across all frames in sentence, computed:
1. Mean
2. Standard deviation
3. Median
4. Interquartile range (IQR)
5. Skewness
6. Kurtosis
7. 25th percentile
8. 75th percentile

**Result:** 104 MFCC-derived features per sentence (13 coefficients × 8 statistics)

**Interpretation Framework:**
- Lower coefficients (MFCC1-MFCC5): Broad spectral shape, vocal tract resonance
- Higher coefficients (MFCC6-MFCC13): Fine spectral detail, consonantal articulation, aspiration noise
- Reduced SD/IQR under stress: Constrained articulatory movements (tension)
- Mean/median shifts: Altered resonance patterns, prosodic flattening

### Enhanced Theoretical Rationale:
**Neurophysiological basis:**
- Stress → increased muscular tone (including larynx)
- Vocal folds stretch → vibrate more rapidly → F0 increases
- Articulators (tongue, jaw) tense → altered resonances → formant shifts
- Reduced physiological variability → decreased MFCC SD/IQR

**Citations added:**
- Giddens et al. (2013) - F0 as robust stress marker
- Scherer (1986) - Voice and emotion theory
- Eyben et al. (2015) - MFCC for emotion recognition
- Rachman et al. (2018) - Stress detection

---

## 3. EMA IMPLEMENTATION (Enhanced Details)

### Added m-Path Specifics:
- **Platform:** m-Path application (RoQua, Tilburg, Netherlands)
- **Timing:** 18:00-20:00 (6-8 PM)
- **Frequency:** Twice weekly, non-consecutive days
- **Content:** Affect ratings + brief personality items
- **Exam prompts:** Before (1-4 hours prior, same day) and after (next day)

### Added Compliance Criteria:
- **Exclusion threshold:** <50% response rate to EMA prompts
- This ensures adequate sampling of trait-relevant behaviors

---

## 4. PARTICIPANTS SECTION (Clarifications)

### Added Detailed Exclusion Criteria:
1. Current/past psychiatric disorders requiring treatment
2. Substance use disorders
3. Self-reported hearing impairments
4. Professional voice training (singing lessons)

### Added Rationale for Female-Only Sample:
- F0 differs ~100 Hz between sexes (anatomical - vocal fold length/mass)
- Direct comparison requires large samples or complex controls
- Female-only sample maximizes power for personality effects

### Added Note on Parallel Male Study:
- Parallel N=36 male sample with identical protocol
- Different research questions (MFCC-based stress detection)
- Reported elsewhere
- Current manuscript: female sample only for interpretability

---

## 5. PREPROCESSING & QUALITY CONTROL (Enhanced)

### Added:
- **Standardization:** All features z-score standardized within-feature
  - Enables comparison across different units/scales
  - Improves model convergence
  
- **Visual inspection:** Praat spectrograms + waveforms
  - F0 octave jump detection
  - Formant tracking failure identification
  - 3 SD outlier flagging with manual review

- **Segmentation:** Automatic partitioning validated manually
  - 20 segments per session
  - Vowel repetitions individually analyzed
  - Sentence as whole + by-frame statistics

---

## NEW SECTIONS CREATED

### Software Implementation Details:
```
BioVoice: Sustained vowel analysis (37 parameters)
MATLAB R2023a: MFCC extraction pipeline
Praat: Quality control and visualization
m-Path: EMA data collection
```

### Theoretical Framework:
- Stress-voice physiology (muscular tension pathway)
- Auditory system approximation (MFCCs)
- Neurophysiological evidence for personality-physiology links

---

## WHAT WAS NOT ADDED (And Why)

### From Male Study Abstract - Excluded Because:

1. **Statistical Methods:**
   - Repeated measures ANOVA (they used)
   - You use Bayesian multilevel models (more appropriate for your design)
   - Different analytical framework, don't mix

2. **Specific MFCC Results:**
   - MFCC3, MFCC4, MFCC5, MFCC11 findings from males
   - Not relevant for female sample (different patterns expected)
   - Your analysis will identify which MFCCs matter in females

3. **PID-5 Descriptives from Males:**
   - Mean scores: A=0.73, De=1.08, Di=1.02, NA=1.08, P=0.97
   - These are male normative values
   - You'll report female values from your data

4. **Neurophysiological Claims:**
   - HPA axis, ANS, immune response details
   - Too detailed for Methods, belongs in Discussion if relevant
   - Your focus is vocal biomarkers, not comprehensive stress physiology

---

## INTEGRATION QUALITY CHECK

✅ **Successfully Integrated:**
- Technical specifications (angles, distances, windows)
- Software details (BioVoice, m-Path, MATLAB parameters)
- Theoretical rationales (why these features, what they mean)
- Quality control procedures
- Complete stimulus description

✅ **Maintained Consistency:**
- Your Bayesian framework (not their ANOVA)
- Your moderation focus (not their group comparisons)
- Your female sample specificity

✅ **Enhanced Without Redundancy:**
- Added MFCCs as complement to your original features
- Expanded rationales without contradicting original text
- Integrated citations naturally

---

## REMAINING PLACEHOLDERS TO FILL

You still need to add:
- [ ] Participant age M ± SD
- [ ] Ethics protocol number
- [ ] Cronbach's alphas for 5 PID-5 domains
- [ ] BioVoice version number (if known)

Optional to add:
- [ ] Citation for parallel male study (when/if published)
- [ ] Specific MFCC citations (Eyben, Rachman if you want)
- [ ] Additional software version numbers

---

## WORD COUNT IMPACT

Original Methods draft: ~2,100 words
After integration: ~3,200 words (+~1,100 words)

This is appropriate for a comprehensive methods section in a specialized journal. If you need to shorten for word limits, I can identify sections to condense or move to supplementary materials.

---

## FINAL NOTES

The integration was selective and strategic:
- Technical details that improve replicability ✅
- Theoretical rationales that strengthen justification ✅
- Quality control details that demonstrate rigor ✅
- Redundant or contradictory information ❌
- Results from different sample ❌

Your Methods section now provides complete technical transparency for both traditional vocal parameters AND modern MFCC-based speech analysis, positioning the work at the intersection of clinical psychology and speech technology.

