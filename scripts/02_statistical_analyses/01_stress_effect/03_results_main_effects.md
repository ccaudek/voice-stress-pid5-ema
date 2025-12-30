# Results: Main Effects of Exam Stress on Vocal Production

## Analytic Approach

We examined the effects of acute exam-related stress on two core acoustic parameters: fundamental frequency (F0 mean) and normalized noise energy (NNE). Hierarchical Bayesian models were specified to account for the nested structure of repeated acoustic measurements within participants. Two orthogonal contrasts captured distinct phases of the stress response: a *stress contrast* (c₁) comparing pre-exam to baseline recordings, and a *recovery contrast* (c₂) comparing post-exam to pre-exam recordings. Models included random intercepts and random slopes for both contrasts, allowing individual variability in baseline vocal parameters and stress reactivity.

All models were implemented in Stan via rstan and estimated using Hamiltonian Monte Carlo with four chains of 4,000 iterations each (2,000 warmup). Convergence was assessed using R-hat statistics (all < 1.01) and visual inspection of trace plots. Posterior inferences are reported as median estimates with 95% credible intervals (CIs), alongside the posterior probability that each effect is in the hypothesized direction.

---

## Fundamental Frequency (F0 Mean)

### Descriptive Findings

Fundamental frequency exhibited a progressive increase across assessment timepoints. At baseline, mean F0 was [BASELINE_MEAN] Hz (SD = [BASELINE_SD]). This increased to [PRE_MEAN] Hz (SD = [PRE_SD]) immediately before the exam, representing a raw increase of [DIFF_PRE_BASELINE] Hz. Following the exam, F0 remained elevated at [POST_MEAN] Hz (SD = [POST_SD]), showing an additional increase of [DIFF_POST_PRE] Hz from the pre-exam assessment.

### Model Results

The hierarchical Bayesian model confirmed robust stress-related elevation in fundamental frequency. The intercept parameter (α), representing estimated F0 at baseline, had a posterior median of [ALPHA_MEDIAN] Hz (MAD = [ALPHA_MAD], 95% CI [ALPHA_LOWER, ALPHA_UPPER]). 

The stress contrast (β₁) revealed a clear positive effect: F0 increased by [B1_MEDIAN] Hz (MAD = [B1_MAD], 95% CI [B1_LOWER, B1_UPPER]) when comparing pre-exam to baseline recordings. This effect was highly consistent across posterior samples, with P(β₁ > 0) = [B1_PROB]. This finding indicates that acute academic stress reliably elevates vocal pitch, consistent with increased laryngeal tension and autonomic arousal.

The recovery contrast (β₂) was [B2_DIRECTION], with a median estimate of [B2_MEDIAN] Hz (MAD = [B2_MAD], 95% CI [B2_LOWER, B2_UPPER], P(β₂ > 0) = [B2_PROB]). [INTERPRET_B2_F0: If positive and strong: "This suggests that F0 remained elevated even after exam completion, indicating sustained physiological arousal during the immediate post-exam period." If near zero: "This indicates that F0 plateaued after the exam, with minimal further change during the recovery phase." If negative: "This suggests a partial normalization of F0 following exam completion, though levels remained above baseline."]

Between-person variability was substantial, as evidenced by the standard deviation of random intercepts (τ₁ = [TAU1_MEDIAN], 95% CI [TAU1_LOWER, TAU1_UPPER]) and random slopes for the stress contrast (τ₂ = [TAU2_MEDIAN], 95% CI [TAU2_LOWER, TAU2_UPPER]). The residual standard deviation was σ = [SIGMA_MEDIAN] Hz (95% CI [SIGMA_LOWER, SIGMA_UPPER]), reflecting within-person measurement variability.

---

## Normalized Noise Energy (NNE)

### Descriptive Findings

In contrast to F0, NNE exhibited a pattern consistent with reduced glottal noise under stress. At baseline, mean NNE was [BASELINE_NNE] dB (SD = [BASELINE_NNE_SD]). This decreased to [PRE_NNE] dB (SD = [PRE_NNE_SD]) at the pre-exam assessment, reflecting a reduction of [DIFF_NNE_PRE_BASELINE] dB. Post-exam values were [POST_NNE] dB (SD = [POST_NNE_SD]), showing [DESCRIBE_POST_NNE_CHANGE].

### Model Results

The hierarchical model for NNE confirmed a systematic reduction in glottal noise under acute stress. The intercept parameter (α) had a posterior median of [ALPHA_NNE_MEDIAN] dB (MAD = [ALPHA_NNE_MAD], 95% CI [ALPHA_NNE_LOWER, ALPHA_NNE_UPPER]).

Critically, the stress contrast (β₁) showed a robust negative effect: NNE decreased by [B1_NNE_MEDIAN] dB (MAD = [B1_NNE_MAD], 95% CI [B1_NNE_LOWER, B1_NNE_UPPER]) when comparing pre-exam to baseline recordings. The posterior probability that this effect was negative was P(β₁ < 0) = [B1_NNE_PROB], providing strong evidence for stress-induced reduction in glottal noise. More negative NNE values indicate a more periodic, harmonically stable signal, suggesting that acute stress does not destabilize phonation but instead promotes a "cleaner," albeit potentially more effortful, vocal quality.

The recovery contrast (β₂) had a median estimate of [B2_NNE_MEDIAN] dB (MAD = [B2_NNE_MAD], 95% CI [B2_NNE_LOWER, B2_NNE_UPPER], P(β₂ > 0) = [B2_NNE_PROB]). [INTERPRET_B2_NNE: If positive and strong: "This indicates a partial reversal of the stress-induced reduction, with NNE beginning to normalize following exam completion." If near zero: "This suggests that the stress-induced reduction in NNE persisted through the immediate post-exam period." If negative: "This indicates a continued reduction in glottal noise even after the exam."]

Random effects estimates revealed considerable between-person heterogeneity in baseline NNE (τ₁ = [TAU1_NNE_MEDIAN], 95% CI [TAU1_NNE_LOWER, TAU1_NNE_UPPER]) and in stress-related change (τ₂ = [TAU2_NNE_MEDIAN], 95% CI [TAU2_NNE_LOWER, TAU2_NNE_UPPER]). Residual variability was σ = [SIGMA_NNE_MEDIAN] dB (95% CI [SIGMA_NNE_LOWER, SIGMA_NNE_UPPER]).

---

## Summary of Main Effects

Exam-related stress produced dissociable changes in vocal production. Fundamental frequency increased robustly under stress, reflecting heightened autonomic arousal and laryngeal tension. In contrast, NNE decreased, indicating reduced glottal noise and a more controlled, periodic phonatory signal. These patterns suggest that acute stress does not simply destabilize the voice but instead induces simultaneous increases in physiological arousal (indexed by F0) and compensatory control (indexed by reduced noise). The consistent directionality and large effect sizes for both parameters underscore the reliability of these vocal signatures of stress, which were observed across individuals despite substantial between-person variability in baseline vocal characteristics and stress reactivity.

---

**Note for authors:** Replace placeholder values in brackets (e.g., [BASELINE_MEAN]) with actual numerical results from the fitted models. The text is structured to accommodate varying results for the recovery contrast (β₂), which may show different patterns depending on your data.
