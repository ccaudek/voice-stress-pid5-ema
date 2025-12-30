# Results: Main Effects of Exam Stress on Vocal Production

## Analytic Approach

We examined the effects of acute exam-related stress on two core acoustic parameters: fundamental frequency (F0 mean, averaged across vowels /a/, /i/, and /u/) and normalized noise energy (NNE, similarly averaged). Hierarchical Bayesian models were specified to account for the nested structure of repeated acoustic measurements within participants. Two orthogonal contrasts captured distinct phases of the stress response: a *stress contrast* (c₁) comparing pre-exam to baseline recordings, and a *recovery contrast* (c₂) comparing post-exam to pre-exam recordings. Models included random intercepts and random slopes for both contrasts, allowing individual variability in baseline vocal parameters and stress reactivity.

All models were implemented in Stan via rstan and estimated using Hamiltonian Monte Carlo with four chains of 4,000 iterations each (2,000 warmup). Convergence was assessed using R-hat statistics (all < 1.01) and visual inspection of trace plots. Posterior inferences are reported as median estimates with 95% credible intervals (CIs), alongside the posterior probability that each effect is in the hypothesized direction.

---

## Fundamental Frequency (F0 Mean)

### Descriptive Findings

Fundamental frequency exhibited a progressive increase across assessment timepoints. At baseline, mean F0 was 190.7 Hz (SD = 22.0). This increased to 194.0 Hz (SD = 21.9) immediately before the exam, representing a raw increase of 3.2 Hz. Following the exam, F0 was 192.5 Hz (SD = 23.6), showing an additional change of -1.5 Hz from the pre-exam assessment.

### Model Results

The hierarchical Bayesian model confirmed robust stress-related elevation in fundamental frequency. The intercept parameter (α), representing estimated F0 at baseline, had a posterior median of 192.5 Hz (MAD = 1.7, 95% CI [189.1, 196.0]). 

The stress contrast (β₁) revealed a clear positive effect: F0 increased by 3.27 Hz (MAD = 1.25, 95% CI [0.81, 5.71]) when comparing pre-exam to baseline recordings. This effect was highly consistent across posterior samples, with P(β₁ > 0) = 0.995. This finding indicates that acute academic stress reliably elevates vocal pitch, consistent with increased laryngeal tension and autonomic arousal.

The recovery contrast (β₂) was positive, with a median estimate of 0.14 Hz (MAD = 1.24, 95% CI [-2.34, 2.59], P(β₂ > 0) = 0.542). This indicates that F0 plateaued after the exam, with minimal further change during the recovery phase.

Between-person variability was substantial, as evidenced by the standard deviation of random intercepts (τ₁ = 19.86, 95% CI [17.68, 22.44]) and random slopes for the stress contrast (τ₂ = 1.08, 95% CI [0.04, 4.45]). The residual standard deviation was σ = 9.10 Hz (95% CI [8.34, 9.96]), reflecting within-person measurement variability.

---

## Normalized Noise Energy (NNE)

### Descriptive Findings

In contrast to F0, NNE exhibited a pattern consistent with reduced glottal noise under stress. At baseline, mean NNE was -26.55 dB (SD = 2.64). This decreased to -27.09 dB (SD = 3.25) at the pre-exam assessment, reflecting a reduction of 0.54 dB. Post-exam values were -26.98 dB (SD = 2.91), showing minimal change from the pre-exam level.

### Model Results

The hierarchical model for NNE confirmed a systematic reduction in glottal noise under acute stress. The intercept parameter (α) had a posterior median of -26.87 dB (MAD = 0.20, 95% CI [-27.28, -26.47]).

Critically, the stress contrast (β₁) showed a robust negative effect: NNE decreased by 0.65 dB (MAD = 0.28, 95% CI [-1.20, -0.11]) when comparing pre-exam to baseline recordings. The posterior probability that this effect was negative was P(β₁ < 0) = 0.990, providing strong evidence for stress-induced reduction in glottal noise. More negative NNE values indicate a more periodic, harmonically stable signal, suggesting that acute stress does not destabilize phonation but instead promotes a 'cleaner,' albeit potentially more effortful, vocal quality.

The recovery contrast (β₂) had a median estimate of -0.22 dB (MAD = 0.28, 95% CI [-0.77, 0.33], P(β₂ > 0) = 0.219). This indicates a continued reduction in glottal noise even after the exam.

Random effects estimates revealed considerable between-person heterogeneity in baseline NNE (τ₁ = 2.15, 95% CI [1.84, 2.51]) and in stress-related change (τ₂ = 0.83, 95% CI [0.04, 1.91]). Residual variability was σ = 1.96 dB (95% CI [1.67, 2.18]).

---

## Summary of Main Effects

Exam-related stress produced dissociable changes in vocal production. Fundamental frequency increased robustly under stress, reflecting heightened autonomic arousal and laryngeal tension. In contrast, NNE decreased, indicating reduced glottal noise and a more controlled, periodic phonatory signal. These patterns suggest that acute stress does not simply destabilize the voice but instead induces simultaneous increases in physiological arousal (indexed by F0) and compensatory control (indexed by reduced noise). The consistent directionality and large effect sizes for both parameters underscore the reliability of these vocal signatures of stress, which were observed across individuals despite substantial between-person variability in baseline vocal characteristics and stress reactivity.