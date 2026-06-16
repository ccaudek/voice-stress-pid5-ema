# Connected-Speech (MFCC) Analysis — Text for Manuscript and Supplement

*Placeholders are marked `[PLACEHOLDER: …]` for the details you will complete: feature-extraction parameters, and the participant/observation counts with their reconciliation against the main analytic sample. Cross-references (Table S[X], Figure S[Y], etc.) use brackets to be renumbered into your supplement.*

---

## A. Main manuscript text

> Suggested placement: a short subsection at the end of the Results (after the personality-moderation results), or as a robustness paragraph in the Discussion. The full apparatus stays in the Supplement.

**Robustness check: connected-speech spectral features.** Our primary analyses focused on sustained vowels, which provide acoustically stable productions with a direct mapping onto the two mechanisms of interest—autonomic arousal (F0) and phonatory control (NNE). At each session, however, participants also produced a standardized connected-speech sample (see Methods), and establishing whether the stress-related changes extend beyond the controlled vowel task is important for evaluating the generality of the findings. We therefore analyzed mel-frequency cepstral coefficients (MFCCs), a holistic descriptor of the short-term spectral envelope that reflects overall vocal-tract configuration and timbre rather than any single physiological channel. We modeled the 13 coefficient means jointly in a multivariate hierarchical Bayesian model that used the same stress and recovery contrasts and the same subject-level random-effects logic as the F0 and NNE models ([PLACEHOLDER: N_MFCC] participants with complete recordings; full specification, diagnostics, and results in Supplement S[X]).

The connected-speech spectral profile showed a modest stress-related shift during anticipatory stress (multivariate omnibus *p* ≈ .02–.05 across two complementary tests), with the largest coefficient-level posterior summaries for MFCC2 (+0.22 SD, 89% CrI [0.07, 0.37]) and MFCC4 (−0.25 SD, 89% CrI [−0.42, −0.09]) and weaker support for MFCC3 (+0.20 SD, 89% CrI [0.03, 0.36]); the recovery phase did not show a comparable multivariate shift. Consistent with the vowel-based findings, this stress signal was weaker and more diffusely distributed across coefficients than the F0 and NNE effects obtained from sustained vowels.

This pattern indicates that the stress-related vocal signature is not an artifact of the sustained-vowel task. It also makes explicit a trade-off between signal quality and ecological validity: the controlled vowel productions yield a cleaner, mechanistically interpretable stress signal, whereas the connected-speech sample—although closer to everyday speech—carries a weaker and less localized one. We therefore retain sustained-vowel F0 and NNE as the primary outcomes and treat the connected-speech analysis as a supporting robustness check.

> Optional sentence for the Limitations paragraph: The connected-speech material was itself a standardized sentence rather than spontaneous speech; determining whether these spectral effects generalize to fully naturalistic speech will require denser, less constrained sampling.

---

## B. Supplementary materials text

> Suggested heading and placement: a new section in the Supplement, parallel to the existing F0/NNE supplementary sections.

### Connected-Speech Spectral Features: Mel-Frequency Cepstral Coefficient (MFCC) Analysis

**Rationale and feature extraction.** In addition to the sustained vowels analyzed in the main text, participants produced a standardized connected-speech sample at each of the three sessions. To assess whether exam-related stress affects the voice beyond the controlled vowel task, we analyzed mel-frequency cepstral coefficients (MFCCs), which summarize the short-term spectral envelope of the signal and are widely used to characterize connected speech. MFCCs index global spectral shape (vocal-tract configuration, timbre) rather than the specific phonatory mechanisms captured by F0 and NNE, and therefore provide a complementary, holistic probe of stress-related vocal change.

[PLACEHOLDER: MFCC extraction details — connected-speech material used (e.g., the standardized sentence *"Io amo le aiuole della mamma"* and/or the counting task); software and version; sampling rate; frame length and hop size; pre-emphasis and windowing; number of coefficients (13); and the summary statistics computed per coefficient.] For each coefficient we used the per-session mean across analysis frames as the primary representation; this is the canonical MFCC summary and was specified a priori. Higher-order distributional statistics (standard deviation, skewness, kurtosis, interquartile range, percentiles) were extracted but were not modeled, because they are noisier and lack a clear interpretive mapping—paralleling the rationale for restricting the sustained-vowel analyses to F0 and NNE.

[PLACEHOLDER: Sample for this analysis — [N_MFCC] participants contributed usable connected-speech recordings at all three sessions ([N_OBS] = [N_MFCC] × 3 observations). Reconcile this count with the main analytic sample (N = 119) and with the recruited sample, and state the reason for any difference (e.g., connected-speech recordings excluded for file quality or background noise).]

**Model specification.** The 13 coefficient means were standardized (z-scored across all observations) so that effects are expressed in SD units and the priors operate on a common scale. The outcome vector was modeled with a multivariate normal likelihood,

> y_n ∼ MultiNormal(μ_n, Σ), with μ_n = α + u_{s[n]} + β₁·c1_n + β₂·c2_n,

where α, β₁, and β₂ are 13-dimensional vectors; c1 (stress: pre vs. baseline) and c2 (recovery: post vs. pre) are the same orthogonal contrasts used for F0 and NNE; u_s is a participant-specific random intercept; and Σ is the residual covariance. We included a subject-level random intercept only. With three observations per participant and a 13-dimensional outcome, random slopes for the contrasts (39 random effects per participant) are not identifiable; the 13-dimensional random intercept absorbs stable between-person differences in the spectral profile as well as the within-person dependence across the three sessions. The random-intercept and residual covariances were parameterized via their Cholesky factors with LKJ(2) priors on the correlations, and random effects used a non-centered parameterization. Priors were weakly informative on the standardized scale: α ∼ Normal(0, 2); β₁, β₂ ∼ Normal(0, 1), providing mild regularization toward zero that guards against the multiplicity inherent in estimating effects for 13 coefficients; and the covariance scales ∼ Exponential(1). The model was estimated in Stan via cmdstanr ([PLACEHOLDER: chains × iterations]).

**Convergence and fit.** All R̂ values equaled 1.00 (≤ 1.01 throughout), bulk- and tail-ESS were in the thousands for all parameters, and there were no divergent transitions or treedepth saturations. Posterior predictive checks (Figure S[Y]) indicated that the model reproduced the distributional features of the observed coefficients.

**Omnibus tests.** We evaluated whether the MFCC profile shifts with each contrast using two complementary multivariate tests. (i) A one-sample Hotelling's *T*² on the within-person difference vectors indicated a modest stress-related profile shift (*F*(13, 114) = 2.00, *p* = .027; sign-flip permutation *p* = .024) and little evidence for a comparable recovery shift (*F*(13, 114) = 1.56, *p* = .108; permutation *p* = .105). (ii) A Bayesian posterior Wald test—the Mahalanobis distance of the posterior-mean contrast vector from zero in the metric of its posterior covariance, referred to a χ² distribution with 13 degrees of freedom—showed a consistent pattern (stress: *M* = 22.3, *p* = .050; recovery: *M* = 10.5, *p* = .655). The two tests use different reference covariances (the covariance of difference scores for Hotelling's *T*²; the model's posterior covariance, which incorporates the hierarchical structure and shrinkage, for the Wald test), which accounts for the small difference between the two stress *p*-values; taken together, they support a modest stress-related spectral shift and do not support a comparable recovery-phase shift.

We deliberately did not use two otherwise-common approaches, for principled reasons. First, we did not report a Bayes-factor omnibus (e.g., bridge sampling or the Savage–Dickey density ratio): a point-null Bayes factor over a 13-dimensional coefficient vector against weakly-informative priors is dominated by the Occam factor (the Lindley–Bartlett paradox) and is strongly prior-dependent, so with modest per-coefficient effects it can favor the null for reasons of dimensionality rather than evidence; bridge-sampling estimates of the marginal likelihood are also unstable in the high-dimensional parameter space of this model. Second, we did not report PSIS-LOO model comparison: with a 13-dimensional subject-level random intercept and only three observations per participant, leave-one-observation-out cross-validation is unreliable, because removing a single observation strongly perturbs that participant's latent profile and produces heavy-tailed importance ratios (p_loo ≈ 1172 on [PLACEHOLDER: N_OBS] observations, with the majority of Pareto-k̂ diagnostics exceeding 0.7).

**Per-coefficient effects.** Table S[Z] reports the stress (β₁) and recovery (β₂) effects for each coefficient (posterior median, 89% CrI, and probability of direction, PD). For the stress contrast, MFCC2 and MFCC4 showed the strongest directional certainty (PD ≥ .99), and MFCC3 also showed high directional certainty (PD = .97); the remaining coefficients showed weaker directional certainty (PD < .95). For the recovery contrast, the omnibus profile shift was not comparably supported; at the coefficient level, MFCC10 showed the strongest directional certainty (PD = .97), but this isolated pattern should be interpreted cautiously.

**Multivariate effect size.** As a magnitude descriptor we computed the Mahalanobis length of each contrast vector in the residual metric, *D* = √(β′ Σ⁻¹ β): stress *D* = 0.87 (89% CrI [0.65, 1.09]); recovery *D* = 0.70 (89% CrI [0.50, 0.90]). Because *D* is nonnegative, its posterior interval does not approach zero even under a null shift; *D* should therefore be interpreted as an effect-size magnitude—the stress shift is larger than the recovery shift—whereas evidence for a profile shift is summarized by the omnibus tests and coefficient-level posterior summaries above. *D* is expressed relative to the within-occasion residual covariance and is consequently larger than the difference-score-based Hotelling effect, which uses the covariance of between-session differences.

**No personality moderation on connected speech.** Given the modest and diffusely distributed connected-speech main effect, we did not test PID-5 moderation of the MFCC features. Such an analysis would involve 13 coefficients × five trait domains × two contrasts; it would be substantially underpowered and would reintroduce precisely the analytic flexibility we sought to avoid, and any resulting interactions would be uninterpretable. Personality moderation was therefore restricted to the sustained-vowel outcomes (F0 and NNE), for which the stress-related changes were strongest and mechanistically interpretable.

**Data and code.** All R and Stan scripts and the de-identified data for the connected-speech analysis are available at [PLACEHOLDER: OSF LINK].

---

### Table S[Z] (template) — Per-coefficient stress and recovery effects (connected-speech MFCCs)

| Coefficient | Stress β₁ (median) | 89% CrI | PD | Recovery β₂ (median) | 89% CrI | PD |
|---|---|---|---|---|---|---|
| MFCC1 | −0.02 | [−0.15, 0.11] | .60 | −0.02 | [−0.14, 0.10] | .61 |
| MFCC2 | 0.22 | [0.07, 0.37] | .99 | 0.10 | [−0.04, 0.25] | .87 |
| MFCC3 | 0.20 | [0.03, 0.36] | .97 | 0.01 | [−0.15, 0.18] | .55 |
| MFCC4 | −0.25 | [−0.42, −0.09] | .99 | −0.16 | [−0.33, 0.00] | .94 |
| MFCC5 | 0.09 | [−0.08, 0.25] | .80 | −0.01 | [−0.18, 0.16] | .53 |
| MFCC6 | 0.16 | [−0.02, 0.34] | .92 | 0.09 | [−0.09, 0.27] | .78 |
| MFCC7 | 0.05 | [−0.13, 0.24] | .68 | 0.08 | [−0.10, 0.27] | .76 |
| MFCC8 | 0.05 | [−0.11, 0.22] | .70 | −0.12 | [−0.28, 0.05] | .87 |
| MFCC9 | −0.12 | [−0.29, 0.06] | .85 | 0.00 | [−0.18, 0.18] | .50 |
| MFCC10 | 0.10 | [−0.06, 0.26] | .84 | −0.19 | [−0.35, −0.03] | .97 |
| MFCC11 | −0.08 | [−0.28, 0.11] | .75 | −0.09 | [−0.29, 0.10] | .77 |
| MFCC12 | −0.13 | [−0.30, 0.03] | .90 | −0.04 | [−0.21, 0.12] | .67 |
| MFCC13 | 0.05 | [−0.13, 0.23] | .66 | −0.04 | [−0.23, 0.14] | .65 |

*Note.* Effects are in SD units of the standardized coefficient. PD = probability of direction. CrI = 89% credible interval. β₁ = stress contrast (pre vs. baseline); β₂ = recovery contrast (post vs. pre). Values are reported descriptively and should be read together with the multivariate omnibus summaries rather than as separate decision rules.
