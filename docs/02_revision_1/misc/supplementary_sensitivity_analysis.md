## Supplementary Sensitivity Analysis for Main Effects: Robust Student-t Models

To assess whether the main findings were sensitive to the Gaussian residual assumption and to the influence of potentially outlying observations, we fitted robust versions of the main-effects models for both acoustic outcomes: mean fundamental frequency, F0, and normalized noise energy, NNE. The robust models retained the same fixed-effect structure as the primary models, with two planned contrasts: stress, comparing PRE with BASELINE, and recovery, comparing POST with PRE. The hierarchical structure was also retained, but the subject-level random intercepts and random slopes were modeled as correlated random effects. The residual likelihood was changed from Gaussian to Student-t with fixed degrees of freedom, ν = 4, providing heavier tails and therefore reduced sensitivity to influential observations.

The robust models were fitted as sensitivity analyses rather than as entirely separate substantive models. Thus, the key question was whether the direction, magnitude, and uncertainty of the stress and recovery effects remained consistent with the original Gaussian models, and whether the robust specification improved predictive diagnostics.

We report posterior medians, MADs, and central 89% credible intervals, computed as the 5.5th and 94.5th posterior percentiles.

### F0 model

For F0, the original Gaussian model estimated a positive stress effect, with a median effect of 3.27 Hz, MAD = 1.25, and an 89% credible interval from 1.25 to 5.27 Hz. The posterior probability that the stress effect was positive was 0.995. In the robust Student-t model, the estimated stress effect was smaller, with a median of 2.30 Hz, MAD = 1.14, and an 89% credible interval from 0.47 to 4.16 Hz. The posterior probability that the stress effect was positive remained high, pd = 0.979.

Thus, the robust model attenuated the estimated F0 stress effect by approximately 0.97 Hz, but the effect remained positive and credibly different from zero under the 89% credible interval.

For the recovery contrast, the original Gaussian model showed essentially no clear effect, median = 0.14 Hz, MAD = 1.24, 89% CI [-1.89, 2.11], with P(b2 > 0) = 0.542. The robust model estimated a slightly negative recovery effect, median = -0.39 Hz, MAD = 1.14, 89% CI [-2.21, 1.41], with P(b2 > 0) = 0.368. This result remains compatible with no reliable recovery effect.

| Outcome |            Model | Parameter    | Median |  MAD |        89% CI | Directional probability |
| ------- | ---------------: | ------------ | -----: | ---: | ------------: | ----------------------: |
| F0      |         Gaussian | Stress, b1   |   3.27 | 1.25 |  [1.25, 5.27] |       P(b1 > 0) = 0.995 |
| F0      | Robust Student-t | Stress, b1   |   2.30 | 1.14 |  [0.47, 4.16] |       P(b1 > 0) = 0.979 |
| F0      |         Gaussian | Recovery, b2 |   0.14 | 1.24 | [-1.89, 2.11] |       P(b2 > 0) = 0.542 |
| F0      | Robust Student-t | Recovery, b2 |  -0.39 | 1.14 | [-2.21, 1.41] |       P(b2 > 0) = 0.368 |

The residual scale parameter was lower in the robust model, median σ = 6.52, 89% CI [5.93, 7.16]. Because this is the scale parameter of a Student-t likelihood rather than a Gaussian residual standard deviation, it should not be interpreted as directly equivalent to the Gaussian residual standard deviation.

### NNE model

For NNE, the original Gaussian model estimated a negative stress effect, median = -0.65 dB, MAD = 0.28, 89% CI [-1.10, -0.21], with P(b1 < 0) = 0.990. The robust Student-t model produced a somewhat larger negative estimate, median = -0.79 dB, MAD = 0.27, 89% CI [-1.21, -0.36], with P(b1 < 0) = 0.998.

Thus, the NNE stress effect was not weakened by the robust specification. Instead, the robust model slightly strengthened the estimated negative stress-related change in NNE.

For the recovery contrast, the original Gaussian model estimated a small negative effect, median = -0.22 dB, MAD = 0.28, 89% CI [-0.67, 0.23], with P(b2 > 0) = 0.219. The robust model estimated a somewhat more negative effect, median = -0.32 dB, MAD = 0.25, 89% CI [-0.73, 0.09], with P(b2 > 0) = 0.105. Although the interval still included zero, the robust model shifted the recovery estimate in a more negative direction.

| Outcome |            Model | Parameter    | Median |  MAD |         89% CI | Directional probability |
| ------- | ---------------: | ------------ | -----: | ---: | -------------: | ----------------------: |
| NNE     |         Gaussian | Stress, b1   |  -0.65 | 0.28 | [-1.10, -0.21] |       P(b1 < 0) = 0.990 |
| NNE     | Robust Student-t | Stress, b1   |  -0.79 | 0.27 | [-1.21, -0.36] |       P(b1 < 0) = 0.998 |
| NNE     |         Gaussian | Recovery, b2 |  -0.22 | 0.28 |  [-0.67, 0.23] |       P(b2 > 0) = 0.219 |
| NNE     | Robust Student-t | Recovery, b2 |  -0.32 | 0.25 |  [-0.73, 0.09] |       P(b2 > 0) = 0.105 |

The Student-t residual scale for NNE was median σ = 1.44, 89% CI [1.28, 1.60]. As for F0, this parameter indexes the scale of the Student-t residual distribution and is not directly comparable to the Gaussian residual standard deviation.

### Predictive comparison using PSIS-LOO

The robust models improved the approximate leave-one-observation-out cross-validation diagnostics. In the original Gaussian models, some observations had problematic Pareto-k values, indicating sensitivity to influential observations. In contrast, both robust Student-t models had all Pareto-k estimates below 0.7, indicating reliable PSIS-LOO estimates.

For F0, the robust model had better expected log predictive density than the Gaussian model. The LOO comparison favored the robust model by 16.3 elpd units, SE = 5.9. This suggests a clear improvement in expected out-of-sample predictive performance.

For NNE, the robust model was also preferred, with an elpd improvement of 12.9 units, SE = 6.9. This provides weaker but still meaningful evidence in favor of the robust specification.

| Outcome | Preferred model  | elpd difference for Gaussian vs robust | SE difference | Interpretation                    |
| ------- | ---------------- | -------------------------------------: | ------------: | --------------------------------- |
| F0      | Robust Student-t |                                  -16.3 |           5.9 | Robust model clearly preferred    |
| NNE     | Robust Student-t |                                  -12.9 |           6.9 | Robust model moderately preferred |

Here, negative elpd differences indicate worse expected predictive accuracy for the Gaussian model relative to the robust Student-t model.

### Interpretation

Overall, the sensitivity analysis supports the robustness of the main inferential conclusions. The stress effect remained positive for F0 and negative for NNE under the robust Student-t specification. The F0 stress effect was attenuated but remained directionally strong and credibly positive. The NNE stress effect was slightly strengthened, with very high posterior probability for a negative effect.

The recovery effects were less stable and remained uncertain in both outcomes. For F0, the recovery effect was close to zero in the Gaussian model and slightly negative in the robust model, with credible intervals spanning zero in both cases. For NNE, the recovery effect became more negative in the robust model, but uncertainty remained sufficient that this effect should be interpreted cautiously.

The robust models also improved predictive diagnostics. The elimination of problematic Pareto-k values suggests that the Student-t likelihood successfully reduced the influence of observations that were poorly handled by the Gaussian residual model. Because the substantive conclusions for the stress contrast were preserved while predictive diagnostics improved, the robust Student-t models provide a useful confirmation of the primary findings and may be preferred for sensitivity reporting.

### Suggested manuscript wording

> As a sensitivity analysis, we refitted the main-effects models using a robust Student-t residual likelihood with ν = 4 and correlated subject-level random effects. This specification was intended to assess whether the Gaussian models were sensitive to influential observations. For F0, the robust model estimated a positive stress effect, median = 2.30 Hz, MAD = 1.14, 89% CI [0.47, 4.16], P(b1 > 0) = 0.979, compared with median = 3.27 Hz, MAD = 1.25, 89% CI [1.25, 5.27], P(b1 > 0) = 0.995 in the Gaussian model. For NNE, the robust model estimated a negative stress effect, median = -0.79 dB, MAD = 0.27, 89% CI [-1.21, -0.36], P(b1 < 0) = 0.998, compared with median = -0.65 dB, MAD = 0.28, 89% CI [-1.10, -0.21], P(b1 < 0) = 0.990 in the Gaussian model. Recovery effects remained uncertain in both outcomes. PSIS-LOO comparisons favored the robust models over the Gaussian models for both F0, elpd difference = 16.3, SE = 5.9, and NNE, elpd difference = 12.9, SE = 6.9. Moreover, all Pareto-k estimates were below 0.7 in the robust models, indicating reliable LOO estimates. These results suggest that the main stress effects are robust to a heavy-tailed residual specification and that the Student-t models provide improved predictive diagnostics.
