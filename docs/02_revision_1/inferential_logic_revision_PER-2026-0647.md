# Inferential logic — revision text for Ms. PER-2026-0647

Ready-to-paste English text implementing the unified, non-dichotomous Bayesian
inferential framework (probability of direction as the primary index; the credible
interval reported as a measure of *magnitude uncertainty*, not a test; 89% equal-tailed
width chosen as a deliberately arbitrary descriptive convention following McElreath).

**Before pasting — recompute the intervals.** All credible intervals must be regenerated
uniformly as **89% equal-tailed** posterior intervals, i.e.
`posterior::quantile2(.x, probs = c(0.055, 0.945))`, from the saved draws. The F0 values
filled in below come from your fresh refit via a normal approximation
(median ± 1.598·SD); for the near-symmetric γ posteriors they will match the exact
5.5/94.5 quantiles to ~0.01–0.05 Hz, but replace them with the exact numbers. The **NNE**
intervals are not yet available in this session (no NNE refit run here): the bracketed
values in §4 are approximations to be replaced by the exact 89% quantiles from the NNE
model.

---

## 1. Supplement — new subsection: "Inferential Framework and Reporting"

*(Place near the top of the Statistical Models section of the Supplement, before the
model specifications; cross-reference it from the main-text* Inference *subsection.)*

### Inferential Framework and Reporting

All analyses were conducted within a Bayesian estimation framework, and we report and
interpret full posterior distributions rather than dichotomous accept/reject decisions
about individual parameters. Our inferential aim was to characterize the *direction*,
*relative strength*, and *uncertainty* of the estimated associations and to interpret the
overall pattern of results across parameters. For an exploratory study with a modest
sample, this is the scientifically informative output of the analysis, whereas a binary
verdict on each parameter would discard most of that information and invite the very
significance-testing logic that a Bayesian treatment is meant to supersede
(Kruschke & Liddell, 2018; McElreath, 2020).

We summarize each parameter with three complementary quantities.

1. **Posterior median** — a point summary of the most probable parameter value given the
   model and the data.

2. **Probability of direction (pd)** — the posterior probability that the parameter takes
   its more probable sign, pd = max[P(θ > 0), P(θ < 0)] (Makowski et al., 2019). The pd is
   our primary index of evidence. It ranges from .50 (the sign is completely uncertain) to
   1.00 (the sign is effectively certain) and has a direct verbal reading: a pd of .97
   means that, given the model and data, there is a 97% posterior probability that the
   effect lies in the stated direction. This is a probability statement *about the
   parameter*, which is available only in the Bayesian framework; under the frequentist
   framework the parameter is a fixed unknown and no probability can be attached to it. We
   treat the pd as a *continuous* index of directional evidence and deliberately impose no
   threshold above which an effect is declared "present"; we report it for every parameter
   and interpret the gradient of values.

3. **89% credible interval (equal-tailed)** — the central 89% of the posterior, reported
   as a summary of the *magnitude uncertainty* of the estimate. This interval is **not** a
   significance test: whether or not it includes zero carries no special inferential
   status, and we do not interpret it that way. The 89% width (rather than 95%) is chosen
   deliberately and follows McElreath (2020): any interval width is arbitrary, and using a
   value other than 95% signals that the number is a descriptive convenience rather than a
   decision boundary, discouraging the reflexive mapping of "the 95% interval excludes
   zero" onto "p < .05." A 50% interval, or any other width, would serve the same
   descriptive purpose; what matters is the location and spread of the posterior mass, not
   a cutoff.

Two consequences of this framework are worth making explicit. First, **we draw conclusions
from the pattern of results across the full set of parameters, not from the status of any
single estimate.** The claim that personality moderation of vocal stress responses is
domain- and phase-specific rests on the *joint* configuration of directional probabilities
across the ten F0 and ten NNE moderation parameters, not on any one interaction crossing a
threshold. Second, **direction is better identified than magnitude.** As shown in the
prior-sensitivity analysis (Supplement, *Prior-Sensitivity Analysis*), the sign and
directional probability of the moderation effects are robust across prior specifications,
whereas their point magnitudes are only weakly constrained by the present sample. We
therefore foreground direction and directional evidence (pd) and treat all reported
magnitudes — and the widths of their credible intervals — as estimates carrying
substantial uncertainty (Gelman & Carlin, 2014).

We did not use Bayes factors or region-of-practical-equivalence (ROPE) decision rules.
Bayes factors reintroduce a thresholded, model-selection logic and are sensitive to the
prior on the parameter under test — undesirable here, since the prior-sensitivity analysis
shows the moderation magnitudes to be prior-dependent. A ROPE would require specifying a
region of practical equivalence in the original vocal units (Hz, dB), for which there is no
established basis in this novel measurement context. The probability of direction provides
a more transparent and assumption-light summary of the evidence for our exploratory,
directional questions.

---

## 2. Main text — replacement for the *Inference* subsection (≈ par. 53)

*(Replaces the current text beginning "Effects were considered credible if 95% credible
intervals excluded zero…". Removes the credible-interval decision rule and the Bayes-factor
sentence.)*

**Inference.** All analyses are Bayesian, and we report and interpret full posterior
distributions rather than dichotomous test outcomes. For each parameter we report the
posterior median, the probability of direction (pd; the posterior probability that the
parameter takes its more probable sign), and an 89% equal-tailed credible interval. The pd
is our primary index of evidence and is interpreted as a continuous measure of directional
certainty, not as a threshold to be crossed; it is a probability statement about the
parameter that has no frequentist counterpart (Makowski et al., 2019; McElreath, 2020). The
credible interval summarizes uncertainty about the *magnitude* of an effect and is not used
as a significance test — its inclusion or exclusion of zero carries no special status, and
the 89% width is an explicitly arbitrary descriptive choice (McElreath, 2020). We interpret
the overall pattern of directional evidence across parameters rather than the status of any
single estimate and, because the present sample constrains effect magnitudes only loosely
(Supplement, *Prior-Sensitivity Analysis*), we foreground the direction and strength of
associations over their precise size. Full details of the inferential framework are given
in the Supplement.

---

## 3. The probability of direction is not a *p*-value

*(Use in the response letter under R2.11, and/or as a short paragraph in the Supplement
subsection above. It pre-empts the predictable "isn't pd just 1 − p?" objection.)*

Although the probability of direction is numerically related to the one-sided frequentist
*p*-value (for a roughly symmetric posterior, the two-sided *p* ≈ 2 × (1 − pd);
Makowski et al., 2019), it is conceptually distinct in two essential respects. First, the
pd is a statement about the *parameter*: the posterior probability that the effect has a
given sign, conditional on the model and the data. A *p*-value is a statement about the
*data*: the probability, under an assumed null, of obtaining a test statistic at least as
extreme as the one observed. Only the Bayesian framework licenses probability statements
about parameters; in the frequentist framework the parameter is a fixed unknown and such
probabilities are undefined. Second — and more important for our use — we do not employ the
pd as a decision threshold. We do not classify effects as "present" or "absent" according
to whether the pd exceeds a cutoff; we report it for every parameter and interpret the
continuous gradient of directional evidence across the full set of results. The numerical
correspondence with a one-sided *p*-value therefore does not make our inference a
significance test in disguise: the quantity is interpreted differently (as a posterior
probability about the parameter) and used differently (as a graded index rather than a
binary rule).

---

## 4. Results — rewritten paragraphs 66–68 (and 69)

*(pd leads each result; the credible interval follows, explicitly framed as magnitude
uncertainty. Dichotomous language ("only one showed clear evidence", "no credible
moderation", "below conventional thresholds") is removed. The Disinhibition pd values are
corrected to the refit values, .54/.54.)*

### [66] Arousal-related pitch responses (F0)

For F0 we estimated ten moderation parameters: five PID-5 domains (Negative Affectivity,
Detachment, Antagonism, Disinhibition, Psychoticism) crossed with the stress and recovery
contrasts. Table 1 reports the posterior median, probability of direction (pd), and 89%
credible interval for each interaction; following our inferential framework, we interpret
the pattern of directional evidence across parameters rather than the status of any single
estimate.

During the stress phase, directional evidence was concentrated on a single domain. Negative
Affectivity showed strong evidence of amplifying the stress-induced rise in F0 (pd = .97):
individuals higher in emotional reactivity and threat sensitivity tended to show a larger
anticipatory increase in pitch. The posterior median was 3.14 Hz per SD of latent trait
(89% CrI [0.45, 5.83] Hz); the width of this interval shows that, although the *direction*
of the association is well supported, its *magnitude* is estimated with substantial
uncertainty (see also the prior-sensitivity analysis, Supplement). None of the other four
domains showed appreciable directional evidence in the stress phase (Detachment pd = .58,
Antagonism pd = .53, Disinhibition pd = .54, Psychoticism pd = .56), indicating that
stress-phase amplification of F0 was specific to Negative Affectivity.

In the recovery phase (Antagonism is considered separately below), Detachment showed the
clearest directional tendency, toward *reduced* F0 elevation (median = −2.04 Hz per SD,
pd = .88, 89% CrI [−4.78, 0.71]) — a suggestive pattern we interpret cautiously in the
Discussion. Psychoticism showed weaker evidence in the same negative direction (median =
−1.42 Hz per SD, pd = .80), while Negative Affectivity and Disinhibition showed essentially
no directional evidence of recovery-phase moderation (pd = .61 and .54, respectively).

### [67] (recovery contrast — Antagonism)

For the recovery contrast, Antagonism showed the strongest directional evidence of any F0
moderation (pd = .98): individuals higher in callousness and interpersonal antagonism
tended to show continued F0 elevation across the post-exam period rather than a return
toward baseline. The posterior median was 3.16 Hz per SD (89% CrI [0.57, 5.74] Hz) —
again, well-supported direction alongside considerable magnitude uncertainty. This effect
warrants caution: the average recovery contrast was near zero (see above), so it is best
read as evidence of *heterogeneity* in post-stressor arousal trajectories rather than a
population-level failure to recover, and the recovery phase affords less precision for
detecting interactions than the stress phase.

### [68] Voice-quality moderation (NNE)

`[Recompute all NNE intervals as exact 89% quantiles from the NNE refit; the bracketed
values below are approximations.]`

For NNE, the stress phase showed little directional evidence of moderation for any domain
(all pd < .83). The recovery phase, however, showed a distinct pattern. Psychoticism showed
strong directional evidence (pd = .96): higher Psychoticism was associated with less
negative NNE — i.e., increased glottal noise — in the post-exam period, consistent with
reduced phonatory control during stress de-escalation. The posterior median was 0.88 dB per
SD (89% CrI [≈ 0.07, 1.69] dB `[insert exact 5.5–94.5% quantiles]`). Antagonism showed
weaker directional evidence in the opposite direction (median = −0.43 dB per SD, pd = .82,
89% CrI [≈ −1.19, 0.33] dB `[insert exact]`). No other domain showed appreciable NNE
moderation (Table 2).

### [69] Summary (lightly revised for consistency)

The pattern of directional evidence reveals domain- and phase-specificity in how personality
shapes vocal stress responses. Negative Affectivity provided the clearest directional
evidence for arousal-related pitch (F0) during anticipatory stress, whereas voice quality
(NNE) showed a separate signature: the strongest directional evidence there was for
Psychoticism, and only in the recovery phase (pd = .96). Read together — and across the full
set of parameters rather than any single one — these results suggest that different
personality domains are expressed in distinct acoustic dimensions and at distinct phases of
the stress-response cycle: internalizing-related reactivity (Negative Affectivity) in
autonomic arousal indexed by F0 during stress, and cognitive–perceptual dysregulation
(Psychoticism) in phonatory control indexed by NNE during de-escalation.

---

## Tables 1 and 2 — note

Recompute every cell interval as the 89% equal-tailed posterior interval and keep a pd
column. Replace the current table note "Bold = strong certainty (PD > 0.95)" with a
descriptive, non-decisional version, e.g.: *"pd = probability of direction. Bold marks
pd ≥ .95 as a visual aid only; it is not a decision threshold. The 89% credible interval
(CrI) summarizes magnitude uncertainty and is not a significance test."*

---

## F0 89% equal-tailed intervals (to verify against exact quantiles)

| Parameter | Median | 89% CrI | pd |
|---|---:|---|---:|
| Negative Affectivity × stress | 3.14 | [0.45, 5.83] | .97 |
| Detachment × stress | −0.35 | [−3.10, 2.40] | .58 |
| Antagonism × stress | −0.09 | [−2.67, 2.48] | .53 |
| Disinhibition × stress | −0.21 | [−3.17, 2.74] | .54 |
| Psychoticism × stress | −0.25 | [−2.98, 2.48] | .56 |
| Negative Affectivity × recovery | −0.45 | [−3.12, 2.22] | .61 |
| Detachment × recovery | −2.04 | [−4.78, 0.71] | .88 |
| Antagonism × recovery | 3.16 | [0.57, 5.74] | .98 |
| Disinhibition × recovery | −0.17 | [−3.12, 2.78] | .54 |
| Psychoticism × recovery | −1.42 | [−4.16, 1.32] | .80 |

---

## References to add (verify against your reference manager)

- Gelman, A., & Carlin, J. (2014). Beyond power calculations: Assessing type S (sign) and
  type M (magnitude) errors. *Perspectives on Psychological Science, 9*(6), 641–651.
- Kruschke, J. K., & Liddell, T. M. (2018). The Bayesian New Statistics: Hypothesis testing,
  estimation, meta-analysis, and power analysis from a Bayesian perspective.
  *Psychonomic Bulletin & Review, 25*(1), 178–206.
- Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., & Lüdecke, D. (2019). Indices of effect
  existence and significance in the Bayesian framework. *Frontiers in Psychology, 10*, 2767.
- McElreath, R. (2020). *Statistical Rethinking: A Bayesian Course with Examples in R and
  Stan* (2nd ed.). CRC Press.
