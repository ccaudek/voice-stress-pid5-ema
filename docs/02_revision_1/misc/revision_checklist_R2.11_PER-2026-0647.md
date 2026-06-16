# Revision checklist — R2.11 propagation (Ms. PER-2026-0647)

Work document for editing the **Word master** (`acustic_sensing.docx`) with tracked changes.
Each item gives **FIND** (verbatim submitted text, with the printed manuscript line number as a
locator), **REPLACE** (ready-to-paste), and **SOURCE** (where the value lives in
`manuscript_values.md`). Values shown here are the **exact** ones from the regenerated outputs;
where the earlier `inferential_logic_revision_PER-2026-0647.md` used a normal approximation, the
numbers below supersede it.

Two cross-cutting facts to keep in mind while editing:

- **CrI relabel.** The submitted "95% CrIs" were in fact ~90% intervals (labeling error). All
  intervals are now **89% equal-tailed** and *narrower* than the submitted ones — so most CrIs
  change even when the median does not.
- **NNE −0.79 vs −0.65 (read item D3 + G first).** The submitted main-effects/abstract value
  **−0.79 dB** is the *robust Student-t* estimate; the **primary Gaussian** value is **−0.65 dB**.
  This needs your confirmation, then propagation to abstract + main effects.

---

## A. Global conventions (apply throughout, do these first)

- [x] Replace every **"95% credible interval" / "95% CrI"** → **"89% CrI"** (and "95% credible
  intervals" → "89% credible intervals"). This includes Table 1, Table 2, and Table 3 headers
  (`Median [95% CrI]` → `Median [89% CrI]`).
- [x] Standardize **"PD"** → **"pd"** (the Supplement and new text use lowercase `pd`). Optional but
  recommended for consistency.
- [ ] Remove all dichotomous decision language: "credible if … excluded zero", "Bayes factor(s)",
  "did not exceed the conventional threshold", "below conventional thresholds", "strong certainty
  (PD > 0.95)". Replacements are specified below where they occur.
- [ ] No new "significant", "reliable effect", or "the interval excludes/includes zero ⇒ effect"
  phrasing should be introduced.

---

## B. Methods — *Inference* subsection (printed line ~354)

- [ ] **FIND** (the whole current Inference paragraph, beginning):
  > "**Inference.** Effects were considered credible if 95% credible intervals excluded zero. […]
  > Bayes factors comparing moderation models to null models without interactions."

- [x] **REPLACE** with the rewritten paragraph (full text in
  `inferential_logic_revision_PER-2026-0647.md`, §2). Key sentences:
  > **Inference.** All analyses are Bayesian, and we report and interpret full posterior
  > distributions rather than dichotomous test outcomes. For each parameter we report the
  > posterior median, the probability of direction (pd; the posterior probability that the
  > parameter takes its more probable sign), and an 89% equal-tailed credible interval. The pd is
  > our primary index of evidence … its inclusion or exclusion of zero carries no special status,
  > and the 89% width is an explicitly arbitrary descriptive choice (McElreath, 2020). … because
  > the present sample constrains effect magnitudes only loosely (Supplement,
  > *Prior-Sensitivity Analysis*), we foreground the direction and strength of associations over
  > their precise size. Full details … are given in the Supplement.

- [x] **Delete the Bayes-factor sentence entirely** (no BF anywhere in the pipeline now).
- [ ] Add refs: Makowski et al. (2019), McElreath (2020), Kruschke & Liddell (2018),
  Gelman & Carlin (2014). (List in `inferential_logic_revision…md`.)

---

## C. Methods — MCMC settings reconciliation

- [ ] Verify the sampler description matches what was actually run **and** the Supplement: four
  chains × 4,000 iterations (2,000 warmup), `adapt_delta` as set, R-hat < 1.01. The Supplement
  states "four chains of 4,000 iterations each (2,000 warmup)" — make the main text identical.
- [ ] If the Methods still describe an earlier configuration tied to the old Psychoticism×recovery
  value (−1.22), update it; the current refit gives **−1.42** (item E).

---

## D. Results — Main effects (printed lines ~374–400)

Pull exact intercepts and variance components (τ, σ) from
`manuscript_values.md` → *"Results, F0/NNE main effects"* (`parameter_estimates_summary.csv`);
the headline contrasts are:

- [ ] **D1 — F0 stress (β₁).** FIND: "F0 increased by 3.27 Hz (MAD = 1.25, 95% CrI [0.81, 5.71])"
  → REPLACE: "F0 increased by **3.27 Hz** (MAD = 1.25, **89% CrI [1.25, 5.27]**, pd = .995)".
- [ ] **D2 — F0 recovery (β₂).** FIND: "median = 0.14 Hz, MAD = 1.24, 95% CrI [-2.34, 2.59],
  𝑃(β2 > 0) = 0.542" → REPLACE: "median = 0.14 Hz, MAD = 1.24, **89% CrI [−1.89, 2.11]**,
  pd = .54".
- [ ] **D3 — NNE stress (β₁) — CRITICAL.** FIND: "NNE decreased by 0.79 dB (MAD = 0.31, 95% CrI
  [-1.30, -0.30]), with 𝑃(β1 < 0) = 0.995" → REPLACE: "NNE decreased by **0.65 dB** (MAD = 0.28,
  **89% CrI [−1.10, −0.21]**, pd = .99)". **The −0.79 dB value is the robust Student-t estimate;
  it now appears only in the Supplement sensitivity analysis. Confirm −0.65 is the primary
  Gaussian value to report.**
- [ ] **D4 — NNE recovery (β₂).** FIND: "β2 = -0.19 dB (MAD = 0.30, 95% CrI [-0.69, 0.31]). The
  95% credible interval includes zero and the directional probability is …" → REPLACE: "β2 =
  **−0.22 dB** (MAD = 0.28, **89% CrI [−0.67, 0.23]**, pd = .78)." Drop "the interval includes
  zero" sentence; report direction/pd instead.
- [ ] **D5 — Variance components (F0 and NNE).** Recompute every τ/σ interval to 89% from
  `parameter_estimates_summary.csv` (e.g. F0 τ₁ [17.68, 22.44] → 89%; σ [8.34, 9.96] → 89%; NNE
  τ₁, τ₂, σ likewise). Medians unchanged; only the brackets narrow.

> SOURCE: `manuscript_values.md` → "Results, F0/NNE main effects" (and the EMA-vs-baseline section
> for D1's robustness cross-check).

---

## E. Results — Moderation (printed lines ~422–446) + Tables 1 & 2

- [ ] **E1 — Replace the moderation prose** (¶66–68) with the rewritten version in
  `inferential_logic_revision_PER-2026-0647.md` §4, but **insert the exact 89% intervals below**
  (the §4 brackets were approximations). pd leads each result; the CrI follows as magnitude
  uncertainty; remove "only one showed clear evidence", "no credible moderation", "below
  conventional thresholds".

**F0 — Table 1 cells (replace all ten; corrections in bold):** SOURCE
`table1_f0_moderation_89cri.csv` / `direction_posterior_summary_89cri.csv`.

| Parameter | Median (Hz/SD) | 89% CrI | pd |
|---|---:|---|---:|
| Negative Affectivity × stress | 3.14 | [0.45, 5.84] | .97 |
| Detachment × stress | −0.35 | [−3.10, 2.40] | .58 |
| Antagonism × stress | −0.09 | [−2.67, 2.48] | .53 |
| **Disinhibition × stress** | −0.21 | [−3.17, 2.74] | **.54** |
| Psychoticism × stress | −0.25 | [−2.98, 2.48] | .56 |
| Negative Affectivity × recovery | −0.45 | [−3.12, 2.22] | .61 |
| Detachment × recovery | −2.04 | [−4.76, 0.73] | .88 |
| Antagonism × recovery | 3.16 | [0.61, 5.76] | .98 |
| **Disinhibition × recovery** | −0.17 | [−3.12, 2.78] | **.54** |
| **Psychoticism × recovery** | **−1.42** | [−4.15, 1.33] | .80 |

- [ ] **E2 — NegAff×stress in prose.** FIND: "𝛾1 = 3.14 Hz per SD, 95% CrI [0.37, 5.89], PD …" →
  "3.14 Hz per SD, **89% CrI [0.45, 5.84]**, pd = .97".
- [ ] **E3 — Antagonism×recovery in prose.** FIND: "Hz per SD, 95% CrI [0.51, 5.78], PD = 0.97" →
  "3.16 Hz per SD, **89% CrI [0.61, 5.76]**, pd = **.98**" (note pd .97 → .98).
- [ ] **E4 — Psychoticism×recovery (F0).** Anywhere the submitted value **−1.22** appears →
  **−1.42** (89% CrI [−4.15, 1.33], pd = .80).
- [ ] **E5 — Disinhibition pd.** Anywhere the submitted **.64 / .67** appears → **.54 / .54**.

**NNE — Table 2 cells (headline rows):** SOURCE `direction_posterior_summary_89cri.csv` (NNE).

| Parameter | Median (dB/SD) | 89% CrI | pd |
|---|---:|---|---:|
| Negative Affectivity × stress | −0.46 | [−1.27, 0.34] | .82 |
| Antagonism × recovery | −0.43 | [−1.18, 0.33] | .82 |
| Psychoticism × recovery | 0.88 | [0.07, 1.68] | .96 |

- [ ] **E6 — Psychoticism×recovery (NNE) in prose.** FIND: "95% CrI [0.05, 1.72], PD = 0.96,
  SNR = 1.74" → "0.88 dB per SD, **89% CrI [0.07, 1.68]**, pd = .96".
- [ ] **E7 — Antagonism×recovery (NNE) in prose.** FIND: "𝛾2 = -0.43 dB, 95% CrI [-1.37, 0.49],
  PD = 0.82" → "−0.43 dB per SD, **89% CrI [−1.18, 0.33]**, pd = .82". Drop "though this fell …"
  (any threshold tail).

- [ ] **E8 — Table notes (Tables 1 & 2).** Replace "Bold = strong certainty (PD > 0.95)" with:
  > "pd = probability of direction. Bold marks pd ≥ .95 as a visual aid only; it is not a decision
  > threshold. The 89% credible interval (CrI) summarizes magnitude uncertainty and is not a
  > significance test."

---

## F. Results — EMA vs baseline: "precision" → "replication" (printed lines ~471–491)

- [ ] **F1 — ΔELPD threshold.** FIND: "(Δ ELPD = -3.0, SE = 3.6) and Baseline-only model
  (Δ ELPD = -4.6, SE = 4.0) **did not exceed the conventional threshold for meaningful
  differences (|Δ| < 2 SE)**." → REPLACE: "(ΔELPD = −3.0, SE = 3.6; and −4.6, SE = 4.0). These
  differences are small relative to their standard errors, indicating similar predictive accuracy
  across measurement approaches." (Drop the |Δ| < 2 SE rule.)

- [ ] **F2 — The core reframe (¶~479–484).** FIND: "The EMA-based model yielded a **more precise**
  estimate (𝛾1 = 3.07 Hz, 95% CrI [−0.44, 6.55], PD = 0.96) compared to the baseline model
  (𝛾1 = 2.65 Hz, 95% CrI [−2.20, 7.52], PD = 0.86). The EMA estimate showed a 28% narrower
  credible interval and stronger directional evidence. **This pattern was consistent across other
  PID-5 domains: EMA-derived estimates systematically showed tighter uncertainty bounds** …" →
  REPLACE:
  > "For Negative Affectivity—the domain with the strongest stress moderation—the EMA-based and
  > baseline-questionnaire estimates **agreed in direction** (both positive; EMA median = 3.07 Hz,
  > 89% CrI [insert exact], pd = .96; baseline median = 2.65 Hz, 89% CrI [insert exact], pd = .86),
  > a convergence of directional evidence across two independent measurement approaches. The EMA
  > interval was also narrower (89% CrI width ≈ 5.6 vs 8.0 Hz), which we report descriptively
  > rather than as a precision claim. **This cross-method agreement was specific to the focal
  > effect: for the remaining PID-5 domains the two approaches yielded weak and not consistently
  > concordant estimates** (Supplementary Table S2)."

  > **Empirical note (important):** the manifest shows EMA and baseline **disagree in sign** for
  > Antagonism, Psychoticism, and Disinhibition (`same_sign = FALSE`). The submitted "systematic
  > across domains" claim is **not supported** and must be removed/qualified as above. Insert exact
  > 89% bounds from `manuscript_values.md` → *"EMA vs baseline"*
  > (`precision_improvement_summary_improved.csv`).

- [x] **F3 — ¶~489–491.** FIND: "it does **enhance inferential precision for moderation
  effects**—a distinction relevant for theory testing …" → REPLACE: "the EMA and baseline
  approaches yielded **directionally convergent** estimates for the focal effect, which is the
  relevant consideration for theory testing, even though aggregate predictive accuracy was
  similar." (Remove the precision framing.)

---

## G. Abstract (printed lines ~22–31)

- [ ] **G1 — NNE value.** FIND: "normalized noise energy (NNE) decreased by **0.79 dB**" →
  "decreased by **0.65 dB**" (primary Gaussian; tie to item D3 confirmation).
- [ ] **G2 — precision claim.** FIND: "brief EMA-based personality assessment matched comprehensive
  baseline questionnaires in predictive accuracy **while yielding more precise moderation
  estimates**." → REPLACE: "… matched comprehensive baseline questionnaires in predictive
  accuracy, **with directionally convergent moderation estimates for the focal effect**."
- [ ] **G3** — F0 "3.27 Hz" is unchanged (matches the refit); no edit.

---

## H. Discussion (printed lines ~497–499 and ~633–637)

- [x] **H1 — ¶~497–499.** FIND: "while yielding **greater precision in key moderation estimates**."
  → REPLACE: "with **directionally convergent moderation estimates** for the focal trait."
- [ ] **H2 — ¶~633–637.** FIND: "EMA-based measurement yielded **more precise moderation
  estimates**, consistent with the idea that repeated sampling can reduce measurement error … even
  when overall predictive accuracy is equivalent, **improved inferential precision** can strengthen
  theory tests …" → REPLACE: reframe to cross-method **directional convergence** for the focal
  effect; drop "more precise" and "improved inferential precision". Keep the measurement-error
  point only as a hypothesis, not a demonstrated precision gain.
- [ ] **H3 — ¶~494.** "we observed **reliable** stress-related changes" → "we observed
  **directionally well-supported** stress-related changes" (avoid "reliable").

---

## I. Prior-sensitivity cross-reference (Methods/Results)

- [ ] Add one sentence (Methods or Results) pointing to the new Supplement section: "A
  prior-sensitivity analysis (Supplement, *Prior-Sensitivity Analysis*) shows that the **direction**
  of the moderation effects is robust across prior specifications (pd ≥ ~.92), whereas their
  **magnitudes** are prior-dependent; we therefore foreground direction over magnitude." SOURCE:
  `prior_sensitivity_summary.csv`.

---

## J. For the response letter (not the manuscript body)

- [ ] **Disclose the CrI relabel:** state plainly that the submitted "95% CrIs" were ~90% intervals
  (a labeling error) and that all intervals are now reported as 89% equal-tailed, recomputed
  uniformly from the saved draws.
- [ ] **NNE −0.79 → −0.65:** explain that the main-effects/abstract value now reports the primary
  Gaussian estimate (−0.65 dB); the −0.79 dB figure is the robust Student-t estimate, presented in
  the Supplement sensitivity analysis.
- [ ] **Bayes factors removed:** note they have been dropped in favor of pd + 89% CrI, per the
  unified non-dichotomous framework (and because the prior-sensitivity analysis shows magnitudes
  are prior-dependent, to which BFs are sensitive).
- [ ] **"pd is not a p-value":** include the paragraph from `inferential_logic_revision…md` §3 to
  pre-empt the "isn't pd just 1 − p?" objection.

---

### Quick value reference (all from the manifest, 89% CrI)

| Where | Effect | Value |
|---|---|---|
| Main | F0 stress | 3.27 Hz [1.25, 5.27], pd .995 |
| Main | F0 recovery | 0.14 Hz [−1.89, 2.11], pd .54 |
| Main | NNE stress | **−0.65 dB** [−1.10, −0.21], pd .99 |
| Main | NNE recovery | −0.22 dB [−0.67, 0.23], pd .78 |
| Mod F0 | NegAff × stress | 3.14 [0.45, 5.84], pd .97 |
| Mod F0 | Antagonism × recovery | 3.16 [0.61, 5.76], pd .98 |
| Mod F0 | Detachment × recovery | −2.04 [−4.76, 0.73], pd .88 |
| Mod F0 | Psychoticism × recovery | **−1.42** [−4.15, 1.33], pd .80 |
| Mod F0 | Disinhibition × stress / recovery | pd **.54 / .54** |
| Mod NNE | Psychoticism × recovery | 0.88 [0.07, 1.68], pd .96 |
| Mod NNE | Antagonism × recovery | −0.43 [−1.18, 0.33], pd .82 |
| Mod NNE | NegAff × stress | −0.46 [−1.27, 0.34], pd .82 |
| Prior-sens | min pd across priors | ~.92 (direction robust; magnitude prior-dependent) |
