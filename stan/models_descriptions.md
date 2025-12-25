Below is a **manuscript-ready description** of the two Bayesian models, written in **formal scientific English** and suitable for the *Methods / Statistical Analysis* section of a psychology journal. I focus especially on the **F0 model**, which is the primary and theoretically strongest one, and then describe the **NNE model** as a parallel, secondary analysis.

I avoid code-level detail and instead describe the **generative assumptions, contrasts, and interpretation**, which is what reviewers care about.

---

## Bayesian Modeling of Vocal Stress Reactivity and Personality Moderation

### Overview

To examine how acute exam-related stress affects vocal production and whether these effects are moderated by personality pathology traits, we estimated hierarchical Bayesian models implemented in **Stan**. The models jointly integrated (a) repeated acoustic measurements obtained at three stress-relevant time points and (b) intensive ecological momentary assessment (EMA) data used to estimate stable latent personality traits with explicit correction for measurement error.

Two acoustic outcomes were analyzed using parallel model structures:

1. **Fundamental frequency (F0 mean)**, indexing vocal arousal (primary outcome);
2. **Noise-to-Harmonics Energy (NNE)**, indexing phonatory control and glottal noise (secondary outcome).

Both models used identical contrast coding, random-effects structures, and latent trait formulations, differing only in the acoustic outcome and its interpretation.

---

## Model 1: Fundamental Frequency (F0 Mean)

### Acoustic Outcome

The primary dependent variable was **mean fundamental frequency (F0)**, expressed in Hertz. To increase robustness and reduce vowel-specific noise, F0 was **averaged across the three sustained vowels (/a/, /i/, /u/)** for each recording occasion. Each participant contributed up to three observations, corresponding to:

* a **baseline** day distant from the exam,
* the **day before the exam** (stress condition),
* the **day after the exam** (recovery condition).

---

### Stress and Recovery Coding

Stress was modeled using **two orthogonal contrast variables**:

* **Stress contrast (c1)**: compares the pre-exam day to baseline
  (baseline = −0.5, pre = +0.5, post = 0)
* **Recovery contrast (c2)**: compares the post-exam day to the pre-exam day
  (baseline = 0, pre = −0.5, post = +0.5)

This parameterization allowed the model to separately estimate **acute stress reactivity** and **post-stress recovery** while retaining all three time points in a single model.

---

### Latent Personality Traits (PID-5 EMA)

Five PID-5 trait domains (Negative Affectivity, Detachment, Antagonism, Disinhibition, Psychoticism) were derived from EMA data collected repeatedly over approximately 2.5 months.

Rather than using observed scale scores directly, each domain was modeled as a **latent between-person trait**. Specifically:

* Each EMA observation was treated as a noisy indicator of a person-specific latent trait value.
* A **measurement model** estimated the latent trait means while accounting for domain-specific measurement error.
* All EMA trait indicators were standardized prior to modeling, such that latent trait coefficients reflect effects per **1 SD increase** in the trait.

This approach substantially reduced attenuation bias due to measurement error and allowed the vocal models to operate on denoised personality estimates.

---

### Hierarchical Outcome Model

The vocal outcome was modeled as:

* a **population-level intercept**,
* **fixed effects** of stress (c1) and recovery (c2),
* **random intercepts and random slopes** for stress and recovery at the participant level,
* **main effects of latent personality traits** on baseline vocal pitch,
* **interaction terms** capturing moderation of stress and recovery effects by each latent trait.

Formally, for participant *i* at time *t*:

[
\text{F0}*{it} = \alpha + u*{0i}

* (b_1 + u_{1i}),c1_t
* (b_2 + u_{2i}),c2_t
* \sum_d a_d,\theta_{id}
* \sum_d g_{1d},c1_t,\theta_{id}
* \sum_d g_{2d},c2_t,\theta_{id}
* \varepsilon_{it}
  ]

where:

* ( \theta_{id} ) are latent PID-5 traits,
* ( g_{1d} ) quantify **moderation of stress reactivity**,
* ( g_{2d} ) quantify **moderation of recovery**.

Random effects were modeled using a non-centered parameterization with independent variance components to ensure stable estimation.

---

### Inference and Interpretation

Inference focused on **posterior distributions**, not null-hypothesis testing. For each effect we examined:

* posterior means and credible intervals,
* **probability of direction (PD)**, defined as the posterior probability that an effect is positive or negative.

For F0 mean, results indicated:

* a robust **increase in F0 under stress**, reflecting heightened vocal arousal;
* strong evidence that **Negative Affectivity amplified stress-related F0 increases**;
* evidence that **Antagonism impaired post-stress recovery**, with elevated pitch persisting after the exam.

Other PID-5 domains showed little evidence of systematic moderation.

---

## Model 2: Noise-to-Harmonics Energy (NNE)

### Acoustic Outcome

As a secondary analysis, we modeled **Noise-to-Harmonics Energy (NNE)**, an index of glottal noise and phonatory control. As with F0, NNE was **averaged across vowels** to improve reliability.

In the present dataset, NNE values were consistently negative, with **more negative values indicating lower glottal noise and greater phonatory control**.

---

### Model Structure

The NNE model used **the same hierarchical structure** as the F0 model:

* identical stress and recovery contrasts,
* the same latent PID-5 measurement model,
* random intercepts and random slopes for stress and recovery,
* trait × stress and trait × recovery interactions.

Only the acoustic outcome differed.

---

### Interpretation

Unlike F0, NNE showed a **robust decrease (more negative values) under stress**, indicating reduced glottal noise and increased phonatory control. This pattern is consistent with **stress-induced hyperadduction or increased laryngeal tension**, rather than vocal destabilization.

In contrast to F0, **NNE showed little evidence of personality moderation**, suggesting that control-related aspects of vocal stress responses are more uniform across individuals, whereas arousal-related pitch responses are selectively shaped by affective vulnerability.

---

## Summary of the Modeling Approach

Together, the two models support a **dual-process account of vocal stress reactivity**:

* **F0 mean** primarily reflects **arousal**, which is strongly modulated by Negative Affectivity and partially by Antagonism during recovery.
* **NNE** reflects **phonatory control**, which changes robustly under stress but is largely independent of personality traits.

Methodologically, these conclusions were enabled by combining intensive EMA data with hierarchical Bayesian models that explicitly accounted for measurement error, individual heterogeneity, and uncertainty in effect direction.

---

If you want, next I can:

* condense this into a **shorter Methods subsection** (for journals with strict word limits),
* or help you phrase a **statistical preregistration-style summary** of the models.
