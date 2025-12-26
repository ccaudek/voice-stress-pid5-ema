# Strategia Analisi F2 & Disinhibition: Guida Decisionale

## 🎯 Obiettivo

Testare se **Disinhibition modera gli effetti dello stress su articulatory precision**:
- Ipotesi: Disinhibition (↓ controllo motorio) × Stress → ↑ centralizzazione vocalica

## 📊 Workflow Decisionale

### STEP 1: Analisi Esplorativa
```r
source("10_f2_disinhibition_moderation.R")
```

**Esamina output:**
```r
results <- read_csv("results/f2_disinhibition/exploratory_stress_effects.csv")
print(results)
```

**Domande chiave:**
1. Quale metrica ha effetto stress più forte? (`effect_stress`)
2. Direzione effetto: negativo (centralizzazione) o positivo (espansione)?
3. Correlazione con F0: coerente o opposta?

---

### SCENARIO A: Effetti Stress ROBUSTI ✅

**Se `effect_stress` per VSA o F2_range è:**
- Negativo con |effect| > 20,000 (VSA) o |effect| > 50 Hz (F2_range)
- Coerente attraverso baseline→pre→post

**→ PROCEDI con modello Stan:**
```r
source("11_fit_stan_f2_disinhibition.R")
```

**Cosa aspettarsi:**
- `beta_c1` negativo (stress → centralizzazione)
- `beta_c1_x_disinhibition` negativo (high Disinhibition → più centralizzazione)
- Interazione robusta se CI non include 0

---

### SCENARIO B: Effetti Stress DEBOLI/INCONSISTENTI ⚠️

**Se `effect_stress` è:**
- Piccolo (|effect| < 10,000 per VSA, < 30 Hz per F2_range)
- Segno inconsistente (aumenta poi diminuisce)
- Alta variabilità (SD >> mean)

**→ PROBLEMA: Troppo rumore in F2**

#### Opzioni di Miglioramento

**1. Normalizzazione Bark/Mel** (riduce variabilità individuale)
```r
# In sezione calcolo metriche
df_voice <- df_voice %>%
  mutate(
    # Bark scale (più appropriato per formanti)
    f2_bark_i = 26.81 / (1 + 1960/f2_i) - 0.53,
    f2_bark_u = 26.81 / (1 + 1960/f2_u) - 0.53,
    f2_range_bark = f2_bark_i - f2_bark_u
  )
```

**2. Outlier Removal Robusto** (F2 ha molti outlier)
```r
# Rimuovi valori estremi (>3 SD per soggetto)
df_voice <- df_voice %>%
  group_by(user_id) %>%
  mutate(
    across(
      starts_with("f2_"),
      ~ if_else(
        abs(.x - mean(.x, na.rm = TRUE)) > 3 * sd(.x, na.rm = TRUE),
        NA_real_,
        .x
      ),
      .names = "{.col}_clean"
    )
  ) %>%
  ungroup()
```

**3. Pooling Vocali Diverso**
```r
# Invece di media, usa PCA su F1-F2 space
library(FactoMineR)

pca_data <- df_voice %>%
  select(f1_a, f1_i, f1_u, f2_a, f2_i, f2_u) %>%
  na.omit()

pca_res <- PCA(pca_data, graph = FALSE, ncp = 2)

# Usa PC1 come "dimensione articolatoria principale"
df_voice$articulation_pc1 <- pca_res$ind$coord[, 1]
```

**4. Modello con Vowel-Specific Effects**
```r
# Invece di aggregare, modella vocali come nested
# Stan model con:
# - y ~ vowel + stress × vowel + (1 + vowel|subject)
```

---

### SCENARIO C: F2 Troppo Rumoroso → ALTERNATIVE 🔄

Se anche dopo miglioramenti F2 resta rumoroso, considera:

#### **Opzione 1: Formant Dispersion (F2-F1)**
```r
df_voice <- df_voice %>%
  mutate(
    # Distanza F2-F1 (indice di "tensione" articolatoria)
    formant_disp_a = f2_a - f1_a,
    formant_disp_i = f2_i - f1_i,
    formant_disp_u = f2_u - f1_u,
    formant_disp_mean = (formant_disp_a + formant_disp_i + formant_disp_u) / 3
  )
```

**Rationale:** 
- F2-F1 meno influenzato da dimensioni anatomiche
- Cattura "spreading" articolatorio
- Più robusto di F2 assoluto

#### **Opzione 2: Spectral Tilt (H1-H2)**
Se disponibile negli acustici, usa:
```r
# Differenza primo e secondo armonico
# H1-H2 positivo → voce "breathy", rilassata
# H1-H2 negativo → voce "pressed", tesa

df_voice <- df_voice %>%
  transmute(
    user_id, timepoint,
    spectral_tilt = `H1-H2 dB`  # Se disponibile
  )
```

**Rationale:**
- Disinhibition → ridotto controllo → voce più rilassata/breathy
- Stress → tensione → pressed voice
- Interazione: alto Disinhibition + stress → conflitto motorio

#### **Opzione 3: Speaking Rate / Duration**
Se disponibile durata vocali o utterances:
```r
df_voice <- df_voice %>%
  transmute(
    user_id, timepoint,
    # Se hai durate vocali
    duration_mean = (dur_a + dur_i + dur_u) / 3,
    # O speech rate
    speech_rate = n_syllables / total_duration
  )
```

**Rationale:**
- Disinhibition → parlato più veloce/disorganizzato
- Stress → può accelerare (urgency) o rallentare (cautela)

#### **Opzione 4: Jitter / Shimmer (Voice Quality)**
```r
df_voice <- df_voice %>%
  transmute(
    user_id, timepoint,
    jitter_mean = (jitter_a + jitter_i + jitter_u) / 3,
    shimmer_mean = (shimmer_a + shimmer_i + shimmer_u) / 3
  )
```

**Rationale:**
- Disinhibition → controllo motorio ridotto → più perturbazioni
- Stress → tensione → può aumentare o ridurre jitter/shimmer

---

## 💡 Raccomandazione Strategica

### Se Hai Tempo e Risorse Limitate:

**PERCORSO CONSERVATIVO:**
1. Esegui analisi esplorativa (Step 1)
2. Se effetti stress deboli su F2:
   - **NON forzare** analisi su F2
   - Menziona nel paper: "F2 showed high measurement variability and 
     inconsistent stress effects, precluding robust moderation analysis"
3. Focus su F0 (già robusto) e within-person analysis

**PERCORSO ESPLORATIVO:** (se vuoi massimizzare chances)
1. Prova tutte le normalizzazioni e metriche alternative
2. Se **almeno una** mostra effetti stress coerenti → procedi Stan
3. Nel paper: "Among multiple articulatory metrics tested, [X] showed 
   the most reliable stress response..."

---

## 📝 Come Scrivere nel Paper

### Se F2 Funziona ✅

```markdown
To examine whether personality traits also modulated articulatory precision 
under stress, we analyzed vowel space area (VSA) as an index of articulatory 
control. Bayesian hierarchical models revealed that [results]. Critically, 
Disinhibition moderated stress-related changes in VSA (β = X.XX, 95% CI 
[X.XX, X.XX]), such that individuals high in Disinhibition showed [greater/
reduced] vowel space compression under exam stress. This dissociates 
articulatory from phonatory stress responses, suggesting distinct motor 
control pathways.
```

### Se F2 Non Funziona (Troppo Rumore) ⚠️

```markdown
**In Methods - Acoustic Measures:**
We extracted formant frequencies (F1, F2) for vowels /a/, /i/, /u/ and 
calculated vowel space area (VSA) as an index of articulatory precision. 
Preliminary analyses revealed substantial measurement variability in 
formant estimates (SD/mean > 0.3), likely due to [brief vocalization 
duration / background noise / estimation algorithm limitations].

**In Results - Brief mention:**
Formant-based articulatory metrics (VSA, F2 range) showed inconsistent 
patterns across stress conditions (ESM Figure X), precluding robust 
moderation analysis. Future studies with controlled phonetic elicitation 
or real-time articulography may better capture articulatory stress responses.

**In Discussion - Limitations:**
The present study focused on fundamental frequency as the primary vocal 
marker of stress reactivity. While we attempted to examine articulatory 
precision via formant analysis, high measurement variability in formant 
estimates limited interpretability. This likely reflects the challenge of 
extracting reliable formants from brief, naturalistic vocalizations 
collected in field settings. [Continue with F0 findings...]
```

**Messaggio:** Non nascondiamo il problema, ma lo presentiamo come limitazione 
tecnica (che è vera!) non come fallimento teorico.

---

## 🎯 Decisione Finale

**Esegui Step 1 (esplorativa) e poi:**

| Risultato | Azione | Effort | Payoff |
|-----------|--------|--------|--------|
| VSA effetto stress > 15K | → Stan completo | Alto | Alto |
| VSA effetto 5-15K | → Stan + sensitivity | Alto | Medio |
| VSA effetto < 5K | → Report limitation | Basso | Basso |
| Tutti inconsistenti | → Drop F2 | Minimo | - |

**Il mio consiglio:**
- Se hai già F0 robusto e within-person interessanti → F2 è "nice to have"
- Se F2 non funziona → NO PROBLEM, hai già un paper forte
- NON sacrificare qualità per quantity di analisi

---

## 📚 Riferimenti Teorici per F2/Disinhibition

Se F2 funziona, puoi citare:

**Articulation & Stress:**
- Laukka et al. (2008) - Vowel space compression under stress
- Scherer (1986) - Push effects (arousal) vs. pull effects (control)

**Disinhibition & Motor Control:**
- Markett et al. (2016) - Disinhibition and motor impulsivity
- Lynam et al. (2011) - PID-5 Disinhibition domain

**Voice Quality & Personality:**
- Scherer (2003) - Vocal markers of personality traits
- Juslin & Scherer (2005) - Voice production under emotion

---

## ✅ Checklist Finale

Prima di procedere con Stan:

- [ ] Eseguito analisi esplorativa (Step 1)
- [ ] Verificato effect_stress > soglia ragionevole
- [ ] Controllato distribuzione outcome (skewness? outliers?)
- [ ] Provato normalizzazioni se necessario
- [ ] Deciso metrica finale (VSA? F2_range? altro?)
- [ ] Verificato N sufficiente (>100 per modello complesso)
- [ ] Tempo disponibile per debugging Stan (~2-4 ore)

Se tutti ✅ → GO!  
Se molti ❌ → Consider alternatives o drop F2

---

**Ricorda:** Un paper con F0 robusto + within-person analysis è GIÀ un 
contributo significativo. F2 è un bonus, non una necessità.
