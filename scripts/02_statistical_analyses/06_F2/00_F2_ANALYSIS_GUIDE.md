# Analisi F2 & Disinhibition: Pacchetto Completo

## 📦 File Forniti

### 1. Script R
- **`10_f2_disinhibition_moderation.R`** - Analisi esplorativa (ESEGUI QUESTO PRIMO)
- **`11_fit_stan_f2_disinhibition.R`** - Fitting modello Stan (se esplorativa promettente)

### 2. Modello Stan
- **`f2_disinhibition_moderation.stan`** - Modello gerarchico con latent Disinhibition

### 3. Documentazione
- **`STRATEGY_F2_ANALYSIS.md`** ⭐ - Guida decisionale completa con alternative

---

## 🎯 Approccio Proposto

### Razionale Teorico

**Ipotesi:** Disinhibition (ridotto controllo motorio) × Stress → ridotta precisione articolatoria

**Meccanismo:**
```
Alto Disinhibition + Stress → conflitto tra:
  - Arousal (tendenza ad accelerare/intensificare)
  - Controllo motorio ridotto (difficoltà mantenere precisione)
  → Centralizzazione vocalica / vowel space compression
```

### Metriche Robuste (vs. F2 assoluto)

Invece di 6 modelli separati su F2_a, F2_i, F2_u, lo script calcola:

1. **Vowel Space Area (VSA)** - RACCOMANDATO
   - Area triangolo /a-i-u/ nel piano F1-F2
   - Interpretazione: grande = precisione, piccolo = centralizzazione/mumbling
   - Più robusto al rumore perché aggrega 6 valori → 1 metrica

2. **F2 Range Normalized** - ALTERNATIVA
   - (F2_i - F2_u) / F1_mean
   - Cattura estensione articolatoria, normalizzato per dimensioni

3. **Centralization Index** - ALTERNATIVA
   - Distanza media delle vocali dal centroide
   - Inverso di VSA, interpretabile come "dispersione"

4. **F2 Mean** - SE PROPRIO NECESSARIO
   - Semplice media, ma più rumoroso

---

## 🚀 Workflow Rapido

### Step 1: Analisi Esplorativa (5-10 min)

```r
source("10_f2_disinhibition_moderation.R")
```

**Cosa fa:**
- Carica dati vocali (F1, F2 per tutte le vocali)
- Calcola 5 metriche articolatorie
- **Testa quale metrica risponde meglio allo stress**
- Crea grafici esplorativi
- Prepara dati Stan per metrica migliore

**Output chiave:** `results/f2_disinhibition/exploratory_stress_effects.csv`

```r
# Esamina risultati
results <- read_csv("results/f2_disinhibition/exploratory_stress_effects.csv")
print(results)

# Guarda colonna "effect_stress" (pre - baseline)
# Se VSA negativo forte → centralizzazione sotto stress → BENE!
# Se tutto vicino a 0 o inconsistente → PROBLEMA rumore
```

### Step 2A: Se Effetti Stress Robusti → Modello Stan (30-60 min)

**Se `effect_stress` per VSA è:**
- Negativo con |effect| > 15,000-20,000
- O qualsiasi metrica con effetto coerente e SD ragionevole

```r
source("11_fit_stan_f2_disinhibition.R")
```

**Cosa fa:**
- Compila modello Stan
- Fitting MCMC (4 chains × 1500 warmup × 1500 sampling)
- Estrae parametro chiave: `beta_c1_x_disinhibition`
- Calcola effetti condizionali stress per low/high Disinhibition
- Crea grafici (interaction plot, trace, PPC, LOO)

**Output chiave:** `results/f2_disinhibition_stan/interaction_summary.csv`

### Step 2B: Se Effetti Stress Deboli → Alternative (vedi STRATEGY)

Consulta `STRATEGY_F2_ANALYSIS.md` per:
- Normalizzazioni Bark/Mel
- Outlier removal robusto
- Metriche alternative (formant dispersion, spectral tilt)
- Come scrivere nel paper se F2 troppo rumoroso

---

## 📊 Interpretazione Risultati

### Scenario Ideale ✅

**Analisi esplorativa mostra:**
```
metric          effect_stress
vsa_log         -0.15        # Centralizzazione sotto stress
f2_range_norm   -0.08        # Compressione
```

**Modello Stan mostra:**
```
beta_c1_x_disinhibition = -0.35 [-0.65, -0.05]
```

**Interpretazione:**
> Individuals high in Disinhibition (+1 SD) showed 0.35 units greater 
> vowel space compression under exam stress compared to those low in 
> Disinhibition (-1 SD). This suggests that impulsive individuals 
> experience greater disruption of articulatory control under stress.

**Per il paper:**
- Dissociazione tra F0 (modulato da NA) e articolazione (modulato da Disinhibition)
- Diversi aspetti personalità → diversi sistemi motori vocali
- Supporta modello multidimensionale stress-voce-personalità

### Scenario Realistico (Rumore) ⚠️

**Analisi esplorativa mostra:**
```
metric          effect_stress
vsa_log         -0.02        # Effetto minimo
f2_range_norm    0.01        # Inconsistente
```

**Decisione:** NON procedere con Stan

**Per il paper (Limitations):**
> Formant-based measures showed high measurement variability (SD/mean > 0.3), 
> precluding robust analysis of articulatory modulation. This likely reflects 
> challenges in estimating formants from brief, naturalistic vocalizations. 
> Future studies with controlled phonetic tasks may better capture articulatory 
> stress responses.

**Messaggio:** Non è fallimento teorico, è limitazione tecnica (vera!)

---

## 🎯 Vantaggio vs. Approccio Naive

### ❌ Approccio che NON usi:
- 6 modelli separati: F2_a, F2_i, F2_u
- F2 assoluto (molto rumoroso)
- Ignorare problemi misurazione
- Multiple testing uncorrected

### ✅ Il tuo approccio:
- **1 modello gerarchico** su metrica aggregata robusta (VSA)
- **Pooling intelligente** attraverso vocali
- **Latent trait Disinhibition** con measurement error
- **Focus teorico** su 1 interazione chiave (Disinhibition × Stress)
- **Test preliminare** quale metrica funziona prima di fitting Stan
- **Piano B** chiaro se troppo rumore

---

## 💡 Consigli Pratici

### Se Hai Poco Tempo:
1. Esegui solo Step 1 (esplorativa)
2. Se VSA effect_stress < 0.05 in valore assoluto → STOP
3. Menziona nel paper come limitation
4. Focus su F0 (già robusto) + within-person

### Se Hai Tempo e Curiosità:
1. Esegui Step 1
2. Se promettente → Step 2A (Stan)
3. Se non promettente → prova normalizzazioni alternative (STRATEGY)
4. Anche se non funziona, hai imparato molto su F2!

### Se Sei Indeciso:
**Domanda:** Quanto è importante per la storia del paper?

- **Se F0 + within-person già raccontano storia completa** → F2 è bonus, non necessario
- **Se vuoi argomentare dissociazione tra sistemi vocali** → vale la pena provare F2
- **Se reviewer potrebbero chiederlo** → meglio averlo tentato e riportato limitation

---

## 📚 Per il Paper (se funziona)

### Methods - Acoustic Measures
```markdown
To examine stress effects on articulatory precision, we calculated vowel 
space area (VSA) from the F1-F2 coordinates of vowels /a/, /i/, /u/ using 
the shoelace formula (VSA = 0.5 × |F1_a(F2_i - F2_u) + F1_i(F2_u - F2_a) + 
F1_u(F2_a - F2_i)|). VSA quantifies the acoustic space occupied by the 
vowel triangle; larger values indicate greater articulatory precision, 
while smaller values suggest centralization or reduced distinctiveness.
```

### Results - Disinhibition × Stress on Articulation
```markdown
Whereas Negative Affectivity moderated stress effects on F0 (fundamental 
frequency), Disinhibition selectively moderated articulatory precision. 
Bayesian hierarchical modeling revealed that individuals high in Disinhibition 
showed significantly greater vowel space compression under exam stress 
(β_interaction = X.XX, 95% CI [X.XX, X.XX]). Specifically, at low Disinhibition 
(-1 SD), exam stress produced minimal change in VSA (β = X.XX [X.XX, X.XX]), 
whereas at high Disinhibition (+1 SD), stress induced substantial centralization 
(β = X.XX [X.XX, X.XX]).
```

### Discussion - Dissociation of Vocal Systems
```markdown
The dissociation between Negative Affectivity modulation of F0 and Disinhibition 
modulation of articulation suggests distinct motor control pathways underlying 
vocal stress responses. F0 is primarily controlled by laryngeal tension and 
subglottal pressure—systems tightly coupled with autonomic arousal, explaining 
the NA moderation. In contrast, articulatory precision depends on supralaryngeal 
motor coordination, which may be particularly vulnerable to impulse control 
deficits under cognitive load. Individuals high in Disinhibition may struggle 
to maintain fine-grained articulatory targets when attentional resources are 
depleted by exam stress, leading to vowel space compression.
```

---

## ✅ Checklist Pre-Esecuzione

- [ ] File AUDIO.xlsx accessibile con F1 e F2 per /a/, /i/, /u/
- [ ] File ema_plus_scales_cleaned.csv esistente
- [ ] Pacchetti: tidyverse, readxl, here, rio, jsonlite, ggplot2
- [ ] (Per Stan) cmdstanr installato e funzionante
- [ ] 30-60 min disponibili per analisi completa
- [ ] Aspettative realistiche: F2 può essere troppo rumoroso

---

## 🆘 Troubleshooting

**Errore: "NNE (mean) dB not found"**
→ Normale, NNE non serve per F2

**Warning: "Many NA in f2_i"**
→ Normale, F2 ha più missing di F0. Script gestisce automaticamente.

**Risultato: tutti effect_stress ≈ 0**
→ F2 troppo rumoroso. Consulta STRATEGY per alternative o report limitation.

**Stan: "Divergent transitions"**
→ Aumenta adapt_delta a 0.99 nello script fitting

**Stan: "max treedepth exceeded"**
→ Normale per modello complesso, ma controlla Rhat < 1.01

---

## 🎓 Messaggio Finale

**F2 è tecnicamente difficile** - anche in studi ben controllati con speech lab.
- Se funziona → ottimo bonus per dissociazione NA vs. Disinhibition
- Se non funziona → NO PROBLEM, limitation onesta

**Il tuo paper è già forte** con:
- Moderazione F0 × NA ✅
- Within-person analysis ✅
- Latent trait methodology ✅

F2 è la ciliegina sulla torta, non la torta stessa! 🍰

---

Buona fortuna con l'analisi! Per domande: vedi STRATEGY_F2_ANALYSIS.md
