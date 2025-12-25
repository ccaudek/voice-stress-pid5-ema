# GUIDA: PRIOR INFORMATIVI PER ANALISI FORMANT

## PROBLEMA IDENTIFICATO

Nel prior predictive check, hai osservato che:
- **Dati reali**: F2 per /a/ ha moda intorno a 1200 Hz
- **Prior predictions (yrep)**: Moda intorno a 0 o valori negativi

Questo indica che i **prior erano troppo vaghi** per la scala dei dati formant.

## PERCHÉ SUCCEDE?

I prior originali erano:
```r
prior(normal(0, 100), class = "b")          # Coefficienti
prior(exponential(1), class = "sigma")       # Errore!
```

Il problema principale era nell'**intercetta**:
- NON c'era un prior specifico per l'intercetta
- brms usava il default: `student_t(3, 1231, 295)` (basato sui dati)
- Ma con prior vaghi sui coefficienti, le predizioni erano comunque sbagliate

## SOLUZIONE: PRIOR INFORMATIVI

I **prior informativi** sono basati sulla conoscenza del dominio:

### 1. Scala dei dati
F2 (seconda formante) per vocali italiane:
- /a/: ~1200 Hz (vocale bassa centrale)
- /i/: ~2200 Hz (vocale alta anteriore)
- /u/: ~1000 Hz (vocale alta posteriore)

### 2. Effetti plausibili
Per stress/personality effects su F2:
- **Small**: 20-50 Hz (2-4% della media)
- **Medium**: 50-100 Hz (5-8%)
- **Large**: 100-200 Hz (10-15%)

Effetti > 200 Hz sarebbero enormi e improbabili.

### 3. Variabilità
F2 ha naturalmente:
- **Between-person SD**: ~150-250 Hz (grande differenza tra persone)
- **Within-person SD**: ~50-100 Hz (variabilità nelle ripetizioni)

## PRIOR INFORMATIVI SPECIFICATI

### Per f2_mean (vocale /a/):

```r
priors_informative <- c(
  # 1. INTERCETTA: Centrata sulla media osservata
  prior(normal(1200, 400), class = "Intercept"),
  # Permette range 400-2000 Hz (±2SD)
  
  # 2. COEFFICIENTI: Effetti moderati stress/personality
  prior(normal(0, 100), class = "b"),
  # Permette effetti da -200 a +200 Hz (±2SD)
  # Questo copre anche gli effetti grandi (~15% della media)
  
  # 3. SIGMA INTERCETTA: Sulla scala log
  prior(normal(4.6, 0.5), class = "Intercept", dpar = "sigma"),
  # log(100) ≈ 4.6
  # Permette SD da ~60 a ~165 Hz
  
  # 4. SIGMA COEFFICIENTI: Piccoli cambiamenti proporzionali
  prior(normal(0, 0.3), class = "b", dpar = "sigma"),
  # Permette variazioni ±30% in SD sotto stress
  
  # 5. RANDOM EFFECTS: Ampia variabilità tra soggetti
  prior(exponential(0.01), class = "sd")
  # Media = 100 Hz, permette range 10-300 Hz
)
```

### Per altre vocali:

Le statistiche variano per vocale:

| Vocale | F2 Mean | F2 SD | Intercept Prior | Sigma Prior |
|--------|---------|-------|-----------------|-------------|
| /a/ | ~1200 | ~200 | normal(1200, 400) | normal(5.3, 0.5) |
| /i/ | ~2200 | ~300 | normal(2200, 600) | normal(5.7, 0.5) |
| /u/ | ~1000 | ~150 | normal(1000, 300) | normal(5.0, 0.5) |

## SCRIPT FORNITI

### 1. script_prior_informativi.R
**Cosa fa**:
- Estrae statistiche descrittive per ogni vocale
- Calcola prior informativi basati sui dati
- Visualizza distribuzioni F2

**Output**:
- `results/f2_descriptive_stats.csv`
- `figures/f2_distributions_by_vowel.pdf`

**Quando usarlo**:
Prima di qualsiasi analisi, per capire la scala dei dati.

### 2. script_test_PRIOR_INFORMATIVI.R
**Cosa fa**:
- Calcola prior informativi automaticamente dai dati
- Fa prior predictive check con prior informativi
- Testa un modello con prior appropriati

**Output**:
- Prior predictive check che dovrebbe essere ragionevole
- Posterior predictive check per verificare fit
- Risultati del modello test

**Quando usarlo**:
Dopo aver verificato le statistiche, per testare la pipeline completa.

## WORKFLOW CONSIGLIATO

### PASSO 1: Estrai statistiche (2 min)
```r
source("script_prior_informativi.R")
```

Controlla l'output:
- Le distribuzioni F2 hanno senso?
- Le medie sono dove te le aspetti (~1200 per /a/)?
- Le SD sono ragionevoli (~100-200)?

### PASSO 2: Prior predictive check (5 min)
```r
source("script_test_PRIOR_INFORMATIVI.R")
```

**CRITICO**: Guarda il plot `prior_predictive_informative_f2.pdf`

**COSA CERCARE**:
✅ **BUONO**: yrep (curve colorate) si sovrappongono ragionevolmente a y (curva nera)
- Stessa scala (1000-1500 Hz)
- Stessa forma (unimodale)
- Range simile

❌ **CATTIVO**: yrep molto diverso da y
- Scale diverse (es. yrep centrato su 0)
- Forme diverse (es. yrep bimodale)
- Range impossibili (es. valori negativi)

### PASSO 3: Fit modello (10 min)
Se prior predictive check è OK, lo script continua automaticamente con il fit.

### PASSO 4: Posterior predictive check
Guarda `posterior_predictive_informative_f2.pdf`

**COSA CERCARE**:
✅ **OTTIMO**: yrep quasi indistinguibile da y
- Il modello ha appreso dai dati
- Fit eccellente

⚠️ **ACCETTABILE**: yrep simile a y ma con qualche differenza
- Fit buono ma non perfetto
- Normale per modelli complessi

❌ **PROBLEMATICO**: yrep molto diverso da y
- Modello non cattura pattern nei dati
- Possibili problemi struttura modello

## INTERPRETAZIONE PRIOR INFORMATIVI

### Prior deboli vs informativi vs strong

```r
# PRIOR TROPPO VAGO (originale - CATTIVO)
prior(normal(0, 100), class = "Intercept")
# Per F2, permette valori -300 a +300 Hz
# Impossibili! F2 è sempre positivo e > 500 Hz

# PRIOR INFORMATIVO (nuovo - BUONO)
prior(normal(1200, 400), class = "Intercept")  
# Per /a/, permette 400-2000 Hz
# Ragionevole! Copre variabilità biologica

# PRIOR TROPPO STRETTO (ipotetico - RISCHIOSO)
prior(normal(1200, 50), class = "Intercept")
# Troppo restrittivo, forse perdi dati validi
# Usa solo se sei MOLTO sicuro della scala
```

### Quanto sono informativi?

I prior che usiamo sono **moderatamente informativi**:
- Non sono completamente vaghi (permetterebbero impossibili)
- Non sono troppo restrittivi (permettono scoperte)
- Sono basati su conoscenze fonetiche solide

### Prior e interpretazione bayesiana

In analisi Bayesiana:
```
Posterior ∝ Likelihood × Prior
```

**Prior informativi**:
- Guidano il modello verso soluzioni plausibili
- Riducono overfitting
- Migliorano convergenza
- Rendono stime più stabili

**MA**: Se prior troppo strong, dominano sui dati!

Regola generale:
- Prior dovrebbero essere **più vaghi** della conoscenza effettiva
- Lasciano che i dati "parlino"
- Ma escludono valori impossibili

## SENSITIVITY ANALYSIS (opzionale)

Per verificare che i risultati non dipendano troppo dai prior:

```r
# Fit con prior informativi
m1 <- brm(..., prior = priors_informative)

# Fit con prior più vaghi
priors_vague <- c(
  prior(normal(1200, 1000), class = "Intercept"),  # Più vago
  prior(normal(0, 200), class = "b"),               # Più vago
  ...
)
m2 <- brm(..., prior = priors_vague)

# Confronta stime
compare_estimates <- bind_rows(
  fixef(m1) %>% as_tibble(rownames = "param") %>% mutate(model = "informative"),
  fixef(m2) %>% as_tibble(rownames = "param") %>% mutate(model = "vague")
)

# Se stime molto simili → risultati robusti
# Se molto diverse → prior troppo influenti
```

## DOMANDE FREQUENTI

**Q: Sto "sbirciando" nei dati per specificare prior?**
A: Sì, ma va bene! Usi statistiche descrittive (media/SD), non le relazioni che vuoi testare. Non stai guardando le correlazioni stress-voice o personality-voice.

**Q: I prior informativi rendono l'analisi meno "oggettiva"?**
A: No. Incorporano conoscenza del dominio (fonetica). Prior completamente vaghi che permettono F2 negativi non sono "oggettivi", sono semplicemente sbagliati.

**Q: Devo riportare i prior nel manoscritto?**
A: Sì! Nel Methods, aggiungi:
```
"We specified weakly informative priors based on the physiological 
range of formant frequencies: intercepts centered on observed means 
with SDs allowing ±2SD range, coefficients normal(0, 100) permitting 
effects up to ±200 Hz, and exponential(0.01) for random effect SDs."
```

**Q: Posso usare prior diversi per /a/, /i/, /u/?**
A: Sì, anzi è raccomandato! Ogni vocale ha scala diversa. Lo script calcola automaticamente prior appropriati per ciascuna.

## CONCLUSIONE

I prior informativi:
1. ✅ Risolvono il problema del prior predictive check
2. ✅ Migliorano convergenza e stabilità
3. ✅ Incorporano conoscenza fonetica
4. ✅ Non "forzano" risultati specifici
5. ✅ Sono standard practice in analisi Bayesiana

**Procedi ora con**:
1. `source("script_test_PRIOR_INFORMATIVI.R")`
2. Verifica prior/posterior predictive checks
3. Se OK, passa allo script completo (che userà stessi prior)
