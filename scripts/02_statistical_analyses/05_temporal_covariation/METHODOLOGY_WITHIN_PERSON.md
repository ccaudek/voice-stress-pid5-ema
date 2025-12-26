# Analisi Covariazione Within-Person: PID-5 EMA e Caratteristiche Vocali

## Razionale

Questa analisi estende l'approccio di moderazione between-person esplorando 
la **covariazione within-person** tra fluttuazioni momentanee del PID-5 EMA 
e caratteristiche vocali nei 3 timepoint di registrazione (baseline, pre-esame, 
post-esame).

**Domanda di ricerca:**  
Le deviazioni momentanee di un individuo dal proprio livello medio di 
personalità (PID-5) sono associate a deviazioni contemporanee nelle sue 
caratteristiche vocali?

## Metodologia

### 1. Struttura dei Dati

- **Caratteristiche vocali:** 3 osservazioni per soggetto
  - Baseline (lontano dall'esame)
  - Pre-esame (giorno prima)
  - Post-esame (giorno dopo)

- **PID-5 EMA:** ~20 misurazioni ripetute per soggetto
  - Distribuite su ~2.5 mesi
  - Categorizzate in 3 periodi: baseline, pre_exam, post_exam

### 2. Matching Temporale

Per ogni soggetto e timepoint vocale:
1. Identificare tutte le misurazioni EMA dello stesso periodo (exam_period)
2. Aggregare (media) le misurazioni EMA per timepoint
3. Risultato: 3 misurazioni PID-5 aggregate per soggetto, allineate ai 
   3 timepoint vocali

### 3. Decomposizione Within/Between Person

Per ogni variabile (PID-5 e voce):

**Person Mean (PM):**  
`PM_i = media(X_i1, X_i2, X_i3)`

**Within-Person Deviation (WP):**  
`WP_it = X_it - PM_i`

Dove:
- `i` = soggetto
- `t` = timepoint (1=baseline, 2=pre, 3=post)

**Interpretazione:**
- PM cattura le differenze stabili **tra** persone
- WP cattura le fluttuazioni **entro** persona rispetto al proprio livello medio

### 4. Analisi di Covariazione

#### A. Approccio Correlazionale (R base)

**Within-person correlations:**  
`cor(PID5_WP, F0_WP)`

- Rimuove la variabilità between-person
- Focus puro sulle fluttuazioni intra-individuali
- Interpretazione: quando un individuo devia dal suo livello medio di 
  personalità, devia anche dalla sua F0 tipica?

#### B. Approccio Bayesiano Gerarchico (Stan)

**Modello:**

```
Livello 1 (within-person):
F0_it = α + α_i + β_WP · PID5_WP_it + ε_it

Livello 2 (between-person):
α_i ~ Normal(0, σ_α)
```

**Parametri chiave:**
- `β_WP`: coefficienti within-person (uno per dominio PID-5)
  - Interpretazione: per ogni SD di aumento nella deviazione WP del PID-5, 
    quanto cambia la deviazione WP di F0 (Hz)?
- `σ_α`: variabilità delle intercette random (differenze individuali stabili)
- `R²_within`: proporzione di varianza WP spiegata dai predittori WP

**Vantaggi rispetto a correlazioni semplici:**
1. Stima simultanea di tutti i domini PID-5
2. Controllo per effetti random (intercette individuali)
3. Quantificazione incertezza (intervalli credibili)
4. Posterior predictive checks
5. Model comparison (LOO-CV)

## Interpretazione dei Risultati

### Coefficienti Within-Person (β_WP)

**Scenario 1: β_WP > 0 (CI non include 0)**  
Quando un individuo ha un livello momentaneo di [dominio PID-5] più alto 
del suo solito, tende anche ad avere una F0 più alta del suo solito.

**Esempio pratico:**  
Se β_WP[Negative Affectivity] = 5.2 Hz [2.1, 8.3]:
- In momenti in cui una persona è più negativamente affettiva del suo 
  normale (+1 SD), la sua F0 aumenta di ~5 Hz rispetto al suo normale
- Interpretazione: l'arousal emotivo momentaneo si riflette nella voce

**Scenario 2: β_WP ≈ 0 (CI include 0)**  
Nessuna evidenza di covariazione within-person.  
Possibili interpretazioni:
- Le fluttuazioni momentanee di quel dominio non si riflettono nella voce
- L'effetto è puramente between-person (vedi moderazione)
- Potenza statistica limitata (solo 3 timepoint per soggetto)

### Varianza Spiegata (R²_within)

**R²_within basso (~0.05-0.15):**  
- Normale per analisi within-person con pochi timepoint
- Le deviazioni momentanee PID-5 spiegano una piccola frazione della 
  variabilità vocale within-person
- La maggior parte della variabilità WP è dovuta ad altri fattori 
  (situazionali, fisiologici, errore di misurazione)

**R²_within moderato (~0.20-0.40):**  
- Evidenza robusta di covariazione within-person
- I tratti di personalità momentanei sono predittori rilevanti delle 
  caratteristiche vocali

## Confronto con Analisi di Moderazione

| Aspetto | Moderazione (Between-Person) | Covariazione (Within-Person) |
|---------|------------------------------|------------------------------|
| **Livello di analisi** | Differenze tra persone | Fluttuazioni entro persona |
| **N osservazioni** | N_subj × 3 × N_ema (~20) | N_subj × 3 |
| **Domanda** | I tratti moderano l'effetto stress? | Le fluttuazioni trait covariino con voce? |
| **Parametro chiave** | β_interaction | β_WP |
| **Variabilità** | Between-person | Within-person |
| **Interpretazione** | Chi ha più NA reagisce di più | Quando sei più NA, voce cambia di più |

**Complementarità:**
- **Moderazione:** identifica *chi* è più reattivo allo stress
- **Covariazione WP:** identifica se le fluttuazioni *momentanee* di 
  personalità si riflettono nella voce

## Limitazioni

1. **Pochi timepoint:** solo 3 osservazioni per soggetto limitano la potenza 
   statistica per effetti WP

2. **Aggregazione EMA:** le misurazioni EMA sono aggregate per periodo, 
   perdendo la risoluzione temporale fine

3. **Matching temporale imperfetto:** le misurazioni EMA e vocali non sono 
   esattamente contemporanee (stesso periodo, non stesso momento)

4. **Direzione causale:** l'analisi è correlazionale, non permette inferenze 
   causali

5. **Generalizzabilità:** campione femminile, contesto accademico specifico

## Estensioni Possibili

1. **Più caratteristiche vocali:** analizzare NNE, jitter, shimmer, ecc.

2. **Effetti cross-lagged:** se ci fossero più misurazioni temporali ravvicinate

3. **Moderatori WP:** testare se l'associazione WP varia tra persone 
   (cross-level interaction)

4. **Analisi multilivello completa:** stimare simultaneamente effetti WP e BP 
   in un unico modello

5. **Latent growth curves:** modellare traiettorie temporali esplicite

## File Output

### Da script R base (04_within_person_ema_voice_covariation.R):
- `variance_decomposition.csv`: ICC e scomposizione varianza
- `within_person_correlations.csv`: matrice correlazioni WP
- `f0_pid5_within_correlations.csv`: correlazioni per dominio
- Grafici scatter, boxplot, matrice correlazioni

### Da modello Stan (05_fit_stan_within_person_model.R):
- `parameter_summary.csv`: diagnostici convergenza
- `beta_wp_summary.csv`: coefficienti WP con intervalli credibili
- `r2_within_summary.csv`: varianza spiegata WP
- Grafici: trace, densities, intervals, PPC, LOO

## Riferimenti Teorici

**Within-person analysis:**
- Hamaker, E. L. (2012). Why researchers should think "within-person"
- Curran, P. J., & Bauer, D. J. (2011). The disaggregation of within-person 
  and between-person effects

**EMA & personality:**
- Fleeson, W. (2001). Toward a structure- and process-integrated view of 
  personality
- Wright, A. G. C., & Zimmermann, J. (2019). Applied ambulatory assessment

**Voice & affect:**
- Scherer, K. R. (2003). Vocal communication of emotion
- Bachorowski, J. A., & Owren, M. J. (1995). Vocal expression of emotion
