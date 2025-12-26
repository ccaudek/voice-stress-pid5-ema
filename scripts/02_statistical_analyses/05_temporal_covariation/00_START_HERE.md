# Analisi Covariazione Within-Person: PID-5 EMA e Voce

## 📋 Panoramica Completa

Ho preparato un pacchetto completo per analizzare la **covariazione within-person** 
tra le fluttuazioni momentanee del PID-5 EMA e le caratteristiche vocali nei 
3 timepoint (baseline, pre-esame, post-esame).

Questo approccio è **complementare** all'analisi di moderazione già completata:
- **Moderazione (between-person):** chi ha più Negative Affectivity reagisce 
  di più allo stress
- **Covariazione (within-person):** quando una persona è momentaneamente più 
  alta del suo solito su NA, la sua voce cambia di più

## 📦 Contenuto del Pacchetto

### 1. Script Principale (OBBLIGATORIO)

**`00_quick_test_setup.R`**
- Test rapido per verificare che dati e pacchetti siano ok
- Da eseguire PRIMA di tutto
- Tempo: ~30 secondi

**`04_within_person_ema_voice_covariation.R`**
- Analisi esplorativa completa
- Matching temporale EMA-voce
- Calcolo deviazioni within-person
- Correlazioni, grafici, preparazione dati Stan
- Tempo: 1-2 minuti

### 2. Analisi Bayesiana (OPZIONALE ma CONSIGLIATA)

**`05_fit_stan_within_person_model.R`**
- Fitting modello gerarchico con Stan
- Inferenza robusta con intervalli credibili
- Diagnostici convergenza, PPC, LOO-CV
- Richiede: cmdstanr installato
- Tempo: 5-15 minuti

**`within_person_covariation.stan`**
- Modello Bayesiano gerarchico
- Stima effetti within-person controllando per random intercepts
- Quantifica incertezza parametri

### 3. Documentazione

**`README_WITHIN_PERSON.md`** ⭐ START HERE
- Guida operativa step-by-step
- Interpretazione risultati
- Troubleshooting comuni
- Checklist completa

**`METHODOLOGY_WITHIN_PERSON.md`**
- Razionale metodologico dettagliato
- Confronto between vs within-person
- Limitazioni e estensioni possibili
- Riferimenti teorici

## 🚀 Quick Start

### Passo 1: Test Setup (30 sec)
```r
source("00_quick_test_setup.R")
```

Verifica:
- ✓ Pacchetti R installati
- ✓ File dati esistenti e accessibili
- ✓ Variabili PID-5 presenti
- ✓ (Opzionale) CmdStan funzionante

### Passo 2: Analisi Esplorativa (1-2 min)
```r
source("04_within_person_ema_voice_covariation.R")
```

Output in `results/within_person_covariation/`:
- Correlazioni within-person
- Grafici scatter per dominio
- Decomposizione varianza (ICC)
- Dati preparati per Stan

### Passo 3: Modello Bayesiano (5-15 min, opzionale)
```r
source("05_fit_stan_within_person_model.R")
```

Output in `results/within_person_stan/`:
- Coefficienti β_WP con intervalli credibili
- R² within-person
- Diagnostici convergenza
- PPC e LOO-CV

## 🎯 Cosa Aspettarsi

### Risultato Atteso 1: Correlazioni Significative
Se trovi correlazioni WP significative per Negative Affectivity:

```
domain                         cor_f0_within  p_value
pid5_negative_affectivity      0.15           0.045
```

**Interpretazione:**
Quando una persona è momentaneamente più negativamente affettiva del suo 
solito, la sua F0 tende ad essere più alta del suo solito.

**Implicazione teorica:**
Le fluttuazioni stato-dipendenti della personalità si riflettono nella voce, 
suggerendo un meccanismo di accoppiamento momento-per-momento tra affetto e 
produzione vocale.

### Risultato Atteso 2: Effetti Nulli su Altri Domini
Se trovi correlazioni WP non significative per Detachment, Antagonism:

```
domain                  cor_f0_within  p_value
pid5_detachment         -0.03          0.714
pid5_antagonism          0.05          0.532
```

**Interpretazione:**
Le fluttuazioni momentanee di questi domini non si riflettono sistematicamente 
nella voce.

**Implicazione teorica:**
Dissociazione tra diversi aspetti della personalità: l'arousal emotivo (NA) 
si riflette nella voce, ma il distacco interpersonale (Detachment) no.

### Risultato Atteso 3: R² Within Basso
```
r2_within: 0.12 [0.03, 0.23]
```

**Interpretazione:**
~12% della varianza within-person in F0 è spiegata dai 5 domini PID-5.

**È normale?** SÌ, perché:
1. Solo 3 timepoint per soggetto (potenza limitata)
2. Voce influenzata da molti fattori (situazione, fisiologia)
3. Errore di misurazione in EMA e voce

**Cosa significa?** Anche un R² piccolo può essere teoricamente rilevante se 
i coefficienti sono robusti e interpretabili.

## 📊 Confronto con Analisi di Moderazione

| Aspetto | Moderazione (già fatta) | Covariazione (nuovo) |
|---------|------------------------|----------------------|
| **Livello** | Between-person | Within-person |
| **N obs** | N_subj × N_ema (~20/subj) | N_subj × 3 |
| **Domanda** | Chi reagisce di più? | Quando reagisce di più? |
| **Parametro** | β_interaction | β_WP |
| **Esempio** | Chi ha più NA → F0 ↑ sotto stress | Quando NA ↑ → F0 ↑ |

**Complementarità:**
1. **Moderazione** identifica differenze stabili tra persone
2. **Covariazione WP** identifica dinamiche momento-per-momento entro persona

**Insieme** forniscono un quadro completo della relazione personalità-voce.

## 🔬 Valore Aggiunto per il Manoscritto

### Sezione Results
Puoi aggiungere una sotto-sezione:

> **Within-Person Covariation Between Momentary PID-5 and Voice**
>
> To complement the between-person moderation analysis, we examined whether 
> momentary fluctuations in PID-5 domains covaried with acoustic features 
> within individuals. For each participant and timepoint, we calculated 
> within-person deviations from their person mean [...]

### Interpretazione Teorica
La covariazione WP permette di distinguere:

1. **Trait moderation (between):** 
   - "Individui con alto NA sono più reattivi allo stress"
   - Meccanismo: vulnerabilità disposizionale

2. **State covariation (within):**
   - "Momenti di alto NA → voce più acuta"
   - Meccanismo: accoppiamento affetto-voce momento-per-momento

### Figura Aggiuntiva Possibile
Scatter plot facetted per dominio PID-5:
- X-axis: PID-5 WP deviation
- Y-axis: F0 WP deviation
- Regression line con 95% CI
- Caption: "Within-person covariation between momentary personality 
  fluctuations and voice"

## ⚠️ Limitazioni da Menzionare

1. **Pochi timepoint:** solo 3 misurazioni per soggetto limitano la potenza 
   per effetti WP

2. **Matching temporale:** EMA aggregate per periodo, non esattamente 
   contemporanee alle registrazioni vocali

3. **Direzione causale:** analisi correlazionale, non permette inferenze causali

4. **Generalizzabilità:** campione femminile, contesto accademico

## 📝 Checklist Pre-Esecuzione

Prima di eseguire gli script, verifica:

- [ ] Hai completato `01_clean_after_merge.R` (file `ema_plus_scales_cleaned.csv` esiste)
- [ ] File AUDIO.xlsx con sheet BASELINE, PRE, POST accessibile
- [ ] Pacchetti tidyverse, readxl, here, rio, lubridate installati
- [ ] (Opzionale) cmdstanr e CmdStan installati per modello Bayesiano
- [ ] Directory `data/raw/` e `data/processed/` esistono
- [ ] Hai 15-20 minuti disponibili per analisi completa

## 🆘 Supporto

### In caso di problemi:

1. **Esegui prima:** `00_quick_test_setup.R`
   - Ti dirà esattamente cosa manca

2. **Consulta:** `README_WITHIN_PERSON.md`
   - Sezione Troubleshooting per errori comuni

3. **Verifica:** versioni pacchetti
   ```r
   packageVersion("tidyverse")  # >= 2.0.0
   packageVersion("cmdstanr")   # >= 0.7.0 (se usato)
   ```

4. **Path non standard?** 
   - Modifica in testa agli script:
   ```r
   voice_path <- here::here("TUO/PATH/AUDIO.xlsx")
   ```

## 📚 Letture Consigliate

Per contestualizzare l'analisi within-person:

1. **Hamaker, E. L. (2012).** Why researchers should think "within-person"  
   *New Ideas in Psychology*

2. **Wright, A. G. C., & Zimmermann, J. (2019).** Applied ambulatory assessment  
   *Psychological Assessment*

3. **Fleeson, W. (2001).** Toward a structure- and process-integrated view  
   *Journal of Personality and Social Psychology*

---

**Buon lavoro con l'analisi!** 🎉

Per qualsiasi domanda metodologica o tecnica, consulta la documentazione 
inclusa o fammi sapere.

Corrado
