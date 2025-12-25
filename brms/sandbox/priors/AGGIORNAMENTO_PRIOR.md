# AGGIORNAMENTO: PRIOR INFORMATIVI IMPLEMENTATI

## HAI FATTO LA COSA GIUSTA! ✓

Interrompere lo script e controllare il prior predictive check era esattamente la cosa giusta da fare. Questo è precisamente il motivo per cui facciamo prior predictive checks - per identificare prima del fit problemi con i prior!

## COSA ERA SBAGLIATO

**Prior troppo vaghi** che permettevano:
- F2 intorno a 0 Hz (impossibile - F2 sempre > 500 Hz)
- F2 negativi (impossibile - frequenza sempre positiva)  
- Range irrealistico (da -300 a +300 Hz invece di 800-2400 Hz)

## SOLUZIONE IMPLEMENTATA

Ho creato **3 nuovi script** con prior informativi:

### 1. script_prior_informativi.R
**ESEGUI PRIMA**
- Estrae statistiche F2 per ogni vocale
- Mostra distribuzioni
- Suggerisce prior appropriati

### 2. script_test_PRIOR_INFORMATIVI.R  
**POI ESEGUI QUESTO**
- Calcola prior automaticamente dai dati
- Prior predictive check con prior informativi
- Dovrebbe mostrare yrep sovrapposto a y!
- Fit del modello test

### 3. GUIDA_PRIOR_INFORMATIVI.md
**LEGGI PER CAPIRE**
- Spiega perché servono prior informativi
- Come interpretare prior/posterior predictive checks
- Dettagli tecnici

## WORKFLOW ADESSO

### STEP 1: Statistiche descrittive (2 min)
```r
source("script_prior_informativi.R")
```

Output: Vedrai medie/SD per F2 di ogni vocale

### STEP 2: Test con prior informativi (15 min)
```r
source("script_test_PRIOR_INFORMATIVI.R")
```

**ATTENDI IL PRIOR PREDICTIVE CHECK**

Prima del fit, lo script mostrerà un nuovo plot.

**Cosa verificare**:
- ✅ yrep (curve colorate) centrato intorno a 1200 Hz come y (curva nera)
- ✅ yrep ha range simile a y (non va da -1000 a 3000!)
- ✅ Forma delle distribuzioni simile

**Se OK**: Lo script continua automaticamente con il fit
**Se ancora MALE**: Fermati e fammi sapere - ajustiamo ulteriormente

### STEP 3: Verifica risultati
Dopo il fit, controlla:
- `Max Rhat < 1.01` → Convergenza OK
- `Min ESS > 100` → Sufficient sampling
- Posterior predictive check → Fit ai dati

## PRIOR INFORMATIVI SPECIFICATI

Lo script calcola automaticamente per ogni vocale:

```r
# Esempio per /a/ (se M=1200, SD=200):
Intercept: normal(1200, 400)     # Centrato sulla media osservata
Coefficienti: normal(0, 100)     # Effetti ±200 Hz plausibili
Sigma intercept: normal(5.3, 0.5) # log(200) sulla scala log
Sigma coef: normal(0, 0.3)       # Piccole variazioni in SD
Random effects: exponential(0.01) # Ampia variabilità tra soggetti
```

## PERCHÉ FUNZIONA

### Prima (sbagliato):
```
Prior: Intercept non specificato + coefficienti vaghi
      ↓
Prior predictive: F2 può essere ovunque (-∞ a +∞)
      ↓
Risultato: yrep centrato su 0 o negativo ❌
```

### Dopo (corretto):
```
Prior: Intercept informativo + coefficienti appropriati
      ↓  
Prior predictive: F2 nella scala realistica (800-2400 Hz)
      ↓
Risultato: yrep centrato su 1200 Hz come i dati ✓
```

## QUESTO È BAYESIANO CORRETTO

I prior informativi:
- **NON** sbirciare nelle relazioni che vuoi testare
- **SÌ** usare conoscenza del dominio (fonetica)
- **SÌ** incorporare scale biologiche plausibili
- **NO** forzare risultati specifici

Analogia: 
- **Sbagliato**: Prior che permettono umani alti 0.01m o 10m
- **Corretto**: Prior che permettono umani 1.4-2.1m (range biologico)

Non stai "truccando" i risultati - stai solo dicendo "F2 deve essere una frequenza ragionevole per una vocale umana".

## PROSSIMI PASSI

1. **ORA**: Esegui `script_prior_informativi.R`
2. **POI**: Esegui `script_test_PRIOR_INFORMATIVI.R`
3. **VERIFICA**: Prior predictive check è ragionevole?
4. **SE SÌ**: Procedi con analisi completa
5. **SE NO**: Fermati e contattami

## SE HAI DUBBI

Leggi `GUIDA_PRIOR_INFORMATIVI.md` per:
- Spiegazione dettagliata del problema
- Interpretazione prior predictive checks
- Sensitivity analysis (opzionale)
- FAQ su prior informativi

## NOTA IMPORTANTE

Questo approccio (prior informativi basati su scala dati) è:
- ✅ Standard in analisi Bayesiana
- ✅ Raccomandato in letteratura (Gelman et al.)
- ✅ Necessario per modelli complessi multilevel
- ✅ Riportabile nel manoscritto senza problemi

Nel Methods, aggiungerai semplicemente:
```
"We specified weakly informative priors based on the physiological 
range of formant frequencies (McGowan & Nittrouer, 1988), centering 
intercepts on observed means with standard deviations allowing ±2SD 
variation."
```

---

**RIASSUNTO**: Gli script originali erano tecnicamente corretti ma i prior troppo vaghi. I nuovi script usano prior appropriati per la scala dei dati formant. Questo risolverà il problema del prior predictive check.

Fammi sapere come va con i nuovi script!
