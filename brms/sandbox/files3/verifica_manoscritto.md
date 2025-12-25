# VERIFICA MANOSCRITTO: STRESS-VOICE-PERSONALITY

## SOMMARIO ESECUTIVO

Ho verificato la coerenza tra l'abstract/metodi e i file di risultati forniti. Emergono **3 problemi critici** e diversi aspetti da chiarire. La struttura generale è solida, ma alcuni valori riportati nell'abstract non corrispondono ai file forniti, probabilmente perché provengono da analisi supplementari (es. modelli pooled across vowels) non incluse nei file caricati.

---

## 1. PROBLEMI CRITICI DA RISOLVERE

### 1.1 Discrepanza dimensione campione
- **Abstract**: "141 university students"
- **File analisi**: n = 109 in tutte le analisi
- **AZIONE RICHIESTA**: Chiarire se:
  - 141 è il campione iniziale e 109 quello finale dopo esclusioni
  - Inserire nel metodo un flowchart delle esclusioni
  - Aggiornare l'abstract con il campione analitico corretto

### 1.2 Beta values non corrispondenti
Diversi coefficienti nell'abstract non compaiono nei file forniti:

| Effetto | Abstract | File forniti | Problema |
|---------|----------|--------------|----------|
| Negative Affectivity × Stress | β=5.81 [2.05, 9.61] | β=5.49 [1.63, 9.33] (vowel /i/) | Valori vicini ma non identici |
| Detachment × Recovery | β=-4.63 [-8.53, -0.72] | β=-3.85 [-7.79, 0.15] (vowel /i/) | Valori diversi + CI include zero |
| Antagonism × Recovery | β=3.65-4.25 | β=2.43-3.39 (tutti marginali) | Range diverso + non significativi |

**IPOTESI**: Questi valori provengono da modelli pooled/meta-analitici across vowels non inclusi nei file.

**AZIONE RICHIESTA**: 
- Fornire i risultati dei modelli pooled, OPPURE
- Aggiornare l'abstract con i valori dei modelli per singola vocale
- Considerare se alcuni effetti sono abbastanza robusti per essere riportati come "significativi"

### 1.3 Risultati formant completamente mancanti
L'abstract riporta:
- "Psychoticism moderated articulatory instability under stress (F2 variability: β=-0.28 [-0.55, -0.00])"
- "Negative Affectivity effects extended to formant stability (F2 SD: β=-0.16 [-0.27, -0.04])"
- "within-person emotional lability uniquely predicted articulatory (not prosodic) stress responses"

**PROBLEMA**: I file forniti includono solo f0_mean, f0_std, jitter, nne. Non ci sono risultati per:
- F1_mean, F2_mean (formant frequencies)
- F1_std, F2_std (formant variability)
- Temporal covariation per formants

**AZIONE RICHIESTA**: Fornire i file con i risultati completi dei modelli formant.

---

## 2. PROBLEMI MODERATI

### 2.1 Soglia R² leggermente superata
- **Abstract**: "ΔR²<0.025"
- **Dati**: 
  - F0: ΔR² = -0.004 ✓
  - F2: ΔR² = -0.030 ✗ (supera la soglia)

**RACCOMANDAZIONE**: 
- Cambiare la formulazione in "ΔR²<0.03" oppure
- Scrivere "minimal differences (ΔR² ranging from 0.004 to 0.030)"

### 2.2 Jitter e voice quality
L'abstract dice: "paradoxically improved voice quality (reduced jitter, enhanced harmonic clarity)"

Nei dati:
- NNE mostra miglioramento sotto stress ✓
- Jitter NON mostra effetti significativi di stress ✗

**RACCOMANDAZIONE**: Rimuovere il riferimento a jitter oppure specificare "minimal jitter changes"

### 2.3 Effetti "consistently significant" che sono marginali
Per Antagonism × Recovery, l'abstract dice "consistently facilitated it across vowels" ma:
- /a/: marginale (CI: -0.40, 5.82)
- /i/: non significativo
- /u/: marginale (CI: -0.03, 6.75)

**RACCOMANDAZIONE**: Riformulare come "showed a consistent trend toward facilitating recovery" oppure verificare se un modello pooled è significativo.

---

## 3. VALIDAZIONI POSITIVE

### 3.1 EMA validation ✓
Le correlazioni EMA-baseline (r=0.35-0.55) supportano l'affermazione di equivalenza predittiva.

### 3.2 Main effects stress ✓
- Pitch aumenta 3-4 Hz: confermato (β=3.70 per /a/, β=4.02 per /u/)
- Voice quality migliora: confermato per NNE

### 3.3 Negative Affectivity × Stress ✓ (con riserva)
Effetto significativo per vowel /i/, anche se il valore esatto differisce lievemente.

---

## 4. ANALISI AGGIUNTIVE NECESSARIE

Per completare la verifica e supportare tutte le affermazioni dell'abstract, servono:

### 4.1 PRIORITÀ ALTA
```r
# 1. Modelli formant completi
# Per ogni vocale (/a/, /i/, /u/):
outcomes_formant <- c("f1_mean", "f1_std", "f2_mean", "f2_std")

# 2. Temporal covariation per formants
# Between-within decomposition per F2_std

# 3. Meta-analisi across vowels (se non già fatta)
# Pooled estimates per gli effetti principali
```

### 4.2 PRIORITÀ MEDIA
```r
# 4. Effect sizes (Cohen's d)
# L'abstract menziona d=0.15-0.25 per main effects

# 5. Tabella descrittiva campione
# Età M, SD
# N esclusioni con motivi

# 6. Sensitivity analyses
# Verificare robustezza effetti con/senza outliers
```

### 4.3 SUPPLEMENTARI (utili per reviewer)
```r
# 7. Model comparison tables
# Confronto modelli con/senza interazioni

# 8. Prediction accuracy
# Cross-validation o out-of-sample prediction

# 9. Robustness checks
# Effetti con diversi prior
# Effetti con listwise vs multiple imputation
```

---

## 5. CHECKLIST PRE-SUBMISSION

### Dati e codice
- [ ] Verificare N finale nelle analisi (109 vs 141)
- [ ] Generare tutti i risultati formant (F1, F2)
- [ ] Verificare provenienza beta values in abstract
- [ ] Considerare modelli pooled across vowels se utili

### Abstract
- [ ] Aggiornare beta values con dati verificati
- [ ] Correggere soglia ΔR² (0.025 → 0.03)
- [ ] Rimuovere/qualificare menzione jitter
- [ ] Attenuare claim su Antagonism ("consistent trend")
- [ ] Verificare claim su Detachment recovery (CI include zero)

### Methods
- [ ] Inserire età M, SD del campione
- [ ] Flowchart esclusioni (da 141 a 109?)
- [ ] Aggiungere n finale dopo quality checks EMA
- [ ] Verificare coerenza n. items EMA (15 items: 3×5 domini)

### Results
- [ ] Assicurare coerenza numerica completa con tabelle
- [ ] Riportare tutti gli effetti menzionati in abstract
- [ ] Decidere strategia per effetti marginali (Detachment, Antagonism)

### Supplementary Materials
- [ ] Tabelle complete tutti i modelli (anche non significativi)
- [ ] Diagnostic plots (trace plots, posterior predictive checks)
- [ ] Sensitivity analyses
- [ ] Codice riproducibile completo

---

## 6. RACCOMANDAZIONI STRATEGICHE

### 6.1 Gestione effetti marginali
Per Detachment × Recovery (β=-3.85, CI=[-7.79, 0.15]):

**Opzione A - Conservativa**: 
"Detachment showed a trend toward impairing recovery (β=-3.85, 95% CI [-7.79, 0.15], p=0.06)"

**Opzione B - Bayesian interpretation**: 
"The posterior distribution for Detachment's effect on recovery was predominantly negative (β=-3.85), with 97% posterior probability of harm"

**Opzione C - Meta-analitica**: 
Se hai modelli pooled che sono significativi, usa quelli

### 6.2 Strategia formant results
Poiché formants sono centrali per le conclusioni ("articulatory vs prosodic"), suggerisco:

1. Creare sezione Results dedicata: "Articulatory Control: Formant Analysis"
2. Presentare prima effetti prosodici (F0), poi articolatori (F1, F2)
3. Contrasto esplicito: "While Negative Affectivity modulated prosodic features (F0), Psychoticism primarily affected articulatory control (F2 variability)"

### 6.3 Gestione campione
Per chiarire la discrepanza 141 vs 109:

```
Participants
-----------
We initially recruited 141 female university students. After applying 
data quality criteria (see Data Quality section), the final analytical 
sample comprised 109 participants who completed:
- Full PID-5 at baseline
- ≥50% EMA compliance (≥12 assessments)
- Valid voice recordings at all three timepoints
```

---

## 7. DOMANDE SPECIFICHE PER TE

1. **Modelli pooled**: Hai fatto meta-analisi across vowels? I beta nell'abstract provengono da lì?

2. **Strategia missing**: Hai usato listwise deletion o multiple imputation? Questo spiega il drop da 141 a 109?

3. **Prior sensitivity**: Hai fatto sensitivity analysis con diversi prior? Importante per effetti marginali.

4. **Formants**: Quali modelli hai già run per formants? Posso aiutarti a generarli se mancano.

5. **Effect sizes**: Come hai calcolato Cohen's d menzionati nell'abstract?

6. **Recovery definition**: c2_recovery è il contrasto post-exam vs baseline o post vs pre?

---

## 8. PROSSIMI PASSI SUGGERITI

1. **IMMEDIATO**: Inviami i risultati formant se li hai già
2. **BREVE TERMINE**: Chiarire provenienza beta values e decidere strategia per abstract
3. **MEDIO TERMINE**: Generare analisi mancanti e aggiornare manoscritto
4. **PRIMA SUBMISSION**: Triple-check coerenza numerica tra abstract, tables, e text

Sono pronto ad aiutarti con qualsiasi di questi aspetti. Dimmi da dove vuoi iniziare!
