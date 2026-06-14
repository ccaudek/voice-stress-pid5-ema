# QUICK START - SUITE ANALISI FORMANT

## 🚀 PARTENZA RAPIDA

### 1. Copia la suite nella directory del progetto

```bash
# Dalla directory del tuo progetto R:
cp -r /path/to/SUITE_FORMANT ./
```

### 2. Verifica prerequisiti

```r
# Controlla che esista il file dati
file.exists("results/df_analysis.rds")  # Deve essere TRUE

# Controlla pacchetti
library(tidyverse)  # OK?
library(brms)       # OK?
library(cmdstanr)   # OK?
```

### 3. Esegui analisi

**MODO SEMPLICE** (tutto automatico):
```r
source("SUITE_FORMANT/00_master_script.R")
```

Lo script ti chiederà conferma prima dell'analisi completa (2-4 ore).

---

## ⏱️ TIMELINE

```
00:00 - Step 1: Diagnostica outliers        [1 min]
00:01 - Step 2: Winsorizzazione (se nec.)   [1 min]
00:02 - Step 3: Calcolo prior               [30 sec]
00:03 - Step 4: Test modello                [10 min]
00:13 - ⏸️  PAUSA - Verifica test OK
00:13 - Step 5: Conferma per analisi completa
00:14 - Step 5: Analisi completa            [2-4 ore]
02:14 - Step 6: Estrazione risultati        [1 min]
02:15 - ✓ COMPLETATO
```

---

## 🎯 CHECKPOINT OBBLIGATORI

### Checkpoint 1: Dopo Step 4

**File da controllare**: `figures/04_prior_check.pdf`

**Domanda**: Le curve colorate (yrep) si sovrappongono alla curva nera (y)?

- ✅ **SÌ**: Procedi → Step 5
- ❌ **NO**: FERMA → Debug necessario

### Checkpoint 2: Dopo Step 4

**Console output**: Cerca "CONVERGENZA OK" o "PROBLEMI"

- ✅ **"CONVERGENZA OK"**: Procedi → Step 5
- ❌ **"PROBLEMI"**: FERMA → Debug necessario

---

## 📁 OUTPUT FINALE

Dopo completamento, troverai:

**PRINCIPALE**:
- `results/05_formant_significant_effects.csv` ← **QUESTO È IMPORTANTE!**

**SECONDARI**:
- `results/05_formant_moderation_results.csv` (tutti i risultati)
- `results/06_convergence_summary.csv` (diagnostica)

---

## 🐛 PROBLEMI COMUNI

### Errore: "df_analysis.rds non trovato"

**Fix**: Sei nella directory sbagliata
```r
setwd("/path/to/your/project")
```

### Prior check fallisce (yrep lontano da y)

**Fix**: Apri `SUITE_FORMANT/04_test_modello.R`

Trova:
```r
prior(normal(0, 100), class = "b"),
```

Verifica che NON sia `normal(0, 1000)` (typo comune!).

### Test non converge

**Fix**: Apri `SUITE_FORMANT/04_test_modello.R`

Cambia:
```r
control = list(adapt_delta = 0.95)
```
in:
```r
control = list(adapt_delta = 0.99)
```

E ri-esegui solo step 4:
```r
source("SUITE_FORMANT/04_test_modello.R")
```

---

## 📊 DOPO L'ANALISI

### Controlla risultati significativi

```r
results <- read_csv("results/05_formant_significant_effects.csv")

# Filtra interazioni chiave per abstract
key <- results %>%
  filter(str_detect(parameter, "c1_stress:pid5_psychoticism|
                                c1_stress:pid5_negative_affectivity|
                                c2_recovery:pid5_detachment"))

print(key)
```

### Confronta con abstract

**Abstract dice**:
- Psychoticism moderated F2 variability: β = -0.28 [-0.55, -0.00]
- Negative Affectivity effects: β = -0.16 [-0.27, -0.04]

**Verifica**:
```r
# Cerca questi parametri nei risultati
results %>%
  filter(str_detect(parameter, "psychoticism.*f2_std|
                                negative_affectivity.*f2"))
```

Se valori diversi → Aggiorna abstract con valori corretti!

---

## 💡 TIPS

### Velocizza analisi (se urgente)

Apri `SUITE_FORMANT/05_analisi_formant_completa.R`

Cambia:
```r
chains = 4,
iter = 4000,
```
in:
```r
chains = 2,    # Meno chains
iter = 2000,   # Meno iterazioni
```

**Trade-off**: Più veloce ma meno preciso.

### Riprendi da un certo step

Se interrompi:
```r
# Salta step già completati
source("SUITE_FORMANT/05_analisi_formant_completa.R")  # Riprendi da qui
```

Gli script controllano automaticamente se file esistono già.

---

## ❓ DOMANDE FREQUENTI

**Q: Quanto tempo serve?**
A: 2.5-4.5 ore totali. La maggior parte è Step 5 (2-4 ore).

**Q: Posso eseguire overnight?**
A: Sì! Usa modo automatico e rispondi "y" quando chiede conferma Step 5.

**Q: Cosa faccio se si blocca?**
A: 
1. Ctrl+C per fermare
2. Identifica quale step stava eseguendo
3. Esegui solo quello step in isolamento
4. Controlla errori console

**Q: Devo rifare tutto se cambio qualcosa?**
A: No! Esegui solo gli step necessari:
- Cambi prior → Ri-esegui da step 4
- Cambi winsorizzazione → Ri-esegui da step 2
- Cambi parametri model → Ri-esegui solo step 5

**Q: Dataset winsorizzato o originale?**
A: Scripts decidono automaticamente:
- Se esiste `df_analysis_winsorized.rds` → usa quello
- Altrimenti → usa `df_analysis.rds`

---

## 📞 SUPPORTO

Se problemi persistono:
1. Controlla `README_SUITE.md` per dettagli
2. Leggi sezione DEBUGGING
3. Esegui step singoli per isolare problema
4. Condividi output console e file errore

---

## ✅ CHECKLIST PRE-ESECUZIONE

- [ ] File `results/df_analysis.rds` esiste
- [ ] Pacchetti installati (tidyverse, brms, cmdstanr)
- [ ] cmdstan configurato (`cmdstanr::check_cmdstan_toolchain()`)
- [ ] Sei nella directory corretta del progetto
- [ ] Hai 2-4 ore disponibili (o esegui step-by-step)

**PRONTO?**

```r
source("SUITE_FORMANT/00_master_script.R")
```

🚀 **GO!**
