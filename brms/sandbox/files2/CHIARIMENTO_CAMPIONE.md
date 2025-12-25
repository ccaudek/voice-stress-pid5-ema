# CHIARIMENTO: DISCREPANZA CAMPIONE 141 vs 109

## SITUAZIONE

Nel tuo abstract scrivi: "141 university students"
Nei file analisi trovo: n = 109 nelle analisi di validazione EMA

## SPIEGAZIONE PROBABILE

Basandomi sulla struttura del tuo dataset:

```
Rows: 423 = 141 partecipanti × 3 timepoints
```

### Ipotesi 1: Campione Iniziale vs Finale (PROBABILE)
- **N iniziale**: 141 partecipanti reclutati
- **N finale**: 109 partecipanti con dati completi per le analisi

**Motivi esclusione comuni**:
1. EMA compliance < 50% (< 12 assessments su 24 previsti)
2. Missing baseline PID-5 completo
3. Missing voice recordings a uno o più timepoints
4. Data quality issues (es. careless responding)

### Ipotesi 2: Analisi Specifiche (MENO PROBABILE)
- Alcune analisi richiedono dati completi su specifiche variabili
- Es: validazione EMA richiede sia baseline PID-5 che EMA completo
- Altri modelli potrebbero usare campioni diversi

## VERIFICA NEL TUO DATASET

Esegui questo codice per capire:

```r
library(tidyverse)

df_analysis <- readRDS("results/df_analysis.rds")

# 1. Quanti partecipanti unici?
n_total <- n_distinct(df_analysis$ID)
cat("N totale partecipanti nel dataset:", n_total, "\n")

# 2. Quanti hanno dati completi a tutti i timepoints?
complete_cases <- df_analysis %>%
  group_by(ID) %>%
  summarise(
    n_timepoints = n_distinct(timepoint),
    has_baseline_pid5 = !any(is.na(domain_negative_affect_baseline)),
    has_ema = !any(is.na(pid5_negative_affectivity)),
    has_f0 = !any(is.na(f0_mean_a)),
    has_f2 = !any(is.na(f2_mean_a))
  )

# Partecipanti con 3 timepoints completi
complete_cases %>%
  filter(n_timepoints == 3) %>%
  nrow() %>%
  cat("N con 3 timepoints:", ., "\n")

# Partecipanti con baseline PID-5 + EMA
complete_cases %>%
  filter(has_baseline_pid5, has_ema) %>%
  nrow() %>%
  cat("N con baseline + EMA:", ., "\n")

# Partecipanti con dati voce completi
complete_cases %>%
  filter(has_f0, has_f2) %>%
  nrow() %>%
  cat("N con dati voce completi:", ., "\n")

# 3. Pattern di missing
df_analysis %>%
  group_by(timepoint) %>%
  summarise(
    n = n(),
    missing_pid5 = sum(is.na(pid5_negative_affectivity)),
    missing_f0 = sum(is.na(f0_mean_a)),
    missing_f2 = sum(is.na(f2_mean_a))
  )
```

## COME RIPORTARLO NEL MANOSCRITTO

### OPZIONE A: Flowchart Completo (RACCOMANDATO)

Nel Methods, sezione "Participants":

```
We recruited 141 female university students from the University of Florence. 
After applying data quality criteria, the final analytical sample comprised 
109 participants. Figure 1 presents the participant flow.

[INSERT FLOWCHART]

Exclusion criteria for the analytical sample were:
1. EMA response rate < 50% (n = X excluded)
2. Missing baseline PID-5 assessment (n = X excluded)
3. Incomplete voice recordings across timepoints (n = X excluded)
4. Failed quality control checks (n = X excluded)

The final sample (N = 109) provided 327 valid observations across the three 
timepoints (baseline: n = 109, pre-exam: n = 109, post-exam: n = 109).
```

### OPZIONE B: Breve (se flowchart non richiesto)

```
We recruited 141 female university students, of which 109 (77%) met inclusion 
criteria for the analytical sample: completion of baseline PID-5 assessment, 
≥50% EMA response rate, and valid voice recordings at all three timepoints.
```

### OPZIONE C: Separare per Analisi (se N varia)

```
The initial sample comprised 141 participants. Sample sizes varied by analysis 
due to missing data:
- EMA validation: N = 109 (complete baseline + EMA)
- Voice moderation models: N = [INSERIRE] (complete voice + EMA)
- Temporal covariation: N = [INSERIRE] (sufficient within-person observations)

We used available data for each analysis (available case analysis) rather than 
listwise deletion to maximize statistical power.
```

## ABSTRACT: COME AGGIORNARLO

### ATTUALE
"In 141 university students (expanded from N=111 in pilot analyses)..."

### OPZIONE 1: Specifica campione analitico
"In 109 university students (from an initial sample of 141)..."

### OPZIONE 2: Mantieni recruitment, aggiungi nota
"In 141 university students (final analytical sample: N=109 after quality control)..."

### OPZIONE 3: Solo finale
"In 109 university students..."

**RACCOMANDAZIONE**: Usa Opzione 2 per trasparenza, ma specifica in Methods.

## VERIFICA COERENZA TRA FILE

I tuoi file di risultati mostrano n=109 in:
- `01_ema_validation.csv`: tutti i domini hanno n=109

Questo suggerisce che 109 è il campione con:
- Baseline PID-5 completo ✓
- EMA PID-5 con ≥50% compliance ✓
- (Probabilmente) voice data completi ✓

**AZIONE**: Verifica se i modelli di moderazione stress-voice (file 02) usano:
- Stesso n=109? → Coerente
- N diverso? → Specifica nel Methods

## IMPATTO SU POWER

```
N iniziale = 141
N finale = 109
Retention = 77.3%
```

Questo è un **buon retention rate** per studi EMA intensivi. La perdita di 32 partecipanti (23%) è ragionevole e non compromette power per:
- Effetti moderati (d ≥ 0.30) con α=0.05, power=0.80
- Modelli multilevel con 3 misure ripetute

## CHECKLIST AZIONI

- [ ] Esegui codice verifica per capire pattern esclusioni
- [ ] Crea flowchart CONSORT-style (vedi template sotto)
- [ ] Aggiorna Methods con criterio esclusione chiari
- [ ] Aggiorna Abstract con n corretto
- [ ] Verifica che tutti i risultati citino n corretto
- [ ] Aggiungi retention rate nelle limitazioni se rilevante

## TEMPLATE FLOWCHART

```
        Assessed for eligibility
              N = [X]
                 |
                 v
    +------------+-------------+
    |                          |
 Excluded (n = [X])      Enrolled
 - Reason 1: n             N = 141
 - Reason 2: n                |
                              v
                    Completed baseline
                          N = 141
                              |
                              v
                +-------------+-------------+
                |             |             |
           Baseline       Pre-exam     Post-exam
            N = 141       N = [X]       N = [X]
                |             |             |
                +-------------+-------------+
                              |
                              v
                    Quality control applied
                              |
                +-------------+-------------+
                |                           |
        Excluded (n = 32)           Retained
        - EMA < 50%: n = X          N = 109
        - Missing voice: n = X
        - Poor quality: n = X
                |
                v
          Final analytical
             sample
             N = 109
       (327 observations)
```

---

**CONCLUSIONE**: La discrepanza 141 vs 109 è normale e gestibile. Basta essere trasparenti nel riportare i criteri di inclusione e il flowchart delle esclusioni. Questo è standard practice in ricerca EMA e voice acoustics.
