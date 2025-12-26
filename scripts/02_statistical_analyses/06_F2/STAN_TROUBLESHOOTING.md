# Stan Troubleshooting: F2 Analysis

## ✅ Problema Risolto

**Errore originale:**
```
Index must be of type int or int[] or must be a range. 
Instead found type real.
```

**Causa:** Sintassi `for (d in [1, 2, 3, 5])` non valida in Stan

**Soluzione:** Espanso loop manualmente nel modello corretto

---

## 🚀 Come Usare i Modelli

### Opzione 1: Modello Semplificato (RACCOMANDATO per iniziare)

Nel file `11_fit_stan_f2_disinhibition.R`, usa:
```r
model_type <- "simple"
```

**Vantaggi:**
- Converge più facilmente
- Meno parametri da stimare
- Verifica che dati/setup funzionano
- Comunque stima l'interazione chiave!

**Limitazioni:**
- No random slopes (solo random intercepts)
- Assume stress effect uguale per tutti (al netto di Disinhibition)

### Opzione 2: Modello Completo

Dopo che `simple` converge bene:
```r
model_type <- "full"
```

**Vantaggi:**
- Random slopes stress (cattura variabilità individuale)
- Correlazione intercepts-slopes
- Più flessibile

**Limitazioni:**
- Più difficile convergenza
- Richiede più iterazioni warmup

---

## 🔧 Parametri MCMC Raccomandati

### Per Modello Simple
```r
n_chains <- 4
n_iter_warmup <- 1000
n_iter_sampling <- 1000
adapt_delta <- 0.90
max_treedepth <- 10
```

### Per Modello Full (se simple funziona)
```r
n_chains <- 4
n_iter_warmup <- 1500
n_iter_sampling <- 1500
adapt_delta <- 0.95
max_treedepth <- 12
```

### Se Ancora Problemi
```r
n_chains <- 4
n_iter_warmup <- 2000
n_iter_sampling <- 2000
adapt_delta <- 0.99
max_treedepth <- 15
```

---

## ⚠️ Problemi Comuni e Soluzioni

### 1. Divergent Transitions

**Sintomo:**
```
Warning: X of Y iterations ended with a divergence.
```

**Soluzioni (in ordine):**
```r
# 1. Aumenta adapt_delta
adapt_delta <- 0.95  # era 0.90
adapt_delta <- 0.99  # se ancora problemi

# 2. Aumenta warmup
n_iter_warmup <- 2000  # era 1000

# 3. Reparametrizza (già fatto con non-centered)
# I modelli forniti usano già non-centered parameterization
```

### 2. Max Treedepth Exceeded

**Sintomo:**
```
Warning: X iterations exceeded the maximum treedepth.
```

**Soluzione:**
```r
max_treedepth <- 12  # era 10
max_treedepth <- 15  # se ancora problemi
```

**NON preoccuparti se:**
- Solo poche iterazioni (<5%)
- Rhat comunque < 1.01
- ESS comunque > 400

### 3. Low ESS (Effective Sample Size)

**Sintomo:**
```
ess_bulk < 100 o ess_tail < 100
```

**Soluzioni:**
```r
# 1. Aumenta sampling iterations
n_iter_sampling <- 2000  # era 1000

# 2. Aumenta thinning (se memoria problema)
n_thin <- 2  # era 1 (prendi 1 su 2)

# 3. Verifica che non ci siano altri warnings
# (divergences, treedepth, Rhat)
```

### 4. Rhat > 1.01

**Sintomo:**
```
Some Rhat values are greater than 1.01
```

**Questo è SERIO! Soluzioni:**
```r
# 1. Aumenta warmup drasticamente
n_iter_warmup <- 3000

# 2. Prova seed diverso
seed <- 12345  # prova 54321, 99999, ecc.

# 3. Controlla se problema è in pochi parametri
diag_summary %>% filter(rhat > 1.01)
# Se solo 1-2 parametri → ok-ish
# Se molti → problema serio
```

### 5. Initialization Failed

**Sintomo:**
```
Rejecting initial value
Error during sampling
```

**Soluzione:**
```r
# Specifica init values migliori
init_function <- function() {
  list(
    alpha = 0,
    beta_c1 = 0,
    beta_c2 = 0,
    beta_disinhibition = 0,
    beta_c1_x_disinhibition = 0,
    sigma_y = sd(stan_data$y),
    sigma_alpha = sd(stan_data$y) / 2,
    mu_disinhibition = 0,
    sigma_disinhibition = 1
  )
}

fit <- mod$sample(
  data = stan_data,
  init = init_function,  # Aggiungi questo
  # ... altri parametri
)
```

---

## 📊 Diagnostici da Controllare

### 1. Durante Fitting

Guarda output:
```
Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling)
```

**Red flags:**
- "Rejecting initial value" ripetuto molte volte
- "divergent transition" ripetuto (>1% iterations)
- Sampling molto lento (>1 min per 100 iter)

### 2. Dopo Fitting

**ESSENZIALI:**
```r
# Rhat: DEVE essere < 1.01 (preferibilmente < 1.005)
max(diag_summary$rhat, na.rm = TRUE)

# ESS: preferibilmente > 400 (minimo 100)
min(c(diag_summary$ess_bulk, diag_summary$ess_tail), na.rm = TRUE)
```

**NICE TO HAVE:**
```r
# Trace plots: catene si mischiano bene?
mcmc_trace(draws, pars = "beta_c1_x_disinhibition")
# Dovrebbero sembrare "hairy caterpillars" sovrapposti

# PPC: posterior predictions coprono dati osservati?
ppc_dens_overlay(y_obs, y_rep)
# Densità simulate (linee azzurre) dovrebbero coprire osservata (scura)
```

---

## 🎯 Workflow Intelligente

### Step 1: Parti Semplice
```r
model_type <- "simple"
n_iter_warmup <- 1000
n_iter_sampling <- 1000
adapt_delta <- 0.90
```

**Se converge (Rhat < 1.01):**
→ Perfetto! Usa questi risultati.

### Step 2: Prova Complesso (opzionale)
```r
model_type <- "full"
n_iter_warmup <- 1500
n_iter_sampling <- 1500
adapt_delta <- 0.95
```

**Se converge:**
→ Ancora meglio! Più flessibile.

**Se NON converge:**
→ Torna a `simple`. Va benissimo!

### Step 3: Confronta (se entrambi convergono)
```r
# Carica entrambi i fit
fit_simple <- readRDS("fit_simple.rds")
fit_full <- readRDS("fit_full.rds")

# Confronta LOO
loo_simple <- loo(fit_simple$draws("log_lik"))
loo_full <- loo(fit_full$draws("log_lik"))

loo_compare(loo_simple, loo_full)
```

**Se differenza < 2 SE:**
→ Usa `simple` (più semplice)

**Se `full` molto meglio:**
→ Usa `full`

---

## 💡 Quando Preoccuparsi

### ✅ OK se:
- Poche divergences (<1% iterations)
- Rhat < 1.01 per parametri chiave
- ESS > 100 (preferibilmente > 400)
- PPC ragionevole

### ⚠️ Dubbia se:
- Rhat 1.01-1.05 per alcuni parametri
- ESS 50-100
- Molti max_treedepth warnings (ma Rhat ok)

### ❌ PROBLEMA se:
- Rhat > 1.05 per molti parametri
- ESS < 50
- >5% divergent transitions
- Initialization failure continua

**Se PROBLEMA:**
1. Prova aumentare warmup a 3000+
2. Prova adapt_delta = 0.99
3. Controlla che dati non abbiano NA o Inf
4. Considera modello ancora più semplice (no latent trait)

---

## 🆘 Ultimo Resort: Modello Minimalista

Se anche `simple` non converge, crea:

```stan
// Solo fixed effects, no latent trait
data {
  int N;
  vector[N] y;
  vector[N] c1;
  vector[N] disinhibition_observed;  // Usa scala osservata
}
parameters {
  real alpha;
  real beta_c1;
  real beta_disinhibition;
  real beta_interaction;
  real<lower=0> sigma;
}
model {
  alpha ~ normal(0, 10);
  beta_c1 ~ normal(0, 5);
  beta_disinhibition ~ normal(0, 3);
  beta_interaction ~ normal(0, 2);
  sigma ~ normal(0, 10);
  
  y ~ normal(alpha + beta_c1 * c1 + 
             beta_disinhibition * disinhibition_observed +
             beta_interaction * c1 .* disinhibition_observed, 
             sigma);
}
```

Questo **dovrebbe sempre** convergere. Se non converge → problema nei dati.

---

## 📞 Messaggi di Aiuto

Se vedi errori non elencati qui, cerca:
1. Messaggio esatto errore
2. Google: "cmdstanr [messaggio errore]"
3. Stan forums: https://discourse.mc-stan.org/

Oppure dimmi l'errore esatto e ti aiuto!

---

## ✅ Checklist Pre-Fitting

- [ ] Dati caricati correttamente (no NA in y, c1, c2, X)
- [ ] N_voice = N_subj × 3 (circa)
- [ ] Outcome standardizzato o scala ragionevole (|y| < 100)
- [ ] Scelto modello (simple per iniziare)
- [ ] Parametri MCMC impostati
- [ ] Tempo disponibile (~30-60 min per fitting)
- [ ] Spazio disco ok (~500 MB per samples)

Buona fortuna! 🚀
