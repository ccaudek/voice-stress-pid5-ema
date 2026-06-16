
Con l'albero completo posso darti un piano ordinato. Il principio è uno solo, da applicare ovunque si riportino **stime di effetti**: intervalli **89% a code uguali** (quantili 5.5/94.5), **pd come indice primario e descrittivo**, **nessuna soglia/classificazione** ("Strong/Moderate", "CrI esclude lo zero", PD>.95), **niente Bayes factor né ROPE**. Fanno eccezione le quantità che *non* sono inferenza sugli effetti (affidabilità, assurance Monte Carlo, descrittive): lì il 95% è difendibile, decidi tu.

## Già fatto
- `02_statistical_analyses/02_F0mean/` (01–06) ✓
- `02_statistical_analyses/03_NNE/` (02–05 + nuovo 06) ✓

## Fase 1 — numeri "core" del manoscritto (priorità massima, legati a R2.11)

1. **Effetti principali F0/NNE** → `02_statistical_analyses/01_stress_effect/` (`01_main_effects_analysis.R`, `02_manuscript_tables_figures.R`, `05_populate_results.R`, `06_diagnostic_report.R`).
   Qui stanno i par. 60–63 (es. F0 stress 3.27 [0.81, 5.71], riportato come mediana ± 1.96·MAD). Da portare a **89% quantili**. Le conclusioni non cambiano (pd ≈ .995), è solo uniformità di metodo. Output: `results/stress/...`.

2. **EMA vs Baseline / "più preciso"** → `02_statistical_analyses/04_followup/` (`01_loo_comparison_f0_pid5_improved.R`, `02_create_loo_figures_improved.R`).
   Qui c'è il claim dei par. 73–75, 98 e abstract ("CrI più stretti / più preciso"; Tab. 4, NA 3.07 [−0.44, 6.55]). Due interventi: intervalli a **89%** *e* riformulazione da "precisione" a "replicazione" (è anche E3/E4/R2.12). Output: `results/followup/...`.

3. **Prior-sensitivity** → `02_statistical_analyses/10_prior_sensitivity_check/04_prior_sensitivity_bundle_only.R`.
   Verifica che gli intervalli siano **89%** e che il testo (`results/prior_sensitivity/prior_sensitivity_text.txt`) sia la versione *direzionale onesta* (non "slight/unchanged"). È la nuova sotto-sezione del SM.

## Fase 2 — analisi di supporto nel manoscritto/SM

4. **Manipulation check EMA** → `02_statistical_analyses/08_revision1_02/` (attivi: `02,03,04,05`; ignora `OLD/`). brms con contrasti adiacenti: intervalli a **89%**, pd descrittiva. Output: `results/manipulation_check/...` (le tabelle `reviewer_ready_*`, `contrast_summary.csv`).

5. **MFCC connected speech** → `02_statistical_analyses/07_revision1_01/` (`02_mfcc_main_effects_analysis.R`, `03_mfcc_omnibus_bayesian.R`). Intervalli per-coefficiente a **89%**, pd descrittiva. L'omnibus (M / p) è una singola statistica riassuntiva: decidi se tenerlo come tale o riformularlo, ma togli qualsiasi soglia. Output: `results/stress/tables/mfcc_*`.

6. **Voice–affect coupling (R1.9)** → `02_statistical_analyses/09_voice_affect_coupling/03_voice_affect_coupling_1.R`. r con CrI + pd → **89%**. Output: `results/coupling/...`.

7. **Within-person temporal covariation** → `02_statistical_analyses/05_temporal_covariation/` (`02_fit_models.R`, `03_analyze_heterogeneity.R`, `04_create_manuscript_materials.R`). La frase del SM "99.7% dei CrI attraversa lo zero" va portata a **89%** e, soprattutto, riformulata come "pendenze individuali stimate con grande imprecisione" (non come test). Output: `results/within_person/...`, `results/temporal/...`.

## Fase 3 — generatori finali e mirror pubblico

8. **Generatori di tabelle/figure del manoscritto** → cartella `brms/` (`12_stress_main_effects_summary.R`, `13_generate_manuscript_tables.R`, `14_generate_manuscript_figures.R`, `10_export_results_for_review.R`, `03c_posterior_probability.R`). **Qui devo chiederti una conferma** (vedi sotto): se sono questi a costruire Tabella 1/2/4 e le figure (`figure1_moderation_coefficients`, `figureS1/S2`), vanno trattati con le stesse regole; se sono superati da `05_create_table1` + `03_create_figures`, si ignorano.

9. **Mirror OSF** → `only_for_me_repo_osf/scripts/` e `osf/scripts/` (00–07) + i relativi `stan/`. Dato che dati e codice diventano pubblici (R2.17), **devono coincidere** con la pipeline corretta (89%, pd, niente soglie/BF). Da sincronizzare **per ultimo**, quando tutto il resto è stabile.

## Lascia com'è (o tua scelta)
- **Assurance** (`01_preprocessing/mde/06_assurance_effect_gt0.R`, 0.92 [0.90, 0.94]): è un CI Monte Carlo su una proporzione, non un CrI sugli effetti → terrei il 95%, etichettandolo bene.
- **Affidabilità** (`01_preprocessing/reliability/…`, Tab. S1/S2): i CI di affidabilità sono convenzionalmente al 95% → difendibile tenerli, con una nota.
- **Descrittive** (`…/05_descriptives_from_stan_bundles_F0_NNE.R`): nessun intervallo sugli effetti → nessuna modifica.

## File-output stantii da cancellare (per non rileggerli per sbaglio)
- `results/NNE/direction_certainty_table.csv`, `results/NNE/theoretical_alignment_table.csv` (vecchi, 95%/classificati)
- `results/f0mean/direction_certainty_table.csv`, `results/f0mean/theoretical_alignment_table.csv`
- `results/diagnostics/convergence_diagnostics_key_params.csv`, `results/diagnostics/supplementary_table_diagnostics.csv` (senza suffisso `_89cri`)
- `results/table1_f0_moderation.csv`, `results/table1_f0_moderation.tex` (il 05 corretto ora scrive `…_89cri.tex`)

## Ordine di riesecuzione consigliato
1. Cancella i file stantii sopra.
2. Per ogni script da modificare, **stesso checklist di F0/NNE**: cerca `0.025|0.975|0.05[^0-9]|0.95`, `.width`, `median_qi/stat_halfeye` (default 95%), `bayes`/`BF`/`bridge`, `rope`, classificazioni → correggi. (Posso farlo io cartella per cartella come abbiamo fatto.)
3. Rilancia in quest'ordine: Fase 1 (1→2→3), poi Fase 2 (4→5→6→7), poi rigenera tabelle/figure (Fase 3.8), infine sincronizza OSF (3.9).
4. Verifica che ogni CSV/figura nuovo abbia il suffisso/data attesi e che i numeri combacino tra loro.
5. **Solo allora** tocca il manoscritto, seguendo la lista di propagazione R2.11 che avevamo (par. 53, 66–68, Tab. 1–2, par. 73–75/98/abstract, sotto-sezione prior-sensitivity nel SM, riconciliazioni −1.42 e MCMC).

## Una cosa da confermarmi (determina se la `brms/` va toccata)
Le **Tabelle 1/2/4 e le figure** del manoscritto sono prodotte da:
(a) `02_statistical_analyses/02_F0mean/05_create_table1…` + `03_create_figures…` (e analoghi NNE), oppure
(b) i generatori in `brms/` (`13_…tables`, `14_…figures`)?

E dove vengono calcolati i **Bayes factor** citati al par. 53 (probabilmente in `brms/03*`)? Quelli vanno individuati e tolti. Se mi dici quale dei due tracciati è "canonico" (e se `brms/` è ancora vivo o archiviato), restringo la Fase 3 a ciò che serve davvero ed evitiamo di correggere codice morto.

Se vuoi, partiamo subito dalla Fase 1.1 (`01_stress_effect`): mandami i quattro script e te li allineo con lo stesso standard.
