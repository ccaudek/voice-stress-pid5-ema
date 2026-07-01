22 giugno 2026

Della lista "cosa resta" si chiude anche E4. Restano i punti che dipendono da te (blocco EMA/compliance, inventario feature, QC registrazioni, dichiarazione AI, audit riferimenti, push OSF) e la rifinitura finale di numeri di sezione e page-ref.


19 giugno 2026

Te lo organizzo come checklist, distinguendo ciò che richiede solo te da ciò che è tecnico. Prima un paio di verifiche rapide sullo stato attuale (sezione "Methodological contribution", running head, titolo):Verificato lo stato attuale. La parte **analitica e di scrittura è essenzialmente completa**: tutte le analisi richieste dai reviewer sono integrate e coerenti nei tre documenti, i valori sono allineati a `manuscript_values.md`, il linguaggio inferenziale è ripulito, gli errori (E6, CrI, MCMC, esclusioni) corretti. Quello che resta è soprattutto **contenuto fattuale che solo tu hai, due-tre decisioni, e la rifinitura finale di riferimenti/numeri**.

**A. Decisioni (tue, bloccanti)**
1. **E4** — la sezione *"Methodological contribution: intensive ambulatory trait assessment"* è **ancora nel manoscritto**. Devi decidere se *rivederla* (riformulandola come "replica tra strategie di misura") o *rimuoverla*, e aggiornare di conseguenza la risposta E4 nella lettera.
2. **R2.16** — dichiarazione sull'uso di AI: sì/no e, se sì, quali strumenti e per cosa.
3. **R2.22** — conferma il titolo definitivo (il running head è **già aggiornato** a "VOCAL STRESS RESPONSES AND PERSONALITY PATHOLOGY", quindi quel sotto-punto è chiuso).

**B. Informazioni che solo tu hai (fattuali)**
4. **R1.2 / R2.9** — stato di validazione del brief PID-5 EMA: validato per uso *momentary* o short-form trasversale adattato a current-state? Poi le reliability (ω_B = .85, ω_W = .72) vanno nel main text.
5. **R1.3** — inventario *completo* delle feature estratte dai vocali sostenuti (oltre a F0/NNE/jitter/shimmer: formanti F1–F3? durate?) e disclosure di cosa non è stato analizzato.
6. **R1.5 / R2.8** — questo è il **blocco EMA che avevi rimandato**: prompt programmati, compliance (mean/median/min/max), missingness, e soprattutto riconciliare "twice-weekly su 2.5 mesi" (~20–22) con la media osservata di 27 (range 12–31).
7. **R2.20** — QC registrazioni: chiarire il fatto (vocali analizzati = laboratorio; smartphone = campione separato) e riportare screening/esclusioni effettivi.
8. **R1.6** — verifica che la mia proposta di riconciliazione esclusioni (soglia ≥50% come regola operativa, careless-screen agganciato, "<5" rimosso) corrisponda a quanto avete realmente applicato.

**C. Task tecnici**
9. **Placeholder nel supplemento (5)**: dettagli di estrazione MFCC (materiale, software, sampling rate, frame/hop, n. coefficienti), N_MFCC / N_OBS, chains × iterations, link OSF.
10. **OSF (R2.17)**: push degli script ancora non caricati (manipulation check, coupling, MFCC, exam-grade) e conferma che tutto sia nel repo.
11. **Riferimenti (R2.18)**: audit APA-7 completo; confermare la voce **Wright et al. (2025)** nella reference list; aggiungere Ringwald et al. e Kaurin et al. (R2.1) e Molenaar (2004)/Schimmack (2003) (R1.8).
12. **Numerazione cross-ref**: una volta reso il supplemento, sostituire ovunque (manoscritto, supplemento, lettera) i segnaposto S[X] / Table S[Z] / Figure S[Y] con i numeri effettivi — la nuova sezione *Voice–Affect Coupling* e *Connected-Speech Spectral Features* vanno numerate e i rimandi (R1.9, R1.4, R1.7) aggiornati.

**D. Per ultimo**
13. **Page/line refs**: riempire tutti i `[PLACEHOLDER: page refs]` nella lettera dopo la re-paginazione finale.

Nessuna **nuova analisi** serve — su questo restiamo d'accordo. Di tutta la lista, gli unici punti che toccano il *contenuto scientifico* sono E4 (decisione) e il blocco EMA (#6); il resto è rifinitura editoriale e bookkeeping. Se vuoi, posso preparare la riformulazione E4 (versione "replica tra strategie di misura") come bozza tracciata, così resta solo da decidere se inserirla o tagliare.



------

La tua idea è sensata, con una raffinatura che secondo me serve alla luce di cosa ha chiesto l'editor.
L'editor (E3/E4) non vuole solo togliere l'over-claim: vuole che la cornice corretta — "replica tra strategie di misura" — sia nel testo principale, perché (a) il confronto EMA-vs-baseline è uno dei tre obiettivi dichiarati e compare nell'abstract, e (b) R2.12/R2.13 vogliono la cautela (intervalli più stretti = rimozione di varianza, non vantaggio; varianza di metodo condivisa). Se l'interpretazione finisse solo nel supplemento, un Results + Table 4 senza la lettura riformulata nel testo risulterebbe sbilanciato.
Quindi la mia proposta: rimuovo l'intestazione/sezione "Methodological contribution" (è proprio l'etichetta che l'editor contesta) e la sostituisco con un paragrafo breve (2 frasi) riformulato come "replica" che chiude i punti dell'editor/R2.12/R2.13 e rimanda al supplemento per il dettaglio; il dettaglio metodologico (decomposizione della varianza, LOO, direzioni future) va nel supplemento come nuova sezione. Risparmia parole e mette ogni cosa dove i reviewer la cercheranno.


------

La scelta più sicura è **non presentarlo come un brief PID-5 già validato per EMA/momentary use**. Nel manoscritto, infatti, la misura è descritta come una selezione di **3 item per dominio**, scelti in base a loading/rappresentatività da lavori precedenti, poi **riformulati al presente/stato corrente** (“At this moment…”), mentre il PID-5 originale misura disposizioni generali (“I worry…”).  Anche la response letter segnala ancora questo punto come placeholder: bisogna dichiarare esplicitamente se sia una misura EMA validata oppure una short form trasversale adattata. 

Quindi io suggerirei questa posizione:

**“We used an EMA-adapted brief PID-5 item set, not a previously validated momentary PID-5 instrument.”**

Questo è più difendibile perché il PID-5 ufficiale, sia full sia brief, è descritto come misura self-report di tratti/personality trait domains: il full PID-5 è a 220 item e il PID-5-BF ufficiale è a 25 item, 5 item per dominio, da completare come descrizione generale di sé, non come misura momentary. ([Psychiatry][1]) Nel vostro studio, invece, avete una versione **15-item, 3 item per dominio, current-state wording**: quindi non è semplicemente “il PID-5-BF validato”, ma una **EMA adaptation**.

### Cosa fare nel manoscritto

Cambierei il titolo della sottosezione da **“Brief PID-5 for EMA”** a qualcosa come:

**EMA-adapted brief PID-5 item set**

Poi inserirei un paragrafo molto trasparente:

> To reduce participant burden, we administered an EMA-adapted brief PID-5 item set comprising three items per domain. These items were selected from prior cross-sectional validation work on the basis of factor loadings and domain representativeness, and were reworded to refer to participants’ current state. Thus, the measure should not be understood as a previously validated momentary PID-5 instrument; rather, it is a short trait-based PID-5 item set adapted for repeated current-state assessment. We therefore evaluated its psychometric performance in the present EMA data by reporting multilevel reliability, within-person variability, and convergence with the full baseline PID-5.

Subito dopo metterei i numeri già disponibili:

> Multilevel reliability was adequate to good, with high between-person reliability (ω_B = .85), supporting the use of EMA-derived person-level latent traits as moderators, and adequate within-person reliability (ω_W = .72), indicating that the items captured meaningful occasion-level variation.

Qui è importante precisare che **il costrutto usato nei modelli principali è person-level/trait-like**, stimato da osservazioni EMA ripetute, non una vera analisi state-level dei PID-5 momentary states. Questo riduce il rischio che il reviewer dica: “Avete validato male uno state PID-5”. La vostra risposta diventa: “Non stiamo sostenendo una validazione piena di uno state PID-5; usiamo EMA repeated indicators per stimare con incertezza un standing person-level sui domini PID-5.”

### Cosa fare nella response letter

Per R1.2/R2.9, scriverei qualcosa del genere:

> We agree that a valid short trait form is not automatically a valid momentary measure. We have therefore revised the manuscript to describe the instrument more precisely as an EMA-adapted brief PID-5 item set rather than as a previously validated momentary PID-5 measure. The items were selected from prior cross-sectional validation work for domain representativeness and then reworded to refer to the current moment. To evaluate whether this adapted item set was suitable for the present analyses, we now report multilevel reliability in the main text: between-person reliability was high (ω_B = .85), supporting the use of EMA-derived person-level latent traits as moderators, and within-person reliability was adequate (ω_W = .72), indicating meaningful occasion-level variation. We also report convergence with the full baseline PID-5 and within-person variability in the Supplement. We have toned down any language implying external EMA validation and now frame these analyses as providing study-specific psychometric support for the adapted measure.

### Piccola revisione concettuale utile

Evita frasi come:

> “To establish construct validity of the brief EMA assessment…”

Meglio:

> “To provide psychometric support for the EMA-adapted item set…”

oppure:

> “As study-specific evidence for the EMA-adapted measure…”

“Construct validity” suona troppo forte, perché R2.9 sta proprio chiedendo se la validazione esista già. La soluzione migliore è riconoscere il limite e trasformarlo in un punto di forza metodologico: **abbiamo adattato una misura trait-based per EMA, non la trattiamo come già validata, e ne riportiamo affidabilità multilivello, variabilità entro-persona e convergenza con il PID-5 completo.**

[1]: https://www.psychiatry.org/File%20Library/Psychiatrists/Practice/DSM/APA_DSM5_The-Personality-Inventory-For-DSM-5-Full-Version-Adult.pdf?utm_source=chatgpt.com "The Personality Inventory for DSM-5 (PID-5) Adult This measure can be ..."

----------------

Ilaria: 

questo è il blocco EMA che avevi rimandato: prompt programmati, compliance (mean/median/min/max), missingness, e soprattutto riconciliare "twice-weekly su 2.5 mesi" (~20–22) con la media osservata di 27 (range 12–31).

