# PRISM — Pharmacovigilance Real-time Intelligence Signal Monitor

A Shiny dashboard that detects drug safety signals from the FDA Adverse Event Reporting System (FAERS) and measures how early those signals appear relative to official FDA label changes.

**Live app:** https://mmdothim.shinyapps.io/PRISMPV/

> The app is deployed as **PRISMPV** on shinyapps.io (account: `mmdothim`). The legacy URL `signal-to-label` is no longer active.

---

## Table of Contents

1. [What it does](#what-it-does)
2. [Dashboard tabs](#dashboard-tabs)
3. [Signal detection](#signal-detection)
4. [Negative control arm (specificity)](#negative-control-arm-specificity)
5. [Monitor Your Drug — live query behavior](#monitor-your-drug--live-query-behavior)
6. [Black Box Warning detection](#black-box-warning-bbw-detection)
7. [Reference Cohort charts](#reference-cohort-charts)
8. [Signal assessment record (export)](#signal-assessment-record-export)
9. [Regulatory Timeline Intelligence](#regulatory-timeline-intelligence)
10. [Drug name resolution](#drug-name-resolution)
11. [AE synonym mapping](#ae-synonym-mapping)
12. [Adverse event term selection](#adverse-event-term-selection)
13. [openFDA API key and caching](#openfda-api-key-and-caching)
14. [Setup](#setup)
15. [Tests](#tests)
16. [Data pipeline](#data-pipeline)
17. [Deployment](#deployment)
18. [Project structure](#project-structure)
19. [Drug cohort](#drug-cohort)
20. [Cohort analysis findings](#cohort-analysis-findings)
21. [Data sources](#data-sources)
22. [References](#references)
23. [Code reference](#code-reference)

---

## What it does

PRISM uses the Proportional Reporting Ratio (PRR) with Evans criteria to identify statistically disproportionate drug-adverse event pairs in FAERS data, reporting the Reporting Odds Ratio (ROR) alongside it as a cross-check. It can query any drug sold in the US, not just those in the reference cohort. For drugs that map to one of the 12 mechanistic classes in the curated 42-drug cohort, PRISM also provides historical timeline comparisons showing how the current signal compares to past FDA actions on similar drugs.

---

## Dashboard tabs

| Tab | Description |
|-----|-------------|
| **Monitor Your Drug** | Live openFDA query for any drug + adverse event: signal status, PRR and ROR with CIs, BBW detection, cohort benchmark (when applicable), and a downloadable assessment record |
| **Reference Cohort** | Signal-to-label lag across all 42 cohort drugs, faceted by mechanistic class, with a per-drug quarterly PRR drill-down |
| **Drug Table** | Searchable table of cohort data with data provenance panel |
| **Methodology** | Signal detection math, thresholds, PRR vs ROR vs EBGM/IC, and limitations |

---

## Signal detection

### PRR formula

PRISM queries four counts from the openFDA API per drug-AE-quarter combination:

| Count | Definition |
|-------|-----------|
| **a** | Reports with target drug AND target AE |
| **B** | All reports with target drug (any AE) |
| **C** | All reports with target AE (any drug) |
| **D** | All reports in the quarter |

openFDA returns **marginals, not 2×2 cells**, so `compute_prr()` reconstructs the
true cells before computing anything:

```
c_cell  = C − a          # event in other drugs
cd_cell = D − B          # other-drug total
bd_cell = D − C          # non-event total

PRR   = (a / B) / (c_cell / cd_cell)
SE    = sqrt(1/a − 1/B + 1/c_cell − 1/cd_cell)
95% CI = exp(ln(PRR) ± 1.96 × SE)

chi²  = D × (|a×D − B×C| − D/2)² / (B × cd_cell × C × bd_cell)
```

The chi-squared is a full Pearson statistic with Yates continuity correction, not
the `(a−E)²/E` shortcut. Any quarter in which a required marginal or reconstructed
cell is zero or negative yields `NA` rather than an estimated value. The 95% CI uses
the log-normal approximation for ratio measures (Rothman, 2008).

`tests/test_prr_formula.R` verifies this against known cell configurations and gates
both the pipeline and every deploy.

### Reporting Odds Ratio

PRR compares **proportions**; ROR compares **odds** of the same 2×2 table. ROR needs
the two cells PRR never forms, reconstructed from the marginals alongside the rest:

```
b_cell = count_b − a          # drug, no event
d_cell = cd_cell − c_cell     # other drug, no event

ROR = (a × d_cell) / (b_cell × c_cell)
SE  = sqrt(1/a + 1/b_cell + 1/c_cell + 1/d_cell)
```

Both are reported because the regulators differ: **FDA** screens with PRR (and EBGM
internally), **EMA** uses ROR in EudraVigilance. For a rare event the two converge,
so ROR is a cross-check — a divergence beyond 25% means the event is *not* rare in
the exposed population, which the Monitor tab states explicitly.

**Signal criteria are applied to PRR only.** ROR is displayed and exported, never
thresholded, so the detection rule stays a single documented method.

### Term matching

Queries use `patient.reaction.reactionmeddrapt.exact`, matching the whole Preferred
Term. A quoted phrase alone is still a substring match, so a short PT would absorb
every longer one containing it — `thrombosis` would sweep in `deep vein thrombosis`,
and both are separately curated terms. That inflation does not cancel in the ratio:
it moves PRR in either direction depending on how a drug's case mix within a PT
family compares with the population's.

This requires every curated term to be a real MedDRA PT, since a non-PT returns zero
rather than an error. MedDRA inverts some word orders (`neoplasm malignant`,
`haemorrhage intracranial`) and subdivides others (`stroke` exists only as
`ischaemic stroke` and `haemorrhagic stroke`). `tests/test_pt_terms.R` validates all
127 terms against openFDA on every deploy.

### Signal criteria (Evans + Rothman)

Detection has **two layers**, and both must be satisfied.

**Layer 1 — per quarter.** A quarter crosses when all four are met:

| Criterion | Threshold | Rationale |
|-----------|-----------|-----------|
| Report count (a) | ≥ 3 | Minimum sample size |
| PRR | ≥ 2.0 | Disproportionality |
| 95% CI lower bound | > 1.0 | Statistical significance |
| chi-squared | ≥ 4.0 | Independence test |

**Layer 2 — persistence.** A *signal* requires **2 crossings within any trailing 6 quarters**. One isolated crossing is not a signal.

The second layer is not decoration. A ten-year series is ~40 quarters, so accepting any single crossing gives ~40 chances to cross; the [negative control arm](#negative-control-arm-specificity) measured that rule firing on 17 of 37 pairs with no plausible association, against 10 of 37 under persistence. One rule governs the whole app — live status, signal duration, and cohort lag — implemented once as `first_persistent_index()` in `R/00_utils.R`.

### Signal classification

Applying layer 2 to the most recent 6 quarters:

- **CONFIRMED** — 2 or more of the last 6 quarters crossed (meets the signal rule)
- **EMERGING** — exactly 1 of the last 6 crossed (below the signal rule; a watch state, not a signal)
- **NOT DETECTED** — no crossings in the last 6 quarters
- **INSUFFICIENT DATA** — fewer than 10 total reports across all queried quarters, or no single quarter with ≥ 3 reports; PRR is not reliable at this sample size

### Signal duration metrics

**Signal Duration** is months since the signal began — the first quarter meeting
criteria in 2 of any trailing 6 quarters, the same rule used for the status above
and for the cohort lag;
**Current Streak** is consecutive signalling quarters ending at the most recent one.
A long duration with no current streak indicates a fading signal; a short duration
with a long streak, a newly emerging but consistent one.

---

## Negative control arm (specificity)

Every drug in the reference cohort was selected *because* FDA acted on it, so almost all of them signal. That measures how **fast** the method detects real signals; it says nothing about how **often** it fires when it should not. A method that flagged everything would score identically on the cohort alone.

The negative control arm supplies the missing denominator. Each pair takes a drug already in the cohort, over that drug's own window, and pairs it with an adverse event it has no plausible mechanism for and no label mention — atorvastatin and somnambulism, rivaroxaban and tuberculosis, esomeprazole and tendon rupture. The identical detection rule runs over them with nothing relaxed, so anything that fires is a false positive by construction.

This is the negative control outcomes approach used by OHDSI/OMOP (Ryan et al. 2013; Schuemie et al. 2016).

**Curation rules.** A pair qualifies only if the drug has no mechanistic route to the event and the event is absent from its label. Events that are pharmacologically promiscuous — death, myocardial infarction, GI haemorrhage, rhabdomyolysis, diabetes mellitus — are excluded as control events entirely, because too many drugs have a defensible route to them. The eight events used are ones with mechanism-specific causes: osteonecrosis of jaw, tendon rupture, bladder cancer, pathological gambling, somnambulism, tuberculosis, T-cell lymphoma, and *C. difficile* colitis.

**Scoring.** Pairs with fewer than 3 reports across the whole window are excluded from the rate: the n ≥ 3 criterion means they could never have signalled, and counting them as passes would inflate specificity.

The remainder are scored under the rule PRISM actually uses — **2 crossings within any trailing 6 quarters** — and, for comparison, under the looser alternative of accepting any single crossing. The second column exists because a threshold is only defensible if you can say what the alternative would have cost.

Same pairs, same counts, same criteria; only the rule for collapsing ~40 quarterly verdicts into one signal differs. That difference is not cosmetic. Scoring each quarter independently gives ~40 chances to cross; at a nominal 5% per-quarter error rate the chance of at least one false crossing is 1 − 0.95⁴⁰ ≈ **87%**. The looser rule was always going to behave this way — the arm is what made it measurable, and why it was rejected.

`first_persistent_index()` in `R/00_utils.R` implements the persistence rule and is covered by `tests/test_prr_formula.R`, including a property test asserting it is strictly stricter than the any-crossing alternative.

**Two kinds of failure.** They need separating. A pair firing in a *single* quarter out of forty is multiplicity, exactly as the arithmetic predicts. A pair firing in ten or more is not noise — esomeprazole and osteonecrosis of jaw fires across 15 quarters at PRR 34, because PPIs and bisphosphonates reach the same elderly and oncology populations. That is channelling, it is real disproportionality, and no threshold removes it. It is the concrete reason disproportionality output needs clinical review before it means anything.

**Why 43 pairs and not 6.** With zero failures out of 6, the exact 95% upper bound on the false-positive rate is about 39% — nearly uninformative. At ~40 informative pairs that bound falls to roughly 7%, which is tight enough to state. The arm is sized for the confidence interval, not for the point estimate.

Curation rationale for every pair, including exclusions and their reasons, is in `data/negative_controls.csv`. Results are computed by `scripts/02_signal_detection.R` into `data/negative_controls.rds` and shown on the Methods tab.

**Known limitation.** Negative controls can be misclassified, and one in this set was: ciprofloxacin paired with osteonecrosis of jaw fires hard, because ciprofloxacin is *indicated for* osteomyelitis and is therefore co-reported with jaw necrosis as its treatment. That is confounding by indication (protopathic bias), not a method failure. It is retained in the CSV with the reason recorded and excluded from the primary rate rather than silently deleted. Schuemie et al. make the same point: a negative control set is a sanity check on the threshold, not a validation study.

---

## Monitor Your Drug — live query behavior

The Monitor tab queries 12 quarters of live FAERS data but **excludes the 2 most recent quarters** to account for the ~6-month FAERS reporting lag (reports take time to be submitted, processed, and indexed by openFDA). The effective query window is therefore 10 completed quarters, starting 3 quarters back from today. The UI tip text says "10 quarters" — this matches the 10 quarters actually shown, not the 12 requested.

All four openFDA API calls per quarter are fired **in parallel** using `curl`'s async multi pool (`curl::curl_fetch_multi`), with a pool of up to 12 total connections and 6 per host. This cuts query time from ~4 minutes (sequential) down to roughly 30–60 seconds for a 12-quarter window.

After the query completes, results are displayed as:
- Signal status value box with CONFIRMED / EMERGING / NOT DETECTED / INSUFFICIENT DATA
- Current PRR with 95% CI
- Signal duration (months since the signal began, persistence rule) and current streak (consecutive signal quarters)
- PRR trend chart with signal threshold line
- Regulatory Context panel (cohort benchmark, BBW check, label coverage check)
- Raw Quarterly Data table (collapsible)
- Regulatory Timeline Intelligence card (when applicable — see below)

Historical timeline comparison (cohort benchmark value boxes and dot plot) is only shown for drugs that map to a reference cohort class that has at least 3 drugs with lag data. For non-cohort drugs, PRISM shows signal status, BBW detection, and text-based regulatory context only. The rationale: the N=40 hand-picked drugs with known label changes is not a generalizable baseline for arbitrary drug comparisons.

---

## Black Box Warning (BBW) detection

The Monitor tab checks the FDA Drug Labeling API for Boxed Warnings on any searched drug (not limited to the 42-drug cohort). The query searches both `openfda.brand_name` and `openfda.generic_name` fields, returning up to 5 label results.

If a BBW is found and the queried adverse event (or its synonyms / medical-root equivalents) appears in the warning text, an alert banner is displayed. The app also checks `contraindications`, `warnings_and_precautions`, and `warnings` sections to determine whether the queried AE is already on the label.

---

## Reference Cohort charts

**Primary view — signal-to-label lag.** One row per drug, sorted by lag and anchored
at zero, faceted by mechanistic class. Colour encodes only the sign: a bar right
means the FAERS signal preceded the label change, left means FDA acted first. Dates
appear as a text column rather than geometry, since the x-axis can encode comparable
lag lengths or a calendar, not both. Plot height is computed from the row count so
spacing stays constant as the cohort grows.

The cohort answers a cross-drug question — how early is the signal relative to FDA
action — which is one number per drug, so a single row per drug is the encoding that
fits it.

**Drill-down — quarterly PRR trend.** Collapsed by default, showing the evidence
behind one row:

- **One y-axis**, PRR only, log-scaled. Counts on a secondary axis would need an
  arbitrary scaling factor, making any apparent relationship between the series an
  artefact of that constant. Log scale because PRR spans two orders of magnitude —
  Ambien/somnambulism reaches ~161 against a typical 2–5.
- **Report count is dot size**, so a high PRR on a small dot reads as fragile.
- Points fill dark when they meet all signal criteria, pale when not.

Signal and label-change annotations flip left of their line past 70% of the query
window; 15 of the 42 drugs have a label change late enough that a right-hand box
would run off the panel.

A drug with no computable PRR in any quarter renders an explicit empty state —
`compute_prr()` returns `NA` for degenerate cells, so the filtered frame can be
empty and the downstream `max()`/`range()` calls would otherwise produce `NaN`.

---

## Signal assessment record (export)

`write_audit_log()` has always appended every query to `data/audit_log.csv` —
timestamp, drug, event, status, PRR and CI — for ICH E2E / GVP IX traceability.
Nothing read it back, so the trail existed only on the server.

The Monitor tab now offers **Download assessment record**, which appears once a
query has run. It produces a timestamped CSV containing:

| Block | Contents |
|-------|----------|
| Provenance | Generation time (UTC), drug queried, the canonical ingredient it resolved to, MedDRA PT, data source, query window, quarters analysed |
| Method | Named method and the thresholds, read from the live constants rather than retyped |
| Result | Signal status, current PRR and ROR with 95% CIs, total reports, quarters meeting criteria, months since the signal began |
| Limits | A stated interpretation caveat — no denominator, reporting and notoriety bias, no stratification by indication or age |
| Quarterly data | Every quarter's four counts, PRR, ROR, chi-squared and pass/fail |

Including the full quarterly series is what makes it an audit artefact rather than
a screenshot: the conclusion can be recomputed from the record without rerunning
the app. GVP Module IX expects signal management to be documented and auditable,
and this is the artefact that would be attached to a signal validation form.

Plain CSV by choice — it opens in Excel, it diffs cleanly in version control, and
it carries no formatting that could obscure a value.

---

## Regulatory Timeline Intelligence

When a signal is CONFIRMED or EMERGING and the FDA has not yet taken action (no label change recorded in the cohort for the queried drug), PRISM shows a **Regulatory Timeline Intelligence** card. This card uses historical signal-to-label lag data from the reference cohort to contextualize where the current signal stands:

- **Class-specific** benchmarks are used when the queried drug maps to a cohort class with at least 3 reference drugs carrying lag data. Falls back to all-class data otherwise.
- Reports the **median lag**, **IQR (Q25–Q75)**, and **percentile** of the current signal duration relative to historical lags.
- Classifies the signal position as one of: **EARLY**, **APPROACHING**, **EXPECTED WINDOW**, or **OVERDUE**.

This is contextual, not predictive. FAERS alone cannot predict when FDA will act.

---

## Drug name resolution

`resolve_drug_names()` translates a brand name to its canonical active ingredient
via the openFDA labeling API before querying FAERS — `LIPITOR` → `ATORVASTATIN`,
`OZEMPIC` → `SEMAGLUTIDE`. Combination products are skipped; salt forms, dosage
forms and route words are stripped via `PHARMA_QUALIFIERS`. Falls back to the input
on any API error or ambiguous result.

`build_url()` searches three FAERS fields with OR logic —
`patient.drug.medicinalproduct`, `openfda.brand_name`, `openfda.generic_name` — so a
report matches whether the reporter used the brand or the generic.

**Biologic suffixes.** FDA requires a 4-letter suffix on biologic nonproprietary
names (`tafasitamab-cxix`). `canonical_ingredient_token()` strips it and replaces
remaining non-letters with a space, so a hyphenated combination
(`SACUBITRIL-VALSARTAN`) splits into two words and falls through rather than
collapsing into one invalid token. Deleting the hyphen instead would weld the suffix
onto the stem — a name matching no FAERS records, which surfaces as "no signal"
rather than as a failed lookup:

| Welded | Reports | Correct | Reports |
|---|---|---|---|
| `TAFASITAMABCXIX` | 0 | `TAFASITAMAB` | 1,267 |
| `RETIFANLIMABDLWR` | 0 | `RETIFANLIMAB` | 87 |
| `AXATILIMABCSFR` | 0 | `AXATILIMAB` | 128 |

**Discontinued brands.** `LEVAQUIN` and `COUMADIN` have no current label, so the
labeling API returns 404 and a boxed-warning check against the brand alone would
classify a long-standing warning as an emerging signal. `fetch_label_results()`
retries with the generic name — from the cohort's brand-to-generic map, then
`resolve_drug_names()`. Withdrawn drugs (Vioxx, Avandia) are unrecoverable: openFDA
holds no label for them under any name.

---

## AE synonym mapping

FDA label text often uses different terminology than MedDRA Preferred Terms. Two complementary mechanisms bridge this gap when checking whether a BBW or label section already covers a queried adverse event:

**Curated synonym map (`ae_synonyms`)** — high-precision exact matches:

| MedDRA PT | Also matches |
|-----------|-------------|
| agranulocytosis | neutropenia, granulocytopenia |
| hepatic failure | hepatotoxicity, liver failure, liver injury |
| thrombosis | thromboembolic, blood clot, vascular occlusion |
| somnambulism | sleepwalking, complex sleep behavior |
| osteonecrosis of jaw | jaw necrosis, ONJ |
| gastrointestinal haemorrhage | gi bleeding, gastrointestinal bleeding, hemorrhage |
| clostridium difficile colitis | c. difficile, CDAD |
| acute kidney injury | renal failure, renal impairment, nephrotoxicity |
| neuropathy peripheral | peripheral neuropathy, polyneuropathy, nerve damage |
| spinal cord haematoma | spinal/epidural hematoma, epidural hematoma, paralysis |
| thyroid cancer | thyroid c-cell, c-cell tumor, medullary thyroid carcinoma |

The last three exist because MedDRA and FDA labels disagree on wording. MedDRA
inverts *neuropathy peripheral* and uses British spelling (*haematoma*), while
labels write *peripheral neuropathy* and *spinal/epidural hematoma*. The GLP-1
boxed warning never uses the word "cancer" at all — it says *thyroid C-cell
tumors* / *medullary thyroid carcinoma*, and the latter is not itself a MedDRA PT.

**Medical root map (`medical_root_map`)** — high-recall cross-language matching:

Bridges Latin/Greek medical roots to common English equivalents (e.g., `hepat` → `liver`, `cardi` → `heart`, `thrombo` → `clot`, `gastro` → `stomach/bowel`). This prevents the endless whack-a-mole of adding per-drug synonym patches.

Both maps are applied by `expand_ae_terms()`, which also extracts meaningful individual words from the AE phrase (≥ 4 characters, excluding stop words and generic clinical terms like "syndrome", "disorder", "failure").

---

## Adverse event term selection

The Monitor tab provides a curated dropdown of 127 MedDRA Preferred Terms selected for regulatory relevance — serious, unexpected, life-threatening, or historically linked to FDA action. Organized by system organ class:

Cardiac, Vascular/Thromboembolic, Hepatic, Renal, Neurological, Neuropsychiatric, Respiratory, Gastrointestinal, Musculoskeletal, Skin, Endocrine/Metabolic, Haematological, Immune/Allergic, Infectious, Oncology, Ocular, General.

**Excluded categories:**
- **Common pharmacological effects** (nausea, headache, dizziness) — rarely trigger regulatory action
- **Reproductive/teratogenic outcomes** (teratogenicity, foetal death, congenital anomaly, spontaneous abortion) — poorly suited to FAERS-based detection due to REMS-suppressed exposure, pregnancy registry surveillance, and fragmented MedDRA coding

### Haematological coverage

The list carries both the base cytopenias and their severe variants, because for
oncology, haematology and JAK products the cytopenias **are** the dose-limiting
toxicities and the reason those labels carry monitoring requirements. Ruxolitinib
is the clearest case: thrombocytopenia is its defining risk and drives
platelet-count-based dosing.

| Base term | Reports | Severe variant | Reports |
|-----------|---------|----------------|---------|
| anaemia | 188,339 | aplastic anaemia | 5,234 |
| neutropenia | 133,700 | febrile neutropenia | 65,353 |
| thrombocytopenia | 110,048 | pancytopenia | 55,112 |
| leukopenia | 49,236 | agranulocytosis | 18,250 |

Listing base and variant terms separately is only meaningful under exact-field
matching. With substring matching `anaemia` would absorb `aplastic anaemia` and
`neutropenia` would absorb `febrile neutropenia`, so the two entries would return
overlapping counts.

**Worked example.** `JAKAFI` + `thrombocytopenia`, 2023 Q3 – 2025 Q4: CONFIRMED,
signal in 10 of 10 quarters, PRR 3.57 (95% CI 2.47–5.17), ROR 3.62 (2.48–5.28),
309 reports. PRR and ROR agree to within 1.4%, as expected for an event that is
~0.6% of all FAERS reports. One quarter stands out — 2024 Q4 at PRR 7.04 with 58
reports and χ² 286, roughly double its neighbours. A single-quarter spike in
spontaneous reporting usually reflects a reporting event rather than a change in
underlying risk.

---

## openFDA API key and caching

### API key

PRISM reads `OPENFDA_API_KEY` from the environment. Without one it runs at the
anonymous limit of ~1,000 requests/day per IP; with one, ~120,000. A single live
query issues roughly 50 requests (4 counts × 12 quarters, plus label and
name-resolution lookups), so the anonymous cap is reached after about 20 queries
across all users of the public app. Keys are free and instant at
<https://open.fda.gov/apis/authentication/>.

Locally: `cp .env.example .env`, then run the container with
`--env-file /home/manny/prism/.env`. For the deployed app, add a repo secret named
`OPENFDA_API_KEY`. shinyapps.io supports no secure environment variables
(`rsconnect`'s `envVars=` is Posit Connect only), so the workflow writes the key
into the bundle as a generated `R/zzz_env.R` — acceptable for a rate-limit token
that carries no data access, but not a pattern for a real credential.

The key is appended at fetch time, never in the URL builders, so it cannot enter a
cache key; `redact_key()` strips it from any logged URL.

### Response cache

An in-memory cache sits in front of every openFDA call:

| Data | Expiry | Why |
|------|--------|-----|
| FAERS quarter counts | none | Only closed quarters are ever queried (the window stops 9 months back), so counts cannot change |
| Label lookups, name resolution | 24 h | FDA labeling changes over time |

Two of the four counts per quarter — *event across all drugs* and *all reports* —
are drug-independent, so they are shared by every user's every query. The container
runs a single R process, so one cache serves all sessions until restart. Failed
lookups are never cached, so a transient outage is retried rather than remembered.

The cache is deliberately memory-only: `repo/data/` is tracked by git and watched
by the auto-sync, so on-disk cache files would generate commits and trigger deploys.

---

## Setup

### Prerequisites

- R ≥ 4.1
- Internet access (the app and pipeline both query the openFDA API in real time)

### Install dependencies

```r
source("install_packages.R")
```

Required packages: `curl`, `jsonlite`, `dplyr`, `lubridate`, `ggplot2`, `ggrepel`, `shiny`, `bslib`, `DT`

### Run the data pipeline

```r
source("run_pipeline.R")
```

See [Data pipeline](#data-pipeline) for details. Allow 45–60 minutes for the full cohort pull.

### Launch the dashboard

```r
shiny::runApp()
```

---

## Tests

Four regression suites. The three offline ones gate `run_pipeline.R` **and** every
deploy, so neither a cohort refresh nor a shipped build can proceed with a failing
test. A syntax gate parses every `.R` file before the deploy step runs.

```bash
Rscript tests/test_prr_formula.R         # PRR, ROR, Rothman CI, Yates chi-squared
Rscript tests/test_resolve_token.R       # generic-name -> canonical ingredient
Rscript tests/test_negative_controls.R   # negative control curation rules
Rscript tests/test_pt_terms.R            # every curated term is a real MedDRA PT
```

The first three are pure and offline. `test_pt_terms.R` needs network — it asks
openFDA whether each of the 127 curated terms resolves under exact-field matching
— and runs in CI only. It skips itself cleanly (exit 0) when openFDA is
unreachable, so it can never redden a deploy for an unrelated reason.

`test_prr_formula.R` builds known 2×2 configurations, feeds the equivalent
marginals, and asserts the textbook values come back — guarding the cell
reconstruction and the Yates correction.

`test_resolve_token.R` covers `canonical_ingredient_token()`, including the FDA
biologic suffix case, where deleting the hyphen yields a canonical name matching no
FAERS records (see [Drug name resolution](#drug-name-resolution)).

`test_negative_controls.R` enforces the curation rules for the specificity arm.
Its central assertion is that no negative control may pair a drug with an event
its **own class** was labelled for. That rule exists because an earlier attempt
at a comparison arm used same-class drugs as controls, which cannot work: when
FDA labels a risk class-wide, every member receives the label, so a same-class
control is an uncollected case rather than a control. FDA's Feb 2012 statin
diabetes change named Lescol and Livalo by name. Measurement agreed — 7 of 8
era-matched comparators signalled, Belsomra reaching PRR 36 against Ambien's 38
for the same event. The test also bars promiscuous control events, requires a
written rationale per pair, and fails if the arm shrinks below 20 primary pairs,
where the confidence interval stops being worth reporting.

**Parse gate.** CI runs `parse()` over `app.R`, `run_pipeline.R`, and every file
in `R/`, `scripts/`, and `tests/` before deploying. The R suites source only
`R/00_utils.R` and `R/20_pt_terms.R`, so a syntax fault in `app.R` or the UI
module used to reach shinyapps.io and be caught only by the post-deploy smoke
test — after the bad bundle was already live.

---

## Data pipeline

`run_pipeline.R` executes three scripts in sequence, halting on errors in the first two and treating the third as non-fatal:

```
run_pipeline.R
  ├── tests/                       → regression gate (halts on failure)
  ├── scripts/01_faers_pull.R      → data/faers_raw.rds, data/provenance.rds
  ├── scripts/02_signal_detection.R → data/combined.rds
```

### 01_faers_pull.R

Pulls quarterly FAERS counts for each cohort drug-AE pair — four API calls per
drug/AE/quarter (`count_a` drug+event, `count_b` drug, `count_c` event, `count_d`
all reports), with a 0.25s delay to stay inside openFDA's rate limit. Bounded retry
with backoff on 429/5xx, and a completeness gate that refuses to save if any count
is missing. Outputs `data/faers_raw.rds` and `data/provenance.rds`.

It then pulls the negative control pairs listed in `data/negative_controls.csv`
into `data/faers_negative_controls.rds`, under the same completeness gate. The
pairs are read from the CSV rather than hardcoded, so the curation rationale and
the query list cannot drift apart. Runtime ~45–60 minutes for the 42 cohort drugs,
plus roughly the same again for the 43 control pairs.

### 02_signal_detection.R

Computes PRR, ROR and chi-squared via `compute_prr()`, flags each quarter with
`check_signal()`, finds each pair's first signalling quarter, and joins
`data/label_changes.csv` to derive `lag_days` / `lag_months` / `lag_years` and
`signal_detected_before_change`. Outputs `data/combined.rds`.

It then applies the identical detection rule to the negative control pairs and
writes `data/negative_controls.rds`: per-pair results plus the specificity
summary (informative pairs, false positives, and a Clopper–Pearson upper bound on
the false-positive rate). Negative controls are kept in a **separate** artifact
rather than added to `combined`, because `first_signals` is grouped by
`(drug, pt)` while the label join is on `drug` alone — a second event for a
cohort drug would fan out every row for that drug. The join back to the curation
CSV is asserted, so a key mismatch stops the pipeline instead of silently
shrinking the denominator.

### Data freshness

The reference cohort data in `data/faers_raw.rds` and `data/combined.rds` is **point-in-time** — it reflects FAERS counts as of the pipeline run date. openFDA periodically reprocesses historical FAERS records, which can cause counts for past quarters to drift over time. Re-running `run_pipeline.R` will refresh the cohort data against the current openFDA index.

The **Monitor Your Drug** tab always queries live FAERS data in real time; it is not affected by the pipeline run date.

### Automatic quarterly refresh

A cron job runs `scripts/refresh_cohort.sh` on the 1st of January, April, July and
October at 3:00 AM to re-pull FAERS data, recompute signals and reload the app.

```bash
# Preflight only — verifies every assumption, changes nothing
./scripts/refresh_cohort.sh --check

# Full refresh (~45-60 min)
./scripts/refresh_cohort.sh

# Cron mode (log file only)
./scripts/refresh_cohort.sh --quiet

# Check last refresh log
cat /home/manny/prism/logs/refresh.log
```

The script requires docker, so it must run as a user in the `docker` group.

The script runs the pipeline in `prism-local:latest` via `run_pipeline.R`, so the
regression tests gate the refresh. It then performs a plain `docker restart` with no
image rebuild: `repo/` is bind-mounted into the container, so new data under
`repo/data/` is visible immediately.

Safety behaviour:

- Preflight verifies that docker is reachable, the image exists, the container is
  running, and that `docker inspect` reports the `repo/` bind-mount. Without the
  mount a restart would serve stale data, so the script aborts instead.
- `data/` is snapshotted before the pull and restored on any failure, since a bad
  pull would otherwise be auto-committed and auto-deployed.
- The script holds the auto-sync flock for the duration of the run, so the watcher
  cannot commit a partially written `.rds`. The refreshed data lands as one commit.
  A refresh in progress therefore looks like a stalled watcher; this is expected.
- After the restart it polls for HTTP 200 and `<title>PRISM</title>` rather than
  sleeping for a fixed interval, since a cold container takes 45-60 seconds to load
  its R packages.

The pipeline run date and FAERS date range are displayed in the dashboard footer.

---

## Deployment

Deployment is **automatic**. `.github/workflows/deploy.yml` triggers on every push
to `edward-auto` and deploys to shinyapps.io as **PRISMPV** under account
`mmdothim`. There is no manual ship gate — `master` is not a deploy trigger.

```
push to edward-auto
  ├── Run R regression tests        (gate — deploy stops here on failure)
  ├── Inject openFDA API key        (only when the repo secret is set)
  ├── Deploy to shinyapps.io        (rsconnect)
  └── Smoke test deployed app       (gate — must return 200 + <title>PRISM</title>)
```

Paths excluded from triggering a deploy: `**.md`, `.claude/**`, `deploy/**`.

The workflow declares `concurrency: deploy-shinyapps-prismpv` with
`cancel-in-progress: true`. Without it, overlapping runs collide on the shinyapps
app lock and the later run fails, producing an alternating success/failure pattern
unrelated to the health of the app.

The two gates cover different failures: the tests catch a broken formula or
resolver before it ships, and the smoke test prevents a green run on a deployed app
that does not load.

### Manual deploy

```r
rsconnect::deployApp(appDir = ".", appName = "PRISMPV",
                     account = "mmdothim", server = "shinyapps.io")
```

Always use `appName = "PRISMPV"`; the archived `signal-to-label` slot should not be
redeployed. `data/audit_log.csv` is written at runtime on the server and is not
bundled — the user-facing export is the
[assessment record](#signal-assessment-record-export).

---

## Project structure

```
prism/
├── app.R                  # Server logic + shinyApp() entry point (1,244 lines)
├── R/                     # Auto-sourced by Shiny in name order, BEFORE app.R
│   ├── 00_utils.R         # Packages, openFDA client, PRR/ROR maths, caching
│   ├── 10_cohort_data.R   # Cohort load, negative controls, lookups
│   ├── 20_pt_terms.R      # Curated MedDRA Preferred Terms
│   ├── 30_signal_query.R  # Live query path, BBW + label coverage, synonyms
│   ├── 40_timeline.R      # Regulatory timeline + cohort lag chart
│   └── 50_ui.R            # UI definition
├── scripts/
│   ├── 01_faers_pull.R    # Pull FAERS counts from openFDA
│   ├── 02_signal_detection.R
│   └── refresh_cohort.sh  # Quarterly refresh (installed as prism-refresh)
├── tests/                 # test_prr_formula.R, test_resolve_token.R,
│                          # test_negative_controls.R, test_pt_terms.R
├── .github/workflows/deploy.yml
├── data/                  # label_changes.csv + negative_controls.csv (curated)
│                          # + pipeline .rds output
│                          # + audit_log.csv (ICH E2E / GVP IX trail)
├── deploy/caddy/          # Live Caddy block, pulled from the VPS by auto-sync
├── run_pipeline.R         # Test gate + pipeline scripts in order
├── install_packages.R     # One-time dependency installer
└── .env.example           # Template for OPENFDA_API_KEY (.env is gitignored)
```

**The numeric prefixes are load-bearing.** Shiny sources `R/` alphabetically before
`app.R`, so `00_utils.R` must attach every package the app uses — they cannot live
in `app.R`, because `50_ui.R` calls `page_navbar()` at source time and would run
first. Getting this wrong parses cleanly and fails at startup with an HTTP 500.

---

## Drug cohort

42 drugs, classified by **mechanism** rather than therapeutic area:

| Class | Drugs | Adverse event tracked |
|-------|-------|----------------------|
| Atypical Antipsychotic | Abilify, Risperdal, Seroquel, Zyprexa | Increased mortality in elderly dementia patients; Pathological gambling |
| Bisphosphonate | Actonel, Boniva, Fosamax, Reclast | Osteonecrosis of jaw |
| CAR-T Cell Therapy | Abecma, Breyanzi, Kymriah, Yescarta | T-cell lymphoma |
| Fluoroquinolone | Avelox, Cipro, Floxin, Levaquin | Tendon rupture |
| HMG-CoA Reductase Inhibitor | Crestor, Lipitor, Pravachol, Zocor | Diabetes mellitus; Rhabdomyolysis |
| Nonbenzodiazepine Z-drug | Ambien, Intermezzo, Lunesta, Sonata | Somnambulism |
| Proton Pump Inhibitor | Nexium, Prevacid, Prilosec, Protonix | Clostridium difficile colitis |
| TNF-alpha Inhibitor | Cimzia, Enbrel, Humira, Remicade | Lymphoma; Tuberculosis |
| COX-2 Selective NSAID | Celebrex, Mobic, Vioxx | Myocardial infarction |
| JAK Inhibitor | Olumiant, Rinvoq, Xeljanz | Myocardial infarction |
| Factor Xa Inhibitor | Eliquis, Xarelto | Gastrointestinal haemorrhage |
| PPAR-gamma Agonist (TZD) | Actos, Avandia | Bladder cancer; Myocardial infarction |

Classes are mechanistic rather than therapeutic areas. A therapeutic grouping such
as *Antidiabetic* spans a TZD, an SGLT2 inhibitor and a DPP-4 inhibitor, and
*Antithrombotic* spans Factor Xa inhibitors, a direct thrombin inhibitor and a P2Y12
antiplatelet — so a class-specific signal-to-label estimate would average across
drugs with no shared pharmacology.

As a result, seven classes fall below the timeline model's three-drug minimum and
use the all-drug benchmark instead. This is intended: a class-specific estimate
drawn from an artificial grouping is less useful than no class-specific estimate.

The drug-class lookup (`drug_class_map` in `app.R`) extends beyond the 40 cohort
drugs to cover commonly queried relatives, and is kept in sync with the cohort
classes. It also carries classes with no cohort members yet — **GLP-1 Receptor
Agonist** (Ozempic, Wegovy, Trulicity, Mounjaro, Zepbound), **IL-12/23**,
**IL-23** and **IL-4R-alpha inhibitors**, **orexin receptor antagonists** — which
resolve for display but fall back to the all-drug benchmark.

---

## Cohort analysis findings

Derived from the 42-drug cohort using exact MedDRA PT matching (data current as of
2026-09-05).

**FAERS detects the cytopenia and infection risks it is often assumed to miss.**
All four PPIs signal for *Clostridium difficile* colitis — Protonix in 17 quarters
(max PRR 17.7), Prevacid 12 (11.1), Nexium 10 (7.7), Prilosec 4 (7.8).

**Bisphosphonate ONJ was found in the literature, not in FAERS.** Every first
signal postdates its label change:

| Drug | Label change | First FAERS signal |
|------|--------------|--------------------|
| Fosamax | 2005-11-12 | 2006 Q2 |
| Actonel | 2005-11-12 | 2006 Q3 |
| Reclast | 2009-09-01 | 2010 Q2 |
| Boniva | 2007-05-09 | never signals |

Osteonecrosis of the jaw was identified from dental case series; FAERS reporting
followed the FDA notification rather than preceding it.

**A negative lag means the label caused the data.** Seroquel's signal for `death`
begins 2007 Q2 against an April 2005 boxed warning — the cohort's minimum lag at
−23.8 months. Once a warning is published, clinicians code for the event and
reporting rises *because* of the label change. This is notoriety bias, not a
detection failure.

**Disproportionality cannot substitute for a controlled comparison.** The 2005
antipsychotic mortality warning came from a meta-analysis of 17 placebo-controlled
trials. FAERS cannot stratify by age or indication, so it could not have reached
that conclusion regardless of what the reporting shows.

**Ambien is the strength outlier** — max PRR 161.4 across 42 signal quarters, with
a 9.1-year signal-to-label lag, the cohort maximum.

**No signal detected:** Floxin (tendon rupture) and Sonata (somnambulism).
Intermezzo is marginal at one quarter.

### Signal-to-label lag

Median lag across the cohort is **17.0 months**, with a signal detected for 33 of
42 drugs. A signal begins at the first quarter where criteria were met in 2 of any
trailing 6 quarters — the same rule that marks a live query CONFIRMED, so one
definition governs the whole app.

An earlier version accepted a single isolated crossing and reported 37.2 months
across 36 drugs. That rule was dropped once the [negative control
arm](#negative-control-arm-specificity) measured it: it fires on **17 of 37** pairs
with no plausible association, against **10 of 37** under persistence. Every
single-quarter false positive disappeared under the stricter rule —
Ambien/bladder cancer, Ambien/tendon rupture, Celebrex/bladder cancer,
Cipro/gambling, Fosamax/bladder cancer, Protonix/tendon rupture — which is
textbook multiplicity: ~40 quarters is ~40 chances to cross.

Since a spurious early crossing can only move a lag longer, never shorter, the
37.2-month figure was an overestimate by roughly a factor of two. It is retained
in `combined.rds` as `lag_months_first_crossing` and printed by the pipeline for
comparison, but nothing in the app reads it. The Methods tab shows the
specificity comparison that drove the decision, not the two lag figures.

| | Rejected rule | **Rule in use** |
|---|---|---|
| Definition | any single crossing | 2 of any trailing 6 quarters |
| Median lag | 37.2 months | **17.0 months** |
| Drugs with a signal | 36 / 42 | 33 / 42 |
| Specificity (negative controls) | 54.1% | **73.0%** |
| False-positive rate, 95% upper bound | 63.1% | 44.1% |

### What persistence does not fix

Ten pairs survive the stricter rule, and they are a different problem. Three
unrelated classes all signal for osteonecrosis of jaw — esomeprazole (PRR 34.2
across 15 quarters), atorvastatin (15.8, 14 quarters), zolpidem (14.0, 10
quarters) — alongside adalimumab and tendon rupture (7.4, 7 quarters). None has a
mechanism for the event. All reach the same elderly and oncology populations that
receive bisphosphonates, so the disproportionality is real and reflects
**channelling in the FAERS denominator**, not pharmacology. No threshold removes
it, which is the concrete reason disproportionality output is a screening step
that requires clinical review rather than a conclusion.

With 37 informative pairs the interval is wide (95% upper bound 44.1%), so 73%
should be read as "roughly three in four", not a precise estimate.

---

## Data sources

- **FAERS:** [openFDA Drug Event API](https://open.fda.gov/apis/drug/event/) — optional API key via `OPENFDA_API_KEY`
- **Drug labeling:** [openFDA Drug Labeling API](https://open.fda.gov/apis/drug/label/) — queried in real time for BBW and contraindication checks
- **Label changes:** Manually curated from FDA safety communications, drug safety labeling changes, and published literature (`data/label_changes.csv`)

---

## References

Evans, S.J.W., Waller, P.C., & Davis, S. (2001). Use of proportional reporting ratios (PRRs) for signal generation from spontaneous adverse drug reaction reports. *Pharmacoepidemiology and Drug Safety*, 10(6), 483–486.

Rothman, K.J., Lanes, S., & Sacks, S.T. (2004). The reporting odds ratio and its advantages over the proportional reporting ratio. *Pharmacoepidemiology and Drug Safety*, 13(8), 519–523.

Rothman, K.J. (2008). *Modern Epidemiology* (3rd ed.). Lippincott Williams & Wilkins. (Log-normal CI approximation for ratio measures.)

Ryan, P.B., Schuemie, M.J., Welebob, E., Duke, J., Valentine, S., & Hartzema, A.G. (2013). Defining a reference set to support methodological research in drug safety. *Drug Safety*, 36(S1), S33–S47.

Schuemie, M.J., Ryan, P.B., Hripcsak, G., Madigan, D., & Suchard, M.A. (2016). Measuring signal detection performance: can we trust negative controls and do we need them? *Drug Safety*, 39(11), 1039–1042.

European Medicines Agency. (2012). *Guideline on good pharmacovigilance practices (GVP), Module IX — Signal management*. EMA/827661/2011.

ICH E2E. (2004). *Pharmacovigilance planning*. International Conference on Harmonisation of Technical Requirements for Registration of Pharmaceuticals for Human Use.

---

## Code reference

Shared helpers live in `R/00_utils.R`, documented at the point of definition:
`compute_prr()` (PRR, ROR, CIs, Yates chi-squared from openFDA marginals),
`check_signal()` (Evans + Rothman criteria), `resolve_drug_names()` and
`canonical_ingredient_token()` (brand → active ingredient), `build_url()`,
`fetch_total()`, and the response cache.

Detection thresholds are constants: `SIGNAL_MIN_REPORTS = 3`, `SIGNAL_MIN_PRR = 2`,
`SIGNAL_MIN_CHISQ = 4`.
