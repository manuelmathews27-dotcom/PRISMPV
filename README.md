# PRISM — Pharmacovigilance Real-time Intelligence Signal Monitor

A Shiny dashboard that detects drug safety signals from the FDA Adverse Event Reporting System (FAERS) and measures how early those signals appear relative to official FDA label changes.

**Live app:** https://mmdothim.shinyapps.io/PRISMPV/

> The app is deployed as **PRISMPV** on shinyapps.io (account: `mmdothim`). The legacy URL `signal-to-label` is no longer active.

---

## Table of Contents

1. [What it does](#what-it-does)
2. [Dashboard tabs](#dashboard-tabs)
3. [Signal detection](#signal-detection)
4. [Monitor Your Drug — live query behavior](#monitor-your-drug--live-query-behavior)
5. [Black Box Warning detection](#black-box-warning-bbw-detection)
6. [Reference Cohort charts](#reference-cohort-charts)
7. [Signal assessment record (export)](#signal-assessment-record-export)
8. [Regulatory Timeline Intelligence](#regulatory-timeline-intelligence)
9. [Drug name resolution](#drug-name-resolution)
10. [AE synonym mapping](#ae-synonym-mapping)
11. [Adverse event term selection](#adverse-event-term-selection)
12. [openFDA API key and caching](#openfda-api-key-and-caching)
13. [Setup](#setup)
14. [Tests](#tests)
15. [Data pipeline](#data-pipeline)
16. [Deployment](#deployment)
17. [Project structure](#project-structure)
18. [Drug cohort](#drug-cohort)
19. [Cohort analysis findings](#cohort-analysis-findings)
20. [Data sources](#data-sources)
21. [References](#references)
22. [Code reference](#code-reference)

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
116 terms against openFDA on every deploy.

### Signal criteria (Evans + Rothman)

A signal is flagged when **all four** criteria are met in a given quarter:

| Criterion | Threshold | Rationale |
|-----------|-----------|-----------|
| Report count (a) | ≥ 3 | Minimum sample size |
| PRR | ≥ 2.0 | Disproportionality |
| 95% CI lower bound | > 1.0 | Statistical significance |
| chi-squared | ≥ 4.0 | Independence test |

### Signal classification

Based on the most recent 6 quarters:

- **CONFIRMED** — signal met in 2 or more of the last 6 quarters
- **EMERGING** — signal met in exactly 1 of the last 6 quarters
- **NOT DETECTED** — signal not met in any of the last 6 quarters
- **INSUFFICIENT DATA** — fewer than 10 total reports across all queried quarters, or no single quarter with ≥ 3 reports; PRR is not reliable at this sample size

### Signal duration metrics

**Signal Duration** is months since the signal was first detected in any quarter;
**Current Streak** is consecutive signalling quarters ending at the most recent one.
A long duration with no current streak indicates a fading signal; a short duration
with a long streak, a newly emerging but consistent one.

---

## Monitor Your Drug — live query behavior

The Monitor tab queries 12 quarters of live FAERS data but **excludes the 2 most recent quarters** to account for the ~6-month FAERS reporting lag (reports take time to be submitted, processed, and indexed by openFDA). The effective query window is therefore 10 completed quarters, starting 3 quarters back from today. The UI tip text says "10 quarters" — this matches the 10 quarters actually shown, not the 12 requested.

All four openFDA API calls per quarter are fired **in parallel** using `curl`'s async multi pool (`curl::curl_fetch_multi`), with a pool of up to 12 total connections and 6 per host. This cuts query time from ~4 minutes (sequential) down to roughly 30–60 seconds for a 12-quarter window.

After the query completes, results are displayed as:
- Signal status value box with CONFIRMED / EMERGING / NOT DETECTED / INSUFFICIENT DATA
- Current PRR with 95% CI
- Signal duration (months since first signal) and current streak (consecutive signal quarters)
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
| Result | Signal status, current PRR and ROR with 95% CIs, total reports, quarters meeting criteria, months since first signal |
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

The Monitor tab provides a curated dropdown of 116 MedDRA Preferred Terms selected for regulatory relevance — serious, unexpected, life-threatening, or historically linked to FDA action. Organized by system organ class:

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

Three regression suites. The first two gate `run_pipeline.R` **and** every deploy,
so neither a cohort refresh nor a shipped build can proceed with a failing test.

```bash
Rscript tests/test_prr_formula.R      # PRR, ROR, Rothman CI, Yates chi-squared
Rscript tests/test_resolve_token.R    # generic-name -> canonical ingredient
Rscript tests/test_pt_terms.R         # every curated term is a real MedDRA PT
```

The first two are pure and offline. `test_pt_terms.R` needs network — it asks
openFDA whether each of the 116 curated terms resolves under exact-field matching
— and runs in CI only. It skips itself cleanly (exit 0) when openFDA is
unreachable, so it can never redden a deploy for an unrelated reason.

`test_prr_formula.R` builds known 2×2 configurations, feeds the equivalent
marginals, and asserts the textbook values come back — guarding the cell
reconstruction and the Yates correction.

`test_resolve_token.R` covers `canonical_ingredient_token()`, including the FDA
biologic suffix case, where deleting the hyphen yields a canonical name matching no
FAERS records (see [Drug name resolution](#drug-name-resolution)).

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
is missing. Outputs `data/faers_raw.rds` and `data/provenance.rds`. Runtime ~45–60
minutes for 42 drugs.

### 02_signal_detection.R

Computes PRR, ROR and chi-squared via `compute_prr()`, flags each quarter with
`check_signal()`, finds each pair's first signalling quarter, and joins
`data/label_changes.csv` to derive `lag_days` / `lag_months` / `lag_years` and
`signal_detected_before_change`. Outputs `data/combined.rds`.

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
├── app.R                      # Server logic + shinyApp() entry point
├── R/                         # Sourced automatically by Shiny, in name order,
│   │                          # BEFORE app.R — the numeric prefixes make that
│   │                          # dependency order explicit rather than incidental.
│   ├── 00_utils.R             # Packages, openFDA client, PRR maths, caching,
│   │                          # name resolution. Must load first: 10_ calls
│   │                          # compute_prr() at load time, and 50_ builds the
│   │                          # `ui` object at source time.
│   ├── 10_cohort_data.R       # Reference cohort load, class remap, lookups
│   ├── 20_pt_terms.R          # Curated MedDRA Preferred Terms
│   ├── 30_signal_query.R      # Live query path, BBW + label coverage checks
│   ├── 40_timeline.R          # Regulatory timeline + cohort lag chart
│   └── 50_ui.R                # UI definition
├── scripts/
│   ├── 01_faers_pull.R        # Pull FAERS data from openFDA API
│   ├── 02_signal_detection.R  # Compute PRR, identify first signal quarter
│   └── refresh_cohort.sh      # Quarterly refresh (installed as prism-refresh)
├── tests/
│   ├── test_prr_formula.R     # Regression: PRR, Rothman CI, Yates chi-squared
│   └── test_resolve_token.R   # Regression: canonical ingredient token
├── .github/workflows/
│   └── deploy.yml             # CI: tests -> key injection -> deploy -> smoke test
├── data/
│   ├── label_changes.csv      # Curated: 42 drugs with label change dates and types
│   ├── faers_raw.rds          # Pipeline output: raw counts per drug/AE/quarter
│   ├── combined.rds           # Pipeline output: signals + label change lag
│   ├── provenance.rds         # Pipeline run metadata
│   └── audit_log.csv          # Query audit trail (ICH E2E / GVP IX)
├── deploy/caddy/              # Live Caddy block, pulled from the VPS by auto-sync
├── .env.example               # Template for OPENFDA_API_KEY (.env is gitignored)
├── run_pipeline.R             # Test gate + the three pipeline scripts in order
├── install_packages.R         # One-time dependency installer
└── rsconnect/                 # shinyapps.io deployment config
```

`app.R` holds the server logic and the entry point — 927 lines. UI, the openFDA
client, the synonym engine and the timeline model each live in their own file
under `R/`.

**Load order is a real constraint, not cosmetic.** Shiny sources `R/`
alphabetically before `app.R`, so `R/00_utils.R` attaches every package the app
uses — they cannot live in `app.R`, because `R/50_ui.R` calls `page_navbar()` at
source time and would run first. Getting this wrong parses cleanly and fails at
startup with an HTTP 500.


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

**A negative lag means the label caused the data.** Seroquel's first signal for
`death` is 2007 Q1 against an April 2005 boxed warning — the cohort's minimum lag
at −20.8 months. Once a warning is published, clinicians code for the event and
reporting rises *because* of the label change. This is notoriety bias, not a
detection failure.

**Disproportionality cannot substitute for a controlled comparison.** The 2005
antipsychotic mortality warning came from a meta-analysis of 17 placebo-controlled
trials. FAERS cannot stratify by age or indication, so it could not have reached
that conclusion regardless of what the reporting shows.

**Ambien is the strength outlier** — max PRR 161.4 across 42 signal quarters, with
a 9.3-year signal-to-label lag.

**No signal detected:** Floxin (tendon rupture) and Sonata (somnambulism).
Intermezzo is marginal at one quarter.

Median signal-to-label lag across the cohort is **37.2 months**, with signals
detected for 36 of 42 drugs.

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
