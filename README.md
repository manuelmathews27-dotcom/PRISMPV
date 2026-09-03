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
7. [Regulatory Timeline Intelligence](#regulatory-timeline-intelligence)
8. [Drug name resolution](#drug-name-resolution)
9. [AE synonym mapping](#ae-synonym-mapping)
10. [Adverse event term selection](#adverse-event-term-selection)
11. [openFDA API key and caching](#openfda-api-key-and-caching)
12. [Setup](#setup)
13. [Tests](#tests)
14. [Data pipeline](#data-pipeline)
15. [Deployment](#deployment)
16. [Project structure](#project-structure)
17. [Drug cohort](#drug-cohort)
18. [Cohort analysis findings](#cohort-analysis-findings)
19. [Data sources](#data-sources)
20. [References](#references)
21. [API reference — R/00_utils.R](#api-reference--r00_utilsr)

---

## What it does

PRISM uses the Proportional Reporting Ratio (PRR) with Evans criteria to identify statistically disproportionate drug-adverse event pairs in FAERS data. It can query any drug sold in the US, not just those in the reference cohort. For drugs that map to one of the 12 mechanistic classes in the curated 42-drug cohort, PRISM also provides historical timeline comparisons showing how the current signal compares to past FDA actions on similar drugs.

---

## Dashboard tabs

| Tab | Description |
|-----|-------------|
| **Monitor Your Drug** | Live openFDA query for any drug + adverse event, with signal status, BBW detection, and cohort benchmark (when applicable) |
| **Reference Cohort** | Signal-to-label lag across all 42 cohort drugs, faceted by mechanistic class, with a per-drug quarterly PRR drill-down |
| **Drug Table** | Searchable table of cohort data with data provenance panel |
| **Methodology** | Signal detection math, thresholds, PRR vs EBGM/IC comparison, and limitations |

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

The Monitor tab reports two complementary duration measures:

- **Signal Duration** — months since the signal was first detected in any quarter. Used for regulatory timeline comparison against historical lag data.
- **Current Streak** — consecutive quarters ending at the most recent quarter where signal criteria are met. Returns "None" if the latest quarter does not meet criteria. Indicates signal persistence and stability.

A long duration with no current streak may indicate an intermittent or fading signal. A short duration with a long streak suggests a newly emerging but consistent signal.

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

**Primary view — signal-to-label lag.** One row per drug, sorted by lag and
anchored at zero, faceted by mechanistic class. Colour encodes only the sign of
the lag: a bar to the right means the FAERS signal preceded the label change, a
bar to the left means FDA acted first. Real dates appear as a text column rather
than as geometry, because the x-axis can encode either comparable lag lengths or
a calendar, not both. Plot height is computed from the row count server-side so
row spacing stays constant as the cohort grows.

This replaced a per-drug quarterly PRR line as the landing view. The cohort
answers a cross-drug question — how early is the signal relative to FDA action —
which is one number per drug; a per-drug time series was the wrong encoding for
it, and quarterly PRR on sparse counts is genuinely spiky rather than badly
styled.

**Drill-down — quarterly PRR trend.** Collapsed by default, showing the evidence
behind a single row for the drug selected in the sidebar:

- **One y-axis.** PRR only, log-scaled. The earlier version drew report counts as
  bars against PRR as a line on a secondary axis, joined by an arbitrary scaling
  factor (`sf <- count_max / prr_max`), so the apparent relationship between the
  two series was an artefact of that constant. The log scale matters because PRR
  spans two orders of magnitude across the cohort — Ambien/somnambulism reaches
  ~161 against a typical 2–5, which flattens every other drug on a linear axis.
- **Report count is dot size.** A larger dot is a better-supported estimate, so
  a high PRR on a small dot reads as fragile — which is the honest picture for
  the rare-event products. Yescarta has computable PRR in only 11 of 32 quarters.
- Points are filled dark when they meet all signal criteria, pale when not.

Annotation boxes for the signal and label-change dates flip to the left of their
line past 70% of the query window. Without that, 15 of the 42 drugs clipped the
box off the right edge — anything with a label change late in its pull window
(Ambien at 87%, Yescarta at 81%, the PPIs, statins, Z-drugs, Vioxx, Xeljanz).

A drug with no computable PRR in any quarter renders an explicit empty state
rather than erroring. `compute_prr()` returns `NA` for degenerate cells, so the
filtered frame can be empty; the downstream `max()` and `range()` calls then
produce `-Inf`/`NaN` and the annotation branch throws "missing value where
TRUE/FALSE needed".

---

## Regulatory Timeline Intelligence

When a signal is CONFIRMED or EMERGING and the FDA has not yet taken action (no label change recorded in the cohort for the queried drug), PRISM shows a **Regulatory Timeline Intelligence** card. This card uses historical signal-to-label lag data from the reference cohort to contextualize where the current signal stands:

- **Class-specific** benchmarks are used when the queried drug maps to a cohort class with at least 3 reference drugs carrying lag data. Falls back to all-class data otherwise.
- Reports the **median lag**, **IQR (Q25–Q75)**, and **percentile** of the current signal duration relative to historical lags.
- Classifies the signal position as one of: **EARLY**, **APPROACHING**, **EXPECTED WINDOW**, or **OVERDUE**.

This is contextual, not predictive. FAERS alone cannot predict when FDA will act.

---

## Drug name resolution

`resolve_drug_names()` in `R/00_utils.R` translates a brand name to its canonical active ingredient via the openFDA Drug Labeling API before querying FAERS. For example:

- `LIPITOR` → `ATORVASTATIN`
- `HUMIRA` → `ADALIMUMAB`
- `OZEMPIC` → `SEMAGLUTIDE`

The function skips combination products (names containing `AND`, `;`, `/`, `,`) and
strips pharmaceutical qualifiers (salt forms, dosage-form words, route words) using
the `PHARMA_QUALIFIERS` constant. Falls back to the original input on any API error
or ambiguous result.

### Biologic suffixes

FDA requires a 4-letter suffix on biologic nonproprietary names, such as
`tafasitamab-cxix`. `canonical_ingredient_token()` strips the suffix and replaces
any remaining non-letters with a space rather than deleting them, so a hyphenated
combination such as `SACUBITRIL-VALSARTAN` splits into two words and falls through
instead of collapsing into a single invalid token.

Earlier behaviour deleted the hyphen and produced `TAFASITAMABCXIX`, which matches
no FAERS records. The app reported this as "no signal" rather than as a failed
lookup:

| Canonical produced | FAERS reports | Correct form | Reports |
|---|---|---|---|
| `TAFASITAMABCXIX` | 0 | `TAFASITAMAB` | 1,267 |
| `RETIFANLIMABDLWR` | 0 | `RETIFANLIMAB` | 87 |
| `AXATILIMABCSFR` | 0 | `AXATILIMAB` | 128 |

### Discontinued brands

Brands with no current FDA label, such as `LEVAQUIN` and `COUMADIN`, return HTTP 404
from the labeling API. A boxed-warning check against the raw name therefore found
nothing and classified an existing warning as an emerging signal.
`fetch_label_results()` retries with the generic name, taken from the cohort's
brand-to-generic map and falling back to `resolve_drug_names()`.

Withdrawn drugs such as Vioxx and Avandia cannot be recovered this way, because
openFDA holds no label for them under any name.

`build_url()` then searches FAERS across three fields with OR logic:
- `patient.drug.medicinalproduct` (free-text as reported)
- `patient.drug.openfda.brand_name` (standardized brand name)
- `patient.drug.openfda.generic_name` (standardized generic name)

This catches FAERS reports regardless of whether the reporter used the brand or
generic name.

### Phrase quoting

Multi-word values are wrapped in `%22`. Without the quotes Lucene splits them:
`reactionmeddrapt:TENDON PAIN` parses as `reactionmeddrapt:TENDON` OR a free-text
match on `PAIN`. Single-word terms are unaffected, but roughly 70 of the 112 curated
PT terms contain more than one word:

| Term | Unquoted | Exact phrase |
|---|---|---|
| tendon pain | 3,561,634 | 7,475 |
| hepatic failure | 929,971 | 43,799 |
| acute kidney injury | 901,197 | 150,318 |
| herpes zoster | 225,468 | 60,592 |

Phrase matching is substring-based rather than exact-field, so `cardiac failure`
also matches `cardiac failure congestive`. This groups a PT family together, at the
cost of counts being broader than a strict PT match.

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

The Monitor tab provides a curated dropdown of 112 MedDRA Preferred Terms selected for regulatory relevance — serious, unexpected, life-threatening, or historically linked to FDA action. Organized by system organ class:

Cardiac, Vascular/Thromboembolic, Hepatic, Renal, Neurological, Neuropsychiatric, Respiratory, Gastrointestinal, Musculoskeletal, Skin, Endocrine/Metabolic, Haematological, Immune/Allergic, Infectious, Oncology, Ocular, General.

**Excluded categories:**
- **Common pharmacological effects** (nausea, headache, dizziness) — rarely trigger regulatory action
- **Reproductive/teratogenic outcomes** (teratogenicity, foetal death, congenital anomaly, spontaneous abortion) — poorly suited to FAERS-based detection due to REMS-suppressed exposure, pregnancy registry surveillance, and fragmented MedDRA coding

---

## openFDA API key and caching

### API key

PRISM reads an optional key from the `OPENFDA_API_KEY` environment variable.
Without one it runs at the anonymous limit of roughly 1,000 requests per day per IP;
with one that ceiling is about 120,000.

A single live query issues roughly 50 requests (4 counts × 12 quarters, plus label
and name-resolution lookups), so the anonymous cap is reached after about 20 queries
across all users of the public app.

```bash
cp .env.example .env      # .env is gitignored — never commit a key
# then supply it to the container:
docker run --env-file /home/manny/prism/.env ...
```

Get a free key instantly at <https://open.fda.gov/apis/authentication/>.

For the deployed app, add a repo secret named `OPENFDA_API_KEY`; the workflow
injects it at build time. shinyapps.io supports no secure environment variables
(`rsconnect`'s `envVars=` is Posit Connect only and errors on shinyapps), so the
key is written into the bundle as a generated `R/zzz_env.R`. This is acceptable for
a rate-limit token, which carries no data access and can be regenerated at any time.
It is not a suitable pattern for a real credential.

The key is appended at fetch time, never in the URL builders, so it can never enter
a cache key; `redact_key()` strips it from any logged URL.

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

Two offline regression suites. Both gate `run_pipeline.R` and every deploy, so
neither a cohort refresh nor a shipped build can proceed with a failing test.

```bash
Rscript tests/test_prr_formula.R      # PRR, Rothman CI, Yates chi-squared
Rscript tests/test_resolve_token.R    # generic-name -> canonical ingredient
```

Neither hits the network.

`test_prr_formula.R` builds known 2×2 configurations, feeds the equivalent
marginals, and asserts the textbook values come back — guarding the cell
reconstruction and the Yates correction.

`test_resolve_token.R` covers `canonical_ingredient_token()`, including the FDA
biologic suffix case that previously produced a canonical name matching no FAERS
records (see [Drug name resolution](#drug-name-resolution)).

---

## Data pipeline

`run_pipeline.R` executes three scripts in sequence, halting on errors in the first two and treating the third as non-fatal:

```
run_pipeline.R
  ├── tests/                       → regression gate (halts on failure)
  ├── scripts/01_faers_pull.R      → data/faers_raw.rds, data/provenance.rds
  ├── scripts/02_signal_detection.R → data/combined.rds
  └── scripts/03_visualizations.R   (preview plots, non-fatal if it fails)
```

### 01_faers_pull.R

Pulls quarterly FAERS report counts from the openFDA API for each of the 40 cohort drug-AE pairs. For each drug-AE-quarter combination, four API calls are made (sequentially within a drug, 0.25-second delay between calls to stay within openFDA rate limits):

- `count_a` — drug + event
- `count_b` — drug, any event
- `count_c` — event, any drug
- `count_d` — all reports in the quarter

Outputs:
- `data/faers_raw.rds` — raw counts (one row per drug / AE / quarter)
- `data/provenance.rds` — pipeline run metadata (timestamp, R version, platform, date range, drugs queried, record count)

**Runtime:** approximately 45–60 minutes for 42 drugs. Set `OPENFDA_API_KEY` to avoid the anonymous daily cap — see [openFDA API key and caching](#openfda-api-key-and-caching).

### 02_signal_detection.R

Loads `faers_raw.rds`, computes PRR and chi-squared via `compute_prr()`, applies `check_signal()` to flag each quarter, and identifies the first quarter where each drug-AE pair met signal criteria. Joins with `data/label_changes.csv` to compute:

- `signal_start_date` — first quarter with signal
- `lag_days` / `lag_months` / `lag_years` — time from first signal to label change
- `signal_detected_before_change` — boolean

Prints a summary of median/min/max lag to the console.

Output: `data/combined.rds`

### 03_visualizations.R

Standalone preview plots (same functions used in the Shiny app). Non-fatal if it fails. Renders three charts:

1. `plot_lag_bar()` — signal-to-label lag bar chart, sorted by lag, with median line
2. `plot_prr_trend(drug)` — quarterly PRR line chart for a single drug, with signal-detected and label-change markers
3. `plot_change_type()` — label change type breakdown bar chart

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
app lock and the later run fails, which previously produced an alternating
success/failure pattern unrelated to the health of the app.

The two gates cover different failures: the tests catch a broken formula or
resolver before it ships, and the smoke test prevents a green run on a deployed app
that does not load.

### Manual deploy (rarely needed)

```r
library(rsconnect)
rsconnect::deployApp(
  appDir  = ".",
  appName = "PRISMPV",
  account = "mmdothim",
  server  = "shinyapps.io"
)
```

**Important:** Always use `appName = "PRISMPV"`. The old `signal-to-label`
deployment is archived and should not be redeployed.

The `rsconnect/` directory contains `.dcf` config files for three deployment slots
(`PRISMPV`, `prismrx`, `signal-to-label`). Only `PRISMPV` is the active production
deployment.

### Pre-deployment checklist

- Run `run_pipeline.R` to regenerate `data/faers_raw.rds`, `data/combined.rds`, and `data/provenance.rds` if the cohort or date ranges have changed.
- Verify `data/label_changes.csv` is up to date.
- Confirm `shiny::runApp()` works locally before deploying.
- The `data/audit_log.csv` file is written at runtime on the server and will not be bundled in the deployment.

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
│   ├── 03_visualizations.R    # Standalone preview plots
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

`app.R` was a single 2,162-line file holding UI, server, the API client, the
synonym engine and the timeline model. It is now 927 lines of server logic, with
the rest in `R/`. No behaviour changed in the split.

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

The cohort was previously grouped into 10 therapeutic areas. Two of those groupings
combined unrelated mechanisms: *Antidiabetic* covered a TZD, an SGLT2 inhibitor and a
DPP-4 inhibitor, and *Antithrombotic* combined two Factor Xa inhibitors with a direct
thrombin inhibitor and a P2Y12 antiplatelet. The class-specific signal-to-label
estimate was therefore averaged across drugs with no shared pharmacology.

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

Analysis of the reference cohort revealed several systematic limitations of FAERS-based signal detection:

**Entire-class failures:**
- All 4 PPIs (Nexium, Prilosec, Prevacid, Protonix) show no FAERS signal for C. difficile colitis. The entire class fails to generate disproportionality for this known risk.
- All 4 bisphosphonates (Fosamax, Actonel, Boniva, Reclast) detected signals only after the label change for osteonecrosis of jaw. ONJ was identified from dental case reports in the literature, not from FAERS spontaneous reporting.

**Drugs where FAERS signal detection was not applicable:**
- Seroquel and Zyprexa show no signal for cerebrovascular accident / mortality. The BBW for increased mortality in elderly dementia patients was based on 17 placebo-controlled clinical trials, not spontaneous reports. FAERS cannot stratify by age or indication.
- 6 additional drugs show no FAERS signal: Floxin, Voltaren, Eliquis, Sonata, Intermezzo, and Januvia.

**Outliers:**
- Ambien has an extreme PRR of 57.6 for somnambulism and a 9.3-year signal-to-label lag, making it a significant outlier in both signal strength and regulatory response time.

These findings underscore that FAERS disproportionality analysis has well-defined blind spots: class-wide effects, risks identified through clinical trials or published literature, and AEs with fragmented MedDRA coding.

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

## API reference — R/00_utils.R

All functions below are sourced by both the pipeline scripts and `app.R`.

### Constants

| Constant | Value | Description |
|----------|-------|-------------|
| `SIGNAL_MIN_REPORTS` | `3L` | Minimum report count (count_a) for a signal to be considered |
| `SIGNAL_MIN_PRR` | `2` | Minimum PRR threshold (Evans criteria) |
| `SIGNAL_MIN_CHISQ` | `4` | Minimum chi-squared threshold (Evans criteria) |
| `AUDIT_LOG_PATH` | `"data/audit_log.csv"` | Path where audit log rows are appended |
| `PHARMA_QUALIFIERS` | (vector) | Salt forms, dosage-form words, and route words stripped from resolved generic names |

### `resolve_drug_names(drug_name)`

Translates a brand or generic drug name to its canonical active ingredient by querying the openFDA Drug Labeling API.

**Parameters:**
- `drug_name` — character; brand or generic name (case-insensitive)

**Returns:** character scalar — canonical active ingredient (uppercase), or the original `drug_name` uppercased on failure.

**Behavior:** Skips combination products. Strips `PHARMA_QUALIFIERS` tokens. Falls back to original input on HTTP error, empty result, or ambiguous match.

### `build_url(drug_name, pt_term, q_start, q_end)`

Builds an openFDA FAERS API query URL for a drug-AE-quarter combination.

**Parameters:**
- `drug_name` — character or NULL; if provided, searches three drug name fields with OR logic
- `pt_term` — character or NULL; MedDRA Preferred Term (lowercase, space-separated)
- `q_start` — character; quarter start date in `YYYYMMDD` format
- `q_end` — character; quarter end date in `YYYYMMDD` format

**Returns:** character; full openFDA API URL with `&limit=1` (only the total count is needed).

### `fetch_total(url)`

Fetches the total report count for a single openFDA API URL (synchronous).

**Parameters:**
- `url` — character; openFDA API URL

**Returns:** integer; total count from `meta.results.total`, `0L` on HTTP 404, `NA_integer_` on other errors or parse failures.

### `parse_multi_resp(resp)`

Parses a single response object from `curl::curl_fetch_multi` into a report count.

**Parameters:**
- `resp` — curl response object or NULL

**Returns:** integer; same semantics as `fetch_total()`.

### `compute_prr(df)`

Computes PRR, 95% CI, and Yates-corrected chi-squared from a data frame with
`count_a`, `count_b`, `count_c`, `count_d` columns. Reconstructs the 2×2 cells from
the openFDA marginals first. Rather than flooring the marginals, it applies a
degenerate-cell guard: if any required marginal or derived cell is zero or negative,
PRR, CI and chi-squared are all `NA` for that row.

**Parameters:**
- `df` — data frame with columns `count_a`, `count_b`, `count_c`, `count_d`

**Returns:** the input data frame with additional columns:

| Column | Description |
|--------|-------------|
| `c_cell` | Reconstructed cell: event in other drugs (`C − a`) |
| `cd_cell` | Reconstructed other-drug total (`D − B`) |
| `bd_cell` | Reconstructed non-event total (`D − C`) |
| `PRR` | Proportional Reporting Ratio |
| `PRR_log_se` | Log-scale standard error of PRR |
| `PRR_lo` | 95% CI lower bound (log-normal approximation) |
| `PRR_hi` | 95% CI upper bound |
| `chi_sq` | Pearson chi-squared with Yates continuity correction |

### `check_signal(count_a, PRR, chi_sq, PRR_lo)`

Returns TRUE when all Evans + Rothman criteria are met for a single quarter.

**Parameters:**
- `count_a` — integer; report count
- `PRR` — numeric; Proportional Reporting Ratio
- `chi_sq` — numeric; chi-squared statistic
- `PRR_lo` — numeric or NA; 95% CI lower bound. When NA, the CI gate is skipped.

**Returns:** logical scalar.

### `write_audit_log(...)`

Appends one row to `data/audit_log.csv` for ICH E2E / GVP IX traceability. Creates the file with a header on first write; appends without header on subsequent writes. Errors are caught and logged to the R console (non-fatal).

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `drug` | character | Drug name as entered by user |
| `ae` | character | AE term queried |
| `status` | character | Signal status (CONFIRMED / EMERGING / NOT DETECTED / INSUFFICIENT DATA) |
| `current_prr` | numeric | Most recent quarter PRR |
| `prr_lo` | numeric | Most recent 95% CI lower bound |
| `prr_hi` | numeric | Most recent 95% CI upper bound |
| `n_reports` | integer | Total reports across all queried quarters |
| `quarters_queried` | integer | Number of quarters in the query window |
| `session_id` | character | Session identifier (default `""`) |

**Audit log columns:** `timestamp`, `session_id`, `drug_queried`, `ae_queried`, `signal_status`, `prr`, `prr_ci_lo`, `prr_ci_hi`, `total_reports`, `quarters_queried`
