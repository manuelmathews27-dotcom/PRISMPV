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
6. [Regulatory Timeline Intelligence](#regulatory-timeline-intelligence)
7. [Drug name resolution](#drug-name-resolution)
8. [AE synonym mapping](#ae-synonym-mapping)
9. [Adverse event term selection](#adverse-event-term-selection)
10. [Setup](#setup)
11. [Data pipeline](#data-pipeline)
12. [Deployment](#deployment)
13. [Project structure](#project-structure)
14. [Drug cohort](#drug-cohort)
15. [Cohort analysis findings](#cohort-analysis-findings)
16. [Data sources](#data-sources)
17. [API reference — R/utils.R](#api-reference--rutilsr)

---

## What it does

PRISM uses the Proportional Reporting Ratio (PRR) with Evans criteria to identify statistically disproportionate drug-adverse event pairs in FAERS data. It can query any drug sold in the US, not just those in the reference cohort. For drugs that map to one of the 10 therapeutic classes in the curated 40-drug cohort, PRISM also provides historical timeline comparisons showing how the current signal compares to past FDA actions on similar drugs.

---

## Dashboard tabs

| Tab | Description |
|-----|-------------|
| **Monitor Your Drug** | Live openFDA query for any drug + adverse event, with signal status, BBW detection, and cohort benchmark (when applicable) |
| **Reference Cohort** | Historical analysis of 40 drugs showing signal-to-label lag, filterable by class and signal status |
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

```
PRR  = (a / B) / (C / D)
chi² = (a − E)² / E,  where E = B × C / D
95% CI = exp(ln(PRR) ± 1.96 × SE)
SE   = sqrt(1/a − 1/B + 1/C − 1/D)
```

B, C, and D are obtained from separate openFDA API queries (marginal totals, not inner cells of a 2×2 table). The 95% CI uses the log-normal approximation for ratio measures (Rothman, 2008).

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
- **INSUFFICIENT DATA** — fewer than 10 total reports or no quarter with 3+ reports; PRR not reliable

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

Historical timeline comparison (cohort benchmark value boxes and dot plot) is only shown for drugs that map to one of the 10 reference cohort therapeutic classes. For non-cohort drugs, PRISM shows signal status, BBW detection, and text-based regulatory context only. The rationale: the N=40 hand-picked drugs with known label changes is not a generalizable baseline for arbitrary drug comparisons.

---

## Black Box Warning (BBW) detection

The Monitor tab checks the FDA Drug Labeling API for Boxed Warnings on any searched drug (not limited to the 40-drug cohort). The query searches both `openfda.brand_name` and `openfda.generic_name` fields, returning up to 5 label results.

If a BBW is found and the queried adverse event (or its synonyms / medical-root equivalents) appears in the warning text, an alert banner is displayed. The app also checks `contraindications`, `warnings_and_precautions`, and `warnings` sections to determine whether the queried AE is already on the label.

---

## Regulatory Timeline Intelligence

When a signal is CONFIRMED or EMERGING and the FDA has not yet taken action (no label change recorded in the cohort for the queried drug), PRISM shows a **Regulatory Timeline Intelligence** card. This card uses historical signal-to-label lag data from the reference cohort to contextualize where the current signal stands:

- **Class-specific** benchmarks are used when the queried drug maps to one of the 10 cohort therapeutic classes and at least 3 reference drugs have lag data. Falls back to all-class data otherwise.
- Reports the **median lag**, **IQR (Q25–Q75)**, and **percentile** of the current signal duration relative to historical lags.
- Classifies the signal position as one of: **EARLY**, **APPROACHING**, **EXPECTED WINDOW**, or **OVERDUE**.

This is contextual, not predictive. FAERS alone cannot predict when FDA will act.

---

## Drug name resolution

`resolve_drug_names()` in `R/utils.R` translates a brand name to its canonical active ingredient via the openFDA Drug Labeling API before querying FAERS. For example:

- `LIPITOR` → `ATORVASTATIN`
- `HUMIRA` → `ADALIMUMAB`
- `OZEMPIC` → `SEMAGLUTIDE`

The function skips combination products (names containing `AND`, `;`, `/`, `,`) and strips pharmaceutical qualifiers (salt forms, dosage-form words, route words) from the resolved generic name using the `PHARMA_QUALIFIERS` constant. Falls back to the original input name on any API error or ambiguous result.

`build_url()` then searches FAERS across three fields with OR logic:
- `patient.drug.medicinalproduct` (free-text as reported)
- `patient.drug.openfda.brand_name` (standardized brand name)
- `patient.drug.openfda.generic_name` (standardized generic name)

This catches FAERS reports regardless of whether the reporter used the brand or generic name.

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

**Medical root map (`medical_root_map`)** — high-recall cross-language matching:

Bridges Latin/Greek medical roots to common English equivalents (e.g., `hepat` → `liver`, `cardi` → `heart`, `thrombo` → `clot`, `gastro` → `stomach/bowel`). This prevents the endless whack-a-mole of adding per-drug synonym patches.

Both maps are applied by `expand_ae_terms()`, which also extracts meaningful individual words from the AE phrase (≥ 4 characters, excluding stop words and generic clinical terms like "syndrome", "disorder", "failure").

---

## Adverse event term selection

The Monitor tab provides a curated dropdown of MedDRA Preferred Terms selected for regulatory relevance — serious, unexpected, life-threatening, or historically linked to FDA action. Organized by system organ class:

Cardiac, Vascular/Thromboembolic, Hepatic, Renal, Neurological, Neuropsychiatric, Respiratory, Gastrointestinal, Musculoskeletal, Skin, Endocrine/Metabolic, Haematological, Immune/Allergic, Infectious, Oncology, Ocular, General.

**Excluded categories:**
- **Common pharmacological effects** (nausea, headache, dizziness) — rarely trigger regulatory action
- **Reproductive/teratogenic outcomes** (teratogenicity, foetal death, congenital anomaly, spontaneous abortion) — poorly suited to FAERS-based detection due to REMS-suppressed exposure, pregnancy registry surveillance, and fragmented MedDRA coding

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

## Data pipeline

`run_pipeline.R` executes three scripts in sequence, halting on errors in the first two and treating the third as non-fatal:

```
run_pipeline.R
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

**Runtime:** approximately 45–60 minutes for 40 drugs. No API key is required for low-volume queries (under 1,000/day).

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

---

## Deployment

The app is deployed to shinyapps.io as **PRISMPV** under account `mmdothim`.

### Deploy with rsconnect

```r
library(rsconnect)

# Authenticate first (one-time):
# rsconnect::setAccountInfo(name="mmdothim", token="...", secret="...")

rsconnect::deployApp(
  appDir  = ".",
  appName = "PRISMPV",
  account = "mmdothim",
  server  = "shinyapps.io"
)
```

**Important:** Always use `appName = "PRISMPV"`. The old `signal-to-label` deployment is archived and should not be redeployed.

The `rsconnect/` directory contains `.dcf` config files for three deployment slots (`PRISMPV`, `prismrx`, `signal-to-label`). Only `PRISMPV` is the active production deployment.

### Pre-deployment checklist

- Run `run_pipeline.R` to regenerate `data/faers_raw.rds`, `data/combined.rds`, and `data/provenance.rds` if the cohort or date ranges have changed.
- Verify `data/label_changes.csv` is up to date.
- Confirm `shiny::runApp()` works locally before deploying.
- The `data/audit_log.csv` file is written at runtime on the server and will not be bundled in the deployment.

---

## Project structure

```
signal-to-label/
├── app.R                      # Shiny app (UI + server + all live-query logic)
├── R/
│   └── utils.R                # Shared helpers (openFDA queries, PRR, audit logging)
├── scripts/
│   ├── 01_faers_pull.R        # Pull FAERS data from openFDA API
│   ├── 02_signal_detection.R  # Compute PRR, identify first signal quarter
│   └── 03_visualizations.R   # Standalone preview plots (same as in-app charts)
├── data/
│   ├── label_changes.csv      # Curated: 40 drugs with label change dates and types
│   ├── faers_raw.rds          # Pipeline output: raw counts (one row per drug/AE/quarter)
│   ├── combined.rds           # Pipeline output: signals + label change lag
│   ├── provenance.rds         # Pipeline run metadata
│   ├── audit_log.csv          # Query audit trail (ICH E2E / GVP IX) — written at runtime
│   ├── combined.csv           # CSV export of combined.rds
│   ├── combined_export.csv    # CSV export (alternative format)
│   └── faers_raw_export.csv   # CSV export of faers_raw.rds
├── run_pipeline.R             # Runs the three pipeline scripts in order
├── install_packages.R         # One-time dependency installer
├── inspect.R                  # Ad-hoc data inspection script (not used in production)
├── test_bbw.R                 # Manual test: BBW detection for specific drugs
├── test_check.R               # Manual test: signal check logic
├── test_cipro.R               # Manual test: Cipro FAERS query
├── test_cipro_bbw.R           # Manual test: Cipro BBW detection
├── test_query.R               # Manual test: openFDA URL and query behavior
└── rsconnect/                 # shinyapps.io deployment config
    └── shinyapps.io/
        └── mmdothim/
            ├── PRISMPV.dcf    # Active production deployment
            ├── prismrx.dcf    # Secondary slot
            └── signal-to-label.dcf  # Archived — do not redeploy
```

The `test_*.R` and `inspect.R` scripts are development/debugging utilities and are not sourced by the app or pipeline. They are safe to run manually from RStudio for ad-hoc verification.

---

## Drug cohort

10 therapeutic classes, 4 drugs each (40 total):

| Class | Drugs | Adverse event tracked |
|-------|-------|----------------------|
| Antidiabetic | Avandia, Actos, Invokana, Januvia | MI; bladder cancer; amputation; pancreatitis |
| Statin | Zocor, Lipitor, Crestor, Pravachol | Rhabdomyolysis; diabetes mellitus |
| Fluoroquinolone | Cipro, Levaquin, Avelox, Floxin | Tendon rupture |
| Antipsychotic | Abilify, Seroquel, Zyprexa, Risperdal | Pathological gambling; mortality |
| NSAID | Celebrex, Vioxx, Voltaren, Mobic | Myocardial infarction |
| PPI | Nexium, Prilosec, Prevacid, Protonix | Clostridium difficile colitis |
| TNF Inhibitor | Humira, Enbrel, Remicade, Cimzia | Tuberculosis; lymphoma |
| Bisphosphonate | Fosamax, Actonel, Boniva, Reclast | Osteonecrosis of jaw |
| Antithrombotic | Plavix, Pradaxa, Xarelto, Eliquis | Drug interaction; GI haemorrhage |
| Sedative-Hypnotic | Ambien, Lunesta, Sonata, Intermezzo | Somnambulism |

The drug-class lookup (`drug_class_map` in `app.R`) extends beyond the 40 cohort drugs to cover commonly queried related drugs (e.g., Ozempic, Jardiance, Wegovy for Antidiabetic; Brilinta, Effient for Antithrombotic). When a user queries one of these extended drugs, PRISM matches it to the appropriate class and shows class-level cohort benchmarks.

---

## Cohort analysis findings

Analysis of the 40-drug reference cohort revealed several systematic limitations of FAERS-based signal detection:

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

- **FAERS:** [openFDA Drug Event API](https://open.fda.gov/apis/drug/event/) — no API key required for low-volume queries (under 1,000/day)
- **Drug labeling:** [openFDA Drug Labeling API](https://open.fda.gov/apis/drug/label/) — queried in real time for BBW and contraindication checks
- **Label changes:** Manually curated from FDA safety communications, drug safety labeling changes, and published literature (`data/label_changes.csv`)

---

## API reference — R/utils.R

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

Computes PRR, 95% CI, and chi-squared from a data frame with `count_a`, `count_b`, `count_c`, `count_d` columns. Applies a floor of 1 to B, C, D to avoid division by zero.

**Parameters:**
- `df` — data frame with columns `count_a`, `count_b`, `count_c`, `count_d`

**Returns:** the input data frame with additional columns:

| Column | Description |
|--------|-------------|
| `PRR` | Proportional Reporting Ratio |
| `PRR_log_se` | Log-scale standard error of PRR |
| `PRR_lo` | 95% CI lower bound (log-normal approximation) |
| `PRR_hi` | 95% CI upper bound |
| `E` | Expected count under independence |
| `chi_sq` | Pearson chi-squared statistic |

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
