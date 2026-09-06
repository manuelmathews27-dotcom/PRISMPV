# 01_faers_pull.R
# Pull adverse event report counts from openFDA FAERS API
# for each drug in our cohort, by quarter.
#
# openFDA API docs: https://open.fda.gov/apis/drug/event/
# No API key required for low-volume queries (<1000/day).

library(dplyr)
library(lubridate)

source("R/00_utils.R")


# ── Helper: pull quarterly counts for one drug/event pair ─────────────────────
pull_quarterly_counts <- function(drug_name, pt_term, start_year, end_year) {

  quarters <- seq(
    from = as.Date(paste0(start_year, "-01-01")),
    to   = as.Date(paste0(end_year,   "-12-31")),
    by   = "quarter"
  )

  results <- vector("list", length(quarters))

  for (i in seq_along(quarters)) {
    q_start <- format(quarters[i], "%Y%m%d")
    q_end   <- format(quarters[i] + months(3) - days(1), "%Y%m%d")

    count_a <- fetch_total(build_url(drug_name, pt_term,  q_start, q_end))  # drug + event
    count_b <- fetch_total(build_url(drug_name, NULL,     q_start, q_end))  # drug, any event
    count_c <- fetch_total(build_url(NULL,      pt_term,  q_start, q_end))  # event, any drug
    count_d <- fetch_total(build_url(NULL,      NULL,     q_start, q_end))  # all reports

    results[[i]] <- tibble(
      drug    = drug_name,
      pt      = pt_term,
      quarter = quarters[i],
      count_a = count_a,
      count_b = count_b,
      count_c = count_c,
      count_d = count_d
    )

    Sys.sleep(0.25)  # stay within openFDA rate limits
  }

  bind_rows(results)
}


# ── Cohort definition ─────────────────────────────────────────────────────────
# drug: the name as it appears in FAERS reports (usually brand name, uppercase)
# pt  : MedDRA Preferred Term (lowercase, as stored in openFDA)

cohort <- list(
  # ── PPAR-gamma Agonist / TZD (2) ──
  list(drug = "AVANDIA",       pt = "myocardial infarction",         start = 2004, end = 2010),
  list(drug = "ACTOS",         pt = "bladder cancer",                 start = 2004, end = 2013),
  # ── HMG-CoA Reductase Inhibitor (4) ──
  list(drug = "ZOCOR",         pt = "rhabdomyolysis",                 start = 2004, end = 2013),
  list(drug = "LIPITOR",       pt = "diabetes mellitus",              start = 2004, end = 2014),
  list(drug = "CRESTOR",       pt = "diabetes mellitus",              start = 2004, end = 2014),
  list(drug = "PRAVACHOL",     pt = "diabetes mellitus",              start = 2004, end = 2014),
  # ── Fluoroquinolone (4) ──
  list(drug = "CIPRO",         pt = "tendon rupture",                 start = 2004, end = 2010),
  list(drug = "LEVAQUIN",      pt = "tendon rupture",                 start = 2004, end = 2010),
  list(drug = "AVELOX",        pt = "tendon rupture",                 start = 2004, end = 2010),
  list(drug = "FLOXIN",        pt = "tendon rupture",                 start = 2004, end = 2010),
  # ── Atypical Antipsychotic (4) ──
  list(drug = "ABILIFY",       pt = "pathological gambling",          start = 2012, end = 2018),
  list(drug = "SEROQUEL",      pt = "death",                          start = 2004, end = 2007),
  list(drug = "ZYPREXA",       pt = "death",                          start = 2004, end = 2007),
  list(drug = "RISPERDAL",     pt = "death",                          start = 2004, end = 2007),
  # ── COX-2 Selective NSAID (3) ──
  list(drug = "CELEBREX",      pt = "myocardial infarction",          start = 2000, end = 2007),
  list(drug = "VIOXX",         pt = "myocardial infarction",          start = 2000, end = 2005),
  list(drug = "MOBIC",         pt = "myocardial infarction",          start = 2000, end = 2007),
  # ── Proton Pump Inhibitor (4) ──
  list(drug = "NEXIUM",        pt = "clostridium difficile colitis",  start = 2004, end = 2014),
  list(drug = "PRILOSEC",      pt = "clostridium difficile colitis",  start = 2004, end = 2014),
  list(drug = "PREVACID",      pt = "clostridium difficile colitis",  start = 2004, end = 2014),
  list(drug = "PROTONIX",      pt = "clostridium difficile colitis",  start = 2004, end = 2014),
  # ── TNF-alpha Inhibitor (4) ──
  list(drug = "HUMIRA",        pt = "tuberculosis",                   start = 2004, end = 2011),
  list(drug = "ENBREL",        pt = "tuberculosis",                   start = 2004, end = 2011),
  list(drug = "REMICADE",      pt = "lymphoma",                       start = 2004, end = 2008),
  list(drug = "CIMZIA",        pt = "tuberculosis",                   start = 2008, end = 2012),
  # ── Bisphosphonate (4) ──
  list(drug = "FOSAMAX",       pt = "osteonecrosis of jaw",           start = 2004, end = 2008),
  list(drug = "ACTONEL",       pt = "osteonecrosis of jaw",           start = 2004, end = 2008),
  list(drug = "BONIVA",        pt = "osteonecrosis of jaw",           start = 2004, end = 2009),
  list(drug = "RECLAST",       pt = "osteonecrosis of jaw",           start = 2007, end = 2011),
  # ── Factor Xa Inhibitor (2) ──
  list(drug = "XARELTO",       pt = "gastrointestinal haemorrhage",   start = 2011, end = 2015),
  list(drug = "ELIQUIS",       pt = "gastrointestinal haemorrhage",   start = 2013, end = 2016),
  # ── JAK Inhibitor (3) ── FDA required boxed-warning revision 2021-09-01 after
  # the ORAL Surveillance RCT. Trial-driven, not FAERS-driven: a useful contrast case.
  list(drug = "XELJANZ",       pt = "myocardial infarction",         start = 2013, end = 2022),
  list(drug = "OLUMIANT",      pt = "myocardial infarction",         start = 2018, end = 2022),
  list(drug = "RINVOQ",        pt = "myocardial infarction",         start = 2019, end = 2022),
  # ── CAR-T Cell Therapy (4) ── class-wide boxed warning for secondary T-cell
  # malignancy required 2024-04-19. Low report volumes are expected here.
  list(drug = "YESCARTA",      pt = "t-cell lymphoma",               start = 2018, end = 2025),
  list(drug = "KYMRIAH",       pt = "t-cell lymphoma",               start = 2018, end = 2025),
  list(drug = "BREYANZI",      pt = "t-cell lymphoma",               start = 2021, end = 2025),
  list(drug = "ABECMA",        pt = "t-cell lymphoma",               start = 2021, end = 2025),
  # ── Sedative-Hypnotic (4) ──
  list(drug = "AMBIEN",        pt = "somnambulism",                   start = 2010, end = 2020),
  list(drug = "LUNESTA",       pt = "somnambulism",                   start = 2010, end = 2020),
  list(drug = "SONATA",        pt = "somnambulism",                   start = 2010, end = 2020),
  list(drug = "INTERMEZZO",    pt = "somnambulism",                   start = 2012, end = 2020)
)


# ── Pull ──────────────────────────────────────────────────────────────────────
message("Pulling FAERS data for ", length(cohort), " drugs (this takes ~45-60 min)...")

faers_raw <- lapply(cohort, function(x) {
  message("  Pulling: ", x$drug, " / ", x$pt)
  pull_quarterly_counts(x$drug, x$pt, x$start, x$end)
})

faers_raw <- bind_rows(faers_raw)

# Never publish a partially fetched cohort. fetch_total() returns NA only after
# a non-transient failure or after exhausting its transient retries; without
# this gate, the pipeline would still save and auto-deploy incomplete counts.
count_cols <- c("count_a", "count_b", "count_c", "count_d")
missing_counts <- vapply(faers_raw[count_cols], function(x) sum(is.na(x)), integer(1))
if (any(missing_counts > 0L)) {
  stop(
    "FAERS pull incomplete; refusing to save data with missing counts: ",
    paste(names(missing_counts), missing_counts, sep = "=", collapse = ", ")
  )
}

saveRDS(faers_raw, "data/faers_raw.rds")

# Save pipeline provenance metadata
provenance <- list(
  pipeline_run_utc   = format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
  r_version          = paste0(R.version$major, ".", R.version$minor),
  platform           = R.version$platform,
  faers_date_range   = c(
    earliest = as.character(min(faers_raw$quarter, na.rm = TRUE)),
    latest   = as.character(max(faers_raw$quarter, na.rm = TRUE))
  ),
  drugs_queried      = unique(faers_raw$drug),
  n_records          = nrow(faers_raw),
  api_source         = "https://api.fda.gov/drug/event.json"
)
saveRDS(provenance, "data/provenance.rds")
message("Done. Saved to data/faers_raw.rds and data/provenance.rds")
