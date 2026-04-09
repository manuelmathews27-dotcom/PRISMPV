# 01_faers_pull.R
# Pull adverse event report counts from openFDA FAERS API
# for each drug in our cohort, by quarter.
#
# openFDA API docs: https://open.fda.gov/apis/drug/event/
# No API key required for low-volume queries (<1000/day).

library(dplyr)
library(lubridate)

source("R/utils.R")


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
  # ── Antidiabetic (4) ──
  list(drug = "AVANDIA",       pt = "myocardial infarction",         start = 2004, end = 2010),
  list(drug = "ACTOS",         pt = "bladder cancer",                 start = 2004, end = 2013),
  list(drug = "INVOKANA",      pt = "amputation",                     start = 2014, end = 2018),
  list(drug = "JANUVIA",       pt = "pancreatitis",                   start = 2006, end = 2011),
  # ── Statin (4) ──
  list(drug = "ZOCOR",         pt = "rhabdomyolysis",                 start = 2004, end = 2013),
  list(drug = "LIPITOR",       pt = "diabetes mellitus",              start = 2004, end = 2014),
  list(drug = "CRESTOR",       pt = "diabetes mellitus",              start = 2004, end = 2014),
  list(drug = "PRAVACHOL",     pt = "diabetes mellitus",              start = 2004, end = 2014),
  # ── Fluoroquinolone (2 of 4 — expand to full 40 later) ──
  list(drug = "CIPRO",         pt = "tendon rupture",                 start = 2004, end = 2010),
  list(drug = "LEVAQUIN",      pt = "tendon rupture",                 start = 2004, end = 2010),
  list(drug = "AVELOX",        pt = "tendon rupture",                 start = 2004, end = 2010),
  list(drug = "FLOXIN",        pt = "tendon rupture",                 start = 2004, end = 2010),
  # ── Antipsychotic (4) ──
  list(drug = "ABILIFY",       pt = "pathological gambling",          start = 2012, end = 2018),
  list(drug = "SEROQUEL",      pt = "death",                          start = 2004, end = 2007),
  list(drug = "ZYPREXA",       pt = "death",                          start = 2004, end = 2007),
  list(drug = "RISPERDAL",     pt = "death",                          start = 2004, end = 2007),
  # ── NSAID (4) ──
  list(drug = "CELEBREX",      pt = "myocardial infarction",          start = 2000, end = 2007),
  list(drug = "VIOXX",         pt = "myocardial infarction",          start = 2000, end = 2005),
  list(drug = "VOLTAREN",      pt = "myocardial infarction",          start = 2000, end = 2007),
  list(drug = "MOBIC",         pt = "myocardial infarction",          start = 2000, end = 2007),
  # ── PPI (4) ──
  list(drug = "NEXIUM",        pt = "clostridium difficile colitis",  start = 2004, end = 2014),
  list(drug = "PRILOSEC",      pt = "clostridium difficile colitis",  start = 2004, end = 2014),
  list(drug = "PREVACID",      pt = "clostridium difficile colitis",  start = 2004, end = 2014),
  list(drug = "PROTONIX",      pt = "clostridium difficile colitis",  start = 2004, end = 2014),
  # ── TNF Inhibitor (4) ──
  list(drug = "HUMIRA",        pt = "tuberculosis",                   start = 2004, end = 2011),
  list(drug = "ENBREL",        pt = "tuberculosis",                   start = 2004, end = 2011),
  list(drug = "REMICADE",      pt = "lymphoma",                       start = 2004, end = 2008),
  list(drug = "CIMZIA",        pt = "tuberculosis",                   start = 2008, end = 2012),
  # ── Bisphosphonate (4) ──
  list(drug = "FOSAMAX",       pt = "osteonecrosis of jaw",           start = 2004, end = 2008),
  list(drug = "ACTONEL",       pt = "osteonecrosis of jaw",           start = 2004, end = 2008),
  list(drug = "BONIVA",        pt = "osteonecrosis of jaw",           start = 2004, end = 2009),
  list(drug = "RECLAST",       pt = "osteonecrosis of jaw",           start = 2007, end = 2011),
  # ── Antithrombotic (4) ──
  list(drug = "PLAVIX",        pt = "drug interaction",               start = 2004, end = 2012),
  list(drug = "PRADAXA",       pt = "gastrointestinal haemorrhage",   start = 2010, end = 2014),
  list(drug = "XARELTO",       pt = "gastrointestinal haemorrhage",   start = 2011, end = 2015),
  list(drug = "ELIQUIS",       pt = "gastrointestinal haemorrhage",   start = 2013, end = 2016),
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
