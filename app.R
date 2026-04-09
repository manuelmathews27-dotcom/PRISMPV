# app.R — PRISM — Pharmacovigilance Real-time Intelligence Signal Monitor
# Tabs:
#   1. Monitor Your Drug  — live FAERS query + signal status + benchmark
#   2. Reference Cohort   — historical 40-drug analysis
#   3. Drug Table         — searchable cohort data
#   4. Methodology        — signal detection math and limitations

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)
library(ggrepel)

# ── Load historical reference data ────────────────────────────────────────────
if (!file.exists("data/combined.rds") || !file.exists("data/faers_raw.rds"))
  stop("Data files missing. Run run_pipeline.R first to generate data/faers_raw.rds and data/combined.rds")
combined  <- readRDS("data/combined.rds")
faers_raw <- readRDS("data/faers_raw.rds")

# Load pipeline provenance (graceful fallback if not yet generated)
provenance <- if (file.exists("data/provenance.rds")) readRDS("data/provenance.rds") else NULL

signals <- compute_prr(faers_raw)

# Pre-compute detection limitation notes for flagged classes
detection_notes <- list(
  Bisphosphonate = list(
    short = "Literature-detected (dental case series)",
    long  = "Literature-detected signal \u2014 ONJ was identified from dental case series,
             not FAERS spontaneous reports. FAERS signal lagged the label change."
  ),
  Antipsychotic = list(
    short = "Trial-based (placebo-controlled RCTs)",
    long  = "Trial-based signal \u2014 BBW was driven by 17 placebo-controlled clinical trials
             showing excess mortality in elderly dementia patients. FAERS cannot stratify
             by age or indication, making this risk structurally undetectable via
             disproportionality analysis."
  ),
  PPI = list(
    short = "No FAERS signal (class-wide failure)",
    long  = "Class-wide detection failure \u2014 no PPI in the cohort generated a FAERS
             disproportionality signal for C. difficile colitis."
  )
)

get_detection_type <- function(tc, ae) {
  if (tc == "Bisphosphonate") "Bisphosphonate"
  else if (tc == "Antipsychotic" && grepl("mortality|death", ae, ignore.case = TRUE)) "Antipsychotic"
  else if (tc == "PPI") "PPI"
  else NULL
}

detection_alert <- function(type) {
  if (is.null(type)) return(NULL)
  tags$div(
    class = "alert alert-warning py-1 px-2 mt-2 mb-0",
    style = "font-size:0.78rem;",
    icon("triangle-exclamation"),
    detection_notes[[type]]$long
  )
}

combined <- combined |>
  rowwise() |>
  mutate(Note = {
    dt <- get_detection_type(therapeutic_class, adverse_event)
    if (is.null(dt)) "" else detection_notes[[dt]]$short
  }) |>
  ungroup()

# Benchmark stats from cohort (drugs where FAERS signal preceded label change)
benchmark_drugs <- combined |> filter(!is.na(lag_months), lag_months > 0)

# Drug-to-class lookup for matching queried drugs to reference cohort classes
# Includes cohort drugs + common related drugs users might query
drug_class_map <- c(
  # Antidiabetic (cohort: Avandia, Actos, Invokana, Januvia)
  "AVANDIA" = "Antidiabetic", "ACTOS" = "Antidiabetic",
  "INVOKANA" = "Antidiabetic", "JANUVIA" = "Antidiabetic",
  "OZEMPIC" = "Antidiabetic", "JARDIANCE" = "Antidiabetic",
  "FARXIGA" = "Antidiabetic", "VICTOZA" = "Antidiabetic", "TRULICITY" = "Antidiabetic",
  "MOUNJARO" = "Antidiabetic", "WEGOVY" = "Antidiabetic", "BYETTA" = "Antidiabetic",
  # Statin (cohort: Zocor, Lipitor, Crestor, Pravachol)
  "ZOCOR" = "Statin", "LIPITOR" = "Statin", "CRESTOR" = "Statin", "PRAVACHOL" = "Statin",
  "LESCOL" = "Statin", "LIVALO" = "Statin", "ALTOPREV" = "Statin",
  # Fluoroquinolone (cohort: Cipro, Levaquin, Avelox, Floxin)
  "CIPRO" = "Fluoroquinolone", "LEVAQUIN" = "Fluoroquinolone",
  "AVELOX" = "Fluoroquinolone", "FLOXIN" = "Fluoroquinolone",
  # Antipsychotic (cohort: Abilify, Seroquel, Zyprexa, Risperdal)
  "ABILIFY" = "Antipsychotic", "SEROQUEL" = "Antipsychotic",
  "ZYPREXA" = "Antipsychotic", "RISPERDAL" = "Antipsychotic",
  "CLOZARIL" = "Antipsychotic", "GEODON" = "Antipsychotic", "LATUDA" = "Antipsychotic",
  # NSAID (cohort: Celebrex, Vioxx, Voltaren, Mobic)
  "CELEBREX" = "NSAID", "VIOXX" = "NSAID", "VOLTAREN" = "NSAID", "MOBIC" = "NSAID",
  "ADVIL" = "NSAID", "ALEVE" = "NSAID", "NAPROSYN" = "NSAID",
  # PPI (cohort: Nexium, Prilosec, Prevacid, Protonix)
  "NEXIUM" = "PPI", "PRILOSEC" = "PPI", "PREVACID" = "PPI", "PROTONIX" = "PPI",
  "DEXILANT" = "PPI", "ACIPHEX" = "PPI",
  # TNF Inhibitor (cohort: Humira, Enbrel, Remicade, Cimzia)
  "HUMIRA" = "TNF Inhibitor", "ENBREL" = "TNF Inhibitor",
  "REMICADE" = "TNF Inhibitor", "CIMZIA" = "TNF Inhibitor",
  "STELARA" = "TNF Inhibitor", "DUPIXENT" = "TNF Inhibitor",
  # Bisphosphonate (cohort: Fosamax, Actonel, Boniva, Reclast)
  "FOSAMAX" = "Bisphosphonate", "ACTONEL" = "Bisphosphonate",
  "BONIVA" = "Bisphosphonate", "RECLAST" = "Bisphosphonate",
  # Antithrombotic (cohort: Plavix, Pradaxa, Xarelto, Eliquis)
  "PLAVIX" = "Antithrombotic", "PRADAXA" = "Antithrombotic",
  "XARELTO" = "Antithrombotic", "ELIQUIS" = "Antithrombotic",
  "BRILINTA" = "Antithrombotic", "EFFIENT" = "Antithrombotic", "SAVAYSA" = "Antithrombotic",
  # Sedative-Hypnotic (cohort: Ambien, Lunesta, Sonata, Intermezzo)
  "AMBIEN" = "Sedative-Hypnotic", "LUNESTA" = "Sedative-Hypnotic",
  "SONATA" = "Sedative-Hypnotic", "INTERMEZZO" = "Sedative-Hypnotic",
  "BELSOMRA" = "Sedative-Hypnotic", "SILENOR" = "Sedative-Hypnotic",
  "ROZEREM" = "Sedative-Hypnotic"
)

drug_choices <- sort(unique(signals$drug))

# ── MedDRA Preferred Terms — curated for regulatory signal detection ──────────
# Criteria: serious, unexpected, life-threatening, or historically led to FDA
# action (BBW, contraindication, withdrawal). Excludes common pharmacological
# effects (nausea, headache, dizziness) that rarely trigger regulatory action.
pt_terms <- sort(c(
  # ── Cardiac ──
  "myocardial infarction", "cardiac arrest", "cardiac failure",
  "ventricular tachycardia", "ventricular fibrillation",
  "electrocardiogram QT prolonged", "torsade de pointes",
  "cardiomyopathy", "myocarditis", "cardiac tamponade", "sudden death",
  # ── Vascular / Thromboembolic ──
  "pulmonary embolism", "deep vein thrombosis", "thrombosis",
  "stroke", "cerebrovascular accident", "haemorrhagic stroke",
  "hypertensive crisis", "shock", "circulatory collapse", "vasculitis",
  # ── Hepatic ──
  "hepatic failure", "drug-induced liver injury", "hepatitis",
  "hepatotoxicity", "hepatic necrosis", "jaundice", "cholestasis",
  # ── Renal ──
  "acute kidney injury", "renal failure", "nephrotic syndrome",
  "tubulointerstitial nephritis", "renal tubular necrosis",
  # ── Neurological ──
  "seizure", "status epilepticus", "peripheral neuropathy",
  "Guillain-Barre syndrome", "progressive multifocal leukoencephalopathy",
  "encephalopathy", "cerebral haemorrhage", "intracranial haemorrhage",
  "demyelination", "encephalitis", "tardive dyskinesia",
  # ── Neuropsychiatric ──
  "suicidal ideation", "suicide attempt", "completed suicide",
  "psychotic disorder", "hallucination",
  "pathological gambling", "somnambulism",
  "serotonin syndrome", "neuroleptic malignant syndrome",
  # ── Respiratory ──
  "interstitial lung disease", "pneumonitis", "pulmonary fibrosis",
  "respiratory failure", "acute respiratory distress syndrome",
  "pulmonary hypertension",
  # ── Gastrointestinal ──
  "gastrointestinal haemorrhage", "gastrointestinal perforation",
  "pancreatitis", "pancreatitis acute",
  "intestinal obstruction", "clostridium difficile colitis",
  # ── Musculoskeletal ──
  "rhabdomyolysis", "tendon rupture", "tendonitis",
  "osteonecrosis of jaw", "osteonecrosis",
  "pathological fracture", "amputation",
  # ── Skin ──
  "Stevens-Johnson syndrome", "toxic epidermal necrolysis",
  "drug reaction with eosinophilia and systemic symptoms",
  "angioedema", "alopecia",
  # ── Endocrine / Metabolic ──
  "diabetic ketoacidosis", "lactic acidosis",
  "hypoglycaemia", "adrenal insufficiency",
  "diabetes mellitus", "thyroid cancer",
  # ── Haematological ──
  "agranulocytosis", "pancytopenia", "aplastic anaemia",
  "thrombotic thrombocytopenic purpura", "haemolytic uraemic syndrome",
  "disseminated intravascular coagulation", "febrile neutropenia",
  # ── Immune / Allergic ──
  "anaphylactic reaction", "anaphylactic shock",
  "cytokine release syndrome", "systemic lupus erythematosus",
  # ── Infectious ──
  "tuberculosis", "sepsis", "septic shock",
  "opportunistic infection",
  # ── Oncology ──
  "bladder cancer", "lymphoma", "hepatocellular carcinoma",
  "malignant neoplasm", "skin cancer",
  # ── Ocular ──
  "blindness", "optic neuritis", "retinal detachment",
  # ── General ──
  "death", "multiple organ dysfunction syndrome",
  "drug interaction", "drug dependence"
))


# ── Shared helpers ───────────────────────────────────────────────────────────
source("R/utils.R")

# Match a drug/AE pair against the reference cohort (checks brand + generic name)
find_cohort_match <- function(drug_upper, ae_lower) {
  tryCatch(
    combined |>
      filter(toupper(drug_name) == drug_upper |
             grepl(drug_upper, toupper(generic_name), fixed = TRUE)) |>
      filter(grepl(ae_lower, tolower(adverse_event), fixed = TRUE) |
             grepl(tolower(adverse_event), ae_lower, fixed = TRUE)),
    error = function(e) data.frame()
  )
}

# Synonym map: MedDRA PT terms → additional words that may appear in FDA label text
ae_synonyms <- list(
  "somnambulism"               = c("sleep-walking", "sleepwalking", "sleep walking", "complex sleep behav"),
  "tendon rupture"             = c("tendinitis", "tendonitis", "tendon disorder"),
  "tendonitis"                 = c("tendinitis", "tendon rupture", "tendon disorder"),
  "myocardial infarction"      = c("heart attack", "cardiovascular event", "myocardial ischemia"),
  "cerebrovascular accident"   = c("stroke", "cerebrovascular event"),
  "rhabdomyolysis"             = c("myopathy", "muscle breakdown"),
  "gastrointestinal haemorrhage" = c("gi bleeding", "gastrointestinal bleeding", "hemorrhage", "bleeding"),
  "haemorrhage"                  = c("hemorrhage", "bleeding", "blood loss"),
  "osteonecrosis of jaw"       = c("jaw necrosis", "onj"),
  "clostridium difficile colitis" = c("c. difficile", "cdad", "c difficile"),
  "pancreatitis"               = c("pancreatic inflammation"),
  "bladder cancer"             = c("urinary bladder neoplasm"),
  "tuberculosis"               = c("tb ", "mycobacterial"),
  "lymphoma"                   = c("lymphoproliferative", "malignancy"),
  "pathological gambling"      = c("compulsive gambling", "impulse control"),
  "amputation"                 = c("limb amputation", "lower limb"),
  "diabetes mellitus"          = c("hyperglycemia", "blood glucose increased"),
  "death"                      = c("mortality", "fatal"),
  "agranulocytosis"            = c("neutropenia", "granulocytopenia"),
  "hepatic failure"            = c("hepatotoxicity", "liver failure", "liver injury", "hepatic injury"),
  "hepatotoxicity"             = c("hepatic failure", "liver failure", "liver injury", "hepatic injury"),
  "thrombosis"                 = c("arterial occlusion", "vascular occlusion", "thrombotic", "thromboembolic", "blood clot"),
  "blindness"                  = c("vision loss", "visual field defect", "permanent vision loss"),
  "gastrointestinal perforation" = c("ischemic colitis", "gastrointestinal adverse", "bowel perforation", "intestinal perforation"),
  "acute kidney injury"          = c("renal failure", "renal impairment", "kidney failure", "nephrotoxicity"),
  "renal failure"                = c("acute kidney injury", "renal impairment", "kidney failure", "nephrotoxicity")
)

# Medical root-to-organ mapping: Latin/Greek roots → common English equivalents
# This lets us match "hepatotoxicity" against BBW text that says "liver",
# "cardiac failure" against text that says "heart", etc.
medical_root_map <- list(
  "hepat"   = c("liver"),
  "liver"   = c("hepat"),
  "cardi"   = c("heart", "myocardi"),
  "heart"   = c("cardi", "myocardi"),
  "myocardi"= c("heart", "cardi"),
  "renal"   = c("kidney"),
  "kidney"  = c("renal", "nephro"),
  "nephro"  = c("kidney", "renal"),
  "pulmon"  = c("lung", "respiratory"),
  "lung"    = c("pulmon", "respiratory"),
  "cerebr"  = c("brain", "stroke"),
  "thrombo" = c("clot", "emboli"),
  "emboli"  = c("clot", "thrombo"),
  "gastro"  = c("stomach", "intestin", "bowel", "gi "),
  "intestin"= c("bowel", "gastro", "colon", "colitis"),
  "colit"   = c("intestin", "bowel", "gastro", "inflammatory bowel"),
  "pancrea" = c("pancrea"),
  "dermat"  = c("skin", "cutaneous", "rash"),
  "skin"    = c("dermat", "cutaneous"),
  "ocular"  = c("eye", "vision", "optic"),
  "eye"     = c("ocular", "vision", "optic"),
  "neur"    = c("nerve", "brain"),
  "myelosup"= c("neutropeni", "leukopeni", "pancytopeni", "bone marrow"),
  "neutropeni" = c("myelosup", "bone marrow", "granulocytopeni"),
  "anemi"   = c("blood", "hemoglobin"),
  "hemorrhag" = c("bleeding", "blood loss", "haemorrhag"),
  "haemorrhag"= c("bleeding", "blood loss", "hemorrhag"),
  "toxic"   = c("toxicity", "damage", "injury"),
  "fibros"  = c("fibrosis", "scarring"),
  "necros"  = c("necrosis", "death of tissue"),
  "immun"   = c("immune", "autoimmun")
)

# Expand an AE term into search terms using three strategies:
# 1. Full AE phrase + curated synonyms (high precision)
# 2. Medical root extraction + cross-language mappings (high recall)
# This prevents the endless whack-a-mole of adding per-drug synonym patches.
expand_ae_terms <- function(ae_lower) {
  terms <- ae_lower  # search for full phrase first
  # Add curated synonyms if available
  syns <- ae_synonyms[[ae_lower]]
  if (!is.null(syns)) terms <- c(terms, syns)
  # Add individual words from the AE phrase (but skip very short/generic words)
  words <- unlist(strsplit(ae_lower, "\\s+"))
  meaningful <- words[nchar(words) >= 4 & !words %in% c("with", "from", "that", "this", "have", "been", "does", "were", "type", "acute", "chronic", "severe", "mild", "moderate", "drug", "induced", "related", "syndrome", "disease", "disorder", "condition", "failure", "injury", "event", "reaction", "symptom", "signs", "interaction", "increased", "decreased", "associated", "reported")]
  terms <- c(terms, meaningful)
  # Add medical root cross-mappings
  ae_and_words <- c(ae_lower, meaningful)
  for (aw in ae_and_words) {
    for (root in names(medical_root_map)) {
      if (grepl(root, aw, fixed = TRUE)) {
        terms <- c(terms, medical_root_map[[root]])
      }
    }
  }
  unique(terms)
}

# Fetch label results from the openFDA Drug Label API for a given drug.
# Returns a list of label result objects, or NULL on failure/empty.
# Uses simplifyVector = FALSE to avoid jsonlite data-frame coercion issues with
# heterogeneous label schemas (the root cause of silent failures for many drugs).
fetch_label_results <- function(drug_name) {
  dn <- URLencode(drug_name, reserved = TRUE)
  url <- paste0(
    "https://api.fda.gov/drug/label.json?search=(openfda.brand_name:",
    dn, "+openfda.generic_name:", dn, ")&limit=5"
  )
  tryCatch({
    h <- curl::new_handle()
    curl::handle_setopt(h, timeout = 10L)
    resp <- curl::curl_fetch_memory(url, handle = h)
    if (resp$status_code != 200) return(NULL)
    body <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
    results <- body$results
    if (!is.list(results) || length(results) == 0) return(NULL)
    results
  }, error = function(e) NULL)
}

# Safely extract first non-empty string from a label field (may be list or character).
# Strips HTML tags and collapses whitespace.
extract_label_text <- function(field) {
  if (is.null(field)) return(NULL)
  txt <- if (is.list(field)) unlist(field) else as.character(field)
  txt <- txt[!is.na(txt) & nchar(txt) > 0]
  if (length(txt) == 0) return(NULL)
  clean <- gsub("<[^>]+>", " ", txt[1])
  trimws(gsub("\\s+", " ", clean))
}

# Check if a drug has a boxed warning via openFDA labeling API
# Returns list(has_bbw = TRUE/FALSE, bbw_text = "cleaned text or NULL")
# Strategy: Request multiple label results (generic drugs may have many entries,
# and only some carry the structured boxed_warning field). Scan all results for
# an explicit boxed_warning. No fallback to general warnings sections.
check_boxed_warning <- function(drug_name) {
  no_bbw <- list(has_bbw = FALSE, bbw_text = NULL)
  results <- fetch_label_results(drug_name)
  if (is.null(results)) return(no_bbw)

  for (r in results) {
    clean <- extract_label_text(r$boxed_warning)
    if (!is.null(clean) && nchar(clean) > 0)
      return(list(has_bbw = TRUE, bbw_text = substr(clean, 1, 4000)))
  }
  no_bbw

  no_bbw
}

# Check if a drug already has labeling (warnings, contraindications, or BBW)
# that mentions the queried AE. Returns TRUE if the AE is already on the label.
check_label_covers_ae <- function(drug_name, ae_term) {
  results <- fetch_label_results(drug_name)
  if (is.null(results)) return(FALSE)

  # Gather text from all safety-relevant sections across ALL returned labels
  all_text <- character(0)
  for (r in results) {
    for (field in c("boxed_warning", "contraindications",
                    "warnings_and_precautions", "warnings_and_cautions", "warnings")) {
      val <- r[[field]]
      if (!is.null(val)) {
        txt <- if (is.list(val)) unlist(val) else as.character(val)
        txt <- txt[!is.na(txt) & nchar(txt) > 0]
        all_text <- c(all_text, txt)
      }
    }
  }
  if (length(all_text) == 0) return(FALSE)
  combined_text <- tolower(paste(gsub("<[^>]+>", " ", all_text), collapse = " "))
  ae_words <- expand_ae_terms(tolower(ae_term))
  if (length(ae_words) == 0) return(FALSE)
  any(sapply(ae_words, function(w) grepl(w, combined_text, fixed = TRUE)))
}

# Pull N quarters of FAERS data for a drug/AE pair
# progress_cb: optional function(pct, detail) called after each API call
pull_live_signal <- function(drug_name, pt_term, n_quarters = 12, progress_cb = NULL) {
  current_q  <- floor_date(Sys.Date(), "quarter")
  quarters   <- seq(
    current_q - months(3 * n_quarters),
    current_q - months(9),   # exclude last 2 quarters (FAERS has ~6 month lag)
    by = "quarter"
  )
  n_total  <- length(quarters)

  if (!is.null(progress_cb)) progress_cb(value = 0.05, detail = "5% — resolving drug names")

  # Resolve to canonical active ingredient so "LIPITOR" → "ATORVASTATIN", "BAYER" → "ASPIRIN" etc.
  canonical <- resolve_drug_names(drug_name)

  if (!is.null(progress_cb)) progress_cb(value = 0.1, detail = "10% — queuing API calls")

  # Build all URLs upfront for parallel fetching — always use canonical ingredient
  urls <- vector("list", n_total)
  for (i in seq_along(quarters)) {
    q_start <- format(quarters[i], "%Y%m%d")
    q_end   <- format(quarters[i] + months(3) - days(1), "%Y%m%d")
    urls[[i]] <- c(
      a = build_url(canonical, pt_term, q_start, q_end),
      b = build_url(canonical, NULL,    q_start, q_end),
      c = build_url(NULL,      pt_term, q_start, q_end),
      d = build_url(NULL,      NULL,    q_start, q_end)
    )
  }

  # Fire all requests in parallel using curl's async multi pool
  pool <- curl::new_pool(total_con = 12, host_con = 6)
  resp_env <- new.env(parent = emptyenv())

  for (i in seq_along(urls)) {
    for (key in names(urls[[i]])) {
      local({
        tag <- paste0(i, "_", key)
        h <- curl::new_handle()
        curl::handle_setopt(h, timeout = 15L, connecttimeout = 10L)
        curl::curl_fetch_multi(
          urls[[i]][[key]],
          handle = h,
          done = function(resp) { resp_env[[tag]] <- resp },
          fail = function(msg)  { resp_env[[tag]] <- NULL },
          pool = pool
        )
      })
    }
  }
  curl::multi_run(pool = pool)

  if (!is.null(progress_cb)) progress_cb(value = 0.9, detail = "90% — parsing results")

  results <- vector("list", n_total)
  for (i in seq_along(quarters)) {
    get_resp <- function(tag) if (exists(tag, envir = resp_env)) resp_env[[tag]] else NULL
    ca <- parse_multi_resp(get_resp(paste0(i, "_a")))
    cb <- pmax(parse_multi_resp(get_resp(paste0(i, "_b"))), 1)
    cc <- pmax(parse_multi_resp(get_resp(paste0(i, "_c"))), 1)
    cd <- pmax(parse_multi_resp(get_resp(paste0(i, "_d"))), 1)
    results[[i]] <- tibble(quarter = quarters[i],
                           count_a = ca, count_b = cb,
                           count_c = cc, count_d = cd)
  }

  if (!is.null(progress_cb)) progress_cb(value = 1.0, detail = "100% — done")
  out <- bind_rows(results) |>
    compute_prr() |>
    mutate(signal_met = check_signal(count_a, PRR, chi_sq, PRR_lo))
  # Attach live query provenance as attributes
  attr(out, "query_time_utc")   <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  attr(out, "faers_api_source") <- "openFDA FAERS"
  attr(out, "query_window")     <- paste0(
    fmt_quarter(min(quarters)), " to ", fmt_quarter(max(quarters)))
  attr(out, "canonical_ingredient") <- canonical
  out
}

# Determine signal status from a data frame of quarterly PRR results
signal_status <- function(df) {
  recent <- tail(df, 6)
  n_sig  <- sum(recent$signal_met, na.rm = TRUE)
  if      (n_sig >= 2) "CONFIRMED"
  else if (n_sig == 1) "EMERGING"
  else                 "NOT DETECTED"
}

# How many consecutive quarters has a signal been active (counting back from
# the most recent quarter)?  Returns 0 if the latest quarter is not a signal.
quarters_active <- function(df) {
  s <- df$signal_met
  s[is.na(s)] <- FALSE
  n <- length(s)
  if (n == 0 || !s[n]) return(0L)
  run <- 0L
  for (i in seq(n, 1L)) {
    if (s[i]) run <- run + 1L else break
  }
  run
}

# Months since the first quarter where signal criteria were met
months_since_first_signal <- function(df) {
  s <- df$signal_met
  s[is.na(s)] <- FALSE
  if (!any(s)) return(0)
  first_q <- df$quarter[which(s)[1]]
  round(as.numeric(difftime(Sys.Date(), first_q, units = "days")) / 30.44)
}

# Format a Date to "YYYY QN" quarter label
fmt_quarter <- function(d) paste0(format(d, "%Y"), " Q", quarter(d))

# ── Regulatory Timeline Intelligence ────────────────────────────────────────
# Predict expected FDA action window based on historical signal-to-label lags.
# Uses class-specific data when available (min 3 drugs), falls back to all drugs.
# Returns a list with: estimate, ci_lo, ci_hi, percentiles, reference drugs used.
predict_timeline <- function(months_active, drug_class = NULL, benchmark_df) {
  if (!is.null(drug_class) && drug_class %in% benchmark_df$therapeutic_class) {
    ref <- benchmark_df |> filter(therapeutic_class == drug_class)
    if (nrow(ref) < 3) ref <- benchmark_df
  } else {
    ref <- benchmark_df
  }
  lags  <- ref$lag_months
  med   <- median(lags)
  q25   <- quantile(lags, 0.25)
  q75   <- quantile(lags, 0.75)
  pct_at <- round(100 * mean(lags <= months_active))

  list(
    ref_class     = if (!is.null(drug_class) && drug_class %in% ref$therapeutic_class)
                      drug_class else "All classes",
    n_ref         = length(lags),
    ref_drugs     = ref$drug_name,
    median_lag    = med,
    q25 = q25, q75 = q75,
    months_active = months_active,
    pct_at        = pct_at,
    risk          = if (months_active >= q75) "OVERDUE"
                    else if (months_active >= med) "EXPECTED WINDOW"
                    else if (months_active >= q25) "APPROACHING"
                    else "EARLY"
  )
}


# ── UI ────────────────────────────────────────────────────────────────────────
prism_logo <- tags$svg(
  xmlns = "http://www.w3.org/2000/svg", viewBox = "0 0 48 40",
  width = "72", height = "60", style = "vertical-align:middle;",
  tags$defs(
    tags$linearGradient(id = "pg", x1 = "0%", y1 = "0%", x2 = "100%", y2 = "100%",
      tags$stop(offset = "0%",   style = "stop-color:#c4b5fd;stop-opacity:1"),
      tags$stop(offset = "100%", style = "stop-color:#818cf8;stop-opacity:1")
    )
  ),
  # prism triangle body
  tags$polygon(points = "24,2 46,38 2,38",
               fill = "url(#pg)", stroke = "rgba(255,255,255,0.4)",
               `stroke-width` = "1.2"),
  # incoming white beam (left side)
  tags$line(x1 = "0", y1 = "18", x2 = "13", y2 = "24",
            stroke = "white", `stroke-width` = "2", opacity = "0.85"),
  # outgoing rainbow beams (right side)
  tags$line(x1 = "35", y1 = "22", x2 = "48", y2 = "12",
            stroke = "#f87171", `stroke-width` = "1.8", opacity = "0.95"),
  tags$line(x1 = "35", y1 = "25", x2 = "48", y2 = "19",
            stroke = "#fbbf24", `stroke-width` = "1.8", opacity = "0.95"),
  tags$line(x1 = "35", y1 = "28", x2 = "48", y2 = "26",
            stroke = "#34d399", `stroke-width` = "1.8", opacity = "0.95"),
  tags$line(x1 = "35", y1 = "31", x2 = "48", y2 = "33",
            stroke = "#60a5fa", `stroke-width` = "1.8", opacity = "0.95")
)

prism_title <- tags$div(
  style = "display:flex; align-items:center; gap:14px;",
  prism_logo,
  tags$span("PRISM",
    style = paste0(
      "font-weight:800; font-size:1.5rem; letter-spacing:0.14em;",
      "background:linear-gradient(90deg,#e0c3fc,#a5b4fc,#93c5fd);",
      "-webkit-background-clip:text; -webkit-text-fill-color:transparent;",
      "background-clip:text;"
    )
  )
)

ui <- page_navbar(
  title    = prism_title,
  theme    = bs_theme(
    bootswatch = "flatly",
    primary    = "#1e1b4b",
    base_font  = font_google("Inter")
  ),
  fillable = FALSE,
  header   = tags$head(
    tags$style("
    html { overflow-y: scroll !important; height: auto !important; }
    body, .bslib-page-fill, .bslib-flow-mobile, .tab-content, .tab-pane,
    .bslib-sidebar-layout, .bslib-page-navbar, .bslib-page-navbar > .container-fluid,
    .bslib-sidebar-layout > .main, .bslib-sidebar-layout > :not(.sidebar) {
      overflow: visible !important; overflow-y: visible !important;
      height: auto !important; max-height: none !important; min-height: unset !important;
    }
    .sidebar, .bslib-sidebar-layout > .sidebar, .sidebar > .sidebar-content {
      overflow: visible !important; overflow-y: visible !important;
      height: auto !important; max-height: none !important;
      position: sticky !important; top: 60px;
    }
    /* Fix DT filter widgets overflowing */
    .dataTables_wrapper thead th { overflow: visible !important; }
    .dataTables_filter input, thead .form-control { box-sizing: border-box !important; }
    thead td { overflow: visible !important; white-space: nowrap; }
    .noUi-target { margin: 4px 2px !important; }
    thead input[type='search'] { width: 100% !important; box-sizing: border-box !important; }
    .navbar { background-color: #1e1b4b !important; border-bottom: 1px solid rgba(255,255,255,0.08); padding: 0.4rem 1rem !important; }
    .navbar .nav-link { color: rgba(255,255,255,0.75) !important; white-space: nowrap; }
    .navbar .nav-link:hover, .navbar .nav-link.active { color: #fff !important; }
    .navbar > .container-fluid { flex-wrap: nowrap !important; align-items: center !important; position: relative !important; }
    .navbar-brand { flex-shrink: 0 !important; display: flex !important; align-items: center !important; padding-top: 0 !important; padding-bottom: 0 !important; }
    .navbar-nav { position: absolute !important; left: 50% !important; transform: translateX(-50%) !important; flex-wrap: nowrap !important; gap: 0.25rem; align-items: center !important; }
    #loading-overlay {
      display:none; position:fixed; inset:0; z-index:9999;
      background:rgba(255,255,255,0.75);
      justify-content:center; align-items:center; flex-direction:column;
    }
    #loading-overlay.active { display:flex; }
    .spinner { width:48px; height:48px; border:5px solid #e5e7eb; border-top-color:#1e1b4b;
      border-radius:50%; animation:spin 0.8s linear infinite; }
    @keyframes spin { to { transform:rotate(360deg); } }
    #loading-overlay .loading-text { margin-top:16px; font-size:0.95rem; color:#1e1b4b; font-weight:500; }
    "),
    tags$div(id = "loading-overlay",
      tags$div(class = "spinner"),
      tags$div(class = "loading-text", "Querying FDA FAERS database...")
    ),
    tags$script(HTML("
      $(document).on('click', '#run_check', function() {
        $('#loading-overlay').addClass('active');
      });
      $(document).on('shiny:value shiny:error', function(e) {
        if (e.name === 'monitor_status_row') {
          $('#loading-overlay').removeClass('active');
        }
      });
    "))
  ),

  # ── Tab 1: Monitor Your Drug ─────────────────────────────────────────────
  nav_panel(
    title = "Monitor Your Drug",
    icon  = icon("magnifying-glass-chart"),

    layout_sidebar(
      fillable = FALSE,
      sidebar  = sidebar(
        width = 280,
        h6("Enter a drug and adverse event to check its current FAERS signal status.",
           class = "text-muted mb-3"),
        textInput("live_drug", "Drug name (brand or generic)",
                  placeholder = "e.g. HUMIRA, atorvastatin"),
        selectizeInput("live_ae", "Adverse event (MedDRA Preferred Term)",
                       choices  = c("Select a PT term..." = "", pt_terms),
                       selected = "",
                       options  = list(create = FALSE, placeholder = "Type to search PT terms...")),
        actionButton("run_check", "Check Signal",
                     class = "btn-primary w-100 mt-2", icon = icon("play")),
        hr(),
        div(class = "text-muted", style = "font-size:0.78rem;",
          strong("Tips:"), br(),
          "Enter a brand or generic name — PRISM automatically searches all equivalent names (e.g. LIPITOR also searches atorvastatin).", br(), br(),
          "MedDRA PT terms: search the dropdown — only Preferred Terms work in FAERS.", br(), br(),
          "Each check queries 10 quarters of live FDA data — allow ~1 min."
        )
      ),

      # Welcome message (hidden once a check has run)
      conditionalPanel(
        "input.run_check == 0",
        div(class = "d-flex align-items-center justify-content-center py-5",
          div(class = "text-center text-muted",
            tags$i(class = "fa fa-chart-line fa-3x mb-3", style = "color:#adb5bd;"),
            h5("Enter a drug and adverse event, then click Check Signal"),
            p("The tool queries FDA's FAERS database in real time and assesses",
              " whether a disproportionality signal exists for your drug.")
          )
        )
      ),

      # Results (shown after first check) — all output IDs are static in the DOM
      conditionalPanel(
        "input.run_check > 0",
        uiOutput("monitor_status_row"),
        uiOutput("resolved_names_note"),
        uiOutput("label_change_banner"),
        layout_columns(
          col_widths = c(8, 4),
          card(
            card_header(uiOutput("live_chart_title")),
            plotOutput("live_chart", height = "360px"),
            card_footer(class = "text-muted", style = "font-size:0.75rem;",
                        uiOutput("live_provenance"))
          ),
          card(
            card_header("Regulatory Context"),
            card_body(
              uiOutput("reg_context"),
              hr(),
              uiOutput("rec_text")
            )
          )
        ),
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            span(icon("table-list"), " Raw Quarterly Data"),
            tags$button(class = "btn btn-sm btn-outline-secondary",
                        id = "toggle_quarterly",
                        onclick = "var body = $(this).closest('.card').find('.card-body'); body.toggle(); $(this).text(body.is(':visible') ? 'Hide' : 'Show'); if(body.is(':visible')) { $($.fn.dataTable.tables(true)).DataTable().columns.adjust(); }",
                        "Show")
          ),
          card_body(style = "display:none;", DTOutput("raw_quarterly_table"))
        ),
        # ── Regulatory Timeline Intelligence (hidden when FDA already acted) ──
        uiOutput("timeline_card")
      )
    )
  ),

  # ── Tab 2: Reference Cohort ──────────────────────────────────────────────
  nav_panel(
    title = "Reference Cohort",
    icon  = icon("clock-rotate-left"),

    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        selectInput("class_filter", "Filter by class:",
                    choices = c("All classes" = "", sort(unique(combined$therapeutic_class))),
                    selected = ""),
        selectInput("signal_filter", "Signal detected?",
                    choices = c("All drugs" = "", "Yes — FAERS signal found" = "yes",
                                "No — no FAERS signal" = "no"),
                    selected = ""),
        selectInput("drug_select", "Select drug:",
                    choices = drug_choices, selected = drug_choices[1]),
        hr(),
        uiOutput("drug_info_box")
      ),
      card(
        card_header("Historical Signal-to-Label Timeline"),
        plotOutput("prr_trend", height = "450px")
      ),
      card(
        card_header("How to read this chart"),
        card_body(
          tags$ul(
            tags$li(
              span(style="display:inline-block;width:12px;height:12px;background:#90b8e0;border-radius:2px;margin-right:5px;"),
              span(style="display:inline-block;width:12px;height:12px;background:#e8501a;border-radius:2px;margin-right:5px;"),
              span(style="display:inline-block;width:12px;height:12px;background:#2ca02c;border-radius:2px;margin-right:5px;"),
              strong("Bar colour"), ": blue = pre-signal, orange = signal active / awaiting label update, green = post-label change."
            ),
            tags$li(
              span(style="color:#e05c00;font-weight:600;", "Orange line"),
              ": PRR (right axis). Values above the dashed line indicate disproportionate reporting."
            ),
            tags$li(
              span(style="color:darkgreen;font-weight:600;", "Green dotted line"),
              ": quarter when signal was first confirmed."
            ),
            tags$li(
              span(style="color:firebrick;font-weight:600;", "Red line"),
              ": date of FDA label update."
            ),
            tags$li(
              strong("Signal criteria"),
              ": PRR \u2265 2, 95% CI lower bound > 1, n \u2265 3, \u03c7\u00b2 \u2265 4 (Evans + Rothman CI)."
            )
          )
        )
      )
    )
  ),

  # ── Tab 3: Drug Table ────────────────────────────────────────────────────
  nav_panel(
    title = "Drug Table",
    icon  = icon("table"),
    card(
      card_header("Reference Cohort — Full Data"),
      card_body(DTOutput("drug_table"))
    ),
    card(
      card_header(icon("fingerprint"), " Data Provenance"),
      card_body(
        if (!is.null(provenance)) {
          tags$table(class = "table table-sm table-borderless mb-0",
            style = "font-size:0.85rem;",
            tags$tbody(
              tags$tr(tags$td(class="text-muted", "Pipeline run"),
                      tags$td(class="fw-semibold", provenance$pipeline_run_utc, " UTC")),
              tags$tr(tags$td(class="text-muted", "FAERS date range"),
                      tags$td(class="fw-semibold",
                              paste(provenance$faers_date_range["earliest"], "to",
                                    provenance$faers_date_range["latest"]))),
              tags$tr(tags$td(class="text-muted", "Drugs in cohort"),
                      tags$td(class="fw-semibold",
                              paste(provenance$drugs_queried, collapse = ", "))),
              tags$tr(tags$td(class="text-muted", "Records"),
                      tags$td(class="fw-semibold", provenance$n_records)),
              tags$tr(tags$td(class="text-muted", "API source"),
                      tags$td(class="fw-semibold",
                              tags$a(href="https://open.fda.gov/apis/drug/event/",
                                     target="_blank", "openFDA FAERS API"))),
              tags$tr(tags$td(class="text-muted", "R version"),
                      tags$td(class="fw-semibold", provenance$r_version))
            )
          )
        } else {
          p(class = "text-muted mb-0",
            "Provenance not available. Re-run run_pipeline.R to generate data/provenance.rds.")
        }
      )
    )
  ),

  # ── Tab 4: Methodology ───────────────────────────────────────────────────
  nav_panel(
    title = "Methodology",
    icon  = icon("flask"),

    layout_columns(
      col_widths = c(12),

      card(
        card_header(icon("database"), " Data Source"),
        card_body(
          p("All adverse event data comes from the",
            tags$a(href = "https://open.fda.gov/apis/drug/event/", target = "_blank",
                   "FDA Adverse Event Reporting System (FAERS)"),
            "through the openFDA API. FAERS is a spontaneous reporting system where healthcare professionals,
            consumers, and manufacturers submit reports of suspected adverse drug reactions."),
          p("Drug labeling data (Boxed Warnings, Contraindications) is pulled in real time from the",
            tags$a(href = "https://open.fda.gov/apis/drug/label/", target = "_blank",
                   "openFDA Drug Labeling API"), "."),
          tags$h6(class = "mt-3 fw-semibold", "Limitations of FAERS data"),
          tags$ul(
            tags$li(tags$strong("Underreporting:"), " Most adverse events go unreported. No reports does not mean no risk."),
            tags$li(tags$strong("No causation:"), " A report means a patient took a drug and had an event. It does not prove the drug caused it."),
            tags$li(tags$strong("Reporting bias:"), " Media attention, lawsuits, and FDA safety communications can drive spikes in reporting that have nothing to do with true incidence."),
            tags$li(tags$strong("Duplicate reports:"), " The same case can be submitted by the manufacturer, the doctor, and the patient separately.")
          )
        )
      ),

      card(
        card_header(icon("square-root-variable"), " Signal Detection: PRR Method"),
        card_body(
          p("PRISM uses the", tags$strong("Proportional Reporting Ratio (PRR)"),
            "to measure whether a drug-AE pair is reported more often than expected compared to all other
            drugs in the FAERS database. PRR is the standard disproportionality metric used by the
            EMA and was first described by Evans et al. (2001)."),
          tags$h6(class = "mt-3 fw-semibold", "Count definitions"),
          p("PRISM queries four counts from the openFDA API per drug-AE-quarter combination:"),
          tags$table(class = "table table-bordered table-sm", style = "max-width: 550px; font-size: 0.9rem;",
            tags$thead(
              tags$tr(tags$th("Count"), tags$th("Definition"))
            ),
            tags$tbody(
              tags$tr(tags$td(tags$strong("a")), tags$td("Reports with target drug AND target AE")),
              tags$tr(tags$td(tags$strong("B")), tags$td("All reports with target drug (any AE)")),
              tags$tr(tags$td(tags$strong("C")), tags$td("All reports with target AE (any drug)")),
              tags$tr(tags$td(tags$strong("D")), tags$td("All reports in the quarter"))
            )
          ),
          p(class = "text-muted", style = "font-size: 0.85rem;",
            "B, C, and D are marginal totals (not inner cells of a 2\u00d72 table).
            Each is obtained from a separate openFDA API query."),
          tags$div(style = "background: #f8f9fa; border-radius: 6px; padding: 12px 16px; margin: 12px 0; font-family: monospace; font-size: 0.9rem;",
            tags$div("PRR = (a / B) / (C / D)"),
            tags$div(class = "mt-1", "\u03c7\u00b2 = (a \u2212 E)\u00b2 / E, where E = B \u00d7 C / D"),
            tags$div(class = "mt-1", "95% CI = exp(ln(PRR) \u00b1 1.96 \u00d7 SE)"),
            tags$div(class = "mt-1", "SE = \u221a(1/a \u2212 1/B + 1/C \u2212 1/D)")
          ),
          p("This is equivalent to the standard Evans PRR formula [a/(a+b)] / [c/(c+d)]
            when a is small relative to the marginals, which holds for the vast majority of
            drug-AE pairs in FAERS. The 95% confidence interval uses the log-normal
            approximation for ratio measures (Rothman, 2008)."),
          tags$h6(class = "mt-3 fw-semibold", "Why PRR and not EBGM or IC?"),
          p("The FDA uses", tags$strong("EBGM (Empirical Bayesian Geometric Mean)"),
            "internally (DuMouchel, 1999). It applies Bayesian shrinkage to reduce false positives
            when report counts are low. The WHO Uppsala Monitoring Centre uses the",
            tags$strong("Information Component (IC)"), "(Bate et al., 1998; Nor\u00e9n et al., 2013)
            for their global VigiBase database."),
          p("Both of these methods need access to the full FAERS database to compute the prior distributions
            that drive the shrinkage. The openFDA API only returns counts for individual queries, not the
            full reporting distribution. PRR is the right frequentist alternative when working through an API,
            and it is still the standard at the EMA and MHRA.")
        )
      ),

      card(
        card_header(icon("check-double"), " Signal Classification Criteria"),
        card_body(
          p("A signal is", tags$strong("met"), "in a given quarter when all four of the following hold:"),
          tags$table(class = "table table-sm table-bordered", style = "max-width: 500px; font-size: 0.9rem;",
            tags$thead(
              tags$tr(tags$th("Criterion"), tags$th("Threshold"), tags$th("Rationale"))
            ),
            tags$tbody(
              tags$tr(tags$td("Report count (a)"), tags$td("\u2265 3"), tags$td("Minimum sample size")),
              tags$tr(tags$td("PRR"), tags$td("\u2265 2.0"), tags$td("Disproportionality")),
              tags$tr(tags$td("95% CI lower bound"), tags$td("> 1.0"), tags$td("Statistical significance")),
              tags$tr(tags$td("\u03c7\u00b2"), tags$td("\u2265 4.0"), tags$td("Independence test"))
            )
          ),
          p(class = "mt-3", "Signal status is based on the most recent 6 quarters:"),
          tags$ul(
            tags$li(tags$span(class = "badge bg-danger", "CONFIRMED"), " Signal met in 2+ of the last 6 quarters"),
            tags$li(tags$span(class = "badge bg-warning text-dark", "EMERGING"), " Signal met in exactly 1 of the last 6 quarters"),
            tags$li(tags$span(class = "badge bg-success", "NOT DETECTED"), " Signal not met in any of the last 6 quarters")
          ),
          p(class = "text-muted mt-2", "These thresholds come from Evans et al. (2001). We added a
            CI lower bound > 1 requirement (per Rothman) to reduce false positives in quarters with very few reports."),
          tags$h6(class = "mt-3 fw-semibold", "Signal duration metrics"),
          p("The Monitor tab reports two complementary duration measures:"),
          tags$ul(
            tags$li(tags$strong("Signal Duration"), " \u2014 months since the signal was first detected in any quarter. Used for regulatory timeline comparison against historical lag data."),
            tags$li(tags$strong("Current Streak"), " \u2014 consecutive quarters where signal criteria are currently met. Indicates signal persistence and stability.")
          ),
          p(class = "text-muted", "A long duration with a short streak may indicate an intermittent signal. A short duration with a long streak suggests a newly emerging but consistent signal.")
        )
      ),

      card(
        card_header(icon("book"), " Reference Cohort"),
        card_body(
          p("The reference cohort includes", tags$strong("40 drugs"), "across",
            tags$strong("10 therapeutic classes"), "where FDA took regulatory action (Boxed Warning, Contraindication,
            Warning, or Withdrawal) after post-market safety signals. These are known, documented cases."),
          p("For each drug, we pulled FAERS data for the adverse event that led to the label change,
            computed PRR per quarter from approval through the label change date, and measured the",
            tags$strong("signal-to-label lag"),
            ": how long it took from when the FAERS signal first appeared to when FDA acted."),
          tags$h6(class = "mt-3 fw-semibold", "What the cohort tells us"),
          tags$ul(
            tags$li("For some classes (fluoroquinolones, antidiabetics, antithrombotics), FAERS signals
                    showed up well before FDA acted."),
            tags$li("For others (PPIs, bisphosphonates), FAERS was not the driver. FDA acted based on
                    clinical trials and published case series instead."),
            tags$li("Lag times range from under 1 month to over 111 months, so FAERS alone cannot predict when
                    FDA will act.")
          ),
          p(class = "text-muted", "This is why PRISM shows historical context, not predictions.")
        )
      ),

      card(
        card_header(icon("scale-balanced"), " Limitations"),
        card_body(
          tags$ul(
            tags$li("PRISM only uses FAERS data. It does not include clinical trial data,
                    published literature, or international databases like EudraVigilance or VigiBase."),
            tags$li("PRR is a screening tool, not a risk assessment. A signal means something is worth
                    investigating, not that the drug caused the event."),
            tags$li("The openFDA API has a 1 to 3 quarter lag, so the most recent quarter may be incomplete."),
            tags$li("PRISM searches three drug name fields (brand name, generic name, and free-text
                    medicinal product) but can still miss reports with misspellings, abbreviations,
                    or non-US trade names."),
            tags$li("This tool is for educational and research purposes only. It is not regulatory advice.")
          )
        )
      ),

      card(
        card_header(icon("quote-left"), " References"),
        card_body(
          tags$ol(style = "font-size: 0.9rem;",
            tags$li("Evans SJW, Waller PC, Davis S. (2001). Use of proportional reporting ratios (PRRs) for signal
                    generation from spontaneous adverse drug reaction reports.", tags$em("Pharmacoepidemiology and
                    Drug Safety"), ", 10(6), 483\u2013486."),
            tags$li("Rothman KJ, Greenland S, Lash TL. (2008).", tags$em("Modern Epidemiology"), ", 3rd ed.
                    Lippincott Williams & Wilkins. [Log-normal CI for ratio measures]"),
            tags$li("DuMouchel W. (1999). Bayesian data mining in large frequency tables, with an application to
                    the FDA spontaneous reporting system.", tags$em("The American Statistician"),
                    ", 53(3), 177\u2013190. [EBGM/MGPS method used by FDA]"),
            tags$li("Bate A, Lindquist M, Edwards IR, et al. (1998). A Bayesian neural network method for
                    adverse drug reaction signal generation.",
                    tags$em("European Journal of Clinical Pharmacology"),
                    ", 54(4), 315\u2013321. [Original IC method used by WHO/UMC]"),
            tags$li("Nor\u00e9n GN, Hopstadius J, Bate A. (2013). Shrinkage observed-to-expected ratios for robust
                    and transparent large-scale pattern discovery.",
                    tags$em("Statistical Methods in Medical Research"),
                    ", 22(1), 57\u201369. [Shrinkage IC refinement]"),
            tags$li("FDA. openFDA: FAERS API documentation.",
                    tags$a(href = "https://open.fda.gov/apis/drug/event/", target = "_blank",
                           "https://open.fda.gov/apis/drug/event/")),
            tags$li("FDA. openFDA: Drug Labeling API documentation.",
                    tags$a(href = "https://open.fda.gov/apis/drug/label/", target = "_blank",
                           "https://open.fda.gov/apis/drug/label/"))
          )
        )
      )
    )
  ),

  footer = tags$footer(
    class = "text-center py-3",
    style = "border-top:1px solid #e2e8f0; margin-top:2rem; background:#f8fafc;",
    tags$div(
      style = "display:inline-flex; align-items:center; gap:6px;",
      tags$svg(
        xmlns = "http://www.w3.org/2000/svg", viewBox = "0 0 48 40",
        width = "28", height = "23", style = "vertical-align:middle;",
        tags$defs(
          tags$linearGradient(id = "pg-footer", x1 = "0%", y1 = "0%", x2 = "100%", y2 = "100%",
            tags$stop(offset = "0%",   style = "stop-color:#c4b5fd;stop-opacity:1"),
            tags$stop(offset = "100%", style = "stop-color:#818cf8;stop-opacity:1")
          )
        ),
        tags$polygon(points = "24,2 46,38 2,38",
                     fill = "url(#pg-footer)", stroke = "rgba(255,255,255,0.4)",
                     `stroke-width` = "1.2"),
        tags$line(x1 = "0", y1 = "18", x2 = "13", y2 = "24",
                  stroke = "white", `stroke-width` = "2", opacity = "0.85"),
        tags$line(x1 = "35", y1 = "22", x2 = "48", y2 = "12",
                  stroke = "#f87171", `stroke-width` = "1.8", opacity = "0.95"),
        tags$line(x1 = "35", y1 = "25", x2 = "48", y2 = "19",
                  stroke = "#fbbf24", `stroke-width` = "1.8", opacity = "0.95"),
        tags$line(x1 = "35", y1 = "28", x2 = "48", y2 = "26",
                  stroke = "#34d399", `stroke-width` = "1.8", opacity = "0.95"),
        tags$line(x1 = "35", y1 = "31", x2 = "48", y2 = "33",
                  stroke = "#60a5fa", `stroke-width` = "1.8", opacity = "0.95")
      ),
      tags$span(
        style = paste0(
          "font-weight:700; font-size:0.85rem; letter-spacing:0.1em;",
          "background:linear-gradient(90deg,#c4b5fd,#818cf8,#60a5fa);",
          "-webkit-background-clip:text; -webkit-text-fill-color:transparent;",
          "background-clip:text;"
        ),
        "PRISM"
      ),
      tags$span(
        style = "font-size:0.75rem; color:#cbd5e1; margin:0 3px;",
        "|"
      ),
      tags$span(
        style = "font-size:0.75rem; color:#64748b;",
        "Pharmacovigilance Real-time Intelligence Signal Monitor"
      ),
      if (!is.null(provenance)) tags$span(
        style = "font-size:0.72rem; color:#94a3b8; margin-left:12px;",
        paste0("Cohort study period: ",
               provenance$faers_date_range["earliest"], " to ",
               provenance$faers_date_range["latest"],
               " | Last refreshed ",
               format(as.POSIXct(provenance$pipeline_run_utc, tz = "UTC"), "%B %d, %Y"),
               " | Monitor tab queries live data")
      )
    )
  )
)


# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Tab 1: Monitor Your Drug ───────────────────────────────────────────────

  live_data <- eventReactive(input$run_check, {
    req(nchar(trimws(input$live_drug)) > 0, nchar(trimws(input$live_ae)) > 0)
    withProgress(message = "Querying FDA FAERS", value = 0,
                 detail  = "0% — starting...", {
      pull_live_signal(
        drug_name   = toupper(trimws(input$live_drug)),
        pt_term     = tolower(trimws(input$live_ae)),
        n_quarters  = 12,
        progress_cb = function(value, detail) {
          setProgress(value = value, detail = detail)
        }
      )
    })
  })

  # Generate a unique session ID for audit trail
  audit_session_id <- paste0("S-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
                             sample(1000:9999, 1))

  # Write audit log after each query
  observeEvent(input$run_check, {
    df     <- live_data()
    total_a <- sum(df$count_a, na.rm = TRUE)
    max_a   <- max(df$count_a, na.rm = TRUE)
    sparse  <- total_a < 10 | max_a < 3
    status  <- if (sparse) "INSUFFICIENT DATA" else signal_status(df)
    prr     <- tail(df$PRR[!is.na(df$PRR) & is.finite(df$PRR)], 1)
    prr_lo  <- tail(df$PRR_lo[!is.na(df$PRR_lo) & is.finite(df$PRR_lo)], 1)
    prr_hi  <- tail(df$PRR_hi[!is.na(df$PRR_hi) & is.finite(df$PRR_hi)], 1)
    if (length(prr) == 0) prr <- NA
    if (length(prr_lo) == 0) prr_lo <- NA
    if (length(prr_hi) == 0) prr_hi <- NA
    write_audit_log(
      drug             = trimws(input$live_drug),
      ae               = tolower(trimws(input$live_ae)),
      status           = status,
      current_prr      = prr,
      prr_lo           = prr_lo,
      prr_hi           = prr_hi,
      n_reports        = total_a,
      quarters_queried = nrow(df),
      session_id       = audit_session_id
    )
  })

  # Shared reactive: compute status/stats once, used by all monitor outputs
  monitor_stats <- reactive({
    req(input$run_check > 0)
    df          <- live_data()
    total_a     <- sum(df$count_a, na.rm = TRUE)
    max_a       <- max(df$count_a, na.rm = TRUE)
    sparse      <- total_a < 10 | max_a < 3
    status      <- if (sparse) "INSUFFICIENT DATA" else signal_status(df)
    n_active    <- quarters_active(df)
    current_prr <- tail(df$PRR[!is.na(df$PRR) & is.finite(df$PRR)], 1)
    if (length(current_prr) == 0) current_prr <- NA
    current_prr_lo <- tail(df$PRR_lo[!is.na(df$PRR_lo) & is.finite(df$PRR_lo)], 1)
    current_prr_hi <- tail(df$PRR_hi[!is.na(df$PRR_hi) & is.finite(df$PRR_hi)], 1)
    if (length(current_prr_lo) == 0) current_prr_lo <- NA
    if (length(current_prr_hi) == 0) current_prr_hi <- NA
    prr_above_not_met <- sum(!is.na(df$PRR) & df$PRR >= 2 & !df$signal_met, na.rm = TRUE)
    months_first <- months_since_first_signal(df)
    list(df = df, status = status, n_active = n_active, months_first = months_first,
         current_prr = current_prr,
         current_prr_lo = current_prr_lo, current_prr_hi = current_prr_hi,
         prr_above_not_met = prr_above_not_met, total_a = total_a, sparse = sparse)
  })

  output$monitor_status_row <- renderUI({
    s <- monitor_stats()
    status_theme <- switch(s$status,
      "CONFIRMED"         = "danger",
      "EMERGING"          = "warning",
      "NOT DETECTED"      = "success",
      "INSUFFICIENT DATA" = "secondary")
    status_icon <- switch(s$status,
      "CONFIRMED"         = "triangle-exclamation",
      "EMERGING"          = "circle-exclamation",
      "NOT DETECTED"      = "circle-check",
      "INSUFFICIENT DATA" = "database")
    layout_columns(
      fill = FALSE,
      value_box(title = "Signal Status", value = s$status,
                showcase = icon(status_icon), theme = status_theme,
                if (s$status == "INSUFFICIENT DATA")
                  p(style = "font-size:0.78rem;",
                    "Only ", s$total_a, " report(s) found across 10 quarters.",
                    " Too sparse for reliable PRR — try the generic name or a broader AE term.")
                else if (s$status == "NOT DETECTED" && s$prr_above_not_met > 0)
                  p(style = "font-size:0.78rem;",
                    icon("triangle-exclamation"), " PRR \u2265 2 in ",
                    s$prr_above_not_met, " quarter(s) but report volume too low",
                    " (n\u00a0<\u00a03 or \u03c7\u00b2\u00a0<\u00a04) to confirm signal.")
              ),
      value_box(title = "Current PRR",
                value = if (is.na(s$current_prr)) "N/A" else round(s$current_prr, 2),
                showcase = icon("chart-line"), theme = "primary",
                p(if (is.na(s$current_prr)) "Insufficient reports"
                  else paste0("95% CI: [",
                              if (is.na(s$current_prr_lo)) "—" else round(s$current_prr_lo, 2), ", ",
                              if (is.na(s$current_prr_hi)) "—" else round(s$current_prr_hi, 2), "]")),
                p(if (!is.na(s$current_prr_lo) && s$current_prr_lo > 1)
                    "CI lower bound > 1 — signal credible"
                  else if (!is.na(s$current_prr) && s$current_prr >= 2)
                    "PRR elevated but CI includes 1"
                  else if (!is.na(s$current_prr))
                    "Below detection threshold")),
      value_box(title = "Consecutive Signal Quarters", value = s$n_active,
                showcase = icon("calendar"),
                theme = if (s$n_active >= 2) "warning" else "secondary")
    )
  })

  # ── Resolved names note: show which active ingredient is being searched
  output$resolved_names_note <- renderUI({
    req(input$run_check > 0)
    df <- live_data()
    canonical <- attr(df, "canonical_ingredient")
    input_name <- toupper(trimws(isolate(input$live_drug)))
    if (is.null(canonical)) return(NULL)
    # Only show if we resolved to a different name than what the user typed
    if (canonical == input_name) return(NULL)
    div(
      class = "alert alert-light d-flex align-items-center mb-2 py-2",
      style = "font-size:0.85rem; border-left:4px solid #6c757d;",
      icon("pills", class = "me-2"),
      span(
        tags$strong(input_name), " resolved to active ingredient ",
        tags$strong(canonical),
        " — results include all ", canonical, " products regardless of brand."
      )
    )
  })

  # ── Label change banner: check if queried drug/AE already has a known label change
  output$label_change_banner <- renderUI({
    req(input$run_check > 0)
    drug_upper <- toupper(trimws(isolate(input$live_drug)))
    ae_lower   <- tolower(trimws(isolate(input$live_ae)))

    req(nchar(drug_upper) > 0, nchar(ae_lower) > 0)
    match <- find_cohort_match(drug_upper, ae_lower)

    if (nrow(match) > 0) {
      m <- match[1, ]
      return(div(
        class = "alert alert-info d-flex align-items-center mb-2",
        style = "font-size:0.9rem; border-left:4px solid #0d6efd;",
        icon("circle-info", class = "me-2", style = "font-size:1.2rem;"),
        div(
          strong("FDA has already acted on this drug/AE. "),
          span(paste0(m$drug_name, " received a \"", m$label_change_type,
                      "\" for ", m$adverse_event, " on ",
                      format(m$label_change_date, "%B %d, %Y"), ".")),
          br(),
          span(class = "text-muted", style = "font-size:0.82rem;",
               "The signal data below reflects current FAERS reporting — useful for monitoring ongoing trends, ",
               "but the primary regulatory action has already occurred.")
        )
      ))
    }
    # If no match in our cohort, check openFDA for a BBW that matches the queried AE
    bbw <- check_boxed_warning(drug_upper)
    if (bbw$has_bbw && !is.null(bbw$bbw_text)) {
      # Check if the queried AE term (or synonyms/roots) appears in the BBW/warnings text
      bbw_lower <- tolower(bbw$bbw_text)
      ae_words <- expand_ae_terms(ae_lower)
      ae_match <- length(ae_words) > 0 && any(sapply(ae_words, function(w) grepl(w, bbw_lower, fixed = TRUE)))
      if (ae_match) {
        return(div(
          class = "alert alert-warning d-flex align-items-start mb-2",
          style = "font-size:0.9rem; border-left:4px solid #ffc107;",
          icon("exclamation-triangle", class = "me-2 mt-1", style = "font-size:1.2rem;"),
          div(
            strong("This drug has a Boxed Warning related to this adverse event. "),
            span("The FDA label for ", drug_upper,
                 " includes a Boxed Warning (the most serious safety alert) ",
                 "that covers risks relevant to your query.")
          )
        ))
      }
    }
  })

  # Reactive: does this drug/AE already have a known label change in our cohort?
  has_existing_action <- reactive({
    req(input$run_check > 0)
    drug_upper <- toupper(trimws(isolate(input$live_drug)))
    ae_lower   <- tolower(trimws(isolate(input$live_ae)))
    req(nchar(drug_upper) > 0, nchar(ae_lower) > 0)
    nrow(find_cohort_match(drug_upper, ae_lower)) > 0
  })

  output$live_chart_title <- renderUI({
    req(input$run_check > 0)
    paste0(toupper(trimws(isolate(input$live_drug))), " — ",
           tools::toTitleCase(tolower(trimws(isolate(input$live_ae)))))
  })

  output$live_provenance <- renderUI({
    req(input$run_check > 0)
    df <- live_data()
    q_time   <- attr(df, "query_time_utc")   %||% "unknown"
    q_window <- attr(df, "query_window")     %||% "unknown"
    drug_val <- toupper(trimws(isolate(input$live_drug)))
    dailymed_url <- paste0(
      "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=",
      URLencode(drug_val, reserved = TRUE))
    tagList(
      icon("fingerprint", style = "margin-right:4px;"),
      paste0("Queried ", q_time, " UTC | Window: ", q_window, " | "),
      tags$a(href = dailymed_url, target = "_blank", "View FDA label on DailyMed"),
      paste0(" | Quarters: ", nrow(df))
    )
  })

  # Reactive: resolve the queried drug's therapeutic class and filter benchmark
  bench_filtered <- reactive({
    req(input$run_check > 0)
    drug_upper <- toupper(trimws(isolate(input$live_drug)))
    queried_class <- drug_class_map[drug_upper]  # NA if not found
    class_match <- !is.na(queried_class) &&
                   queried_class %in% benchmark_drugs$therapeutic_class
    if (class_match) {
      df <- benchmark_drugs |> filter(therapeutic_class == queried_class)
    } else {
      df <- benchmark_drugs
    }
    list(
      df      = df,
      class   = if (class_match) queried_class else NULL,
      median  = median(df$lag_months),
      q25     = quantile(df$lag_months, 0.25),
      q75     = quantile(df$lag_months, 0.75),
      n       = nrow(df)
    )
  })

  output$reg_context <- renderUI({
    s             <- monitor_stats()
    b             <- bench_filtered()
    months_active <- s$months_first
    class_label   <- if (!is.null(b$class)) b$class else "all classes"
    already       <- has_existing_action()

    if (already) {
      # Drug/AE already has a label change — show historical context
      drug_upper <- toupper(trimws(isolate(input$live_drug)))
      ae_lower   <- tolower(trimws(isolate(input$live_ae)))
      m <- find_cohort_match(drug_upper, ae_lower)
      if (nrow(m) > 0) {
        m <- m[1, ]
        lag_text <- if (!is.na(m$lag_months) && m$lag_months > 0)
          paste0("The FAERS signal preceded the label change by ", round(m$lag_months, 1), " months.")
        else if (!is.na(m$lag_months) && m$lag_months <= 0)
          "The FDA acted based on clinical trial data before a clear FAERS signal emerged."
        else
          "No FAERS signal was detected for this drug/AE prior to the label change."
        tagList(
          div(class = "mb-2",
            icon("shield-halved", style = "color:#0d6efd;"),
            strong(" FDA Action on Record")
          ),
          tags$table(class = "table table-sm table-borderless mb-1",
            style = "font-size:0.85rem;",
            tags$tbody(
              tags$tr(tags$td(class="text-muted", "Action"),
                      tags$td(class="fw-semibold", m$label_change_type)),
              tags$tr(tags$td(class="text-muted", "Date"),
                      tags$td(class="fw-semibold", format(m$label_change_date, "%B %d, %Y"))),
              tags$tr(tags$td(class="text-muted", "Adverse event"),
                      tags$td(class="fw-semibold", m$adverse_event)),
              tags$tr(tags$td(class="text-muted", "Class"),
                      tags$td(class="fw-semibold", m$therapeutic_class))
            )
          ),
          p(class = "text-muted mb-0", style = "font-size:0.82rem;", lag_text)
        )
      }
    } else if (s$status == "INSUFFICIENT DATA") {
      p(class = "mb-0 text-muted",
        "Insufficient FAERS reports to compute a reliable signal. ",
        "Try searching by generic name, or check if the drug is reported under a different brand name in FAERS.")
    } else if (s$status == "NOT DETECTED") {
      if (!is.null(b$class)) {
        tagList(
          p(class = "mb-1",
            "No disproportionality signal detected in the queried period."),
          p(class = "mb-0 text-muted", style = "font-size:0.85rem;",
            "Based on ", strong(b$n), " ", class_label,
            " reference drugs, signals typically take a median of ",
            strong(paste0(round(b$median, 0), " months")), " to lead to FDA action.")
        )
      } else {
        p(class = "mb-1",
          "No disproportionality signal detected in the queried period.")
      }
    } else {
      # CONFIRMED or EMERGING
      if (!is.null(b$class)) {
        remaining <- round(b$median - months_active)
        tagList(
          p(class = "mb-1",
            "Comparing against ", strong(b$n), " ", class_label,
            " reference drug(s). Median signal-to-label lag: ",
            strong(paste0(round(b$median, 0), " months")),
            " (IQR: ", round(b$q25, 0), "\u2013", round(b$q75, 0), ")."),
          if (s$status == "CONFIRMED" && remaining > 0)
            p(class = "mb-0 mt-1",
              "Signal active ~", strong(paste0(months_active, " months")),
              ". Precedent suggests ~", remaining, " more months to FDA action.")
          else if (s$status == "CONFIRMED" && remaining <= 0)
            p(class = "mb-0 mt-1 text-danger",
              "Signal active ~", strong(paste0(months_active, " months")),
              " — exceeds the class median. Review label currency urgently.")
          else
            p(class = "mb-0 mt-1 text-muted", style = "font-size:0.85rem;",
              "Signal is early-stage. Most reference drugs required 2\u20134 confirmed quarters before FDA acted.")
        )
      } else {
        # No class match — show signal status without benchmark
        p(class = "mb-1",
          if (s$status == "CONFIRMED")
            "A disproportionality signal has been confirmed for this drug/AE pair."
          else
            "An emerging disproportionality signal has been detected for this drug/AE pair.")
      }
    }
  })

  output$rec_text <- renderUI({
    s <- monitor_stats()
    div(style = "font-size:0.88rem;",
      switch(s$status,
        "CONFIRMED" = tagList(
          strong(class = "text-danger", "Recommended next steps:"),
          tags$ol(class = "mb-0 mt-1 ps-3",
            tags$li("Open a formal signal evaluation per ICH E2D/EMA GVP Module IX."),
            tags$li("Assess causality and clinical impact; document in your signal management system."),
            tags$li("Determine whether label update (W&P, Boxed Warning, or AR table) is warranted."),
            tags$li("Include in upcoming PSUR/PBRER with benefit-risk reassessment.")
          )
        ),
        "EMERGING" = tagList(
          strong(class = "text-warning", "Recommended next steps:"),
          tags$ol(class = "mb-0 mt-1 ps-3",
            tags$li("Flag in your signal tracking log; set a review trigger for next quarter."),
            tags$li("Pull full case narratives for the reporting quarters to assess clinical plausibility."),
            tags$li("Do not file a label change yet — confirm signal over 2+ quarters before escalating.")
          )
        ),
        "NOT DETECTED" = tagList(
          strong(class = "text-success", "Recommended next steps:"),
          tags$ol(class = "mb-0 mt-1 ps-3",
            tags$li("Continue scheduled FAERS surveillance (quarterly or per your SOP)."),
            tags$li("Document 'No signal detected' in your pharmacovigilance system for this period."),
            tags$li("Re-run this check after the next FAERS data refresh (~3 months).")
          )
        ),
        "INSUFFICIENT DATA" = tagList(
          strong(class = "text-secondary", "Recommended next steps:"),
          tags$ol(class = "mb-0 mt-1 ps-3",
            tags$li("Search by generic name (e.g. 'semaglutide' instead of 'OZEMPIC')."),
            tags$li("Check the openFDA FAERS database directly to confirm how this drug is reported."),
            tags$li("Consider whether the AE term is at the correct MedDRA PT level — broader terms have more reports.")
          )
        )
      )
    )
  })

  output$live_chart <- renderPlot({
    req(input$run_check > 0)
    df <- live_data()
    req(nrow(df) > 0)

    # If no reports found at all, show informative blank chart
    if (all(df$count_a == 0, na.rm = TRUE)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste0("No FAERS reports found for this drug/AE combination.\n",
                                  "Check the brand name spelling or try the generic name."),
                   size = 5, color = "grey40", hjust = 0.5, vjust = 0.5) +
          theme_void()
      )
    }

    prr_max <- max(df$PRR, na.rm = TRUE)
    if (!is.finite(prr_max) || prr_max == 0) prr_max <- 2.5
    y_max <- max(prr_max * 1.15, 3)

    ggplot(df, aes(x = quarter, y = PRR)) +
      # 95% CI ribbon
      geom_ribbon(aes(ymin = pmax(PRR_lo, 0), ymax = pmin(PRR_hi, y_max)),
                  fill = "#1a3a6b", alpha = 0.12, na.rm = TRUE) +
      # CI lower bound > 1 reference line
      geom_hline(yintercept = 1, linetype = "dotted",
                 color = "grey50", linewidth = 0.6) +
      annotate("label",
        x = min(df$quarter), y = 1,
        label = "CI lower bound = 1", hjust = 0,
        vjust = 1.4,
        color = "grey50", fill = "white", label.size = NA,
        size = 2.8, fontface = "italic"
      ) +
      # Threshold line
      geom_hline(yintercept = 2, linetype = "dashed",
                 color = "darkorange", linewidth = 0.9) +
      annotate("label",
        x = min(df$quarter), y = 2,
        label = "PRR = 2  (detection threshold)", hjust = 0,
        vjust = if (prr_max >= 2) -0.4 else 1.4,
        color = "darkorange", fill = "white", label.size = NA,
        size = 3.3, fontface = "bold"
      ) +
      # PRR line
      geom_line(color = "#1a3a6b", linewidth = 1.4, na.rm = TRUE) +
      # Points coloured by signal status
      geom_point(aes(color = signal_met), size = 3.5, na.rm = TRUE) +
      scale_color_manual(
        values = c("FALSE" = "#6b7280", "TRUE" = "#e05c00"),
        labels = c("FALSE" = "Criteria not met", "TRUE" = "Signal criteria met"),
        na.value = "#6b7280", name = NULL
      ) +
      scale_y_continuous(name = "PRR", limits = c(0, y_max)) +
      scale_x_date(labels = function(d) fmt_quarter(d), date_breaks = "6 months") +
      labs(x = NULL,
           caption = paste0("Shaded band = 95% CI  \u2014  ",
                            "Signal = PRR \u2265 2, CI lower > 1, n \u2265 3, \u03c7\u00b2 \u2265 4")) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position  = "bottom",
        axis.text.x      = element_text(angle = 30, hjust = 1),
        panel.grid.minor = element_blank()
      )
  })

  output$raw_quarterly_table <- DT::renderDT({
    req(input$run_check > 0)
    df <- live_data()
    df |>
      mutate(
        Quarter    = fmt_quarter(quarter),
        `Drug+AE (a)` = count_a,
        `Drug only (b)` = count_b,
        `AE only (c)` = count_c,
        `All other (d)` = count_d,
        PRR        = round(PRR, 2),
        `PRR CI low`  = round(PRR_lo, 2),
        `PRR CI high` = round(PRR_hi, 2),
        `Chi-sq`   = round(chi_sq, 2),
        Signal     = ifelse(signal_met, "YES", "no")
      ) |>
      select(Quarter, `Drug+AE (a)`, `Drug only (b)`, `AE only (c)`, `All other (d)`,
             PRR, `PRR CI low`, `PRR CI high`, `Chi-sq`, Signal) |>
      DT::datatable(options = list(paging = FALSE, searching = FALSE, info = FALSE,
                                   scrollX = TRUE, ordering = FALSE,
                                   autoWidth = FALSE),
                    rownames = FALSE, style = "bootstrap4")
  })
  outputOptions(output, "raw_quarterly_table", suspendWhenHidden = FALSE)

  # ── Regulatory Timeline Intelligence ──────────────────────────────────────

  timeline_data <- reactive({
    req(input$run_check > 0)
    s <- monitor_stats()
    b <- bench_filtered()
    months_active <- s$months_first
    predict_timeline(months_active, b$class, benchmark_drugs)
  })

  # Conditional timeline card — only show when the AE is NOT already on the drug's label
  output$timeline_card <- renderUI({
    req(input$run_check > 0)
    already <- has_existing_action()
    if (already) return(NULL)

    # Check if the AE is already mentioned in the drug's FDA label
    drug_upper <- toupper(trimws(isolate(input$live_drug)))
    ae_lower   <- tolower(trimws(isolate(input$live_ae)))
    if (check_label_covers_ae(drug_upper, ae_lower)) return(NULL)

    # Only show historical comparison when the drug maps to a known therapeutic class
    b <- bench_filtered()
    if (is.null(b$class)) return(NULL)

    card(
      card_header(icon("clock"), " Historical Context — How Similar Drugs Played Out"),
      card_body(
        uiOutput("timeline_summary"),
        plotOutput("timeline_plot", height = "180px"),
        div(class = "text-muted mt-1", style = "font-size:0.75rem;",
          "Each dot represents a drug where a FAERS signal eventually led to an FDA label change. ",
          "The yellow band shows the historical IQR. Your drug's current signal duration is marked in red.")
      )
    )
  })

  output$timeline_summary <- renderUI({
    s <- monitor_stats()

    drug_upper <- toupper(trimws(isolate(input$live_drug)))
    ae_lower   <- tolower(trimws(isolate(input$live_ae)))
    already_match <- find_cohort_match(drug_upper, ae_lower)
    already_acted <- nrow(already_match) > 0

    if (s$status %in% c("NOT DETECTED", "INSUFFICIENT DATA")) {
      return(div(class = "text-muted py-3 text-center",
               icon("clock", style = "font-size:1.5rem; color:#adb5bd;"),
               p(class = "mt-2 mb-0",
                 "Historical comparison activates when a signal is confirmed or emerging.",
                 br(), "Run a query that produces a signal to see how it compares to past drugs.")))
    }
    tl <- timeline_data()
    months_active <- tl$months_active

    risk_color <- switch(tl$risk,
      "OVERDUE" = "danger", "EXPECTED WINDOW" = "warning",
      "APPROACHING" = "info", "EARLY" = "secondary"
    )

    # Contextual plain-English explanation
    explainer <- switch(tl$risk,
      "OVERDUE" = paste0(
        "This signal has been active for ", months_active, " months — longer than ",
        tl$pct_at, "% of similar ", tl$ref_class, " drugs at the time FDA acted on them."),
      "EXPECTED WINDOW" = paste0(
        "At ", months_active, " months, this signal falls within the window ",
        "where FDA historically acted on similar ", tl$ref_class, " drugs ",
        "(IQR: ", round(tl$q25), "–", round(tl$q75), " months)."),
      "APPROACHING" = paste0(
        "This signal has been active for ", months_active, " months. ",
        "Historically, FDA action on ", tl$ref_class, " drugs began around ",
        round(tl$q25), " months from signal detection."),
      "EARLY" = paste0(
        "At ", months_active, " months, this signal is relatively early. ",
        "For reference, ", tl$ref_class, " drugs historically had a median of ",
        tl$median_lag, " months between signal detection and label change.")
    )

    # Override explainer if FDA already acted
    if (already_acted) {
      m <- already_match[1, ]
      explainer <- paste0(
        "Note: FDA already issued a \"", m$label_change_type, "\" for ",
        m$drug_name, " / ", m$adverse_event, " on ",
        format(m$label_change_date, "%B %d, %Y"),
        ". The timeline below shows how this compares to other ",
        tl$ref_class, " drugs historically.")
    }

    streak_months <- s$n_active * 3

    tagList(
      layout_columns(
        fill = FALSE, col_widths = c(3, 3, 3, 3),
        value_box(
          title = "Signal Duration",
          value = paste0(months_active, " months"),
          showcase = icon("clock"),
          showcase_layout = showcase_left_center(width = 0.15),
          theme = risk_color,
          p(style = "font-size:0.78rem; margin:0;",
            "Since first signal detection")
        ),
        value_box(
          title = "Current Streak",
          value = if (streak_months == 0) "None" else paste0(streak_months, " months"),
          showcase = icon("arrow-trend-up"),
          showcase_layout = showcase_left_center(width = 0.15),
          theme = if (streak_months == 0) "light"
                  else if (streak_months >= 12) "danger"
                  else if (streak_months >= 6) "dark"
                  else "info",
          p(style = "font-size:0.78rem; margin:0;",
            if (streak_months == 0) "Signal not active in latest quarter"
            else paste0(s$n_active, " consecutive quarter", if (s$n_active != 1) "s"))
        ),
        value_box(
          title = "Historical Median",
          value = paste0(tl$median_lag, " months"),
          showcase = icon("chart-line"),
          showcase_layout = showcase_left_center(width = 0.15),
          theme = "primary",
          p(style = "font-size:0.78rem; margin:0;",
            "IQR: ", round(tl$q25), "–", round(tl$q75), " mo for ", tl$ref_class)
        ),
        value_box(
          title = "Historical Precedent",
          value = paste0(tl$pct_at, "%"),
          showcase = icon("chart-bar"),
          showcase_layout = showcase_left_center(width = 0.15),
          theme = "secondary",
          p(style = "font-size:0.78rem; margin:0;",
            "of ", tl$n_ref, " ", tl$ref_class, " drugs had changes by this point")
        )
      ),
      div(class = "mt-2 p-2", style = "background:#f8f9fa; border-radius:6px; font-size:0.85rem;",
        icon("lightbulb", style = "color:#d97706; margin-right:4px;"),
        explainer
      )
    )
  })

  output$timeline_plot <- renderPlot({
    s <- monitor_stats()
    if (s$status %in% c("NOT DETECTED", "INSUFFICIENT DATA")) return(NULL)
    tl <- timeline_data()
    b  <- bench_filtered()
    ref <- b$df
    months_active <- tl$months_active
    x_max <- max(c(ref$lag_months, months_active) * 1.15, 24)

    # Dodge median vs "now" labels when close
    too_close <- abs(tl$median_lag - months_active) < (x_max * 0.12)
    med_hjust <- if (too_close && months_active <= tl$median_lag) 1.1 else -0.1
    now_hjust <- if (too_close && months_active > tl$median_lag) 1.1 else -0.1
    med_y <- if (too_close) 1.15 else 1.05
    now_y <- if (too_close) 0.95 else 1.05

    p <- ggplot(ref, aes(x = lag_months, y = 0.5)) +
      # IQR band
      annotate("rect", xmin = tl$q25, xmax = tl$q75, ymin = 0.15, ymax = 0.85,
               fill = "#fde68a", alpha = 0.5) +
      # Median line
      geom_vline(xintercept = tl$median_lag, color = "#d97706",
                 linetype = "solid", linewidth = 0.9) +
      annotate("text", x = tl$median_lag, y = med_y,
               label = paste0("Median: ", tl$median_lag, " mo"),
               hjust = med_hjust, color = "#d97706", size = 3.2, fontface = "bold") +
      # Reference drug dots
      geom_point(size = 5, color = "#6366f1", alpha = 0.8) +
      ggrepel::geom_text_repel(
        aes(label = drug_name), size = 2.6, color = "grey30",
        nudge_y = 0.25, segment.size = 0.25, segment.color = "grey60",
        max.overlaps = 20, seed = 42
      ) +
      # "You are here" marker
      geom_vline(xintercept = months_active, color = "#dc2626",
                 linetype = "dotted", linewidth = 1.1) +
      annotate("text", x = months_active, y = now_y,
               label = paste0("You: ", months_active, " mo"),
               hjust = now_hjust, color = "#dc2626", size = 3.2, fontface = "bold") +
      scale_x_continuous(
        name = "Months from signal to FDA label action",
        limits = c(0, x_max),
        breaks = seq(0, 200, by = 12),
        labels = function(x) paste0(x, " mo")
      ) +
      scale_y_continuous(limits = c(-0.1, 1.4)) +
      labs(y = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank()
      )
    p
  })


  # ── Tab 2: Reference Cohort ────────────────────────────────────────────────

  # Update drug dropdown when class/signal filters change
  observe({
    filtered <- combined
    if (nchar(input$class_filter) > 0) {
      filtered <- filtered |> filter(therapeutic_class == input$class_filter)
    }
    if (nchar(input$signal_filter) > 0) {
      if (input$signal_filter == "yes") {
        filtered <- filtered |> filter(!is.na(signal_start_quarter))
      } else {
        filtered <- filtered |> filter(is.na(signal_start_quarter))
      }
    }
    choices <- sort(toupper(filtered$drug_name))
    current <- isolate(input$drug_select)
    selected <- if (current %in% choices) current else choices[1]
    updateSelectInput(session, "drug_select", choices = choices, selected = selected)
  })

  output$prr_trend <- renderPlot({
    req(input$drug_select)

    drug_signals <- signals |>
      filter(drug == input$drug_select, !is.na(PRR), is.finite(PRR))

    drug_meta    <- combined |> filter(toupper(drug_name) == input$drug_select)
    label_date   <- drug_meta$label_change_date[1]
    signal_date  <- drug_meta$signal_start_date[1]
    pt_label     <- drug_meta$adverse_event[1]
    generic_name <- drug_meta$generic_name[1]

    prr_max   <- max(drug_signals$PRR,     na.rm = TRUE)
    count_max <- max(drug_signals$count_a, na.rm = TRUE)
    if (!is.finite(prr_max) || prr_max == 0) prr_max <- 2.5
    if (count_max == 0) count_max <- 1

    sf       <- count_max / max(prr_max, 2.5)
    thresh_y <- 2 * sf

    drug_signals <- drug_signals |>
      mutate(period = case_when(
        !is.na(label_date)  & quarter >= label_date  ~ "Post-label update",
        !is.na(signal_date) & quarter >= signal_date ~ "Post-signal / pre-label",
        TRUE                                          ~ "Pre-signal"
      ))

    p <- ggplot(drug_signals, aes(x = quarter)) +
      geom_col(aes(y = count_a, fill = period), width = 70, alpha = 0.80) +
      scale_fill_manual(
        values = c("Pre-signal"              = "#90b8e0",
                   "Post-signal / pre-label" = "#e8501a",
                   "Post-label update"       = "#2ca02c"),
        name = NULL
      ) +
      geom_line(aes(y = PRR * sf), color = "#e05c00", linewidth = 0.9) +
      geom_point(aes(y = PRR * sf), color = "#e05c00", size = 1.8) +
      geom_hline(yintercept = thresh_y, linetype = "dashed",
                 color = "darkorange", linewidth = 0.8) +
      scale_y_continuous(
        name     = "Quarterly report count",
        sec.axis = sec_axis(~ . / sf, name = "PRR")
      ) +
      labs(x = NULL) +
      theme_minimal(base_size = 14) +
      theme(
        axis.title.y.left  = element_text(color = "#2e75b6"),
        axis.title.y.right = element_text(color = "#e05c00"),
        legend.position    = "bottom",
        legend.title       = element_blank(),
        panel.grid.minor   = element_blank()
      )

    date_range    <- range(drug_signals$quarter)
    date_midpoint <- date_range[1] + (date_range[2] - date_range[1]) / 2

    if (!is.na(label_date)) {
      p <- p +
        geom_vline(xintercept = as.numeric(label_date),
                   linetype = "solid", color = "firebrick", linewidth = 1.2) +
        annotate("label",
          x = label_date, y = count_max * 0.92,
          label = paste0("Label change\n", format(label_date, "%b %Y")),
          hjust = -0.05, vjust = 1,
          color = "firebrick", fill = "white", label.size = NA,
          size = 3.5, fontface = "bold"
        )
    }

    if (!is.na(signal_date)) {
      sig_hjust <- if (signal_date < date_midpoint) -0.05 else 1.05
      sig_y     <- if (thresh_y > count_max * 0.45) count_max * 0.25 else count_max * 0.60
      p <- p +
        geom_vline(xintercept = as.numeric(signal_date),
                   linetype = "dotted", color = "darkgreen", linewidth = 1.2) +
        annotate("label",
          x = signal_date, y = sig_y,
          label = paste0("Signal confirmed\n", format(signal_date, "%b %Y")),
          hjust = sig_hjust, vjust = 0.5,
          color = "darkgreen", fill = "white", label.size = NA,
          size = 3.5, fontface = "bold"
        )
    }

    p
  })

  output$drug_info_box <- renderUI({
    req(input$drug_select)
    meta <- combined |> filter(toupper(drug_name) == input$drug_select)
    lag  <- meta$lag_months[1]
    tc   <- meta$therapeutic_class[1]
    ae   <- meta$adverse_event[1]
    lag_color <- if (is.na(lag)) "text-secondary"
                 else if (lag < 0) "text-danger"
                 else if (lag < 24) "text-success"
                 else "text-warning"

    # Detection limitation flag (reuses pre-computed helpers)
    det_type       <- get_detection_type(tc, ae)
    detection_note <- detection_alert(det_type)

    # Footnote for negative lag (signal came after label change)
    lag_footnote <- if (!is.na(lag) && lag < 0 && is.null(detection_note))
      tags$small(class="text-muted fst-italic",
        "\u2020 FDA acted before FAERS signal emerged")

    div(class = "mt-1",
      div(class = "mb-3",
        div(class = "fw-bold fs-5 lh-sm", meta$drug_name[1]),
        div(class = "text-muted fst-italic", style = "font-size:0.85rem;",
            meta$generic_name[1])
      ),
      tags$table(class = "table table-sm table-borderless mb-1",
        style = "font-size:0.85rem; table-layout:fixed; width:100%;",
        tags$colgroup(tags$col(style="width:42%;"), tags$col(style="width:58%;")),
        tags$tbody(
          tags$tr(tags$td(class="text-muted","Class"),
                  tags$td(class="fw-semibold", meta$therapeutic_class[1])),
          tags$tr(tags$td(class="text-muted","Approved"),
                  tags$td(class="fw-semibold", meta$approval_year[1])),
          tags$tr(
            style = "background-color:#f0f4ff;",
            tags$td(class="text-muted fw-semibold","AE tracked"),
            tags$td(class="fw-bold", style="color:#1a3a6b;",
                    tools::toTitleCase(meta$adverse_event[1]))
          ),
          tags$tr(tags$td(class="text-muted","Label change"),
                  tags$td(class="fw-semibold", format(meta$label_change_date[1], "%b %d, %Y"))),
          tags$tr(tags$td(class="text-muted","Change type"),
                  tags$td(class="fw-semibold", meta$label_change_type[1])),
          tags$tr(tags$td(class="text-muted","Signal lag"),
                  tags$td(class=paste("fw-bold", lag_color),
                    if (is.na(lag)) "No FAERS signal"
                    else if (lag < 0) paste0(abs(lag), " mo early\u2020")
                    else paste0(lag, " months")))
        )
      ),
      lag_footnote,
      detection_note
    )
  })


  # ── Tab 3: Drug Table ──────────────────────────────────────────────────────

  output$drug_table <- DT::renderDT({
    tbl <- combined |>
      mutate(
        Brand = ifelse(nchar(Note) > 0,
          paste0('<span style="display:inline-flex;align-items:center;white-space:nowrap;">',
                 htmltools::htmlEscape(drug_name),
                 '<span data-bs-toggle="tooltip" data-bs-placement="right" title="',
                 htmltools::htmlEscape(Note), '"',
                 ' style="cursor:pointer;color:#d97706;font-size:0.85rem;margin-left:5px;">',
                 '<i class="fa fa-circle-info"></i></span></span>'),
          drug_name)
      ) |>
      select(
        Brand,
        Generic          = generic_name,
        Class            = therapeutic_class,
        `Adverse Event`  = adverse_event,
        `Signal Quarter` = signal_start_quarter,
        `Label Change`   = label_change_date,
        `Change Type`    = label_change_type,
        `Lag (months)`   = lag_months
      )
    DT::datatable(tbl,
      escape = FALSE,
      options = list(
        pageLength = 10, scrollX = TRUE, autoWidth = FALSE,
        columnDefs = list(
          list(width = "90px", targets = c(4, 5)),
          list(width = "80px", targets = 7)
        ),
        initComplete = DT::JS(
          "function(settings, json) {",
          "  var tooltipTriggerList = [].slice.call(",
          "    document.querySelectorAll('[data-bs-toggle=\"tooltip\"]'));",
          "  tooltipTriggerList.map(function(el) {",
          "    return new bootstrap.Tooltip(el);",
          "  });",
          "}"
        ),
        drawCallback = DT::JS(
          "function(settings) {",
          "  var tooltipTriggerList = [].slice.call(",
          "    this.api().table().container().querySelectorAll('[data-bs-toggle=\"tooltip\"]'));",
          "  tooltipTriggerList.map(function(el) {",
          "    return new bootstrap.Tooltip(el);",
          "  });",
          "}"
        )
      ),
      rownames = FALSE, filter = "top", style = "bootstrap4")
  })
}

shinyApp(ui, server)
