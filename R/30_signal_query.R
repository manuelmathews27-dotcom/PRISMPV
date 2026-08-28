# signal_query.R — live openFDA querying and signal classification
# The Monitor tab's data path: label lookups, BBW/label-coverage checks, AE term
# expansion, the parallel quarterly FAERS pull, and signal status derivation.
# Sourced automatically by Shiny before app.R (all files in R/ are).

# ── Shared helpers ───────────────────────────────────────────────────────────
# NOTE: no source("R/utils.R") here. Shiny sources every file in R/ automatically,
# in alphabetical order, before app.R — the 00_/10_/... prefixes make that order
# explicit rather than incidental. utils is 00_ because 10_cohort_data.R calls
# compute_prr() at load time.

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
  # British PT spelling vs American label spelling.
  "spinal cord haematoma"      = c("spinal/epidural hematoma", "spinal hematoma",
                                   "epidural hematoma", "spinal haematoma",
                                   "epidural haematoma", "paralysis"),
  "spinal epidural haematoma"  = c("spinal/epidural hematoma", "epidural hematoma",
                                   "spinal hematoma", "epidural haematoma",
                                   "spinal haematoma", "paralysis"),
  # The GLP-1 boxed warning never says "cancer" -- it says thyroid C-cell
  # tumors / medullary thyroid carcinoma (which is not itself a MedDRA PT).
  "thyroid cancer"             = c("thyroid c-cell", "c-cell tumor",
                                   "medullary thyroid carcinoma", "thyroid carcinoma",
                                   "thyroid tumor", "thyroid tumour"),
  # FDA labels write "peripheral neuropathy"; the MedDRA PT inverts it. Without
  # this the label-coverage check leans on the word-split fallback alone.
  "neuropathy peripheral"      = c("peripheral neuropathy", "polyneuropathy",
                                   "nerve damage", "paraesthesia"),
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
# Brand -> generic, built from the cohort we already ship. This is the ONLY
# reliable route for DISCONTINUED brands: openFDA has no current label for them,
# so the label endpoint 404s and every automated fallback fails too (verified
# 2026-08-25 -- drug/ndc returns no generic_name, and deriving it from FAERS
# returns concomitant meds like prednisone, not the ingredient).
cohort_generic_map <- setNames(
  tolower(trimws(combined$generic_name)),
  toupper(trimws(combined$drug_name))
)

# Pick an alternative name to retry a label lookup with, or NULL if there is none.
label_fallback_name <- function(drug_name) {
  up <- toupper(trimws(drug_name))
  gen <- unname(cohort_generic_map[up])
  if (!is.na(gen) && nzchar(gen) && toupper(gen) != up) return(gen)
  canon <- resolve_drug_names(drug_name)          # works when a label exists
  if (length(canon) == 1 && toupper(canon) != up) return(canon)
  NULL
}

fetch_label_results <- function(drug_name) {
  res <- fetch_label_results_one(drug_name)
  if (!is.null(res)) return(res)
  # BUG FIXED 2026-08-25 -- both check_boxed_warning() and check_label_covers_ae()
  # are handed the RAW user input. LEVAQUIN returns HTTP 404 (brand discontinued,
  # no current label), so has_bbw came back FALSE and the app called the
  # fluoroquinolone peripheral-neuropathy boxed warning an EMERGING signal --
  # a decades-old BBW presented as new, the worst direction for a safety tool.
  # LEVOFLOXACIN returns it immediately. Same false negative hit COUMADIN
  # (warfarin) and AVANDIA (rosiglitazone).
  alt <- label_fallback_name(drug_name)
  if (is.null(alt)) return(NULL)
  fetch_label_results_one(alt)
}

fetch_label_results_one <- function(drug_name) {
  dn <- URLencode(drug_name, reserved = TRUE)
  url <- paste0(
    "https://api.fda.gov/drug/label.json?search=(openfda.brand_name:",
    dn, "+openfda.generic_name:", dn, ")&limit=5"
  )
  # TTL cache: label text changes when FDA updates it, so don't cache forever.
  # Called repeatedly per query (BBW check + on-label AE check), so even a
  # short-lived cache removes duplicate calls within a single user request.
  cached(paste0("label:", toupper(drug_name)), ttl_sec = LABEL_CACHE_TTL, compute = function() {
  tryCatch({
    h <- curl::new_handle()
    curl::handle_setopt(h, timeout = 10L)
    resp <- curl::curl_fetch_memory(openfda_authed_url(url), handle = h)
    if (resp$status_code != 200) return(NULL)
    body <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
    results <- body$results
    if (!is.list(results) || length(results) == 0) return(NULL)
    results
  }, error = function(e) NULL)
  })
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

  # Fire all requests in parallel using curl's async multi pool, but only for the
  # URLs we don't already have. This is the hot path: 4 calls per quarter x 12
  # quarters = ~48 requests per query. Two of the four (c = event across all
  # drugs, d = all reports) don't depend on the drug at all, so they are shared
  # by every user's every query — after the first query of a session those are
  # always hits. Counts are for closed quarters only, so hits never go stale.
  pool       <- curl::new_pool(total_con = 12, host_con = 6)
  counts_env <- new.env(parent = emptyenv())   # tag -> integer count
  n_hit <- 0L; n_miss <- 0L

  for (i in seq_along(urls)) {
    for (key in names(urls[[i]])) {
      tag <- paste0(i, "_", key)
      url <- urls[[i]][[key]]
      hit <- cache_get(paste0("count:", url))
      if (!is.null(hit)) {
        counts_env[[tag]] <- hit
        n_hit <- n_hit + 1L
        next
      }
      n_miss <- n_miss + 1L
      local({
        tag_ <- tag; url_ <- url
        h <- curl::new_handle()
        curl::handle_setopt(h, timeout = 15L, connecttimeout = 10L)
        curl::curl_fetch_multi(
          openfda_authed_url(url_),
          handle = h,
          done = function(resp) {
            val <- parse_multi_resp(resp)
            counts_env[[tag_]] <- val
            # Never cache a failure — NA must be retried on the next query.
            if (!is.na(val)) cache_set(paste0("count:", url_), val)
          },
          fail = function(msg) { counts_env[[tag_]] <- NA_integer_ },
          pool = pool
        )
      })
    }
  }

  if (!is.null(progress_cb)) {
    progress_cb(value = 0.35,
                detail = sprintf("fetching %d of %d quarters-worth (%d cached)",
                                 n_miss, n_hit + n_miss, n_hit))
  }

  if (n_miss > 0) curl::multi_run(pool = pool)

  if (!is.null(progress_cb)) progress_cb(value = 0.9, detail = "90% — parsing results")

  results <- vector("list", n_total)
  for (i in seq_along(quarters)) {
    get_count <- function(tag) {
      if (exists(tag, envir = counts_env, inherits = FALSE)) counts_env[[tag]] else NA_integer_
    }
    ca <- get_count(paste0(i, "_a"))
    cb <- pmax(get_count(paste0(i, "_b")), 1)
    cc <- pmax(get_count(paste0(i, "_c")), 1)
    cd <- pmax(get_count(paste0(i, "_d")), 1)
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
