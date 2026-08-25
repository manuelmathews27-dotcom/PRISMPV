# R/utils.R — Shared helpers for PRISM pipeline and Shiny app

library(jsonlite)
library(curl)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── openFDA API key ──────────────────────────────────────────────────────────
# Read from the OPENFDA_API_KEY env var. NEVER hardcode a key here — this repo
# is public. Unset is fine: every call below still works, just at the anonymous
# rate limit (~1,000 req/day per IP vs ~120,000/day with a key).
#
# Set it via a .env file (already gitignored) or on the container:
#   docker run -e OPENFDA_API_KEY=... ...
# Get a free key at https://open.fda.gov/apis/authentication/
openfda_api_key <- function() trimws(Sys.getenv("OPENFDA_API_KEY", ""))

# Append the key to a request URL at fetch time. Kept separate from the URL
# BUILDERS on purpose, so the key never becomes part of a cache key or a log line.
openfda_authed_url <- function(url) {
  key <- openfda_api_key()
  if (!nzchar(key)) return(url)
  paste0(url, if (grepl("?", url, fixed = TRUE)) "&" else "?", "api_key=", key)
}

# Strip the key before a URL is ever printed. fetch_total() logs a URL prefix on
# error, and this app runs in public — a leaked key would be in the container log.
redact_key <- function(url) sub("([?&]api_key=)[^&]*", "\\1REDACTED", url)

# ── Response cache ───────────────────────────────────────────────────────────
# Why this is safe: pull_live_signal() only ever queries quarters older than the
# ~6-month FAERS reporting lag (it stops at current_quarter - 9 months), so every
# FAERS count it fetches is for a CLOSED quarter and can never change. Those are
# cached with no expiry. Label lookups DO change as FDA updates labeling, so they
# take a TTL instead.
#
# Deliberately in-memory only. The container runs a single R process
# (`R -e shiny::runApp(...)`), so one cache is shared by every session and every
# user until restart — which is where most of the saving comes from, since the
# drug-independent counts (c and d below) are identical across all queries.
# A disk cache was considered and rejected: repo/data/ is tracked by git and
# watched by the every-minute auto-sync, so cache files would generate commits
# and trigger deploys.
.prism_cache     <- new.env(parent = emptyenv())
CACHE_MAX_ENTRIES <- 5000L   # bound memory on a public app

cache_get <- function(key, ttl_sec = Inf) {
  if (!exists(key, envir = .prism_cache, inherits = FALSE)) return(NULL)
  entry <- get(key, envir = .prism_cache, inherits = FALSE)
  if (is.finite(ttl_sec) &&
      as.numeric(difftime(Sys.time(), entry$at, units = "secs")) > ttl_sec) {
    rm(list = key, envir = .prism_cache)
    return(NULL)
  }
  entry$value
}

cache_set <- function(key, value) {
  # Cheap bound: once full, drop the oldest half rather than evicting per-write.
  if (length(ls(.prism_cache)) >= CACHE_MAX_ENTRIES) {
    keys <- ls(.prism_cache)
    ages <- vapply(keys, function(k) as.numeric(get(k, envir = .prism_cache)$at), numeric(1))
    rm(list = keys[order(ages)][seq_len(length(keys) %/% 2)], envir = .prism_cache)
  }
  assign(key, list(value = value, at = Sys.time()), envir = .prism_cache)
  invisible(value)
}

# NULL is never cached — a failed lookup must be retried, not remembered.
cached <- function(key, ttl_sec = Inf, compute) {
  hit <- cache_get(key, ttl_sec)
  if (!is.null(hit)) return(hit)
  val <- compute()
  if (!is.null(val)) cache_set(key, val)
  val
}

LABEL_CACHE_TTL <- 24 * 3600   # FDA labeling changes; re-check daily

# Signal detection thresholds (Evans criteria)
SIGNAL_MIN_REPORTS <- 3L
SIGNAL_MIN_PRR     <- 2
SIGNAL_MIN_CHISQ   <- 4

# ── Resolve brand ↔ generic names via openFDA label API ─────────────────────
# Returns a character vector of unique drug name tokens to search for.
# E.g. "LIPITOR" → c("LIPITOR", "ATORVASTATIN")
PHARMA_QUALIFIERS <- c(
  # Salt forms
  "CALCIUM", "SODIUM", "HYDROCHLORIDE", "HCL", "MESYLATE", "MALEATE",
  "FUMARATE", "BESYLATE", "TARTRATE", "SULFATE", "PHOSPHATE", "POTASSIUM",
  "MAGNESIUM", "CHLORIDE", "ACETATE", "SUCCINATE", "CITRATE", "BROMIDE",
  # Dosage forms & routes
  "ORAL", "INJECTABLE", "TABLET", "TABLETS", "CAPSULE", "CAPSULES",
  "FILM", "COATED", "EXTENDED", "RELEASE", "SOLUTION", "INJECTION",
  "OPHTHALMIC", "TOPICAL", "NASAL", "TRANSDERMAL", "PATCH", "CREAM",
  "GEL", "OINTMENT", "DROPS", "SPRAY", "MEDICATED", "SYSTEM",
  # Connectors & short words
  "AND", "FOR", "IN", "WITH", "OF", "MG", "ML", "USP")

# ── Generic name -> canonical single-ingredient token ────────────────────────
# Pure string logic, split out from resolve_drug_names so it is unit-testable
# without hitting openFDA (see tests/test_resolve_token.R). Returns NA when the
# name is not a clean single ingredient, which tells the caller to fall back to
# whatever the user typed.
#
# BUG FIXED 2026-08-24 — biologics resolved to a token matching ZERO FAERS rows.
# FDA requires a meaningless 4-letter suffix on biologic nonproprietary names
# ("tafasitamab-cxix"). The old line `gsub("[^A-Z ]", "", ...)` DELETED the
# hyphen, welding it into TAFASITAMABCXIX — one word, >2 chars, not a qualifier,
# so it passed every check and was returned as canonical. Measured against FAERS:
#   TAFASITAMABCXIX 0 reports   vs TAFASITAMAB 1267   (Monjuvi)
#   RETIFANLIMABDLWR 0          vs RETIFANLIMAB 87    (Zynyz)
#   AXATILIMABCSFR 0            vs AXATILIMAB 128     (Niktimvo)
# The app then showed "no signal" with no error — a lookup failure that reads as
# a clean negative. This affected every biologic licensed since 2017.
#
# Two changes: strip the trailing 4-letter suffix, and replace remaining
# non-letters with a SPACE rather than deleting them, so a hyphenated
# combination ("SACUBITRIL-VALSARTAN") splits into two words and correctly
# falls through instead of welding into one bogus token.
canonical_ingredient_token <- function(generic_name) {
  g <- toupper(trimws(generic_name))
  if (length(g) != 1 || is.na(g) || !nzchar(g)) return(NA_character_)
  g <- sub("-[A-Z]{4}$", "", g)          # drop FDA biologic suffix
  cleaned <- gsub("[^A-Z ]", " ", g)     # SPACE, not "" — keeps tokens separate
  w <- unlist(strsplit(trimws(cleaned), "\\s+"))
  w <- w[nchar(w) > 2 & !w %in% PHARMA_QUALIFIERS]
  if (length(w) == 1) w else NA_character_
}

resolve_drug_names <- function(drug_name) {
  original <- toupper(trimws(drug_name))
  # Cached: the same drug is resolved on every query, and this is 1 of the ~50
  # calls a single query makes. TTL'd because label data changes.
  #
  # The closure must distinguish two outcomes that both used to just fall back to
  # `original`, because now they get cached for a day:
  #   NULL          = transient failure (HTTP error / exception). NOT cached, so
  #                   an openFDA blip can't pin a drug as unresolved for 24h.
  #   NA_character_ = queried fine, genuinely no single-ingredient match (e.g. a
  #                   combination product). A stable fact, so it IS cached.
  # Both surface as `original` to the caller, preserving the old return contract.
  resolved <- cached(paste0("resolve:", original), ttl_sec = LABEL_CACHE_TTL,
                     compute = function() {
  tryCatch({
    dn <- URLencode(original, reserved = TRUE)
    url <- paste0(
      "https://api.fda.gov/drug/label.json?search=(openfda.brand_name:",
      dn, "+openfda.generic_name:", dn, ")&limit=5")
    h  <- curl::new_handle()
    curl::handle_setopt(h, timeout = 10L, connecttimeout = 5L)
    resp <- curl::curl_fetch_memory(openfda_authed_url(url), handle = h)
    if (resp$status_code != 200) return(NULL)
    body <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
    results <- body$results
    if (!is.list(results) || length(results) == 0) return(NA_character_)

    # Extract the canonical active ingredient from the first single-ingredient result.
    # This is the standardized generic name (e.g. ASPIRIN, ATORVASTATIN, SEMAGLUTIDE).
    for (r in results) {
      openfda <- r$openfda
      if (is.null(openfda)) next
      gn <- toupper(trimws(unlist(openfda$generic_name)))
      if (is.null(gn) || length(gn) == 0) next
      # Skip combos
      if (any(grepl(" AND |;|/|,", gn))) next
      tok <- canonical_ingredient_token(gn[1])
      if (!is.na(tok)) return(tok)
    }
    NA_character_          # queried OK, no single-ingredient match
  }, error = function(e) NULL)   # transient — don't cache
  })
  if (is.null(resolved) || all(is.na(resolved))) original else resolved
}

# ── openFDA query URL builder ────────────────────────────────────────────────
# Multi-word values MUST be wrapped in %22 (a URL-encoded double quote) or Lucene
# treats them as separate tokens: `reactionmeddrapt:TENDON PAIN` parses as
# `reactionmeddrapt:TENDON` OR a free-text match on `PAIN` across the whole record.
# Measured inflation from the unquoted form (2026-08-25):
#   tendon pain           3,561,634 vs     7,475 exact  (477x)
#   hepatic failure         929,971 vs    43,799        ( 21x)
#   acute kidney injury     901,197 vs   150,318        (  6x)
#   herpes zoster           225,468 vs    60,592        (3.7x)
# Single-word terms are identical either way, which is why this went unnoticed --
# but ~70 of the 110 curated PT terms are multi-word. The inflation hits count_a
# and count_c together so it partly cancels in the PRR ratio, but not cleanly (the
# factor differs per drug), and chi-squared uses the raw cells, so it is inflated
# outright. Drug names are quoted for the same reason ("certolizumab pegol").
quote_term <- function(x) paste0("%22", gsub(" ", "+", x), "%22")

build_url <- function(drug_name = NULL, pt_term = NULL, q_start, q_end) {
  parts <- c()
  if (!is.null(drug_name)) {
    dn <- quote_term(toupper(drug_name))
    parts <- c(parts, paste0(
      "(patient.drug.medicinalproduct:", dn,
      "+patient.drug.openfda.brand_name:", dn,
      "+patient.drug.openfda.generic_name:", dn, ")"))
  }
  if (!is.null(pt_term))
    parts <- c(parts, paste0("patient.reaction.reactionmeddrapt:", quote_term(pt_term)))
  parts <- c(parts, paste0("receivedate:[", q_start, "+TO+", q_end, "]"))
  paste0("https://api.fda.gov/drug/event.json?search=",
         paste(parts, collapse = "+AND+"), "&limit=1")
}

# ── Fetch total count for one openFDA query (synchronous, with logging) ──────
# Cached on the un-keyed URL. Counts for closed quarters are immutable, so a hit
# is always valid. NA (a failure) is not cached — `cached()` skips NULL, and the
# NA path returns NULL to it explicitly.
fetch_total <- function(url) {
  ck  <- paste0("count:", url)
  hit <- cache_get(ck)
  if (!is.null(hit)) return(hit)
  # NOTE: no `return()` inside the tryCatch block. In R, `return()` there exits
  # the ENCLOSING FUNCTION, not the block — which would jump straight past the
  # cache_set below. openFDA answers 404 for a zero-result search, and that is a
  # hot path for sparse drug/AE pairs, so it must reach the cache like any other
  # count. if/else yielding a value keeps every branch flowing to one exit.
  val <- tryCatch({
    h <- curl::new_handle()
    curl::handle_setopt(h, timeout = 15L, connecttimeout = 10L)
    resp <- curl::curl_fetch_memory(openfda_authed_url(url), handle = h)
    if (resp$status_code == 404) {
      0L
    } else if (resp$status_code != 200) {
      message("[FAERS] HTTP ", resp$status_code, " for ", substr(redact_key(url), 1, 120))
      NA_integer_
    } else {
      parsed <- fromJSON(rawToChar(resp$content))
      parsed$meta$results$total %||% 0L
    }
  }, error = function(e) {
    message("[FAERS] Error: ", conditionMessage(e), " — URL: ",
            substr(redact_key(url), 1, 120))
    NA_integer_
  })
  if (!is.na(val)) cache_set(ck, val)   # failures stay uncached and get retried
  val
}

# ── Parse a curl multi response ──────────────────────────────────────────────
parse_multi_resp <- function(resp) {
  if (is.null(resp)) return(NA_integer_)
  if (resp$status_code == 404) return(0L)
  if (resp$status_code != 200) {
    message("[FAERS] HTTP ", resp$status_code, " in batch response")
    return(NA_integer_)
  }
  tryCatch({
    parsed <- fromJSON(rawToChar(resp$content))
    parsed$meta$results$total %||% 0L
  }, error = function(e) {
    message("[FAERS] Parse error: ", conditionMessage(e))
    NA_integer_
  })
}

# ── Compute PRR + CI + Yates-corrected Pearson chi-squared ──────────────────
# Inputs are openFDA marginals, NOT the four 2x2 cells:
#   count_a = drug X AND event Y         = a
#   count_b = drug X, any event          = a + b   (row marginal)
#   count_c = event Y, any drug          = a + c   (column marginal)
#   count_d = all reports in period      = N       (grand total)
#
# Textbook PRR requires the comparator "other drugs". Reconstruct:
#   c_cell  = count_c - count_a          (event in OTHER drugs)
#   cd_cell = count_d - count_b          (OTHER drugs total)
#
# PRR   = (a / (a+b)) / (c / (c+d))
# log-SE = sqrt(1/a - 1/(a+b) + 1/c - 1/(c+d))   [Rothman]
# chi^2 = N * (|ad - bc| - N/2)^2 / ((a+b)(c+d)(a+c)(b+d))   [Pearson w/ Yates]
#         collapses to marginals because (ad - bc) = count_a*count_d - count_b*count_c
compute_prr <- function(df) {
  df |>
    dplyr::mutate(
      c_cell  = count_c - count_a,                 # event in other drugs
      cd_cell = count_d - count_b,                 # other-drug total (c + d)
      bd_cell = count_d - count_c,                 # non-event total  (b + d)

      # Degenerate-cell guard: any required marginal/cell at zero makes PRR undefined.
      ok = count_a  > 0 & count_b  > 0 & count_c  > 0 & count_d  > 0 &
           c_cell  > 0 & cd_cell > 0 & bd_cell > 0,

      PRR = ifelse(ok, (count_a / count_b) / (c_cell / cd_cell), NA_real_),

      PRR_log_se = ifelse(ok,
                          sqrt(1/count_a - 1/count_b + 1/c_cell - 1/cd_cell),
                          NA_real_),
      PRR_lo = ifelse(ok, exp(log(PRR) - 1.96 * PRR_log_se), NA_real_),
      PRR_hi = ifelse(ok, exp(log(PRR) + 1.96 * PRR_log_se), NA_real_),

      # Full Pearson chi-squared with Yates continuity correction (Evans criterion form)
      # Cast to numeric to avoid integer overflow on large FAERS counts.
      chi_sq_num  = pmax(abs(as.numeric(count_a) * as.numeric(count_d) -
                              as.numeric(count_b) * as.numeric(count_c)) -
                          as.numeric(count_d) / 2, 0)^2,
      chi_sq_den  = as.numeric(count_b) * as.numeric(cd_cell) *
                    as.numeric(count_c) * as.numeric(bd_cell),
      chi_sq = ifelse(ok & chi_sq_den > 0,
                      as.numeric(count_d) * chi_sq_num / chi_sq_den, NA_real_)
    ) |>
    dplyr::select(-ok, -chi_sq_num, -chi_sq_den)
}

# ── Check if signal criteria are met ─────────────────────────────────────────
# Uses PRR lower 95% CI bound > 1 as the primary disproportionality gate,
# combined with Evans criteria (PRR >= 2, chi-sq >= 4, n >= 3).
check_signal <- function(count_a, PRR, chi_sq, PRR_lo = NA_real_) {
  base <- !is.na(count_a) & count_a >= SIGNAL_MIN_REPORTS &
          !is.na(PRR)     & PRR     >= SIGNAL_MIN_PRR &
          !is.na(chi_sq)  & chi_sq  >= SIGNAL_MIN_CHISQ
  # If CI available, additionally require lower bound > 1
  ci_ok <- is.na(PRR_lo) | PRR_lo > 1
  base & ci_ok
}

# ── Audit trail logging ─────────────────────────────────────────────────────
# Appends one row per query to data/audit_log.csv for ICH E2E / GVP IX traceability.
AUDIT_LOG_PATH <- "data/audit_log.csv"

write_audit_log <- function(drug, ae, status, current_prr, prr_lo, prr_hi,
                            n_reports, quarters_queried, session_id = "") {
  entry <- data.frame(
    timestamp         = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    session_id        = session_id,
    drug_queried      = drug,
    ae_queried        = ae,
    signal_status     = status,
    prr               = round(current_prr, 4),
    prr_ci_lo         = round(prr_lo, 4),
    prr_ci_hi         = round(prr_hi, 4),
    total_reports     = n_reports,
    quarters_queried  = quarters_queried,
    stringsAsFactors  = FALSE
  )
  write_header <- !file.exists(AUDIT_LOG_PATH)
  tryCatch(
    write.table(entry, AUDIT_LOG_PATH, append = TRUE, sep = ",",
                row.names = FALSE, col.names = write_header, quote = TRUE),
    error = function(e) message("[AUDIT] Failed to write log: ", conditionMessage(e))
  )
}
