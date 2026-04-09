# R/utils.R — Shared helpers for PRISM pipeline and Shiny app

library(jsonlite)
library(curl)

`%||%` <- function(a, b) if (!is.null(a)) a else b

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

resolve_drug_names <- function(drug_name) {
  original <- toupper(trimws(drug_name))
  tryCatch({
    dn <- URLencode(original, reserved = TRUE)
    url <- paste0(
      "https://api.fda.gov/drug/label.json?search=(openfda.brand_name:",
      dn, "+openfda.generic_name:", dn, ")&limit=5")
    h  <- curl::new_handle()
    curl::handle_setopt(h, timeout = 10L, connecttimeout = 5L)
    resp <- curl::curl_fetch_memory(url, handle = h)
    if (resp$status_code != 200) return(original)
    body <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
    results <- body$results
    if (!is.list(results) || length(results) == 0) return(original)

    # Extract the canonical active ingredient from the first single-ingredient result.
    # This is the standardized generic name (e.g. ASPIRIN, ATORVASTATIN, SEMAGLUTIDE).
    for (r in results) {
      openfda <- r$openfda
      if (is.null(openfda)) next
      gn <- toupper(trimws(unlist(openfda$generic_name)))
      if (is.null(gn) || length(gn) == 0) next
      # Skip combos
      if (any(grepl(" AND |;|/|,", gn))) next
      cleaned <- gsub("[^A-Z ]", "", gn[1])
      w <- unlist(strsplit(cleaned, "\\s+"))
      w <- w[nchar(w) > 2 & !w %in% PHARMA_QUALIFIERS]
      if (length(w) == 1) return(w)
    }
    original
  }, error = function(e) original)
}

# ── openFDA query URL builder ────────────────────────────────────────────────
build_url <- function(drug_name = NULL, pt_term = NULL, q_start, q_end) {
  parts <- c()
  if (!is.null(drug_name)) {
    dn <- gsub(" ", "+", toupper(drug_name))
    parts <- c(parts, paste0(
      "(patient.drug.medicinalproduct:", dn,
      "+patient.drug.openfda.brand_name:", dn,
      "+patient.drug.openfda.generic_name:", dn, ")"))
  }
  if (!is.null(pt_term))
    parts <- c(parts, paste0("patient.reaction.reactionmeddrapt:", gsub(" ", "+", pt_term)))
  parts <- c(parts, paste0("receivedate:[", q_start, "+TO+", q_end, "]"))
  paste0("https://api.fda.gov/drug/event.json?search=",
         paste(parts, collapse = "+AND+"), "&limit=1")
}

# ── Fetch total count for one openFDA query (synchronous, with logging) ──────
fetch_total <- function(url) {
  tryCatch({
    h <- curl::new_handle()
    curl::handle_setopt(h, timeout = 15L, connecttimeout = 10L)
    resp <- curl::curl_fetch_memory(url, handle = h)
    if (resp$status_code == 404) return(0L)
    if (resp$status_code != 200) {
      message("[FAERS] HTTP ", resp$status_code, " for ", substr(url, 1, 120))
      return(NA_integer_)
    }
    parsed <- fromJSON(rawToChar(resp$content))
    parsed$meta$results$total %||% 0L
  }, error = function(e) {
    message("[FAERS] Error: ", conditionMessage(e), " — URL: ", substr(url, 1, 120))
    NA_integer_
  })
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

# ── Compute PRR columns from a data frame with count_a/b/c/d ────────────────
compute_prr <- function(df) {
  df |>
    dplyr::mutate(
      B      = pmax(count_b, 1),
      C      = pmax(count_c, 1),
      D      = pmax(count_d, 1),
      PRR    = (count_a / B) / (C / D),
      # 95% CI via log-normal approximation (Rothman)
      PRR_log_se = ifelse(count_a > 0,
                          sqrt(1/count_a - 1/B + 1/C - 1/D),
                          NA_real_),
      PRR_lo  = ifelse(!is.na(PRR_log_se), exp(log(PRR) - 1.96 * PRR_log_se), NA_real_),
      PRR_hi  = ifelse(!is.na(PRR_log_se), exp(log(PRR) + 1.96 * PRR_log_se), NA_real_),
      E      = B * C / D,
      chi_sq = ifelse(E > 0, (count_a - E)^2 / E, NA_real_)
    )
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
