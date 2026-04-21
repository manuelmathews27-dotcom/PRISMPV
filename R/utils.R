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
