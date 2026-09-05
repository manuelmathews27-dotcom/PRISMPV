# Validates that every curated term in R/20_pt_terms.R is a real MedDRA
# Preferred Term, by asking openFDA whether the exact-field query returns
# anything.
#
# Run: Rscript tests/test_pt_terms.R   (from repo root)
#
# WHY THIS EXISTS
# Queries use `patient.reaction.reactionmeddrapt.exact`, which matches the whole
# PT. A term that is not a PT therefore returns ZERO — not an error, just a clean
# empty result that reads in the UI as "no reports for this pair". Three curated
# entries were in exactly that state before 2026-09-05:
#   "stroke", "malignant neoplasm", "intracranial haemorrhage"
# They appeared to work only because the query had been a substring match.
#
# UNLIKE the other two suites this one NEEDS NETWORK, so it is not part of the
# offline gate in run_pipeline.R. It skips cleanly (exit 0) when openFDA is
# unreachable or no API key is set, so it can never fail a deploy for a reason
# unrelated to the terms themselves.

find_root <- function() {
  here <- normalizePath(getwd())
  for (i in 1:5) {
    if (file.exists(file.path(here, "R", "00_utils.R"))) return(here)
    here <- dirname(here)
  }
  stop("cannot locate R/00_utils.R from ", getwd())
}
setwd(find_root())
suppressWarnings(suppressPackageStartupMessages(source("R/00_utils.R")))
suppressWarnings(suppressPackageStartupMessages(source("R/20_pt_terms.R")))

cat(sprintf("Validating %d curated MedDRA Preferred Terms against openFDA\n",
            length(pt_terms)))

pt_exact_count <- function(term) {
  url <- paste0(
    "https://api.fda.gov/drug/event.json?search=",
    "patient.reaction.reactionmeddrapt.exact:", quote_term(toupper(term)),
    "&limit=1")
  tryCatch({
    h <- curl::new_handle()
    curl::handle_setopt(h, timeout = 20L, connecttimeout = 10L)
    resp <- curl::curl_fetch_memory(openfda_authed_url(url), handle = h)
    if (resp$status_code == 404) return(0L)          # valid query, no matches
    if (resp$status_code != 200) return(NA_integer_) # transient — treat as skip
    body <- jsonlite::fromJSON(rawToChar(resp$content))
    body$meta$results$total %||% 0L
  }, error = function(e) NA_integer_)
}

counts <- vapply(pt_terms, pt_exact_count, integer(1))

unreachable <- sum(is.na(counts))
if (unreachable > length(pt_terms) / 2) {
  cat("SKIP: openFDA unreachable for ", unreachable, " of ", length(pt_terms),
      " terms — not a verdict on the term list.\n", sep = "")
  quit(status = 0L)
}

invalid <- names(counts)[!is.na(counts) & counts == 0]
thin    <- names(counts)[!is.na(counts) & counts > 0 & counts < 100]

for (t in names(counts)) {
  if (is.na(counts[[t]])) next
  if (counts[[t]] == 0) cat(sprintf("  FAIL: %-42s not a MedDRA PT (exact match returns 0)\n", t))
}
if (length(thin) > 0) {
  cat("\n  Note — valid but very low volume (<100 reports); confirm the form is right:\n")
  for (t in thin) cat(sprintf("    %-42s %d\n", t, counts[[t]]))
}
if (unreachable > 0) {
  cat(sprintf("\n  %d term(s) could not be checked (transient); not counted as failures.\n",
              unreachable))
}

if (length(invalid) > 0) {
  cat(sprintf("\n%d term(s) are not valid MedDRA Preferred Terms.\n", length(invalid)))
  cat("Queries for these return zero and read as 'no reports' rather than an error.\n")
  quit(status = 1L)
}
cat(sprintf("\nAll %d checked terms are valid MedDRA Preferred Terms.\n",
            sum(!is.na(counts))))
