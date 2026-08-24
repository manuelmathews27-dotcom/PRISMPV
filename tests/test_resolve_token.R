# Regression test for canonical_ingredient_token() in R/utils.R.
# Run: Rscript tests/test_resolve_token.R   (from repo root)
# Exits non-zero on any failure so it can gate run_pipeline.R.
#
# Pure string logic — no network. Guards the 2026-08-24 fix where FDA biologic
# suffixes were welded onto the stem ("tafasitamab-cxix" -> TAFASITAMABCXIX),
# producing a canonical term that matched ZERO FAERS records while the app
# reported a clean "no signal".

find_utils <- function() {
  here <- normalizePath(getwd())
  for (i in 1:5) {
    p <- file.path(here, "R", "utils.R")
    if (file.exists(p)) return(here)
    here <- dirname(here)
  }
  stop("cannot locate R/utils.R from ", getwd())
}
setwd(find_utils())
suppressWarnings(suppressPackageStartupMessages(source("R/utils.R")))

FAIL <- 0L

expect <- function(input, want) {
  got <- canonical_ingredient_token(input)
  ok <- (is.na(want) && is.na(got)) || (!is.na(got) && !is.na(want) && got == want)
  if (ok) {
    cat(sprintf("  ok:   %-28s -> %s\n", input, format(got)))
  } else {
    cat(sprintf("  FAIL: %-28s -> %s  (want %s)\n", input, format(got), format(want)))
    FAIL <<- FAIL + 1L
  }
}

cat("\n-- biologic suffixes must be stripped, not welded --\n")
expect("tafasitamab-cxix",  "TAFASITAMAB")    # Monjuvi
expect("retifanlimab-dlwr", "RETIFANLIMAB")   # Zynyz
expect("axatilimab-csfr",   "AXATILIMAB")     # Niktimvo
expect("TAFASITAMAB-CXIX",  "TAFASITAMAB")    # already upper
expect("trastuzumab-dkst",  "TRASTUZUMAB")    # biosimilar
expect("insulin glargine-yfgn", NA_character_)  # 2 words after suffix -> not single

cat("\n-- plain single ingredients unchanged --\n")
expect("RUXOLITINIB", "RUXOLITINIB")
expect("pemigatinib", "PEMIGATINIB")
expect("aspirin",     "ASPIRIN")

cat("\n-- salt forms / dosage words stripped as qualifiers --\n")
expect("ATORVASTATIN CALCIUM", "ATORVASTATIN")
expect("ONDANSETRON HYDROCHLORIDE", "ONDANSETRON")

cat("\n-- combinations must NOT resolve (caller falls back to user input) --\n")
# The pre-fix code deleted the hyphen and returned SACUBITRILVALSARTAN.
expect("SACUBITRIL-VALSARTAN", NA_character_)
expect("TRIFLURIDINE/TIPIRACIL", NA_character_)

cat("\n-- degenerate input --\n")
expect("", NA_character_)
expect("MG", NA_character_)

if (FAIL > 0L) {
  cat(sprintf("\n%d failure(s)\n", FAIL)); quit(status = 1L)
}
cat("\nAll canonical_ingredient_token tests passed.\n")
