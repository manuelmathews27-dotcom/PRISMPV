# Curation guard for data/negative_controls.csv.
# Run: Rscript tests/test_negative_controls.R   (from repo root)
# Pure CSV logic — no network. Exits non-zero on any failure so it can gate
# run_pipeline.R and the deploy.
#
# WHY THIS EXISTS
# An earlier attempt at a comparison arm used SAME-CLASS drugs as controls
# (Lescol as a control for the statin cohort, Dexilant for the PPI cohort, and
# so on). That is structurally impossible: when FDA labels a risk CLASS-WIDE,
# every member of the class receives the label, so a same-class "control" is
# just an uncollected case. FDA's Feb 2012 statin diabetes change named Lescol
# and Livalo explicitly. Empirically the mistake was just as bad — 7 of 8
# era-matched comparators signalled, Belsomra reaching PRR 36 against Ambien's
# 38 for the same event.
#
# Check 2 below encodes that lesson: a negative control may never pair a drug
# with an event that its OWN class was labelled for.

find_root <- function() {
  here <- normalizePath(getwd())
  for (i in 1:5) {
    if (file.exists(file.path(here, "R", "00_utils.R"))) return(here)
    here <- dirname(here)
  }
  stop("cannot locate R/00_utils.R from ", getwd())
}
setwd(find_root())

FAIL <- 0L
fail <- function(fmt, ...) { cat(sprintf(paste0("  FAIL: ", fmt, "\n"), ...)); FAIL <<- FAIL + 1L }
ok   <- function(fmt, ...) cat(sprintf(paste0("  ok:   ", fmt, "\n"), ...))

nc <- read.csv("data/negative_controls.csv", stringsAsFactors = FALSE)
lc <- read.csv("data/label_changes.csv",     stringsAsFactors = FALSE)

cat(sprintf("\nChecking %d negative control pairs\n", nrow(nc)))

# ── 1. Schema and completeness ───────────────────────────────────────────────
cat("\n-- schema --\n")
need <- c("drug_name", "generic_name", "pt_term", "window_start", "window_end",
          "source_class", "rationale", "status")
missing_cols <- setdiff(need, names(nc))
if (length(missing_cols)) {
  fail("missing column(s): %s", paste(missing_cols, collapse = ", "))
} else {
  ok("all %d required columns present", length(need))
}

blank <- which(!nzchar(trimws(nc$rationale)))
if (length(blank)) {
  fail("%d row(s) have no rationale: %s", length(blank),
       paste(nc$drug_name[blank], collapse = ", "))
} else {
  ok("every pair carries a curation rationale")
}

bad_status <- setdiff(unique(nc$status), c("negative_control", "excluded_confounded"))
if (length(bad_status)) {
  fail("unrecognised status value(s): %s", paste(bad_status, collapse = ", "))
} else {
  ok("status values are all recognised")
}

# ── 2. No negative control may share its own class's labelled event ──────────
# This is the check that makes the arm valid. See the header note.
cat("\n-- no control pairs a drug with its own class's labelled event --\n")
# label_changes stores a prose adverse_event; the pull uses the MedDRA PT.
# One documented divergence: the antipsychotic mortality warning is queried as
# the PT "death".
ae_to_pt <- function(x) {
  x <- tolower(trimws(x))
  ifelse(grepl("mortality", x), "death", x)
}
forbidden <- unique(paste(lc$therapeutic_class, ae_to_pt(lc$adverse_event), sep = " || "))
key <- paste(nc$source_class, tolower(trimws(nc$pt_term)), sep = " || ")
clash <- which(key %in% forbidden)
if (length(clash)) {
  for (i in clash)
    fail("%s (%s) paired with '%s' — that IS this class's labelled event",
         nc$drug_name[i], nc$source_class[i], nc$pt_term[i])
} else {
  ok("no pair collides with a labelled class/event combination (%d checked)", nrow(nc))
}

# ── 3. Promiscuous events must not be used as controls ───────────────────────
# Events with many plausible drug causes cannot support a "no association"
# claim, so they are barred as control events regardless of the drug.
cat("\n-- promiscuous events barred as controls --\n")
barred <- c("death", "myocardial infarction", "gastrointestinal haemorrhage",
            "rhabdomyolysis", "diabetes mellitus", "lymphoma")
hit <- which(tolower(trimws(nc$pt_term)) %in% barred)
if (length(hit)) {
  fail("barred control event(s) used: %s", paste(unique(nc$pt_term[hit]), collapse = ", "))
} else {
  ok("none of the %d barred events appear as a control event", length(barred))
}

# ── 4. Keys unique, windows sane ─────────────────────────────────────────────
cat("\n-- keys and windows --\n")
k <- paste(toupper(nc$drug_name), tolower(nc$pt_term))
if (anyDuplicated(k)) {
  fail("duplicate (drug, event) key(s): %s", paste(unique(k[duplicated(k)]), collapse = ", "))
} else {
  ok("all %d (drug, event) keys are unique", length(k))
}

bad_win <- which(!(nc$window_start <= nc$window_end))
if (length(bad_win)) {
  fail("%d row(s) have window_start > window_end", length(bad_win))
} else {
  ok("every window runs forwards")
}

# ── 5. Enough informative pairs for the interval to mean anything ────────────
cat("\n-- arm is large enough to be worth reporting --\n")
n_primary <- sum(nc$status == "negative_control")
if (n_primary < 20) {
  fail("only %d primary pairs; the 95%% upper bound on the FP rate would be too wide", n_primary)
} else {
  ok("%d primary pairs (95%% upper bound on a 0-failure rate would be %.1f%%)",
     n_primary, 100 * (1 - 0.05^(1 / n_primary)))
}

if (FAIL > 0L) { cat(sprintf("\n%d failure(s)\n", FAIL)); quit(status = 1L) }
cat("\nAll negative control curation checks passed.\n")
