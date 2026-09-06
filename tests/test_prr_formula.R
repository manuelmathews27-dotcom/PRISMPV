# Regression test for compute_prr() in R/00_utils.R.
# Run: Rscript tests/test_prr_formula.R   (from repo root)
# Exits non-zero on any failure so it can gate run_pipeline.R.
#
# Inputs to compute_prr() are openFDA MARGINALS (count_a/b/c/d), not 2x2 cells.
# This test constructs known cell configurations, feeds the equivalent marginals,
# and verifies compute_prr() recovers the textbook PRR, Rothman CI, and
# Yates-corrected Pearson chi-squared.

# Resolve repo root: run from anywhere, find utils.R by walking up from CWD
find_utils <- function() {
  here <- normalizePath(getwd())
  for (i in 1:5) {
    p <- file.path(here, "R", "00_utils.R")
    if (file.exists(p)) return(here)
    here <- dirname(here)
  }
  stop("cannot locate R/00_utils.R from ", getwd())
}
setwd(find_utils())
suppressWarnings(suppressPackageStartupMessages(source("R/00_utils.R")))

EPS <- 1e-6
FAIL <- 0L

expect_equal <- function(label, got, want, eps = EPS) {
  ok <- isTRUE(all.equal(got, want, tolerance = eps))
  if (!ok) {
    cat(sprintf("  FAIL: %s  got=%s  want=%s\n", label, format(got), format(want)))
    FAIL <<- FAIL + 1L
  } else {
    cat(sprintf("  ok:   %s = %s\n", label, format(got)))
  }
}

expect_na <- function(label, got) {
  if (is.na(got)) {
    cat(sprintf("  ok:   %s is NA (as expected)\n", label))
  } else {
    cat(sprintf("  FAIL: %s  got=%s  want=NA\n", label, format(got)))
    FAIL <<- FAIL + 1L
  }
}

# ── cell -> marginal helper (what the openFDA pull effectively returns) ─────
as_marginals <- function(a, b, c, d) {
  data.frame(count_a = a, count_b = a + b, count_c = a + c, count_d = a + b + c + d)
}

# ── textbook PRR / ROR from cells (ground truth) ───────────────────────────
true_prr <- function(a, b, c, d) (a / (a + b)) / (c / (c + d))
# ROR is the plain odds ratio of the same 2x2. It guards the cell reconstruction
# for b and d, which PRR never forms and which therefore had no coverage before.
true_ror <- function(a, b, c, d) (a * d) / (b * c)
true_ror_log_se <- function(a, b, c, d) sqrt(1/a + 1/b + 1/c + 1/d)
true_log_se <- function(a, b, c, d) sqrt(1/a - 1/(a+b) + 1/c - 1/(c+d))
true_chi_yates <- function(a, b, c, d) {
  N <- a + b + c + d
  num <- max(abs(a*d - b*c) - N/2, 0)^2
  den <- (a+b) * (c+d) * (a+c) * (b+d)
  N * num / den
}

cases <- list(
  list(label = "strong signal (drug=10% of reports)", a=10, b=90, c=20, d=880),
  list(label = "borderline at PRR=2 (drug=40%)",       a=80, b=320, c=60, d=540),
  list(label = "weak signal (drug=5%)",                a=5,  b=95, c=100, d=1800),
  list(label = "rare drug + rare event",               a=3,  b=7,  c=50, d=940),
  list(label = "large cells",                          a=200,b=800,c=400,d=8600)
)

cat("── compute_prr() regression tests ──────────────────────────────\n")
for (k in cases) {
  cat(sprintf("case: %s  (a=%d b=%d c=%d d=%d)\n", k$label, k$a, k$b, k$c, k$d))
  df <- as_marginals(k$a, k$b, k$c, k$d)
  out <- compute_prr(df)
  tp  <- true_prr(k$a, k$b, k$c, k$d)
  tse <- true_log_se(k$a, k$b, k$c, k$d)
  tx2 <- true_chi_yates(k$a, k$b, k$c, k$d)
  expect_equal("PRR",    out$PRR,        tp)
  expect_equal("log_SE", out$PRR_log_se, tse)
  expect_equal("PRR_lo", out$PRR_lo,     exp(log(tp) - 1.96 * tse))
  expect_equal("PRR_hi", out$PRR_hi,     exp(log(tp) + 1.96 * tse))

  tr   <- true_ror(k$a, k$b, k$c, k$d)
  trse <- true_ror_log_se(k$a, k$b, k$c, k$d)
  expect_equal("ROR",    out$ROR,    tr)
  expect_equal("ROR_lo", out$ROR_lo, exp(log(tr) - 1.96 * trse))
  expect_equal("ROR_hi", out$ROR_hi, exp(log(tr) + 1.96 * trse))
  # For a rare event PRR and ROR should agree closely; a large gap means the
  # cell reconstruction is wrong, not that the data is unusual.
  if ((k$a + k$c) / (k$a + k$b + k$c + k$d) < 0.05 && abs(tr / tp - 1) > 0.25) {
    cat(sprintf("  FAIL: ROR/PRR diverge implausibly for a rare event (%.3f vs %.3f)\n", tr, tp))
    FAIL <- FAIL + 1L
  }
  expect_equal("chi_sq", out$chi_sq,     tx2)
}

# ── degenerate-cell handling: any zero-count marginal returns NA ────────────
cat("── degenerate inputs ───────────────────────────────────────────\n")
deg <- rbind(
  data.frame(count_a=0,  count_b=100, count_c=30,  count_d=1000),   # a=0
  data.frame(count_a=10, count_b=10,  count_c=10,  count_d=1000),   # a+b = c implies c_cell=0
  data.frame(count_a=5,  count_b=100, count_c=5,   count_d=1000),   # c_cell = 0
  data.frame(count_a=5,  count_b=1000,count_c=100, count_d=1000)    # cd_cell = 0
)
out_deg <- compute_prr(deg)
for (i in seq_len(nrow(deg))) {
  expect_na(sprintf("row %d PRR",    i), out_deg$PRR[i])
  expect_na(sprintf("row %d chi_sq", i), out_deg$chi_sq[i])
}

# ── Evans-criterion sanity: case 2 is a true signal, code must agree ───────
cat("── Evans-criterion gate ────────────────────────────────────────\n")
k <- cases[[2]]  # borderline case where old formula FAILED to flag
out <- compute_prr(as_marginals(k$a, k$b, k$c, k$d))
sig <- check_signal(k$a, out$PRR, out$chi_sq, out$PRR_lo)
expect_equal("check_signal on true PRR>=2 case", as.logical(sig), TRUE)

# ── Persistence rule ──────────────────────────────────────────────────────
# first_persistent_index() converts a series of per-quarter verdicts into one
# signal. The negative control arm showed the any-quarter rule has ~54%
# specificity, so this is the rule the app leans on -- it needs a guard.
cat("── persistence rule (2 of any trailing 6) ──────────────────────\n")
expect_equal("no quarters signal",
             first_persistent_index(c(FALSE, FALSE, FALSE)), NA_integer_)
expect_equal("one isolated crossing is NOT persistent",
             first_persistent_index(c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)),
             NA_integer_)
expect_equal("two adjacent crossings fire on the second",
             first_persistent_index(c(FALSE, TRUE, TRUE, FALSE)), 3L)
expect_equal("two crossings 5 apart are inside the 6-window",
             first_persistent_index(c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)), 6L)
expect_equal("two crossings 6 apart are OUTSIDE the 6-window",
             first_persistent_index(c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)),
             NA_integer_)
expect_equal("NA verdicts are treated as non-signalling",
             first_persistent_index(c(NA, TRUE, NA, TRUE)), 4L)
expect_equal("empty series", first_persistent_index(logical(0)), NA_integer_)
# Rule B must be strictly stricter than Rule A: anything the persistence rule
# flags, the any-quarter rule must also flag. If that ordering ever broke, the
# side-by-side specificity comparison on the Methods tab would be meaningless.
set.seed(42)
violations <- 0L
for (i in 1:500) {
  v <- sample(c(TRUE, FALSE), 40, replace = TRUE, prob = c(0.15, 0.85))
  if (!is.na(first_persistent_index(v)) && !any(v)) violations <- violations + 1L
}
expect_equal("Rule B implies Rule A over 500 random series (violations)",
             violations, 0L)

cat("──────────────────────────────────────────────────────────────\n")
if (FAIL > 0) {
  cat(sprintf("\nFAILED: %d assertion(s) failed.\n", FAIL))
  quit(status = 1, save = "no")
} else {
  cat("\nAll PRR/CI/chi-sq assertions passed.\n")
  quit(status = 0, save = "no")
}
