# Regression test for compute_prr() in R/utils.R.
# Run: Rscript tests/test_prr_formula.R   (from repo root)
# Exits non-zero on any failure so it can gate run_pipeline.R.
#
# Inputs to compute_prr() are openFDA MARGINALS (count_a/b/c/d), not 2x2 cells.
# This test constructs known cell configurations, feeds the equivalent marginals,
# and verifies compute_prr() recovers the textbook PRR, Rothman CI, and
# Yates-corrected Pearson chi-squared.

setwd(dirname(dirname(normalizePath(sys.frame(1)$ofile %||% "tests/test_prr_formula.R"))))
suppressWarnings(suppressPackageStartupMessages(source("R/utils.R")))

`%||%` <- function(a, b) if (is.null(a)) b else a
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

# ── textbook PRR from cells (ground truth) ─────────────────────────────────
true_prr <- function(a, b, c, d) (a / (a + b)) / (c / (c + d))
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

cat("──────────────────────────────────────────────────────────────\n")
if (FAIL > 0) {
  cat(sprintf("\nFAILED: %d assertion(s) failed.\n", FAIL))
  quit(status = 1, save = "no")
} else {
  cat("\nAll PRR/CI/chi-sq assertions passed.\n")
  quit(status = 0, save = "no")
}
