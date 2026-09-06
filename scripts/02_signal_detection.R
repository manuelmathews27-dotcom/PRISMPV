# 02_signal_detection.R
# Compute Proportional Reporting Ratio (PRR) per drug-event pair per quarter,
# then identify the quarter when each signal first met detection criteria.
#
# NOTE ON INPUT SHAPE: openFDA returns marginals, not 2x2 cells.
#   count_a = drug + event                 (= cell a)
#   count_b = drug, any event              (= a + b, row marginal)
#   count_c = event, any drug              (= a + c, column marginal)
#   count_d = all reports                  (= N, grand total)
# compute_prr() in R/00_utils.R reconstructs the true cells before computing.
#
# PRR (textbook, Rothman & Greenland):
#   PRR = (a / (a+b)) / (c / (c+d))
#       = (count_a / count_b) / ((count_c - count_a) / (count_d - count_b))
#
# Chi-squared (Pearson with Yates continuity correction):
#   chi^2 = N * (|a*d - b*c| - N/2)^2 / ((a+b)(c+d)(a+c)(b+d))
#
# Signal threshold (Evans criteria, widely used by regulators):
#   PRR >= 2  AND  chi-squared >= 4  AND  A >= 3
#   plus PRR_lo (95% CI lower bound) > 1 when CI is computable.

library(dplyr)
library(lubridate)

source("R/00_utils.R")

# ── Load raw counts ───────────────────────────────────────────────────────────
faers_raw <- readRDS("data/faers_raw.rds")


# ── Calculate PRR and chi-squared ─────────────────────────────────────────────
signals <- faers_raw |>
  compute_prr() |>
  mutate(
    A = count_a,
    # Preserve the long-standing signals table schema. compute_prr() consumes
    # these openFDA marginals but no longer creates their B/C/D aliases itself.
    B = count_b,
    C = count_c,
    D = count_d,
    signal_met = check_signal(count_a, PRR, chi_sq, PRR_lo)
  ) |>
  select(drug, pt, quarter, A, B, C, D, PRR, PRR_lo, PRR_hi, chi_sq, signal_met)


# ── Find first signal quarter per drug ────────────────────────────────────────
# "First signal" = earliest quarter where signal_met == TRUE
# Single-quarter threshold: appropriate for low-reporting drugs where
# consecutive quarters are hard to achieve with sparse reports.

first_signals <- signals |>
  arrange(drug, quarter) |>
  group_by(drug, pt) |>
  filter(signal_met) |>
  slice_min(quarter, n = 1) |>
  ungroup() |>
  select(drug, pt, signal_start_quarter = quarter,
         PRR_at_signal = PRR, PRR_lo_at_signal = PRR_lo, PRR_hi_at_signal = PRR_hi)


# ── Join with label change data ───────────────────────────────────────────────
label_changes <- read.csv("data/label_changes.csv", stringsAsFactors = FALSE) |>
  mutate(label_change_date = as.Date(label_change_date))

combined <- label_changes |>
  mutate(drug_name_upper = toupper(drug_name)) |>
  left_join(first_signals, by = c("drug_name_upper" = "drug")) |>
  select(-drug_name_upper) |>
  mutate(
    # signal_start_quarter is already a Date (first day of that quarter)
    signal_start_date = signal_start_quarter,
    lag_days   = as.numeric(label_change_date - signal_start_date),
    lag_months = round(lag_days / 30.44, 1),
    lag_years  = round(lag_days / 365.25, 2),

    # Did we detect a signal at all before the label change?
    signal_detected_before_change = !is.na(signal_start_date) & signal_start_date <= label_change_date
  )

saveRDS(combined, "data/combined.rds")
message("Signal detection complete. Results saved to data/combined.rds")

# Quick summary print
cat("\n── Signal-to-Label Lag Summary ──────────────────────────────\n")
cat(sprintf("  Drugs analyzed         : %d\n",   nrow(combined)))
cat(sprintf("  Signals detected       : %d\n",   sum(!is.na(combined$signal_start_quarter))))
cat(sprintf("  Median lag (months)    : %.1f\n", median(combined$lag_months, na.rm = TRUE)))
cat(sprintf("  Min lag (months)       : %.1f\n", min(combined$lag_months,    na.rm = TRUE)))
cat(sprintf("  Max lag (months)       : %.1f\n", max(combined$lag_months,    na.rm = TRUE)))
cat("─────────────────────────────────────────────────────────────\n\n")


# ── Negative control arm: specificity ────────────────────────────────────────
# The case arm above measures how FAST the method detects known signals. It
# cannot measure how OFTEN it fires when it should not, because every drug in it
# was selected for having received an FDA label change. This block runs the
# identical detection rule over drug/event pairs chosen to have no plausible
# association, so every signal is a false positive by construction.
#
# The pairs use the SAME detection rule and the SAME quarterly windows as their
# case counterparts — nothing is relaxed. A pair "signals" if any single quarter
# meets the criteria, which is exactly how first_signals declares a case signal.
if (file.exists("data/faers_negative_controls.rds")) {

  neg_meta <- read.csv("data/negative_controls.csv", stringsAsFactors = FALSE) |>
    mutate(drug = toupper(drug_name), pt = pt_term)

  neg_signals <- readRDS("data/faers_negative_controls.rds") |>
    compute_prr() |>
    mutate(signal_met = check_signal(count_a, PRR, chi_sq, PRR_lo))

  neg_pairs <- neg_signals |>
    group_by(drug, pt) |>
    summarise(
      n_reports      = sum(count_a, na.rm = TRUE),
      max_PRR        = suppressWarnings(max(PRR, na.rm = TRUE)),
      signal_quarters = sum(signal_met, na.rm = TRUE),
      first_signal   = if (any(signal_met, na.rm = TRUE))
                         min(quarter[which(signal_met)]) else as.Date(NA),
      .groups = "drop"
    ) |>
    mutate(
      max_PRR       = ifelse(is.finite(max_PRR), max_PRR, NA_real_),
      ever_signalled = signal_quarters > 0,
      # A pair with fewer than SIGNAL_MIN_REPORTS reports across the whole window
      # CANNOT signal under the n >= 3 criterion, so it is not a real test of the
      # method. Counting it as a "pass" would pad specificity with pairs that were
      # never capable of failing.
      informative    = n_reports >= SIGNAL_MIN_REPORTS
    ) |>
    left_join(neg_meta |> select(drug, pt, rationale, status, source_class),
              by = c("drug", "pt"))

  # The join is the one place this arm can fail silently. If a drug/event key in
  # the pulled data does not match the CSV, `status` becomes NA, the primary
  # filter drops the pair, and specificity is computed over a smaller set than
  # reported -- with no error. Fail loudly instead.
  if (any(is.na(neg_pairs$status))) {
    stop("negative control join failed for: ",
         paste(sprintf("%s/%s", neg_pairs$drug[is.na(neg_pairs$status)],
                       neg_pairs$pt[is.na(neg_pairs$status)]), collapse = ", "))
  }
  if (nrow(neg_pairs) != nrow(neg_meta)) {
    stop("negative control arm: expected ", nrow(neg_meta), " pairs, computed ",
         nrow(neg_pairs), " -- the pull and the CSV disagree")
  }

  # Primary analysis excludes pairs found to be confounded (status marked in the
  # CSV with the reason). Those are curation failures, not method failures, and
  # are reported separately rather than quietly dropped.
  primary   <- neg_pairs |> filter(status == "negative_control", informative)
  excluded  <- neg_pairs |> filter(status != "negative_control")

  n_prim <- nrow(primary)
  n_fp   <- sum(primary$ever_signalled)
  # Exact binomial (Clopper-Pearson) upper bound on the false-positive rate.
  fp_hi  <- if (n_prim > 0) stats::qbeta(0.975, n_fp + 1, n_prim - n_fp) else NA_real_

  negative_controls <- list(
    pairs    = neg_pairs,
    excluded = excluded,
    summary  = list(
      n_pairs        = nrow(neg_pairs),
      n_informative  = n_prim,
      n_uninformative = sum(neg_pairs$status == "negative_control" & !neg_pairs$informative),
      n_excluded     = nrow(excluded),
      n_false_pos    = n_fp,
      fp_rate        = if (n_prim > 0) n_fp / n_prim else NA_real_,
      fp_rate_hi95   = fp_hi,
      specificity    = if (n_prim > 0) 1 - n_fp / n_prim else NA_real_
    )
  )
  saveRDS(negative_controls, "data/negative_controls.rds")

  s <- negative_controls$summary
  cat("\n── Negative Control Arm (specificity) ───────────────────────\n")
  cat(sprintf("  Pairs curated          : %d\n", s$n_pairs))
  cat(sprintf("  Excluded (confounded)  : %d\n", s$n_excluded))
  cat(sprintf("  Too sparse to test     : %d\n", s$n_uninformative))
  cat(sprintf("  Informative pairs      : %d\n", s$n_informative))
  cat(sprintf("  False positives        : %d\n", s$n_false_pos))
  cat(sprintf("  Specificity            : %.1f%%\n", 100 * s$specificity))
  cat(sprintf("  FP rate 95%% upper bound: %.1f%%\n", 100 * s$fp_rate_hi95))
  if (s$n_false_pos > 0) {
    cat("  Pairs that fired:\n")
    for (i in which(primary$ever_signalled))
      cat(sprintf("    %-10s %-30s maxPRR=%6.2f  quarters=%d\n",
                  primary$drug[i], primary$pt[i], primary$max_PRR[i],
                  primary$signal_quarters[i]))
  }
  cat("─────────────────────────────────────────────────────────────\n\n")

} else {
  message("[PRISM] no data/faers_negative_controls.rds — skipping specificity arm")
}
