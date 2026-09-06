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

# cohort_role rides along from label_changes.csv so downstream consumers can
# separate the arms; default to "case" if an older CSV lacks the column.
if (!"cohort_role" %in% names(label_changes)) label_changes$cohort_role <- "case"

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

# ── Summary, split by arm ────────────────────────────────────────────────────
# Cases and controls MUST be reported separately. Pooling them would state a
# signal rate over a set that mixes drugs FDA acted on with drugs it did not,
# which is the exact confusion the control arm exists to resolve. Controls carry
# no label_change_date, so every lag statistic is case-only by construction.
if (!"cohort_role" %in% names(combined)) combined$cohort_role <- "case"
cases    <- combined[combined$cohort_role == "case", ]
controls <- combined[combined$cohort_role == "control", ]

pct <- function(n, d) if (d == 0) "n/a" else sprintf("%d/%d (%.0f%%)", n, d, 100 * n / d)

cat("\n── Signal-to-Label Lag Summary (CASES) ──────────────────────\n")
cat(sprintf("  Drugs analyzed         : %d\n",   nrow(cases)))
cat(sprintf("  Signals detected       : %s\n",   pct(sum(!is.na(cases$signal_start_quarter)), nrow(cases))))
cat(sprintf("  Median lag (months)    : %.1f\n", median(cases$lag_months, na.rm = TRUE)))
cat(sprintf("  Min lag (months)       : %.1f\n", min(cases$lag_months,    na.rm = TRUE)))
cat(sprintf("  Max lag (months)       : %.1f\n", max(cases$lag_months,    na.rm = TRUE)))

if (nrow(controls) > 0) {
  cat("\n── Control arm (no label change for the tracked event) ──────\n")
  cat(sprintf("  Controls analyzed      : %d\n", nrow(controls)))
  cat(sprintf("  Signals detected       : %s\n",
              pct(sum(!is.na(controls$signal_start_quarter)), nrow(controls))))
  cat("  Per control:\n")
  for (i in seq_len(nrow(controls))) {
    cat(sprintf("    %-12s %-30s %s\n",
                controls$drug_name[i], controls$adverse_event[i],
                if (is.na(controls$signal_start_quarter[i])) "no signal"
                else paste("SIGNAL from", controls$signal_start_quarter[i])))
  }
  cat("\n  A control that signals is not automatically a false positive: it may\n")
  cat("  share a class effect that FDA labelled only for other members.\n")
}
cat("─────────────────────────────────────────────────────────────\n\n")
