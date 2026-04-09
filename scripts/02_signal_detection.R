# 02_signal_detection.R
# Compute Proportional Reporting Ratio (PRR) per drug-event pair per quarter,
# then identify the quarter when each signal first met detection criteria.
#
# PRR formula:
#   PRR = (A / B) / (C / D)
#   where:
#     A = reports with THIS drug AND THIS event
#     B = reports with THIS drug (any event)
#     C = reports with THIS event (any drug)
#     D = all reports in the quarter
#
# Signal threshold (Evans criteria, widely used by regulators):
#   PRR >= 2  AND  chi-squared >= 4  AND  A >= 3

library(dplyr)
library(lubridate)

source("R/utils.R")

# ── Load raw counts ───────────────────────────────────────────────────────────
faers_raw <- readRDS("data/faers_raw.rds")


# ── Calculate PRR and chi-squared ─────────────────────────────────────────────
signals <- faers_raw |>
  compute_prr() |>
  mutate(
    A = count_a,
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
