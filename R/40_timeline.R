# timeline.R — Regulatory Timeline Intelligence
# Contextualises a live signal against historical signal-to-label lags from the
# reference cohort. Contextual, not predictive.
# Sourced automatically by Shiny before app.R (all files in R/ are).

# ── Regulatory Timeline Intelligence ────────────────────────────────────────
# Predict expected FDA action window based on historical signal-to-label lags.
# Uses class-specific data when available (min 3 drugs), falls back to all drugs.
# Returns a list with: estimate, ci_lo, ci_hi, percentiles, reference drugs used.
predict_timeline <- function(months_active, drug_class = NULL, benchmark_df) {
  if (!is.null(drug_class) && drug_class %in% benchmark_df$therapeutic_class) {
    ref <- benchmark_df |> filter(therapeutic_class == drug_class)
    if (nrow(ref) < 3) ref <- benchmark_df
  } else {
    ref <- benchmark_df
  }
  lags  <- ref$lag_months
  med   <- median(lags)
  q25   <- quantile(lags, 0.25)
  q75   <- quantile(lags, 0.75)
  pct_at <- round(100 * mean(lags <= months_active))

  list(
    ref_class     = if (!is.null(drug_class) && drug_class %in% ref$therapeutic_class)
                      drug_class else "All classes",
    n_ref         = length(lags),
    ref_drugs     = ref$drug_name,
    median_lag    = med,
    q25 = q25, q75 = q75,
    months_active = months_active,
    pct_at        = pct_at,
    risk          = if (months_active >= q75) "OVERDUE"
                    else if (months_active >= med) "EXPECTED WINDOW"
                    else if (months_active >= q25) "APPROACHING"
                    else "EARLY"
  )
}
