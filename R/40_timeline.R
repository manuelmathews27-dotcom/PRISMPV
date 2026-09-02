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


# ── Cohort lag overview ──────────────────────────────────────────────────────
# Replaces the per-drug quarterly PRR line as the Reference Cohort's primary
# view. The cohort answers a cross-drug question ("how early is the signal
# relative to FDA action?"), which is one number per drug — a per-drug time
# series was the wrong encoding for it, and quarterly PRR on sparse counts is
# genuinely spiky, so no amount of restyling made it readable.
#
# Design notes:
#  * x is LAG, anchored at zero, so bar lengths are directly comparable. Real
#    calendar dates would break that comparison, so they appear as a text column
#    instead of as geometry.
#  * Colour encodes the sign of the lag — the finding — and nothing else. Adding
#    a second colour encoding (era, change type) would fight it.
#  * Faceting by class turns 42 rows into 12 scannable blocks and makes the
#    class-wide blind spots legible: a class where nothing was detected shows up
#    as a visibly short block rather than as absent rows.
plot_cohort_lag <- function(data = combined, facet_by_class = TRUE) {
  d <- data |>
    dplyr::filter(!is.na(lag_months)) |>
    dplyr::mutate(
      preceded   = lag_months >= 0,
      date_label = paste0(
        ifelse(is.na(signal_start_quarter), "—", fmt_quarter(signal_start_quarter)),
        "  →  ", format(label_change_date, "%b %Y")
      )
    ) |>
    dplyr::arrange(lag_months) |>
    dplyr::mutate(drug_name = factor(drug_name, levels = unique(drug_name)))

  if (nrow(d) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No drugs with lag data in this selection.",
                 size = 4.5, colour = "grey40") +
        theme_void()
    )
  }

  med  <- median(d$lag_months, na.rm = TRUE)
  # Leave room on the right for the date column; it is drawn inside the panel so
  # it stays aligned with each row regardless of facet height.
  x_max <- max(d$lag_months, na.rm = TRUE)
  x_min <- min(0, min(d$lag_months, na.rm = TRUE))
  pad   <- (x_max - x_min) * 0.62

  p <- ggplot(d, aes(x = lag_months, y = drug_name, colour = preceded)) +
    geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.6) +
    geom_vline(xintercept = med, linetype = "dashed",
               colour = "#e05c00", linewidth = 0.7) +
    geom_segment(aes(x = 0, xend = lag_months, y = drug_name, yend = drug_name),
                 linewidth = 0.9, alpha = 0.75) +
    geom_point(size = 3.1) +
    geom_text(aes(label = sprintf("%.1f", lag_months)),
              hjust = ifelse(d$preceded, -0.35, 1.35),
              size = 3.1, colour = "grey25", show.legend = FALSE) +
    geom_text(aes(x = x_max + pad * 0.12, label = date_label),
              hjust = 0, size = 2.9, colour = "grey45", show.legend = FALSE) +
    scale_colour_manual(
      values = c(`TRUE` = "#1e1b4b", `FALSE` = "#c1272d"),
      labels = c(`TRUE` = "Signal preceded label change",
                 `FALSE` = "Signal followed label change"),
      name = NULL
    ) +
    scale_x_continuous(
      limits = c(x_min - pad * 0.05, x_max + pad),
      expand = expansion(mult = c(0.02, 0))
    ) +
    labs(
      title    = "Signal-to-label lag by drug",
      subtitle = sprintf(
        "Months from first FAERS signal to FDA label change  ·  median %.1f  ·  %d drugs",
        med, nrow(d)),
      x = "Lag (months)", y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position   = "top",
      legend.justification = "left",
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title.position = "plot",
      axis.text.y = element_text(size = 9)
    )

  if (facet_by_class) {
    p <- p + facet_grid(therapeutic_class ~ ., scales = "free_y", space = "free_y",
                        switch = "y") +
      theme(
        strip.placement  = "outside",
        strip.text.y.left = element_text(angle = 0, hjust = 1, size = 8.5,
                                         colour = "grey30"),
        panel.spacing.y  = grid::unit(0.35, "lines")  # grid:: — not re-exported by ggplot2 in all versions
      )
  }
  p
}
