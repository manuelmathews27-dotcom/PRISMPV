# 03_visualizations.R
# Standalone plots (run these to preview charts outside of the Shiny app).
# These are the same plots used inside app.R.

library(ggplot2)
library(dplyr)
library(lubridate)

source("R/utils.R")

combined <- readRDS("data/combined.rds")
faers_raw <- readRDS("data/faers_raw.rds")

signals <- compute_prr(faers_raw)


# ── Plot 1: Lag bar chart ─────────────────────────────────────────────────────
# How long did it take for each drug's signal to become a label change?

plot_lag_bar <- function(data = combined) {
  data |>
    filter(!is.na(lag_months)) |>
    arrange(lag_months) |>
    mutate(drug_name = factor(drug_name, levels = drug_name)) |>
    ggplot(aes(x = drug_name, y = lag_months, fill = label_change_type)) +
    geom_col(width = 0.7) +
    geom_hline(
      yintercept = median(data$lag_months, na.rm = TRUE),
      linetype = "dashed", color = "firebrick", linewidth = 0.8
    ) +
    annotate("text",
      x = 0.6, y = median(data$lag_months, na.rm = TRUE) + 2,
      label = paste0("Median: ", round(median(data$lag_months, na.rm = TRUE), 1), " months"),
      hjust = 0, color = "firebrick", size = 3.5
    ) +
    scale_fill_brewer(palette = "Set2", name = "Label Change Type") +
    labs(
      title    = "Signal-to-Label Lag by Drug",
      subtitle = "Time from first confirmed FAERS signal to FDA label update",
      x        = NULL,
      y        = "Lag (months)"
    ) +
    coord_flip() +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
}


# ── Plot 2: PRR trend for a single drug ───────────────────────────────────────
# Shows the rising signal alongside the eventual label change date.

plot_prr_trend <- function(drug_choice, data_signals = signals, data_combined = combined) {

  drug_signals <- data_signals |>
    filter(drug == drug_choice, !is.na(PRR))

  drug_meta <- data_combined |>
    filter(drug_name == drug_choice)

  label_date  <- drug_meta$label_change_date[1]
  signal_date <- drug_meta$signal_start_date[1]
  pt_label    <- drug_meta$adverse_event[1]

  ggplot(drug_signals, aes(x = quarter, y = PRR)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(aes(color = PRR >= 2), size = 2.5, show.legend = FALSE) +
    scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "steelblue")) +
    geom_hline(yintercept = 2, linetype = "dashed", color = "orange", linewidth = 0.8) +
    annotate("text", x = min(drug_signals$quarter), y = 2.2,
             label = "PRR = 2 threshold", hjust = 0, color = "orange", size = 3.2) +
    {if (!is.na(label_date))
      geom_vline(xintercept = as.numeric(label_date), linetype = "solid",
                 color = "firebrick", linewidth = 1)
    } +
    {if (!is.na(label_date))
      annotate("text", x = label_date, y = max(drug_signals$PRR, na.rm = TRUE) * 0.95,
               label = "Label\nchange", hjust = -0.1, color = "firebrick", size = 3.2)
    } +
    {if (!is.na(signal_date))
      geom_vline(xintercept = as.numeric(signal_date), linetype = "dotted",
                 color = "darkgreen", linewidth = 1)
    } +
    {if (!is.na(signal_date))
      annotate("text", x = signal_date, y = max(drug_signals$PRR, na.rm = TRUE) * 0.8,
               label = "Signal\ndetected", hjust = 1.1, color = "darkgreen", size = 3.2)
    } +
    labs(
      title    = paste0("PRR Trend: ", tools::toTitleCase(drug_choice)),
      subtitle = paste0("Adverse event tracked: ", tools::toTitleCase(pt_label)),
      x        = "Quarter",
      y        = "Proportional Reporting Ratio (PRR)"
    ) +
    theme_minimal(base_size = 13)
}


# ── Plot 3: Label change type breakdown ───────────────────────────────────────

plot_change_type <- function(data = combined) {
  data |>
    count(label_change_type) |>
    ggplot(aes(x = reorder(label_change_type, n), y = n, fill = label_change_type)) +
    geom_col(show.legend = FALSE, width = 0.6) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Types of Label Changes in Cohort",
      x     = NULL,
      y     = "Number of drugs"
    ) +
    coord_flip() +
    theme_minimal(base_size = 13)
}


# ── Preview plots (comment out if running inside Shiny) ───────────────────────
print(plot_lag_bar())
print(plot_prr_trend("rosiglitazone"))
print(plot_change_type())
