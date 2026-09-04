# run_pipeline.R
# Run this file to execute the full analysis pipeline in order.
# After this completes, launch the dashboard with: shiny::runApp()

# Set working directory to project root (works in RStudio, Rscript, and source())
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  # When run via Rscript or source(), use the script's own location
  this_file <- sys.frame(1)$ofile
  if (!is.null(this_file)) setwd(dirname(this_file))
}

# Gate 0: run regression tests before any data work.
# Fails loudly if anyone reverts the cell-reconstruction fix or the Yates chi-sq,
# or the biologic-suffix handling in canonical_ingredient_token().
for (t in c("tests/test_prr_formula.R", "tests/test_resolve_token.R")) {
  if (system2("Rscript", t, stdout = "", stderr = "") != 0)
    stop("Pipeline halted: ", t, " failed")
}

tryCatch(source("scripts/01_faers_pull.R"),
         error = function(e) stop("01_faers_pull.R failed: ", conditionMessage(e)))

if (!file.exists("data/faers_raw.rds")) stop("Pipeline halted: data/faers_raw.rds not created")

tryCatch(source("scripts/02_signal_detection.R"),
         error = function(e) stop("02_signal_detection.R failed: ", conditionMessage(e)))

if (!file.exists("data/combined.rds")) stop("Pipeline halted: data/combined.rds not created")

# scripts/03_visualizations.R was removed 2026-09-04. Its three functions
# (plot_lag_bar, plot_prr_trend, plot_change_type) had ZERO references anywhere
# in app.R or R/ — the app carries its own charts — so it was a second, silently
# diverging copy. It ran under a non-fatal tryCatch, which is also why the CSV
# exports under data/ went stale without anyone noticing.

message("\nPipeline complete! Launch the dashboard with:\n  shiny::runApp()\n")
