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

tryCatch(source("scripts/01_faers_pull.R"),
         error = function(e) stop("01_faers_pull.R failed: ", conditionMessage(e)))

if (!file.exists("data/faers_raw.rds")) stop("Pipeline halted: data/faers_raw.rds not created")

tryCatch(source("scripts/02_signal_detection.R"),
         error = function(e) stop("02_signal_detection.R failed: ", conditionMessage(e)))

if (!file.exists("data/combined.rds")) stop("Pipeline halted: data/combined.rds not created")

tryCatch(source("scripts/03_visualizations.R"),
         error = function(e) message("03_visualizations.R failed (non-fatal): ", conditionMessage(e)))

message("\nPipeline complete! Launch the dashboard with:\n  shiny::runApp()\n")
