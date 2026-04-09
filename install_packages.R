# Run this once to install all required packages
lib <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(lib)) dir.create(lib, recursive = TRUE)

install.packages(
  c("curl", "jsonlite", "dplyr", "lubridate", "ggplot2", "ggrepel", "shiny", "bslib", "DT"),
  lib   = lib,
  repos = "https://cloud.r-project.org"
)
