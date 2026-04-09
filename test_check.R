setwd("C:/Users/manuz/Desktop/signal-to-label")
source("R/utils.R")
library(dplyr)
library(lubridate)
library(curl)

# Need pull_live_signal from app.R — extract it
source_lines <- readLines("app.R")
start <- grep("^pull_live_signal <- function", source_lines)
# Find matching closing brace
brace_count <- 0
end <- start
for (i in start:length(source_lines)) {
  brace_count <- brace_count + sum(charToRaw(source_lines[i]) == charToRaw("{")) -
                                sum(charToRaw(source_lines[i]) == charToRaw("}"))
  if (brace_count == 0) { end <- i; break }
}
eval(parse(text = source_lines[start:end]))

# Also need signal_status and quarters_active
ss_start <- grep("^signal_status <- function", source_lines)
for (i in ss_start:length(source_lines)) {
  brace_count <- brace_count + sum(charToRaw(source_lines[i]) == charToRaw("{")) -
                                sum(charToRaw(source_lines[i]) == charToRaw("}"))
  if (brace_count == 0) { end <- i; break }
}

# Just define them inline
signal_status <- function(df) {
  recent <- tail(df, 6)
  n_sig <- sum(recent$signal_met, na.rm = TRUE)
  if (n_sig >= 2) "CONFIRMED"
  else if (n_sig == 1) "EMERGING"
  else "NOT DETECTED"
}

cat("=== HUMIRA + oedema ===\n")
df <- pull_live_signal("HUMIRA", "oedema", 12)
cat("Quarters:", nrow(df), "\n")
for (i in 1:nrow(df)) {
  cat(sprintf("Q%d: %s | count_a=%d | PRR=%.2f | PRR_lo=%.2f | chi_sq=%.2f | signal=%s\n",
    i, df$quarter[i], df$count_a[i],
    ifelse(is.na(df$PRR[i]), 0, df$PRR[i]),
    ifelse(is.na(df$PRR_lo[i]), 0, df$PRR_lo[i]),
    ifelse(is.na(df$chi_sq[i]), 0, df$chi_sq[i]),
    df$signal_met[i]))
}
cat("\nSignal status:", signal_status(df), "\n")
cat("Last 6 signal_met:", tail(df$signal_met, 6), "\n")

cat("\n=== AMBIEN BBW check ===\n")
# Check if ambien has boxed_warning in openFDA
h <- curl::new_handle()
curl::handle_setopt(h, timeout = 10L)
resp <- curl::curl_fetch_memory(
  "https://api.fda.gov/drug/label.json?search=openfda.brand_name:AMBIEN&limit=1",
  handle = h)
body <- jsonlite::fromJSON(rawToChar(resp$content))
cat("Has boxed_warning field:", !is.null(body$results$boxed_warning), "\n")
if (!is.null(body$results$boxed_warning)) {
  clean <- gsub("<[^>]+>", "", body$results$boxed_warning[[1]])
  clean <- trimws(gsub("\\s+", " ", clean))
  cat("BBW text (first 300 chars):", substr(clean, 1, 300), "\n")
  cat("Contains 'somnamb':", grepl("somnamb", tolower(clean)), "\n")
}
