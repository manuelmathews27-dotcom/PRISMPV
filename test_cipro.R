setwd("C:/Users/manuz/Desktop/signal-to-label")
source("R/utils.R")
library(dplyr)
library(lubridate)
library(curl)

# Extract pull_live_signal from app.R
source_lines <- readLines("app.R")
start <- grep("^pull_live_signal <- function", source_lines)
brace_count <- 0
end <- start
for (i in start:length(source_lines)) {
  brace_count <- brace_count + sum(charToRaw(source_lines[i]) == charToRaw("{")) -
                                sum(charToRaw(source_lines[i]) == charToRaw("}"))
  if (brace_count == 0) { end <- i; break }
}
eval(parse(text = source_lines[start:end]))

signal_status <- function(df) {
  recent <- tail(df, 6)
  n_sig <- sum(recent$signal_met, na.rm = TRUE)
  if (n_sig >= 2) "CONFIRMED"
  else if (n_sig == 1) "EMERGING"
  else "NOT DETECTED"
}

cat("=== CIPRO + palpitations ===\n")
df <- pull_live_signal("CIPRO", "palpitations", 10)
cat("Quarters:", nrow(df), "\n\n")
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
