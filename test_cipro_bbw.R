library(curl)
library(jsonlite)
h <- new_handle()
handle_setopt(h, timeout = 10L)
resp <- curl_fetch_memory("https://api.fda.gov/drug/label.json?search=openfda.brand_name:CIPRO&limit=1", handle = h)
body <- fromJSON(rawToChar(resp$content))
bbw <- body$results$boxed_warning
if (!is.null(bbw)) {
  clean <- gsub("<[^>]+>", "", bbw[[1]])
  clean <- trimws(gsub("\\s+", " ", clean))
  cat("BBW found, first 500 chars:\n")
  cat(substr(clean, 1, 500), "\n")
  cat("\nContains 'tendon':", grepl("tendon", tolower(clean)), "\n")
  cat("Contains 'tendonitis':", grepl("tendonitis", tolower(clean)), "\n")
  cat("Contains 'tendinitis':", grepl("tendinitis", tolower(clean)), "\n")
  cat("Contains 'rupture':", grepl("rupture", tolower(clean)), "\n")
} else {
  cat("No BBW found\n")
}
