library(jsonlite)

# Test without quotes (works for single-word drug names)
url2 <- "https://api.fda.gov/drug/event.json?search=patient.drug.openfda.generic_name:rosiglitazone&limit=1"
cat("Test - no quotes:\n", url2, "\n")
r2 <- fromJSON(url2)
cat("Result:", r2$meta$results$total, "\n\n")

# Multi-word term with + instead of space (no quotes)
url3 <- "https://api.fda.gov/drug/event.json?search=patient.reaction.reactionmeddrapt:myocardial+infarction&limit=1"
cat("Test - multi-word event:\n", url3, "\n")
r3 <- fromJSON(url3)
cat("Result:", r3$meta$results$total, "\n\n")

# Combined drug + event + date
url4 <- "https://api.fda.gov/drug/event.json?search=patient.drug.openfda.generic_name:rosiglitazone+AND+patient.reaction.reactionmeddrapt:myocardial+infarction+AND+receivedate:[20070101+TO+20070331]&limit=1"
cat("Test - full query:\n", url4, "\n")
r4 <- fromJSON(url4)
cat("Result:", r4$meta$results$total, "\n")
