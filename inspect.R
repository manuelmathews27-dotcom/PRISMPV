library(dplyr)
df <- readRDS("data/faers_raw.rds") |>
  mutate(PRR = (count_a / pmax(count_b,1)) / (pmax(count_c,1) / pmax(count_d,1)))

# Plavix scale info
plavix <- df |> filter(drug == "PLAVIX", !is.na(PRR), is.finite(PRR))
cat("PLAVIX: count_a max =", max(plavix$count_a, na.rm=T),
    " PRR max =", round(max(plavix$PRR, na.rm=T), 2), "\n")
cat("sf =", max(plavix$count_a, na.rm=T) / max(max(plavix$PRR, na.rm=T), 2.5), "\n")
cat("thresh_y (2*sf) =", 2 * max(plavix$count_a, na.rm=T) / max(max(plavix$PRR, na.rm=T), 2.5), "\n")
cat("signal label y (0.65 * count_max) =", 0.65 * max(plavix$count_a, na.rm=T), "\n\n")

# Combined median lag explanation
combined <- readRDS("data/combined.rds")
cat("Lag per drug:\n")
combined |> select(drug_name, signal_start_date, label_change_date, lag_months) |> print()
