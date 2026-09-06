# cohort_data.R — reference cohort load, class remap, and derived lookups
# Loads the pipeline artifacts and shapes them for the app: therapeutic-class
# reconciliation, detection-limitation notes, and the drug -> class lookup.
# Sourced automatically by Shiny before app.R (all files in R/ are).

# ── Load historical reference data ────────────────────────────────────────────
if (!file.exists("data/combined.rds") || !file.exists("data/faers_raw.rds"))
  stop("Data files missing. Run run_pipeline.R first to generate data/faers_raw.rds and data/combined.rds")
combined  <- readRDS("data/combined.rds")

# -- Therapeutic class reclassification (runtime remap) -----------------------
# The classes baked into combined.rds are therapeutic AREAS, not mechanistic
# classes. "Antidiabetic" lumped a TZD, an SGLT2 and a DPP-4 inhibitor;
# "Antithrombotic" mixed two Factor Xa inhibitors with a direct thrombin
# inhibitor and a P2Y12 antiplatelet. Averaging a signal-to-label lag across
# unrelated mechanisms makes the class-specific timeline estimate meaningless.
#
# STATUS 2026-08-26: the mechanistic classes are now written into
# data/label_changes.csv, so a freshly built combined.rds already carries them and
# this remap is a NO-OP. It is kept deliberately as a safety net, because the .rds
# in the repo may still predate that CSV edit (the 2026-08-26 refresh rebuilt it
# from the OLD classes). Once a pipeline run happens after this commit, the
# reconciliation message below will confirm zero rows were remapped and this whole
# block can be deleted.
#
# Splitting drops some classes below the timeline model's 3-drug minimum; those
# fall back to the all-drug estimate, which is the correct behaviour -- a
# prediction from a fabricated class is worse than no class-specific prediction.
CLASS_REMAP <- setNames(
  c("HMG-CoA Reductase Inhibitor","HMG-CoA Reductase Inhibitor",
    "HMG-CoA Reductase Inhibitor","HMG-CoA Reductase Inhibitor",
    "Proton Pump Inhibitor","Proton Pump Inhibitor",
    "Proton Pump Inhibitor","Proton Pump Inhibitor",
    "TNF-alpha Inhibitor","TNF-alpha Inhibitor",
    "TNF-alpha Inhibitor","TNF-alpha Inhibitor",
    "Fluoroquinolone","Fluoroquinolone","Fluoroquinolone","Fluoroquinolone",
    "Bisphosphonate","Bisphosphonate","Bisphosphonate","Bisphosphonate",
    "Atypical Antipsychotic","Atypical Antipsychotic",
    "Atypical Antipsychotic","Atypical Antipsychotic",
    "Nonbenzodiazepine Z-drug","Nonbenzodiazepine Z-drug",
    "Nonbenzodiazepine Z-drug","Nonbenzodiazepine Z-drug",
    "COX-2 Selective NSAID","COX-2 Selective NSAID","COX-2 Selective NSAID",
    "PPAR-gamma Agonist (TZD)","PPAR-gamma Agonist (TZD)",
    "Factor Xa Inhibitor","Factor Xa Inhibitor",
    "JAK Inhibitor","JAK Inhibitor","JAK Inhibitor",
    "CAR-T Cell Therapy","CAR-T Cell Therapy",
    "CAR-T Cell Therapy","CAR-T Cell Therapy"),
  c("atorvastatin","rosuvastatin","simvastatin","pravastatin",
    "omeprazole","esomeprazole","lansoprazole","pantoprazole",
    "adalimumab","etanercept","infliximab","certolizumab pegol",
    "ciprofloxacin","levofloxacin","moxifloxacin","ofloxacin",
    "alendronate","risedronate","ibandronate","zoledronic acid",
    "aripiprazole","risperidone","quetiapine","olanzapine",
    "zolpidem","eszopiclone","zaleplon","zolpidem sublingual",
    "celecoxib","rofecoxib","meloxicam",
    "pioglitazone","rosiglitazone",
    "apixaban","rivaroxaban",
    "tofacitinib","baricitinib","upadacitinib",
    "axicabtagene ciloleucel","tisagenlecleucel",
    "lisocabtagene maraleucel","idecabtagene vicleucel")
)
local({
  g   <- tolower(trimws(combined$generic_name))
  hit <- g %in% names(CLASS_REMAP)
  if (any(!hit)) message("[PRISM] class remap: no mapping for ",
                         paste(unique(combined$generic_name[!hit]), collapse = ", "))
  # Report whether the .rds still disagrees with the CSV, so this block's job is
  # visible in the log rather than silent. n_stale == 0 means the data is already
  # correct and the remap can be removed.
  n_stale <- sum(hit & combined$therapeutic_class != unname(CLASS_REMAP[g]), na.rm = TRUE)
  if (n_stale > 0) {
    message("[PRISM] class remap: corrected ", n_stale,
            " row(s) whose .rds classes predate data/label_changes.csv")
  } else {
    message("[PRISM] class remap: no-op (combined.rds already matches the CSV) ",
            "-- safe to delete CLASS_REMAP")
  }
  combined$therapeutic_class[hit] <<- unname(CLASS_REMAP[g[hit]])
})
faers_raw <- readRDS("data/faers_raw.rds")

# Load pipeline provenance (graceful fallback if not yet generated)
provenance <- if (file.exists("data/provenance.rds")) readRDS("data/provenance.rds") else NULL

# compute_prr() returns the statistics but not the pass/fail verdict, so apply
# the Evans + Rothman criteria here — once, at the source. The Reference Cohort
# drill-down colours points by this, and deriving it per-consumer would risk the
# two drifting apart.
signals <- compute_prr(faers_raw) |>
  dplyr::mutate(signal_met = check_signal(count_a, PRR, chi_sq, PRR_lo))

# Pre-compute detection limitation notes for flagged classes
detection_notes <- list(
  Bisphosphonate = list(
    short = "Literature-detected (dental case series)",
    long  = "Literature-detected signal \u2014 ONJ was identified from dental case series,
             not FAERS spontaneous reports. FAERS signal lagged the label change."
  ),
  Antipsychotic = list(
    short = "Trial-based (placebo-controlled RCTs)",
    long  = "Trial-based signal \u2014 BBW was driven by 17 placebo-controlled clinical trials
             showing excess mortality in elderly dementia patients. FAERS cannot stratify
             by age or indication, making this risk structurally undetectable via
             disproportionality analysis."
  ),
  PPI = list(
    short = "No FAERS signal (class-wide failure)",
    long  = "Class-wide detection failure \u2014 no PPI in the cohort generated a FAERS
             disproportionality signal for C. difficile colitis."
  )
)

get_detection_type <- function(tc, ae) {
  if (tc == "Bisphosphonate") "Bisphosphonate"
  else if (tc == "Atypical Antipsychotic" && grepl("mortality|death", ae, ignore.case = TRUE)) "Antipsychotic"
  else if (tc == "Proton Pump Inhibitor") "PPI"
  else NULL
}

detection_alert <- function(type) {
  if (is.null(type)) return(NULL)
  tags$div(
    class = "alert alert-warning py-1 px-2 mt-2 mb-0",
    style = "font-size:0.78rem;",
    icon("triangle-exclamation"),
    detection_notes[[type]]$long
  )
}

combined <- combined |>
  rowwise() |>
  mutate(Note = {
    dt <- get_detection_type(therapeutic_class, adverse_event)
    if (is.null(dt)) "" else detection_notes[[dt]]$short
  }) |>
  ungroup()

# Benchmark stats from cohort (drugs where FAERS signal preceded label change)
benchmark_drugs <- combined |> filter(!is.na(lag_months), lag_months > 0)

# Drug-to-class lookup for matching queried drugs to reference cohort classes
# Includes cohort drugs + common related drugs users might query
drug_class_map <- c(
  # Kept in sync with CLASS_REMAP above -- a live query only gets a class-specific
  # benchmark if the name it maps to also exists in the reclassified cohort.
  # PPAR-gamma / SGLT2 / DPP-4 / GLP-1 (cohort: Avandia, Actos | Invokana | Januvia | none)
  "AVANDIA" = "PPAR-gamma Agonist (TZD)", "ACTOS" = "PPAR-gamma Agonist (TZD)",
  "INVOKANA" = "SGLT2 Inhibitor", "JARDIANCE" = "SGLT2 Inhibitor",
  "FARXIGA" = "SGLT2 Inhibitor",
  "JANUVIA" = "DPP-4 Inhibitor", "TRADJENTA" = "DPP-4 Inhibitor",
  # GLP-1 RA -- no cohort members yet, so these fall back to the all-drug
  # benchmark. Add a GLP-1 cohort at the next refresh.
  "OZEMPIC" = "GLP-1 Receptor Agonist", "WEGOVY" = "GLP-1 Receptor Agonist",
  "VICTOZA" = "GLP-1 Receptor Agonist", "TRULICITY" = "GLP-1 Receptor Agonist",
  "MOUNJARO" = "GLP-1 Receptor Agonist", "ZEPBOUND" = "GLP-1 Receptor Agonist",
  "BYETTA" = "GLP-1 Receptor Agonist", "SAXENDA" = "GLP-1 Receptor Agonist",
  # HMG-CoA Reductase Inhibitor (cohort: Zocor, Lipitor, Crestor, Pravachol)
  "ZOCOR" = "HMG-CoA Reductase Inhibitor", "LIPITOR" = "HMG-CoA Reductase Inhibitor",
  "CRESTOR" = "HMG-CoA Reductase Inhibitor", "PRAVACHOL" = "HMG-CoA Reductase Inhibitor",
  "LESCOL" = "HMG-CoA Reductase Inhibitor", "LIVALO" = "HMG-CoA Reductase Inhibitor",
  "ALTOPREV" = "HMG-CoA Reductase Inhibitor",
  # Fluoroquinolone (cohort: Cipro, Levaquin, Avelox, Floxin)
  "CIPRO" = "Fluoroquinolone", "LEVAQUIN" = "Fluoroquinolone",
  "AVELOX" = "Fluoroquinolone", "FLOXIN" = "Fluoroquinolone",
  # Atypical Antipsychotic (cohort: Abilify, Seroquel, Zyprexa, Risperdal)
  "ABILIFY" = "Atypical Antipsychotic", "SEROQUEL" = "Atypical Antipsychotic",
  "ZYPREXA" = "Atypical Antipsychotic", "RISPERDAL" = "Atypical Antipsychotic",
  "CLOZARIL" = "Atypical Antipsychotic", "GEODON" = "Atypical Antipsychotic",
  "LATUDA" = "Atypical Antipsychotic",
  # NSAID, split by COX selectivity (cohort: Celebrex, Vioxx, Mobic | Voltaren)
  "CELEBREX" = "COX-2 Selective NSAID", "VIOXX" = "COX-2 Selective NSAID",
  "MOBIC" = "COX-2 Selective NSAID",
  "VOLTAREN" = "Nonselective NSAID", "ADVIL" = "Nonselective NSAID",
  "ALEVE" = "Nonselective NSAID", "NAPROSYN" = "Nonselective NSAID",
  # Proton Pump Inhibitor (cohort: Nexium, Prilosec, Prevacid, Protonix)
  "NEXIUM" = "Proton Pump Inhibitor", "PRILOSEC" = "Proton Pump Inhibitor",
  "PREVACID" = "Proton Pump Inhibitor", "PROTONIX" = "Proton Pump Inhibitor",
  "DEXILANT" = "Proton Pump Inhibitor", "ACIPHEX" = "Proton Pump Inhibitor",
  # TNF-alpha Inhibitor (cohort: Humira, Enbrel, Remicade, Cimzia)
  # STELARA and DUPIXENT were previously listed here and are NOT TNF inhibitors:
  # ustekinumab is anti-IL-12/23 and dupilumab is anti-IL-4Ralpha. Corrected --
  # they were being benchmarked against an unrelated mechanism.
  "HUMIRA" = "TNF-alpha Inhibitor", "ENBREL" = "TNF-alpha Inhibitor",
  "REMICADE" = "TNF-alpha Inhibitor", "CIMZIA" = "TNF-alpha Inhibitor",
  "SIMPONI" = "TNF-alpha Inhibitor",
  "STELARA" = "IL-12/23 Inhibitor", "SKYRIZI" = "IL-23 Inhibitor",
  "DUPIXENT" = "IL-4R-alpha Inhibitor",
  # Bisphosphonate (cohort: Fosamax, Actonel, Boniva, Reclast)
  "FOSAMAX" = "Bisphosphonate", "ACTONEL" = "Bisphosphonate",
  "BONIVA" = "Bisphosphonate", "RECLAST" = "Bisphosphonate",
  # Anticoagulant / antiplatelet, split by mechanism
  # (cohort: Xarelto, Eliquis | Pradaxa | Plavix)
  "XARELTO" = "Factor Xa Inhibitor", "ELIQUIS" = "Factor Xa Inhibitor",
  "SAVAYSA" = "Factor Xa Inhibitor",
  "PRADAXA" = "Direct Thrombin Inhibitor",
  "PLAVIX" = "P2Y12 Inhibitor", "BRILINTA" = "P2Y12 Inhibitor",
  "EFFIENT" = "P2Y12 Inhibitor",
  # JAK Inhibitor (cohort: Xeljanz, Olumiant, Rinvoq)
  "XELJANZ" = "JAK Inhibitor", "OLUMIANT" = "JAK Inhibitor",
  "RINVOQ" = "JAK Inhibitor", "CIBINQO" = "JAK Inhibitor",
  "JAKAFI" = "JAK Inhibitor", "OPZELURA" = "JAK Inhibitor",
  # CAR-T Cell Therapy (cohort: Yescarta, Kymriah, Breyanzi, Abecma)
  "YESCARTA" = "CAR-T Cell Therapy", "KYMRIAH" = "CAR-T Cell Therapy",
  "BREYANZI" = "CAR-T Cell Therapy", "ABECMA" = "CAR-T Cell Therapy",
  "CARVYKTI" = "CAR-T Cell Therapy", "TECARTUS" = "CAR-T Cell Therapy",
  # Nonbenzodiazepine Z-drug (cohort: Ambien, Lunesta, Sonata, Intermezzo)
  "AMBIEN" = "Nonbenzodiazepine Z-drug", "LUNESTA" = "Nonbenzodiazepine Z-drug",
  "SONATA" = "Nonbenzodiazepine Z-drug", "INTERMEZZO" = "Nonbenzodiazepine Z-drug",
  # Not Z-drugs: suvorexant is an orexin antagonist, doxepin a TCA,
  # ramelteon a melatonin agonist. Separated so they are not benchmarked as Z-drugs.
  "BELSOMRA" = "Orexin Receptor Antagonist", "SILENOR" = "Sedative Antidepressant",
  "ROZEREM" = "Melatonin Receptor Agonist"
)

drug_choices <- sort(unique(signals$drug))
