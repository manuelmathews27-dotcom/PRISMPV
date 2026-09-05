# pt_terms.R — curated MedDRA Preferred Terms for the Monitor dropdown
# Selected for regulatory relevance: serious, unexpected, life-threatening, or
# historically linked to FDA action.
# Sourced automatically by Shiny before app.R (all files in R/ are).

# ── MedDRA Preferred Terms — curated for regulatory signal detection ──────────
# Criteria: serious, unexpected, life-threatening, or historically led to FDA
# action (BBW, contraindication, withdrawal). Excludes common pharmacological
# effects (nausea, headache, dizziness) that rarely trigger regulatory action.
# CORRECTED 2026-09-05 — three entries were not MedDRA Preferred Terms at all.
# They returned results only because the query matched substrings of real PTs;
# under exact-field matching they return ZERO:
#   "stroke"                   -> not a PT. Replaced with "ischaemic stroke"
#                                 ("cerebrovascular accident" and "haemorrhagic
#                                  stroke" were already listed, so "stroke" was
#                                  also silently double-counting both).
#   "intracranial haemorrhage" -> MedDRA inverts it: "haemorrhage intracranial".
#   "malignant neoplasm"       -> MedDRA inverts it: "neoplasm malignant".
# None of the three is a reference-cohort pair, so this does not disturb
# combined.rds; it only changes what the Monitor dropdown can query.
pt_terms <- sort(c(
  # ── Cardiac ──
  "myocardial infarction", "cardiac arrest", "cardiac failure",
  "ventricular tachycardia", "ventricular fibrillation",
  "electrocardiogram QT prolonged", "torsade de pointes",
  "cardiomyopathy", "myocarditis", "cardiac tamponade", "sudden death",
  # ── Vascular / Thromboembolic ──
  "pulmonary embolism", "deep vein thrombosis", "thrombosis",
  "ischaemic stroke", "cerebrovascular accident", "haemorrhagic stroke",
  "hypertensive crisis", "shock", "circulatory collapse", "vasculitis",
  # The boxed warning on every DOAC (Xarelto, Eliquis, Savaysa) and on Plavix.
  # MedDRA spells it haematoma; the FDA labels spell it "spinal/epidural
  # hematoma", so the synonym map below bridges the two.
  "spinal cord haematoma", "spinal epidural haematoma",
  # ── Hepatic ──
  "hepatic failure", "drug-induced liver injury", "hepatitis",
  "hepatotoxicity", "hepatic necrosis", "jaundice", "cholestasis",
  # ── Renal ──
  "acute kidney injury", "renal failure", "nephrotic syndrome",
  "tubulointerstitial nephritis", "renal tubular necrosis",
  # ── Neurological ──
  "seizure", "status epilepticus",
  # MedDRA inverts this one: the PT is "neuropathy peripheral". The natural
  # phrasing "peripheral neuropathy" matches only 18 FAERS reports vs 93,293
  # for the correct form -- it was masked until queries became phrase-matched.
  "neuropathy peripheral",
  "Guillain-Barre syndrome", "progressive multifocal leukoencephalopathy",
  "encephalopathy", "cerebral haemorrhage", "haemorrhage intracranial",
  "demyelination", "encephalitis", "tardive dyskinesia",
  # ── Neuropsychiatric ──
  "suicidal ideation", "suicide attempt", "completed suicide",
  "psychotic disorder", "hallucination",
  "pathological gambling", "somnambulism",
  "serotonin syndrome", "neuroleptic malignant syndrome",
  # ── Respiratory ──
  "interstitial lung disease", "pneumonitis", "pulmonary fibrosis",
  "respiratory failure", "acute respiratory distress syndrome",
  "pulmonary hypertension",
  # ── Gastrointestinal ──
  "gastrointestinal haemorrhage", "gastrointestinal perforation",
  "pancreatitis", "pancreatitis acute",
  "intestinal obstruction", "clostridium difficile colitis",
  # ── Musculoskeletal ──
  "rhabdomyolysis", "tendon rupture", "tendonitis",
  "osteonecrosis of jaw", "osteonecrosis",
  "pathological fracture", "amputation",
  # ── Skin ──
  "Stevens-Johnson syndrome", "toxic epidermal necrolysis",
  "drug reaction with eosinophilia and systemic symptoms",
  "angioedema", "alopecia",
  # ── Endocrine / Metabolic ──
  "diabetic ketoacidosis", "lactic acidosis",
  "hypoglycaemia", "adrenal insufficiency",
  "diabetes mellitus", "thyroid cancer",
  # ── Haematological ──
  "agranulocytosis", "pancytopenia", "aplastic anaemia",
  "thrombotic thrombocytopenic purpura", "haemolytic uraemic syndrome",
  "disseminated intravascular coagulation", "febrile neutropenia",
  # ── Immune / Allergic ──
  "anaphylactic reaction", "anaphylactic shock",
  "cytokine release syndrome", "systemic lupus erythematosus",
  # ── Infectious ──
  "tuberculosis", "sepsis", "septic shock",
  "opportunistic infection",
  # ── Oncology ──
  "bladder cancer", "lymphoma", "hepatocellular carcinoma",
  "neoplasm malignant", "skin cancer",
  # ── Ocular ──
  "blindness", "optic neuritis", "retinal detachment",
  # ── General ──
  "death", "multiple organ dysfunction syndrome",
  "drug interaction", "drug dependence"
))
