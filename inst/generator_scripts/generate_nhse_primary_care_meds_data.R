# Generate synthetic primary care medications data
# This script creates fake primary care medication records for 5000 individuals

# Source shared PID generation function
source("generate_pids.R", local = TRUE)

set.seed(42)

required_cols <- c(
  "pid",
  "bsaprescriptionid",
  "chargestatus",
  "costcentreodscode_ofh",
  "costcentresubtype",
  "costcentretype",
  "dispensedpharmacytype",
  "itemactualcost",
  "itemid",
  "itemnic",
  "maternityexemptionflag",
  "outofhoursindicator",
  "paidbnfcode",
  "paidbnfname",
  "paiddisallowedindicator",
  "paiddrugstrength",
  "paidflavourindicator",
  "paidformulation",
  "paidindicator",
  "paidquantity",
  "paidsuppliername",
  "paiddmdcode",
  "prescribedbnfcode",
  "prescribedbnfname",
  "prescribedformulation",
  "prescribedmedicinestrength",
  "prescribedquantity",
  "prescribeddmdcode",
  "prescribertype",
  "privateprescriptionindicator",
  "processedperiod",
  "processingperioddate"
)


config <- getOption("OFH_GEN_CONFIG", NULL)
total_pid_count <- if (!is.null(config)) config$total_pid_count else 5000
all_study_pids <- getOption("OFH_ALL_STUDY_PIDS", generate_pids(total_pid_count))
dataset_cfg <- if (!is.null(config)) config$datasets$nhse_primcare_meds_data else NULL

n_people <- if (!is.null(dataset_cfg$unique_pids)) dataset_cfg$unique_pids else 5000
if (identical(n_people, "ALL")) {
  n_people <- total_pid_count
}
if (n_people > total_pid_count) {
  stop("nhse_primcare_meds_data unique_pids cannot exceed total_pid_count")
}

# Generate consistent alphanumeric PIDs
pids <- sample(all_study_pids, n_people)

# Common BNF medication codes with names and typical strengths
bnf_meds <- data.frame(
  BNFCode = c("0212000B0", "0212000Y0", "0212000AA", "0212000M0", "0212000ZZ",
              "0407010H0", "0407020Q0", "0407020A0", "0406020T0", "0409010A0", "0410000C0", "0407099X0",
              "0601023A0", "0601060D0", "0601012A0", "0601011L0", "0601090Y0",
              "0602010V0", "0602020D0", 
              "0603020T0", "0603020H0", "0603020W0", "0603099Z0",
              "0301011R0", "0302000N0", "0302000C0", "0304010A0", "0302099W0",
              "0403030E0", "0403010F0", "0401020K0",
              "0206010E0", "0205051J0", "0205010J0",
              "0704010T0", "1001030C0", "1001010P0",
              "0501012U0", "0501030P0",
              "0106010E0", "0106020C0", "9999999AA0", "9999999BB0", "9999999CC0"),
  BNFName = c("Atorvastatin 20 mg tablets", "Simvastatin 40 mg tablets", "Rosuvastatin 10 mg tablets", "Pravastatin 40 mg tablets", "Atorvastatin / Amlodipine tablets",
              "Amlodipine 5 mg tablets", "Ramipril 5 mg capsules", "Enalapril 10 mg tablets", "Bisoprolol 5 mg tablets", "Aspirin 75 mg tablets", "Clopidogrel 75 mg tablets", "Lisinopril / Hydrochlorothiazide tablets",
              "Metformin 500 mg tablets", "Gliclazide 80 mg tablets", "Insulin Aspart 100 units/ml prefilled pen", "Insulin Glargine 100 units/ml prefilled pen", "Sitagliptin / Metformin tablets",
              "Levothyroxine Sodium 100 mcg tablets", "Liothyronine 20 mcg tablets",
              "Omeprazole 20 mg capsules", "Lansoprazole 30 mg capsules", "Ranitidine 150 mg tablets", "Esomeprazole / Naproxen tablets",
              "Salbutamol 100 mcg metered dose inhaler", "Beclometasone 200 mcg metered dose inhaler", "Budesonide 200 mcg metered dose inhaler", "Montelukast 10 mg tablets", "Salmeterol / Fluticasone metered dose inhaler",
              "Paracetamol 500 mg tablets", "Ibuprofen 400 mg tablets", "Codeine Phosphate 30 mg tablets",
              "Warfarin 5 mg tablets", "Apixaban 5 mg tablets", "Rivaroxaban 20 mg tablets",
              "Estradiol 2 mg patches", "Tamoxifen 20 mg tablets", "Anastrozole 1 mg tablets",
              "Sertraline 50 mg tablets", "Fluoxetine 20 mg capsules",
              "Folic Acid 5 mg tablets", "Vitamin D 800 units capsules", "Nutritional Shake powder 57g sachets banana", "Nutritional Shake powder 57g sachets strawberry", "Nutritional Shake powder 57g sachets chocolate"),
  Strength = c("20 mg", "40 mg", "10 mg", "40 mg", "20 mg / 5 mg",
               "5 mg", "5 mg", "10 mg", "5 mg", "75 mg", "75 mg", "10 mg / 25 mg",
               "500 mg", "80 mg", "100 units/ml", "100 units/ml", "50 mg / 500 mg",
               "100 mcg", "20 mcg",
               "20 mg", "30 mg", "150 mg", "20 mg / 500 mg",
               "100 mcg", "200 mcg", "200 mcg", "10 mg", "50 microgram/dose / 25 microgram/dose",
               "500 mg", "400 mg", "30 mg",
               "5 mg", "5 mg", "20 mg",
               "2 mg", "20 mg", "1 mg",
               "50 mg", "20 mg",
               "5 mg", "800 units", NA, NA, NA),
  Formulation = c("tablets", "tablets", "tablets", "tablets", "tablets",
                  "tablets", "capsules", "tablets", "tablets", "tablets", "tablets", "tablets",
                  "tablets", "tablets", "injection", "injection", "tablets",
                  "tablets", "tablets",
                  "capsules", "capsules", "tablets", "tablets",
                  "inhaler", "inhaler", "inhaler", "tablets", "inhaler",
                  "tablets", "tablets", "tablets",
                  "tablets", "tablets", "tablets",
                  "patches", "tablets", "tablets",
                  "tablets", "capsules",
                  "tablets", "capsules", "sachets", "sachets", "sachets"),
  stringsAsFactors = FALSE
)

custom_bnf_codes <- NULL
custom_bnf_meds <- NULL
if (!is.null(config) &&
    !is.null(config$code_config) &&
    !is.null(config$code_config$nhse_primcare_meds_data)) {
  custom_bnf_codes <- config$code_config$nhse_primcare_meds_data$bnf_codes
  custom_bnf_meds <- config$code_config$nhse_primcare_meds_data$bnf_meds
}

if (!is.null(custom_bnf_meds)) {
  custom_bnf_meds <- as.data.frame(custom_bnf_meds, stringsAsFactors = FALSE)
  required_custom_cols <- c("BNFCode", "BNFName", "Formulation")
  if (!all(required_custom_cols %in% names(custom_bnf_meds))) {
    stop("Custom bnf_meds must include columns: BNFCode, BNFName, Formulation")
  }
  if (!"Strength" %in% names(custom_bnf_meds)) {
    custom_bnf_meds$Strength <- NA_character_
  }

  custom_bnf_meds$BNFCode <- trimws(as.character(custom_bnf_meds$BNFCode))
  custom_bnf_meds$BNFName <- trimws(as.character(custom_bnf_meds$BNFName))
  custom_bnf_meds$Formulation <- trimws(as.character(custom_bnf_meds$Formulation))
  custom_bnf_meds$Strength <- trimws(as.character(custom_bnf_meds$Strength))
  custom_bnf_meds$Strength[custom_bnf_meds$Strength == ""] <- NA_character_

  if (any(!nzchar(custom_bnf_meds$BNFCode) | !nzchar(custom_bnf_meds$BNFName) | !nzchar(custom_bnf_meds$Formulation))) {
    stop("Custom bnf_meds rows must include non-empty BNFCode, BNFName, and Formulation")
  }

  bnf_meds <- unique(custom_bnf_meds[, c("BNFCode", "BNFName", "Strength", "Formulation"), drop = FALSE])
} else if (!is.null(custom_bnf_codes)) {
  custom_bnf_codes <- as.character(custom_bnf_codes)
  custom_bnf_codes <- trimws(custom_bnf_codes)
  custom_bnf_codes <- unique(custom_bnf_codes[nzchar(custom_bnf_codes)])
}

if (!is.null(custom_bnf_codes) && length(custom_bnf_codes) > 0) {
  keep_idx <- bnf_meds$BNFCode %in% custom_bnf_codes
  if (!any(keep_idx)) {
    stop("No requested bnf_codes matched known BNFCode values in bnf_meds")
  }
  bnf_meds <- bnf_meds[keep_idx, , drop = FALSE]
}

get_pid_ods_code_map <- function(pid_values) {
  unique_p <- unique(pid_values)
  setNames(
    sprintf("%06d", sample(0:999999, length(unique_p), replace = FALSE)),
    unique_p
  )
}

sample_item_actual_cost <- function() {
  # Right-skewed with mean around 800 and rare high outliers.
  v <- rlnorm(1, meanlog = log(280), sdlog = 1.25)
  min(v, 1000000)
}

sample_prescribed_quantity <- function() {
  # Mostly around typical values near 80, with occasional very large outliers.
  if (runif(1) < 0.995) {
    return(as.integer(max(0, round(rlnorm(1, meanlog = log(55), sdlog = 0.7)))))
  }
  as.integer(sample(2000:300000, 1))
}

sample_processed_period <- function() {
  # Spec: number between 200k and 202k, centered near 202000.
  sample(200000:202999, 1, prob = dnorm(200000:202999, mean = 202000, sd = 350))
}

sample_processing_period_date <- function() {
  all_days <- seq(as.Date("2018-01-01"), as.Date("2025-09-01"), by = "day")
  as.character(sample(all_days, 1))
}

paiddmd_pool <- c(
  "1271511000001104 = Atorvastatin 20mg tablets 28 tablet (product)",
  "1068411000001105 = Omeprazole 20mg gastro resistant capsules 28 capsule (product)",
  "32194911000001105 = Ramipril 5mg capsules 28 capsule (product)",
  "31983411000001105 = Bisoprolol fumarate 5mg tablets 28 tablet (product)",
  "32014711000001107 = Aspirin 75mg dispersible tablets 28 tablet (product)",
  "31977611000001102 = Simvastatin 40mg tablets 28 tablet (product)",
  "39077811000001100 = Clopidogrel 75mg tablets 28 tablet (product)",
  "32223611000001109 = Amlodipine 5mg tablets 28 tablet (product)"
)

paid_formulation_pool <- c(
  "0069 = Tablet",
  "0282 = Oral tablet",
  "0020 = Gastra-resistant capsule",
  "0004 = Capsule",
  "0061 = Pressurised inhalation",
  "0042 = Modified-release tablet",
  "0204 = Oral capsule"
)
paid_formulation_prob <- c(0.34, 0.26, 0.20, 0.07, 0.05, 0.05, 0.03)

costcentresubtype_pool <- c(
  "- = Does not apply",
  "01 = Walk-in-Centre",
  "02 = Out-of-hours service",
  "03 = Walk-in-Centre and Out-of-hours service",
  "04 = GP Practice",
  "05 = Health & Justice",
  "06 = Private Controlled Drug practice",
  "07 = Other",
  "08 = Public health service",
  "09 = Community health service",
  "10 = Hospital service",
  "11 = Optometry service",
  "12 = Urgent & emergency care",
  "13 = Hospice",
  "14 = Care home/nursing home",
  "15 = PCN"
)
costcentresubtype_prob <- c(0.03, 0.03, 0.05, 0.02, 0.62, 0.02, 0.01, 0.03, 0.02, 0.04, 0.04, 0.01, 0.03, 0.02, 0.02, 0.01)

costcentretype_pool <- c(
  "15 = GENERAL PRACTITIONER",
  "16 = DENTAL PRACTITIONER",
  "18 = HOSPITAL DOCTOR",
  "22 = SCOTTISH LEAKED DATA BATCHES",
  "23 = WELSH LEAKED DATA BATCHES",
  "33 =NURSE",
  "34 = COMMUNITY NURSE PRESCRIBING CONTRACT",
  "48 = ADDITIONAL PRESCRIBER",
  "5 = HOSPITAL",
  "51 = PRIVATE DOCTOR",
  "52 = PRIVATE NURSE",
  "53 = PRIVATE ADDITIONAL PRESCRIBER",
  "54 = PRIVATE GROUP",
  "7 = GP PRACTICE / COST CENTRE",
  "8 = CONTRACTOR",
  "9 = DENTIST PRACTICE"
)
costcentretype_prob <- c(0.07, 0.02, 0.03, 0.003, 0.003, 0.08, 0.04, 0.03, 0.04, 0.01, 0.01, 0.005, 0.005, 0.62, 0.05, 0.04)

dispensedpharmacytype_pool <- costcentretype_pool
dispensedpharmacytype_prob <- c(0.05, 0.02, 0.04, 0.003, 0.003, 0.06, 0.03, 0.03, 0.08, 0.01, 0.01, 0.005, 0.005, 0.04, 0.62, 0.07)

prescribertype_pool <- costcentretype_pool
prescribertype_prob <- c(0.64, 0.03, 0.04, 0.004, 0.004, 0.08, 0.03, 0.03, 0.04, 0.01, 0.01, 0.005, 0.005, 0.04, 0.03, 0.025)

# Generate primary care medications data
# Each person can have multiple prescriptions
create_primcare_meds <- function(n_people) {
  records <- list()
  pid_ods_code_map <- get_pid_ods_code_map(pids)
  
  for (i in 1:n_people) {
    # Each person has 0-10 prescriptions
    n_prescriptions <- sample(0:10, 1, prob = c(0.2, 0.15, 0.15, 0.12, 0.10, 0.08, 0.07, 0.05, 0.04, 0.03, 0.01))
    
    if (n_prescriptions > 0) {
      for (j in 1:n_prescriptions) {
        med_idx <- sample(1:nrow(bnf_meds), 1)
        formulation <- bnf_meds$Formulation[med_idx]
        formulation_key <- tolower(trimws(formulation))
        was_dispensed <- runif(1) < 0.90
        
        # Set quantity based on formulation type
        if (formulation_key %in% c("tablet", "tablets", "capsule", "capsules")) {
          quantity <- sample(c(28, 30, 56, 60, 84, 90), 1)
        } else if (formulation_key %in% c("injection", "injections")) {
          quantity <- sample(c(1, 2, 3), 1)  # Pens or vials
        } else if (formulation_key %in% c("inhaler", "inhalers")) {
          quantity <- 1  # Single inhaler
        } else if (formulation_key %in% c("patch", "patches")) {
          quantity <- sample(c(4, 8, 12), 1)  # Box of patches
        } else if (formulation_key %in% c("sachet", "sachets")) {
          quantity <- sample(c(28, 56, 84), 1)  # Box of sachets
        } else {
          quantity <- sample(c(1, 2), 1)  # Generic fallback
        }
        
        records[[length(records) + 1]] <- data.frame(
          pid = pids[i],
          bsaprescriptionid = paste0("RX", sprintf("%010d", length(records) + 1)),
          chargestatus = if (was_dispensed) sample(c("- = Unknown", "C = Chargeable at Current Rate", "E = Exempt", "O = Chargeable at Previous Rate", "U = Unknown"), 1, prob = c(0.01, 0.12, 0.82, 0.01, 0.04)) else "U = Unknown",
          costcentreodscode_ofh = unname(pid_ods_code_map[pids[i]]),
          costcentresubtype = sample(costcentresubtype_pool, 1, prob = costcentresubtype_prob),
          costcentretype = sample(costcentretype_pool, 1, prob = costcentretype_prob),
          dispensedpharmacytype = if (was_dispensed) sample(dispensedpharmacytype_pool, 1, prob = dispensedpharmacytype_prob) else NA_character_,
          itemactualcost = if (was_dispensed) round(sample_item_actual_cost(), 2) else NA_real_,
          itemid = as.integer(sample(1:5, 1)),
          itemnic = if (was_dispensed) as.integer(sample(5:51846, 1)) else as.integer(NA),
          maternityexemptionflag = sample(c("0 = no", "1 = yes"), 1, prob = c(0.97, 0.03)),
          outofhoursindicator = as.integer(sample(c(0, 1), 1, prob = c(0.985, 0.015))),
          paidbnfcode = if (was_dispensed) bnf_meds$BNFCode[med_idx] else NA_character_,
          paidbnfname = if (was_dispensed) bnf_meds$BNFName[med_idx] else NA_character_,
          paiddisallowedindicator = if (was_dispensed) sample(c("N = no", "Y = yes"), 1, prob = c(0.995, 0.005)) else NA_character_,
          paiddrugstrength = if (was_dispensed) ifelse(is.na(bnf_meds$Strength[med_idx]), NA, bnf_meds$Strength[med_idx]) else NA_character_,
          paidflavourindicator = if (was_dispensed) sample(c("- = UNKNOWN", "0057 = Tutti frutti", "0070 = Peppermint", "0002 = Aniseed", "0040 = Orange", "0030 = Lemon", "0047 = Plain", "0031 = Lemon & lime"), 1, prob = c(0.84, 0.02, 0.02, 0.01, 0.03, 0.03, 0.03, 0.02)) else NA_character_,
          paidformulation = if (was_dispensed) sample(paid_formulation_pool, 1, prob = paid_formulation_prob) else NA_character_,
          paidindicator = if (was_dispensed) "Y = yes" else "N = no",
          paidquantity = if (was_dispensed) as.integer(quantity) else as.integer(NA),
          paidsuppliername = if (was_dispensed) sample(c("Generic Supplier", "Chiesi Ltf", "3M Health Care Ltd", "A A H Pharmaceuticals Ltd", "Abbot Laboratories Ltd", "Essential Healthcare Ltd"), 1, prob = c(0.62, 0.06, 0.06, 0.12, 0.08, 0.06)) else NA_character_,
          paiddmdcode = if (was_dispensed) sample(paiddmd_pool, 1) else NA_character_,
          prescribedbnfcode = paste0(bnf_meds$BNFCode[med_idx], paste0(sample(c(LETTERS, 0:9), 6, replace = TRUE), collapse = "")),
          prescribedbnfname = bnf_meds$BNFName[med_idx],
          prescribedformulation = bnf_meds$Formulation[med_idx],
          prescribedmedicinestrength = ifelse(is.na(bnf_meds$Strength[med_idx]), NA, bnf_meds$Strength[med_idx]),
          prescribedquantity = sample_prescribed_quantity(),
          prescribeddmdcode = sample(paiddmd_pool, 1),
          prescribertype = sample(prescribertype_pool, 1, prob = prescribertype_prob),
          privateprescriptionindicator = as.integer(sample(c(0, 1), 1, prob = c(0.98, 0.02))),
          processedperiod = sample_processed_period(),
          processingperioddate = sample_processing_period_date(),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(records) == 0) {
    empty <- as.data.frame(setNames(replicate(length(required_cols), logical(0), simplify = FALSE), required_cols), stringsAsFactors = FALSE)
    return(empty)
  }

  out <- do.call(rbind, records)
  # Keep stable column order per output spec.
  out <- out[, required_cols, drop = FALSE]
  out
}

nhse_eng_primcare_meds <- create_primcare_meds(n_people)

# Save to CSV
local({
  p <- "../data/nhse_primcare_meds_data.csv"
  con <- file(p, "wb"); writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con); close(con)
  suppressWarnings(write.table(nhse_eng_primcare_meds, file = p, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE, qmethod = "double", na = "NA", fileEncoding = "UTF-8"))
})
unique_pids_meds <- length(unique(nhse_eng_primcare_meds$pid))
message("Generated nhse_primcare_meds_data.csv")
message(sprintf("%d rows and %d columns (%d unique PIDs with %d prescriptions)", nrow(nhse_eng_primcare_meds), ncol(nhse_eng_primcare_meds), unique_pids_meds, nrow(nhse_eng_primcare_meds)))
