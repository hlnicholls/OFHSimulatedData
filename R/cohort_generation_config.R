#' Default dataset proportions for OFH generation
#'
#' @return A named list of dataset proportions.
#' @export
ofh_default_proportions <- function() {
  list(
    clinic_measurements = 0.70,
    country_region = 0.70,
    nhse_outpat = 0.40,
    nhse_inpat = 0.40,
    nhse_ed = 0.40,
    nhse_primcare_meds = 0.60,
    nhse_engwal_deaths = 0.04
  )
}

#' Default record multipliers for multi-record datasets
#'
#' @return A named list of record multipliers.
#' @export
ofh_default_record_multipliers <- function() {
  list(
    nhse_outpat = 1.60,
    nhse_inpat = 1.50,
    nhse_ed = 1.60
  )
}

#' Default clinical code configuration
#'
#' @return Nested list of default coding pools and generation options.
#' @export
ofh_default_code_config <- function() {
  list(
    nhse_outpat_data = list(
      icd10_descriptions = c(
        I210 = "STEMI of anterolateral wall",
        I214 = "Acute subendocardial myocardial infarction",
        I219 = "Heart attack of unspecified site",
        I250 = "Atherosclerosis of coronary artery",
        I251 = "Atherosclerotic heart disease of native coronary artery",
        I500 = "Congestive heart failure",
        I501 = "Left ventricular failure",
        I509 = "Heart failure, unspecified",
        I420 = "Dilated cardiomyopathy",
        I350 = "Aortic stenosis",
        E110 = "Type 2 diabetes with hyperosmolarity",
        E780 = "Pure hypercholesterolaemia",
        J449 = "COPD, unspecified",
        E660 = "Obesity due to excess calories",
        R69X = "Unknown causes of morbidity"
      ),
      icd10_weights = c(),
      opcs4_descriptions = c(
        K401 = "Percutaneous transluminal balloon angioplasty of coronary artery",
        K451 = "Insertion of drug-eluting stent into coronary artery",
        K561 = "Repair of heart valve",
        M011 = "Kidney transplant",
        E033 = "Endoscopic biopsy of oesophagus",
        L436 = "Diagnostic endoscopy of large intestine"
      ),
      opcs4_weights = c(),
      diag_4_02_missing_prob = 0.85,
      diag_4_03_missing_prob = 0.98,
      opertn_01_no_procedure_prob = 0.92,
      opertn_02_missing_prob = 0.97
    ),
    nhse_inpat_data = list(
      icd10_descriptions = c(
        I21 = "Heart attack",
        I210 = "STEMI of anterolateral wall",
        I214 = "Acute subendocardial myocardial infarction",
        I251 = "Atherosclerotic heart disease of native coronary artery",
        I500 = "Congestive heart failure",
        I639 = "Cerebral infarction, unspecified",
        I480 = "Paroxysmal atrial fibrillation",
        I350 = "Aortic stenosis",
        I822 = "Thrombophlebitis of deep vessels of lower extremities",
        I349 = "Viral infection, unspecified",
        E110 = "Type 2 diabetes with hyperosmolarity",
        E789 = "High cholesterol, unspecified",
        N180 = "Stage 3a chronic kidney disease",
        J440 = "Chronic obstructive pulmonary disease with acute lower respiratory infection",
        E660 = "Obesity due to excess calories"
      ),
      icd10_weights = c(),
      opcs4_descriptions = c(
        K401 = "Percutaneous transluminal balloon angioplasty of coronary artery",
        K451 = "Insertion of drug-eluting stent into coronary artery",
        K561 = "Repair of heart valve",
        K562 = "Replacement of heart valve",
        K011 = "Heart transplant",
        M011 = "Kidney transplant",
        E033 = "Endoscopic biopsy of oesophagus"
      ),
      opcs4_weights = c(),
      single_diag_prob = 0.70,
      opertn_01_missing_prob = 0.90,
      opertn_02_missing_prob = 0.97
    ),
    nhse_engwal_deaths_data = list(
      icd10_descriptions = c(
        I210 = "STEMI of anterolateral wall",
        I214 = "Acute subendocardial myocardial infarction",
        I219 = "Heart attack of unspecified site",
        I500 = "Congestive heart failure",
        I501 = "Left ventricular failure",
        I509 = "Heart failure, unspecified",
        I639 = "Cerebral infarction, unspecified",
        I350 = "Aortic stenosis",
        I822 = "Thrombophlebitis of deep vessels of lower extremities",
        E110 = "Type 2 diabetes with hyperosmolarity",
        E111 = "Type 2 diabetes with ketoacidosis",
        C349 = "Lung cancer, unspecified",
        C67 = "Malignant neoplasm of bladder",
        C80 = "Malignant neoplasm, unspecified",
        J449 = "COPD, unspecified",
        K746 = "Unspecified cirrhosis of liver",
        N189 = "Chronic kidney disease, unspecified",
        J189 = "Pneumonia, unspecified",
        A419 = "Sepsis, unspecified",
        R99 = "Ill-defined and unknown cause of mortality"
      ),
      primary_icd10_weights = c(),
      underlying_icd10_weights = c(),
      s_cod_code_2_missing_prob = 0.60,
      s_cod_code_3_missing_prob = 0.80,
      s_cod_code_4_missing_prob = 0.90
    ),
    nhse_ed_data = list(
      ae_specific_codes = c(
        "01 = Brain", "02 = Head", "03 = Face", "04 = Eye", "05 = Ear",
        "06 = Nose", "07 = Mouth,Jaw,Teeth", "08 = Throat", "09 = Neck",
        "10 = Shoulder", "11 = Axilla", "12 = Upper Arm", "13 = Elbow",
        "14 = Forearm", "15 = Wrist", "16 = Hand", "17 = Digit",
        "18 = Cervical spine", "19 = Thoracic", "20 = Lumbosacral spine",
        "21 = Pelvis", "22 = Chest", "23 = Breast", "24 = Abdomen",
        "25 = Back/buttocks", "26 = Ano/rectal", "27 = Genitalia",
        "28 = Hip", "29 = Groin", "30 = Thigh", "31 = Knee",
        "32 = Lower leg", "33 = Ankle", "34 = Foot", "35 = Toe", "36 = Multiple site"
      ),
      ae_specific_weights = c(),
      icd10_pool = c("I210", "I509", "I48", "J440", "E119", "R10", "R07"),
      icd10_weights = c(),
      read_codes_pool = c("S01.5", "S011", "S02", "S03", "S10", "S20"),
      read_weights = c(),
      diagscheme_levels = c(
        "01 = Accident & Emergency diagnoses",
        "02 = ICD-10",
        "04 = Read coded clinical terms version 2",
        "Null"
      ),
      diagscheme_probs = c(0.55, 0.30, 0.10, 0.05),
      ae_scheme_icd10_prob = 0.25,
      diag_missing_probs = c(0, 0.80, 0.90, 0.95, 0.96, 0.97, 0.98, 0.98, 0.99, 0.99, 0.99, 0.99)
    )
  )
}

.ofh_merge_lists <- function(base, override) {
  if (length(override) == 0) return(base)
  out <- base
  for (nm in names(override)) {
    if (nm %in% names(base) && is.list(base[[nm]]) && is.list(override[[nm]])) {
      out[[nm]] <- .ofh_merge_lists(base[[nm]], override[[nm]])
    } else {
      out[[nm]] <- override[[nm]]
    }
  }
  out
}

#' Build a complete generation configuration
#'
#' @param n Total cohort size.
#' @param proportions Dataset proportion list.
#' @param record_multipliers Multipliers for multi-record datasets.
#' @param code_config User overrides for coding configuration.
#' @return Generation configuration list.
#' @export
ofh_build_config <- function(
  n = 5000,
  proportions = ofh_default_proportions(),
  record_multipliers = ofh_default_record_multipliers(),
  code_config = list()
) {
  stopifnot(is.numeric(n), length(n) == 1, !is.na(n), n > 0)
  n <- as.integer(n)

  ds_n <- function(p) as.integer(max(0, min(n, round(n * p))))

  nhse_outpat_n <- ds_n(proportions$nhse_outpat)
  nhse_inpat_n <- ds_n(proportions$nhse_inpat)
  nhse_ed_n <- ds_n(proportions$nhse_ed)

  merged_code_config <- .ofh_merge_lists(ofh_default_code_config(), code_config)

  cfg <- list(
    total_pid_count = n,
    datasets = list(
      participant_data = list(unique_pids = "ALL"),
      questionnaire_data = list(unique_pids = "ALL"),
      clinic_measurements_data = list(unique_pids = ds_n(proportions$clinic_measurements)),
      nhse_outpat_data = list(
        unique_pids = nhse_outpat_n,
        total_records = as.integer(max(nhse_outpat_n, round(n * record_multipliers$nhse_outpat)))
      ),
      nhse_inpat_data = list(
        unique_pids = nhse_inpat_n,
        total_records = as.integer(max(nhse_inpat_n, round(n * record_multipliers$nhse_inpat)))
      ),
      nhse_engwal_deaths_data = list(unique_pids = ds_n(proportions$nhse_engwal_deaths)),
      nhse_ed_data = list(
        unique_pids = nhse_ed_n,
        total_records = as.integer(max(nhse_ed_n, round(n * record_multipliers$nhse_ed)))
      ),
      nhse_primcare_meds_data = list(unique_pids = ds_n(proportions$nhse_primcare_meds)),
      country_region_data = list(unique_pids = ds_n(proportions$country_region))
    ),
    code_config = merged_code_config
  )

  cfg
}
