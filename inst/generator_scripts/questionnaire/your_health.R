if (file.exists("questionnaire/section_utils.R")) {
	source("questionnaire/section_utils.R")
} else {
	source("section_utils.R")
}

questionnaire_data <- get_questionnaire_data()

cols <- c(
	"pid", "children_bio_first_age_1_1", "children_bio_last_age_1_1", "children_bio_num_1_1", "children_bio_num_2_1",
	"children_birthed_first_age_1_1", "children_birthed_last_age_1_1", "children_birthed_num_1_1", "diag_1_m", "diag_2_m",
	"diag_anaemia_1_m", "diag_auto_1_m", "diag_cancer_1_m", "diag_cancer_skin_1_m", "diag_cvd_1_m", "diag_endocr_1_m",
	"diag_gastro_1_m", "diag_neuro_1_m", "diag_neuro_dev_1_m", "diag_ob_1_m", "diag_opthal_1_m", "diag_osteo_1_m",
	"diag_psych_1_m", "diag_psych_anx_1_m", "diag_psych_depr_1_m", "diag_psych_eat_1_m", "diag_repro_1_m", "diag_resp_1_m",
	"diag_urol_1_m", "gad7_impair_1_1", "gad7_item1_anx_1_1", "gad7_item2_worry_control_1_1", "gad7_item3_worry_amount_1_1",
	"gad7_item4_relax_1_1", "gad7_item5_restless_1_1", "gad7_item6_annoyed_1_1", "gad7_item7_afraid_1_1", "gyn_contracept_implant_1_1",
	"gyn_contracept_methods_1_m", "gyn_contracept_pill_1_1", "gyn_contracept_pill_first_age_1_1", "gyn_contracept_pill_last_age_1_1",
	"gyn_hrt_1_1", "gyn_hrt_first_trt_age_1_1", "gyn_hrt_last_trt_age_1_1", "gyn_hyst_1_1", "gyn_hyst_age_1_1", "gyn_menopause_1_1",
	"gyn_menopause_2_1", "gyn_menopause_last_period_age_1_1", "gyn_menopause_last_period_age_2_1", "gyn_menstr_age_1_1",
	"gyn_menstr_cycle_days_1_1", "gyn_menstr_cycle_days_2_1", "gyn_menstr_last_period_days_1_1", "gyn_menstr_last_period_days_2_1",
	"gyn_ooph_1_1", "gyn_ooph_age_1_1", "health_amputation_1_1", "health_check_colorectal_1_1", "health_check_colorectal_yrs_1_1",
	"health_check_mammogram_1_1", "health_check_mammogram_yrs_1_1", "health_check_prostate_1_1", "health_check_prostate_yrs_1_1",
	"health_check_smear_1_1", "health_check_smear_yrs_1_1", "health_covid_1_1", "health_dental_1_m", "health_falls_1_1",
	"health_pain_acute_1_m", "health_pain_acute_2_m", "health_pain_chest_1_1", "health_pain_chest_subside_1_1", "health_pain_chest_walk_1_1",
	"health_pain_chest_walk_uphill_1_1", "health_pain_chronic_1_m", "health_pain_chronic_back_1_1", "health_pain_chronic_body_1_1",
	"health_pain_chronic_face_1_1", "health_pain_chronic_headache_1_1", "health_pain_chronic_hip_1_1", "health_pain_chronic_knee_1_1",
	"health_pain_chronic_shoulder_1_1", "health_pain_chronic_stomach_1_1", "health_pain_leg_1_1", "health_resp_short_1_1",
	"health_resp_wheeze_1_1", "health_status_chronic_1_1", "health_status_curr_1_1", "health_status_disability_1_1",
	"health_status_disability_support_1_m", "health_status_private_healthcare_1_1", "health_sun_protect_1_1", "health_sun_solarium_1_1",
	"health_weight_chg_1_1", "medicat_1_m", "medicat_a_1_m", "medicat_auto_1_m", "medicat_b_1_m", "medicat_c_1_m", "medicat_cancer_1_m",
	"medicat_cvd_1_m", "medicat_d_1_m", "medicat_diab_1_m", "medicat_endocr_1_m", "medicat_gastro_1_m", "medicat_neuro_1_m",
	"medicat_osteo_1_m", "medicat_pain_1_m", "medicat_prescript_1_m", "medicat_psych_1_m", "medicat_psych_antidepr_1_m",
	"medicat_psych_antipsych_1_m", "medicat_repro_1_m", "medicat_repro_contracept_1_m", "medicat_resp_1_m", "medicat_suppl_1_m",
	"phq9_impair_1_1", "phq9_item1_interest_1_1", "phq9_item2_down_1_1", "phq9_item3_sleep_1_1", "phq9_item4_energy_1_1",
	"phq9_item5_appetite_1_1", "phq9_item6_bad_1_1", "phq9_item7_concentr_1_1", "phq9_item8_movement_1_1", "phq9_item9_harm_1_1",
	"skip_phq9_gad7_1_1"
)

questionnaire_data <- ensure_columns(questionnaire_data, cols)
questionnaire_data <- fill_defaults_for_columns(questionnaire_data, cols)
section_df <- questionnaire_data[, cols, drop = FALSE]

n <- nrow(section_df)

# Determine v1/v2 index split.
na_cols <- c(
  "children_bio_first_age_1_1", "children_bio_last_age_1_1",
  "children_bio_num_1_1", "children_bio_num_2_1",
  "children_birthed_first_age_1_1", "children_birthed_last_age_1_1",
  "children_birthed_num_1_1",
  "gad7_impair_1_1", "gad7_item1_anx_1_1", "gad7_item2_worry_control_1_1",
  "gad7_item3_worry_amount_1_1", "gad7_item4_relax_1_1",
  "gad7_item5_restless_1_1", "gad7_item6_annoyed_1_1", "gad7_item7_afraid_1_1",
  "phq9_impair_1_1", "phq9_item1_interest_1_1", "phq9_item2_down_1_1",
  "phq9_item3_sleep_1_1", "phq9_item4_energy_1_1", "phq9_item5_appetite_1_1",
  "phq9_item6_bad_1_1", "phq9_item7_concentr_1_1", "phq9_item8_movement_1_1",
  "phq9_item9_harm_1_1",
  "skip_phq9_gad7_1_1",
  "health_check_colorectal_1_1", "health_check_colorectal_yrs_1_1",
  "health_check_mammogram_1_1", "health_check_mammogram_yrs_1_1",
  "health_check_prostate_1_1", "health_check_prostate_yrs_1_1",
  "health_check_smear_1_1", "health_check_smear_yrs_1_1",
  "health_pain_chest_1_1", "health_pain_chest_subside_1_1",
  "health_pain_chest_walk_1_1", "health_pain_chest_walk_uphill_1_1",
  "health_pain_chronic_1_m",
  "health_pain_chronic_body_1_1", "health_pain_chronic_face_1_1",
  "health_pain_chronic_knee_1_1", "health_pain_chronic_shoulder_1_1",
  "health_pain_chronic_stomach_1_1",
  "health_status_curr_1_1",
  "diag_anaemia_1_m", "diag_cancer_1_m", "diag_cancer_skin_1_m",
  "diag_gastro_1_m", "diag_neuro_1_m", "diag_neuro_dev_1_m",
  "diag_opthal_1_m", "diag_osteo_1_m",
  "diag_psych_1_m", "diag_psych_anx_1_m", "diag_psych_depr_1_m",
  "diag_psych_eat_1_m", "diag_repro_1_m"
)
for (col in na_cols) section_df[[col]] <- rep(NA_character_, n)

# Determine v1/v2 index split.
if ("questionnaire_version" %in% names(questionnaire_data)) {
  idx_v1 <- which(questionnaire_data$questionnaire_version == 1)
  idx_v2 <- which(questionnaire_data$questionnaire_version == 2)
} else {
  idx_v1 <- sample(seq_len(n), size = max(1L, round(0.05 * n)))
  idx_v2 <- setdiff(seq_len(n), idx_v1)
}

# Female index (~52% of sample)
female_idx <- sample(seq_len(n), size = round(0.52 * n))
female_v1_idx <- intersect(female_idx, idx_v1)
female_v2_idx <- intersect(female_idx, idx_v2)

# Helper: draw multi-select medication answers with realistic prevalence.
draw_medicat_col <- function(options, nn, p_none = 0.55, p_pna = 0.03, p_dnk = 0.04) {
  out <- rep(NA_character_, nn)
  r <- runif(nn)
  out[r < p_pna] <- "Prefer not to answer"
  out[r >= p_pna & r < p_pna + p_dnk] <- "Do not know"
  out[r >= p_pna + p_dnk & r < p_pna + p_dnk + p_none] <- "None of the above"
  multi_idx <- which(r >= p_pna + p_dnk + p_none)
  if (length(multi_idx) > 0) {
    for (k in multi_idx) {
      n_pick <- sample(1:min(2, length(options)), 1, prob = c(0.82, 0.18)[1:min(2, length(options))])
      out[k] <- paste(sample(options, n_pick, replace = FALSE), collapse = "; ")
    }
  }
  out
}

# ===========================================================================
# DIAGNOSES
# ===========================================================================

# Top-level diagnosis: same 18-option list for v1 and v2 (mutually exclusive columns).
diag_top_options <- c(
  "None of the above", "Autoimmune disorder", "Blood disorders (Anaemia)", "Cancer",
  "Digestive system or liver problems",
  "Endocrine, nutritional and metabolic disorders (e.g., diabetes, thyroid disorder, vitamin deficiencies)",
  "Eye or visual problems", "Fractures, breaks, or joint problems",
  "Heart or circulatory disease (e.g. high blood pressure or stroke)",
  "Kidney or urinary system disorders", "Lung or respiratory problems",
  "Mental health conditions (e.g. depression, bipolar disorder)",
  "Neurodevelopmental conditions (e.g., Autism spectrum disorder, ADHD)",
  "Neurological disorders (things that affect that brain or nervous system)",
  "Reproductive system problems", "Other not listed", "Do not know", "Prefer not to answer", NA
)
diag_top_probs <- c(0.40, 0.03, 0.02, 0.08, 0.05, 0.07, 0.02, 0.03, 0.12, 0.03, 0.06, 0.04, 0.01, 0.02, 0.01, 0.02, 0.02, 0.02, 0.05)

section_df$diag_1_m <- rep(NA_character_, n)
section_df$diag_2_m <- rep(NA_character_, n)
if (length(idx_v1) > 0)
  section_df$diag_1_m[idx_v1] <- sample(diag_top_options, length(idx_v1), replace = TRUE, prob = diag_top_probs)
if (length(idx_v2) > 0)
  section_df$diag_2_m[idx_v2] <- sample(diag_top_options, length(idx_v2), replace = TRUE, prob = diag_top_probs)

# Combine for conditional routing.
diag_combined <- section_df$diag_1_m
diag_combined[is.na(diag_combined)] <- section_df$diag_2_m[is.na(diag_combined)]

# Detail diagnosis columns (conditional on top-level selection).
cvd_diag_idx   <- which(grepl("Heart or circulatory disease", diag_combined, fixed = TRUE))
auto_diag_idx  <- which(grepl("Autoimmune disorder", diag_combined, fixed = TRUE))
endocr_diag_idx <- which(grepl("Endocrine, nutritional and metabolic disorders", diag_combined, fixed = TRUE))
resp_diag_idx  <- which(grepl("Lung or respiratory problems", diag_combined, fixed = TRUE))
urol_diag_idx  <- which(grepl("Kidney or urinary system disorders", diag_combined, fixed = TRUE))
repro_diag_idx <- which(grepl("Reproductive system problems", diag_combined, fixed = TRUE))

section_df$diag_cvd_1_m   <- rep(NA_character_, n)
section_df$diag_auto_1_m  <- rep(NA_character_, n)
section_df$diag_endocr_1_m <- rep(NA_character_, n)
section_df$diag_resp_1_m  <- rep(NA_character_, n)
section_df$diag_urol_1_m  <- rep(NA_character_, n)
section_df$diag_ob_1_m    <- rep(NA_character_, n)

if (length(cvd_diag_idx) > 0)
  section_df$diag_cvd_1_m[cvd_diag_idx] <- sample(
    c("High blood pressure (hypertension)", "Angina", "Heart attack (myocardial infarction)",
      "Stroke", "Atrial fibrillation", "Heart failure", "Deep vein thrombosis (DVT)",
      "Pulmonary embolism", "Peripheral vascular disease", "Other cardiovascular condition",
      "Do not know", "Prefer not to answer"),
    length(cvd_diag_idx), replace = TRUE,
    prob = c(0.38, 0.08, 0.09, 0.06, 0.10, 0.04, 0.03, 0.02, 0.04, 0.07, 0.05, 0.04)
  )

if (length(auto_diag_idx) > 0)
  section_df$diag_auto_1_m[auto_diag_idx] <- sample(
    c("Rheumatoid arthritis", "Psoriasis/Psoriatic arthritis", "Type 1 diabetes",
      "Coeliac disease", "Crohn's disease", "Ulcerative colitis", "Lupus (SLE)",
      "Multiple sclerosis", "Thyroid disease (Graves'/Hashimoto's)", "Other autoimmune condition",
      "Do not know", "Prefer not to answer"),
    length(auto_diag_idx), replace = TRUE,
    prob = c(0.15, 0.12, 0.08, 0.09, 0.10, 0.11, 0.04, 0.06, 0.12, 0.06, 0.04, 0.03)
  )

if (length(endocr_diag_idx) > 0)
  section_df$diag_endocr_1_m[endocr_diag_idx] <- sample(
    c("Type 2 diabetes", "Type 1 diabetes", "Prediabetes", "Thyroid disorder (hypothyroidism)",
      "Thyroid disorder (hyperthyroidism)", "Hypercholesterolaemia (high cholesterol)",
      "Vitamin D deficiency", "Metabolic syndrome", "Polycystic ovary syndrome (PCOS)",
      "Other endocrine condition", "Do not know", "Prefer not to answer"),
    length(endocr_diag_idx), replace = TRUE,
    prob = c(0.30, 0.05, 0.08, 0.14, 0.06, 0.16, 0.07, 0.03, 0.04, 0.03, 0.02, 0.02)
  )

if (length(resp_diag_idx) > 0)
  section_df$diag_resp_1_m[resp_diag_idx] <- sample(
    c("Asthma", "Chronic obstructive pulmonary disease (COPD)", "Chronic bronchitis",
      "Emphysema", "Sleep apnoea", "Pneumonia (recurrent)", "Pulmonary fibrosis",
      "Other respiratory condition", "Do not know", "Prefer not to answer"),
    length(resp_diag_idx), replace = TRUE,
    prob = c(0.42, 0.15, 0.10, 0.06, 0.11, 0.04, 0.02, 0.04, 0.03, 0.03)
  )

if (length(urol_diag_idx) > 0)
  section_df$diag_urol_1_m[urol_diag_idx] <- sample(
    c("Chronic kidney disease", "Kidney stones", "Urinary tract infection (recurrent)",
      "Enlarged prostate (BPH)", "Incontinence", "Polycystic kidney disease",
      "Other urological condition", "Do not know", "Prefer not to answer"),
    length(urol_diag_idx), replace = TRUE,
    prob = c(0.14, 0.26, 0.22, 0.10, 0.15, 0.03, 0.05, 0.03, 0.02)
  )

if (length(repro_diag_idx) > 0)
  section_df$diag_ob_1_m[repro_diag_idx] <- sample(
    c("None of the above", "Endometriosis", "Polycystic ovary syndrome (PCOS)", "Fibroids",
      "Heavy menstrual bleeding", "Infertility/subfertility", "Ovarian cysts",
      "Other (not listed)", "Do not know", "Prefer not to answer"),
    length(repro_diag_idx), replace = TRUE,
    prob = c(0.15, 0.14, 0.16, 0.14, 0.14, 0.08, 0.08, 0.05, 0.03, 0.03)
  )

# Remaining detail columns not populated by any reference pass — remain NA.
# (diag_anaemia_1_m, diag_cancer_1_m, diag_cancer_skin_1_m, diag_gastro_1_m,
#  diag_neuro_1_m, diag_neuro_dev_1_m, diag_opthal_1_m, diag_osteo_1_m,
#  diag_psych_1_m, diag_psych_anx_1_m, diag_psych_depr_1_m, diag_psych_eat_1_m,
#  diag_repro_1_m)

# ===========================================================================
# GYNAECOLOGY / REPRODUCTIVE HEALTH
# ===========================================================================

section_df$gyn_menstr_age_1_1 <- rep(NA_character_, n)
if (length(female_idx) > 0)
  section_df$gyn_menstr_age_1_1[female_idx] <- as.character(
    pmin(20, pmax(8, round(rnorm(length(female_idx), mean = 13, sd = 1.8))))
  )

menopause_levels <- c(
  "No", "Yes, naturally", "Yes, due to surgery or treatment",
  "Not sure", "Prefer not to answer"
)
section_df$gyn_menopause_1_1 <- rep(NA_character_, n)
section_df$gyn_menopause_2_1 <- rep(NA_character_, n)
if (length(female_v1_idx) > 0)
  section_df$gyn_menopause_1_1[female_v1_idx] <- sample(
    menopause_levels, length(female_v1_idx), replace = TRUE,
    prob = c(0.55, 0.30, 0.05, 0.04, 0.06)
  )
if (length(female_v2_idx) > 0)
  section_df$gyn_menopause_2_1[female_v2_idx] <- sample(
    menopause_levels, length(female_v2_idx), replace = TRUE,
    prob = c(0.55, 0.30, 0.05, 0.04, 0.06)
  )

all_menopause <- section_df$gyn_menopause_1_1
all_menopause[is.na(all_menopause)] <- section_df$gyn_menopause_2_1[is.na(all_menopause)]
menopause_yes_idx <- which(all_menopause %in% c("Yes, naturally", "Yes, due to surgery or treatment"))
menopause_v1_yes  <- intersect(menopause_yes_idx, idx_v1)
menopause_v2_yes  <- intersect(menopause_yes_idx, idx_v2)

section_df$gyn_menopause_last_period_age_1_1 <- rep(NA_character_, n)
section_df$gyn_menopause_last_period_age_2_1 <- rep(NA_character_, n)
if (length(menopause_v1_yes) > 0)
  section_df$gyn_menopause_last_period_age_1_1[menopause_v1_yes] <- as.character(
    pmin(65, pmax(28, round(rnorm(length(menopause_v1_yes), mean = 50, sd = 4.5))))
  )
if (length(menopause_v2_yes) > 0)
  section_df$gyn_menopause_last_period_age_2_1[menopause_v2_yes] <- as.character(
    pmin(65, pmax(28, round(rnorm(length(menopause_v2_yes), mean = 50, sd = 4.5))))
  )

recent_period_idx <- setdiff(female_idx, menopause_yes_idx)
recent_v1_idx     <- intersect(recent_period_idx, idx_v1)
recent_v2_idx     <- intersect(recent_period_idx, idx_v2)

section_df$gyn_menstr_last_period_days_1_1 <- rep(NA_character_, n)
section_df$gyn_menstr_last_period_days_2_1 <- rep(NA_character_, n)
section_df$gyn_menstr_cycle_days_1_1       <- rep(NA_character_, n)
section_df$gyn_menstr_cycle_days_2_1       <- rep(NA_character_, n)
if (length(recent_v1_idx) > 0) {
  section_df$gyn_menstr_last_period_days_1_1[recent_v1_idx] <- as.character(
    pmin(365, pmax(1, round(rlnorm(length(recent_v1_idx), meanlog = log(21), sdlog = 0.9))))
  )
  section_df$gyn_menstr_cycle_days_1_1[recent_v1_idx] <- as.character(
    pmin(60, pmax(18, round(rnorm(length(recent_v1_idx), mean = 29, sd = 4.8))))
  )
}
if (length(recent_v2_idx) > 0) {
  section_df$gyn_menstr_last_period_days_2_1[recent_v2_idx] <- as.character(
    pmin(365, pmax(1, round(rlnorm(length(recent_v2_idx), meanlog = log(21), sdlog = 0.9))))
  )
  section_df$gyn_menstr_cycle_days_2_1[recent_v2_idx] <- as.character(
    pmin(60, pmax(18, round(rnorm(length(recent_v2_idx), mean = 29, sd = 4.8))))
  )
}

# Contraception (female only).
section_df$gyn_contracept_implant_1_1   <- rep(NA_character_, n)
section_df$gyn_contracept_methods_1_m  <- rep(NA_character_, n)
section_df$gyn_contracept_pill_1_1     <- rep(NA_character_, n)
section_df$gyn_contracept_pill_first_age_1_1 <- rep(NA_character_, n)
section_df$gyn_contracept_pill_last_age_1_1  <- rep(NA_character_, n)

if (length(female_idx) > 0) {
  section_df$gyn_contracept_implant_1_1[female_idx] <- sample(
    c("Yes", "No", "Previously had an implant", "Do not know", "Prefer not to answer"),
    length(female_idx), replace = TRUE,
    prob = c(0.10, 0.72, 0.10, 0.03, 0.05)
  )

  primary_methods <- sample(
    c("None of the above", "Combined pill", "Mini pill", "Contraceptive implant",
      "Intrauterine device (coil)", "Condoms", "Sterilisation", "Other", "Prefer not to answer"),
    length(female_idx), replace = TRUE,
    prob = c(0.18, 0.20, 0.12, 0.08, 0.12, 0.20, 0.04, 0.03, 0.03)
  )
  secondary_methods <- sample(
    c("Combined pill", "Mini pill", "Contraceptive implant",
      "Intrauterine device (coil)", "Condoms", "Natural methods", "Other"),
    length(female_idx), replace = TRUE
  )
  add_secondary <- runif(length(female_idx)) < 0.18
  methods_vec <- primary_methods
  for (k in seq_along(female_idx)) {
    if (isTRUE(add_secondary[k]) &&
        !primary_methods[k] %in% c("None of the above", "Prefer not to answer") &&
        primary_methods[k] != secondary_methods[k]) {
      methods_vec[k] <- paste(primary_methods[k], secondary_methods[k], sep = "; ")
    }
  }
  section_df$gyn_contracept_methods_1_m[female_idx] <- methods_vec

  section_df$gyn_contracept_pill_1_1[female_idx] <- sample(
    c("Prefer not to answer", "Do not know", "No", "Yes"),
    length(female_idx), replace = TRUE,
    prob = c(0.04, 0.05, 0.56, 0.35)
  )
  pill_idx <- which(grepl("Combined pill|Mini pill", section_df$gyn_contracept_methods_1_m))
  if (length(pill_idx) > 0) section_df$gyn_contracept_pill_1_1[pill_idx] <- "Yes"
}

pill_user_idx <- which(grepl("Combined pill|Mini pill", section_df$gyn_contracept_methods_1_m))
if (length(pill_user_idx) > 0) {
  first_ages <- pmin(55, pmax(12, round(rnorm(length(pill_user_idx), mean = 21, sd = 5.5))))
  section_df$gyn_contracept_pill_first_age_1_1[pill_user_idx] <- as.character(first_ages)
  last_ages <- pmin(65, pmax(first_ages, round(first_ages + rexp(length(pill_user_idx), rate = 1 / 9))))
  section_df$gyn_contracept_pill_last_age_1_1[pill_user_idx] <- as.character(last_ages)
}

# Hysterectomy / oophorectomy / HRT (female only).
section_df$gyn_hyst_1_1          <- rep(NA_character_, n)
section_df$gyn_ooph_1_1          <- rep(NA_character_, n)
section_df$gyn_hyst_age_1_1      <- rep(NA_character_, n)
section_df$gyn_ooph_age_1_1      <- rep(NA_character_, n)
section_df$gyn_hrt_1_1           <- rep(NA_character_, n)
section_df$gyn_hrt_first_trt_age_1_1 <- rep(NA_character_, n)
section_df$gyn_hrt_last_trt_age_1_1  <- rep(NA_character_, n)

if (length(female_idx) > 0) {
  section_df$gyn_hyst_1_1[female_idx] <- sample(
    c("Yes", "No", "Do not know", "Prefer not to answer"),
    length(female_idx), replace = TRUE, prob = c(0.11, 0.82, 0.03, 0.04)
  )
  section_df$gyn_ooph_1_1[female_idx] <- sample(
    c("Yes", "No", "Do not know", "Prefer not to answer"),
    length(female_idx), replace = TRUE, prob = c(0.07, 0.86, 0.03, 0.04)
  )

  hyst_yes_idx <- which(section_df$gyn_hyst_1_1 == "Yes")
  if (length(hyst_yes_idx) > 0)
    section_df$gyn_hyst_age_1_1[hyst_yes_idx] <- as.character(
      pmin(75, pmax(20, round(rnorm(length(hyst_yes_idx), mean = 46, sd = 8))))
    )
  ooph_yes_idx <- which(section_df$gyn_ooph_1_1 == "Yes")
  if (length(ooph_yes_idx) > 0)
    section_df$gyn_ooph_age_1_1[ooph_yes_idx] <- as.character(
      pmin(75, pmax(20, round(rnorm(length(ooph_yes_idx), mean = 44, sd = 8))))
    )

  probable_hrt_idx <- union(menopause_yes_idx, union(hyst_yes_idx, ooph_yes_idx))
  probable_hrt_idx <- intersect(probable_hrt_idx, female_idx)
  hrt_yes_idx      <- if (length(probable_hrt_idx) > 0)
    sample(probable_hrt_idx, size = round(0.35 * length(probable_hrt_idx)), replace = FALSE)
  else integer(0)
  non_hrt_pool <- setdiff(female_idx, hrt_yes_idx)
  if (length(non_hrt_pool) > 0)
    section_df$gyn_hrt_1_1[non_hrt_pool] <- sample(
      c("No", "Do not know", "Prefer not to answer"),
      length(non_hrt_pool), replace = TRUE, prob = c(0.90, 0.05, 0.05)
    )
  if (length(hrt_yes_idx) > 0) {
    section_df$gyn_hrt_1_1[hrt_yes_idx] <- "Yes"
    first_trt <- pmin(70, pmax(30, round(rnorm(length(hrt_yes_idx), mean = 51, sd = 6))))
    section_df$gyn_hrt_first_trt_age_1_1[hrt_yes_idx] <- as.character(first_trt)
    last_trt <- pmin(80, pmax(first_trt, round(first_trt + rexp(length(hrt_yes_idx), rate = 1 / 5))))
    section_df$gyn_hrt_last_trt_age_1_1[hrt_yes_idx] <- as.character(last_trt)
  }
}

# ===========================================================================
# HEALTH STATUS
# ===========================================================================

# health_status_chronic_1_1 and health_covid_1_1 already filled by fill_defaults_for_columns.

chronic_yes_idx <- which(section_df$health_status_chronic_1_1 == "Yes")

yn_options <- c("Prefer not to answer", "Do not know", "No", "Yes")

section_df$health_status_disability_1_1 <- sample(yn_options, n, replace = TRUE,
  prob = c(0.03, 0.04, 0.78, 0.15))
if (length(chronic_yes_idx) > 0)
  section_df$health_status_disability_1_1[chronic_yes_idx] <- sample(
    yn_options, length(chronic_yes_idx), replace = TRUE, prob = c(0.03, 0.05, 0.57, 0.35)
  )

section_df$health_status_disability_support_1_m <- rep(NA_character_, n)
if (length(chronic_yes_idx) > 0) {
  support_recipients <- sample(chronic_yes_idx, round(0.30 * length(chronic_yes_idx)))
  if (length(support_recipients) > 0)
    section_df$health_status_disability_support_1_m[support_recipients] <- sample(
      c("Personal Independence Payment (PIP)", "Disability Living Allowance (DLA)",
        "Attendance Allowance", "Carer's Allowance",
        "Employment and Support Allowance (ESA)",
        "Universal Credit (disability element)", "Other disability benefit",
        "None of the above", "Prefer not to answer"),
      length(support_recipients), replace = TRUE,
      prob = c(0.18, 0.12, 0.15, 0.08, 0.10, 0.12, 0.05, 0.15, 0.05)
    )
}

section_df$health_status_private_healthcare_1_1 <- sample(
  c("Yes, all of the time", "Yes, most of the time", "Yes, sometimes",
    "No, never", "Do not know", "Prefer not to answer"),
  n, replace = TRUE, prob = c(0.03, 0.04, 0.05, 0.84, 0.02, 0.02)
)

section_df$health_sun_protect_1_1 <- sample(
  c("Never/rarely", "Sometimes", "Most of the time", "Always",
    "Do not go out in sunshine", "Do not know", "Prefer not to answer"),
  n, replace = TRUE, prob = c(0.15, 0.30, 0.28, 0.18, 0.02, 0.03, 0.04)
)

section_df$health_sun_solarium_1_1 <- rep(NA_character_, n)
if (length(idx_v1) > 0)
  section_df$health_sun_solarium_1_1[idx_v1] <- as.character(
    pmax(0, rpois(length(idx_v1), lambda = 0.3))
  )

# ===========================================================================
# SPECIFIC HEALTH COLUMNS
# ===========================================================================

# Multi-select columns are populated from the PDF value catalog.
section_df$health_dental_1_m <- rep(NA_character_, n)

section_df$health_falls_1_1 <- sample(
  c("No falls", "Only one fall", "More than one fall", "Prefer not to answer"),
  n, replace = TRUE, prob = c(0.82, 0.12, 0.03, 0.03)
)

section_df$health_weight_chg_1_1 <- sample(
  c("No - weigh about the same", "Yes - gained weight", "Yes - lost weight",
    "Do not know", "Prefer not to answer"),
  n, replace = TRUE, prob = c(0.55, 0.22, 0.12, 0.06, 0.05)
)

section_df$health_resp_wheeze_1_1 <- sample(
  yn_options, n, replace = TRUE, prob = c(0.03, 0.03, 0.76, 0.18)
)
section_df$health_resp_short_1_1 <- sample(
  yn_options, n, replace = TRUE, prob = c(0.03, 0.03, 0.82, 0.12)
)
section_df$health_pain_leg_1_1 <- sample(
  yn_options, n, replace = TRUE, prob = c(0.02, 0.03, 0.88, 0.07)
)

# Multi-select acute pain columns are populated from the PDF value catalog.
section_df$health_pain_acute_1_m <- rep(NA_character_, n)
section_df$health_pain_acute_2_m <- rep(NA_character_, n)

# Chronic headache (v1 only).
section_df$health_pain_chronic_headache_1_1 <- rep(NA_character_, n)
if (length(idx_v1) > 0)
  section_df$health_pain_chronic_headache_1_1[idx_v1] <- sample(
    yn_options, length(idx_v1), replace = TRUE, prob = c(0.03, 0.04, 0.78, 0.15)
  )

section_df$health_pain_chronic_back_1_1 <- sample(
  yn_options, n, replace = TRUE, prob = c(0.03, 0.04, 0.70, 0.23)
)
section_df$health_pain_chronic_hip_1_1 <- sample(
  yn_options, n, replace = TRUE, prob = c(0.03, 0.04, 0.78, 0.15)
)

section_df$health_amputation_1_1 <- sample(
  c("No", "Yes, toes", "Yes, leg below the knee", "Yes, leg above the knee",
    "Do not know", "Prefer not to answer"),
  n, replace = TRUE, prob = c(0.985, 0.005, 0.003, 0.002, 0.002, 0.003)
)

# ===========================================================================
# MEDICATIONS
# ===========================================================================

# General medication summary (v2 only).
section_df$medicat_1_m <- rep(NA_character_, n)
if (length(idx_v2) > 0)
  section_df$medicat_1_m[idx_v2] <- sample(
    c("None of the above", "Autoimmune disorders", "Bone health", "Cancer",
      "Diabetic health", "Digestive problems (including acid reflux and liver problems)",
      "Endocrine disorder (e.g., under or over-active thyroid)",
      "Heart or circulatory health (e.g., high blood pressure or stroke)",
      "Lung or breathing problems (including asthma)",
      "Mental health conditions or insomnia (e.g. depression, bipolar disorder)",
      "Neurological disorders (e.g., Alzheimer's, epilepsy, Parkinson's)", "Pain relief",
      "Reproductive or sexual health (including contraception, erectile dysfunction, menopause or hormone medication)",
      "Supplements or nutritional health", "Other", "Do not know", "Prefer not to answer", NA),
    length(idx_v2), replace = TRUE,
    prob = c(0.35, 0.03, 0.02, 0.02, 0.06, 0.03, 0.03, 0.15, 0.04, 0.08, 0.01, 0.10, 0.02, 0.08, 0.01, 0.01, 0.01, 0.02)
  )

# Common medications (all participants, draw_medicat style).
section_df$medicat_a_1_m <- draw_medicat_col(
  c("Cholesterol lowering medication", "Blood pressure medication", "Insulin",
    "Hormone replacement therapy", "Oral contraceptive pill or minipill"),
  n, p_none = 0.52
)
section_df$medicat_auto_1_m <- draw_medicat_col(
  c("Amino salicylates (5-ASAs, mesalazine)", "Cyclosporine", "Other not listed",
    "Azathioprine", "Corticosteriods (e.g. prednisolone)", "Tacrolimus",
    "JAK inhibitors", "Tumour Necrosis Factor (TNF) inhibitors",
    "Tocilizumab", "Rituximab", "Mycophenolate"),
  n, p_none = 0.72
)
section_df$medicat_osteo_1_m <- draw_medicat_col(
  c("Bisphosphonates (e.g. alendronic acid, ibandronic acid, risendronic acid, zoleronic acid)",
    "Selective oestrogen receptor modulators (SERMs, Raloxifene)",
    "Strontium Ranelate",
    "Monoclonal antibodies (Denosumab, Romosozumab)",
    "Parathyroid hormone (e.g. teriparatide)",
    "Vitamin D and/or Calcium supplements"),
  n, p_none = 0.68
)

# CVD medications: v2 participants with CVD diagnosis at higher prevalence.
section_df$medicat_cvd_1_m <- rep(NA_character_, n)
if (length(idx_v2) > 0)
  section_df$medicat_cvd_1_m[idx_v2] <- sample(
    c("None of the above", "Cholesterol lowering medication/statins", "Blood pressure medication",
      "Aspirin (low dose)", "Beta blocker", "Anticoagulant (blood thinners)",
      "Diuretic", "Calcium channel blocker", "Antiarrhythmic",
      "Do not know", "Prefer not to answer", NA),
    length(idx_v2), replace = TRUE,
    prob = c(0.65, 0.10, 0.09, 0.05, 0.03, 0.025, 0.02, 0.015, 0.01, 0.015, 0.01, 0.015)
  )
cvd_med_idx <- intersect(cvd_diag_idx, idx_v2)
cvd_med_takers <- if (length(cvd_med_idx) > 0) sample(cvd_med_idx, round(0.85 * length(cvd_med_idx))) else integer(0)
if (length(cvd_med_takers) > 0)
  section_df$medicat_cvd_1_m[cvd_med_takers] <- sample(
    c("Aspirin (low dose)", "Statin (cholesterol lowering)", "ACE inhibitor/ARB (blood pressure)",
      "Beta blocker", "Calcium channel blocker", "Diuretic",
      "Anticoagulant (warfarin/DOAC)", "Antiplatelet (clopidogrel)",
      "Other cardiovascular medication", "None of the above", "Do not know", "Prefer not to answer"),
    length(cvd_med_takers), replace = TRUE,
    prob = c(0.18, 0.28, 0.20, 0.12, 0.08, 0.05, 0.03, 0.02, 0.01, 0.01, 0.01, 0.01)
  )

# Diabetes medications: conditional on diabetes in endocr diag.
section_df$medicat_diab_1_m <- rep(NA_character_, n)
if (length(idx_v2) > 0)
  section_df$medicat_diab_1_m[idx_v2] <- sample(
    c("None of the above", "Metformin", "Insulin", "DPP-4 Inhibitors (Gliptins)",
      "SGLT2 inhibitors", "GLP-1 (incretin memetics)", "Other not listed",
      "Do not know", "Prefer not to answer", NA),
    length(idx_v2), replace = TRUE,
    prob = c(0.80, 0.06, 0.04, 0.02, 0.015, 0.015, 0.01, 0.02, 0.01, 0.015)
  )
diab_idx <- which(grepl("diabetes", section_df$diag_endocr_1_m, ignore.case = TRUE))
if (length(diab_idx) > 0)
  section_df$medicat_diab_1_m[diab_idx] <- sample(
    c("Metformin", "Insulin", "Sulfonylureas (e.g., gliclazide)",
      "DPP-4 inhibitors (e.g., sitagliptin)", "GLP-1 agonists (e.g., semaglutide)",
      "SGLT2 inhibitors (e.g., empagliflozin)", "Other diabetes medication",
      "None of the above", "Do not know", "Prefer not to answer"),
    length(diab_idx), replace = TRUE,
    prob = c(0.42, 0.18, 0.12, 0.08, 0.05, 0.06, 0.03, 0.02, 0.02, 0.02)
  )

# Category-specific medications (v2 only).
sample_v2_medicat <- function(options, nn) {
  if (nn == 0) return(character(0))
  sample(c(options, "None of the above", "Do not know", "Prefer not to answer"),
    nn, replace = TRUE,
    prob = c(rep(0.70 / length(options), length(options)), 0.20, 0.05, 0.05))
}

section_df$medicat_cancer_1_m  <- rep(NA_character_, n)
section_df$medicat_gastro_1_m  <- rep(NA_character_, n)
section_df$medicat_endocr_1_m  <- rep(NA_character_, n)
section_df$medicat_resp_1_m    <- rep(NA_character_, n)
section_df$medicat_psych_1_m   <- rep(NA_character_, n)
section_df$medicat_psych_antidepr_1_m  <- rep(NA_character_, n)
section_df$medicat_psych_antipsych_1_m <- rep(NA_character_, n)
section_df$medicat_neuro_1_m   <- rep(NA_character_, n)
section_df$medicat_pain_1_m    <- rep(NA_character_, n)
section_df$medicat_repro_1_m   <- rep(NA_character_, n)
section_df$medicat_suppl_1_m   <- rep(NA_character_, n)

if (length(idx_v2) > 0) {
  section_df$medicat_cancer_1_m[idx_v2] <- sample_v2_medicat(
    c("Chemotherapy", "Hormone therapy", "Immunotherapy / Targeted therapy", "Radiotherapy", "Other not listed"),
    length(idx_v2)
  )
  section_df$medicat_gastro_1_m[idx_v2] <- sample_v2_medicat(
    c("Proton pump inhibitors", "Other indigestion medicine", "Laxatives",
      "Aminosalicylates (5-ASAs, mesalazine)", "Azathioprine", "Corticosteroids",
      "Mercaptopurine", "Methotrexate", "JAK inhibitors",
      "Tumour Necrosis Factor (TNF) inhibitors", "Pancreatin", "Other not listed"),
    length(idx_v2)
  )
  section_df$medicat_endocr_1_m[idx_v2] <- sample_v2_medicat(
    c("Levothyroxine", "Carbimazole", "Propylthiouracil", "Beta Blocker",
      "Hydrocortisone", "Prednisolone", "Growth hormone", "Desmopressin",
      "Dopamine agonists", "Other not listed"),
    length(idx_v2)
  )
  section_df$medicat_resp_1_m[idx_v2] <- sample_v2_medicat(
    c("Asthma reliver inhaler (usually blue)", "Asthma preventer inhaler (containing steroid medicine)",
      "Asthma combination inhaler", "Anticholinergic inhaler",
      "Leukotriene receptor antagonist (LTRAs) tablets", "Tablet bronchodilator",
      "Corticosteroids", "Other not listed"),
    length(idx_v2)
  )
  section_df$medicat_psych_1_m[idx_v2] <- sample_v2_medicat(
    c("Antidepressant", "Antipsychotic medication", "Beta-blocker",
      "Benzodiazepine", "Lithium", "Sleeping pills", "Pregabalin",
      "Valproic acid/Sodium valproate", "Other mood stabilising medication", "Other not listed"),
    length(idx_v2)
  )
  section_df$medicat_psych_antidepr_1_m[idx_v2] <- sample_v2_medicat(
    c("Selective serotonin reuptake inhibitor (SSRI)", "Tricyclic antidepressant", "Other"),
    length(idx_v2)
  )
  section_df$medicat_psych_antipsych_1_m[idx_v2] <- sample_v2_medicat(
    c("Typical antipsychotic", "Atypical antipsychotic"),
    length(idx_v2)
  )
  section_df$medicat_neuro_1_m[idx_v2] <- sample_v2_medicat(
    c("Anti-epileptic drugs (AEDs)", "Acetylcholinesterase (AChE) inhibitors",
      "Amantadine", "Amitriptyline for migraines",
      "Catechol-O-methyltransferase (COMT) inhibitors", "Levodopa",
      "Dopamine agonists", "Memantine", "Monoamine oxidase-B inhibitors",
      "Pregabalin or Gabapentin", "Propranolol for migraines", "Riluzole",
      "Triptans", "Other not listed"),
    length(idx_v2)
  )
  section_df$medicat_pain_1_m[idx_v2] <- sample_v2_medicat(
    c("Aspirin", "Ibuprofen (e.g., Nurofen)", "Paracetamol", "Naproxen",
      "Diclofenac", "Opioids", "Other not listed"),
    length(idx_v2)
  )
  section_df$medicat_repro_1_m[idx_v2] <- sample_v2_medicat(
    c("Contraceptive medication, coil, implant or patch",
      "Medication to treat erectile dysfunction",
      "Combined Hormone Replacement Therapy (HRT)", "Oestrogen-only HRT",
      "Oestrogen treatment (Pessary, cream or vaginal ring)", "Testosterone HRT",
      "Oestrogen or testosterone blockers", "Other not listed"),
    length(idx_v2)
  )
  section_df$medicat_suppl_1_m[idx_v2] <- sample_v2_medicat(
    c("Vitamin A", "Vitamin B", "Vitamin C", "Vitamin D", "Vitamin E",
      "Folic acid or Folate (Vit B9)", "Multivitamins +/- minerals",
      "Fish oil (including cod liver oil)", "Glucosamine", "Calcium", "Zinc",
      "Iron", "Selenium", "St John's wort", "Other not listed"),
    length(idx_v2)
  )
}

# Reproductive contraceptive medications (conditional on reproductive diagnosis).
section_df$medicat_repro_contracept_1_m <- rep(NA_character_, n)
if (length(repro_diag_idx) > 0)
  section_df$medicat_repro_contracept_1_m[repro_diag_idx] <- sample(
    c("None of the above", "Combined oral contraceptive", "Progesterone-only pill",
      "Hormone replacement therapy (HRT)", "Hormonal IUD", "GnRH analogue",
      "Metformin", "Fertility medication", "Other (not listed)", "Do not know", "Prefer not to answer"),
    length(repro_diag_idx), replace = TRUE,
    prob = c(0.26, 0.16, 0.10, 0.10, 0.10, 0.05, 0.07, 0.04, 0.05, 0.03, 0.04)
  )

# v1-only medications.
section_df$medicat_b_1_m        <- rep(NA_character_, n)
section_df$medicat_c_1_m        <- rep(NA_character_, n)
section_df$medicat_d_1_m        <- rep(NA_character_, n)
section_df$medicat_prescript_1_m <- rep(NA_character_, n)

if (length(idx_v1) > 0) {
  section_df$medicat_b_1_m[idx_v1] <- sample(
    c("Aspirin", "Ibuprofen (e.g. Nurofen)", "Paracetamol",
      "Ranitidine (e.g. Zantac)", "Omeprazole (e.g. Zanprol)",
      "Laxatives (e.g. Dulcolax, Senokot)", "None of the above",
      "Do not know", "Prefer not to answer"),
    length(idx_v1), replace = TRUE
  )
  section_df$medicat_c_1_m[idx_v1] <- sample(
    c("Vitamin A", "Vitamin B", "Vitamin C", "Vitamin D", "Vitamin E",
      "Folic acid or Folate (Vit B9)", "Multivitamins +/- minerals",
      "None of the above", "Prefer not to answer"),
    length(idx_v1), replace = TRUE
  )
  section_df$medicat_d_1_m[idx_v1] <- sample(
    c("Fish oil (including cod liver oil)", "Glucosamine", "Calcium", "Zinc",
      "Iron", "Selenium", "None of the above", "Prefer not to answer"),
    length(idx_v1), replace = TRUE
  )
  section_df$medicat_prescript_1_m[idx_v1] <- sample(
    c("Glucocorticoids or steroids", "Atypical antipsychotics", "Other", "No",
      "Do not know", "Prefer not to answer"),
    length(idx_v1), replace = TRUE
  )
}

section_df <- apply_pdf_value_catalog(section_df, questionnaire_data, cols)

write_section(section_df, "your_health")