if (file.exists("questionnaire/section_utils.R")) {
	source("questionnaire/section_utils.R")
} else {
	source("section_utils.R")
}

questionnaire_data <- get_questionnaire_data()

cols <- c(
	"pid", "adoption_status_1_1", "birth_place_1_1", "father_age_1_1", "father_age_deceased_1_1", "father_alive_1_1",
	"father_diag_a_1_m", "father_diag_a_2_m", "father_diag_anaemia_1_m", "father_diag_auto_1_m", "father_diag_b_1_m",
	"father_diag_cancer_1_m", "father_diag_cancer_skin_1_m", "father_diag_cvd_1_m", "father_diag_endocr_1_m", "father_diag_gastro_1_m",
	"father_diag_neuro_1_m", "father_diag_neuro_dev_1_m", "father_diag_opthal_1_m", "father_diag_osteo_1_m", "father_diag_psych_1_m",
	"father_diag_psych_anx_1_m", "father_diag_psych_depr_1_m", "father_diag_psych_eat_1_m", "father_diag_repro_1_m", "father_diag_resp_1_m",
	"father_diag_urol_1_m", "immigrate_uk_yr_1_1", "mother_age_1_1", "mother_age_deceased_1_1", "mother_alive_1_1", "mother_diag_a_1_m",
	"mother_diag_a_2_m", "mother_diag_anaemia_1_m", "mother_diag_auto_1_m", "mother_diag_b_1_m", "mother_diag_cancer_1_m",
	"mother_diag_cancer_skin_1_m", "mother_diag_cvd_1_m", "mother_diag_endocr_1_m", "mother_diag_gastro_1_m", "mother_diag_neuro_1_m",
	"mother_diag_neuro_dev_1_m", "mother_diag_opthal_1_m", "mother_diag_osteo_1_m", "mother_diag_psych_1_m", "mother_diag_psych_anx_1_m",
	"mother_diag_psych_depr_1_m", "mother_diag_psych_eat_1_m", "mother_diag_repro_1_m", "mother_diag_resp_1_m", "mother_diag_urol_1_m",
	"sibling_diag_a_1_m", "sibling_diag_a_2_m", "sibling_diag_anaemia_1_m", "sibling_diag_auto_1_m", "sibling_diag_b_1_m", "sibling_diag_cancer_1_m",
	"sibling_diag_cancer_skin_1_m", "sibling_diag_cvd_1_m", "sibling_diag_endocr_1_m", "sibling_diag_gastro_1_m", "sibling_diag_neuro_1_m",
	"sibling_diag_neuro_dev_1_m", "sibling_diag_opthal_1_m", "sibling_diag_osteo_1_m", "sibling_diag_psych_1_m", "sibling_diag_psych_anx_1_m",
	"sibling_diag_psych_depr_1_m", "sibling_diag_psych_eat_1_m", "sibling_diag_repro_1_m", "sibling_diag_resp_1_m", "sibling_diag_urol_1_m",
	"sibling_num_brothers_1_1", "sibling_num_sisters_1_1"
)

questionnaire_data <- ensure_columns(questionnaire_data, cols)
questionnaire_data <- fill_defaults_for_columns(questionnaire_data, cols)
section_df <- questionnaire_data[, cols, drop = FALSE]

n <- nrow(section_df)

# Preserve questionnaire version split for version-specific answer sets.
if ("questionnaire_version" %in% names(questionnaire_data)) {
  idx_v1 <- which(questionnaire_data$questionnaire_version == 1)
  idx_v2 <- which(questionnaire_data$questionnaire_version == 2)
} else {
  idx_v1 <- integer(0)
  idx_v2 <- seq_len(n)
}

# Load canonical family answer dictionary derived from v1/v2 PDFs.
family_answer_lookup <- list()
if (requireNamespace("jsonlite", quietly = TRUE)) {
  generator_root <- resolve_generator_root()
  repo_root <- normalizePath(file.path(generator_root, "..", ".."), mustWork = FALSE)
  dict_path <- file.path(repo_root, "questionnaire_analysis", "question_answer_dictionary_family_health.json")

  if (file.exists(dict_path)) {
    family_dict <- jsonlite::fromJSON(dict_path, simplifyVector = FALSE)
    for (row in family_dict$columns) {
      cname <- tolower(row$column_name)
      family_answer_lookup[[cname]] <- list(
        all = unique(unlist(row$answer_texts, use.names = FALSE)),
        v1 = unique(unlist(row$answer_texts_by_version$v1, use.names = FALSE)),
        v2 = unique(unlist(row$answer_texts_by_version$v2, use.names = FALSE))
      )
    }
  }
}

clean_options <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  x <- gsub("^[-]?[0-9]+\\s+", "", x)
  unique(trimws(x))
}

is_numeric_only_col <- function(v) {
  non_empty <- as.character(v[!is.na(v) & v != ""])
  if (length(non_empty) == 0) return(FALSE)
  all(grepl("^[-+]?[0-9]+(\\.[0-9]+)?$", non_empty))
}

fill_from_dict_if_needed <- function(col, fallback_values = NULL) {
  if (!col %in% names(section_df)) return(invisible(NULL))

  v <- section_df[[col]]
  non_empty <- as.character(v[!is.na(v) & v != ""])
  has_prefixed_digit_artifact <- any(grepl("^[0-9]+\\s+[A-Za-z]", non_empty))
  should_replace <- all(is.na(v) | v == "") || is_numeric_only_col(v) || has_prefixed_digit_artifact
  if (!should_replace) return(invisible(NULL))

  options_all <- clean_options(family_answer_lookup[[col]]$all)
  options_v1 <- clean_options(family_answer_lookup[[col]]$v1)
  options_v2 <- clean_options(family_answer_lookup[[col]]$v2)
  if (length(options_all) == 0 && !is.null(fallback_values)) {
    options_all <- clean_options(fallback_values)
  }

  out <- rep(NA_character_, n)
  if (length(idx_v1) > 0) {
    if (length(options_v1) > 0) {
      out[idx_v1] <- sample(options_v1, length(idx_v1), replace = TRUE)
    } else if (length(options_all) > 0) {
      out[idx_v1] <- sample(options_all, length(idx_v1), replace = TRUE)
    }
  }
  if (length(idx_v2) > 0) {
    if (length(options_v2) > 0) {
      out[idx_v2] <- sample(options_v2, length(idx_v2), replace = TRUE)
    } else if (length(options_all) > 0) {
      out[idx_v2] <- sample(options_all, length(idx_v2), replace = TRUE)
    }
  }

  section_df[[col]] <<- out
  invisible(NULL)
}

fill_if_all_na <- function(col, values) {
	if (!col %in% names(section_df)) return(invisible(NULL))
	v <- section_df[[col]]
  if (all(is.na(v) | v == "") || is_numeric_only_col(v)) {
		section_df[[col]] <<- as.character(values)
	}
	invisible(NULL)
}

# Birth place options.
fill_if_all_na("birth_place_1_1", sample(
  c("England", "Wales", "Scotland", "Northern Ireland", "UK (don't know country)",
    "Republic of Ireland", "India", "Pakistan", "Poland", "Elsewhere",
    "Do not know", "Prefer not to answer"),
  n, replace = TRUE,
  prob = c(0.65, 0.03, 0.05, 0.015, 0.02, 0.015, 0.04, 0.02, 0.01, 0.10, 0.02, 0.015)
))

# Parent vital status.
fill_if_all_na("father_alive_1_1", sample(
  c("Yes", "No", "Do not know", "Prefer not to answer"),
  n, replace = TRUE, prob = c(0.30, 0.56, 0.09, 0.05)
))
fill_if_all_na("mother_alive_1_1", sample(
  c("Yes", "No", "Do not know", "Prefer not to answer"),
  n, replace = TRUE, prob = c(0.45, 0.42, 0.08, 0.05)
))

# Parent ages conditional on vital status (values read after fills above).
father_alive <- section_df$father_alive_1_1
mother_alive <- section_df$mother_alive_1_1

fill_if_all_na("father_age_1_1", ifelse(
  father_alive == "Yes",
  as.character(pmax(40, pmin(105, round(rnorm(n, mean = 73, sd = 8))))),
  NA_character_
))
fill_if_all_na("father_age_deceased_1_1", ifelse(
  father_alive == "No",
  as.character(pmax(30, pmin(100, round(rnorm(n, mean = 72, sd = 12))))),
  NA_character_
))
fill_if_all_na("mother_age_1_1", ifelse(
  mother_alive == "Yes",
  as.character(pmax(40, pmin(105, round(rnorm(n, mean = 71, sd = 8))))),
  NA_character_
))
fill_if_all_na("mother_age_deceased_1_1", ifelse(
  mother_alive == "No",
  as.character(pmax(30, pmin(100, round(rnorm(n, mean = 75, sd = 13))))),
  NA_character_
))

# Sibling counts.
fill_if_all_na("sibling_num_brothers_1_1", as.character(rpois(n, lambda = 0.9)))
fill_if_all_na("sibling_num_sisters_1_1", as.character(rpois(n, lambda = 0.9)))

# Family diagnosis columns: use PDF-derived per-column answer sets when available.
family_diag_cols <- grep("_diag_", names(section_df), value = TRUE)
for (col in family_diag_cols) fill_from_dict_if_needed(col)

# General family diagnosis options (v1).
fill_if_all_na("father_diag_a_1_m", sample(
  c("Cancer", "Diabetes", "Lung disease (COPD/asthma)", "Kidney disease",
    "Dementia/Alzheimer's", "Parkinson's disease", "Depression/anxiety",
    "Arthritis", "Other condition", "None of the above", "Do not know", "Prefer not to answer"),
  n, replace = TRUE,
  prob = c(0.18, 0.16, 0.10, 0.06, 0.08, 0.04, 0.06, 0.12, 0.08, 0.05, 0.04, 0.03)
))
fill_if_all_na("mother_diag_a_1_m", sample(
  c("Cancer", "Diabetes", "Osteoporosis", "Arthritis", "Thyroid disease",
    "Dementia/Alzheimer's", "Depression/anxiety", "Lung disease",
    "Other condition", "None of the above", "Do not know", "Prefer not to answer"),
  n, replace = TRUE,
  prob = c(0.16, 0.14, 0.11, 0.14, 0.10, 0.08, 0.08, 0.06, 0.06, 0.03, 0.02, 0.02)
))
fill_if_all_na("sibling_diag_a_1_m", sample(
  c("Cancer", "Diabetes", "Asthma", "Mental health condition", "Autoimmune condition",
    "Other condition", "None of the above", "Do not know", "Prefer not to answer"),
  n, replace = TRUE,
  prob = c(0.14, 0.18, 0.16, 0.15, 0.12, 0.10, 0.07, 0.05, 0.03)
))

# General family diagnosis options (v2).
diag_a_2_options <- c(
  "Autoimmune disorder", "Blood disorders (Anaemia)", "Cancer",
  "Digestive system or liver problems",
  "Endocrine, nutritional and metabolic disorders (e.g., diabetes, thyroid disorder, vitamin deficiencies)",
  "Eye or visual problems", "Fractures, breaks, or joint problems",
  "Heart or circulatory disease (e.g., high blood pressure or stroke)",
  "Kidney or urinary system disorders", "Lung or respiratory problems",
  "Mental health conditions (e.g. depression, bipolar disorder)",
  "Neurodevelopmental conditions (e.g., Autism spectrum disorder, ADHD)",
  "Neurological disorders (things that affect that brain or nervous system)",
  "Reproductive system problems", "Other not listed",
  "None of the above", "Do not know", "Prefer not to answer"
)
for (col in c("father_diag_a_2_m", "mother_diag_a_2_m", "sibling_diag_a_2_m")) {
  fill_if_all_na(col, sample(diag_a_2_options, n, replace = TRUE,
    prob = rep(1 / length(diag_a_2_options), length(diag_a_2_options))))
}

# CVD detail for family members.
family_cvd_options <- c(
  "Heart attack", "Stroke", "High blood pressure", "Angina",
  "Heart failure", "Atrial fibrillation", "Other cardiovascular condition",
  "Do not know", "Prefer not to answer"
)
fill_if_all_na("father_diag_cvd_1_m", sample(family_cvd_options, n, replace = TRUE,
  prob = c(0.22, 0.16, 0.32, 0.10, 0.06, 0.05, 0.04, 0.03, 0.02)))
fill_if_all_na("mother_diag_cvd_1_m", sample(family_cvd_options, n, replace = TRUE,
  prob = c(0.16, 0.18, 0.35, 0.11, 0.06, 0.05, 0.04, 0.03, 0.02)))
fill_if_all_na("sibling_diag_cvd_1_m", sample(
  c("Heart attack", "Stroke", "High blood pressure", "Diabetes",
    "Other cardiovascular condition", "Do not know", "Prefer not to answer"),
  n, replace = TRUE,
  prob = c(0.18, 0.12, 0.32, 0.20, 0.09, 0.05, 0.04)
))

# Rare/specific diagnosis history for family members.
family_diag_b_options <- c(
  "None of the above", "Do not know", "Prefer not to answer",
  "Parkinson's disease", "Severe depression", "Prostate cancer",
  "Gastric cancer", "Oesophagael cancer", "Lung cancer", "Bowel cancer"
)
for (col in c("father_diag_b_1_m", "mother_diag_b_1_m", "sibling_diag_b_1_m")) {
  fill_if_all_na(col, sample(family_diag_b_options, n, replace = TRUE,
    prob = rep(1 / length(family_diag_b_options), length(family_diag_b_options))))
}

section_df <- apply_pdf_value_catalog(section_df, questionnaire_data, cols)

# Keep vital-status columns coherent with death-age responses.
if ("father_age_deceased_1_1" %in% names(section_df) && "father_alive_1_1" %in% names(section_df)) {
  dead_father_idx <- which(!is.na(section_df$father_age_deceased_1_1) & section_df$father_age_deceased_1_1 != "")
  if (length(dead_father_idx) > 0) section_df$father_alive_1_1[dead_father_idx] <- "No"
  alive_yes_idx <- which(section_df$father_alive_1_1 == "Yes")
  if (length(alive_yes_idx) > 0) section_df$father_age_deceased_1_1[alive_yes_idx] <- NA_character_
  alive_no_idx <- which(section_df$father_alive_1_1 == "No")
  if (length(alive_no_idx) > 0) section_df$father_age_1_1[alive_no_idx] <- NA_character_
}
if ("mother_age_deceased_1_1" %in% names(section_df) && "mother_alive_1_1" %in% names(section_df)) {
  dead_mother_idx <- which(!is.na(section_df$mother_age_deceased_1_1) & section_df$mother_age_deceased_1_1 != "")
  if (length(dead_mother_idx) > 0) section_df$mother_alive_1_1[dead_mother_idx] <- "No"
  alive_yes_idx <- which(section_df$mother_alive_1_1 == "Yes")
  if (length(alive_yes_idx) > 0) section_df$mother_age_deceased_1_1[alive_yes_idx] <- NA_character_
  alive_no_idx <- which(section_df$mother_alive_1_1 == "No")
  if (length(alive_no_idx) > 0) section_df$mother_age_1_1[alive_no_idx] <- NA_character_
}

# Fallback for column where PDF extraction currently misses explicit options.
if ("sibling_diag_cancer_skin_1_m" %in% names(section_df)) {
  v <- section_df$sibling_diag_cancer_skin_1_m
  if (all(is.na(v) | v == "")) {
    section_df$sibling_diag_cancer_skin_1_m <- sample(
      c("None of the above", "Melanoma", "Basal cell", "Squamous cell", "Do not know", "Prefer not to answer"),
      n, replace = TRUE, prob = c(0.70, 0.09, 0.08, 0.06, 0.04, 0.03)
    )
  }
}

write_section(section_df, "family_health")