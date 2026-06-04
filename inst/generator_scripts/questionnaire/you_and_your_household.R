if (file.exists("questionnaire/section_utils.R")) {
	source("questionnaire/section_utils.R", local = TRUE)
} else {
	source("section_utils.R", local = TRUE)
}

questionnaire_data <- get_questionnaire_data()

cols <- c(
	"pid",
	"demog_height_1_1",
	"demog_height_enter_unit_1_1",
	"demog_language_1_1",
	"demog_relatsh_civil_curr_1_1",
	"demog_relatsh_civil_prev_1_1",
	"demog_relatsh_marr_curr_1_1",
	"demog_relatsh_marr_prev_1_1",
	"demog_relatsh_status_1_1",
	"demog_relatsh_status_2_1",
	"demog_sexual_orientation_1_1",
	"demog_transgender_1_1",
	"demog_weight_1_1",
	"demog_weight_enter_unit_1_1",
	"housing_curr_add_yrs_1_1",
	"housing_energy_1_m",
	"housing_heat_1_m",
	"housing_people_1_1",
	"housing_people_relate_1_m",
	"housing_tenure_1_1",
	"housing_type_1_1",
	"housing_vehicles_1_1"
)

questionnaire_data <- ensure_columns(questionnaire_data, cols)
questionnaire_data <- fill_defaults_for_columns(questionnaire_data, cols)
section_df <- questionnaire_data[, cols, drop = FALSE]

n <- nrow(section_df)

# Determine v1/v2 index split from questionnaire_version if present.
if ("questionnaire_version" %in% names(questionnaire_data)) {
  idx_v1 <- which(questionnaire_data$questionnaire_version == 1)
  idx_v2 <- which(questionnaire_data$questionnaire_version == 2)
} else {
  idx_v1 <- sample(seq_len(n), size = max(1L, round(0.05 * n)))
  idx_v2 <- setdiff(seq_len(n), idx_v1)
}

# Height and weight (continuous numeric, all respondents).
section_df$demog_height_1_1 <- as.character(
  round(pmin(220, pmax(120, rnorm(n, mean = 169, sd = 10))), 1)
)
section_df$demog_weight_1_1 <- as.character(
  round(pmin(240, pmax(35, rnorm(n, mean = 78, sd = 18))), 1)
)

# Sexual orientation and transgender (v2 only).
section_df$demog_sexual_orientation_1_1 <- rep(NA_character_, n)
section_df$demog_transgender_1_1        <- rep(NA_character_, n)
if (length(idx_v2) > 0) {
  section_df$demog_sexual_orientation_1_1[idx_v2] <- sample(
    c("Heterosexual or straight", "Gay or lesbian", "Bisexual",
      "Other sexual orientation", "Do not know", "Prefer not to answer"),
    length(idx_v2), replace = TRUE,
    prob = c(0.86, 0.02, 0.03, 0.01, 0.03, 0.05)
  )
  section_df$demog_transgender_1_1[idx_v2] <- sample(
    c("Yes", "No", "Prefer not to answer"),
    length(idx_v2), replace = TRUE,
    prob = c(0.007, 0.94, 0.053)
  )
}

# Relationship status (v1/v2 mutually exclusive).
section_df$demog_relatsh_status_1_1 <- rep(NA_character_, n)
section_df$demog_relatsh_status_2_1 <- rep(NA_character_, n)
relatsh_options <- c(
  "Never married and never registered a civil partnership", "Married",
  "In a registered civil partnership",
  "Separated, but still legally married",
  "Separated, but still legally in a civil partnership",
  "Divorced", "Formerly in a civil partnership which is now legally dissolved",
  "Widowed", "Surviving partner from a registered civil partnership",
  "Other", "Prefer not to answer"
)
relatsh_probs <- c(0.15, 0.45, 0.01, 0.03, 0.01, 0.10, 0.01, 0.08, 0.01, 0.05, 0.10)
if (length(idx_v1) > 0)
  section_df$demog_relatsh_status_1_1[idx_v1] <- sample(relatsh_options, length(idx_v1), replace = TRUE, prob = relatsh_probs)
if (length(idx_v2) > 0)
  section_df$demog_relatsh_status_2_1[idx_v2] <- sample(relatsh_options, length(idx_v2), replace = TRUE, prob = relatsh_probs)

# Conditional relationship follow-ups.
all_relatsh <- section_df$demog_relatsh_status_1_1
all_relatsh[is.na(all_relatsh)] <- section_df$demog_relatsh_status_2_1[is.na(all_relatsh)]

married_idx    <- which(all_relatsh == "Married")
civil_idx      <- which(all_relatsh == "In a registered civil partnership")
prev_marr_idx  <- which(all_relatsh %in% c("Divorced", "Widowed"))
prev_civil_idx <- which(all_relatsh %in% c(
  "Formerly in a civil partnership which is now legally dissolved",
  "Surviving partner from a registered civil partnership"
))

section_df$demog_relatsh_marr_curr_1_1  <- rep(NA_character_, n)
section_df$demog_relatsh_civil_curr_1_1 <- rep(NA_character_, n)
section_df$demog_relatsh_marr_prev_1_1  <- rep(NA_character_, n)
section_df$demog_relatsh_civil_prev_1_1 <- rep(NA_character_, n)

if (length(married_idx) > 0)
  section_df$demog_relatsh_marr_curr_1_1[married_idx] <- sample(
    c("Yes, to someone of the opposite sex", "Yes, to someone of the same sex", "Prefer not to answer"),
    length(married_idx), replace = TRUE, prob = c(0.95, 0.03, 0.02)
  )
if (length(civil_idx) > 0)
  section_df$demog_relatsh_civil_curr_1_1[civil_idx] <- sample(
    c("Yes, to someone of the opposite sex", "Yes, to someone of the same sex", "Prefer not to answer"),
    length(civil_idx), replace = TRUE, prob = c(0.20, 0.75, 0.05)
  )
if (length(prev_marr_idx) > 0)
  section_df$demog_relatsh_marr_prev_1_1[prev_marr_idx] <- sample(
    c("Yes, to someone of the opposite sex", "Yes, to someone of the same sex", "Prefer not to answer"),
    length(prev_marr_idx), replace = TRUE, prob = c(0.95, 0.03, 0.02)
  )
if (length(prev_civil_idx) > 0)
  section_df$demog_relatsh_civil_prev_1_1[prev_civil_idx] <- sample(
    c("Yes, to someone of the opposite sex", "Yes, to someone of the same sex", "Prefer not to answer"),
    length(prev_civil_idx), replace = TRUE, prob = c(0.20, 0.75, 0.05)
  )

# Housing.
section_df$housing_curr_add_yrs_1_1 <- as.character(
  pmax(0, round(rexp(n, rate = 0.07)))
)
# Multi-select household columns are populated from the PDF value catalog.
section_df$housing_people_1_1 <- rep(NA_character_, n)
section_df$housing_vehicles_1_1 <- sample(
  c("None", "One", "Two", "Three", "Four or more", "Do not know", "Prefer not to answer"),
  n, replace = TRUE,
  prob = c(0.10, 0.35, 0.35, 0.10, 0.05, 0.02, 0.03)
)

# Household composition is populated from the PDF value catalog.
section_df$housing_people_relate_1_m <- rep(NA_character_, n)

section_df <- apply_pdf_value_catalog(section_df, questionnaire_data, cols)

write_section(section_df, "you_and_your_household")