if (file.exists("questionnaire/section_utils.R")) {
	source("questionnaire/section_utils.R")
} else {
	source("section_utils.R")
}

questionnaire_data <- get_questionnaire_data()

cols <- c(
	"pid",
	"edu_comp_age_1_1",
	"edu_comp_age_2_1",
	"edu_qual_1_m",
	"housing_income_1_1",
	"work_distance_1_1",
	"work_manual_labour_1_1",
	"work_nights_1_1",
	"work_shifts_1_1",
	"work_status_1_m",
	"work_status_2_m",
	"work_transport_1_m",
	"work_walk_stand_1_1",
	"work_wk_hrs_1_1",
	"work_wk_travel_1_1",
	"work_yrs_1_1"
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

# Education completion age (mutually exclusive v1 / v2 fields).
section_df$edu_comp_age_1_1 <- rep(NA_character_, n)
section_df$edu_comp_age_2_1 <- rep(NA_character_, n)
if (length(idx_v1) > 0)
  section_df$edu_comp_age_1_1[idx_v1] <- as.character(
    pmin(30, pmax(12, round(rnorm(length(idx_v1), mean = 19, sd = 2.8))))
  )
if (length(idx_v2) > 0)
  section_df$edu_comp_age_2_1[idx_v2] <- as.character(
    pmin(30, pmax(12, round(rnorm(length(idx_v2), mean = 19, sd = 2.8))))
  )

# Work status multi-select (v1 / v2 split).
generate_work_status <- function(m) {
  primary <- sample(
    c("In paid employment or self-employed", "Retired", "Looking after home and/or family",
      "Unable to work because of sickness or disability", "Full or part-time student",
      "Doing unpaid or voluntary work", "Unemployed",
      "On paid leave (e.g., maternity/paternity/sick leave)",
      "None of the above", "Prefer not to answer"),
    m, replace = TRUE,
    prob = c(0.58, 0.17, 0.05, 0.03, 0.04, 0.03, 0.05, 0.02, 0.015, 0.015)
  )
  add_second <- runif(m) < 0.14
  second <- sample(
    c("Retired", "Full or part-time student", "Looking after home and/or family",
      "Doing unpaid or voluntary work", "Unemployed"),
    m, replace = TRUE,
    prob = c(0.30, 0.25, 0.20, 0.15, 0.10)
  )
  out <- primary
  for (i in seq_len(m)) {
    if (isTRUE(add_second[i]) && !is.na(out[i]) && out[i] != second[i] &&
        !out[i] %in% c("None of the above", "Prefer not to answer")) {
      out[i] <- paste(out[i], second[i], sep = "; ")
    }
  }
  out
}

section_df$work_status_1_m <- rep(NA_character_, n)
section_df$work_status_2_m <- rep(NA_character_, n)
if (length(idx_v1) > 0) section_df$work_status_1_m[idx_v1] <- generate_work_status(length(idx_v1))
if (length(idx_v2) > 0) section_df$work_status_2_m[idx_v2] <- generate_work_status(length(idx_v2))

# Work detail fields: only for respondents in work-eligible status.
all_work_status <- section_df$work_status_1_m
all_work_status[is.na(all_work_status)] <- section_df$work_status_2_m[is.na(all_work_status)]

eligible_work_status <- c(
  "In paid employment or self-employed",
  "Looking after home and/or family",
  "Doing unpaid or voluntary work",
  "On paid leave (e.g., maternity/paternity/sick leave)"
)
ineligible_only_status <- c("Retired", "Full or part-time student")

status_tokens <- strsplit(ifelse(is.na(all_work_status), "", all_work_status), "; ", fixed = TRUE)
is_work_detail_eligible <- vapply(status_tokens, function(tokens) {
  tokens <- trimws(tokens[nzchar(tokens)])
  if (length(tokens) == 0L) return(FALSE)
  any(tokens %in% eligible_work_status) && !all(tokens %in% ineligible_only_status)
}, logical(1))
work_idx <- which(is_work_detail_eligible)

# Education qualification is populated from the PDF value catalog so multi-select
# responses (pipe-delimited) can be generated when appropriate.
section_df$edu_qual_1_m <- rep(NA_character_, n)

section_df$work_yrs_1_1      <- rep(NA_character_, n)
section_df$work_wk_hrs_1_1   <- rep(NA_character_, n)
section_df$work_wk_travel_1_1 <- rep(NA_character_, n)
section_df$work_distance_1_1  <- rep(NA_character_, n)
section_df$work_transport_1_m  <- rep(NA_character_, n)
section_df$work_walk_stand_1_1 <- rep(NA_character_, n)
section_df$work_manual_labour_1_1 <- rep(NA_character_, n)
section_df$work_shifts_1_1    <- rep(NA_character_, n)
section_df$work_nights_1_1    <- rep(NA_character_, n)

if (length(work_idx) > 0) {
  section_df$work_yrs_1_1[work_idx] <- as.character(
    pmin(50, pmax(0, round(rgamma(length(work_idx), shape = 2.2, scale = 4.5))))
  )
  section_df$work_wk_hrs_1_1[work_idx] <- as.character(
    pmin(90, pmax(1, round(rnorm(length(work_idx), mean = 36, sd = 12))))
  )
  section_df$work_wk_travel_1_1[work_idx] <- as.character(
    pmin(14, pmax(0, round(rnorm(length(work_idx), mean = 6, sd = 3))))
  )
  section_df$work_distance_1_1[work_idx] <- as.character(
    pmin(200, pmax(0, round(rlnorm(length(work_idx), meanlog = log(8), sdlog = 0.9))))
  )
  section_df$work_transport_1_m[work_idx] <- sample(
    c("Car or motor vehicle", "Train", "Bus or coach", "Walk", "Cycle",
      "Work from home", "Other", "Prefer not to answer"),
    length(work_idx), replace = TRUE,
    prob = c(0.50, 0.08, 0.07, 0.17, 0.05, 0.08, 0.03, 0.02)
  )
  section_df$work_walk_stand_1_1[work_idx] <- sample(
    c("Never/rarely", "Sometimes", "Usually", "Always", "Do not know", "Prefer not to answer"),
    length(work_idx), replace = TRUE,
    prob = c(0.28, 0.32, 0.22, 0.10, 0.03, 0.05)
  )
  section_df$work_manual_labour_1_1[work_idx] <- sample(
    c("Never/rarely", "Sometimes", "Usually", "Always", "Do not know", "Prefer not to answer"),
    length(work_idx), replace = TRUE,
    prob = c(0.52, 0.25, 0.12, 0.05, 0.03, 0.03)
  )
  section_df$work_shifts_1_1[work_idx] <- sample(
    c("Never/rarely", "Sometimes", "Usually", "Always", "Do not know", "Prefer not to answer"),
    length(work_idx), replace = TRUE,
    prob = c(0.62, 0.20, 0.09, 0.04, 0.02, 0.03)
  )
  night_shift_idx <- work_idx[
    section_df$work_shifts_1_1[work_idx] %in% c("Sometimes", "Usually", "Always")
  ]
  if (length(night_shift_idx) > 0) {
    section_df$work_nights_1_1[night_shift_idx] <- sample(
      c("Never/rarely", "Sometimes", "Usually", "Always", "Do not know", "Prefer not to answer"),
      length(night_shift_idx), replace = TRUE,
      prob = c(0.32, 0.35, 0.20, 0.07, 0.03, 0.03)
    )
  }
}

section_df <- apply_pdf_value_catalog(section_df, questionnaire_data, cols)

write_section(section_df, "work_and_education")