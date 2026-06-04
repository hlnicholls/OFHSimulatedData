# Generate synthetic participant data
# This script creates fake participant data for 5000 individuals

# Source shared PID generation function
source("generate_pids.R", local = TRUE)

set.seed(42)

config <- getOption("OFH_GEN_CONFIG", NULL)
total_pid_count <- if (!is.null(config)) config$total_pid_count else 5000
all_study_pids <- getOption("OFH_ALL_STUDY_PIDS", generate_pids(total_pid_count))
dataset_cfg <- if (!is.null(config)) config$datasets$participant_data else NULL

if (!is.null(dataset_cfg) &&
    !identical(dataset_cfg$unique_pids, "ALL") &&
    dataset_cfg$unique_pids != total_pid_count) {
  stop("participant_data must always use all study PIDs; set unique_pids = 'ALL'")
}

n_people <- total_pid_count

# Generate consistent alphanumeric PIDs
pids <- all_study_pids

# Only 5% have values in _1_1 columns, rest are NA
sex_1_1_idx <- sample(1:n_people, size = round(0.05 * n_people))
gender_1_1_idx <- sample(1:n_people, size = round(0.05 * n_people))

# Initialize all as NA
demog_sex_1_1 <- rep(NA, n_people)
demog_sex_2_1 <- rep(NA, n_people)
demog_gender_1_1 <- rep(NA, n_people)
demog_gender_2_1 <- rep(NA, n_people)

# Assign values to _1_1 columns for 5%, and _2_1 for the rest
sex_options <- c("Male", "Female", "Prefer not to answer", "Intersex")
sex_probabilities <- c(0.499, 0.499, 0.001, 0.001)

demog_sex_1_1[sex_1_1_idx] <- sample(sex_options, length(sex_1_1_idx), replace = TRUE, prob = sex_probabilities)
demog_sex_2_1[-sex_1_1_idx] <- sample(sex_options, n_people - length(sex_1_1_idx), replace = TRUE, prob = sex_probabilities)

demog_gender_1_1[gender_1_1_idx] <- sample(c("Man", "Woman", "Non-binary"), length(gender_1_1_idx), replace = TRUE, prob = c(0.48, 0.48, 0.04))
demog_gender_2_1[-gender_1_1_idx] <- sample(c("Man", "Woman", "Non-binary", NA), n_people - length(gender_1_1_idx), replace = TRUE, prob = c(0.48, 0.48, 0.02, 0.02))

# Generate participant data

# Generate registration_year and registration_month (all after November 2022)
registration_year_month <- sample(
  seq(as.Date("2022-12-01"), as.Date("2024-12-01"), by = "month"),
  n_people, replace = TRUE
)
registration_year <- as.integer(format(registration_year_month, "%Y"))
registration_month <- as.integer(format(registration_year_month, "%m"))

consent_versions <- as.vector(outer(1:3, 1:8, function(major, minor) paste0(major, ".", minor)))
consent_version <- sample(consent_versions, n_people, replace = TRUE)

participant <- data.frame(
  pid = pids,
  consent_version = consent_version,
  consent_year = registration_year,
  consent_month = registration_month,
  birth_year = sample(1940:2005, n_people, replace = TRUE),
  birth_month = sample(1:12, n_people, replace = TRUE),
  registration_year = registration_year,
  registration_month = registration_month,
  blood_sample = sample(c("Valid Sample", "Invalid Sample"), n_people, replace = TRUE, prob = c(0.75, 0.25)),
  demog_ethnicity_1_1 = sample(
    c(
      "Arab",
      "Any other Asian/Asian British background",
      "Any other Black / African / Caribbean background",
      "Any other mixed multiple ethnic background",
      "Asian or Asian British – Bangladeshi",
      "Asian or Asian British – Indian",
      "Asian or Asian British – Pakistani",
      "Mixed – White and Asian",
      "Mixed – White and Black African",
      "Mixed – White and Black Caribbean",
      "Any other white background",
      "White – English / Welsh / Scottish / Northern Irish / British",
      "White – Gypsy or Irish Traveller",
      "White – Irish",
      "White – Polish",
      "Black or Black British – African",
      "Black or Black British – Caribbean",
      "Prefer not to answer",
      NA
    ),
    n_people,
    replace = TRUE,
    prob = c(
      0.02, 0.03, 0.02, 0.015, 0.025, 0.05, 0.035,
      0.02, 0.015, 0.015, 0.08, 0.50, 0.005, 0.02,
      0.03, 0.04, 0.03, 0.01, 0.04
    )
  ),
  demog_sex_1_1 = demog_sex_1_1,
  demog_sex_2_1 = demog_sex_2_1,
  demog_gender_1_1 = demog_gender_1_1,
  demog_gender_2_1 = demog_gender_2_1
)

# Save to CSV
local({
  p <- "../data/participant_data.csv"
  con <- file(p, "wb"); writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con); close(con)
  suppressWarnings(write.table(participant, file = p, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE, qmethod = "double", na = "NA", fileEncoding = "UTF-8"))
})
message("Generated participant_data.csv")
message(sprintf("%d rows and %d columns", nrow(participant), ncol(participant)))
