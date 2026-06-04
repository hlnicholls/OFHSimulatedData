# Generate synthetic clinic measurements data
# This script creates fake clinic measurements for 5000 individuals

# Source shared PID generation function
# Ensure generate_pids.R exists in your working directory
source("generate_pids.R", local = TRUE)

set.seed(42)

config <- getOption("OFH_GEN_CONFIG", NULL)
total_pid_count <- if (!is.null(config)) config$total_pid_count else 5000
all_study_pids <- getOption("OFH_ALL_STUDY_PIDS", generate_pids(total_pid_count))
dataset_cfg <- if (!is.null(config)) config$datasets$clinic_measurements_data else NULL

n_people <- if (!is.null(dataset_cfg$unique_pids)) dataset_cfg$unique_pids else 5000
if (identical(n_people, "ALL")) {
  n_people <- total_pid_count
}
if (n_people > total_pid_count) {
  stop("clinic_measurements_data unique_pids cannot exceed total_pid_count")
}

# Generate consistent alphanumeric PIDs
pids <- sample(all_study_pids, n_people)

# Helper function to generate random datetimes
random_datetimes <- function(n) {
  start_date <- as.POSIXct("2024-01-01 08:00:00")
  end_date <- as.POSIXct("2026-02-18 17:00:00")
  sec_diff <- as.numeric(difftime(end_date, start_date, units = "secs"))
  random_secs <- runif(n, 0, sec_diff)
  return(start_date + random_secs)
}

# Helper: generate a _skipped / _skipped_reason pair
# 99% "not skipped"; for the 1% skipped ~85% "other reason", ~15% "participant refused"
make_skipped_pair <- function(n) {
  skipped <- sample(c("not skipped", "skipped"), n, replace = TRUE, prob = c(0.99, 0.01))
  reason <- ifelse(
    skipped == "skipped",
    sample(c("other reason", "participant refused"), n, replace = TRUE, prob = c(0.85, 0.15)),
    NA_character_
  )
  list(skipped = skipped, reason = reason)
}

height_skip       <- make_skipped_pair(n_people)
weight_skip       <- make_skipped_pair(n_people)
waist_skip        <- make_skipped_pair(n_people)
heart_first_skip  <- make_skipped_pair(n_people)
heart_second_skip <- make_skipped_pair(n_people)
heart_third_skip  <- make_skipped_pair(n_people)

# Generate clinic measurements data
clinic_measurements <- data.frame(
  pid = pids,
  # Appointment Details
  appointment_datetime = format(random_datetimes(n_people), "%Y-%m-%d %H:%M:%S"),
  appointment_version = sample(c("v1", "v2"), n_people, replace = TRUE),
  
  # Physical Measurements
  height = as.integer(round(rnorm(n_people, mean = 170, sd = 10))), # Integer
  height_skipped = height_skip$skipped,
  height_skipped_reason = height_skip$reason,
  weight = round(rnorm(n_people, mean = 78, sd = 18), 1),           # 1 decimal place
  weight_skipped = weight_skip$skipped,
  weight_skipped_reason = weight_skip$reason,
  waist = round(rnorm(n_people, mean = 88, sd = 15), 1),            # 1 decimal place
  waist_skipped = waist_skip$skipped,
  waist_skipped_reason = waist_skip$reason,

  # Heart First Reading
  heart_first_rate = as.integer(round(rnorm(n_people, mean = 70, sd = 12))),
  heart_first_bp_diastolic = as.integer(round(rnorm(n_people, mean = 78, sd = 10))),
  heart_first_bp_systolic = as.integer(round(rnorm(n_people, mean = 128, sd = 15))),
  heart_first_rhythm = sample(c("regular", "irregular",  NA), n_people, replace = TRUE, prob = c(0.85, 0.08, 0.02)),
  heart_first_skipped = heart_first_skip$skipped,
  heart_first_skipped_reason = heart_first_skip$reason,

  # Heart Second Reading
  heart_second_rate = as.integer(round(rnorm(n_people, mean = 70, sd = 12))),
  heart_second_bp_diastolic = as.integer(round(rnorm(n_people, mean = 78, sd = 10))),
  heart_second_bp_systolic = as.integer(round(rnorm(n_people, mean = 128, sd = 15))),
  heart_second_rhythm = sample(c("regular", "irregular", NA), n_people, replace = TRUE, prob = c(0.85, 0.08, 0.02)),
  heart_second_skipped = heart_second_skip$skipped,
  heart_second_skipped_reason = heart_second_skip$reason,

  # Heart Third Reading
  heart_third_rate = as.integer(round(rnorm(n_people, mean = 70, sd = 12))),
  heart_third_bp_diastolic = as.integer(round(rnorm(n_people, mean = 78, sd = 10))),
  heart_third_bp_systolic = as.integer(round(rnorm(n_people, mean = 128, sd = 15))),
  heart_third_rhythm = sample(c("regular", "irregular", NA), n_people, replace = TRUE, prob = c(0.85, 0.08, 0.02)),
  heart_third_skipped = heart_third_skip$skipped,
  heart_third_skipped_reason = heart_third_skip$reason
)

# Save to CSV (Note: file name fixed from double dots)
local({
  p <- "../data/clinic_measurements_data.csv"
  con <- file(p, "wb"); writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con); close(con)
  suppressWarnings(write.table(clinic_measurements, file = p, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE, qmethod = "double", na = "NA", fileEncoding = "UTF-8"))
})

unique_pids_clinic <- length(unique(clinic_measurements$pid))
message("Generated clinic_measurements_data.csv")
message(sprintf("%d rows and %d columns (%d unique PIDs)", nrow(clinic_measurements), ncol(clinic_measurements), unique_pids_clinic))