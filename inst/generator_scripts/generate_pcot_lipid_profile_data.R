# Generate synthetic PCOT lipid profile data
# This script creates fake lipid profile data for a subset of clinic participants

source("generate_pids.R")

set.seed(42)

config <- getOption("OFH_GEN_CONFIG", NULL)
total_pid_count <- if (!is.null(config)) config$total_pid_count else 5000
all_study_pids <- getOption("OFH_ALL_STUDY_PIDS", generate_pids(total_pid_count))
clinic_cfg <- if (!is.null(config)) config$datasets$clinic_measurements_data else NULL

# Get the number of people with clinic data
n_clinic <- if (!is.null(clinic_cfg$unique_pids)) clinic_cfg$unique_pids else round(0.7 * total_pid_count)
if (identical(n_clinic, "ALL")) n_clinic <- total_pid_count

# 80% of those with clinic data have lipid data
n_lipid <- as.integer(round(0.8 * n_clinic))

# Sample PIDs for lipid data
clinic_pids <- sample(all_study_pids, n_clinic)
lipid_pids <- sample(clinic_pids, n_lipid)

# Helper: generate random datetimes within a plausible range
random_datetimes <- function(n) {
  start_date <- as.POSIXct("2024-01-01 08:00:00")
  end_date <- as.POSIXct("2026-02-18 17:00:00")
  sec_diff <- as.numeric(difftime(end_date, start_date, units = "secs"))
  random_secs <- runif(n, 0, sec_diff)
  return(start_date + random_secs)
}

# Helper: generate skipped/reason columns for first and second readings
make_skipped_pair <- function(n, skipped_probs, reasons) {
  skipped <- sample(names(skipped_probs), n, replace = TRUE, prob = skipped_probs)
  reason <- ifelse(
    skipped == "skipped",
    sample(reasons, n, replace = TRUE),
    NA_character_
  )
  list(skipped = skipped, reason = reason)
}

# Skipped reasons for first reading (from example CSV)
first_skipped_probs <- c("not skipped" = 0.98, "skipped" = 0.02)
first_skipped_reasons <- c("other reason", "participant refused", "machine failure", "insufficient materials", "insufficient specimen")

first_skip <- make_skipped_pair(n_lipid, first_skipped_probs, first_skipped_reasons)

# Only 2% have second reading, rest are skipped
second_skipped_probs <- c("not skipped" = 0.02, "skipped" = 0.98)
second_skipped_reasons <- first_skipped_reasons

second_skip <- make_skipped_pair(n_lipid, second_skipped_probs, second_skipped_reasons)

# Generate values for first reading (if not skipped)
gen_lipid <- function(n, skip) {
  vals <- rep(NA, n)
  idx <- which(skip == "not skipped")
  if (length(idx) > 0) {
    vals[idx] <- round(rnorm(length(idx), mean = 5, sd = 1.2), 2)
  }
  vals
}
gen_hdl <- function(n, skip) {
  vals <- rep(NA, n)
  idx <- which(skip == "not skipped")
  if (length(idx) > 0) {
    vals[idx] <- round(rnorm(length(idx), mean = 1.4, sd = 0.3), 2)
  }
  vals
}
gen_tg <- function(n, skip) {
  vals <- rep(NA, n)
  idx <- which(skip == "not skipped")
  if (length(idx) > 0) {
    vals[idx] <- round(rnorm(length(idx), mean = 1.7, sd = 0.7), 2)
  }
  vals
}
gen_ldl <- function(n, skip) {
  vals <- rep(NA, n)
  idx <- which(skip == "not skipped")
  if (length(idx) > 0) {
    vals[idx] <- round(rnorm(length(idx), mean = 3, sd = 1), 2)
  }
  vals
}
gen_nonhdl <- function(n, skip, tc, hdl) {
  vals <- rep(NA, n)
  idx <- which(skip == "not skipped")
  if (length(idx) > 0) {
    vals[idx] <- round(tc[idx] - hdl[idx], 2)
  }
  vals
}
gen_ratio <- function(n, skip, tc, hdl) {
  vals <- rep(NA, n)
  idx <- which(skip == "not skipped" & !is.na(hdl) & hdl > 0)
  if (length(idx) > 0) {
    vals[idx] <- round(tc[idx] / hdl[idx], 2)
  }
  vals
}

# Appointment details
appointment_version <- sample(c("v1", "v2"), n_lipid, replace = TRUE)
appointment_datetime <- format(random_datetimes(n_lipid), "%Y-%m-%d %H:%M:%S")

# First reading values
tc_first <- gen_lipid(n_lipid, first_skip$skipped)
hdl_first <- gen_hdl(n_lipid, first_skip$skipped)
tg_first <- gen_tg(n_lipid, first_skip$skipped)
ldl_first <- gen_ldl(n_lipid, first_skip$skipped)
nonhdl_first <- gen_nonhdl(n_lipid, first_skip$skipped, tc_first, hdl_first)
tc_hdl_ratio_first <- gen_ratio(n_lipid, first_skip$skipped, tc_first, hdl_first)

# Second reading values
tc_second <- gen_lipid(n_lipid, second_skip$skipped)
hdl_second <- gen_hdl(n_lipid, second_skip$skipped)
tg_second <- gen_tg(n_lipid, second_skip$skipped)
ldl_second <- gen_ldl(n_lipid, second_skip$skipped)
nonhdl_second <- gen_nonhdl(n_lipid, second_skip$skipped, tc_second, hdl_second)
tc_hdl_ratio_second <- gen_ratio(n_lipid, second_skip$skipped, tc_second, hdl_second)

# Assemble data frame
pcot_lipid_profile <- data.frame(
  pid = lipid_pids,
  appointment_version = appointment_version,
  appointment_datetime = appointment_datetime,
  first_reading_skipped = first_skip$skipped,
  first_reading_skipped_reason = first_skip$reason,
  tc_first_reading = tc_first,
  hdl_first_reading = hdl_first,
  tg_first_reading = tg_first,
  ldl_first_reading = ldl_first,
  nonhdl_first_reading = nonhdl_first,
  tc_hdl_ratio_first_reading = tc_hdl_ratio_first,
  second_reading_skipped = second_skip$skipped,
  tc_second_reading = tc_second,
  hdl_second_reading = hdl_second,
  tg_second_reading = tg_second,
  ldl_second_reading = ldl_second,
  nonhdl_second_reading = nonhdl_second,
  tc_hdl_ratio_second_reading = tc_hdl_ratio_second,
  stringsAsFactors = FALSE
)

# Save to CSV
local({
  p <- "../data/pcot_lipid_profile_data.csv"
  con <- file(p, "wb"); writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con); close(con)
  suppressWarnings(write.table(pcot_lipid_profile, file = p, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE, qmethod = "double", na = "NA", fileEncoding = "UTF-8"))
})

cat("\nGenerated pcot_lipid_profile_data.csv\n")
cat(sprintf("%d rows and %d columns (%d unique PIDs)\n", nrow(pcot_lipid_profile), ncol(pcot_lipid_profile), length(unique(pcot_lipid_profile$pid))))
