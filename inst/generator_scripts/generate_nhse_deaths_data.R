# Generate synthetic death records data
# Updated: Restricted to 200 of the 5000 total PIDs
# Guaranteed primary and underlying cause of death

# Source shared PID generation function
source("generate_pids.R")

set.seed(42)

config <- getOption("OFH_GEN_CONFIG", NULL)
total_pid_count <- if (!is.null(config)) config$total_pid_count else 5000
all_study_pids <- getOption("OFH_ALL_STUDY_PIDS", generate_pids(total_pid_count))
dataset_cfg <- if (!is.null(config)) config$datasets$nhse_engwal_deaths_data else NULL

n_total_study_people <- total_pid_count
n_deaths <- if (!is.null(dataset_cfg$unique_pids)) dataset_cfg$unique_pids else 200
if (identical(n_deaths, "ALL")) {
  n_deaths <- total_pid_count
}
if (n_deaths > n_total_study_people) {
  stop("nhse_engwal_deaths_data unique_pids cannot exceed total_pid_count")
}

code_cfg <- if (!is.null(config)) config$code_config$nhse_engwal_deaths_data else NULL
if (is.null(code_cfg)) {
  stop("Missing GEN_CONFIG$code_config$nhse_engwal_deaths_data. Run via 00_run_all_generators.R")
}

# 1. Generate the full pool of PIDs
# 2. Select the subset of 200 people who have death records
# Death records are unique (one per PID), so we don't sample with replacement here
death_pids <- sample(all_study_pids, n_deaths)

generate_primary_cod_entry <- function() {
  sample_code_with_description(
    code_cfg$icd10_descriptions,
    code_cfg$primary_icd10_weights
  )
}

generate_underlying_cod_entry <- function() {
  sample_code_with_description(
    code_cfg$icd10_descriptions,
    code_cfg$underlying_icd10_weights
  )
}

# 3. Generate death records data
# Everyone in this file has a primary and underlying cause of death
death_records <- data.frame(
  pid = death_pids,
  reg_date = as.Date("2020-01-01") + sample(0:1800, n_deaths, replace = TRUE),
  reg_date_of_death = as.Date("2020-01-01") + sample(0:1800, n_deaths, replace = TRUE),
  
  # Guaranteed Primary Cause
  s_cod_code_1 = sapply(1:n_deaths, function(i) generate_primary_cod_entry()),
  
  # Optional secondary causes (2nd through 4th)
  s_cod_code_2 = sapply(1:n_deaths, function(i) if (runif(1) < code_cfg$s_cod_code_2_missing_prob) NA_character_ else generate_primary_cod_entry()),
  s_cod_code_3 = sapply(1:n_deaths, function(i) if (runif(1) < code_cfg$s_cod_code_3_missing_prob) NA_character_ else generate_primary_cod_entry()),
  s_cod_code_4 = sapply(1:n_deaths, function(i) if (runif(1) < code_cfg$s_cod_code_4_missing_prob) NA_character_ else generate_primary_cod_entry()),
  
  # Empty slots for remaining COD fields
  s_cod_code_5 = NA_character_, s_cod_code_6 = NA_character_, s_cod_code_7 = NA_character_,
  s_cod_code_8 = NA_character_, s_cod_code_9 = NA_character_, s_cod_code_10 = NA_character_,
  s_cod_code_11 = NA_character_, s_cod_code_12 = NA_character_, s_cod_code_13 = NA_character_,
  s_cod_code_14 = NA_character_, s_cod_code_15 = NA_character_,
  
  # Guaranteed Underlying Cause
  s_underlying_cod_icd10 = sapply(1:n_deaths, function(i) generate_underlying_cod_entry()),
  
  stringsAsFactors = FALSE
)

# 4. Save to CSV
local({
  p <- "../data/nhse_engwal_deaths_data.csv"
  con <- file(p, "wb"); writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con); close(con)
  suppressWarnings(write.table(death_records, file = p, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE, qmethod = "double", na = "NA", fileEncoding = "UTF-8"))
})

# Summary Verification
cat("\nGenerated nhse_engwal_deaths_data.csv with", nrow(death_records), "rows and", ncol(death_records), "columns\n")
cat("Total deaths recorded:", nrow(death_records), "| PIDs with no death record:", n_total_study_people - nrow(death_records), "\n")