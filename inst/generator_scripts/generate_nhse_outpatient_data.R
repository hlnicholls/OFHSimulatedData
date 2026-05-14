# Generate synthetic NHSE outpatient data
# Updated: Dataset restricted to 2000 of the 5000 total PIDs
# Every visit guaranteed to have a primary diagnosis

# Source shared PID generation function
source("generate_pids.R")
source("nhse_new_columns_helpers.R")

set.seed(42)

config <- getOption("OFH_GEN_CONFIG", NULL)
total_pid_count <- if (!is.null(config)) config$total_pid_count else 5000
all_study_pids <- getOption("OFH_ALL_STUDY_PIDS", generate_pids(total_pid_count))
dataset_cfg <- if (!is.null(config)) config$datasets$nhse_outpat_data else NULL

n_total_study_people <- total_pid_count
n_outpat_people <- if (!is.null(dataset_cfg$unique_pids)) dataset_cfg$unique_pids else 2000
if (identical(n_outpat_people, "ALL")) {
  n_outpat_people <- total_pid_count
}
total_records <- if (!is.null(dataset_cfg$total_records)) dataset_cfg$total_records else 8000

if (n_outpat_people > n_total_study_people) {
  stop("nhse_outpat_data unique_pids cannot exceed total_pid_count")
}
if (total_records < n_outpat_people) {
  stop("nhse_outpat_data total_records must be >= unique_pids")
}
if (total_records > 0 && n_outpat_people == 0) {
  stop("nhse_outpat_data unique_pids must be > 0 when total_records > 0")
}

code_cfg <- if (!is.null(config)) config$code_config$nhse_outpat_data else NULL
if (is.null(code_cfg)) {
  code_cfg <- default_nhse_code_config("nhse_outpat_data")
}

# 1. Generate the full pool of PIDs
# 2. Select the subset of 2000 people who actually have outpatient records
outpat_pids_pool <- sample(all_study_pids, n_outpat_people)

# 3. Create the 8000 records using ONLY those 2000 PIDs
# This models follow-up appointments (multiple rows per PID)
pids_for_data <- c(outpat_pids_pool, sample(outpat_pids_pool, total_records - n_outpat_people, replace = TRUE))

generate_icd10_entry <- function() {
  sample_code_with_description(
    code_cfg$icd10_descriptions,
    code_cfg$icd10_weights
  )
}

generate_opcs4_entry <- function() {
  sample_code_with_description(
    code_cfg$opcs4_descriptions,
    code_cfg$opcs4_weights
  )
}

# 4. Create Outpatient Function
create_outpat <- function(pids) {
  n_rows <- length(pids)
  if (n_rows == 0) {
    df <- data.frame(
      pid = character(0),
      apptdate = as.Date(character(0)),
      stringsAsFactors = FALSE
    )
    for (nm in sprintf("diag_4_%02d", 1:12)) df[[nm]] <- character(0)
    for (nm in sprintf("opertn_%02d", 1:24)) df[[nm]] <- character(0)
    return(df)
  }
  
  df <- data.frame(
    pid = pids,
    apptdate = as.Date("2020-01-01") + sample(0:1800, n_rows, replace = TRUE),
    
    # Primary Diagnosis (Guaranteed non-NA)
    diag_4_01 = sapply(1:n_rows, function(i) generate_icd10_entry()),
    
    # Secondary Diagnoses
    diag_4_02 = sapply(1:n_rows, function(i) if (runif(1) < code_cfg$diag_4_02_missing_prob) NA_character_ else generate_icd10_entry()),
    diag_4_03 = sapply(1:n_rows, function(i) if (runif(1) < code_cfg$diag_4_03_missing_prob) NA_character_ else generate_icd10_entry()),
    
    # Empty slots
    diag_4_04 = NA_character_, diag_4_05 = NA_character_, diag_4_06 = NA_character_,
    diag_4_07 = NA_character_, diag_4_08 = NA_character_, diag_4_09 = NA_character_,
    diag_4_10 = NA_character_, diag_4_11 = NA_character_, diag_4_12 = NA_character_,
    
    # Procedures
    opertn_01 = sapply(1:n_rows, function(i) {
      if (runif(1) < code_cfg$opertn_01_no_procedure_prob) "- = No procedures performed" else generate_opcs4_entry()
    }),
    opertn_02 = sapply(1:n_rows, function(i) if (runif(1) < code_cfg$opertn_02_missing_prob) NA_character_ else generate_opcs4_entry()),
    
    # Remaining Procedure slots
    opertn_03 = NA_character_, opertn_04 = NA_character_, opertn_05 = NA_character_,
    opertn_06 = NA_character_, opertn_07 = NA_character_, opertn_08 = NA_character_,
    opertn_09 = NA_character_, opertn_10 = NA_character_, opertn_11 = NA_character_,
    opertn_12 = NA_character_, opertn_13 = NA_character_, opertn_14 = NA_character_,
    opertn_15 = NA_character_, opertn_16 = NA_character_, opertn_17 = NA_character_,
    opertn_18 = NA_character_, opertn_19 = NA_character_, opertn_20 = NA_character_,
    opertn_21 = NA_character_, opertn_22 = NA_character_, opertn_23 = NA_character_,
    opertn_24 = NA_character_,
    
    stringsAsFactors = FALSE
  )
  
  # Chronological sort per patient
  df <- df[order(df$pid, df$apptdate), ]
  return(df)
}

nhse_eng_outpat <- create_outpat(pids_for_data)
nhse_eng_outpat <- add_nhse_outpat_new_columns(nhse_eng_outpat)

# 5. Save to CSV
local({
  p <- "../data/nhse_outpat_data.csv"
  con <- file(p, "wb"); writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con); close(con)
  suppressWarnings(write.table(nhse_eng_outpat, file = p, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE, qmethod = "double", na = "NA", fileEncoding = "UTF-8"))
})

# Summary Verification
actual_pids <- length(unique(nhse_eng_outpat$pid))
cat("\nGenerated nhse_outpat_data.csv\n")
cat(sprintf("%d rows and %d columns (%d unique PIDs with %d visits)\n", nrow(nhse_eng_outpat), ncol(nhse_eng_outpat), actual_pids, nrow(nhse_eng_outpat)))