# Generate synthetic NHSE inpatient data
# Updated: Dataset restricted to only 2000 of the 5000 total PIDs

# Source shared PID generation function
source("generate_pids.R")
source("nhse_new_columns_helpers.R")

set.seed(42)

config <- get0("GEN_CONFIG", ifnotfound = NULL)
total_pid_count <- if (!is.null(config)) config$total_pid_count else 5000
all_study_pids <- get0("ALL_STUDY_PIDS", ifnotfound = generate_pids(total_pid_count))
dataset_cfg <- if (!is.null(config)) config$datasets$nhse_inpat_data else NULL

n_total_study_people <- total_pid_count
n_inpatient_people <- if (!is.null(dataset_cfg$unique_pids)) dataset_cfg$unique_pids else 2000
if (identical(n_inpatient_people, "ALL")) {
  n_inpatient_people <- total_pid_count
}
total_records <- if (!is.null(dataset_cfg$total_records)) dataset_cfg$total_records else 7500

if (n_inpatient_people > n_total_study_people) {
  stop("nhse_inpat_data unique_pids cannot exceed total_pid_count")
}
if (total_records < n_inpatient_people) {
  stop("nhse_inpat_data total_records must be >= unique_pids")
}
if (total_records > 0 && n_inpatient_people == 0) {
  stop("nhse_inpat_data unique_pids must be > 0 when total_records > 0")
}

code_cfg <- if (!is.null(config)) config$code_config$nhse_inpat_data else NULL
if (is.null(code_cfg)) {
  code_cfg <- default_nhse_code_config("nhse_inpat_data")
}

# 1. Generate the full pool of PIDs (to match your other datasets)
# 2. Select the subset of 2000 people who actually went to the hospital
inpatient_pids_pool <- sample(all_study_pids, n_inpatient_people)

# 3. Create the 7500 records using ONLY those 2000 PIDs
# This naturally results in ~100% of these 2000 people having repeat visits
pids_for_data <- c(inpatient_pids_pool, sample(inpatient_pids_pool, total_records - n_inpatient_people, replace = TRUE))

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

# 4. Create inpatient data 

# Helper to assign diagnoses to columns
assign_diags_to_cols <- function(diags, n_diag_cols = 20) {
  # Pad with NAs if fewer than n_diag_cols
  out <- rep(NA_character_, n_diag_cols)
  n <- min(length(diags), n_diag_cols)
  if (n > 0) out[1:n] <- diags[1:n]
  return(out)
}

create_inpat <- function(pids) {
  n_rows <- length(pids)
  n_diag_cols <- 20
  diag_col_names <- sprintf("diag_4_%02d", 1:n_diag_cols)

  if (n_rows == 0) {
    df <- data.frame(
      pid = character(0),
      admidate = as.Date(character(0)),
      epistart = as.Date(character(0)),
      stringsAsFactors = FALSE
    )
    for (nm in diag_col_names) df[[nm]] <- character(0)
    for (nm in sprintf("opertn_%02d", 1:24)) df[[nm]] <- character(0)
    return(df)
  }

  diag_matrix <- t(sapply(1:n_rows, function(i) {
    n_diags <- if(runif(1) < code_cfg$single_diag_prob) 1 else sample(2:3, 1)
    diagnoses <- sapply(1:n_diags, function(j) generate_icd10_entry())
    assign_diags_to_cols(diagnoses, n_diag_cols)
  }))

  admidate_vals <- as.Date("2020-01-01") + sample(0:1800, n_rows, replace = TRUE)
  
  df <- data.frame(
    pid = pids,
    admidate = admidate_vals,
    epistart = admidate_vals,
    diag_matrix,
    opertn_01 = sapply(1:n_rows, function(i) if (runif(1) < code_cfg$opertn_01_missing_prob) NA_character_ else generate_opcs4_entry()),
    opertn_02 = sapply(1:n_rows, function(i) if (runif(1) < code_cfg$opertn_02_missing_prob) NA_character_ else generate_opcs4_entry()),
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

  names(df)[4:(3+n_diag_cols)] <- diag_col_names
  df <- df[order(df$pid, df$admidate), ]
  return(df)
}

nhse_eng_inpat <- create_inpat(pids_for_data)
nhse_eng_inpat <- add_nhse_inpat_new_columns(nhse_eng_inpat)

# 5. Save to CSV
local({
  p <- "../data/nhse_inpat_data.csv"
  con <- file(p, "wb"); writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con); close(con)
  suppressWarnings(write.table(nhse_eng_inpat, file = p, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE, qmethod = "double", na = "NA", fileEncoding = "UTF-8"))
})

# Summary Verification
actual_pids_in_data <- length(unique(nhse_eng_inpat$pid))
cat("\nGenerated nhse_inpat_data.csv\n")
cat(sprintf("%d rows and %d columns (%d unique PIDs with %d admissions)\n", nrow(nhse_eng_inpat), ncol(nhse_eng_inpat), actual_pids_in_data, nrow(nhse_eng_inpat)))