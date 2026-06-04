# Generate synthetic emergency department data
# Updated: Dataset restricted to 2000 of the 5000 total PIDs
source("generate_pids.R", local = TRUE)
source("nhse_new_columns_helpers.R", local = TRUE)

set.seed(42)

config <- getOption("OFH_GEN_CONFIG", NULL)
total_pid_count <- if (!is.null(config)) config$total_pid_count else 5000
all_study_pids <- getOption("OFH_ALL_STUDY_PIDS", generate_pids(total_pid_count))
dataset_cfg <- if (!is.null(config)) config$datasets$nhse_ed_data else NULL

n_total_study_people <- total_pid_count
n_ed_people <- if (!is.null(dataset_cfg$unique_pids)) dataset_cfg$unique_pids else 2000
if (identical(n_ed_people, "ALL")) {
  n_ed_people <- total_pid_count
}
total_records <- if (!is.null(dataset_cfg$total_records)) dataset_cfg$total_records else 8000

if (n_ed_people > n_total_study_people) {
  stop("nhse_ed_data unique_pids cannot exceed total_pid_count")
}
if (total_records < n_ed_people) {
  stop("nhse_ed_data total_records must be >= unique_pids")
}
if (total_records > 0 && n_ed_people == 0) {
  stop("nhse_ed_data unique_pids must be > 0 when total_records > 0")
}

code_cfg <- if (!is.null(config)) config$code_config$nhse_ed_data else NULL
if (is.null(code_cfg)) {
  code_cfg <- default_nhse_code_config("nhse_ed_data")
}

# 1. Generate the full pool of PIDs
# 2. Select the subset of 2000 people who actually visited A&E
ed_pids_pool <- sample(all_study_pids, n_ed_people)

# 3. Create the 8000 records using ONLY those 2000 PIDs
# This ensures that many of the 2000 people have multiple repeat visits
pids_for_data <- c(ed_pids_pool, sample(ed_pids_pool, total_records - n_ed_people, replace = TRUE))

# 5. Create ED Function
create_ed <- function(pids) {
  n_rows <- length(pids)
  if (n_rows == 0) {
    df <- data.frame(
      pid = character(0),
      arrivaldate = as.Date(character(0)),
      diagscheme = character(0),
      stringsAsFactors = FALSE
    )
    for (nm in sprintf("diag_%02d", 1:12)) df[[nm]] <- character(0)
    return(df)
  }
  
  # Randomly assign schemes
  diagscheme_values <- sample(
    code_cfg$diagscheme_levels,
    n_rows, replace = TRUE, prob = code_cfg$diagscheme_probs
  )
  
  # Helper to pick code
  pick_code <- function(scheme) {
    if (is.na(scheme) || scheme == "Null") return(NA_character_)
    if (scheme == "01 = Accident & Emergency diagnoses") {
      if (runif(1) < code_cfg$ae_scheme_icd10_prob) {
        return(sample_value_from_pool(code_cfg$icd10_pool, code_cfg$icd10_weights))
      }
      return(sample_value_from_pool(code_cfg$ae_specific_codes, code_cfg$ae_specific_weights))
    } else if (scheme == "02 = ICD-10") {
      return(sample_value_from_pool(code_cfg$icd10_pool, code_cfg$icd10_weights))
    } else if (scheme == "04 = Read coded clinical terms version 2") {
      return(sample_value_from_pool(code_cfg$read_codes_pool, code_cfg$read_weights))
    }
    return(NA_character_)
  }

  probs_missing <- code_cfg$diag_missing_probs
  
  diag_cols <- list()
  for (d in 1:12) {
    col_name <- paste0("diag_", sprintf("%02d", d))
    prob_missing <- probs_missing[d]
    
    diag_cols[[col_name]] <- sapply(1:n_rows, function(i) {
      if (d > 1 && runif(1) < prob_missing) return(NA_character_)
      pick_code(diagscheme_values[i])
    })
  }
  
  df <- data.frame(
    pid = pids,
    arrivaldate = as.Date("2020-01-01") + sample(0:1800, n_rows, replace = TRUE),
    diagscheme = diagscheme_values,
    stringsAsFactors = FALSE
  )
  
  df <- cbind(df, as.data.frame(diag_cols, stringsAsFactors = FALSE))
  
  # Sort by PID and Date for a clean clinical history
  df <- df[order(df$pid, df$arrivaldate), ]
  return(df)
}

nhse_eng_ed <- create_ed(pids_for_data)
nhse_eng_ed <- add_nhse_ed_new_columns(nhse_eng_ed)

# 6. Save to CSV
local({
  p <- "../data/nhse_ed_data.csv"
  con <- file(p, "wb"); writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con); close(con)
  suppressWarnings(write.table(nhse_eng_ed, file = p, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE, qmethod = "double", na = "NA", fileEncoding = "UTF-8"))
})

# Summary for verification
actual_pids_in_data <- length(unique(nhse_eng_ed$pid))
message("Generated nhse_ed_data.csv")
message(sprintf("%d rows and %d columns (%d unique PIDs with %d visits)", nrow(nhse_eng_ed), ncol(nhse_eng_ed), actual_pids_in_data, nrow(nhse_eng_ed)))