# Generate synthetic country/region data
# This script creates fake country and region data for 5000 individuals

# Source shared PID generation function
source("generate_pids.R")

set.seed(42)

config <- get0("GEN_CONFIG", ifnotfound = NULL)
total_pid_count <- if (!is.null(config)) config$total_pid_count else 5000
all_study_pids <- get0("ALL_STUDY_PIDS", ifnotfound = generate_pids(total_pid_count))
dataset_cfg <- if (!is.null(config)) config$datasets$country_region_data else NULL

n_people <- if (!is.null(dataset_cfg$unique_pids)) dataset_cfg$unique_pids else 5000
if (identical(n_people, "ALL")) {
  n_people <- total_pid_count
}
if (n_people > total_pid_count) {
  stop("country_region_data unique_pids cannot exceed total_pid_count")
}

# Generate consistent alphanumeric PIDs
pids <- sample(all_study_pids, n_people)

# Generate country and region data
country_region <- data.frame(
  pid = pids,
  country_at_reg = sample(c("England", "Wales", "Scotland", "Northern Ireland", NA), n_people, replace = TRUE, prob = c(0.8, 0.1, 0.05, 0.03, 0.02)),
  region_at_reg = sample(c("North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", 
                           "East of England", "London", "South East", "South West", "East Anglia", NA), 
                        n_people, replace = TRUE, prob = c(0.08, 0.1, 0.08, 0.08, 0.08, 0.1, 0.12, 0.12, 0.08, 0.05, 0.03))
)

# Save to CSV
local({
  p <- "../data/country_region_data.csv"
  con <- file(p, "wb"); writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con); close(con)
  suppressWarnings(write.table(country_region, file = p, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE, qmethod = "double", na = "NA", fileEncoding = "UTF-8"))
})
unique_pids_region <- length(unique(country_region$pid))
cat("\nGenerated country_region_data.csv\n")
cat(sprintf("%d rows and %d columns (%d unique PIDs)\n", nrow(country_region), ncol(country_region), unique_pids_region))
