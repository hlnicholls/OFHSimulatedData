# Bootstrap questionnaire source data for first-run modular execution.

source("generate_pids.R", local = TRUE)

set.seed(42)

config <- getOption("OFH_GEN_CONFIG", NULL)
total_pid_count <- if (!is.null(config)) config$total_pid_count else 5000
all_study_pids <- getOption("OFH_ALL_STUDY_PIDS", generate_pids(total_pid_count))

v1_percentage <- 0.015
v2_percentage <- 1 - v1_percentage

questionnaire_version <- sample(
  c(1, 2),
  total_pid_count,
  replace = TRUE,
  prob = c(v1_percentage, v2_percentage)
)

submission_date <- sample(
  seq(as.Date("2022-12-01"), as.Date("2024-12-01"), by = "day"),
  total_pid_count,
  replace = TRUE
)

questionnaire_data <- data.frame(
  pid = all_study_pids,
  questionnaire_version = questionnaire_version,
  submission_date = submission_date,
  stringsAsFactors = FALSE
)
