if (file.exists("questionnaire/section_utils.R")) {
	source("questionnaire/section_utils.R", local = TRUE)
} else {
	source("section_utils.R", local = TRUE)
}

questionnaire_data <- get_questionnaire_data()

if (!"id" %in% names(questionnaire_data)) {
	questionnaire_data$id <- seq_len(nrow(questionnaire_data))
}

cols <- c("id", "pid", "questionnaire_version", "submission_date")
questionnaire_data <- ensure_columns(questionnaire_data, cols)
section_df <- questionnaire_data[, cols, drop = FALSE]
section_df <- apply_pdf_value_catalog(section_df, questionnaire_data, cols)

write_section(section_df, "questionnaire")