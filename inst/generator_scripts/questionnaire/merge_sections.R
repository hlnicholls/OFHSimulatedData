source("questionnaire/section_utils.R", local = TRUE)

generator_root <- resolve_generator_root()
questionnaire_dir <- file.path(generator_root, "questionnaire")
outputs_dir <- get_output_dir()

required_sections <- c(
	"questionnaire",
	"you_and_your_household",
	"work_and_education",
	"your_lifestyle",
	"your_health",
	"family_health"
)

section_scripts <- file.path(questionnaire_dir, paste0(required_sections, ".R"))
for (script_path in section_scripts) {
	source(script_path, local = TRUE)
}

section_paths <- file.path(outputs_dir, paste0(required_sections, ".csv"))
for (p in section_paths) {
	if (!file.exists(p)) stop(sprintf("Missing section output: %s", p))
}

section_dfs <- lapply(section_paths, function(p) {
	df <- read.csv(p, stringsAsFactors = FALSE)
	names(df) <- tolower(names(df))
	df
})

merged <- section_dfs[[1]]
for (i in 2:length(section_dfs)) {
	merged <- merge(merged, section_dfs[[i]], by = "pid", all = TRUE, sort = FALSE)
}

if ("id" %in% names(merged)) {
	merged <- merged[order(merged$id), , drop = FALSE]
} else {
	merged <- merged[order(merged$pid), , drop = FALSE]
}

drop_cols <- c("id")
keep_cols <- setdiff(names(merged), drop_cols)
final_df <- merged[, keep_cols, drop = FALSE]

# Align output schema to the monolithic questionnaire generator when possible.
reference_df <- tryCatch({
	reference_env <- new.env(parent = baseenv())
	reference_script <- file.path(generator_root, "questionnaire", "bootstrap_source_data.R")
	if (!file.exists(reference_script)) return(NULL)
	sys.source(reference_script, envir = reference_env)
	if (!exists("questionnaire_data", envir = reference_env)) return(NULL)
	ref <- get("questionnaire_data", envir = reference_env)
	names(ref) <- tolower(names(ref))
	ref
}, error = function(e) {
	NULL
})

if (!is.null(reference_df)) {
	reference_cols <- names(reference_df)
	if (length(reference_cols) >= ncol(final_df)) {
		for (col in setdiff(reference_cols, names(final_df))) {
			final_df[[col]] <- reference_df[[col]]
		}
		final_df <- final_df[, reference_cols, drop = FALSE]
	}
}

data_dir <- normalizePath(file.path(generator_root, "..", "data"), mustWork = FALSE)
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

final_path <- file.path(data_dir, "questionnaire_data.csv")
write_csv_utf8bom(final_df, final_path)

message("Generated questionnaire_data.csv")
message(sprintf("%d rows and %d columns", nrow(final_df), ncol(final_df)))
