# Questionnaire Modular Generator

This folder contains the modular questionnaire generation pipeline used by:

- ../03_generate_questionnaire_all.R

The goal is to keep questionnaire generation split by questionnaire section while still producing one final `questionnaire_data.csv` in `../data/`.

## High-level flow

1. `../03_generate_questionnaire_all.R` is the entrypoint used by `../00_run_all_generators.R`.
2. It sources `merge_sections.R`.
3. `merge_sections.R` runs each section script in this folder.
4. Each section script writes one CSV in `questionnaire/outputs/`.
5. `merge_sections.R` merges section outputs on `pid` and writes `../data/questionnaire_data.csv`.

If `../data/questionnaire_data.csv` does not exist (first run), the pipeline bootstraps from `bootstrap_source_data.R` and still completes end-to-end.

## Script responsibilities

- `questionnaire.R`: Base questionnaire identifiers/metadata.
	- Owns `id`, `pid`, `questionnaire_version`, `submission_date`.

- `you_and_your_household.R`: Section-specific household and demographic columns.

- `work_and_education.R`: Work and education columns.

- `your_lifestyle.R`: Activity, smoking, alcohol, sleep, social, and lifestyle columns.

- `your_health.R`: Health, diagnoses, medications, reproductive, screening, PHQ9/GAD7, pain, and related columns.

- `family_health.R`: Family history columns (parents/siblings), family diagnosis detail columns, and related family demographics.

- `merge_sections.R`: Orchestrator + merge logic.
	- Runs all section scripts.
	- Reads `questionnaire/outputs/*.csv`.
	- Merges by `pid`.
	- Writes `../data/questionnaire_data.csv`.

- `section_utils.R`: Shared helper functions used by all section scripts and merge.

## What section_utils.R provides

`section_utils.R` provides these reusable helpers:

- `resolve_legacy_dir()`
	- Finds the `legacy-scripts-baseline` directory from current working directory.
	- Supports running scripts from either `legacy-scripts-baseline` or `questionnaire`.

- `ensure_columns(df, cols)`
	- Ensures every column listed in `cols` exists.
	- Missing columns are added as `NA_character_`.

- `get_questionnaire_data()`
	- Primary source: reads existing `../data/questionnaire_data.csv`.
	- Fallback source: runs `questionnaire/bootstrap_source_data.R` if no questionnaire CSV exists.
	- Normalizes all column names to lowercase.

- `bootstrap_source_data.R`
	- Internal bootstrap generator used only when no existing `questionnaire_data.csv` is present.
	- Allows first-time modular runs with no pre-existing questionnaire CSV.
	- Creates a base questionnaire frame (`pid`, `questionnaire_version`, `submission_date`) that section scripts expand into full section outputs.

- `get_output_dir()`
	- Returns `questionnaire/outputs` and creates it if needed.

- `write_section(df, section_name)`
	- Writes one section CSV to `questionnaire/outputs/<section_name>.csv`.
	- Prints generated row/column counts.

## Running

### Run questionnaire only

From `legacy-scripts-baseline`:

```sh
Rscript 03_generate_questionnaire_all.R
```

### Run entire dataset pipeline

From `legacy-scripts-baseline`:

```sh
Rscript 00_run_all_generators.R
```

## How to update columns in future

Use this checklist whenever columns are added/removed/renamed.

1. Decide the owning section file.
	 - Each column should be owned by exactly one section script.

2. Update that section script's `cols` vector.
	 - Add new lowercase column names.
	 - Remove deprecated names.

3. Port generation logic from old monolith if needed.
	 - If the section should generate new values, add logic in the owning section script.
	 - Keep data types/routing consistent with questionnaire version rules.

4. Keep naming lowercase.
	 - All section output headers are lowercase.

5. Run and validate.
	 - `Rscript 03_generate_questionnaire_all.R`
	 - Check output row count and expected column presence.

6. If multiple sections need shared helpers, add them to `section_utils.R`.

## Important maintenance rules

- Do not re-introduce logic into `../03_generate_questionnaire_all.R`.
	- It should stay a thin wrapper that sources `merge_sections.R`.

- Keep section boundaries clean.
	- Put generation logic where the column ownership lives.

- Merge key is `pid`.
	- Section outputs must include `pid`.

- Section outputs are intermediate artifacts.
	- Final production file is `../data/questionnaire_data.csv`.

## Troubleshooting

- Missing column in final CSV:
	- Confirm column is listed in exactly one section `cols` vector.
	- Confirm that section script ran successfully.

- Merge failure on key:
	- Ensure all section outputs include `pid`.

- Empty or stale values:
	- Re-check section generation logic and routing conditions.
	- Re-run `Rscript 03_generate_questionnaire_all.R`.

