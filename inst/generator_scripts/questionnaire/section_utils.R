resolve_generator_root <- function() {
  cwd <- normalizePath(getwd(), mustWork = TRUE)

  if (file.exists(file.path(cwd, "generate_questionnaire_data.R"))) {
    return(cwd)
  }

  parent <- normalizePath(file.path(cwd, ".."), mustWork = TRUE)
  if (file.exists(file.path(parent, "generate_questionnaire_data.R"))) {
    return(parent)
  }

  stop("Could not locate generator_scripts directory from current working directory.")
}

ensure_columns <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA_character_
    }
  }
  df
}

get_questionnaire_data <- function() {
  generator_root <- resolve_generator_root()
  data_dir <- normalizePath(file.path(generator_root, "..", "data"), mustWork = FALSE)
  data_path <- file.path(data_dir, "questionnaire_data.csv")

  if (file.exists(data_path)) {
    qd <- read.csv(data_path, stringsAsFactors = FALSE)
  } else {
    bootstrap_path <- file.path(generator_root, "questionnaire", "bootstrap_source_data.R")
    if (!file.exists(bootstrap_path)) {
      stop("questionnaire_data.csv not found and bootstrap_source_data.R does not exist at questionnaire/bootstrap_source_data.R")
    }
    source(bootstrap_path, local = TRUE)
    if (!exists("questionnaire_data")) {
      stop("bootstrap_source_data.R did not create questionnaire_data")
    }
    qd <- questionnaire_data
  }

  names(qd) <- tolower(names(qd))
  qd
}

get_output_dir <- function() {
  generator_root <- resolve_generator_root()
  out_dir <- file.path(generator_root, "questionnaire", "outputs")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir
}

write_section <- function(df, section_name) {
  normalize_mojibake_text <- function(x) {
    if (!is.character(x)) return(x)
    y <- x
    y <- gsub("‚Äôs", "'s", y, fixed = TRUE)
    y <- gsub("‚Äô", "'", y, fixed = TRUE)
    y <- gsub("â€™", "'", y, fixed = TRUE)
    y <- gsub("â€˜", "'", y, fixed = TRUE)
    y <- gsub("â€œ", '"', y, fixed = TRUE)
    y <- gsub("â€\u009d", '"', y, fixed = TRUE)
    y <- gsub("â€“", "-", y, fixed = TRUE)
    y <- gsub("â€”", "-", y, fixed = TRUE)
    y <- gsub("¬£", "£", y, fixed = TRUE)
    y <- gsub("Â£", "£", y, fixed = TRUE)
    y
  }

  char_cols <- names(df)[vapply(df, is.character, logical(1))]
  for (cn in char_cols) df[[cn]] <- normalize_mojibake_text(df[[cn]])
  if ("housing_income_1_1" %in% names(df)) {
    x <- df$housing_income_1_1
    # Strip any mojibake prefix that may appear before the pound sign
    x <- gsub("[\u00c2\u00ac]+\u00a3", "\u00a3", x)   # Â£, ¬£, Â¬£ etc.
    x <- gsub("GBP\\s*", "\u00a3", x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)
    df$housing_income_1_1 <- x
  }

  out_path <- file.path(get_output_dir(), paste0(section_name, ".csv"))
  write_csv_utf8bom(df, out_path)
  cat(sprintf("\nGenerated %s\n", basename(out_path)))
  cat(sprintf("%d rows and %d columns\n", nrow(df), ncol(df)))
}

write_csv_utf8bom <- function(df, path) {
  # Write a UTF-8 BOM so Excel detects the encoding correctly,
  # then append the CSV content in UTF-8.
  con <- file(path, open = "wb")
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  close(con)
  suppressWarnings(write.table(df, file = path, append = TRUE, sep = ",",
              row.names = FALSE, col.names = TRUE,
              qmethod = "double", na = "NA",
              fileEncoding = "UTF-8"))
}

fill_if_all_na <- function(df, col, values) {
  if (!col %in% names(df)) return(df)
  cur <- df[[col]]
  if (all(is.na(cur) | cur == "")) df[[col]] <- as.character(values)
  df
}

fill_defaults_for_columns <- function(df, cols) {
  n <- nrow(df)
  if (n == 0) return(df)

  if ("demog_height_enter_unit_1_1" %in% cols) {
    df <- fill_if_all_na(df, "demog_height_enter_unit_1_1", sample(c("Feet/inches", "Centimetres", "Do not know my height", "Prefer not to provide my height"), n, replace = TRUE, prob = c(0.26, 0.64, 0.06, 0.04)))
  }
  if ("demog_weight_enter_unit_1_1" %in% cols) {
    df <- fill_if_all_na(df, "demog_weight_enter_unit_1_1", sample(c("Stones/pounds", "Kilograms", "Do not know my weight", "I do not want to report my weight"), n, replace = TRUE, prob = c(0.34, 0.56, 0.06, 0.04)))
  }
  if ("demog_language_1_1" %in% cols) {
    df <- fill_if_all_na(df, "demog_language_1_1", sample(c("English", "Welsh", "(Scottish) Gaelic", "Punjabi", "Gujarati", "Bengali", "Urdu", "Hindi", "Cantonese", "Mandarin", "Polish", "Arabic", "Other (Including British Sign Language)", "Prefer not to answer"), n, replace = TRUE, prob = c(0.88, 0.01, 0.002, 0.01, 0.008, 0.01, 0.01, 0.01, 0.005, 0.005, 0.01, 0.01, 0.025, 0.02)))
  }
  if ("housing_type_1_1" %in% cols) {
    df <- fill_if_all_na(df, "housing_type_1_1", sample(c("A house or bungalow", "A flat, maisonette or apartment", "Mobile or temporary structure (i.e. caravan)", "Sheltered accommodation", "Care home", "None of the above", "Prefer not to answer"), n, replace = TRUE, prob = c(0.72, 0.22, 0.01, 0.015, 0.005, 0.01, 0.02)))
  }
  if ("housing_energy_1_m" %in% cols) {
    df <- fill_if_all_na(df, "housing_energy_1_m", sample(c("Gas", "Electricity", "Solid fuel (e.g., coal, wood)", "Oil", "Other", "None of the above", "Prefer not to answer"), n, replace = TRUE, prob = c(0.63, 0.26, 0.02, 0.03, 0.02, 0.02, 0.02)))
  }
  if ("housing_heat_1_m" %in% cols) {
    df <- fill_if_all_na(df, "housing_heat_1_m", sample(c("Central heating", "Storage heaters", "Gas/electric fires", "Open fire", "Other heating system", "None of the above", "Do not know", "Prefer not to answer"), n, replace = TRUE, prob = c(0.82, 0.05, 0.03, 0.02, 0.02, 0.01, 0.03, 0.02)))
  }
  if ("housing_income_1_1" %in% cols) {
    df <- fill_if_all_na(df, "housing_income_1_1", sample(c("Less than \u00a318,000", "\u00a318,000 to \u00a330,999", "\u00a331,000 to \u00a351,999", "\u00a352,000 to \u00a3100,000", "Greater than \u00a3100,000", "Do not know", "Prefer not to answer"), n, replace = TRUE, prob = c(0.16, 0.21, 0.25, 0.24, 0.09, 0.03, 0.02)))
  }
  if ("housing_tenure_1_1" %in% cols) {
    df <- fill_if_all_na(df, "housing_tenure_1_1", sample(c("Own outright (by you or someone in your household)", "Own with a mortgage", "Rent - from local authority, local council, housing association, student", "Rent - from private landlord or letting agency", "Pay part rent and part mortgage (shared ownership)", "Live in accommodation rent free", "None of the above", "Prefer not to answer"), n, replace = TRUE, prob = c(0.31, 0.31, 0.10, 0.16, 0.02, 0.04, 0.03, 0.03)))
  }
  if ("activity_walk_pace_1_1" %in% cols) {
    df <- fill_if_all_na(df, "activity_walk_pace_1_1", sample(c("Slow pace", "Steady average pace", "Brisk pace", "Fast pace", "Do not know", "Prefer not to answer"), n, replace = TRUE, prob = c(0.12, 0.48, 0.25, 0.08, 0.03, 0.04)))
  }
  if ("smoke_status_1_1" %in% cols) {
    df <- fill_if_all_na(df, "smoke_status_1_1", sample(c("Yes, every day", "Yes, some days", "Yes, but rarely", "No, not at all", "Prefer not to answer"), n, replace = TRUE, prob = c(0.12, 0.08, 0.08, 0.68, 0.04)))
  }
  if ("smoke_status_2_1" %in% cols) {
    df <- fill_if_all_na(df, "smoke_status_2_1", sample(c("Yes, every day", "Yes, some days", "Yes, but rarely", "No, not at all", "Prefer not to answer"), n, replace = TRUE, prob = c(0.12, 0.08, 0.08, 0.68, 0.04)))
  }
  if ("alcohol_curr_1_1" %in% cols) {
    df <- fill_if_all_na(df, "alcohol_curr_1_1", sample(c("Daily or almost daily", "Three or four times a week", "Once or twice a week", "One to three times a month", "Special occasions only", "Never", "Prefer not to answer"), n, replace = TRUE, prob = c(0.08, 0.14, 0.24, 0.20, 0.18, 0.12, 0.04)))
  }
  if ("health_status_chronic_1_1" %in% cols) {
    df <- fill_if_all_na(df, "health_status_chronic_1_1", sample(c("Yes", "No", "Do not know", "Prefer not to answer"), n, replace = TRUE, prob = c(0.40, 0.52, 0.05, 0.03)))
  }
  if ("health_covid_1_1" %in% cols) {
    df <- fill_if_all_na(df, "health_covid_1_1", sample(c("Yes, confirmed by a positive test", "Yes, suspected by a doctor but not tested", "Yes, my own suspicions", "No", "Do not know", "Prefer not to answer"), n, replace = TRUE, prob = c(0.62, 0.05, 0.08, 0.20, 0.03, 0.02)))
  }

  df
}

sanitize_pdf_values <- function(values) {
  if (length(values) == 0) return(character(0))
  x <- as.character(values)
  x <- gsub("‚Äôs", "'s", x, fixed = TRUE)
  x <- gsub("‚Äô", "'", x, fixed = TRUE)
  x <- gsub("â€™", "'", x, fixed = TRUE)
  x <- gsub("â€˜", "'", x, fixed = TRUE)
  x <- gsub("â€œ", '"', x, fixed = TRUE)
  x <- gsub("â€\u009d", '"', x, fixed = TRUE)
  x <- gsub("¬£", "£", x, fixed = TRUE)
  x <- gsub("Â£", "£", x, fixed = TRUE)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x <- x[nzchar(x)]
  x <- gsub("\\s*OR\\s*$", "", x, ignore.case = TRUE)
  x <- trimws(x)
  x <- x[nzchar(x)]
  # Drop parser artifacts and long instruction/question fragments.
  bad <- grepl("^(TOGGLE|SELECT)\\b", x, ignore.case = TRUE) |
    grepl("\\?", x) |
    grepl("\\b(Enter INTEGER|Enter FLOAT|GO TO|Answer this question)\\b", x, ignore.case = TRUE)
  x <- x[!bad]
  unique(x)
}

split_pdf_values <- function(s) {
  if (is.null(s) || is.na(s) || s == "") return(character(0))
  vals <- strsplit(as.character(s), "\\|\\|", fixed = FALSE)[[1]]
  sanitize_pdf_values(vals)
}

get_pdf_value_catalog <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)

    generator_root <- resolve_generator_root()
    repo_root <- normalizePath(file.path(generator_root, "..", ".."), mustWork = FALSE)
    csv_candidates <- c(
      file.path(generator_root, "questionnaire", "questionnaire_column_unique_values_from_pdfs.csv"),
      file.path(repo_root, "questionnaire_analysis", "questionnaire_column_unique_values_from_pdfs.csv"),
      file.path(repo_root, "inst", "generator_scripts", "questionnaire", "questionnaire_column_unique_values_from_pdfs.csv")
    )
    csv_path <- csv_candidates[file.exists(csv_candidates)][1]
    if (is.na(csv_path) || !nzchar(csv_path)) {
      cache <<- data.frame()
      return(cache)
    }

    cat_df <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
    names(cat_df) <- tolower(names(cat_df))
    cat_df$column_name <- tolower(cat_df$column_name)
    cache <<- cat_df
    cache
  }
})

get_pdf_column_values <- function(col_name) {
  cat_df <- get_pdf_value_catalog()
  if (!nrow(cat_df)) return(NULL)

  row <- cat_df[cat_df$column_name == tolower(col_name), , drop = FALSE]
  if (!nrow(row)) return(NULL)

  list(
    question_type = tolower(as.character(row$question_type[1])),
    versions = strsplit(as.character(row$versions[1]), ";", fixed = TRUE)[[1]],
    all = split_pdf_values(row$possible_values_all[1]),
    v1 = split_pdf_values(row$possible_values_v1[1]),
    v2 = split_pdf_values(row$possible_values_v2[1])
  )
}

# Answer values that must always appear alone in a multi-select cell (never
# combined with other choices via "|").
.EXCLUSIVE_ANSWERS <- c(
  "None of the above",
  "Prefer not to answer",
  "Do not know",
  "No",
  "Only select a response if you currently use gas"
)

# Generate n multi-select responses from a pool of allowed values.
# - Exclusive values (e.g. "None of the above") are never combined with others.
# - Probabilities: ~60 % single answer, ~30 % two answers, ~10 % three answers.
# - edu_qual columns use a qualification-hierarchy heuristic so that
#   degree-holders are also assigned A-levels, and A-level holders are also
#   assigned O-levels/GCSEs, at realistic rates.
sample_multi_select_responses <- function(vals, n, col_name) {
  if (n <= 0 || length(vals) == 0) return(character(n))

  exclusive   <- vals[vals %in% .EXCLUSIVE_ANSWERS]
  combinable  <- vals[!vals %in% .EXCLUSIVE_ANSWERS]

  # ---- EDU_QUAL: hierarchical credential logic ----
  if (grepl("^edu_qual", col_name, ignore.case = TRUE) && length(combinable) >= 2) {
    degree_v  <- grep("College or University degree", combinable, value = TRUE, fixed = TRUE)
    a_level_v <- grep("A levels/AS levels",           combinable, value = TRUE)
    o_level_v <- grep("O levels/GCSEs or equivalent", combinable, value = TRUE, fixed = TRUE)

    return(vapply(seq_len(n), function(i) {
      base <- sample(vals, 1)
      if (base %in% exclusive) return(base)
      chosen <- base
      if (length(degree_v) > 0 && base %in% degree_v) {
        # Degree holder: ~85 % chance they also report A-levels
        if (length(a_level_v) > 0 && runif(1) < 0.85) {
          chosen <- c(chosen, sample(a_level_v, 1))
          # A-levels added: ~70 % chance O-levels/GCSEs too
          if (length(o_level_v) > 0 && runif(1) < 0.70)
            chosen <- c(chosen, sample(o_level_v, 1))
        }
      } else if (length(a_level_v) > 0 && base %in% a_level_v) {
        # A-levels only: ~75 % chance they also report O-levels/GCSEs
        if (length(o_level_v) > 0 && runif(1) < 0.75)
          chosen <- c(chosen, sample(o_level_v, 1))
      }
      paste(chosen, collapse = "|")
    }, character(1)))
  }

  # ---- General multi-select ----
  if (length(combinable) == 0) return(sample(vals, n, replace = TRUE))

  vapply(seq_len(n), function(i) {
    base <- sample(vals, 1)
    # Exclusive answers are always returned on their own.
    if (base %in% exclusive || length(combinable) < 2) return(base)
    # Decide how many answers to combine.
    p <- runif(1)
    k <- if (p < 0.60) 1L else if (p < 0.90) 2L else 3L
    if (k == 1L) return(base)
    pool    <- setdiff(combinable, base)
    extra_k <- min(k - 1L, length(pool))
    if (extra_k == 0L) return(base)
    extra <- sample(pool, extra_k, replace = FALSE)
    paste(c(base, extra), collapse = "|")
  }, character(1))
}

apply_pdf_value_catalog <- function(section_df, questionnaire_data, cols, force_replace_numeric = TRUE) {
  n <- nrow(section_df)
  if (n == 0) return(section_df)

  if ("questionnaire_version" %in% names(questionnaire_data)) {
    idx_v1 <- which(questionnaire_data$questionnaire_version == 1)
    idx_v2 <- which(questionnaire_data$questionnaire_version == 2)
  } else {
    idx_v1 <- sample(seq_len(n), size = max(1L, round(0.015 * n)))
    idx_v2 <- setdiff(seq_len(n), idx_v1)
  }

  is_numeric_only <- function(v) {
    non_empty <- as.character(v[!is.na(v) & v != ""])
    if (!length(non_empty)) return(FALSE)
    all(grepl("^[-+]?[0-9]+(\\.[0-9]+)?$", trimws(non_empty)))
  }

  gen_numeric_values <- function(col, m) {
    if (m <= 0) return(character(0))

    if (grepl("immigrate_uk_yr", col, fixed = TRUE)) {
      return(as.character(sample(1950:2024, m, replace = TRUE)))
    }
    if (grepl("height", col, fixed = TRUE)) {
      vals <- round(pmin(230, pmax(100, rnorm(m, mean = 169, sd = 10))), 1)
      return(sprintf("%.1f", vals))
    }
    if (grepl("weight", col, fixed = TRUE)) {
      vals <- round(pmin(260, pmax(30, rnorm(m, mean = 78, sd = 18))), 1)
      return(sprintf("%.1f", vals))
    }
    if (grepl("_hrs_", col, fixed = TRUE) || grepl("_mins_", col, fixed = TRUE)) {
      max_v <- if (grepl("_hrs_", col, fixed = TRUE)) 24 else 300
      vals <- round(runif(m, min = 0, max = max_v), 1)
      return(sprintf("%.1f", vals))
    }
    if (grepl("_age_", col, fixed = TRUE)) {
      return(as.character(sample(10:100, m, replace = TRUE)))
    }
    if (grepl("_yrs_", col, fixed = TRUE)) {
      return(as.character(sample(0:80, m, replace = TRUE)))
    }
    if (grepl("_num_", col, fixed = TRUE)) {
      return(as.character(sample(0:10, m, replace = TRUE)))
    }
    if (grepl("_days_", col, fixed = TRUE)) {
      return(as.character(sample(0:365, m, replace = TRUE)))
    }

    # Default integer fallback.
    as.character(sample(0:100, m, replace = TRUE))
  }

  for (col in cols) {
    if (!col %in% names(section_df) || col %in% c("pid", "id", "submission_date", "questionnaire_version")) next

    info <- get_pdf_column_values(col)
    if (is.null(info)) next

    cur <- section_df[[col]]
    should_replace <- all(is.na(cur) | cur == "") || (force_replace_numeric && is_numeric_only(cur))
    if (!should_replace) next

    vals_all <- sanitize_pdf_values(info$all)
    vals_v1 <- sanitize_pdf_values(info$v1)
    vals_v2 <- sanitize_pdf_values(info$v2)
    has_v1 <- "v1" %in% tolower(info$versions)
    has_v2 <- "v2" %in% tolower(info$versions)

    if (identical(info$question_type, "numeric")) {
      out <- ifelse(is.na(cur), NA_character_, as.character(cur))

      if (!has_v1 && has_v2 && length(idx_v1) > 0) out[idx_v1] <- NA_character_
      if (has_v1 && !has_v2 && length(idx_v2) > 0) out[idx_v2] <- NA_character_

      if (length(idx_v1) > 0 && has_v1) out[idx_v1] <- gen_numeric_values(col, length(idx_v1))
      if (length(idx_v2) > 0 && has_v2) out[idx_v2] <- gen_numeric_values(col, length(idx_v2))
      if (!has_v1 && !has_v2) out[] <- gen_numeric_values(col, n)

      section_df[[col]] <- out
      next
    }

    if (length(vals_all) == 0 && length(vals_v1) == 0 && length(vals_v2) == 0) next

    out <- ifelse(is.na(cur), NA_character_, as.character(cur))

    # For multi-select questions use the multi-select sampler; for single-choice
    # questions use plain sample().
    is_multi <- identical(info$question_type, "multi_select")
    draw <- function(v, m) {
      if (is_multi) sample_multi_select_responses(v, m, col)
      else          sample(v, m, replace = TRUE)
    }

    # Version-only questions must be NA outside the applicable version.
    if (!has_v1 && has_v2 && length(idx_v1) > 0) {
      out[idx_v1] <- NA_character_
    }
    if (has_v1 && !has_v2 && length(idx_v2) > 0) {
      out[idx_v2] <- NA_character_
    }

    if (length(idx_v1) > 0) {
      if (!has_v1) {
        # v2-only column
      } else if (length(vals_v1) > 0) {
        out[idx_v1] <- draw(vals_v1, length(idx_v1))
      } else if (length(vals_all) > 0) {
        out[idx_v1] <- draw(vals_all, length(idx_v1))
      }
    }

    if (length(idx_v2) > 0) {
      if (!has_v2) {
        # v1-only column
      } else if (length(vals_v2) > 0) {
        out[idx_v2] <- draw(vals_v2, length(idx_v2))
      } else if (length(vals_all) > 0) {
        out[idx_v2] <- draw(vals_all, length(idx_v2))
      }
    }

    section_df[[col]] <- out
  }

  section_df
}
