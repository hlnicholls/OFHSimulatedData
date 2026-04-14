# Shared helpers for adding NHS columns from the NHS columns spec rules.

resolve_data_dictionary_path <- function(candidates) {
  for (nm in candidates) {
    p <- file.path("..", "data_dictionaries", nm)
    if (file.exists(p)) return(p)
  }
  stop(sprintf(
    "Could not locate required data dictionary file. Tried: %s",
    paste(file.path("..", "data_dictionaries", candidates), collapse = ", ")
  ))
}

# Support both historical and current filenames.
NHS_NEW_COL_SPEC_PATH <- resolve_data_dictionary_path(c(
  "nhse_columns_spec.csv",
  "nhse_new_columns_spec.csv",
  "OFH_new_columns.csv"
))
MAINSPEF_VALUES_PATH <- resolve_data_dictionary_path(c(
  "nhse_mainspef_possible_values.csv",
  "mainspef_possible_values.csv"
))
TRETSPEF_VALUES_PATH <- resolve_data_dictionary_path(c(
  "nhse_tretspef_possible_values.csv",
  "tretspef_possible_values.csv"
))

normalize_named_weights <- function(keys, custom_weights = NULL) {
  probs <- rep(1, length(keys))
  names(probs) <- keys

  if (!is.null(custom_weights) && length(custom_weights) > 0) {
    if (is.null(names(custom_weights)) || any(names(custom_weights) == "")) {
      stop("Custom code weights must be named with code values")
    }
    unknown_weights <- setdiff(names(custom_weights), keys)
    if (length(unknown_weights) > 0) {
      stop(sprintf("Unknown weighted codes: %s", paste(unknown_weights, collapse = ", ")))
    }
    for (nm in names(custom_weights)) {
      probs[nm] <- as.numeric(custom_weights[[nm]])
    }
  }

  if (any(!is.finite(probs) | probs <= 0)) {
    stop("All code weights must be finite and > 0")
  }

  probs / sum(probs)
}

sample_code_with_description <- function(code_descriptions, code_weights = NULL) {
  keys <- names(code_descriptions)
  probs <- normalize_named_weights(keys, code_weights)
  code <- sample(keys, 1, prob = probs)
  paste0(code, " = ", unname(code_descriptions[[code]]))
}

sample_value_from_pool <- function(values, value_weights = NULL) {
  probs <- normalize_named_weights(values, value_weights)
  sample(values, 1, prob = probs)
}

default_nhse_code_config <- function(dataset_key) {
  defaults <- list(
    nhse_outpat_data = list(
      icd10_descriptions = c(
        I210 = "STEMI of anterolateral wall",
        I214 = "Acute subendocardial myocardial infarction",
        I219 = "Heart attack of unspecified site",
        I250 = "Atherosclerosis of coronary artery",
        I251 = "Atherosclerotic heart disease of native coronary artery",
        I500 = "Congestive heart failure",
        I501 = "Left ventricular failure",
        I509 = "Heart failure, unspecified",
        I420 = "Dilated cardiomyopathy",
        I350 = "Aortic stenosis",
        E110 = "Type 2 diabetes with hyperosmolarity",
        E780 = "Pure hypercholesterolaemia",
        J449 = "COPD, unspecified",
        E660 = "Obesity due to excess calories",
        R69X = "Unknown causes of morbidity"
      ),
      icd10_weights = c(),
      opcs4_descriptions = c(
        K401 = "Percutaneous transluminal balloon angioplasty of coronary artery",
        K451 = "Insertion of drug-eluting stent into coronary artery",
        K561 = "Repair of heart valve",
        M011 = "Kidney transplant",
        E033 = "Endoscopic biopsy of oesophagus",
        L436 = "Diagnostic endoscopy of large intestine"
      ),
      opcs4_weights = c(),
      diag_4_02_missing_prob = 0.85,
      diag_4_03_missing_prob = 0.98,
      opertn_01_no_procedure_prob = 0.92,
      opertn_02_missing_prob = 0.97
    ),
    nhse_inpat_data = list(
      icd10_descriptions = c(
        I21 = "Heart attack",
        I210 = "STEMI of anterolateral wall",
        I214 = "Acute subendocardial myocardial infarction",
        I251 = "Atherosclerotic heart disease of native coronary artery",
        I500 = "Congestive heart failure",
        I639 = "Cerebral infarction, unspecified",
        I480 = "Paroxysmal atrial fibrillation",
        I350 = "Aortic stenosis",
        I822 = "Thrombophlebitis of deep vessels of lower extremities",
        I349 = "Viral infection, unspecified",
        E110 = "Type 2 diabetes with hyperosmolarity",
        E789 = "High cholesterol, unspecified",
        N180 = "Stage 3a chronic kidney disease",
        J440 = "Chronic obstructive pulmonary disease with acute lower respiratory infection",
        E660 = "Obesity due to excess calories"
      ),
      icd10_weights = c(),
      opcs4_descriptions = c(
        K401 = "Percutaneous transluminal balloon angioplasty of coronary artery",
        K451 = "Insertion of drug-eluting stent into coronary artery",
        K561 = "Repair of heart valve",
        K562 = "Replacement of heart valve",
        K011 = "Heart transplant",
        M011 = "Kidney transplant",
        E033 = "Endoscopic biopsy of oesophagus"
      ),
      opcs4_weights = c(),
      single_diag_prob = 0.70,
      opertn_01_missing_prob = 0.90,
      opertn_02_missing_prob = 0.97
    ),
    nhse_ed_data = list(
      ae_specific_codes = c(
        "01 = Brain", "02 = Head", "03 = Face", "04 = Eye", "05 = Ear",
        "06 = Nose", "07 = Mouth,Jaw,Teeth", "08 = Throat", "09 = Neck",
        "10 = Shoulder", "11 = Axilla", "12 = Upper Arm", "13 = Elbow",
        "14 = Forearm", "15 = Wrist", "16 = Hand", "17 = Digit",
        "18 = Cervical spine", "19 = Thoracic", "20 = Lumbosacral spine",
        "21 = Pelvis", "22 = Chest", "23 = Breast", "24 = Abdomen",
        "25 = Back/buttocks", "26 = Ano/rectal", "27 = Genitalia",
        "28 = Hip", "29 = Groin", "30 = Thigh", "31 = Knee",
        "32 = Lower leg", "33 = Ankle", "34 = Foot", "35 = Toe", "36 = Multiple site"
      ),
      ae_specific_weights = c(),
      icd10_pool = c("I210", "I509", "I48", "J440", "E119", "R10", "R07"),
      icd10_weights = c(),
      read_codes_pool = c("S01.5", "S011", "S02", "S03", "S10", "S20"),
      read_weights = c(),
      diagscheme_levels = c(
        "01 = Accident & Emergency diagnoses",
        "02 = ICD-10",
        "04 = Read coded clinical terms version 2",
        "Null"
      ),
      diagscheme_probs = c(0.55, 0.30, 0.10, 0.05),
      ae_scheme_icd10_prob = 0.25,
      diag_missing_probs = c(0, 0.80, 0.90, 0.95, 0.96, 0.97, 0.98, 0.98, 0.99, 0.99, 0.99, 0.99)
    )
  )

  defaults[[dataset_key]]
}

load_nhse_new_col_spec <- function() {
  spec_df <- read.csv(NHS_NEW_COL_SPEC_PATH, stringsAsFactors = FALSE, check.names = FALSE)

  # Support both historical header `column` and updated typo `coluymn`.
  if (!"column" %in% names(spec_df) && "coluymn" %in% names(spec_df)) {
    names(spec_df)[names(spec_df) == "coluymn"] <- "column"
  }

  if (!all(c("file", "column", "values") %in% names(spec_df))) {
    stop("NHS column spec must contain headers: file, column (or coluymn), values")
  }

  spec_df$file <- trimws(spec_df$file)
  spec_df$column <- trimws(spec_df$column)
  spec_df$values <- ifelse(is.na(spec_df$values), "", spec_df$values)
  spec_df
}

spec_value_text <- function(spec_df, script_name, column_name) {
  idx <- which(spec_df$file == script_name & spec_df$column == column_name)
  if (length(idx) == 0) return(NA_character_)
  spec_df$values[idx[1]]
}

extract_quoted_options <- function(value_text, star_weight = 30) {
  if (is.na(value_text) || !nzchar(value_text)) {
    return(list(values = character(0), probs = numeric(0)))
  }

  # Handle broken quoting in imd04_decile rules.
  if (grepl("imd04_decile|Least deprived", value_text, ignore.case = TRUE)) {
    values <- c(
      "Least deprived 10%",
      "Less deprived 10-20%",
      "LESS DEPRIVED 10-20%",
      "More deprived 30-40%",
      "Most deprived 40-50%"
    )
    probs <- c(0.20, 0.22, 0.06, 0.27, 0.25)
    return(list(values = values, probs = probs))
  }

  matches <- gregexpr('"[^"]*"\\*?', value_text, perl = TRUE)[[1]]
  if (length(matches) == 1 && matches[1] == -1) {
    return(list(values = character(0), probs = numeric(0)))
  }

  tokens <- regmatches(value_text, gregexpr('"[^"]*"\\*?', value_text, perl = TRUE))[[1]]
  values <- trimws(gsub('^"|"\\*?$', '', tokens))
  is_star <- grepl('\\*$', tokens)
  weights <- ifelse(is_star, star_weight, 1)

  # Remove accidental empty entries.
  keep <- nzchar(values)
  values <- values[keep]
  weights <- weights[keep]

  if (length(values) == 0) {
    return(list(values = character(0), probs = numeric(0)))
  }

  probs <- weights / sum(weights)
  list(values = values, probs = probs)
}

sample_from_spec_options <- function(spec_df, script_name, column_name, n, star_weight = 30) {
  value_text <- spec_value_text(spec_df, script_name, column_name)
  parsed <- extract_quoted_options(value_text, star_weight = star_weight)
  if (length(parsed$values) == 0) {
    stop(sprintf("No quoted options found in spec for %s:%s", script_name, column_name))
  }
  sample(parsed$values, n, replace = TRUE, prob = parsed$probs)
}

random_digit_string <- function(n, min_digits, max_digits) {
  lens <- sample(min_digits:max_digits, n, replace = TRUE)
  vapply(
    lens,
    function(d) paste0(sample(0:9, d, replace = TRUE), collapse = ""),
    character(1)
  )
}

random_fixed_digits <- function(n, digits) {
  vapply(
    seq_len(n),
    function(i) paste0(sample(0:9, digits, replace = TRUE), collapse = ""),
    character(1)
  )
}

random_date_between <- function(n, start_date, end_date) {
  s <- as.Date(start_date)
  e <- as.Date(end_date)
  s + sample.int(as.integer(e - s) + 1, n, replace = TRUE) - 1
}

random_military_time <- function(n) {
  hh <- sample(0:23, n, replace = TRUE)
  mm <- sample(0:59, n, replace = TRUE)
  sprintf("%02d%02d", hh, mm)
}

load_possible_values <- function(path) {
  d <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  vals <- trimws(d[[1]])
  vals <- vals[nzchar(vals)]
  vals
}

sample_mainspef <- function(n) {
  vals <- load_possible_values(MAINSPEF_VALUES_PATH)
  weights <- rep(1, length(vals))
  weights[vals == "960"] <- 15
  sample(vals, n, replace = TRUE, prob = weights)
}

sample_tretspef <- function(n) {
  vals <- load_possible_values(TRETSPEF_VALUES_PATH)
  weights <- rep(1, length(vals))
  # Spec says this should be the most common value.
  weights[vals == "110 = Trauma and Orthopaedics Service"] <- 20
  sample(vals, n, replace = TRUE, prob = weights)
}

add_nhse_outpat_new_columns <- function(df) {
  script_name <- "06_generate_nhse_eng_outpat.R"
  spec_df <- load_nhse_new_col_spec()
  n <- nrow(df)
  if (n == 0) return(df)

  df$admincat <- sample_from_spec_options(spec_df, script_name, "admincat", n)
  df$attended <- sample_from_spec_options(spec_df, script_name, "attended", n)
  df$attendkey <- random_digit_string(n, 11, 19)
  df$firstatt <- sample_from_spec_options(spec_df, script_name, "firstatt", n)
  df$imd04_decile <- sample_from_spec_options(spec_df, script_name, "imd04_decile", n)
  df$mainspef <- sample_mainspef(n)
  df$outcome <- sample_from_spec_options(spec_df, script_name, "outcome", n)
  df$priority <- sample_from_spec_options(spec_df, script_name, "priority", n)
  df$procode3_ofh <- random_fixed_digits(n, 6)
  df$protype <- sample_from_spec_options(spec_df, script_name, "protype", n)
  df$refsourc <- sample_from_spec_options(spec_df, script_name, "refsourc", n)
  df$rttperend <- random_date_between(n, "2008-01-01", "2035-12-31")
  df$rttperstart <- random_date_between(n, "2008-01-01", "2035-12-31")
  df$rttperstat <- sample_from_spec_options(spec_df, script_name, "rttperstat", n)

  # Spec rule: no "Null" rows, and only 0.05% "-1" rows.
  df$susspellid <- random_digit_string(n, 13, 19)
  neg1_n <- max(1, round(0.0005 * n))
  neg1_idx <- sample.int(n, neg1_n)
  df$susspellid[neg1_idx] <- "-1 = Unspellable episodes"

  df$tretspef <- sample_tretspef(n)
  df
}

skewed_elecdur <- function(n) {
  base <- rlnorm(n, meanlog = log(45), sdlog = 0.9)
  vals <- pmin(10000, round(base))

  # Inject rare high outliers (>300)
  out_n <- max(1, round(0.01 * n))
  out_idx <- sample.int(n, out_n)
  vals[out_idx] <- sample(301:10000, out_n, replace = TRUE)
  vals
}

add_nhse_inpat_new_columns <- function(df) {
  script_name <- "07_generate_nhse_eng_inpat.R"
  spec_df <- load_nhse_new_col_spec()
  n <- nrow(df)
  if (n == 0) return(df)

  df$acscflag <- sample_from_spec_options(spec_df, script_name, "acscflag", n)
  df$admimeth <- sample_from_spec_options(spec_df, script_name, "admimeth", n)
  df$admisorc <- sample_from_spec_options(spec_df, script_name, "admisorc", n)
  df$aekey <- ifelse(runif(n) < 0.20, "Null", random_fixed_digits(n, 12))

  # Spec says "Null" has zero people for these columns, so keep all non-null values.
  df$bedyear <- sample(1993:2026, n, replace = TRUE)
  df$carersi <- sample_from_spec_options(spec_df, script_name, "carersi", n)
  df$classpat <- sample_from_spec_options(spec_df, script_name, "classpat", n)
  df$disdate <- random_date_between(n, "1993-01-01", "2026-12-31")
  df$disdest <- sample_from_spec_options(spec_df, script_name, "disdest", n)
  df$dismeth <- sample_from_spec_options(spec_df, script_name, "dismeth", n)

  df$disreadydate <- random_date_between(n, "2008-01-01", "2034-12-31")
  df$disreadydate[sample.int(n, 1)] <- as.Date("1800-01-01")

  df$elecdate <- random_date_between(n, "1997-01-01", "2029-12-31")
  df$elecdate[sample.int(n, 1)] <- as.Date("1900-01-01")

  df$elecdur <- skewed_elecdur(n)
  df$epikey <- random_digit_string(n, 11, 19)

  df$epiorder <- as.character(sample(1:15, n, replace = TRUE))
  if (n >= 5) {
    idx <- sample.int(n, 5)
    df$epiorder[idx] <- sample(c("98 = Not applicable", "99 = Not know: a validation error"), 5, replace = TRUE)
  }

  df$epiend <- random_date_between(n, "1997-01-01", "2026-12-31")

  # Enforce temporal ordering constraints requested for inpatient data.
  if ("admidate" %in% names(df)) {
    dis_numeric <- as.numeric(df$disdate)
    adm_numeric <- as.numeric(as.Date(df$admidate))
    df$disdate <- as.Date(
      pmax(dis_numeric, adm_numeric + 1),
      origin = "1970-01-01"
    )
  }
  if ("epistart" %in% names(df)) {
    epiend_numeric <- as.numeric(df$epiend)
    epistart_numeric <- as.numeric(as.Date(df$epistart))
    df$epiend <- as.Date(
      pmax(epiend_numeric, epistart_numeric + 1),
      origin = "1970-01-01"
    )
    df$epidur <- as.integer(as.Date(df$epiend) - as.Date(df$epistart))
  } else {
    df$epidur <- NA_integer_
  }

  df$epitype <- sample_from_spec_options(spec_df, script_name, "epitype", n)
  df$firstreg <- sample_from_spec_options(spec_df, script_name, "firstreg", n)
  df$imd04_decile <- sample_from_spec_options(spec_df, script_name, "imd04_decile", n)
  df$intmanig <- sample_from_spec_options(spec_df, script_name, "intmanig", n)
  df$mainspef <- sample_mainspef(n)
  df$posopdur <- sample(0:30, n, replace = TRUE)
  df$preopdur <- sample(0:30, n, replace = TRUE)
  df$procode3_ofh <- random_fixed_digits(n, 6)
  df$protype <- sample_from_spec_options(spec_df, script_name, "protype", n)
  df$rttperend <- random_date_between(n, "2008-01-01", "2035-12-31")
  df$rttperstart <- random_date_between(n, "2008-01-01", "2035-12-31")
  df$rttperstat <- sample_from_spec_options(spec_df, script_name, "rttperstat", n)
  df$spelbgin <- sample_from_spec_options(spec_df, script_name, "spelbgin", n)
  df$spelend <- sample_from_spec_options(spec_df, script_name, "spelend", n)

  df$susrecid <- ifelse(runif(n) < 0.20, "Null", random_digit_string(n, 13, 19))
  df$susspellid <- df$susrecid
  df$tretspef <- sample_tretspef(n)
  df
}

invest2_values <- c(
  "01 = X-ray plain film",
  "02 = Electrocardiogram",
  "03 = Haematology",
  "04 = Cross match blood/group & save serum for later cross match",
  "05 = Biochemistry",
  "06 = Urilysis",
  "07 = Bacteriology",
  "08 = Histology",
  "09 = Computerised tomography (retired 2006)",
  "10 = Ultrasound",
  "11 = Magnetic resonance imaging",
  "12 = Computerised tomography (exc genito urinary contrast examition/tomography)",
  "13 = Genito urinary contrast examition/tomography",
  "14 = Clotting studies",
  "15 = Immunology",
  "16 = Cardiac enzymes",
  "17 = Arterial/capillary blood gas",
  "18 = Toxicology",
  "19 = Blood culture",
  "20 = Serology",
  "21 = Pregncy test",
  "22 = Dental investigation",
  "23 = Refraction, orthoptic tests and computerised visual fields",
  "24 = None",
  "99 = Other"
)

sample_invest2_value <- function(n) {
  # Majority "24 = None" as specified.
  weights <- rep(1, length(invest2_values))
  weights[invest2_values == "24 = None"] <- 18
  sample(invest2_values, n, replace = TRUE, prob = weights)
}

sample_treat_code <- function(n) {
  sample(as.character(0:222), n, replace = TRUE)
}

add_nhse_ed_new_columns <- function(df) {
  script_name <- "09_generate_nhse_eng_ed.R"
  spec_df <- load_nhse_new_col_spec()
  n <- nrow(df)
  if (n == 0) return(df)

  df$aearrivalmode <- sample_from_spec_options(spec_df, script_name, "aearrivalmode", n)
  df$aeattendcat <- sample_from_spec_options(spec_df, script_name, "aeattendcat", n)
  df$aeattenddisp <- sample_from_spec_options(spec_df, script_name, "aeattenddisp", n)
  df$aedepttype <- sample_from_spec_options(spec_df, script_name, "aedepttype", n)
  df$aerefsource <- sample_from_spec_options(spec_df, script_name, "aerefsource", n)
  df$arrivaldate <- random_date_between(n, "2007-01-01", "2020-09-01")

  valid_concl <- random_military_time(n)
  df$concltime <- ifelse(runif(n) < 0.05, "4000 = Null time submitted", valid_concl)

  valid_dep <- random_military_time(n)
  df$deptime <- ifelse(runif(n) < 0.02, "4000 = Null time submitted", valid_dep)

  df$arrivaltime <- random_military_time(n)
  df$aekey <- random_fixed_digits(n, 12)
  df$epikey <- random_digit_string(n, 11, 19)
  df$imd04_decile <- sample_from_spec_options(spec_df, script_name, "imd04_decile", n)

  # Bespoke investigation logic from specification.
  invest_presence <- c(1.00, 0.40, 0.20, 0.10, 0.06, 0.04, 0.03, 0.02, 0.015, 0.010, 0.007, 0.005)
  for (i in 1:12) {
    col <- sprintf("invest2_%02d", i)
    values <- rep(NA_character_, n)
    keep <- runif(n) < invest_presence[i]
    values[keep] <- sample_invest2_value(sum(keep))
    df[[col]] <- values
  }

  df$procode3_ofh <- random_fixed_digits(n, 6)
  df$protype <- sample_from_spec_options(spec_df, script_name, "protype", n)
  df$susspellid <- ifelse(runif(n) < 0.20, "Null", random_digit_string(n, 13, 19))

  # Bespoke treatment logic from specification (treat_01 includes some Nulls).
  treat_presence <- c(0.80, 0.35, 0.18, 0.10, 0.06, 0.04, 0.03, 0.02, 0.015, 0.010, 0.007, 0.005)
  for (i in 1:12) {
    col <- sprintf("treat_%02d", i)
    values <- rep(NA_character_, n)
    keep <- runif(n) < treat_presence[i]
    values[keep] <- sample_treat_code(sum(keep))
    df[[col]] <- values
  }

  df$trettime <- random_military_time(n)
  df
}
