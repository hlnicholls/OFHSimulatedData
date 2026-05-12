#' Generate synthetic OFH cohort datasets
#'
#' @param n Total synthetic cohort size.
#' @param seed Random seed.
#' @param icd10 Optional named character vector of ICD-10 code descriptions.
#' @param icd10_file Optional path to a TXT/CSV file containing ICD-10 code and description pairs.
#'   TXT must be tab-separated with code and description columns.
#' @param opcs4 Optional named character vector of OPCS-4 code descriptions.
#' @param opcs4_file Optional path to a TXT/CSV file containing OPCS-4 code and description pairs.
#'   TXT must be tab-separated with code and description columns.
#' @param bnf_codes Optional BNF input for primary care meds. Can be either
#'   a character vector of BNF codes or a data frame with columns for code,
#'   name, and formulation (optional strength).
#' @param bnf_codes_file Optional path to a TXT/CSV file for BNF input. TXT
#'   supports one BNF code per line. CSV supports either code-only or structured
#'   medication rows containing code, name, and formulation (optional strength).
#' @param proportions Optional named list of dataset-level coverage proportions.
#'   Names should match \\code{names(ofh_default_proportions())}.
#' @param record_multipliers Optional named list of multipliers for multi-record
#'   datasets. Names should match \\code{names(ofh_default_record_multipliers())}.
#' @param code_config Optional nested list overriding field-level code generation
#'   probabilities and pools. Structure should follow \\code{ofh_default_code_config()}.
#' @param save_csv Whether to write CSV files.
#' @param return_objects Whether to return generated data frames as an R object.
#' @param output_dir Output directory when save_csv is TRUE.
#' @param write_csv Deprecated alias for save_csv.
#' @return Named list of generated data frames when return_objects is TRUE;
#'   otherwise invisible NULL.
#' @export
.ofh_read_codes_file <- function(path, named = FALSE) {
  if (is.null(path)) return(NULL)
  if (!file.exists(path)) stop(sprintf("Code file not found: %s", path))

  ext <- tolower(tools::file_ext(path))
  if (ext == "txt") {
    df <- utils::read.delim(
      path,
      header = FALSE,
      sep = "\t",
      stringsAsFactors = FALSE,
      check.names = FALSE,
      quote = "",
      comment.char = "",
      fill = TRUE
    )
    if (nrow(df) == 0 || ncol(df) == 0) stop(sprintf("No codes found in file: %s", path))

    first_val <- tolower(trimws(as.character(df[1, 1])))
    second_val <- if (ncol(df) >= 2) tolower(trimws(as.character(df[1, 2]))) else ""
    if (first_val %in% c("code", "codes", "icd10", "opcs4", "bnf", "bnf_code", "bnfcode") ||
        second_val %in% c("description", "desc", "name")) {
      df <- df[-1, , drop = FALSE]
    }

    codes <- trimws(as.character(df[[1]]))
    codes <- codes[nzchar(codes)]
    if (length(codes) == 0) stop(sprintf("No codes found in file: %s", path))

    if (!named) return(unique(codes))

    if (ncol(df) < 2) {
      stop(
        sprintf(
          "TXT code files for ICD10/OPCS4 must be tab-separated with code and description columns: %s",
          path
        )
      )
    }

    desc <- trimws(as.character(df[[2]]))
    desc_map <- desc[match(codes, trimws(as.character(df[[1]])))]
    if (any(is.na(desc_map) | !nzchar(desc_map))) {
      stop(sprintf("TXT code file must include a description for every code: %s", path))
    }
    return(stats::setNames(desc_map, codes))
  }

  if (ext == "csv") {
    df <- utils::read.csv(path, header = FALSE, stringsAsFactors = FALSE, check.names = FALSE)
    if (nrow(df) == 0 || ncol(df) == 0) stop(sprintf("No codes found in file: %s", path))

    first_val <- tolower(trimws(as.character(df[1, 1])))
    second_val <- if (ncol(df) >= 2) tolower(trimws(as.character(df[1, 2]))) else ""
    if (first_val %in% c("code", "codes", "icd10", "opcs4", "bnf", "bnf_code", "bnfcode") || second_val == "description") {
      df <- df[-1, , drop = FALSE]
    }

    codes <- trimws(as.character(df[[1]]))
    codes <- codes[nzchar(codes)]
    if (length(codes) == 0) stop(sprintf("No codes found in file: %s", path))

    codes <- unique(codes)
    if (!named) return(codes)

    if (ncol(df) >= 2) {
      desc <- trimws(as.character(df[[2]]))
      desc_map <- desc[match(codes, trimws(as.character(df[[1]])))]
      desc_map[is.na(desc_map) | !nzchar(desc_map)] <- codes[is.na(desc_map) | !nzchar(desc_map)]
      return(stats::setNames(desc_map, codes))
    }

    return(stats::setNames(codes, codes))
  }

  stop("Code files must be .txt or .csv")
}

.ofh_as_named_codes <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  nms <- names(x)
  x <- as.character(x)
  names(x) <- nms
  if (is.null(names(x)) || any(names(x) == "")) {
    names(x) <- x
    return(x)
  }
  x
}

.ofh_parse_bnf_meds_df <- function(df, source_label = "bnf") {
  if (!is.data.frame(df) || nrow(df) == 0) {
    stop(sprintf("No BNF medication rows found in %s", source_label))
  }

  nms <- tolower(trimws(names(df)))
  code_candidates <- c("bnfcode", "bnf_code", "code", "codes", "bnf", "prescribedbnfcode")
  name_candidates <- c("bnfname", "bnf_name", "name", "description", "drug_name", "medication_name", "prescribedbnfname")
  form_candidates <- c("formulation", "form", "prescribedformulation")
  strength_candidates <- c("strength", "drugstrength", "drug_strength", "paiddrugstrength", "prescribedmedicinestrength")

  pick_col <- function(candidates) {
    idx <- match(candidates, nms)
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0) return(NA_integer_)
    idx[1]
  }

  i_code <- pick_col(code_candidates)
  i_name <- pick_col(name_candidates)
  i_form <- pick_col(form_candidates)
  i_strength <- pick_col(strength_candidates)

  # If columns are unlabeled but tabular, accept first 3 columns as
  # code/name/formulation for convenience.
  if (is.na(i_code) || is.na(i_name) || is.na(i_form)) {
    if (ncol(df) >= 3) {
      i_code <- 1
      i_name <- 2
      i_form <- 3
    } else {
      stop(
        sprintf(
          "%s must provide BNF code, name, and formulation columns",
          source_label
        )
      )
    }
  }

  out <- data.frame(
    BNFCode = trimws(as.character(df[[i_code]])),
    BNFName = trimws(as.character(df[[i_name]])),
    Formulation = trimws(as.character(df[[i_form]])),
    stringsAsFactors = FALSE
  )
  if (!is.na(i_strength)) {
    s <- trimws(as.character(df[[i_strength]]))
    s[s == "" | is.na(s)] <- NA_character_
    out$Strength <- s
  } else {
    out$Strength <- NA_character_
  }

  bad <- !nzchar(out$BNFCode) | !nzchar(out$BNFName) | !nzchar(out$Formulation)
  if (any(bad)) {
    stop(sprintf("%s contains rows missing BNF code, name, or formulation", source_label))
  }

  out <- unique(out[, c("BNFCode", "BNFName", "Strength", "Formulation"), drop = FALSE])
  rownames(out) <- NULL
  out
}

.ofh_parse_bnf_input <- function(bnf_codes = NULL, bnf_codes_file = NULL) {
  if (!is.null(bnf_codes) && !is.null(bnf_codes_file)) {
    stop("Provide either bnf_codes or bnf_codes_file, not both")
  }

  if (is.null(bnf_codes) && is.null(bnf_codes_file)) {
    return(list(codes = NULL, bnf_meds = NULL))
  }

  if (!is.null(bnf_codes_file)) {
    if (!file.exists(bnf_codes_file)) {
      stop(sprintf("Code file not found: %s", bnf_codes_file))
    }

    ext <- tolower(tools::file_ext(bnf_codes_file))
    if (ext == "txt") {
      codes <- .ofh_read_codes_file(bnf_codes_file, named = FALSE)
      return(list(codes = codes, bnf_meds = NULL))
    }
    if (ext != "csv") {
      stop("bnf_codes_file must be .txt or .csv")
    }

    # Prefer structured CSV with code/name/formulation headers when available.
    df_h <- utils::read.csv(bnf_codes_file, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
    nms_h <- tolower(trimws(names(df_h)))
    has_structured_headers <- any(nms_h %in% c("bnfname", "bnf_name", "name", "description", "drug_name", "medication_name")) &&
      any(nms_h %in% c("formulation", "form", "prescribedformulation"))

    if (has_structured_headers) {
      bnf_meds <- .ofh_parse_bnf_meds_df(df_h, source_label = "bnf_codes_file")
      return(list(codes = unique(bnf_meds$BNFCode), bnf_meds = bnf_meds))
    }

    # Backward-compatible code-only CSV support.
    codes <- .ofh_read_codes_file(bnf_codes_file, named = FALSE)
    return(list(codes = codes, bnf_meds = NULL))
  }

  if (is.data.frame(bnf_codes)) {
    bnf_meds <- .ofh_parse_bnf_meds_df(bnf_codes, source_label = "bnf_codes")
    return(list(codes = unique(bnf_meds$BNFCode), bnf_meds = bnf_meds))
  }

  codes <- as.character(bnf_codes)
  codes <- trimws(codes)
  codes <- unique(codes[nzchar(codes)])
  if (length(codes) == 0) codes <- NULL
  list(codes = codes, bnf_meds = NULL)
}

generate_ofh_cohort <- function(
  n = 5000,
  seed = 42,
  icd10 = NULL,
  icd10_file = NULL,
  opcs4 = NULL,
  opcs4_file = NULL,
  bnf_codes = NULL,
  bnf_codes_file = NULL,
  proportions = NULL,
  record_multipliers = NULL,
  code_config = NULL,
  save_csv = TRUE,
  return_objects = TRUE,
  output_dir = NULL,
  write_csv = NULL
) {
  if (!is.null(write_csv)) {
    warning("`write_csv` is deprecated; use `save_csv`.", call. = FALSE)
    save_csv <- isTRUE(write_csv)
  }

  if (!is.logical(save_csv) || length(save_csv) != 1 || is.na(save_csv)) {
    stop("save_csv must be a single TRUE/FALSE value")
  }
  if (!is.logical(return_objects) || length(return_objects) != 1 || is.na(return_objects)) {
    stop("return_objects must be a single TRUE/FALSE value")
  }
  if (!isTRUE(save_csv) && !isTRUE(return_objects)) {
    stop("At least one of save_csv or return_objects must be TRUE")
  }

  if (!is.null(icd10) && !is.null(icd10_file)) stop("Provide either icd10 or icd10_file, not both")
  if (!is.null(opcs4) && !is.null(opcs4_file)) stop("Provide either opcs4 or opcs4_file, not both")
  if (!is.null(bnf_codes) && !is.null(bnf_codes_file)) stop("Provide either bnf_codes or bnf_codes_file, not both")

  icd10 <- if (!is.null(icd10_file)) .ofh_read_codes_file(icd10_file, named = TRUE) else .ofh_as_named_codes(icd10)
  opcs4 <- if (!is.null(opcs4_file)) .ofh_read_codes_file(opcs4_file, named = TRUE) else .ofh_as_named_codes(opcs4)
  bnf_input <- .ofh_parse_bnf_input(bnf_codes = bnf_codes, bnf_codes_file = bnf_codes_file)
  bnf_codes <- bnf_input$codes
  bnf_meds <- bnf_input$bnf_meds

  sim <- OFHCohortSimulator$new(
    project_root = ".",
    output_dir = if (isTRUE(save_csv)) output_dir else tempdir(),
    seed = seed
  )

  sim$set_code_pools(icd10 = icd10, opcs4 = opcs4, bnf_codes = bnf_codes, bnf_meds = bnf_meds)
  out <- sim$run_all(
    n = n,
    seed = seed,
    save_csv = save_csv,
    return_objects = return_objects,
    proportions = proportions,
    record_multipliers = record_multipliers,
    code_config = code_config
  )

  if (isTRUE(return_objects)) {
    out
  } else {
    invisible(NULL)
  }
}
