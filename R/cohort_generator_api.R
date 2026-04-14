#' Generate synthetic OFH cohort datasets
#'
#' @param n Total synthetic cohort size.
#' @param seed Random seed.
#' @param icd10 Optional named character vector of ICD-10 code descriptions.
#' @param icd10_file Optional path to a TXT/CSV file with one ICD-10 code per line.
#' @param opcs4 Optional named character vector of OPCS-4 code descriptions.
#' @param opcs4_file Optional path to a TXT/CSV file with one OPCS-4 code per line.
#' @param bnf_codes Optional vector of BNF codes for primary care meds.
#' @param bnf_codes_file Optional path to a TXT/CSV file with one BNF code per line.
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
    codes <- trimws(readLines(path, warn = FALSE))
    codes <- codes[nzchar(codes)]
    if (length(codes) == 0) stop(sprintf("No codes found in file: %s", path))
    if (!named) return(unique(codes))
    return(stats::setNames(unique(codes), unique(codes)))
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
  x <- as.character(x)
  if (is.null(names(x)) || any(names(x) == "")) {
    names(x) <- x
    return(x)
  }
  x
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
  bnf_codes <- if (!is.null(bnf_codes_file)) .ofh_read_codes_file(bnf_codes_file, named = FALSE) else as.character(bnf_codes)

  sim <- OFHCohortSimulator$new(
    project_root = ".",
    output_dir = if (isTRUE(save_csv)) output_dir else tempdir(),
    seed = seed
  )

  sim$set_code_pools(icd10 = icd10, opcs4 = opcs4, bnf_codes = bnf_codes)
  out <- sim$run_all(n = n, seed = seed, save_csv = save_csv, return_objects = return_objects)

  if (isTRUE(return_objects)) {
    out
  } else {
    invisible(NULL)
  }
}
