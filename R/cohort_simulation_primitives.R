#' Generate synthetic OFH-style participant IDs
#'
#' @param n Number of IDs to generate.
#' @return Character vector of unique IDs.
ofh_generate_pids <- function(n) {
  stopifnot(is.numeric(n), length(n) == 1, !is.na(n), n > 0)
  n <- as.integer(n)

  chars <- c(0:9, LETTERS)
  pids <- character(n)
  for (i in seq_len(n)) {
    pids[i] <- paste0(sample(chars, 8, replace = TRUE), collapse = "")
  }
  unique(pids)
}

#' Generate a synthetic participant population
#'
#' @param n Population size.
#' @param seed Random seed.
#' @return Data frame with participant-level columns.
#' @export
generate_ofh_population <- function(n = 1000, seed = 123) {
  stopifnot(is.numeric(seed), length(seed) == 1)
  set.seed(as.integer(seed))

  pid <- ofh_generate_pids(as.integer(n))
  n <- length(pid)

  data.frame(
    pid = pid,
    sex = sample(c("Female", "Male"), n, replace = TRUE, prob = c(0.54, 0.46)),
    birth_year = sample(1935:2006, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

#' Add synthetic inpatient events
#'
#' @param data Data frame containing a pid column.
#' @param events_per_person Average events per person.
#' @param icd10_codes ICD-10 code pool.
#' @param opcs4_codes OPCS-4 code pool.
#' @param seed Random seed.
#' @return Data frame of inpatient events.
#' @export
add_inpatient_events <- function(
  data,
  events_per_person = 5,
  icd10_codes = c("I210", "I500", "I639", "E110", "J440"),
  opcs4_codes = c("K401", "K451", "K561", "M011", "E033"),
  seed = 123
) {
  stopifnot(is.data.frame(data), "pid" %in% names(data))
  stopifnot(is.numeric(events_per_person), length(events_per_person) == 1, events_per_person >= 0)

  set.seed(as.integer(seed))
  pid <- unique(data$pid)
  n <- length(pid)

  k <- as.integer(round(events_per_person * n))
  if (k == 0) {
    return(data.frame(
      pid = character(0),
      admidate = character(0),
      icd10 = character(0),
      opcs4 = character(0),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    pid = sample(pid, k, replace = TRUE),
    admidate = as.character(sample(seq(as.Date("2018-01-01"), as.Date("2025-12-31"), by = "day"), k, replace = TRUE)),
    icd10 = sample(icd10_codes, k, replace = TRUE),
    opcs4 = sample(opcs4_codes, k, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

#' Simulate primary-care drug exposure records
#'
#' @param data Data frame containing a pid column.
#' @param drug_list Character vector of BNF-like codes.
#' @param seed Random seed.
#' @param mean_items_per_person Mean items per participant.
#' @return Data frame of synthetic medication records.
#' @export
simulate_drug_exposure <- function(
  data,
  drug_list = c("0212000B0", "0601023A0"),
  seed = 123,
  mean_items_per_person = 2
) {
  stopifnot(is.data.frame(data), "pid" %in% names(data))
  stopifnot(length(drug_list) > 0)

  set.seed(as.integer(seed))
  pid <- unique(data$pid)
  n <- length(pid)
  n_rows <- as.integer(max(1, round(n * mean_items_per_person)))

  was_dispensed <- runif(n_rows) < 0.90

  data.frame(
    pid = sample(pid, n_rows, replace = TRUE),
    bsaprescriptionid = paste0("RX", sprintf("%010d", seq_len(n_rows))),
    prescribedbnfcode = sample(drug_list, n_rows, replace = TRUE),
    prescribedbnfname = paste("BNF", sample(drug_list, n_rows, replace = TRUE)),
    prescribedformulation = sample(c("0069 = Tablet", "0004 = Capsule", "0061 = Pressurised inhalation"), n_rows, replace = TRUE),
    prescribedmedicinestrength = sample(c("5 mg", "10 mg", "20 mg", "40 mg", NA), n_rows, replace = TRUE),
    prescribedquantity = sample(c(28L, 30L, 56L, 84L, 90L), n_rows, replace = TRUE),
    prescribeddmdcode = paste0(sample(1000000000:9999999999, n_rows, replace = TRUE), " = synthetic dmd"),
    paidindicator = ifelse(was_dispensed, "Y = yes", "N = no"),
    paidbnfcode = ifelse(was_dispensed, sample(drug_list, n_rows, replace = TRUE), NA),
    paidbnfname = ifelse(was_dispensed, paste("BNF", sample(drug_list, n_rows, replace = TRUE)), NA),
    paidquantity = ifelse(was_dispensed, sample(c(28L, 30L, 56L, 84L, 90L), n_rows, replace = TRUE), NA),
    stringsAsFactors = FALSE
  )
}
