test_that("generate_ofh_cohort returns expected outputs", {
  out <- generate_ofh_cohort(n = 80, seed = 1, save_csv = FALSE, return_objects = TRUE)

  expected <- c(
    "participant_data",
    "clinic_measurements_data",
    "questionnaire_data",
    "nhse_outpat_data",
    "nhse_inpat_data",
    "nhse_engwal_deaths_data",
    "nhse_ed_data",
    "nhse_primcare_meds_data",
    "country_region_data"
  )

  expect_true(all(expected %in% names(out)))
  expect_equal(nrow(out$participant_data), 80)
  expect_true("pid" %in% names(out$nhse_primcare_meds_data))
})

test_that("default output folder is created", {
  td <- tempfile("ofh_output_test_")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
  old <- setwd(td)
  on.exit(setwd(old), add = TRUE)

  syn <- OFHCohortSynthesizer$new(project_root = ".", seed = 2)
  syn$run_all(n = 60)

  expect_true(dir.exists(file.path(td, "output")))
  expect_true(file.exists(file.path(td, "output", "participant_data.csv")))
})

test_that("code files can be used for ICD10/OPCS4/BNF", {
  td <- tempdir()
  icd10_file <- file.path(td, "icd10_codes.csv")
  opcs4_file <- file.path(td, "opcs4_codes.csv")
  bnf_file <- file.path(td, "bnf_codes.txt")

  writeLines(
    c(
      "code,description",
      "I210,STEMI of anterolateral wall",
      "I500,Congestive heart failure",
      "I639,Cerebral infarction unspecified"
    ),
    icd10_file
  )
  writeLines(c("0212000B0", "0601023A0"), bnf_file)
  writeLines(c("K401", "K451", "K561"), opcs4_file)

  out <- generate_ofh_cohort(
    n = 70,
    seed = 11,
    icd10_file = icd10_file,
    opcs4_file = opcs4_file,
    bnf_codes_file = bnf_file,
    save_csv = FALSE,
    return_objects = TRUE
  )

  expect_equal(nrow(out$participant_data), 70)
  expect_true("pid" %in% names(out$nhse_primcare_meds_data))
})

test_that("txt code files support tab-separated code and description format", {
  td <- tempdir()
  icd10_file <- file.path(td, "icd10_desc_codes.txt")
  opcs4_file <- file.path(td, "opcs4_desc_codes.txt")
  bnf_file <- file.path(td, "bnf_codes_for_txt_test.txt")

  writeLines(
    c(
      "code\tdescription",
      "I210\tSTEMI of anterolateral wall",
      "I500\tCongestive heart failure"
    ),
    icd10_file
  )
  writeLines(
    c(
      "code\tdescription",
      "K401\tPercutaneous transluminal balloon angioplasty of coronary artery",
      "K451\tInsertion of drug-eluting stent into coronary artery"
    ),
    opcs4_file
  )
  writeLines(c("0212000B0", "0601023A0"), bnf_file)

  out <- generate_ofh_cohort(
    n = 90,
    seed = 21,
    icd10_file = icd10_file,
    opcs4_file = opcs4_file,
    bnf_codes_file = bnf_file,
    save_csv = FALSE,
    return_objects = TRUE
  )

  outpat <- out$nhse_outpat_data
  non_missing_diag <- outpat$diag_4_01[!is.na(outpat$diag_4_01)]
  expect_true(length(non_missing_diag) > 0)
  expect_true(any(grepl("I210 = STEMI of anterolateral wall|I500 = Congestive heart failure", non_missing_diag)))
})

test_that("code-only TXT ICD10/OPCS4 is rejected", {
  td <- tempdir()
  icd10_file <- file.path(td, "icd10_codes_only.txt")
  opcs4_file <- file.path(td, "opcs4_codes_only.txt")
  bnf_file <- file.path(td, "bnf_codes_for_error_test.txt")

  writeLines(c("I500", "N189"), icd10_file)
  writeLines(c("K401", "K451"), opcs4_file)
  writeLines(c("0212000B0", "0601023A0"), bnf_file)

  expect_error(
    generate_ofh_cohort(
      n = 120,
      seed = 31,
      icd10_file = icd10_file,
      opcs4_file = opcs4_file,
      bnf_codes_file = bnf_file,
      save_csv = FALSE,
      return_objects = TRUE
    ),
    "must be tab-separated"
  )
})

test_that("structured BNF CSV supports custom code, name, and formulation", {
  td <- tempdir()
  bnf_file <- file.path(td, "bnf_structured_codes.csv")

  writeLines(
    c(
      "BNFCode,BNFName,Formulation,Strength",
      "ZZZ0001AA,Custom Test Drug A,tablets,10 mg",
      "ZZZ0002BB,Custom Test Drug B,inhaler,100 mcg"
    ),
    bnf_file
  )

  out <- generate_ofh_cohort(
    n = 90,
    seed = 41,
    bnf_codes_file = bnf_file,
    save_csv = FALSE,
    return_objects = TRUE
  )

  meds <- out$nhse_primcare_meds_data
  expected_codes <- c("ZZZ0001AA", "ZZZ0002BB")

  expect_equal(nrow(out$participant_data), 90)
  expect_true(nrow(meds) > 0)
  expect_true(all(na.omit(unique(meds$paidbnfcode)) %in% expected_codes))
  expect_true(all(unique(meds$prescribedbnfname) %in% c("Custom Test Drug A", "Custom Test Drug B")))
  expect_true(all(unique(meds$prescribedformulation) %in% c("tablets", "inhaler")))
})

test_that("vector and file inputs are mutually exclusive", {
  td <- tempdir()
  icd10_file <- file.path(td, "icd10_codes_2.txt")
  writeLines(c("I210", "I500"), icd10_file)

  expect_error(
    generate_ofh_cohort(
      n = 40,
      icd10 = c(I210 = "STEMI"),
      icd10_file = icd10_file,
      save_csv = FALSE,
      return_objects = TRUE
    )
  )
})

test_that("save_csv FALSE does not write output files", {
  td <- tempfile("ofh_no_csv_")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)

  out <- generate_ofh_cohort(
    n = 50,
    seed = 9,
    save_csv = FALSE,
    return_objects = TRUE,
    output_dir = td
  )

  expect_equal(nrow(out$participant_data), 50)
  expect_false(file.exists(file.path(td, "participant_data.csv")))
})

test_that("return_objects FALSE returns NULL but still writes CSVs", {
  td <- tempfile("ofh_csv_only_")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)

  out <- generate_ofh_cohort(
    n = 45,
    seed = 7,
    save_csv = TRUE,
    return_objects = FALSE,
    output_dir = td
  )

  expect_null(out)
  expect_true(file.exists(file.path(td, "participant_data.csv")))
})

test_that("at least one output target is required", {
  expect_error(
    generate_ofh_cohort(n = 20, save_csv = FALSE, return_objects = FALSE),
    "At least one"
  )
})

test_that("probability settings are configurable via generate_ofh_cohort", {
  out <- generate_ofh_cohort(
    n = 120,
    seed = 101,
    save_csv = FALSE,
    return_objects = TRUE,
    proportions = list(
      nhse_outpat = 0.10,
      nhse_inpat = 0.10,
      nhse_ed = 0.10
    ),
    record_multipliers = list(
      nhse_outpat = 0.15,
      nhse_inpat = 0.12,
      nhse_ed = 0.14
    ),
    code_config = list(
      nhse_outpat_data = list(diag_4_02_missing_prob = 0.65)
    )
  )

  expect_equal(nrow(out$participant_data), 120)
  expect_equal(nrow(out$questionnaire_data), 120)
  expect_lt(nrow(out$nhse_outpat_data), 60)
  expect_lt(nrow(out$nhse_inpat_data), 60)
  expect_lt(nrow(out$nhse_ed_data), 60)
})

test_that("partial probability override is accepted", {
  out <- generate_ofh_cohort(
    n = 75,
    seed = 202,
    save_csv = FALSE,
    return_objects = TRUE,
    proportions = list(nhse_outpat = 0.10),
    record_multipliers = list(nhse_outpat = 1.05)
  )

  expect_equal(nrow(out$participant_data), 75)
})

test_that("unknown probability names are rejected", {
  expect_error(
    generate_ofh_cohort(
      n = 30,
      save_csv = FALSE,
      return_objects = TRUE,
      proportions = list(not_a_dataset = 0.5)
    ),
    "Unknown proportions names"
  )
})

test_that("zero proportions auto-adjust multi-record multipliers", {
  out <- generate_ofh_cohort(
    n = 60,
    seed = 303,
    save_csv = FALSE,
    return_objects = TRUE,
    proportions = list(
      nhse_outpat = 0,
      nhse_inpat = 0,
      nhse_ed = 0
    )
  )

  expect_equal(nrow(out$participant_data), 60)
  expect_equal(nrow(out$nhse_outpat_data), 0)
  expect_equal(nrow(out$nhse_inpat_data), 0)
  expect_equal(nrow(out$nhse_ed_data), 0)
})

test_that("named icd10 vector preserves code keys as names not descriptions", {
  out <- generate_ofh_cohort(
    n = 60,
    seed = 401,
    icd10 = c(C61 = "Malignant neoplasm of prostate", Z854 = "Personal history of malignant neoplasm"),
    save_csv = FALSE,
    return_objects = TRUE
  )

  non_missing <- out$nhse_outpat_data$diag_4_01[!is.na(out$nhse_outpat_data$diag_4_01)]
  expect_true(length(non_missing) > 0)
  # entries should be "C61 = ..." not "Malignant neoplasm... = Malignant neoplasm..."
  expect_true(any(grepl("^C61 = |^Z854 = ", non_missing)))
  expect_false(any(grepl("^Malignant neoplasm of prostate = ", non_missing)))
})

test_that("icd10_weights work with named vector icd10 input", {
  expect_no_error(
    generate_ofh_cohort(
      n = 60,
      seed = 402,
      icd10 = c(C61 = "Malignant neoplasm of prostate", Z854 = "Personal history of malignant neoplasm"),
      code_config = list(
        nhse_outpat_data = list(icd10_weights = c(C61 = 5, Z854 = 1)),
        nhse_inpat_data  = list(icd10_weights = c(C61 = 5, Z854 = 1))
      ),
      save_csv = FALSE,
      return_objects = TRUE
    )
  )
})

test_that("icd10 only - default OPCS4 pool is not wiped", {
  out <- generate_ofh_cohort(
    n = 60,
    seed = 403,
    icd10 = c(C61 = "Malignant neoplasm of prostate"),
    save_csv = FALSE,
    return_objects = TRUE
  )

  # opertn_01 should have some non-NA entries from default OPCS4 codes
  opertn <- out$nhse_outpat_data$opertn_01[!is.na(out$nhse_outpat_data$opertn_01)]
  expect_true(length(opertn) > 0)
})

test_that("opcs4 only - default ICD-10 pool is not wiped", {
  out <- generate_ofh_cohort(
    n = 60,
    seed = 404,
    opcs4 = c(K401 = "Percutaneous transluminal balloon angioplasty of coronary artery"),
    save_csv = FALSE,
    return_objects = TRUE
  )

  # diag_4_01 should have non-NA entries from default ICD-10 codes
  diags <- out$nhse_outpat_data$diag_4_01[!is.na(out$nhse_outpat_data$diag_4_01)]
  expect_true(length(diags) > 0)
})