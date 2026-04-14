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
  td <- tempdir()
  old <- setwd(td)
  on.exit(setwd(old), add = TRUE)

  sim <- OFHCohortSimulator$new(project_root = ".", seed = 2)
  sim$run_all(n = 60)

  expect_true(dir.exists(file.path(td, "output")))
  expect_true(file.exists(file.path(td, "output", "participant_data.csv")))
})

test_that("code files can be used for ICD10/OPCS4/BNF", {
  td <- tempdir()
  icd10_file <- file.path(td, "icd10_codes.txt")
  opcs4_file <- file.path(td, "opcs4_codes.csv")
  bnf_file <- file.path(td, "bnf_codes.txt")

  writeLines(c("I210", "I500", "I639"), icd10_file)
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

test_that("write_csv alias remains supported", {
  expect_warning(
    out <- generate_ofh_cohort(n = 35, seed = 5, write_csv = FALSE),
    "deprecated"
  )
  expect_equal(nrow(out$participant_data), 35)
})

test_that("at least one output target is required", {
  expect_error(
    generate_ofh_cohort(n = 20, save_csv = FALSE, return_objects = FALSE),
    "At least one"
  )
})