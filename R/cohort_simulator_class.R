#' Simulator class for OFH synthetic cohort generation
#'
#' @field project_root Kept for API compatibility; package assets are loaded from installed files.
#' @field generator_scripts_dir Path to template generator scripts.
#' @field data_dictionaries_dir Path to template data dictionaries.
#' @field output_dir Output directory for final CSV files.
#' @field seed Random seed.
#' @field config Generation configuration.
#' @export
OFHCohortSimulator <- methods::setRefClass(
  "OFHCohortSimulator",
  fields = list(
    project_root = "character",
    generator_scripts_dir = "character",
    data_dictionaries_dir = "character",
    output_dir = "character",
    seed = "numeric",
    config = "list"
  ),
  methods = list(
    initialize = function(project_root = ".", output_dir = NULL, seed = 42) {
      local_root <- normalizePath(project_root, mustWork = FALSE)
      local_scripts <- file.path(local_root, "inst", "generator_scripts")
      local_dicts <- file.path(local_root, "data_dictionaries")

      installed_scripts <- system.file("generator_scripts", package = "ofhsim")
      installed_dicts <- system.file("data_dictionaries", package = "ofhsim")

      # Prefer repository-local assets during development so script edits in the
      # workspace are used immediately without reinstalling the package.
      if (dir.exists(local_scripts) && dir.exists(local_dicts)) {
        project_root <<- local_root
        generator_scripts_dir <<- local_scripts
        data_dictionaries_dir <<- local_dicts
      } else if (nzchar(installed_scripts) && nzchar(installed_dicts)) {
        project_root <<- local_root
        generator_scripts_dir <<- installed_scripts
        data_dictionaries_dir <<- installed_dicts
      } else {
        stop("Could not locate local or installed generator scripts and data dictionaries")
      }

      out_dir <- output_dir
      if (is.null(output_dir)) {
        out_dir <- file.path(normalizePath(getwd(), mustWork = TRUE), "output")
      } else {
        # Resolve relative paths against caller working directory so output
        # location remains stable even when run_all() temporarily changes wd.
        if (!grepl("^(~|/|[A-Za-z]:[/\\\\])", output_dir)) {
          out_dir <- file.path(normalizePath(getwd(), mustWork = TRUE), output_dir)
        }
        out_dir <- normalizePath(path.expand(out_dir), mustWork = FALSE)
      }
      output_dir <<- out_dir

      seed <<- as.integer(seed)
      config <<- list()
    },

    generate_ofh_population = function(n = 1000, seed = 123) {
      generate_ofh_population(n = n, seed = seed)
    },

    add_inpatient_events = function(data, events_per_person = 5, icd10_codes = c("I210", "I500", "I639"), opcs4_codes = c("K401", "K451"), seed = 123) {
      add_inpatient_events(
        data = data,
        events_per_person = events_per_person,
        icd10_codes = icd10_codes,
        opcs4_codes = opcs4_codes,
        seed = seed
      )
    },

    simulate_drug_exposure = function(data, drug_list = c("0212000B0", "0601023A0"), seed = 123, mean_items_per_person = 2) {
      simulate_drug_exposure(
        data = data,
        drug_list = drug_list,
        seed = seed,
        mean_items_per_person = mean_items_per_person
      )
    },

    set_code_pools = function(icd10 = NULL, opcs4 = NULL, bnf_codes = NULL, bnf_meds = NULL) {
      if (!is.null(icd10) && length(icd10) > 0) {
        icd10_vals <- as.character(unname(icd10))
        icd10_nms <- names(icd10)
        if (is.null(icd10_nms) || any(icd10_nms == "")) {
          icd10_nms <- icd10_vals
        }
        icd10 <- stats::setNames(icd10_vals, icd10_nms)
      }
      if (!is.null(opcs4) && length(opcs4) > 0) {
        opcs4_vals <- as.character(unname(opcs4))
        opcs4_nms <- names(opcs4)
        if (is.null(opcs4_nms) || any(opcs4_nms == "")) {
          opcs4_nms <- opcs4_vals
        }
        opcs4 <- stats::setNames(opcs4_vals, opcs4_nms)
      }

      cc <- list()
      if (!is.null(icd10) || !is.null(opcs4)) {
        icd10_map <- if (!is.null(icd10)) icd10 else c()
        opcs4_map <- if (!is.null(opcs4)) opcs4 else c()

        cc$nhse_outpat_data <- list(icd10_descriptions = icd10_map, opcs4_descriptions = opcs4_map)
        cc$nhse_inpat_data <- list(icd10_descriptions = icd10_map, opcs4_descriptions = opcs4_map)
        cc$nhse_engwal_deaths_data <- list(icd10_descriptions = icd10_map)
      }

      if (!is.null(bnf_codes) || !is.null(bnf_meds)) {
        meds_cfg <- list()
        if (!is.null(bnf_codes) && length(bnf_codes) > 0) {
          meds_cfg$bnf_codes <- as.character(bnf_codes)
        }
        if (!is.null(bnf_meds) && nrow(as.data.frame(bnf_meds)) > 0) {
          meds_cfg$bnf_meds <- as.data.frame(bnf_meds, stringsAsFactors = FALSE)
        }
        cc$nhse_primcare_meds_data <- meds_cfg
      }

      config$code_config <<- cc
      invisible(.self)
    },

    build_config = function(n = 5000, proportions = NULL, record_multipliers = NULL, code_config = NULL) {
      assert_named_numeric_list <- function(x, defaults, label) {
        if (is.null(x)) return(defaults)
        if (!is.list(x)) stop(sprintf("%s must be a named list", label))
        if (is.null(names(x)) || any(!nzchar(names(x)))) {
          stop(sprintf("%s must be a named list", label))
        }
        unknown <- setdiff(names(x), names(defaults))
        if (length(unknown) > 0) {
          stop(sprintf(
            "Unknown %s names: %s",
            label,
            paste(unknown, collapse = ", ")
          ))
        }
        for (nm in names(x)) {
          v <- x[[nm]]
          if (!is.numeric(v) || length(v) != 1 || is.na(v)) {
            stop(sprintf("%s$%s must be a single numeric value", label, nm))
          }
        }
        .ofh_merge_lists(defaults, x)
      }

      base_props <- ofh_default_proportions()
      base_mults <- ofh_default_record_multipliers()

      merged_props <- assert_named_numeric_list(proportions, base_props, "proportions")
      merged_mults <- assert_named_numeric_list(record_multipliers, base_mults, "record_multipliers")

      # Multi-record datasets cannot have positive total_records when
      # unique_pids resolves to zero. Keep multipliers consistent with
      # explicit zero-coverage requests.
      multi_record_map <- c(
        nhse_outpat = "nhse_outpat",
        nhse_inpat = "nhse_inpat",
        nhse_ed = "nhse_ed"
      )
      for (nm in names(multi_record_map)) {
        prop_name <- multi_record_map[[nm]]
        if (!is.null(merged_props[[prop_name]]) && merged_props[[prop_name]] <= 0) {
          merged_mults[[nm]] <- 0
        }
      }

      if (!is.null(code_config) && !is.list(code_config)) {
        stop("code_config must be a list")
      }

      cc_existing <- if (!is.null(config$code_config)) config$code_config else list()
      cc_override <- if (!is.null(code_config)) code_config else list()
      cc <- .ofh_merge_lists(cc_existing, cc_override)

      config <<- ofh_build_config(
        n = n,
        proportions = merged_props,
        record_multipliers = merged_mults,
        code_config = cc
      )
      invisible(.self)
    },

    run_all = function(
      n = 5000,
      seed = NULL,
      save_csv = TRUE,
      return_objects = TRUE,
      proportions = NULL,
      record_multipliers = NULL,
      code_config = NULL
    ) {
      if (!is.null(seed)) seed <<- as.integer(seed)
      if (!is.logical(save_csv) || length(save_csv) != 1 || is.na(save_csv)) {
        stop("save_csv must be a single TRUE/FALSE value")
      }
      if (!is.logical(return_objects) || length(return_objects) != 1 || is.na(return_objects)) {
        stop("return_objects must be a single TRUE/FALSE value")
      }
      if (!isTRUE(save_csv) && !isTRUE(return_objects)) {
        stop("At least one of save_csv or return_objects must be TRUE")
      }

      build_config(
        n = n,
        proportions = proportions,
        record_multipliers = record_multipliers,
        code_config = code_config
      )

      cat("\n=== GENERATION CONFIGURATION ===\n")
      cat("Cohort size:", config$total_pid_count, "participants\n")
      icd10_count <- if (!is.null(config$code_config$nhse_outpat_data$icd10_descriptions)) {
        length(config$code_config$nhse_outpat_data$icd10_descriptions)
      } else {
        0
      }
      opcs4_count <- if (!is.null(config$code_config$nhse_outpat_data$opcs4_descriptions)) {
        length(config$code_config$nhse_outpat_data$opcs4_descriptions)
      } else {
        0
      }
      bnf_count <- if (!is.null(config$code_config$nhse_primcare_meds_data$bnf_meds)) {
        nrow(as.data.frame(config$code_config$nhse_primcare_meds_data$bnf_meds))
      } else if (!is.null(config$code_config$nhse_primcare_meds_data$bnf_codes)) {
        length(config$code_config$nhse_primcare_meds_data$bnf_codes)
      } else {
        0
      }
      cat("ICD-10 codes: ", icd10_count, "\n", sep = "")
      cat("OPCS4 codes: ", opcs4_count, "\n", sep = "")
      cat("BNF codes: ", bnf_count, "\n", sep = "")
      cat("=== GENERATING DATASETS ===\n\n")

      work_root <- tempfile("ofh_build_")
      dir.create(work_root, recursive = TRUE, showWarnings = FALSE)
      work_root <- normalizePath(work_root, mustWork = TRUE)
      on.exit(unlink(work_root, recursive = TRUE, force = TRUE), add = TRUE)

      work_scripts <- file.path(work_root, "generator_scripts")
      work_dicts <- file.path(work_root, "data_dictionaries")
      work_data <- file.path(work_root, "data")
      dir.create(work_data, recursive = TRUE, showWarnings = FALSE)
      dir.create(work_scripts, recursive = TRUE, showWarnings = FALSE)
      dir.create(work_dicts, recursive = TRUE, showWarnings = FALSE)

      file.copy(list.files(generator_scripts_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE),
                work_scripts, recursive = TRUE)
      file.copy(list.files(data_dictionaries_dir, pattern = "\\.csv$", full.names = TRUE),
                work_dicts, recursive = FALSE)

      # Ensure PDF-derived questionnaire value catalog is available in the
      # temp generator workspace used by questionnaire/section_utils.R.
      pdf_catalog_name <- "questionnaire_column_unique_values_from_pdfs.csv"
      pdf_catalog_sources <- c(
        file.path(generator_scripts_dir, "questionnaire", pdf_catalog_name),
        file.path(project_root, "questionnaire_analysis", pdf_catalog_name),
        file.path(project_root, "inst", "generator_scripts", "questionnaire", pdf_catalog_name)
      )
      pdf_catalog_source <- pdf_catalog_sources[file.exists(pdf_catalog_sources)][1]
      if (!is.na(pdf_catalog_source) && nzchar(pdf_catalog_source)) {
        file.copy(
          pdf_catalog_source,
          file.path(work_scripts, "questionnaire", pdf_catalog_name),
          overwrite = TRUE
        )
      }

      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(work_scripts)

      source("generate_pids.R")
      old_ofh_opts <- options(
        OFH_GEN_CONFIG = config,
        OFH_ALL_STUDY_PIDS = generate_pids(config$total_pid_count)
      )
      on.exit(options(old_ofh_opts), add = TRUE)
      set.seed(.self$seed)

      scripts <- c(
        "generate_participant_data.R",
        "generate_clinic_measurements_data.R",
        "generate_questionnaire_data.R",
        "generate_nhse_outpatient_data.R",
        "generate_nhse_inpatient_data.R",
        "generate_nhse_deaths_data.R",
        "generate_nhse_emergency_data.R",
        "generate_nhse_primary_care_meds_data.R",
        "generate_country_region_data.R"
      )
      for (script in scripts) source(script)

      src_data_dir <- file.path(work_root, "data")
      if (!dir.exists(src_data_dir)) {
        stop("Expected data outputs were not created by generator scripts")
      }

      if (isTRUE(save_csv)) {
        out_files <- list.files(src_data_dir, pattern = "\\.csv$", full.names = TRUE)
        if (!dir.exists(.self$output_dir)) {
          dir.create(.self$output_dir, recursive = TRUE, showWarnings = FALSE)
        }
        file.copy(out_files, .self$output_dir, overwrite = TRUE)
        message(sprintf("CSV files saved to: %s", .self$output_dir))
      }

      if (isTRUE(return_objects)) {
        read_dir <- if (isTRUE(save_csv)) .self$output_dir else src_data_dir
        out <- .self$read_outputs(data_dir = read_dir)

        if (interactive()) {
          list2env(out, envir = .GlobalEnv)
          alias_map <- list(
            clinic_data = out$clinic_measurements_data,
            inpatient_data = out$nhse_inpat_data,
            outpatient_data = out$nhse_outpat_data,
            emergency_data = out$nhse_ed_data,
            deaths_data = out$nhse_engwal_deaths_data,
            primary_care_meds_data = out$nhse_primcare_meds_data,
            meds_data = out$nhse_primcare_meds_data,
            country_region_data = out$country_region_data
          )
          list2env(alias_map, envir = .GlobalEnv)
        }

        return(invisible(out))
      }

      invisible(NULL)
    },

    read_outputs = function(data_dir = output_dir) {
      files <- c(
        participant_data = "participant_data.csv",
        clinic_measurements_data = "clinic_measurements_data.csv",
        questionnaire_data = "questionnaire_data.csv",
        nhse_outpat_data = "nhse_outpat_data.csv",
        nhse_inpat_data = "nhse_inpat_data.csv",
        nhse_engwal_deaths_data = "nhse_engwal_deaths_data.csv",
        nhse_ed_data = "nhse_ed_data.csv",
        nhse_primcare_meds_data = "nhse_primcare_meds_data.csv",
        country_region_data = "country_region_data.csv"
      )

      out <- list()
      for (nm in names(files)) {
        p <- file.path(data_dir, files[[nm]])
        out[[nm]] <- utils::read.csv(p, stringsAsFactors = FALSE)
      }
      out
    }
  )
)
