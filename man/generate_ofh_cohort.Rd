\name{generate_ofh_cohort}
\alias{generate_ofh_cohort}
\title{Generate Synthetic OFH Cohort Datasets}
\description{Generate linked synthetic health datasets for a configurable cohort.}
\usage{
generate_ofh_cohort(
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
)
}
\arguments{
  \item{n}{Total synthetic cohort size.}
  \item{seed}{Random seed.}
  \item{icd10}{Optional named character vector of ICD-10 descriptions.}
  \item{icd10_file}{Optional path to a TXT/CSV file containing ICD-10 code and description pairs.
TXT format should be tab-separated with code and description columns.
CSV format should provide code and description columns.}
  \item{opcs4}{Optional named character vector of OPCS-4 descriptions.}
  \item{opcs4_file}{Optional path to a TXT/CSV file containing OPCS-4 code and description pairs.
TXT format should be tab-separated with code and description columns.
CSV format should provide code and description columns.}
  \item{bnf_codes}{Optional BNF input for primary care meds. Can be either
a character vector of BNF codes or a data frame with columns for code,
name, and formulation (optional strength).}
  \item{bnf_codes_file}{Optional path to a TXT/CSV file for BNF input. TXT
supports one BNF code per line. CSV supports either code-only or structured
medication rows containing code, name, and formulation (optional strength).}
  \item{proportions}{Optional named list of dataset-level coverage proportions.
Names should match \code{names(ofh_default_proportions())}.}
  \item{record_multipliers}{Optional named list of multipliers for multi-record
datasets. Names should match \code{names(ofh_default_record_multipliers())}.}
  \item{code_config}{Optional nested list overriding field-level code generation
probabilities and pools. Structure should follow \code{ofh_default_code_config()}.}
  \item{save_csv}{Whether to write CSV outputs to disk.}
  \item{return_objects}{Whether to return generated data frames as an R object.}
  \item{output_dir}{Output directory when \code{save_csv = TRUE}.}
  \item{write_csv}{Deprecated alias for \code{save_csv}.}
}
\value{Named list of generated data frames when \code{return_objects = TRUE}; otherwise invisible \code{NULL}.}
\examples{
out <- generate_ofh_cohort(n = 200, seed = 123, save_csv = FALSE, return_objects = TRUE)
names(out)
}
