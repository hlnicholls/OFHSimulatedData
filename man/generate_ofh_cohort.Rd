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
  \item{icd10_file}{Optional path to a TXT/CSV file with one ICD-10 code per line.}
  \item{opcs4}{Optional named character vector of OPCS-4 descriptions.}
  \item{opcs4_file}{Optional path to a TXT/CSV file with one OPCS-4 code per line.}
  \item{bnf_codes}{Optional vector of BNF code strings.}
  \item{bnf_codes_file}{Optional path to a TXT/CSV file with one BNF code per line.}
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
