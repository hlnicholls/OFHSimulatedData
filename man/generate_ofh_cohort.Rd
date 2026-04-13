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
  write_csv = TRUE,
  output_dir = NULL
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
  \item{write_csv}{Whether to write CSV outputs to disk.}
  \item{output_dir}{Output directory when \code{write_csv = TRUE}.}
}
\value{Named list of generated data frames.}
\examples{
out <- generate_ofh_cohort(n = 200, seed = 123, write_csv = FALSE)
names(out)
}
