\name{generation_primitives}
\alias{generate_ofh_population}
\alias{add_inpatient_events}
\alias{simulate_drug_exposure}
\title{Standalone Simulation Primitives}
\description{Utility functions for generating participant populations and event-level synthetic records.}
\usage{
generate_ofh_population(n = 1000, seed = 123)

add_inpatient_events(
  data,
  events_per_person = 5,
  icd10_codes = c("I210", "I500", "I639", "E110", "J440"),
  opcs4_codes = c("K401", "K451", "K561", "M011", "E033"),
  seed = 123
)

simulate_drug_exposure(
  data,
  drug_list = c("0212000B0", "0601023A0"),
  seed = 123,
  mean_items_per_person = 2
)
}
\arguments{
  \item{data}{Input data frame containing a \code{pid} column.}
  \item{n}{Number of participants.}
  \item{seed}{Random seed.}
  \item{events_per_person}{Mean events per participant.}
  \item{icd10_codes}{ICD-10 code pool.}
  \item{opcs4_codes}{OPCS-4 code pool.}
  \item{drug_list}{Medication code pool.}
  \item{mean_items_per_person}{Mean prescription items per participant.}
}
\value{Data frame output for each function.}
