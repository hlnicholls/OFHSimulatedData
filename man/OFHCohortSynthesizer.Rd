\name{OFHCohortSynthesizer}
\alias{OFHCohortSynthesizer}
\title{Reference Class for OFH Cohort Generation}
\description{Reference class API for configuring and running synthetic cohort generation.}
\usage{
OFHCohortSynthesizer
}
\details{
Create an instance with \code{OFHCohortSynthesizer$new(...)} and run generation via
\code{$run_all(n = ...)}.
}
\examples{
syn <- OFHCohortSynthesizer$new(project_root = ".", seed = 123)
out <- syn$run_all(n = 100, save_csv = FALSE, return_objects = TRUE)
}
