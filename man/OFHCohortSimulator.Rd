\name{OFHCohortSimulator}
\alias{OFHCohortSimulator}
\title{Reference Class for OFH Cohort Generation}
\description{Reference class API for configuring and running synthetic cohort generation.}
\usage{
OFHCohortSimulator
}
\details{
Create an instance with \code{OFHCohortSimulator$new(...)} and run generation via
\code{$run_all(n = ...)}.
}
\examples{
sim <- OFHCohortSimulator$new(project_root = ".", seed = 123)
out <- sim$run_all(n = 100, save_csv = FALSE, return_objects = TRUE)
}
