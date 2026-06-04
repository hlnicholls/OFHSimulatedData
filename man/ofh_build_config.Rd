\name{ofh_build_config}
\alias{ofh_build_config}
\alias{ofh_default_proportions}
\alias{ofh_default_record_multipliers}
\alias{ofh_default_code_config}
\title{Configuration Helpers for OFH Generation}
\description{Helper functions that return default settings and compose full generation configuration lists.}
\usage{
ofh_default_proportions()
ofh_default_record_multipliers()
ofh_default_code_config()
ofh_build_config(
  n = 5000,
  proportions = ofh_default_proportions(),
  record_multipliers = ofh_default_record_multipliers(),
  code_config = list()
)
}
\arguments{
  \item{n}{Total cohort size.}
  \item{proportions}{Dataset proportions list.}
  \item{record_multipliers}{Record multiplier list for event datasets.}
  \item{code_config}{Optional code configuration overrides.}
}
\value{Return value depends on the function called:
\describe{
\item{\code{ofh_default_proportions()}}{Named numeric list of dataset proportions in [0, 1].}
\item{\code{ofh_default_record_multipliers()}}{Named numeric list of multipliers for multi-record datasets.}
\item{\code{ofh_default_code_config()}}{Nested named list containing default code pools, weights, and generation controls by dataset.}
\item{\code{ofh_build_config()}}{Named list with \code{total_pid_count} (integer), \code{datasets} (nested list of dataset sizing settings), and \code{code_config} (merged code configuration list).}
}
}
