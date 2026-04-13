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
\value{A named list configuration object.}
