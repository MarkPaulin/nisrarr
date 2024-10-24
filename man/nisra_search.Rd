% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nisra_data_portal.R
\name{nisra_search}
\alias{nisra_search}
\title{Search for a NISRA dataset}
\usage{
nisra_search(
  keyword = NULL,
  regex = NULL,
  dataset_code = NULL,
  variables = NULL,
  datefrom = NULL,
  flush_cache = FALSE
)
}
\arguments{
\item{keyword}{Text to search for in dataset titles}

\item{regex}{Regular expression for searching dataset titles}

\item{dataset_code}{Dataset to find}

\item{variables}{Variables to search for in datasets}

\item{datefrom}{Date to search from}

\item{flush_cache}{Ignore cached values}
}
\value{
A \code{\link[tibble:tibble]{tibble::tibble()}} of dataset information matching the search terms
}
\description{
Search for a NISRA dataset
}
