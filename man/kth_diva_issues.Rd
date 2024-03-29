% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks.R
\name{kth_diva_issues}
\alias{kth_diva_issues}
\title{Potential data quality issues for DiVA author data}
\usage{
kth_diva_issues(authors = kth_diva_authors())
}
\arguments{
\item{authors}{a tibble with authors data, default: kth_diva_authors()}
}
\value{
a list with slots for data frames (details, overview, and associated
publications)
}
\description{
This function summarizes some potential data quality issues for DiVA
author data, for example records with ORCIDs that relates to multiple
KTH author identifiers and vice versa. Some of these records may be
candidates for merging author data at the source.
}
