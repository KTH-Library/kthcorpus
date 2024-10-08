% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aurora_classifier.R
\name{aurora}
\alias{aurora}
\title{SDG classification for a text snippet}
\usage{
aurora(snippet, fmt = c("table", "object"))
}
\arguments{
\item{snippet}{the text snippet, character}

\item{fmt}{the response returned, one of "table" or "object"}
}
\value{
either table or object with the response
}
\description{
Predict a suitable SDG goal for the provided text
}
\details{
See details about the API: \url{https://aurora-sdg.labs.vu.nl/sdg-classifier/api}
}
