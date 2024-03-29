% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/curated.R
\name{kth_diva_curated}
\alias{kth_diva_curated}
\title{Curated DiVA corpus data
A curated version of DiVA data with S2_publication_ID and S2_authors}
\usage{
kth_diva_curated(jq = NULL)
}
\arguments{
\item{jq}{query for \href{https://docs.ropensci.org/jqr/}{filtering the data}}
}
\value{
tibble
}
\description{
Curated DiVA corpus data
A curated version of DiVA data with S2_publication_ID and S2_authors
}
\examples{
\dontrun{
 kth_diva_curated(jq = 'select(.PID == "13125" or .PID == "461429")')
 kth_diva_curated(jq = 'select(.S2_publication_ID == "f00ec4c737ca6722a4629eb5bcc25787c2173df5")')
}
}
