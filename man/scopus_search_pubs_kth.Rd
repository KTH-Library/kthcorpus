% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scopus_client.R
\name{scopus_search_pubs_kth}
\alias{scopus_search_pubs_kth}
\title{Retrieve publications from Scopus API from KTH - The Royal Institute of Technology.}
\usage{
scopus_search_pubs_kth(
  beg_loaddate,
  end_loaddate,
  include_early_access = TRUE,
  beg_pubyear
)
}
\arguments{
\item{beg_loaddate}{date expressed as "yyyymmdd", by default current date minus 7 days}

\item{end_loaddate}{date expressed as "yyyymmdd", by default current date}

\item{include_early_access}{logical, by default TRUE}

\item{beg_pubyear}{optional parameter to restrict criteria to include records after
a specific publication year}
}
\description{
This function allows for using the "load date" when fetching publications for
KTH - The Royal Institute of Technology. By default the time interval for the
previous two weeks is used.
}
\details{
Note: when using a subscriber API key, requests are only allowed from
institutional IPs. From Elsevier's API documentation:

"Elsevier Research Products APIs rely primarily on Institutional IP address
for authentication. API access through proxies is not supported, however
Elsevier will provide remote access direct to the APIs using a special
access credential ("Institutional Token").
If you are working away from your main institutional network
or your institution accesses Scopus.com, Scival.com, or ScienceDirect.com
through a proxy, please contact us to enquire about Institutional Token access."

The rate limits that apply for using Scopus Search is a max paging length of 25 for
complete views with a 5000 item total results limit and weekly 20k results and at
the most 9 requests per second.
}
