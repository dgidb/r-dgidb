% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dgidb.R
\name{get_sources}
\alias{get_sources}
\title{Get Sources}
\usage{
get_sources(source_type = NULL, api_url = NULL)
}
\arguments{
\item{source_type}{type of source to look up. Fetches all sources otherwise, Default: NULL}

\item{api_url}{API endpoint for GraphQL request, Default: NULL}
}
\value{
all sources of relevant type in a json object
}
\description{
Perform a source lookup for relevant aggregate sources
}
\examples{
source_type <- list(
  DRUG = "drug",
  GENE = "gene",
  INTERACTION = "interaction",
  POTENTIALLY_DRUGGABLE = "potentially_druggable"
)
sources <- get_sources(source_type$POTENTIALLY_DRUGGABLE)
}
