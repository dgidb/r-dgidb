% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dgidb.R
\name{post_query}
\alias{post_query}
\title{Post Query}
\usage{
post_query(api_url, query_file, variables)
}
\arguments{
\item{api_url}{API endpoint for GraphQL request}

\item{query_file}{path to GraphQL query file}

\item{variables}{variables to be passed to GraphQL query}
}
\value{
data from GraphQL API response
}
\description{
Sends a POST request to DGIdb GraphQL API with the specified paramters
}
