% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_endpoint.R
\name{create_endpoint}
\alias{create_endpoint}
\title{Create an endpoint for fetching consumption data from Octopus Energy API}
\usage{
create_endpoint(mpan, serial)
}
\arguments{
\item{mpan}{A string representing the MPAN (Meter Point Administration Number).}

\item{serial}{A string representing the meter serial number.}
}
\value{
A string representing the complete URL endpoint for fetching consumption data.
}
\description{
Create an endpoint for fetching consumption data from Octopus Energy API
}
\details{
The function constructs a URL that can be used to fetch consumption data for a specific electricity meter point.
The URL includes parameters for pagination and a specific time period.
}
\examples{
# create_endpoint("1234567890123", "ABC123456789")
}
