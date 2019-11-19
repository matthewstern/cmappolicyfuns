\name{blsAPI2}
\alias{blsAPI2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fetch BLS time series data
}
\description{
This function wraps around the \code{blsAPI()} function from https://github.com/mikeasilva/blsAPI to allow for simple access to the BLS v2 API (which requires registration). It simplifies the use of that function by requiring fewer arguments be specified, including looking for a BLS API key in Renviron. It also spreads the API results into a relatively tidy output.
}
\usage{
blsAPI2(seriesid = NULL, startyear = 2018, endyear = startyear)
}
\arguments{
  \item{seriesid}{
The BLS series to pull. This can be one or many (specified in a vector). Per API documentation, max is 20.
}
  \item{startyear}{
The first year of data to pull.
}
  \item{endyear}{
The last year of data to pull. The API supports a max range of 20 years per query. By default, this equals \code{startyear}, so if unspecified function will return 1 year.
}
}
\details{
Requires packages \code{tidyverse} and \code{blsAPI}. Also requires a BLS API key, which can be installed via:
\code{bls_api_key("your key here", install=TRUE, overwrite=TRUE)
readRenviron("~/.Renviron")}
}
\value{
A tibble.

}
\references{

}
\author{

}
\note{
}
\examples{
CPI_raw <- blsAPI2(c("CUURS23ASA0", "CUURS12ASA0"), 2000, 2018)
}

