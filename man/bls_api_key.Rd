\name{bls_api_key}
\alias{bls_api_key}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Install a BLS API Key in Your .Renviron File for Repeated Use
}
\description{
This function will add your BLS API key to your .Renviron file so it can be called securely without being stored in your code. After you have installed your key, it can be called any time by typing Sys.getenv("BLS_API_KEY") If you already have an .Renviron file, the function will append the key to your existing file, while making a backup of your original file for disaster recovery purposes.

This is a direct copy of the \code{census_api_key} function from tidycensus. Thanks to the authors of that package!
}
\usage{
bls_api_key(key, overwrite = FALSE, install = FALSE)
}
\arguments{
  \item{overwrite}{
If this is set to TRUE, it will overwrite an existing CENSUS_API_KEY that you already have in your \code{.Renviron} file. Defaults to false.
}
  \item{install}{
if TRUE, will install the key in your \code{.Renviron} file for use in future sessions. Defaults to FALSE.
}
}
\details{
}
\value{
}
\references{

}
\author{

}
\note{
}
\examples{
bls_api_key("YOUR KEY HERE", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")
}

