\name{assemble_peer_acs}
\alias{assemble_peer_acs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Assemble ACS data for region and peers
}
\description{
This function wraps around the tidycensus \code{get_acs()} function to quietly collect 1-year ACS data for a set of counties, a set of peer MSAs, and the country. It collects data for a range of years, and if asked can iterate through racially-specific detailed census tables. It tracks suppressed data, and attempts to replace suppressed 1-year data with data from the 5-year ACS. Finally, it sums or calculates a weighted average for the set of counties. It defaults to CMAP's 7 county region, but this can be overridden for use by other regions.
}
\usage{
assemble_peer_acs(table = NULL, variables = NULL, years = 2018,
  peers = NULL, racial = FALSE, race = NULL, try_suppressed = TRUE,
  avg_weight = NULL, state_fips = "17",
  counties = c("031", "043", "089", "093", "097", "111", "197")))
}
\arguments{
  \item{table}{
The ACS table to pull. This can be a regular (e.g. \code{"B23001"}) or race-specific (e.g. \code{"B23002H"}) table, but if \code{racial=TRUE} input the generic version of the race-specific table (e.g. \code{"B23002"}). If specified, user cannot also specity \code{variables}.
}
  \item{variables}{
The ACS variable(s) to pull. This can be one (e.g. \code{"B23001_001"}) or multiple (e.g. \code{c("B23001_001", "B23001_002")}) variables. As with \code{table}, variables from race-specific tables can be called, but remove their race-specific suffix if \code{racial=TRUE}. If specified, user cannot also specity \code{table}.
}
  \item{years}{
The year (e.g. \code{2015}) or years (e.g. \code{c(2013, 2018)}, \code{2013:2018}) to pull.
}
  \item{peers}{
A tibble representing the list of MSAs to pull. If left as \code{NULL}, function will default to a list of 35 peer regions defined in tibble \code{peeracs::default_peers}. The user can also create a tibble in the global environment called \code{PEERS}. In other words, \code{peeracs::default_peers} is overridden by \code{PEERS} which is overriden by \code{peers}. This tibble must have a numeric variable \code{GEOID} with 5-digit FIPS codes representing metropolitan statistical areas, and a character variable \code{NAME_short} with a concise name for each MSA. 
}
  \item{racial}{
Set to \code{TRUE} if the table is racially-specific or variables are from racially-specific tables. If \code{TRUE} this function will recurse using the codes \code{c(B, D, H, I)} to collect data from 'Black or African-American', 'Asian', 'White(non-hispanic)', and 'Hispanic or Latino' tables, respectively. Note that this introduces a small amount of mutual exclusivity error.
}
  \item{race}{
Typically left as null. Can be used to attempt to overwrite the character value stored in the variable \code{RACE} of the output tibble. Use if function returns "see var name" in \code{RACE}.
}
  \item{try_suppressed}{
If \code{TRUE} (the default) the function will attempt to replace any counties with suppressed data with 5-year ACS data. Where \code{years} is a range, if a county is suppressed >0 years, all years will be replaced with 5 year data. Set to \code{FALSE} if 5 year ACS data does not exist any of the tables, variables, or years in question.
}
  \item{avg_weight}{
This function adds a summary row for data pulled at the county level. If \code{NULL} (the default) the function will sum. If a variable is specified (e.g. \code{"B00101_001"}) the specified variable will be used to construct a weighted average instead. Use when the variables in question are themselves summaries rather than counts. The specified variable must already be collected via \code{table} or \code{variables}.
}
  \item{state_fips}{
Defaults to \code{"17"} for Illinois unless overridden.
}
  \item{counties}{
A vector of 3 digit FIPS codes representing counties. Defaults to the 7 county CMAP region unless overridden.
}
  \item{racecode}{
Ignore this variable. It is used in the recursive process when \code{racial=TRUE}.
}
}
\details{
Requires packages \code{tidyverse} and \code{tidycensus}. Also requires a census key, which can be installed via:
\code{census_api_key("your key here", install=TRUE, overwrite=TRUE) 
readRenviron("~/.Renviron")}
}
\value{
A tibble.

The output includes rows for \code{GEOG = c("county", "MSA", "country")}. County rows are repeated by \code{SURVEY = c("acs1", "acs5")} if suppressed 1-year data was replaced with 5-year data. A summary row (\code{GEOG = "region"}) includes either sums or weighted averages for all variables. Note that 5-year data is used in calulating the summary row for ALL years for a given county if it is suppressed for even one year. The above rows are repeated for each \code{YEAR} and each \code{RACE} if \code{racial = TRUE}.

The output includes columns for all specified variables, and their margins of error. The variable \code{SUPPRESSED} represents the number of variables with suppressed data for the given geography and year. \code{COUNT} is only populated for the regional summary row, and represents the number of non-suppressed counties feeding into the summary. If \code{try_suppressed = TRUE}, \code{COUNT} should equal \code{len(counties)} but \code{SURVEY} may equal "acs1,acs5" indicating some 5-year data was used. If \code{try_suppressed = FALSE}, \code{COUNT} may be less than the number of counties.
}
\references{

}
\author{

}
\note{
Any ACS table or vintage available on data.census.gov should be available through tidyverse and this function. As of tidycensus v0.9, there is an artificial limitation that prevents accessing 2010 and 2011 ACS data. This can be removed by deleting lines 45 to 48 in \code{tidycensus::get_acs}. You can edit the function with the command \code{trace("get_acs", edit=TRUE)}.
}
\examples{
# The table B23001 for range of years
data <- assemble_peer_acs("B23001", years = 2014:2017)

# the racially-specific table B23002 for just one year
assemble_peer_acs("B23002", years = 2015, racial=TRUE, try_suppressed = FALSE)

# a selection of variables for a range of years
data <- assemble_peer_acs(variables = c("B23001_001", "B23001_002", "B23001_003"),
                                        years = 2014:2017)

# a selection of variables for a range of years, by race, with a weighted average
data <- assemble_peer_acs(variables = c("B19013_001", "B01001_001"),
                                        years = 2014:2015, racial = TRUE,
                                        avg_weight = "B01001_001")
}

