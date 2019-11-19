
blsAPI2 <- function(seriesid = NULL, startyear = 2018, endyear = startyear) {
  if(!require(tidyverse)) stop("This package requires 'tidyverse'")
  if(!require(blsAPI)) stop("This package requires blsAPI from github. Run `devtools::install_github('mikeasilva/blsAPI')`")

  if (Sys.getenv("BLS_API_KEY") != "") {
    key <- Sys.getenv("BLS_API_KEY")
  } else if (is.null(key)) {
    stop("A BLS API key is required.  Obtain one at https://data.bls.gov/registrationEngine/, and then supply the key to the `bls_api_key` function to use it throughout your session.")
  }

  year_range <- endyear - startyear
  if (year_range < 0) stop("End year is must be greater than or equal to startyear.", call. = FALSE)
  if (year_range >20) stop("BLS API has a 20 year maximum.", call. = FALSE)
  if (length(seriesid) > 50) stop("BLS API has a 50 series maximum.", call. = FALSE)

  payload <- list('seriesid' = seriesid,
                  'startyear' = startyear,
                  'endyear' = endyear,
                  'annualaverage' = TRUE,
                  'registrationkey' = key)
  response <- blsAPI(payload, 2, TRUE)

  response2 <- as_tibble(response) %>%
    arrange(year, period) %>%
    mutate(year = as.numeric(year), value = as.numeric(value)) %>%
    select(-period) %>%
    pivot_wider(names_from = "periodName", values_from = "value")

  return(response2)
}








bls_api_key <- function (key, overwrite = FALSE, install = FALSE) {

  # Borrowing this code straight from tidycensus package.
  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if (file.exists(renv)) {
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }
    if (!file.exists(renv)) {
      file.create(renv)
    }
    else {
      if (isTRUE(overwrite)) {
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv = read.table(renv, stringsAsFactors = FALSE)
        newenv <- oldenv[-grep("BLS_API_KEY", oldenv),
                         ]
        write.table(newenv, renv, quote = FALSE, sep = "\n",
                    col.names = FALSE, row.names = FALSE)
      }
      else {
        tv <- readLines(renv)
        if (any(grepl("BLS_API_KEY", tv))) {
          stop("A BLS_API_KEY already exists. You can overwrite it with the argument overwrite=TRUE", call. = FALSE)
        }
      }
    }
    keyconcat <- paste0("BLS_API_KEY='", key, "'")
    write(keyconcat, renv, sep = "\n", append = TRUE)
    message("Your API key has been stored in your .Renviron and can be accessed by Sys.getenv(\"BLS_API_KEY\"). \nTo use now, restart R or run `readRenviron(\"~/.Renviron\")`")
    return(key)
  }
  else {
    message("To install your API key for use in future sessions, run this function with `install = TRUE`.")
    Sys.setenv(BLS_API_KEY = key)
  }
}
