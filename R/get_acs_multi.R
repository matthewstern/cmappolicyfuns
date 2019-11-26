
get_acs_multi <- function(geography, variables = NULL, table = NULL,
                          cache_table = FALSE, years = 2017, racial = FALSE, endyear = NULL,
                          output = "wide", state = NULL, county = NULL, summary_var = NULL,
                          key = NULL, moe_level = 90, survey = "acs5", ...){

  if(!require(tidyverse) | !require(tidycensus)) stop("This package requires 'tidyverse' and 'tidycensus'.")

  data <- tibble()

  # determine table or variables
  if (is.null(table) && is.null(variables)){
    stop("Either a table or vector of variables must be supplied", call. = FALSE)
  }
  if (!is.null(table) && !is.null(variables)){
    stop("Both a table and a vector of variables cannot be supplied. Provide one or the other", call. = FALSE)
  }

  # select correct subset of races
  if (is.logical(racial)){
    if(racial){
      # if racial == TRUE, use all races except all
      races <- slice(race_iterations, 2:10)
    } else {
      # if racial == FALSE, use only all
      races <- slice(race_iterations, 1)
    }
  } else if(is.character(racial)){
    # If a racial == [some string], subset possible race iterations using the string
    races <- filter(race_iterations, str_detect(racial, CODE))
    if (nrow(races)==0) stop("If `racial` is a string, it must contain Census race specifiers [A:I] or '_' for non-racial.")
  } else stop("`racial` must be a logical or a string", call. = FALSE)

  # identify mode
  if (!is.null(table)) mode <- "table" else mode <- "variables"

  # for each race
  for(i in 1:nrow(races)){

    # identify variables
    THIS_RACE_CODE <- races$CODE[i]
    THIS_RACE_NAME <- races$RACE[i]

    # if in ALL RACES, do not modify inputs
    if(THIS_RACE_NAME == "All"){
      table2 <- table
      variables2 <- variables
      summary_var2 <- summary_var

    # if in some other race, modify inputs to add racial character
    } else {
      if(mode == "table"){
        table2 <- paste0(table, THIS_RACE_CODE)
        variables2 <- NULL
      }else{
        variables2 <- sub(x=variables,pattern = "_", replacement = paste0(THIS_RACE_CODE, "_"))
        table2 <- NULL
      }
      if(length(summary_var)){
        summary_var2 <- sub(x=summary_var,pattern = "_", replacement = paste0(THIS_RACE_CODE, "_"))
      } else summary_var2 <- NULL
    }

    # create `source` for print-back
    if(mode == "table"){
      source <- table2
    }else if(length(variables)<5) {
      source <- paste(variables2, collapse = ", ")
    } else {
      source <- paste0(paste(variables2[1:3], collapse = ", "),", ...")
    }
    if(length(summary_var2)) source <- paste(source, summary_var2, sep = ", ")

    # for each year
    for(THIS_YEAR in years){
      # print update
      cat(" Getting data: ", survey, " | ", str_pad(source, 8, side="right"),  " | ", THIS_YEAR, " | ", THIS_RACE_NAME, "\n")

      # get data
      data_this <- suppressMessages(get_acs(geography = geography, variables = variables2, table = table2,
                                            cache_table = cache_table, year = THIS_YEAR, endyear = endyear,
                                            output = output, state = state, county = county, summary_var = summary_var2,
                                            key = key, moe_level = moe_level, survey = survey, ...)) %>%
        mutate(YEAR = THIS_YEAR, RACE = THIS_RACE_NAME, SURVEY = survey,
               GEO = case_when(
                 geography == "metropolitan statistical area/micropolitan statistical area" ~ "MSA",
                 geography == "us" ~ "nation",
                 TRUE ~ geography)) %>%
        select(YEAR, RACE, SURVEY, GEO, everything())

      # Remove race code from variable names so that join aligns
      if (THIS_RACE_NAME != "All"){
        names(data_this) <- gsub(x = names(data_this), pattern = paste0(THIS_RACE_CODE, "_"), replacement = "_")
      }

      # add data to master
      data <- bind_rows(data, data_this)
    }

  }

  return(data)
}


get_acs_local <- function(table = NULL, years = 2018, racial = FALSE, survey = "acs1", path = NULL, peers = default_peers){

  data <- tibble()

  # select correct subset of races
  if (is.logical(racial)){
    if(racial){
      # if racial == TRUE, use all races except all
      races <- slice(race_iterations, 2:10)
    } else {
      # if racial == FALSE, use only all
      races <- slice(race_iterations, 1)
    }
  } else if(is.character(racial)){
    # If a racial == [some string], subset possible race iterations using the string
    races <- filter(race_iterations, str_detect(racial, CODE))
    if (nrow(races)==0) stop("If `racial` is a string, it must contain Census race specifiers [A:I] or '_' for non-racial.")
  } else stop("`racial` must be a logical or a string", call. = FALSE)

  # for each race
  for(i in 1:nrow(races)){

    # identify variables
    THIS_RACE_CODE <- races$CODE[i]
    THIS_RACE_NAME <- races$RACE[i]

    # append race code if needed
    if(THIS_RACE_NAME == "All"){
      table2 <- table
    } else {
      table2 <- paste0(table, THIS_RACE_CODE)
    }

    # for each year
    for(THIS_YEAR in years){
      # print update
      cat(" Getting data: ", survey, " | ", str_pad(table2, 8, side="right"),  " | ", THIS_YEAR, " | ", THIS_RACE_NAME, "\n")

      # identify survey type
      subname <- case_when(
        year == 2006 ~ "EST",   # 2006 data only existed as 1 year, and file names just identify "EST".
        survey == "acs5" ~ "5YR",
        survey == "acs3" ~ "3YR",
        survey == "acs1" ~ "1YR"
      )

      # load in local data
      raw <- suppressWarnings(read_csv(
        file = paste0(path,"ACS_", substr(as.character(year),3,4),"_", subname, "_",table2,"_with_ann.csv"),
        skip = 1,
        col_types = cols(
          .default = col_double(),
          Id = col_character(),
          Geography = col_character()
        )
      ))

      # modify variable names
      for(i in 1:length(names(raw))) {
        name <- names(raw)[i]

        # move "Estimate" and "Margin of Error" to end
        if(substr(name,1,10)=="Estimate; "){
          name <- sub("Estimate; ","", name)
          name <- paste0(name, "_E")
        }
        if(substr(name,1,17)=="Margin of Error; "){
          name <- sub("Margin of Error; ","", name)
          name <- paste0(name, "_M")
        }
        # remove excess punctuation and spaces
        name <- gsub(": - "," ", name)
        name <- gsub(":","", name)

        # address inconsistencies in labeling over time
        name <- gsub(", GED, or alternative", " (includes equivalency)", name)

        # replace name
        names(raw)[i] <- name
      }

      # adjust to conform with API data
      raw_improved <- mutate(raw,
                    YEAR = THIS_YEAR,
                    SURVEY = survey,
                    GEO = case_when(
                      Geography == "United States" ~ "nation",
                      str_sub(Geography, -10,-1) == "Metro Area" ~ "MSA",
                      grepl("County", Geography) ~ "county"),
                    RACE = THIS_RACE_NAME
      ) %>%
        filter(Id2 %in% peers$GEOID | GEO == "nation" | GEO == "county") %>%
        left_join(peers, by=c("Id2"="GEOID")) %>%
        mutate(
          NAME = case_when(
            GEO == "MSA" ~ NAME_short,
            GEO == "nation" ~ "United States",
            GEO == "county" ~ Geography
          ),
          GEOID = case_when(
            GEO == "nation" ~ "1",
            TRUE ~ as.character(Id2)
          )
        ) %>%
        select(YEAR, RACE, SURVEY, GEO, GEOID, NAME,everything(), -Id, -Id2, -Geography, -NAME_short)

      data <- bind_rows(data, raw_improved)
    }
  }

  return(data)

}



data_api <- get_acs_multi("county", table = "B01001", years = 2013, racial = "BDHI", state = "17", county = c("031", "043", "089", "093", "097", "111", "197"), survey = "acs1", output = "wide" )
data_local <- get_acs_local(table = "B01001", year = 2006, racial = "BDHI", path = "S:/Projects_FY20/Policy Development/Regional Economic Indicators/DataManagement/ACS_19_pull/pre-2010 tables from factfinder/")
data_reg <- get_acs("county", table = "B01001", year = 2013, state ="17", county =c("031", "043", "089", "093", "097", "111", "197"), survey = "acs1", output = "wide")
names <- tibble(api = names(data_api), local = names(data_local))


assemble_peer_acs2 <- function(table = NULL, variables = NULL,  # Use either singular `table` or vector of `variables`
                               years_API = NULL,                # Years to pull from API. Can be a vector.
                               years_local = NULL,              # years to pull from local dir. Can be a vector.
                               racial = FALSE,                  # = TRUE all racial iterations. ="[string of chars]" for specific iterations
                               peers = NULL,                    # Pass in Peer MSA table. Requires variables `GEOID` and `NAME_short`.
                               try_suppressed = TRUE,           # = FALSE to not attempt replacing suppressed 1year data with 5year. (when 5 year doesn't exist)
                               summary_var = NULL,              # If null, counties will SUM for regional total. Specify a weight variable to trigger weighted average.
                               local_dir = NULL,                # local directory for files not yet on data.census.gov
                               state_fips = "17", counties = c("031", "043", "089", "093", "097", "111", "197")) # Can be overridden for non-CMAP use
{

  #### check for valid inputs

  if(!require(tidyverse) | !require(tidycensus)) stop("This package requires 'tidyverse' and 'tidycensus'.")

  # determine table or variables
  if (is.null(table) && is.null(variables)){
    stop("Either a table or vector of variables must be supplied", call. = FALSE)
  }
  if (!is.null(table) && !is.null(variables)){
    stop("Both a table and a vector of variables cannot be supplied. Provide one or the other", call. = FALSE)
  }


  # if user did not pass peer MSA table, use default peers list.
  if(is.null(peers)){
      peers <- default_peers
      message( "User did not pass variable `peers` so using default peers list.")
  }

  # confirm correct variables exist in peers tibble
  if(!("GEOID" %in% names(peers)) | !("NAME_short" %in% names(peers))){
    stop("`peers` must be a tibble containing variables `GEOID` and `NAME_short`.", call. = FALSE)
  }

  raw_api <- tibble()
  raw_local <- tibble()


  if(length(years_API)){
    message("Fetching data from API.")

    # get national data
    cat("National...\n")
    raw_us <- get_acs_multi(geography = "us", table=table, variables=variables, cache_table = TRUE, years = years_API,
                            racial = racial, survey = "acs1", summary_var = summary_var)

    # get MSA data
    cat("MSA...\n")
    raw_msa <- get_acs_multi(geography = "metropolitan statistical area/micropolitan statistical area",
                             table=table, variables=variables, cache_table = TRUE, years = years_API,
                             racial = racial, survey = "acs1", summary_var = summary_var) %>%
                filter(GEOID %in% peers$GEOID) %>%
                merge(peers, by="GEOID") %>%
                select(-NAME) %>%
                select(NAME = NAME_short, everything())

    cat("Counties...\n")
    raw_county <- get_acs_multi(geography = "county", table=table, variables=variables, cache_table = TRUE, years = years_API,
                                racial = racial, state=state_fips, county=counties, survey = "acs1", summary_var = summary_var)

    raw <- bind_rows(raw_us, raw_msa, raw_county) %>%
      count_suppressed()
  }

  # get local data
  if(length(years_local)){
    message("Fetching local tables downloaded from factfinder.")

    # errors
    if (is.null(table)) stop("You must specify a table for local pull.", call. = FALSE)
    if (is.null(local_dir)) stop("You must specify a local directory that contains downoaded tables.", call. = FALSE)

    # get data
    raw_local <- get_acs_local(table = table, years = years_local, racial = racial, survey = "acs1", path = local_dir, peers = peers) %>%
      count_suppressed()

    # if data was also pulled from API...
    if(length(names(raw))){
      # modify variable names to match API names
      message("Opening a tibble to verify names of local vs API variables.")
      View(tibble(local = names(raw_local), API = names(raw)))
      names(raw_local) <- names(raw)

      # and add local info to API info
      raw <- bind_rows(raw, raw_local)
    }
  }

  # clean up county names
  raw$NAME <-gsub(x = raw$NAME, pattern = " County, Illinois", replacement = "")

  # identify suppressed counties
  suppressed_counties <- raw %>%
    filter(GEO == "county", SUPPRESSED>0) %>%
    select(GEOID, NAME) %>%
    distinct()

  # print suppressed counties
  if (nrow(suppressed_counties) > 0) message(paste(" Geogs with suppressed data:", paste(suppressed_counties$NAME, collapse=", ")))

  # look up 5 year data for suppressed counties
  if (nrow(suppressed_counties) > 0 | try_suppressed = TRUE){
    # recursive here?
  }

  # summarize county data

  # clean up data

  # at some point, clean up county names:


  return(raw)

}

count_suppressed <- function(df){
  # create a new dataframe without Margins of error
  df_estimates <- select(df, ends_with("E"))

  ## return a dataframe with a new column in which the counts of estimates (not MOEs) with NA values is returned
  add_column(df, SUPPRESSED = apply(df_estimates, MARGIN = 1, function(x) sum(is.na(x))), .before = 0)
}


data <- assemble_peer_acs2(table = "B19013", years_local = 2009, years_API = 2013 ,racial = "BDHI", local_dir = path)
