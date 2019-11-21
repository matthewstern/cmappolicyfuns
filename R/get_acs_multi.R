
get_acs_multi <- function(geography, variables = NULL, table = NULL,
                          cache_table = FALSE, years = 2017, racial = FALSE, endyear = NULL,
                          output = "tidy", state = NULL, county = NULL, summary_var = NULL,
                          key = NULL, moe_level = 90, survey = "acs5", ...){

  if(!require(tidyverse) | !require(tidycensus)) stop("This package requires 'tidyverse' and 'tidycensus'.")

  data <- tibble()

  # identify mode
  if (!is.null(table)){
    mode <- "table"
    source <- table
  }else{
    mode <- "variables"
    if(length(variables)<6) {
      source <- paste(variables, collapse = ", ")
    } else{
      source <- paste0(paste(variables[1:5], collapse = ", "),"...")
    }
  }

  # select correct subset of races
  if (is.logical(racial)){
    if(racial){
      # if racial == TRUE, use all race codes except all
      races <- slice(race_iterations, 2:10)
    } else {
      # if racial == FALSE, use only all
      races <- slice(race_iterations, 1)
    }
  } else if(is.character(racial)){
    # use subset of race iterations specified by string
    races <- filter(race_iterations, str_detect(racial, CODE))
  }

  # for each race
  for(i in 1:nrow(races)){

    # identify variables
    THIS_RACE_CODE <- races$CODE[i]
    THIS_RACE_NAME <- races$RACE[i]

    # if in ALL RACES, do not modify
    if(THIS_RACE_NAME == "All"){
      table2 <- table
      variables2 <- variables
      # if in some other race, modify input
    } else {
      if(mode == "table"){
        table2 <- paste0(table, THIS_RACE_CODE)
        variables2 <- NULL
      }
      if(mode == "variables"){
        variables2 <- sub(x=variables,pattern = "_", replacement = paste0(THIS_RACE_CODE, "_"))
        table2 <- NULL
      }
    }

    # for each year
    for(THIS_YEAR in years){
      # print update
      cat("Getting data: ", survey, " | ", str_pad(source, 8, side="right"),  " | ", THIS_YEAR, " | ", THIS_RACE_NAME, "\n")

      # get data
      data_this <- suppressMessages(get_acs(geography = geography, variables = variables2, table = table2,
                                            cache_table = cache_table, year = THIS_YEAR, endyear = endyear,
                                            output = output, state = state, county = county, summary_var = summary_var,
                                            key = key, moe_level = moe_level, survey = survey, ...)) %>%
        mutate(YEAR = THIS_YEAR, RACE = THIS_RACE_NAME) %>%
        select(YEAR, RACE, everything())

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

