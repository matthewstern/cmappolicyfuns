

print_update <- function(geog, source, survey, year, race){
  cat("Getting data: ", survey, " | ", str_pad(source, 8, side="right"), " | ", str_pad(geog, 9, side="right"), "| ", year, " | ", race, "\n")
}

count_suppressed <- function(df){
  # create a new dataframe without Margins of error
  df_estimates <- select(df, ends_with("E"))

  ## return a dataframe with a new column in which the counts of estimates (not MOEs) with NA values is returned
  add_column(df, SUPPRESSED = apply(df_estimates, MARGIN = 1, function(x) sum(is.na(x))), .before = 0)
}

assemble_peer_acs <- function(table = NULL, variables = NULL,  # Use either singular `table` or vector of `variables`
                              years = 2018,                    # Required. can be a vector of years.
                              peers = NULL,                    # Pass in Peer MSA table. Requires variables `GEOID` and `NAME_short`.
                              racial = FALSE,                  # = TRUE for iterating through racial tables or variables
                              race = NULL,                     # Only use to attempt to override the default value in output's race column
                              try_suppressed = TRUE,           # = FALSE to not attempt replacing suppressed 1year data with 5year. (when 5 year doesn't exist)
                              avg_weight = NULL,               # If null, counties will SUM for regional total. Specify a weight variable to trigger weighted average.
                              racecode = NULL,                 # Used internally only, for racial iterations when racial = TRUE
                              state_fips = "17", counties = c("031", "043", "089", "093", "097", "111", "197")) # Can be overridden for non-CMAP use
{

  # PART 1: Check for valid inputs ---------------------------------------------

  if(!require(tidyverse) | !require(tidycensus)) stop("This package requires 'tidyverse' and 'tidycensus'.")

  # determine table or variables
  if (is.null(table) && is.null(variables)){
    stop("Either a table or vector of variables must be supplied", call. = FALSE)
  }
  if (!is.null(table) && !is.null(variables)){
    stop("Both a table and a vector of variables cannot be supplied. Provide one or the other", call. = FALSE)
  }

  # define mode, name descriptive data source for print-back, and select last character(s) for racial check
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

  # if user did not pass peer MSA table, look for `PEERS` in outer scope. Accept it, error if needed, or use sample peer list as needed.
  if(is.null(peers)){
    if(exists("PEERS")){
      if(is_tibble(PEERS)){
        peers <- PEERS
        message(" User did not pass variable `peers` but `PEERS` is defined in global environment. Using `PEERS`.")
      }else{
        stop("`user did not pass variable `peers`. Function attempted to use `PEERS` from global environment but it is not a tibble.", call. = FALSE)
      }
    }else{
      peers <- default_peers
      message( "User did not pass variable `peers` so using default peers list.")
      #peers <- tribble(
      #  ~GEOID, ~NAME_short,
      #  16980, "Chicago (MSA)",
      #  14460, "Boston",
      #  35620, "New York",
      #  47900, "Washington, D.C.")
      #message(" User did not pass variable `peers` so using Chicago, Boston, NYC, and DC by default.")
    }
  }

  # confirm correct variables exist in peers tibble
  if(!("GEOID" %in% names(peers)) | !("NAME_short" %in% names(peers))){
    stop("`peers` must be a tibble containing variables `GEOID` and `NAME_short`.", call. = FALSE)
  }

  # PART 2: Create internal data -----------------------------------------------
  # create empty datasets
  data <- tibble()
  suppressed_counties <- tibble()

  races <- tribble(
    ~CODE, ~RACE,
    "B", "Black or African-American",
    "D", "Asian",
    "H", "White (non-hispanic)",
    "I", "Hispanic or Latino")

  peers_geoids <- c(peers$GEOID)



  # PART 3: In racial mode, iterate through function repeatedly -----------------
  if(racial){

    for(racecode in races$CODE){
      # if in table mode, adjust table name for each race while not passing variables
      if(mode == "table"){
        table2 <- paste0(table,racecode)
        variables2 <- NULL

        # if in variable mode, adjust variable names for each race while not passing table
      }else if(mode == "variables"){
        variables2 <- sub(x=variables,pattern = "_", replacement = paste0(racecode, "_"))
        #LOCS_TO_INSERT <- str_locate(variables,"_")[,1]
        table2 <- NULL

        # if mode is something else, error and stop.
      }else stop("Error: unknown mode", call. = FALSE)

      # run function for this race
      race_specific_data <- assemble_peer_acs(table = table2, variables = variables2, racial = FALSE, # these attributes have been modified
                                              race = race, years = years, racecode = racecode, try_suppressed = try_suppressed, # these have not been modified
                                              avg_weight = avg_weight, state_fips = state_fips, counties = counties, peers = peers) # these have not been modified

      # Remove race code from variable names so that join aligns
      names(race_specific_data) <- gsub(x = names(race_specific_data), pattern = paste0(racecode, "_"), replacement = "_")

      # add this data to the master set
      data <- bind_rows(data, race_specific_data)
    }
    return(data)
  }

  # PART 4: best descriptive race variable ------------------------------

  # if this is a racial iteration, look up correct race value
  if(!is.null(racecode)){
    if(!is.null(race)){
      message(paste0(" User inputted `race`='",race,"' but race appears to be '", races$RACE[match(racecode, races$CODE)], "'. User input overridden."))
    }
    race = races$RACE[match(racecode, races$CODE)]

    # if not a racial iteration, and in variable mode...
  }else if(mode == "variables"){
    # if user did not specify race, put in placeholder.
    if(is.null(race)){
      message(" It is not known whether the called variables are race-specific. `Race`='see var name'. Consider replacing.")
      race = "see var name"

      # if user did specify race, accept their value.
    }else{
      message(paste0(" Accepting user input of `race`=", race,"."))
    }

    # if not a racial iteration, and in table mode...
  }else if(mode == "table"){

    # identify last character in table name
    last_char <- substr(table, nchar(table), nchar(table))

    # check if last character of table is a race (must be A through I):
    if (last_char %in% LETTERS[1:9]){

      # if last character is a race known to this function, look up the correct race
      if(last_char %in% races$CODE){
        # but display an error if user also tried to input a race
        if(!is.null(race)){
          message(paste0(" User inputted `race`='",race,"' but race appears to be '", races$RACE[match(last_char, races$CODE)], "'. User input overridden."))
        }

        race = races$RACE[match(last_char, races$CODE)]

        # if last character is a race unknown to this function and user did not pass race variable, put in placeholder.
      }else if(is.null(race)){
        message(paste0(" Table appears to have race code '", last_char, "' which is not known to this function. `Race`='see var name'. Consider replacing."))
        race = "see var name"

        # if last character is a race unknown to this function and user DID pass race variable, accept it.
      }else{
        message(paste0(" Table appears to have race code '", last_char, "' which is not known to this function. `Race`='",race,"'."))
      }

      # if last character of table is NOT a race:
    }else{
      # and user did not enter a race:
      if(is.null(race)){
        race = "All"

        # if user entered a race, accept it.
      }else{
        message(paste0(" Table does not appear to be a racial table, but user inputted `race`=",race,". User input accepted."))
      }
    }
  }else{stop("Error in function mode detection.", call. = FALSE)}


  # PART 5: Look up 1 year ACS data --------------------------------------------
  for(year in years){
    # get CMAP7 county data
    print_update("County", source, "acs1", year, race)
    raw_7co <- suppressMessages(get_acs(geography="county", table=table, variables = variables, survey="acs1", year=year, output="wide",
                                        state=state_fips, county=counties, cache_table=TRUE)) %>%
      mutate(SURVEY = "acs1", GEO = "county")

    # Shorten county names
    raw_7co$NAME <- gsub(x = raw_7co$NAME, pattern = " County, Illinois", replacement = "")

    # get peer MSA data
    print_update("MSA", source, "acs1", year, race)
    raw_msa <- suppressMessages(get_acs(geography="metropolitan statistical area/micropolitan statistical area",
                                        table=table, variables = variables, survey="acs1", year=year, output="wide", cache_table=TRUE)) %>%
      filter(GEOID %in% peers_geoids) %>%
      merge(peers, by="GEOID") %>%
      mutate(SURVEY = "acs1", GEO="MSA") %>%
      select(-NAME) %>%
      select(NAME=NAME_short, everything())

    # get national data
    print_update("National", source, "acs1", year, race)
    raw_us <- suppressMessages(get_acs(geography="us", table=table, variables = variables, survey="acs1", year=year, output="wide", cache_table=TRUE) %>%
                                 mutate(SURVEY = "acs1", GEO="nation"))

    # combine data
    raw <- bind_rows(raw_7co, raw_msa, raw_us) %>%
      mutate(YEAR = year, RACE = race) %>%
      select(YEAR, SURVEY, GEO, GEOID, RACE, everything())

    # count suppressed data
    raw <- count_suppressed(raw)

    # Identify suppressed geographies
    suppressed_geogs <- select(filter(raw, SUPPRESSED>0), GEOID, GEO, NAME)

    # if there are suppressed geogs, report which
    if(nrow(suppressed_geogs)>0){
      message(paste(" Geogs with suppressed data:", paste(suppressed_geogs$NAME, collapse=", ")))

      # add counties to master suppressed counties list
      suppressed_counties <- suppressed_geogs %>%
        filter(GEO == "county") %>%
        select(GEOID, NAME) %>%
        bind_rows(suppressed_counties) %>%
        distinct()

    }
    # add data to primary dataset
    data <- bind_rows(data, raw)
  }

  # PART 6: Replace suppressed with 5 year ACS data ----------------------------------
  if(nrow(suppressed_counties)>0 && try_suppressed == TRUE){
    message(paste(" Attempting to replace suppressed data:", paste0(suppressed_counties$NAME, collapse = ", "), "..."))

    # make a tibble of three digit county FIPS codes:
    suppressed_counties_list <- transmute(suppressed_counties, GEOID_SHORT = substr(GEOID, 3, 5))
    # and convert it to a list
    suppressed_counties_list <- suppressed_counties_list$GEOID_SHORT

    # create empty dataset for ACS5 values
    was_suppressed <- tibble()

    for(year in years){
      # print update
      print_update("County", source, "acs5", year, race)

      # get ACS5 data
      raw_was_suppressed <- suppressMessages(get_acs(geography="county", table=table, variables = variables, survey="acs5", year=year, output="wide",
                                                     state=state_fips, county=suppressed_counties_list, cache_table=TRUE)) %>%
        mutate(SURVEY = "acs5", GEO = "county", YEAR = year, RACE = race) %>%
        select(YEAR, SURVEY, GEO, GEOID, RACE, everything())

      # Shorten county names
      raw_was_suppressed$NAME <- gsub(x = raw_was_suppressed$NAME, pattern = " County, Illinois", replacement = "")

      # add it to master ACS5 table
      was_suppressed <- bind_rows(was_suppressed, raw_was_suppressed)
    }

    # check for suppressed values in 5 year data
    was_suppressed <- count_suppressed(was_suppressed)

    # add 5 year data to main data
    data <- bind_rows(data, was_suppressed)
  }

  # PART 7: Summarize multi-county region ----------------------------------

  # add E for estimate onto end of weight variable specified by user
  avg_weight2 <- paste0(avg_weight,"E")

  # if this is a racial iteration, insert race code as well:
  if (!is.null(racecode)){
    avg_weight2 <- sub(x = avg_weight2, pattern = "_", replacement = paste0(racecode,"_"))
  }

  # calculate correct type of sum
  if (is.null(avg_weight)){
    summary_type <- "sum"
    message(" Calculating regional total using sum.")
  }else{
    # check to see if variable specified exists in data
    if(avg_weight2 %in% names(data)) {
      summary_type <- "weighted.mean"
      message(paste0(" Calculating regional total using mean weighted on ", avg_weight,"."))
    }else{
      message(" `avg_weight` not a column in data. Returning data without row for regional totals.")
      return(data)
    }
  }

  # create sub-list of the correct rows to total, preferring ACS5 rows where they exist
  counties_to_total <- filter(data, GEO == "county") %>%
    group_by(YEAR) %>%
    arrange(desc(SURVEY), .by_group = TRUE) %>% # brings acs5 years to top
    distinct(NAME, .keep_all = TRUE) # %>% # drops acs1 years where acs5 years exist

  # create a string of surveys used for each summary
  surveys_used <- ungroup(counties_to_total) %>%
    distinct(SURVEY) %>%
    summarise(SURVEY = paste(SURVEY, collapse=",")) %>%
    as.character()

  # identify how many counties are being used to generate regional total
  # (should be all in region unless get_suppressed = FALSE and some data are suppressed)
  counties_counted <- summarise(counties_to_total, count = sum(SUPPRESSED == 0)) %>%
    select(count) %>%
    min() %>%
    as.character()

  # calculate the totals based on the summary type selected
  if(summary_type == "sum"){
    # generate totals row using summarize at
    totals_rows <- select(counties_to_total, -SUPPRESSED, -GEO, -GEOID, -NAME, -RACE) %>%
      summarize_at(vars(ends_with("E")), sum, na.rm = TRUE) %>%
      mutate(SURVEY = surveys_used, GEO = "region", NAME = "Chicago (CMAP7)", RACE = race, COUNT = counties_counted, GEOID = "0")
  }else{
    # generate weighted averages
    totals_rows <- select(counties_to_total, -SUPPRESSED, -GEO, -GEOID, -NAME, -RACE) %>%
      summarize_at(vars(ends_with("E")), funs(weighted.mean(., w=UQ(rlang::sym(avg_weight2)))), na.rm = TRUE) %>%   # https://stackoverflow.com/questions/26003574/use-dynamic-variable-names-in-dplyr
      mutate(SURVEY = surveys_used, GEO = "region", NAME = "Chicago (CMAP7)", RACE = race, COUNT = counties_counted, GEOID = "0") %>%
      # the above code created a mean of the weight variable, weighted on itself. This is confusing and unnecessary. Drop it.
      select(-!!avg_weight2)
  }


  # add the total row
  data <- bind_rows(data, totals_rows)

  # organize data
  data <- select(data, YEAR, GEO, GEOID, NAME, RACE, SURVEY, COUNT, SUPPRESSED, everything()) %>%
    arrange(YEAR, GEO)

  # PART 8: Return -------------------------------------------------------
  return(data)
}
