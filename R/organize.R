#' Organizes the raw data provided by ANA for its stations
#'
#' @encoding UTF-8
#'
#' @description Takes as input a list containing data frames of raw records
#'   for each station (output from [hydrobr::stationsData()]) and organizes
#'   them into tidy data frames. As some dates may be provided with different
#'   consistency levels, priority is given to consisted data (consistency level = 2)
#'
#' @param stationsDataResult list, tibble data frame; provides a list containing
#'   the data frames of raw records for each station downloaded from ANA web API
#'    (output from [hydrobr::stationsData()] function).
#'
#' @return A list containing an organized data frame [tibble::tibble()] object
#'    for each station. The data frames will contain the following columns:
#'    station_code: station unique identifier (chr).
#'    consistency_level: data consistency level (1 = raw, 2 = consisted) (chr).
#'    date: date format YYYY-MM-DD (Date).
#'    rainfall_mm/streamflow_m3_s: rain/streamflow gauge measurement (dbl).
#'
#' @examplesIf interactive()
#' # Fech a inventory of fluviometric stations for the state of Minas Gerais
#'
#' inv <- inventory(
#'   states = "MINAS GERAIS",
#'   stationType = "flu",
#'   as_sf = TRUE,
#'   aoi = NULL
#' )
#'
#' # Download the first 10 stations from the inventory
#'
#' s_data <- stationsData(
#'   inventoryResult = inv[1:10,],
#'   deleteNAstations = TRUE
#' )
#'
#' # Organize the data for the stations
#'
#' org_data <- organize(
#'   stationsDataResult = s_data
#' )
#'
#' @export
#'
#' @importFrom rlang .data
organize <- function(stationsDataResult) {
  
  # ## Verification if arguments are in the desired format
  # # is stationsDataResult an outcome from stationsData function?
  # if (!attributes(stationsDataResult)$hydrobr_class %in% 'stationsData') {
  #   stop(
  #     call. = FALSE,
  #     '`stationsDataResult` does not inherit attribute "stationsData".
  #     The outcome from the stationsData() function should be passed as argument'
  #   )
  # }
  
  ##
  # Single workflow regardless of station type
  stationsDataResult <- stationsDataResult[stationsDataResult != 'No Data'] %>%
    # Join all stations into single df
    do.call(what = dplyr::bind_rows) %>%
    # Select desired columns
    dplyr::select(
      dplyr::contains('estacaocodigo'),
      dplyr::contains('nivelconsistencia'),
      dplyr::matches('data$'),
      dplyr::matches("vazao..$"),
      dplyr::matches("chuva..$"),
      dplyr::matches("cota..$")
    ) %>%
    # Rename from pt to en
    dplyr::rename(
      'station_code' = 1,
      'consistency_level' = 2
    ) %>%
    # Group by station id
    dplyr::group_by_at('station_code') %>%
    # Make single column for flow/precipitation data
    tidyr::pivot_longer(cols = 4:34) %>%
    # Fix date columns
    dplyr::mutate(date = .data$data + as.numeric(stringr::str_extract(.data$name, pattern = "[0-9]+")) - 1) %>%
    # Remove duplicates by selecting highest consistency level
    dplyr::group_by_at(c('station_code', 'date')) %>%
    dplyr::filter(
      .data$consistency_level == max(.data$consistency_level),
      # Other duplicates may appear because more columns than days for certain months
      lubridate::month(.data$date) == lubridate::month(.data$data)
    ) %>%
    # If there is still a duplicate row, we select the first one
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    # Select desired columns
    dplyr::select(
      dplyr::matches('station_code$'),
      dplyr::matches('consistency_level$'),
      dplyr::matches('date$'),
      dplyr::matches('name$'),
      dplyr::matches('value$')
    ) %>%
    # Organize by station code and date
    dplyr::arrange(dplyr::across(c('station_code', 'date')))
  
  # Rename column for rainfall or streamflow
  if (stringr::str_remove(stationsDataResult$name[1], pattern = "[0-9]+") == "chuva") {
    stationsDataResult <- stationsDataResult %>%
      dplyr::rename(rainfall_mm = 5)
  } else {if (stringr::str_remove(stationsDataResult$name[1], pattern = "[0-9]+") == "vazao") {
    stationsDataResult <- stationsDataResult %>%
      dplyr::rename(streamflow_m3_s = 5)
  }  else {
    stationsDataResult <- stationsDataResult %>%
      dplyr::rename(level_cm = 5)
  }
    
  }
  
  # Output format
  organizedResult <- stationsDataResult %>%
    # Remove name column
    dplyr::select(-'name') %>%
    # Filter stations with data
    dplyr::group_by_at('station_code') %>%
    dplyr::filter(dplyr::n() > 0) %>%
    # Transform into lists with tibble for each station
    dplyr::ungroup()
  
  # Split into lists
  organizedResult <- split(organizedResult, organizedResult$station_code)
  
  #Pad data to complete dates. Set consistency level = 2 to missing data. Will be computed as NA in selectStation
  
  organizedResult <- lapply(organizedResult, FUN = function(x) padr::pad(x,
                                                                         start_val = as.Date(paste(lubridate::year(dplyr::first(x$date)),
                                                                                                   01, 01, sep = "-")),
                                                                         end_val = as.Date(paste(lubridate::year(dplyr::last(x$date)),
                                                                                                 12, 31, sep = "-"))) %>% dplyr::mutate(station_code = unique(x$station_code)) %>%
                              dplyr::mutate(consistency_level = ifelse(is.na(consistency_level),
                                                                       2, consistency_level))) %>%
    suppressMessages()
  # attr(organizedResult, "hydrobr_class") <- "organize"
  return(organizedResult)
}

if(getRversion() >= "2.15.1")  utils::globalVariables("consistency_level")
