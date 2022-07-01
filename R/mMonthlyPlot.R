#' Plot streamflow or rainfall monthly average graph
#'
#' @encoding UTF-8
#'
#' @description Plot streamflow or rainfall monthly average to help users to identify hydrological year
#'
#' @param organizeResult list, tibble data frame; provides a list containing
#'   the data frames of raw records for each station downloaded from ANA web API
#'   (output from [hydrobr::stationsData()] function).
#' @param maxMissing numeric; indicates the maximum threshold of missing data allowed at each year.
#'   The default is 10 percent.
#' @param minYears numeric; indicates the minimum years of complete data allowed. The
#'   default is 15 years.
#' @param consistedOnly logical; should only consisted data be considered?
#'   The default is TRUE.
#'
#' @return Saved plot.
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
#' # Filter the data for desired period and quality contorl
#'
#' mMonthlyPlot(
#'   stationsDataResult = org_data,
#'   maxMissing = 10,
#'   minYears = 15,
#'   consistedOnly = FALSE
#' )
#'
#' @export
#' @importFrom rlang .data
mMonthlyPlot = function(organizeResult,
                        maxMissing = 10,
                        minYears = 15,
                        consistedOnly = TRUE){

  wtr_yr <- function(dates, start_month=9) {
    # Convert dates into POSIXlt
    dates.posix = as.POSIXlt(dates)
    # Year offset
    offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
    # Water year
    adj.year = dates.posix$year + 1899 + offset
    # Return the water year
    adj.year
  }

  ## Verification if arguments are in the desired format
  # is stationsDataResult an outcome from stationsData function?
  if (!attributes(organizeResult)$hydrobr_class %in% 'organize') {
    stop(
      call. = FALSE,
      '`organizeResult` does not inherit attribute "organize".
       The outcome from the organize() function should be passed as argument'
    )
  }

  # Is maxMissing numeric
  if (!is.numeric(maxMissing) | length(maxMissing) != 1) {
    stop(
      call. = FALSE,
      '`maxMissing` should be a numeric vector of length == 1 (ex: 10).
       See arguments details for more information.'
    )
  }

  # Is minYears numeric
  if (!is.numeric(minYears) | length(minYears) != 1) {
    stop(
      call. = FALSE,
      '`minYears` should be a numeric vector of length == 1 (ex: 15).
       See arguments details for more information.'
    )
  }

  # is consistedOnly logical?
  if (!is.logical(consistedOnly) | length(consistedOnly) != 1) {
    stop(
      call. = FALSE,
      "`consistedOnly` should be logical and have length == 1 (either TRUE or FALSE)."
    )
  }

  #################


  # Type of station?
  if (names(organizeResult[[1]])[4] == "streamflow_m3_s") {
    stationType <- "flu"
  } else {
    stationType <- "plu"
  }
  varName <- switch (stationType, plu = 'rainfall_mm', flu = 'stream_flow_m3_s')

  ##
  # Single workflow regardless of station type
  organizeResultDF <- organizeResult %>%
    # Join all stations into single df
    do.call(what = dplyr::bind_rows) %>%
    # Rename streamflow/precipitation variable
    dplyr::rename('value' = 4)

  # Are only consisted data to be considered?
  if (consistedOnly == TRUE) {
    organizeResultDF <- organizeResultDF %>%
      # Instead of filtering, we change them to NA, so we know where there was unconsisted data
      dplyr::mutate(value = dplyr::if_else(.data$consistency_level == 2, .data$value, NA_real_))
  }


  # Create df based on beginning and end of water year
  selectDataResult <- split(organizeResultDF, organizeResultDF$station_code) %>%
    # For each station, create a sequence of dates for waterYear/monthWaterYear
    purrr::map(.f = function(df){
      # Create tibble with sequence of dates
      dplyr::tibble(date = seq(
        from = paste0(min(lubridate::year(df$date)),
                      stringr::str_pad(1, side = 'left', pad = 0, width = 2),
                      '01') %>% as.Date(.data, format = '%Y%m%d'),
        to   = paste0(max(lubridate::year(df$date))+1,
                      stringr::str_pad(1, side = 'left', pad = 0, width = 2),
                      '01') %>% as.Date(.data, format = '%Y%m%d'),
        by = 1
      )) %>%
        # Add civil/water year columns
        dplyr::mutate(
          civilYear      = lubridate::year(.data$date),
          monthCivilYear = paste(stringr::str_pad(lubridate::month(.data$date), side = 'left', pad = 0, width = 2),
                                 lubridate::year(.data$date),
                                 sep = "-"),
          waterYear      =  wtr_yr(.data$date, start_month = 1) %>% as.character() %>% as.numeric(),
          monthWaterYear = paste(stringr::str_pad(lubridate::month(.data$date), side = 'left', pad = 0, width = 2),
                                 .data$waterYear, sep = "-")
        ) %>%
        dplyr::left_join(df, by = 'date') %>%
        # Fill station_code column
        dplyr::mutate(station_code = unique(df$station_code))
    }) %>%
    # Join all stations into single df
    do.call(what = dplyr::bind_rows)


  ## annual failure matrix
  options(warn =-1)
    # Creating failureMatrix
    failureMatrix <- selectDataResult %>%
      # Group by station and wateryear
      dplyr::group_by_at(c('station_code', 'waterYear')) %>%
      # Check percentage of missing data
      dplyr::mutate(missing = 100*sum(is.na(.data$value))/dplyr::n()) %>%
      # Select waterYear, station, and % missing
      dplyr::select(
        dplyr::matches('^waterYear$'),
        dplyr::matches('station_code'),
        dplyr::matches('missing')
      ) %>%
      # Filter by maxMissing
      dplyr::mutate(missing = .data$missing <= maxMissing) %>%
      dplyr::distinct() %>%
      # Filter by minYears
      dplyr::group_by_at('station_code') %>%
      dplyr::filter(sum(.data$missing == T) >= minYears) %>%
      # Spread each station into a column
      tidyr::pivot_wider(names_from = .data$station_code, values_from = .data$missing)

  ##select completed years, evaluate month average and plot graph
    plot <- selectDataResult %>%
      # Join with failure matrix
      dplyr::left_join(
        failureMatrix %>%
          tidyr::pivot_longer(cols = 2:ncol(failureMatrix), names_to = 'station_code', values_to = 'keep')
      ) %>%
      # Filter data to keep
      dplyr::filter(.data$keep == T) %>%
      # add month
      dplyr::mutate(monthCY = as.factor(lubridate::month(date))) %>%
      dplyr::group_by_at(c("station_code", "monthCY")) %>%
      dplyr::summarise(value = if (varName == "stream_flow_m3_s") {mean(value, na.rm = TRUE)} else {sum(value, na.rm = TRUE)/length(unique(.data$civilYear))}, .groups = 'drop') %>%
      ggplot2::ggplot()+
      ggplot2::aes(monthCY, value, group = 1)+
      ggplot2::facet_wrap(ggplot2::vars(station_code), scale = "free")+
      ggplot2::geom_col() +
      ggplot2::theme_bw(base_size = 14)+
      ggplot2::labs(x = "Month",
           y = if (varName == "stream_flow_m3_s") {"Average Streamflow (m3_s)"} else {"Average Rainfall (mm)"})



  return(plot)
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("value", "monthCY"))
