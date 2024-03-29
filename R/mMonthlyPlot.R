#' Plot streamflow or rainfall monthly average graph
#'
#' @encoding UTF-8
#'
#' @description Plot streamflow or rainfall monthly average to help users identify hydrological year
#'
#' @param organizeResult list, tibble data frame; provides a list containing
#'   the data frames of raw records for each station downloaded from ANA web API
#'   (output from [hydrobr::organize()] function).
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
#' # Filter the data for desired period and quality control
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


  # ## Verification if arguments are in the desired format
  # # is stationsDataResult an outcome from stationsData function?
  # if (!attributes(organizeResult)$hydrobr_class %in% 'organize') {
  #   stop(
  #     call. = FALSE,
  #     '`organizeResult` does not inherit attribute "organize".
  #      The outcome from the organize() function should be passed as argument'
  #   )
  # }

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
  if (any(names(organizeResult[[1]]) %in% "streamflow_m3_s")) {
    stationType <- "flu"
  } else {
    stationType <- "plu"
  }

  varName <- switch (stationType, plu = 'rainfall_mm', flu = 'streamflow_m3_s')

  ##
  # Single workflow regardless of station type
  organizeResultDF <- organizeResult %>%
    # Join all stations into single df
    dplyr::bind_rows() %>%
    # Rename streamflow/precipitation variable
    dplyr::rename('value' = varName)


  # Create civil and water year columns
  organizeResultDF <- organizeResultDF %>%
    dplyr::mutate(
      # Retrieve year from date
      civilYear = lubridate::year(.data$date)
    )

  # Are only consisted data to be considered?
  if (consistedOnly == TRUE) {
    organizeResultDF <- organizeResultDF %>%
      # Instead of filtering, we change them to NA, so we know where there was inconsistent data
      dplyr::mutate(value = dplyr::if_else(.data$consistency_level == 2, .data$value, NA_real_))
  }

  # Identify civil years with less than `maxMissing` years
  failuredf <- organizeResultDF %>%
    # Group by station and civilYear
    dplyr::group_by_at(c('station_code', 'civilYear')) %>%
    # Number of observations in each group
    dplyr::summarise(
      N = dplyr::if_else(
        length(.data$value) < 365,
        365,
        length(.data$value) %>% as.double()),
      # Percentage of missing data
      missing = 100*sum(is.na(.data$value))/.data$N,
      .groups = "drop_last") %>%
    # Adjust missing to logical based on `maxMissing`
    dplyr::mutate(missing = .data$missing <= maxMissing)

  # Organize plot data
  plotData <- organizeResultDF %>%
    dplyr::ungroup() %>%
    # Join with failure matrix
    dplyr::left_join(failuredf, by = c("station_code", "civilYear")) %>%
    #filter year with less that "minporc"
    dplyr::filter(missing == TRUE) %>%
    dplyr::group_by_at('station_code') %>%
    #count number of years of each station and verify if there is all months (1:12)
    dplyr::mutate(lengthYears = length(unique(.data$civilYear)),
                  lengthMonths = length(unique(lubridate::month(.data$date)))) %>%
    #stations with minimal year and all months
    dplyr::filter(.data$lengthYears >= minYears & .data$lengthMonths >= 12)

  plot <- plotData %>%
    # Compute civil month
    dplyr::mutate(monthCY = factor(lubridate::month(date), levels = 1:12)) %>%
    # Group by station and month
    dplyr::group_by_at(c("station_code", "monthCY")) %>%
    # Calc average for streamflow, sum for monthly precipitation
    dplyr::summarise(
      value = if (varName == "streamflow_m3_s")
        {mean(.data$value, na.rm = TRUE)} else
          {sum(.data$value, na.rm = TRUE)/length(unique(.data$civilYear))}, .groups = 'drop') %>%
    # Plot
    ggplot2::ggplot()+
    ggplot2::aes(x = .data$monthCY,
                 y = .data$value) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(ggplot2::vars(station_code), scale = "free")+
    ggplot2::theme_bw(base_size = 12)+
    ggplot2::labs(x = "Month",
                  y = if (varName == "streamflow_m3_s")
                    {expression(paste("Average streamflow (m"^3," s"^-1,")"))} else
                      {expression(paste("Average rainfall (mm month"^-1,")"))})

  # Plot!
  return(plot)
}


# if(getRversion() >= "2.15.1")  utils::globalVariables(c("value", "monthCY"))
