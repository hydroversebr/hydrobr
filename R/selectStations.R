#' Summarizes and filter the stations (organized) data by month or year
#'
#' @encoding UTF-8
#'
#' @description Takes as input a list containing data frames of organized records
#'   for each station (output from [hydrobr::organize()]) and (i) filters the time
#'   series within a range of years, (ii) filters out months or years exceeding
#'   the maximum threshold of missing data, and (iii) filters out stations
#'   with less than a minimum years of complete observations.
#'
#' @param organizeResult list, tibble data frame; provides a list containing
#'   the data frames of raw records for each station downloaded from ANA web API
#'   (output from [hydrobr::stationsData()] function).
#' @param mode character; indicates in which scale to check missing data, 'monthly'
#'   or 'yearly'. The default is 'yearly'.
#' @param maxMissing numeric; indicates the maximum threshold of missing data allowed.
#'   The default is 10 percent.
#' @param minYears numeric; indicates the minimum years of complete data allowed. The
#'   default is 15 years.
#' @param month numeric; indicates the month when the water year begins. The default is
#'   1 (use civil year).
#' @param iniYear numeric; filters the time series to begin on this year (inclusive).
#'   If you choose to use water year instead of civil year, e.g., month = 6,
#'   the first observation used is from the date "01-06-`iniYear`".
#'   The default is NULL (use entire period).
#' @param finYear numeric; filters the time series to end on this year (inclusive).
#'   If you choose to use water year instead of civil year, e.g., month = 6,
#'   the last observation used is from the date "31-05-`finYear`".
#'   The default is NULL (use entire period).
#' @param consistedOnly logical; should only consisted data be considered?
#'   The default is FALSE.
#' @param plot logical; plot the figure? The default is TRUE. The figure is saved
#'   regardless.
#'
#' @return A list containing 4 objects:
#'   * a list containing the data frames [tibble::tibble()] for each station after
#'   removing periods exceeding `maxMissing` and filtering out stations which
#'   observational period is shorter than `minYears`.
#'   * a failureMatrix indicating if the period exceeds the threshold of `maxMissing`
#'   data and columns only  for stations with at least `minYears` of complete
#'   observational data
#'   * a missingMatrix indicating the percentage of missing data and columns only
#'   for stations with at least `minYears` of complete observational data
#'   * the saved plot.
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
#' org_data <- organize(stationsDataResult = s_data)
#'
#' # Filter the data for desired period and quality contorl
#'
#' final_data <- selectStations(
#'   organizeResult = org_data,
#'   mode = "yearly",
#'   maxMissing = 10,
#'   minYears = 15,
#'   month = 1,
#'   iniYear = NULL,
#'   finYear = NULL,
#'   consistedOnly = FALSE,
#'   plot = TRUE
#' )
#'
#' @export
#'
#' @importFrom rlang .data
#'

selectStations <- function(organizeResult,
                           mode = "yearly",
                           maxMissing = 10,
                           minYears = 15,
                           month = 1,
                           iniYear = NULL,
                           finYear = NULL,
                           consistedOnly = FALSE,
                           plot = TRUE) {


  # For the failureMatrix, NA represents no data, FALSE represents that the %
  #  of missing data exceeded the threshold `maxMissing`, and TRUE represents
  #  that the % of missing data did not exceed the threshold.
  # For the missingMatrix, NA represents no data, and numeric values represent
  #  the % of missing data.


  # ## Verification if arguments are in the desired format
  # # is stationsDataResult an outcome from stationsData function?
  # if (!attributes(organizeResult)$hydrobr_class %in% 'organize') {
  #   stop(
  #     call. = FALSE,
  #     '`organizeResult` does not inherit attribute "organize".
  #      The outcome from the organize() function should be passed as argument'
  #   )
  # }

  # Is mode a character vector?
  if (!is.character(mode) | length(mode) != 1) {
    stop(
      call. = FALSE,
      '`mode` should be a character vector of length == 1 (either "yearly" or "monthly").
       See arguments details for more information.'
    )
  }

  # Is mode "yearly" or "monthly"?
  if (!mode %in% c('yearly', 'monthly')) {
    stop(
      call. = FALSE,
      '`mode` should be a character vector of length == 1 (either "yearly" or "monthly").
       See arguments details for more information.'
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

  # Is month numeric
  if (!is.numeric(month) | length(month) != 1) {
    stop(
      call. = FALSE,
      '`month` should be a numeric vector of length == 1.
       `month` should represent a month of the year, e.g., 1 (January).
       See arguments details for more information.'
    )
  }

  # Is iniYear numeric
  if (!is.null(iniYear)){
    if (!is.numeric(iniYear) | length(iniYear) != 1) {
      stop(
        call. = FALSE,
        '`iniYear` should be a numeric vector of length == 1 (ex: 1990).
         See arguments details for more information.'
      )
    }
  }

  # Is finYear numeric
  if (!is.null(finYear)){
    if (!is.numeric(finYear) | length(finYear) != 1) {
      stop(
        call. = FALSE,
        '`finYear` should be a numeric vector of length == 1 (ex: 2020).
         See arguments details for more information.'
      )
    }
  }

  # is consistedOnly logical?
  if (!is.logical(consistedOnly) | length(consistedOnly) != 1) {
    stop(
      call. = FALSE,
      "`consistedOnly` should be logical and have length == 1 (either TRUE or FALSE)."
    )
  }

  # Type of station?
  # if (names(organizeResult[[1]])[4] == "streamflow_m3_s") {
  #   stationType <- "flu"
  # } else {
  #   stationType <- "plu"
  # }
  varName <- names(organizeResult[[1]])[4]


  ##first date to be considered based on month

  #identify first date

  firstDates = lapply(organizeResult, FUN = function(y) y %>%
                        dplyr::pull(date) %>%
                        dplyr::first() %>%
                        base::gsub(x = ., pattern = "-01-",paste("-", month, "-", sep = "")) %>%
                        base::as.Date())

  #Filter first date

  organizeResult = lapply(1:length(organizeResult), FUN = function(i)

    organizeResult[[i]] %>%
      dplyr::filter(date>=firstDates[i])

  )

  ##
  # Single workflow regardless of station type
  organizeResultDF <- organizeResult %>%
    # Join all stations into single df
    dplyr::bind_rows() %>%
    # Rename streamflow/precipitation variable
    dplyr::rename('value' = 4)

  # Create civil and water year columns
  organizeResultDF <- organizeResultDF %>%
    dplyr::mutate(
      # Retrieve year from date
      civilYear      = lubridate::year(.data$date),
      # Retrieve year-month from date
      monthCivilYear = .data$date - (lubridate::day(.data$date) - 1),
      # Calculate water year
      waterYear      = lubridate::year(.data$date %>%
                                         lubridate::add_with_rollback(months(-(month - 1)))),
      # same as monthCivilYear but replace civilYear by waterYear
      monthWaterYear = as.Date(paste0(.data$waterYear, substr(.data$monthCivilYear, 5, 10)))
    )

  # Are only consisted data to be considered?
  if (consistedOnly == TRUE) {
    organizeResultDF <- organizeResultDF %>%
      # Instead of filtering, we change them to NA, so we know where there was unconsisted data
      dplyr::mutate(value = dplyr::if_else(.data$consistency_level == 2, .data$value, NA_real_))
  }

  # Filter time series based on initial and final year arguments

  selectDataResult <- organizeResultDF %>%
    # Group by station to find initial and final date (important is ini/finYear = NULL)
    dplyr::group_by_at('station_code') %>%
    # Create reference initial and final date for each station
    dplyr::mutate(
      # initial date
      initialDate = dplyr::if_else(
        # In case iniYear is NULL
        is.null(iniYear),
        # T > Use first year of the series and provided month
        paste0(min(lubridate::year(.data$date)),
               '-',
               month,
               '-01'),
        # F > Use iniYear and month
        paste0(iniYear,
               '-',
               month,
               '-01')),

      # final date
      finalDate = dplyr::if_else(
        # In case finYear is NULL
        is.null(finYear),
        # T > Use final year of the series and last day of month - 1
        paste0(max(lubridate::year(.data$date)),
               '-',
               if ((month - 1) == 0) 12 else (month - 1),
               '-',
               lubridate::days_in_month(if ((month - 1) == 0) 12 else (month - 1))),
        # F > Use finYear and month - 1
        paste0(finYear, '-',
               if ((month - 1) == 0) 12 else (month - 1), '-',
               lubridate::days_in_month(if ((month - 1) == 0) 12 else (month - 1))))
    ) %>%
    # Filter dates within initial and final date for each station
    dplyr::filter(.data$date >= as.Date(.data$initialDate),
                  .data$date <= as.Date(.data$finalDate)) %>%
    # Remove initialDate and finalDate columns
    dplyr::select(-dplyr::matches('lDate'))

  ## Is mode yearly or monthly??
  # Remove warning from using discrete variable in alpha for plot
  options(warn =-1)
  if (mode == 'yearly') {

    # Creating failureMatrix
    failureMatrix <- selectDataResult %>%
      # Group by station and wateryear
      dplyr::group_by_at(c('station_code', 'waterYear')) %>%
      # Get lenght of year in days
      dplyr::mutate(N = dplyr::if_else(length(.data$value) < 365,
                                       365,
                                       length(.data$value) %>% as.double())) %>%
      # Check percentage of missing data
      dplyr::mutate(missing = 100*sum(is.na(.data$value))/.data$N) %>%
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

    # Creating missingMatrix
    missingMatrix <- selectDataResult %>%
      # Group by station and wateryear
      dplyr::group_by_at(c('station_code', 'waterYear')) %>%
      # Get lenght of year in days
      dplyr::mutate(N = dplyr::if_else(length(.data$value) < 365,
                                       365,
                                       length(.data$value) %>% as.double())) %>%
      # Check percentage of missing data
      dplyr::mutate(missing = 100*sum(is.na(.data$value))/.data$N) %>%
      # Select waterYear, station, and % missing
      dplyr::select(
        dplyr::matches('^waterYear$'),
        dplyr::matches('station_code'),
        dplyr::matches('missing')
      ) %>%
      dplyr::distinct() %>%
      # Filter by minYears
      dplyr::filter(.data$station_code %in% names(failureMatrix)[-1]) %>%
      # Spread each station into a column
      tidyr::pivot_wider(names_from = .data$station_code, values_from = .data$missing)

    # Creating plot with consistency level and missing data
    g <- selectDataResult %>%
      # Group by station and wateryear
      dplyr::group_by_at(c('station_code', 'waterYear')) %>%
      # Get lenght of year in days
      dplyr::mutate(N = dplyr::if_else(length(.data$value) < 365,
                                       365,
                                       length(.data$value) %>% as.double())) %>%
      # Check % of consisted and missing
      dplyr::summarise(consisted = 100*sum(.data$consistency_level == 2)/dplyr::n(),
                       missing   = 100*sum(is.na(.data$value))/.data$N,
                       .groups = 'drop') %>%
      dplyr::distinct() %>%
      # stations with at least minYears
      dplyr::filter(.data$station_code %in% names(failureMatrix)[-1]) %>%
      ggplot2::ggplot() +
      ggplot2::geom_tile(ggplot2::aes(x = .data$waterYear,
                                      y = .data$station_code,
                                      fill = .data$consisted == 100,
                                      alpha = .data$missing < maxMissing), color = 'black') +
      ggplot2::scale_fill_manual(name = 'consistency level',
                                 values = c("#00b0f6", '#f8766d'),
                                 breaks = c(T, F),
                                 labels = c('Consisted', 'Raw')) +
      ggplot2::scale_alpha_manual(name = '(transparency*)',
                                  breaks = c(F, T),
                                  values = c(0.4, 1),
                                  labels = c(paste0('>  ', maxMissing, '% missing'),
                                             paste0('<= ', maxMissing, '% missing'))) +
      ggplot2::theme_bw() +
      ggplot2::labs(caption = if (consistedOnly == T)
        '*transparency based on consisted data available' else
          '*transparency based on all data available')

  } else {

    # Station selection
    keepStations <- selectDataResult %>%
      # Group by station and wateryear
      dplyr::group_by_at(c('station_code', 'monthWaterYear')) %>%
      # Check percentage of missing data
      dplyr::mutate(missing = 100*sum(is.na(.data$value))/
                      lubridate::days_in_month(.data$monthWaterYear)) %>%
      # Select waterYear, station, and % missing
      dplyr::select(
        dplyr::matches('monthWaterYear'),
        dplyr::matches('station_code'),
        dplyr::matches('missing')
      ) %>%
      dplyr::distinct() %>%
      # Filter by maxMissing
      dplyr::filter(.data$missing <= maxMissing) %>%
      # n_months > minYears?
      dplyr::mutate(month = lubridate::month(monthWaterYear)) %>%
      dplyr::group_by_at(c('station_code', 'month')) %>%
      dplyr::select(
        dplyr::matches('station_code'),
        dplyr::matches('^month$')
      ) %>%
      dplyr::filter(dplyr::n() >= minYears) %>%
      dplyr::distinct() %>%
      # Which stations have all 12 months?
      dplyr::group_by_at('station_code') %>%
      dplyr::select(dplyr::matches('station_code')) %>%
      dplyr::filter(dplyr::n() == 12) %>%
      dplyr::distinct()

    # Creating failureMatrix
    failureMatrix <- selectDataResult %>%
      # Select stations in keepStations
      dplyr::filter(.data$station_code %in% keepStations$station_code) %>%
      # Group by station and wateryear
      dplyr::group_by_at(c('station_code', 'monthWaterYear')) %>%
      # Check percentage of missing data
      dplyr::mutate(missing = 100*sum(is.na(.data$value))/
                      lubridate::days_in_month(.data$monthWaterYear)) %>%
      # Select waterYear, station, and % missing
      dplyr::select(
        dplyr::matches('monthWaterYear'),
        dplyr::matches('station_code'),
        dplyr::matches('missing')
      ) %>%
      # Filter by maxMissing
      dplyr::mutate(missing = .data$missing <= maxMissing) %>%
      dplyr::distinct() %>%
      # Spread each station into a column
      tidyr::pivot_wider(names_from = .data$station_code, values_from = .data$missing)

    # Creating missingMatrix
    missingMatrix <- selectDataResult %>%
      # Select stations in keepStations
      dplyr::filter(.data$station_code %in% keepStations$station_code) %>%
      # Group by station and wateryear
      dplyr::group_by_at(c('station_code', 'monthWaterYear')) %>%
      # Check percentage of missing data
      dplyr::mutate(missing = 100*sum(is.na(.data$value))/
                      lubridate::days_in_month(.data$monthWaterYear)) %>%
      # Select waterYear, station, and % missing
      dplyr::select(
        dplyr::matches('monthWaterYear'),
        dplyr::matches('station_code'),
        dplyr::matches('missing')
      ) %>%
      dplyr::distinct() %>%
      # Spread each station into a column
      tidyr::pivot_wider(names_from = .data$station_code, values_from = .data$missing)

    # Creating plot with consistency level and missing data
    g <- selectDataResult %>%
      # stations with at least minYears
      dplyr::filter(.data$station_code %in% keepStations$station_code) %>%
      # Group by station code and monthWaterYear
      dplyr::group_by_at(c('station_code', 'monthWaterYear')) %>%
      # Check % of consisted and missing
      dplyr::summarise(consisted = 100*sum(.data$consistency_level == 2)/dplyr::n(),
                       missing   = 100*sum(is.na(.data$value))/
                         lubridate::days_in_month(.data$monthWaterYear),
                       .groups = 'drop') %>%
      dplyr::distinct() %>%
      ggplot2::ggplot() +
      ggplot2::geom_tile(ggplot2::aes(x = .data$monthWaterYear,
                                      y = .data$station_code,
                                      fill = .data$consisted == 100,
                                      alpha = .data$missing < maxMissing)) +
      ggplot2::scale_fill_manual(name = 'consistency level',
                                 values = c("#00b0f6", '#f8766d'),
                                 breaks = c(T, F),
                                 labels = c('Consisted', 'Raw')) +
      ggplot2::scale_alpha_manual(name = '(transparency)',
                                  breaks = c(F, T),
                                  values = c(0.4, 1),
                                  labels = c(paste0('>  ', maxMissing, '% missing'),
                                             paste0('<= ', maxMissing, '% missing'))) +
      ggplot2::scale_x_date(date_labels = '%m-%Y') +
      ggplot2::theme_bw() +
      ggplot2::labs(caption = if (consistedOnly == T)
        '*transparency based on consisted data available' else
          '*transparency based on all data available')
  }
  options(warn = 1)

  ##
  # Select waterYear or monthWateYear by station to keep
  selectDataResult <- selectDataResult %>%
    # Join with failure matrix
    dplyr::left_join(
      failureMatrix %>%
        tidyr::pivot_longer(cols = 2:ncol(failureMatrix), names_to = 'station_code', values_to = 'keep')
    ) %>%
    # Filter data to keep
    dplyr::filter(.data$keep == T) %>%
    # Select columns
    dplyr::select(
      dplyr::matches('station_code'),
      dplyr::matches('consistency_level'),
      dplyr::matches('^date$'),
      dplyr::matches('value'),
      dplyr::matches('^civilYear'),
      dplyr::matches('^monthCivilYear'),
      dplyr::matches('^waterYear'),
      dplyr::matches('^monthWaterYear')
    )

  # Create column indicating `maxMissing` allowed
  if (maxMissing == 0) {
    selectDataResult <- selectDataResult %>% dplyr::mutate(maxMissing = "0")
  } else {
    selectDataResult <- selectDataResult %>% dplyr::mutate(maxMissing = paste0("<= ", maxMissing, "%"))
  }

  # Fix name
  names(selectDataResult)[4] <- varName

  # Plot?
  if (plot == T) {print(g)}


  # Return

  #ajeitando umas coisas

  failureMatrix[is.na(failureMatrix)] = FALSE
  missingMatrix[is.na(failureMatrix)] = 100

  failureMatrix = failureMatrix %>%
    dplyr::select(dplyr::any_of(c(names(failureMatrix)[1], sort(names(failureMatrix))[-1])))

  missingMatrix %>%
    dplyr::select(dplyr::any_of(names(failureMatrix)))

  out <- list(split(selectDataResult, selectDataResult$station_code),
              failureMatrix,
              missingMatrix,
              g)
  names(out) <- c("series",
                  "failureMatrix",
                  "missingMatrix",
                  "plot")
  # class(out) <- c(class(out), 'selectData')
  return(out)
}
