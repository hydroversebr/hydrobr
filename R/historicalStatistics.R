#' (UNDER DEVELOPMENT) Compute flow statistics by month or year
#'
#' @encoding UTF-8
#'
#' @description Takes as input a list containing data frames of organized and filtered records
#'   for each station (output from [hydrobr::selectStations()]) and compute historical
#'   streamflow or rainfall statistics
#'
#' @param selectStationsResult list, tibble data frame; provides a list containing
#'   the data frames of filtered records for each station
#'   (output from [hydrobr::selectStations()] function).
#'
#' @param statistics character; indicates statistics.
#'  * The supported statistics for streamflow are:
#' (1) mean stream flow (Qmean); (2)  minimum of seven-day moving average of daily stream flow (Q7);
#' (3) stream flow associated with a percentage of time (Qperm); (4) maximum stream flow (Qmax);
#' and (5) minimum stream flow (Qmin).
#'  * The supported statistics are: (1) total rainfall (Rtotal); (2)  maximum rainfall (Rmax);
#' (3) rainy days (Rdays).
#'  * The default value is "Qmean".
#'
#' @param permanence numeric; percentage of time if "Qperm" is choose as statistic parameter
#'   The default is 95 percent.
#'
#' @param byMonth = logical. if byMounth = TRUE, historical statistics is performed for each month.
#'
#' @return A list containing 3 objects:
#'   * a list containing statistic a data frame [tibble::tibble()] object for each station.
#'   * a data frame [tibble::tibble()] with statistic of all stations in wide format
#'   * a data frame [tibble::tibble()] with statistic of all stations in longer format
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
#' final_data <- selectStations(
#'   stationsDataResult = org_data,
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
#' # annual mean stream flow serie for each station
#' Qmean_years = flowStatistics(final_data, statistics = "Qmean")
#'
#' @export
#' @importFrom rlang .data
#' @importFrom rlang :=


historicalStatistics = function(selectStationsResult,
                                statistics = "Qmean",
                                permanence = 95,
                                byMonth = FALSE)
{

  ## Verification if arguments are in the desired format
  # is selectStationsResult an outcome from selectStations?
  if (attributes(selectStationsResult)$class[2] %in% 'stationsData') {
    stop(
      call. = FALSE,
      '`inventoryResults` does not inherit attribute "inventory".
         The outcome from the inventory() function should be passed as argument'
    )
  }

  if (!is.character(statistics) | length(statistics) != 1 | !statistics %in% c("Qmean", "Qperm", "Q7", "Qmin", "Qmax", "Rtotal", "Rmax", "Rdays")) {
    stop(
      call. = FALSE,
      '`statistics` should be a character vector of length == 1 (either "Qmean", "Qperm", "Q7", "Qmin" or "Qmax").
       See arguments details for more information.'
    )
  }

  #if `selectStationsResult` and `statistics` belong same kind of data

  if (names(selectStationsResult[[1]][[1]])[4]=="stream_flow_m3_s") {

    if (!statistics %in% c("Qmean", "Qperm", "Q7", "Qmin", "Qmax")){

      stop(
        call. = FALSE,
        'for streamflow data from `selectStations` must choose streamflow `statistics` (either "Qmean", "Qperm", "Q7", "Qmin" or "Qmax").
       See arguments details for more information.'
      )
    }
  }

  #if `selectStationsResult` and `statistics` belong same kind of data

  if (names(selectStationsResult[[1]][[1]])[4]=="rainfall_mm") {

    if (!statistics %in% c("Rtotal", "Rmax", "Rdays")){

      stop(
        call. = FALSE,
        'for rainfall data from `selectStations` must choose rainfall `statistics` (either "Rtotal", "Rmax" or "Rdays").
       See arguments details for more information.'
      )
    }
  }

  #if "Qperm" is chosen, verify "permanence" parameter
  if (statistics=="Qperm" & (!is.numeric(permanence) | length(permanence) != 1 | permanence>100 | permanence < 0)) {
    stop(
      call. = FALSE,
      '`permanence` should be a numeric vector of length == 1 (ranging from 0 to 100).
       See arguments details for more information.'
    )
  }


  selectStationsResult <- selectStationsResult$series


  #byMonth = FALSE

  if (byMonth == FALSE){

    # compute Qmean and other statistics

  if (statistics == "Qmean") {

    series <- selectStationsResult %>%
      do.call(what = dplyr::bind_rows) %>%
      dplyr::group_by_at("station_code") %>%
      dplyr::summarise(
        station_code = unique(station_code),
        Qmean_m3_s = mean(stream_flow_m3_s, na.rm = TRUE),
        .groups = 'drop')

  } else if (statistics == "Q7") {

    series <- selectStationsResult %>%
      do.call(what = dplyr::bind_rows) %>%
      dplyr::group_by_at("station_code") %>%
      dplyr::summarise(
        station_code = unique(station_code),
        Q7_m3_s = min(zoo::rollapply(stats::na.omit(stream_flow_m3_s), 7, FUN = mean, partial = TRUE, align = "left")),
        .groups = 'drop')

  } else if (statistics == "Qperm") {

    series <- selectStationsResult %>%
      do.call(what = dplyr::bind_rows) %>%
      dplyr::group_by_at("station_code") %>%
      dplyr::summarise(
        station_code = unique(station_code),
        !!paste("Q", permanence, "_m3_s", sep = "") := stats::quantile(stream_flow_m3_s, 1 - permanence / 100, na.rm = TRUE),
        .groups = 'drop')

  } else if (statistics == "Qmin") {

    series <- selectStationsResult %>%
      do.call(what = dplyr::bind_rows) %>%
      dplyr::group_by_at("station_code") %>%
      dplyr::summarise(
        station_code = unique(station_code),
        Qmin_m3_s = min(stream_flow_m3_s, na.rm = TRUE),
        .groups = 'drop')

  } else if (statistics == "Qmax"){

    series <- selectStationsResult %>%
      do.call(what = dplyr::bind_rows) %>%
      dplyr::group_by_at("station_code") %>%
      dplyr::summarise(
        station_code = unique(station_code),
        Qmax_m3_s = max(stream_flow_m3_s, na.rm = TRUE),
        .groups = 'drop')

  } else if (statistics == "Rtotal") {


      series <- selectStationsResult %>%
        do.call(what = dplyr::bind_rows) %>%
        dplyr::group_by_at("station_code") %>%
        dplyr::summarise(
          station_code = unique(station_code),
          rainTotal_mm = sum(rainfall_mm, na.rm = TRUE)/length(unique(lubridate::year(date))),
          .groups = 'drop')


    } else if (statistics == "Rmax") {

      series <- selectStationsResult %>%
        do.call(what = dplyr::bind_rows) %>%
        dplyr::group_by_at("station_code") %>%
        dplyr::summarise(
          station_code = unique(station_code),
          rainMax_mm = max(rainfall_mm, na.rm = TRUE),
          .groups = 'drop')

    } else if (statistics == "Rdays") {

      series <- selectStationsResult %>%
        do.call(what = dplyr::bind_rows) %>%
        dplyr::group_by_at("station_code") %>%
        dplyr::summarise(
          station_code = unique(station_code),
          rainyDays = round(sum(rainfall_mm>0, na.rm = TRUE)/length(unique(lubridate::year(date))),0),
          .groups = 'drop')

    }


  #reorder results

      #reorder series based on date

    out = series %>%
      dplyr::arrange(station_code)
#
#     #series_matrix reordered
#
#     series_matrix = series %>%
#       dplyr::arrange(station_code) %>%
#       tidyr::pivot_wider(names_from = .data$station_code, values_from = 2)
#
#
#   out <- list(series = series %>%
#                 base::split(.$station_code),
#               df_series = series,
#               series_matrix = series_matrix)

  }

  if (byMonth == TRUE){

    #rename column names of fillGaps to execute functions

   if (statistics == "Qmean") {

    series <- selectStationsResult %>%
      do.call(what = dplyr::bind_rows) %>%
      dplyr::mutate(month = lubridate::month(date)) %>%
      dplyr::group_by_at(c("station_code", "month")) %>%
      dplyr::summarise(
        station_code = unique(station_code),
        Qmean_m3_s = mean(stream_flow_m3_s, na.rm = TRUE),
        .groups = 'drop')

    } else if (statistics == "Q7") {

      series <- selectStationsResult %>%
        do.call(what = dplyr::bind_rows) %>%
        dplyr::mutate(month = lubridate::month(date)) %>%
        dplyr::group_by_at(c("station_code", "month"))  %>%
        dplyr::summarise(
          station_code = unique(station_code),
          Q7_m3_s = min(zoo::rollapply(stats::na.omit(stream_flow_m3_s), 7, FUN = mean, partial = TRUE, align = "left")),
          .groups = 'drop')

    } else if (statistics == "Qperm") {

      series <- selectStationsResult %>%
        do.call(what = dplyr::bind_rows) %>%
        dplyr::mutate(month = lubridate::month(date)) %>%
        dplyr::group_by_at(c("station_code", "month")) %>%
        dplyr::summarise(
          station_code = unique(station_code),
          !!paste("Q", permanence, "_m3_s", sep = "") := stats::quantile(stream_flow_m3_s, 1 - permanence / 100, na.rm = TRUE),
          .groups = 'drop')

    } else if (statistics == "Qmin") {

      series <- selectStationsResult %>%
        do.call(what = dplyr::bind_rows) %>%
        dplyr::mutate(month = lubridate::month(date)) %>%
        dplyr::group_by_at(c("station_code", "month")) %>%
        dplyr::summarise(
          station_code = unique(station_code),
          Qmin_m3_s = min(stream_flow_m3_s, na.rm = TRUE),
          .groups = 'drop')

    } else if (statistics == "Qmax") {

      series <- selectStationsResult %>%
        do.call(what = dplyr::bind_rows) %>%
        dplyr::mutate(month = lubridate::month(date)) %>%
        dplyr::group_by_at(c("station_code", "month")) %>%
        dplyr::summarise(
          station_code = unique(station_code),
          Qmax_m3_s = max(stream_flow_m3_s, na.rm = TRUE),
          .groups = 'drop')

    } else if (statistics == "Rtotal") {

      series <- selectStationsResult %>%
        do.call(what = dplyr::bind_rows) %>%
        dplyr::mutate(month = lubridate::month(date)) %>%
        dplyr::group_by_at(c("station_code", "month")) %>%
        dplyr::summarise(
          station_code = unique(station_code),
          rainTotalAvarege_mm = sum(rainfall_mm, na.rm = TRUE)/(length(month)/lubridate::days_in_month(month)),
          .groups = 'drop') %>%
        dplyr::distinct()

    } else if (statistics == "Rmax") {

      series <- selectStationsResult %>%
        do.call(what = dplyr::bind_rows) %>%
        dplyr::mutate(month = lubridate::month(date)) %>%
        dplyr::group_by_at(c("station_code", "month")) %>%
        dplyr::summarise(
          station_code = unique(station_code),
          rainMax_mm = max(rainfall_mm, na.rm = TRUE),
          .groups = 'drop')

    } else if (statistics == "Rdays") {

      series <- selectStationsResult %>%
        do.call(what = dplyr::bind_rows) %>%
        dplyr::mutate(month = lubridate::month(date)) %>%
        dplyr::group_by_at(c("station_code", "month")) %>%
        dplyr::summarise(
          station_code = unique(station_code),
          rainyDays = round(sum(rainfall_mm>0, na.rm = TRUE)/(length(month)/lubridate::days_in_month(month)),0),
          .groups = 'drop') %>%
        dplyr::distinct()

    }

    out = series %>%
      dplyr::arrange(station_code)

    # series_matrix = series %>%
    #   dplyr::arrange(station_code) %>%
    #   tidyr::pivot_wider(names_from = .data$station_code, values_from = 3)
    #
    #
    # out <- list(series = series %>%
    #               base::split(.$station_code),
    #             df_series = series,
    #             series_matrix = series_matrix)
    }

  class(out) <- c(class(out), 'histStatistics')

  return(out)

  }


if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",
                                                        'station_code',
                                                        'stream_flow_m3_s',
                                                        'monthWaterYear',
                                                        'waterYear',
                                                        'rainfall_mm'))

