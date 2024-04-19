#' Compute streamflow or rainfall statistics by month or year
#'
#' @encoding UTF-8
#'
#' @description Takes as input a list containing data frames of organized and filtered records
#'   for each station (output from [hydrobr::selectStations()]) and compute streamflow or rainfall
#'   statistics by month or year.
#'
#' @param selectStationsResultSeries list, tibble data frame; provides a list containing
#'   the data frames of filtered records for each station
#'   (series output from [hydrobr::selectStations()] function).
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
#' @param rainyDays numeric; number of day to be consider if "Rmax" is choose as statistic parameters.
#' For example, if rainyDays = 2, seriesStatistics will compute max value considering 2 day of raining for each wateryear (or monthWaterYear).
#' Default is 1 day.
#'
#' @param byMonth logical;  if byMounth = TRUE, seriesStatistics is performed for by month. default = FALSE.
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
#' Qmean_years = seriesStatistics(final_data$series, statistics = "Qmean")
#'
#' @export
#' @importFrom rlang .data
#' @importFrom rlang :=


seriesStatistics = function(selectStationsResultSeries,
                             statistics = "Qmean",
                             permanence = 95,
                             rainyDays = 1,
                             byMonth = FALSE)
{

  # ## Verification if arguments are in the desired format
  # # is selectStationsResultSeries an outcome from selectStations?
  # if (attributes(selectStationsResultSeries)$class[2] %in% 'stationsData') {
  #   stop(
  #     call. = FALSE,
  #     '`inventoryResults` does not inherit attribute "inventory".
  #        The outcome from the inventory() function should be passed as argument'
  #   )
  # }

  #if `selectStationsResultSeries` and `statistics` belong same kind of data

  if (names(selectStationsResultSeries[[1]])[4]=="stream_flow_m3_s") {

    if (!statistics %in% c("Qmean", "Qperm", "Q7", "Qmin", "Qmax")){

      stop(
        call. = FALSE,
        'for streamflow data from `selectStations` must choose streamflow `statistics` (either "Qmean", "Qperm", "Q7", "Qmin" or "Qmax").
       See arguments details for more information.'
      )
    }
  }

  #if `selectStationsResultSeries` and `statistics` belong same kind of data

  if (names(selectStationsResultSeries[[1]])[4]=="rainfall_mm") {

    if (!statistics %in% c("Rtotal", "Rmax", "Rdays")){

      stop(
        call. = FALSE,
        'for streamflow data from `selectStations` must choose streamflow `statistics` (either "Rtotal", "Rmax" or "Rdays").
       See arguments details for more information.'
      )
    }
  }

  if (!is.character(statistics) | length(statistics) != 1 | !statistics %in% c("Qmean", "Qperm", "Q7", "Qmin", "Qmax", "Rtotal", "Rmax", "Rdays")) {
    stop(
      call. = FALSE,
      '`statistics` should be a character vector of length == 1.
      if `selectStationsResultSeries` is a streamflow data must use streamflow `statistics` (either "Qmean", "Qperm", "Q7", "Qmin" or "Qmax").
      if `selectStationsResultSeries` is a rainfall data must use rainfall `statistics` (either "Rtotal", "Rmax" or "Rdays").
       See arguments details for more information.'
    )
  }

  #if "Qperm" is chosen, verify "permanence" parameter
  if (statistics=="Qperm" & (!is.numeric(permanence) | length(permanence) != 1 | permanence>100 | permanence < 0)) {
    stop(
      call. = FALSE,
      '`permanence` should be a numeric vector of length == 1 (ranging from 0 to 100).
       See arguments details for more information.'
    )
  }




  #identify if input is annual or month
  if (byMonth == FALSE) {
    period <- "waterYear"
  } else {
    period <- "monthWaterYear"
  }


  # compute Qmean and other statistics

  if (statistics == "Qmean") {

    series <- selectStationsResultSeries %>%
      do.call(what = dplyr::bind_rows) %>%
      dplyr::group_by_at(c(period, "station_code")) %>%
      dplyr::summarise(
        station_code = unique(station_code),
        Qmean_m3_s = mean(stream_flow_m3_s, na.rm = TRUE),
        .groups = 'drop') %>%
      dplyr::select(c(2, 1, 3))


  } else if (statistics == "Q7") {

    series <- selectStationsResultSeries %>% dplyr::bind_rows() %>%
      dplyr::group_by_at(c(period, "station_code")) %>%
      dplyr::mutate(Q7_m3_s = zoo::rollapply(.data$stream_flow_m3_s,
                                             7, FUN = mean, partial = TRUE, align = "left")) %>%
      dplyr::slice(-(dplyr::n() - 5):-dplyr::n()) %>%
      dplyr::filter(Q7_m3_s==min(Q7_m3_s, na.rm = T)) %>%
      dplyr::slice(1) %>%
      dplyr::select(c(1, dplyr::all_of(period),
                      Q7_m3_s))

  } else if (statistics == "Qperm") {

    series <- selectStationsResultSeries %>%
      do.call(what = dplyr::bind_rows) %>%
      dplyr::group_by_at(c(period, "station_code")) %>%
      dplyr::summarise(
        station_code = unique(station_code),
        !!paste("Q", permanence, "_m3_s", sep = "") := stats::quantile(stream_flow_m3_s, 1 - permanence / 100, na.rm = TRUE),
        .groups = 'drop') %>%
      dplyr::select(c(2, 1, 3))

  } else if (statistics == "Qmin") {

    series <- selectStationsResultSeries %>%
      do.call(what = dplyr::bind_rows) %>%
      dplyr::group_by_at(c(period, "station_code")) %>%
      dplyr::summarise(
        station_code = unique(station_code),
        Qmin_m3_s = min(stream_flow_m3_s, na.rm = TRUE),
        .groups = 'drop') %>%
      dplyr::select(c(2, 1, 3))

  } else if (statistics == "Qmax") {

    series <- selectStationsResultSeries %>%
      do.call(what = dplyr::bind_rows) %>%
      dplyr::group_by_at(c(period, "station_code")) %>%
      dplyr::summarise(
        station_code = unique(station_code),
        Qmax_m3_s = max(stream_flow_m3_s, na.rm = TRUE),
        .groups = 'drop') %>%
      dplyr::select(c(2, 1, 3))

  } else if (statistics == "Rtotal") {

    # selectStationsResultSeries %>%
    #   do.call(what = dplyr::bind_rows) %>%
    #   dplyr::filter(station_code == 1746018 & waterYear == 1988) %>%
    #   dplyr::pull(rainfall_mm) %>%
    #   sum(na.rm = TRUE)

    series <- selectStationsResultSeries %>%
      do.call(what = dplyr::bind_rows) %>%
      dplyr::group_by_at(c(period, "station_code")) %>%
      dplyr::summarise(
        station_code = unique(station_code),
        rainTotal_mm = sum(rainfall_mm, na.rm = TRUE),
        .groups = 'drop') %>%
      dplyr::select(c(2, 1, 3))

  } else if (statistics == "Rmax"){

    series <- selectStationsResultSeries %>% dplyr::bind_rows() %>%
      dplyr::group_by_at(c(period, "station_code")) %>%
      dplyr::mutate(Rmax= zoo::rollapply(.data$rainfall_mm,
                                         width = rainyDays, FUN = sum, partial = TRUE, align = "left")) %>%
      dplyr::slice(-(dplyr::n() - 5):-dplyr::n()) %>%
      dplyr::filter(Rmax==max(Rmax, na.rm = T)) %>%
      dplyr::slice(1) %>%
      dplyr::select(c(1, dplyr::all_of(period),
                      Rmax)) %>%
      stats::setNames(c("station_code", "waterYear", paste0("rainMax", rainyDays, "days_mm")))
  }

  else if (statistics == "Rdays") {

    series <- selectStationsResultSeries %>%
      do.call(what = dplyr::bind_rows) %>%
      dplyr::group_by_at(c(period, "station_code")) %>%
      dplyr::summarise(
        station_code = unique(station_code),
        rainyDays = sum(rainfall_mm>0, na.rm = TRUE),
        .groups = 'drop') %>%
      dplyr::select(c(2, 1, 3))

  }


  #reorder results

  if (period == "monthWaterYear"){

    #reorder series based on date

    series = series %>%
      dplyr::arrange(station_code, monthWaterYear)

    #series_matrix reordered

    series_matrix = series %>%
      dplyr::arrange(station_code) %>%
      tidyr::pivot_wider(names_from = .data$station_code, values_from = 3) %>%
      dplyr::arrange(monthWaterYear)

  } else {

    series = series %>%
      dplyr::arrange(station_code, "waterYear")

    series_matrix = series %>%
      dplyr::ungroup() %>%
      dplyr::arrange(station_code) %>%
      tidyr::pivot_wider(names_from = station_code, values_from = 3) %>%
      dplyr::mutate(date = as.Date(paste("01", "01", waterYear, sep = "-"),
                                   tryFormats = "%d-%m-%Y")) %>%
      dplyr::arrange(date) %>%
      dplyr::select(-date)

  }

  out <- list(series = series %>%
                base::split(.$station_code),
              df_series = series,
              series_matrix = series_matrix)
  out$series[3]

  # class(out) <- c(class(out), 'flowStatistics')

  return(out)

}


if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",
                                                        'station_code',
                                                        'stream_flow_m3_s',
                                                        'monthWaterYear',
                                                        'waterYear',
                                                        'rainfall',
                                                        'Rmax'))

