#' Compute rainfall statistics by month or year
#'
#' @encoding UTF-8
#'
#' @description Takes as input a list containing data frames of organized and filtered records
#'   for each station (output from [hydrobr::selectStations()]) and compute rainfall statistics
#'   by month or year.
#'
#' @param selectStationsResult list, tibble data frame; provides a list containing
#'   the data frames of filtered records for each station
#'   (output from [hydrobr::selectStations()] function).
#'
#' @param statistics character; indicates statistics. The supported statistics are:
#' (1) total rainfall (Rtotal); (2)  maximum rainfall (Rmax);
#' (3) rainy days (Rdays). The default is "Rtotal".

#'
#' @return A list containing 3 objects:
#'   * a list containing statistic a data frame [tibble::tibble()] object for each station.
#'   * a data frame [tibble::tibble()] with statistic of all stations in wide format
#'   * a data frame [tibble::tibble()] with statistic of all stations in longer format
#'
#' @examplesIf interactive()
#' # Fecht a inventory of fluviometric stations for the state of Minas Gerais
#'
#' inv <- inventory(
#'   states = "MINAS GERAIS",
#'   stationType = "plu",
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
#' Qmean_years = rainStatistics(final_data, statistics = "Rtotal")
#'
#' @export
#' @importFrom rlang .data
rainStatistics = function(selectStationsResult, statistics = "Rtotal")
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

  if (!is.character(statistics) | length(statistics) != 1 | !statistics %in% c("Rtotal", "Rmax", "Rdays")) {
    stop(
      call. = FALSE,
      '`statistics` should be a character vector of length == 1 ("Rtotal", "Rmax" or "Rdays").
       See arguments details for more information.'
    )
  }

  #identify if input is annual or month
  if (names(selectStationsResult$failureMatrix)[1] == "waterYear") {
  period <- "waterYear"
} else {
  period <- "monthWaterYear"
}


selectStationsResult <- selectStationsResult$series

# compute Qmean and other statistics

if (statistics == "Rtotal") {

  # selectStationsResult %>%
  #   do.call(what = dplyr::bind_rows) %>%
  #   dplyr::filter(station_code == 1746018 & waterYear == 1988) %>%
  #   dplyr::pull(rainfall_mm) %>%
  #   sum(na.rm = TRUE)

  series <- selectStationsResult %>%
    do.call(what = dplyr::bind_rows) %>%
    dplyr::group_by_at(c(period, "station_code")) %>%
    dplyr::summarise(
      station_code = unique(station_code),
      rainTotal_mm = sum(rainfall_mm, na.rm = TRUE),
      .groups = 'drop') %>%
    dplyr::select(c(2, 1, 3))



} else if (statistics == "Rmax") {

  series <- selectStationsResult %>%
    do.call(what = dplyr::bind_rows) %>%
    dplyr::group_by_at(c(period, "station_code")) %>%
    dplyr::summarise(
      station_code = unique(station_code),
      rainMax_mm = max(rainfall_mm, na.rm = TRUE),
      .groups = 'drop') %>%
    dplyr::select(c(2, 1, 3))

} else if (statistics == "Rdays") {

  series <- selectStationsResult %>%
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
     tidyr::pivot_wider(names_from = .data$station_code, values_from = 3) %>%
     dplyr::mutate(date = as.Date(paste("01", "01", waterYear, sep = "-"),
                                  tryFormats = "%d-%m-%Y")) %>%
     dplyr::arrange(date) %>%
     dplyr::select(-date)

 }

  out <- list(series = series %>%
                base::split(.$station_code),
              df_series = series,
              series_matrix = series_matrix)

  class(out) <- c(class(out), 'rainStatistics')

  return(out)

}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("rainfall_mm"))
