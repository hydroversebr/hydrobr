#' Compute flow statistics by month or year
#'
#' @encoding UTF-8
#'
#' @description Takes as input a list containing data frames of organized and filtered records
#'   for each station (output from [hydrobr::selectStations()]) and compute historical
#'   streamflow or rainfall statistics
#'
#' @param selectStationsResultSeries list, tibble data frame; provides a list containing
#'   the data frames of filtered records for each station
#'   (series output from [hydrobr::selectStations()] function).
#'
#' @param statistics character; indicates statistics.
#'
#'  * The supported statistics for streamflow are:
#' (1) mean stream flow (Qmean);
#' (2) minimum of seven-day moving average of daily stream flow associated with return period (Q7T);
#' (3) stream flow associated with a percentage of time (Qperm);
#' (4) maximum stream flow (Qmax);
#' (5) minimum stream flow (Qmin).
#'
#'  * The supported statistics are:
#'  (1) total rainfall (Rtotal);
#'  (2)  maximum rainfall (Rmax);
#'  (3) rainy days (Rdays).
#'
#'  * The default value is "Qmean".
#'
#' @param basedOn character; indicate if statistics must evaluated considering year ("wateryear") or month ("monthwateryear"). Default is "wateryear".
#'
#' @param pReturn numeric; return period if "Q7T" is choose as statistic parameter.
#'   The default is 10 year.
#'
#' @param permanence numeric; percentage of time if "Qperm" is choose as statistic parameter.
#'   The default is 95 percent.
#'
#' @return A list containing 3 objects:
#'   * a list containing statistic a data frame [tibble::tibble()] object for each station.
#'   * a data frame [tibble::tibble()] with statistic of all stations in wide format.
#'   * a data frame [tibble::tibble()] with statistic of all stations in longer format.
#'
#' @details Q7T is evaluated based on empirical distribution using Kimbal method.
#'
#' @examplesIf interactive()
#' # Fech a inventory of fluviometric stations for the state of Minas Gerais.
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
#' Qmean_years = historicalStatistics(final_data$series, statistics = "Qmean")
#'
#' @export
#' @importFrom rlang .data
#' @importFrom rlang :=


historicalStatistics = function (selectStationsResultSeries,
                                  statistics = "Qperm",
                                  basedOn = "waterYear",
                                  permanence = 95,
                                  pReturn = 10)
{

  if (basedOn =="waterYear"){
    byMonth = FALSE
  } else {byMonth = TRUE}


  if (!is.character(statistics) | length(statistics) != 1 |
      !statistics %in% c("Qmean", "Qperm", "Q7T", "Qmin", "Qmax",
                         "Rtotal", "Rmax", "Rdays")) {
    stop(call. = FALSE, "`statistics` should be a character vector of length == 1 (either \"Qmean\", \"Qperm\", \"Q7\", \"Qmin\" or \"Qmax\").\n       See arguments details for more information.")
  }
  if (names(selectStationsResultSeries[[1]])[4] == "stream_flow_m3_s") {
    if (!statistics %in% c("Qmean", "Qperm", "Q7T", "Qmin",
                           "Qmax")) {
      stop(call. = FALSE, "for streamflow data from `selectStations` must choose streamflow `statistics` (either \"Qmean\", \"Qperm\", \"Q7\", \"Qmin\" or \"Qmax\").\n       See arguments details for more information.")
    }
  }
  if (names(selectStationsResultSeries[[1]])[4] == "rainfall_mm") {
    if (!statistics %in% c("Rtotal", "Rmax", "Rdays")) {
      stop(call. = FALSE, "for rainfall data from `selectStations` must choose rainfall `statistics` (either \"Rtotal\", \"Rmax\" or \"Rdays\").\n       See arguments details for more information.")
    }
  }
  if (statistics == "Qperm" & (!is.numeric(permanence) | length(permanence) !=
                               1 | permanence > 100 | permanence < 0)) {
    stop(call. = FALSE, "`permanence` should be a numeric vector of length == 1 (ranging from 0 to 100).\n       See arguments details for more information.")
  }
  if (statistics == "Q7" & (!is.numeric(pReturn) | length(pReturn) !=
                            1 | pReturn < 1)) {
    stop(call. = FALSE, "`permanence` should be a numeric vector of length == 1 (ranging from 0 to +Inf).\n       See arguments details for more information.")
  }

  selectStationsResultS <- selectStationsResultSeries

  if (byMonth == FALSE) {
    if (statistics == "Qmean") {
      series <- selectStationsResultS %>% do.call(what = dplyr::bind_rows) %>%
        dplyr::group_by_at("station_code") %>% dplyr::summarise(station_code = unique(station_code),
                                                                Qmean_m3_s = mean(stream_flow_m3_s, na.rm = TRUE),
                                                                .groups = "drop")
    }
    else if (statistics == "Q7T") {

      serieQ7 = hydrobr::seriesStatistics(selectStationsResultSeries = selectStationsResultS,
                                          statistics = "Q7")

      series = serieQ7$series %>% do.call(what = dplyr::bind_rows) %>%
        dplyr::group_by_at("station_code") %>%
        dplyr::summarise(station_code = unique(station_code),
                         `:=`(!!paste("Q7_", pReturn, "_m3_s", sep = ""),
                              Q7_m3_s %>% Q7Tempiric(pReturn = pReturn) %>% as.numeric()))


    }
    else if (statistics == "Qperm") {
      series <- selectStationsResultS %>% do.call(what = dplyr::bind_rows) %>%
        dplyr::group_by_at("station_code") %>% dplyr::summarise(station_code = unique(station_code),
                                                                `:=`(!!paste("Q", permanence, "_m3_s", sep = ""),
                                                                     stats::quantile(stream_flow_m3_s, 1 - permanence/100,
                                                                                     na.rm = TRUE)), .groups = "drop")
    }
    else if (statistics == "Qmin") {
      series <- selectStationsResultS %>% do.call(what = dplyr::bind_rows) %>%
        dplyr::group_by_at("station_code") %>% dplyr::summarise(station_code = unique(station_code),
                                                                Qmin_m3_s = min(stream_flow_m3_s, na.rm = TRUE),
                                                                .groups = "drop")
    }
    else if (statistics == "Qmax") {
      series <- selectStationsResultS %>% do.call(what = dplyr::bind_rows) %>%
        dplyr::group_by_at("station_code") %>% dplyr::summarise(station_code = unique(station_code),
                                                                Qmax_m3_s = max(stream_flow_m3_s, na.rm = TRUE),
                                                                .groups = "drop")
    }
    else if (statistics == "Rtotal") {
      series <- selectStationsResultS %>% do.call(what = dplyr::bind_rows) %>%
        dplyr::group_by_at("station_code") %>% dplyr::summarise(station_code = unique(station_code),
                                                                rainTotal_mm = sum(rainfall_mm, na.rm = TRUE)/length(unique(lubridate::year(date))),
                                                                .groups = "drop")
    }
    else if (statistics == "Rmax") {
      series <- selectStationsResultS %>% do.call(what = dplyr::bind_rows) %>%
        dplyr::group_by_at("station_code") %>% dplyr::summarise(station_code = unique(station_code),
                                                                rainMax_mm = max(rainfall_mm, na.rm = TRUE),
                                                                .groups = "drop")
    }
    else if (statistics == "Rdays") {
      series <- selectStationsResultS %>% do.call(what = dplyr::bind_rows) %>%
        dplyr::group_by_at("station_code") %>% dplyr::summarise(station_code = unique(station_code),
                                                                rainyDays = round(sum(rainfall_mm > 0, na.rm = TRUE)/length(unique(lubridate::year(date))),
                                                                                  0), .groups = "drop")
    }
    out = series %>% dplyr::arrange(station_code)
  }
  if (byMonth == TRUE) {
    if (statistics == "Qmean") {
      series <- selectStationsResultS %>% do.call(what = dplyr::bind_rows) %>%
        dplyr::mutate(month = lubridate::month(date)) %>%
        dplyr::group_by_at(c("station_code", "month")) %>%
        dplyr::summarise(station_code = unique(station_code),
                         Qmean_m3_s = mean(stream_flow_m3_s, na.rm = TRUE),
                         .groups = "drop")
    }
    else if (statistics == "Q7T") {


      serieQ7 = hydrobr::seriesStatistics(selectStationsResultSeries = selectStationsResultS,
                                          statistics = "Q7")

      series = serieQ7$series %>% do.call(what = dplyr::bind_rows) %>%
        dplyr::mutate(month = lubridate::month(monthWaterYear)) %>%
        dplyr::group_by_at(c("station_code", "month")) %>%
        dplyr::summarise(station_code = unique(station_code),
                         `:=`(!!paste("Q7_", pReturn, "_m3_s", sep = ""),
                              Q7_m3_s %>% Q7Tempiric(pReturn = pReturn) %>% as.numeric()),
                         .groups = "drop")

    }
    else if (statistics == "Qperm") {
      series <- selectStationsResultS %>% do.call(what = dplyr::bind_rows) %>%
        dplyr::mutate(month = lubridate::month(date)) %>%
        dplyr::group_by_at(c("station_code", "month")) %>%
        dplyr::summarise(station_code = unique(station_code),
                         `:=`(!!paste("Q", permanence, "_m3_s", sep = ""),
                              stats::quantile(stream_flow_m3_s, 1 - permanence/100,
                                              na.rm = TRUE)), .groups = "drop")
    }
    else if (statistics == "Qmin") {
      series <- selectStationsResultS %>% do.call(what = dplyr::bind_rows) %>%
        dplyr::mutate(month = lubridate::month(date)) %>%
        dplyr::group_by_at(c("station_code", "month")) %>%
        dplyr::summarise(station_code = unique(station_code),
                         Qmin_m3_s = min(stream_flow_m3_s, na.rm = TRUE),
                         .groups = "drop")
    }
    else if (statistics == "Qmax") {
      series <- selectStationsResultS %>% do.call(what = dplyr::bind_rows) %>%
        dplyr::mutate(month = lubridate::month(date)) %>%
        dplyr::group_by_at(c("station_code", "month")) %>%
        dplyr::summarise(station_code = unique(station_code),
                         Qmax_m3_s = max(stream_flow_m3_s, na.rm = TRUE),
                         .groups = "drop")
    }
    else if (statistics == "Rtotal") {
      series <- selectStationsResultS %>% do.call(what = dplyr::bind_rows) %>%
        dplyr::mutate(month = lubridate::month(date)) %>%
        dplyr::group_by_at(c("station_code", "month")) %>%
        dplyr::summarise(station_code = unique(station_code),
                         rainTotalAvarege_mm = sum(rainfall_mm, na.rm = TRUE)/(length(month)/lubridate::days_in_month(month)),
                         .groups = "drop") %>% dplyr::distinct()
    }
    else if (statistics == "Rmax") {
      series <- selectStationsResultS %>% do.call(what = dplyr::bind_rows) %>%
        dplyr::mutate(month = lubridate::month(date)) %>%
        dplyr::group_by_at(c("station_code", "month")) %>%
        dplyr::summarise(station_code = unique(station_code),
                         rainMax_mm = max(rainfall_mm, na.rm = TRUE),
                         .groups = "drop")
    }
    else if (statistics == "Rdays") {
      series <- selectStationsResultS %>% do.call(what = dplyr::bind_rows) %>%
        dplyr::mutate(month = lubridate::month(date)) %>%
        dplyr::group_by_at(c("station_code", "month")) %>%
        dplyr::summarise(station_code = unique(station_code),
                         rainyDays = round(sum(rainfall_mm > 0, na.rm = TRUE)/(length(month)/lubridate::days_in_month(month)),
                                           0), .groups = "drop") %>% dplyr::distinct()
    }
    out = series %>% dplyr::arrange(station_code)
  }
  # class(out) <- c(class(out), "histStatistics")
  return(out)
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",
                                                        'station_code',
                                                        'stream_flow_m3_s',
                                                        'monthWaterYear',
                                                        'waterYear',
                                                        'rainfall_mm',
                                                        'Q7_m3_s'))

