#' Compute flow statistics by month or year
#'
#' @encoding UTF-8
#'
#' @description Takes as input a list containing data frames of organized and filtered records
#'   for each station (output from [hydrobr::selectStations()]) and compute flow statistics
#'   by month or year.
#'
#' @param selectStationsResult list, tibble data frame; provides a list containing
#'   the data frames of filtered records for each station
#'   (output from [hydrobr::selectStations()] function).
#' @param statistic character; indicates statistics. The supported statistics are:
#' (1) mean stream flow (Qmean); (2)  minimum of seven-day moving average of daily stream flow (Q7);
#' (3) stream flow associated with a percentage of time (Qperm); (4) maximum stream flow (Qmax);
#' and (5) minimum stream flow (Qmin).
#' @param permanence numeric; percentage of time if "Qperm" is choose as statistic parameter
#'   The default is 90 percent.
#'
#' @return A list containing statistic data frame [tibble::tibble()] object
#'    for each station.
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
flowStatistics = function(selectStationsResult, statistics = "Qmean", permanence = 95)
{

  #identify if input is annual or month
  if (names(selectStationsResult$failureMatrix)[1]=="waterYear"){

    period = "waterYear"

  } else if(names(selectStationsResult$failureMatrix)[1]=="monthWaterYear"){

    period = "monthWaterYear"

  } else {stop ("Please choose \"selectStations\" output result \"Annual\" or \"Monthly\".")}


  selectStationsResult = selectStationsResult$serie

  #compute Qmean and other statistics

  if(statistics == "Qmean"){
    df = lapply(selectStationsResult, function(x) x %>%
                  dplyr::group_by_at(period) %>%
                  dplyr::summarise(Qmean = mean(stream_flow_m3_s, na.rm = TRUE)))

  } else if (statistics == "Q7"){


    df = lapply(selectStationsResult, function(x) x %>%
                  na.omit() %>%
                  dplyr::group_by_at(period) %>%
                  dplyr::summarise(Q7 = min(zoo::rollapply(stream_flow_m3_s, 7, FUN = mean, partial = TRUE, align = "left"))))

  } else if (statistics == "Qperm"){

    df = lapply(selectStationsResult, function(x) x %>%
                  dplyr::group_by_at(period) %>%
                  dplyr::summarise(Qperm = quantile(stream_flow_m3_s, 1 - permanence/100, na.rm = TRUE)))

  } else if(statistics == "Qmin"){
    df = lapply(selectStationsResult, function(x) x %>%
                  dplyr::group_by_at(period) %>%
                  dplyr::summarise(Qmin = min(stream_flow_m3_s, na.rm = TRUE)))

  } else if(statistics == "Qmax"){
    df = lapply(selectStationsResult, function(x) x %>%
                  dplyr::group_by_at(period) %>%
                  dplyr::summarise(Qmax = max(stream_flow_m3_s, na.rm = TRUE)))

  } else {stop("Please choose \"statistics\" parameter among \"Qmean\", \"Qmin\", \"Qmax\" or \"Qperm\".")}

  return(df)

}
