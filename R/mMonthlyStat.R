#' Mean and Median Monthly Statistics
#'
#'@description Evaluate Mean and Median monthly statistics based [hydrobr::selectStations] function series result

#'
#' @param selectionResultSeries list of tibble; lista de tibbles obtidos com a função [hydrobr::selectStations] contendo a série de dados diários.
#'
#' @return dataframe contendo o início do mês úmido e seco.
#'
#' @details Computa estatística mensal (média ou mediana) nos meses do ano e, posteriormente, e realiza uma média movel de 6 meses para identificar período seco e úmido no ano.
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
#'mStats = mMonthlyStat(final_data)
#'
#'
#' @export



mMonthlyStat = function(selectionResultSeries){

  stopifnot(
    "`selectionsResultSerie` parameter must be a list of tibble resulted from `selectStation` function" = is.list(selectionResultSeries),
    "`selectionsResultSerie` parameter must be a list of tibble resulted from `selectStation` function" = identical(names(selectionResultSeries[[1]]), c(
      "station_code", "consistency_level", "date", "stream_flow_m3_s",
      "civilYear", "monthCivilYear", "waterYear", "monthWaterYear",
      "maxMissing"
    )) | identical(names(selectionResultSeries[[1]]), c(
      "station_code", "consistency_level", "date", "rainfall_mm",
      "civilYear", "monthCivilYear", "waterYear", "monthWaterYear",
      "maxMissing"
    ))
  )

  MonthlyMR = lapply(selectionResultSeries, FUN = function(y) y %>%
                       dplyr::ungroup() %>%
                       dplyr::mutate(month = lubridate::month(monthCivilYear)) %>%
                       dplyr::group_by_at(c("station_code", "month")) %>%
                       dplyr::summarise(monthlyMean = base::mean(stream_flow_m3_s,
                                                                 na.rm = TRUE),
                                        monthlyMedian = stats::median(stream_flow_m3_s,
                                                                      na.rm = TRUE)) %>%
                       dplyr::ungroup() %>%
                       base::suppressMessages()) %>%
    stats::setNames(names(selectionResultSeries))

  return(MonthlyMR)
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("monthCivilYear",
                                                        'selectionResultSeries',
                                                        'MonthlyM',
                                                        'medianRoll6Mean',
                                                        'meanRoll6Mean'))
