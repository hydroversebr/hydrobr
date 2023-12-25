#' Sazonality
#'
#'@description Identifica os 6 meses secos e úmidos nas estações em análise.
#'
#' @param selectionResultSeries list of tibble; lista de tibbles obtidos com a função [hydrobr::selectStations] contendo a série de dados diários.
#'
#' @param statistic character; "mean" or "median" monthly statistics.
#'
#' @return dataframe contendo o início do mês úmido e seco.
#'
#' @details Computa estatística mensal (média ou mediana) nos meses do ano e, posteriormente, e realiza uma média movel de 6 meses para identificar período seco e úmido no ano.
#'
#'
#' @examples
#'
#' # Fech a inventory of fluviometric stations for the state of Minas Gerais
#'
#' inv <- inventory(
#'   states = "MINAS GERAIS",
#'   stationType = "flu",
#'   as_sf = TRUE,
#'   aoi = NULL
#' )
#'
#' # Download stations from the inventory
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
#'sazon = sazonality(final_data$series, statistic = "median")
#'
#'
#'
#' @export
#'




sazonality = function(selectionResultSeries, statistic = "median"){

  stopifnot(
    "`statistic` parameter must be `median` or `mean`" = statistic %in% c("median", "mean"),
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




  mMensal = hydrobr::mMonthlyStat(selectionResultSeries)




  sazon = mMensal %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(station_code) %>%
    dplyr::slice(rep(1:dplyr::n(), 2)) %>%
    dplyr::mutate(meanRoll6Mean = zoo::rollapply(.data$monthlyMean,
                                                 width = 6,
                                                 FUN = base::mean,
                                                 partial = T,
                                                 align = "left"),
                  medianRoll6Mean = zoo::rollapply(.data$monthlyMedian,
                                                   width = 6,
                                                   FUN = base::mean,
                                                   partial = T,
                                                   align = "left"),
                  months = zoo::rollapply(.data$month,
                                          width = 6,
                                          FUN = function(y) {
                                            paste(dplyr::first(y),
                                                  dplyr::last(y),
                                                  sep = "-")},
                                          partial = T,
                                          align = "left")) %>%
    dplyr::slice(1:12)



  if (statistic == "median") {
    AnoHS <- sazon %>%
      dplyr::filter(medianRoll6Mean == min(medianRoll6Mean)) %>%
      dplyr::select(1, 2, 7) %>% stats::setNames(c("station_code",
                                                   "FirstDryMonth", "DryMonths"))%>%
      dplyr::ungroup()
    AnoHU <- sazon %>%
      dplyr::filter(medianRoll6Mean == max(medianRoll6Mean))%>%
      dplyr::select(1, 2, 7) %>% stats::setNames(c("station_code",
                                                   "FirstwetMonth", "WetMonths"))%>%
      dplyr::ungroup()
  } else {

    AnoHS <- sazon %>%
      dplyr::filter(meanRoll6Mean == min(meanRoll6Mean)) %>%
      dplyr::select(1, 2, 7) %>% stats::setNames(c("station_code",
                                                   "FirstDryMonth", "DryMonths")) %>%
      dplyr::ungroup()
    AnoHU <- sazon %>%
      dplyr::filter(meanRoll6Mean == max(meanRoll6Mean))%>%
      dplyr::select(1, 2, 7) %>% stats::setNames(c("station_code",
                                                   "FirstwetMonth", "WetMonths"))%>%
      dplyr::ungroup()


  }

  sazon <- dplyr::left_join(AnoHU, AnoHS, by = "station_code") %>%
    dplyr::select(c(1, 4, 2, 5, 3))

  return(sazon)

}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("MonthlyMResult",
                                                        'selectionResultSeries',
                                                        'MonthlyM',
                                                        'medianRoll6Mean',
                                                        'meanRoll6Mean'))



