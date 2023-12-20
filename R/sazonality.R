#' Sazonality
#'
#'@description Identifica os 6 meses secos e úmidos nas estações em análise

#'
#' @param selectionsResultSeries list of tibble; lista de data frames obtidos com a função selectStations considerando o ano hidrológico igual o civil (month = 1)
#'
#' @param statistic character; "mean" or "median" monthly statistics
#'
#' @return dataframe contendo o início do mês úmido e seco.
#'
#' @details Computa estatística mensal (média ou mediana) nos meses do ano e, posteriormente, e realiza uma média movel de 6 meses para identificar período seco e úmido no ano.
#'
#' @export
#'
#'
#'



sazonalityFunction = function(selectionsResultSeries, statistic = "median"){

  MedianasMensais = function(x){

    mediaMovel = x %>%
      dplyr::ungroup() %>%
      dplyr::mutate(mes = lubridate::month(monthCivilYear)) %>%
      dplyr::group_by_at(c("station_code", "mes")) %>%
      dplyr::summarise(mediaMes = base::mean(stream_flow_m3_s, na.rm = TRUE),
                medianaMes = stats::median(stream_flow_m3_s, na.rm = TRUE)) %>%
      base::suppressMessages() %>%
      dplyr::slice(rep(1:dplyr::n(), 2)) %>% #repetir data_frame
      dplyr::mutate(mediaMovel6 = dplyr::if_else(statistic == "median",
                                          zoo::rollapply(.data$medianaMes,
                                                         6, FUN = base::mean,
                                                         partial = T,
                                                         align = "left"),
                                          zoo::rollapply(.data$mediaMes,
                                                         6, FUN = base::mean,
                                                         partial = T,
                                                         align = "left")),
             meses = zoo::rollapply(.data$mes,
                                    6, FUN = function(y) paste(dplyr::first(y), dplyr::last(y), sep = "-"),
                                    partial = T,
                                    align = "left")) %>%
      dplyr::slice(1:12)




  }


  AnoHS = lapply(selectionsResult,FUN = MedianasMensais) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(station_code) %>%
    dplyr::filter(mediaMovel6==min(mediaMovel6)) %>%
    dplyr::select(1,2, 6) %>%
    stats::setNames(c("station_code", "FirstDryMonth", "DryMonths"))


  AnoHU = lapply(selectionsResult,FUN = MedianasMensais) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(station_code) %>%
    dplyr::filter(mediaMovel6==max(mediaMovel6)) %>%
    dplyr::select(1,2, 6) %>%
    stats::setNames(c("station_code", "FirstwetMonth", "WetMonths"))

  Sazonalidade = dplyr::left_join(AnoHU, AnoHS, by = "station_code") %>%
    dplyr::select(c(1,2,4,3,5))

  return(Sazonalidade)

}


if(getRversion() >= "2.15.1")  utils::globalVariables(c("monthCivilYear",
                                                        'selectionsResult',
                                                        'mediaMovel6',
                                                        'lastDateQ7',
                                                        'waterYear',
                                                        'doisMeses',
                                                        'daysMonth2',
                                                        'daysMonth2',
                                                        'monthQ7',
                                                        'civilYear',
                                                        'x',
                                                        'freq'))

