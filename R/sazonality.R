#' Sazonality
#'
#'@description Identifica os 6 meses secos e úmidos nas estações em análise

#'
#' @param selectionsResultSeries list of tibble; lista de data frames obtidos com a função selectStations considerando o ano hidrológico igual o civil (month = 1)
#'
#' @return dataframe contendo o início do mês úmido e seco.
#'
#' @details Computa estatística mensal (média ou mediana) nos meses do ano e, posteriormente, e realiza uma média movel de 6 meses para identificar período seco e úmido no ano.
#'
#' @export
#'
#' @examples
#'
#'



sazonalityFunction = function(selectionsResultSeries, statistic = "median"){

  MedianasMensais = function(x){

    mediaMovel = x %>%
      ungroup() %>%
      mutate(mes = lubridate::month(monthCivilYear)) %>%
      group_by_at(c("station_code", "mes")) %>%
      summarise(mediaMes = mean(stream_flow_m3_s, na.rm = TRUE),
                medianaMes = median(stream_flow_m3_s, na.rm = TRUE)) %>%
      base::suppressMessages() %>%
      slice(rep(1:n(), 2)) %>% #repetir data_frame
      mutate(mediaMovel6 = dplyr::if_else(statistic == "median",
                                          zoo::rollapply(.data$medianaMes,
                                                         6, FUN = mean,
                                                         partial = T,
                                                         align = "left"),
                                          zoo::rollapply(.data$mediaMes,
                                                         6, FUN = mean,
                                                         partial = T,
                                                         align = "left")),
             meses = zoo::rollapply(.data$mes,
                                    6, FUN = function(y) paste(first(y), last(y), sep = "-"),
                                    partial = T,
                                    align = "left")) %>%
      slice(1:12)




  }


  AnoHS = lapply(selectionsResult,FUN = MedianasMensais) %>%
    dplyr::bind_rows() %>%
    group_by(station_code) %>%
    dplyr::filter(mediaMovel6==min(mediaMovel6)) %>%
    dplyr::select(1,2, 6) %>%
    stats::setNames(c("station_code", "FirstDryMonth", "DryMonths"))


  AnoHU = lapply(selectionsResult,FUN = MedianasMensais) %>%
    dplyr::bind_rows() %>%
    group_by(station_code) %>%
    dplyr::filter(mediaMovel6==max(mediaMovel6)) %>%
    dplyr::select(1,2, 6) %>%
    stats::setNames(c("station_code", "FirstwetMonth", "WetMonths"))

  Sazonalidade = left_join(AnoHU, AnoHS, by = "station_code") %>%
    select(c(1,2,4,3,5))

  return(Sazonalidade)

}


