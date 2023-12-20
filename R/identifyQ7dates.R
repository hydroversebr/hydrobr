#' Retorna a série de Q7 anual e respectivas datas
#'
#' @encoding UTF-8
#'
#' @description A partir da série histórica de vazões nas estações (output from [hydrobr::selectStations()]), retorna os valores de Q7 anual com respectivas datasTakes as input a list containing data frames of organized records
#'
#' @param selectStationsResultSeries list of tibble data frame;
#' @param order character; quando há valores mínimos de Q7 num ano, escolher o primeiro (firstDate) ou último (lastDate).
#'
#' @return A list containing 2 objects:
#'   * a list of data frames [tibble::tibble()] for each station containing Q7 dates, value and month of ocorrence for each wateryear
#'   * a dataframe containing Q7 month frequency for each station
#'
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
#'
#' Q7stats = identifyQ7dates( final_data$series)
#'
#' @export
#'
#' @importFrom rlang .data

identifyQ7dates = function(selectStationsResultSeries, order = "lastDate"){



  series1 = lapply(selectStationsResultSeries, function(x) {x %>%
      dplyr::arrange(date) %>%
      dplyr::group_by_at(c("waterYear")) %>%
      dplyr::mutate(Q7_m3_s = zoo::rollapply(.data$stream_flow_m3_s,
                                             7, FUN = mean,
                                             partial = TRUE,
                                             align = "left"),
                    dataQ7 = zoo::rollapply(.data$date,
                                            7, FUN = function(x) paste(min(x), max(x), sep = "//"),
                                            partial = TRUE,
                                            align = "left")) %>%

      dplyr::slice(-(dplyr::n()-5):-dplyr::n()) %>%
      dplyr::filter(Q7_m3_s==min(Q7_m3_s, na.rm = T)) %>%
      {if(order == "lastDate") dplyr::arrange(., dplyr::desc(date)) else dplyr::arrange(., date)} %>%
      dplyr::slice(1) %>%
      dplyr::mutate(firstDateQ7 = as.Date(substr(dataQ7, 0,10)),
                    lastDateQ7 = as.Date(substr(dataQ7, 13,23)),
                    doisMeses = dplyr::if_else(lubridate::month(firstDateQ7)==lubridate::month(lastDateQ7), "sim", "nao"),
                    daysMonth1 = dplyr::if_else(doisMeses == "nao",
                                         lubridate::days_in_month(lubridate::month(firstDateQ7)) - lubridate::mday(firstDateQ7)+1,
                                         NA),
                    daysMonth2 = dplyr::if_else(doisMeses == "nao",
                                         lubridate::mday(lastDateQ7),
                                         NA),
                    monthQ7 = dplyr::if_else(daysMonth2>=daysMonth1, lubridate::month(lastDateQ7), lubridate::month(firstDateQ7)),
                    monthQ7 = dplyr::if_else(is.na(monthQ7), lubridate::month(lastDateQ7), monthQ7)) %>%
      dplyr::select(dplyr::everything(), -dataQ7, -doisMeses, -daysMonth1, -daysMonth2) %>%
      dplyr::ungroup() %>%
      dplyr::select(station_code, civilYear, waterYear, Q7_m3_s, firstDateQ7, lastDateQ7, monthQ7)}

  )

  series1_freq = lapply(series1, function(y)  y %>%
                          dplyr::pull(monthQ7) %>%
                          plyr::count() %>%
                          dplyr::right_join(tibble::tibble(x = 1:12, by = "x")) %>%
                          base::suppressMessages() %>%
                          dplyr::select(c(1,2)) %>%
                          dplyr::arrange(x) %>%
                          dplyr::mutate(station_code = unique(y$station_code),
                                        freq = dplyr::if_else(is.na(freq), 0, freq),
                                        month = as.factor(x)) %>%
                          dplyr::select(station_code, month, freq)) %>%
    dplyr::bind_rows() %>%
    dplyr::as_tibble() %>%
    tidyr::pivot_wider(id_cols = station_code, names_from = month, values_from = freq)


  listResult = list(series1, series1_freq)
  names(listResult) = c("Q7stats", "Q7monthFreq")
  return(listResult)


}


if(getRversion() >= "2.15.1")  utils::globalVariables(c("n",
                                                        'dataQ7',
                                                        'firstDateQ7',
                                                        'lastDateQ7',
                                                        'waterYear',
                                                        'doisMeses',
                                                        'daysMonth1',
                                                        'daysMonth2',
                                                        'monthQ7',
                                                        'civilYear',
                                                        'x',
                                                        'freq'))
