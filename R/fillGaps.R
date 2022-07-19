#' (UNDER DEVELOPMENT) Fill gaps at monthly or annual time series
#'
#' @encoding UTF-8
#'
#' @description Takes as input a list containing annual or monthly time series statistic
#'    for each station (output from [hydrobr::seriesStatistics()])
#'    and try to fill gaps based on linear regression among them.
#'
#' @param StatisticsResult list, tibble data frame; A list containing statistic data frame [tibble::tibble()] object
#'    for each station (output from [hydrobr::seriesStatistics()]).
#' @param minimumCor value; minimum correlation between stations. default = 0.84
#' @param minimunObsPairs value; minimum of observation pairwise between stations to be filled with.
#' If 'StatisticsResult' is annual time series, minimunObsPairs is equal to number of commom years.
#' If 'StatisticsResult' is monthly time series, minimunObsPairs is equal to number of common months.
#'
#' @return A list containing 4 objects:
#'   * a list containing statistic a data frame [tibble::tibble()] object for each station.
#'   gap filled.
#'   * a data frame [tibble::tibble()] with statistic of all stations in wide format
#'   * a data frame [tibble::tibble()] with statistic of all stations in longer format
#'   * a failureMatrix indicating if the gap was filled (TRUE) or not (FALSE)
#'   * the saved plot.

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
#' # Annual mean stream flow serie for each station
#' Qmean_years = flowStatistics(final_data, statistics = "Qmean")
#'
#' #fill Gaps of Annual time series
#'
#' Qmean_years_filled = fillGaps(StatisticsResult = Qmean_years,
#'                               minimumCor = 0.84,
#'                               minimunObsPairs = 10)
#'
#' @export
#' @importFrom rlang :=

fillGaps = function(StatisticsResult, minimumCor = 0.84, minimunObsPairs = 10){


  ## Verification if arguments are in the desired format
  # is StatisticsResult an outcome from rainStatistics or flowStatistics function?
  if (!attributes(StatisticsResult)$class[2] %in% c('flowStatistics','rainStatistics')) {
    stop(
      call. = FALSE,
      '`StatisticsResult` does not inherit attribute "flowStatistics" or "rainStatistics".
       The outcome from the flowStatistics() or rainStatistics() function should be passed as argument'
    )
  }

  # Is mode a character vector?
  if (!is.numeric(minimumCor) | length(minimumCor) != 1) {
    stop(
      call. = FALSE,
      '`minimumCor` should be a numeric vector of length == 1 (bigger then 0 and lesse then 1).
       See arguments details for more information.'
    )
  }

  # Is mode a character vector?
  if (!is.numeric(minimunObsPairs) | length(minimunObsPairs) != 1 | !minimunObsPairs >= 5) {
    stop(
      call. = FALSE,
      '`minimunObsPairs` should be a numeric vector of length == 1 (bigger or equal to 5).
       See arguments details for more information.'
    )
  }

  #identify type of serie (annual or monthly)
  #if monthly: minimunObsPairs = minimunObsPairs*12

  if (names(StatisticsResult$series[[1]])[2]=="waterYear"){

    period = "waterYear"

  } else if(names(StatisticsResult$series[[1]])[2]=="monthWaterYear"){

    period = "monthWaterYear"

    minimunObsPairs = minimunObsPairs*12

  } else {stop ("Please choose \"selectStations\" output result \"Anual\" or \"Monthly\".")}


  resultados = list()

  #convert list of station serie to tibble and rename columns
  df = StatisticsResult$series_matrix %>%
    dplyr::select(dplyr::all_of(period), sort(colnames(.))[-ncol(.)])



  #correlation between stations
  corN = tibble::as_tibble(stats::cor(df[,-1], use = "pairwise.complete.obs"))

  preenchidos = list()

  for (i in 1:ncol(corN)){

    #identify station that will be filled
    estPrencher = names(corN)[i]
    estPrencher

    #identify correlation order and get stations name

    ordem = corN[i,] %>%
      t() %>%
      as.data.frame() %>%
      dplyr::arrange(-V1) %>%
      t() %>%
      tibble::as_tibble() %>%
      dplyr::select(-dplyr::all_of(estPrencher))


    ordem = names(ordem)[ordem>=abs(minimumCor)]
    ordem


    if (length(stats::na.omit(ordem))>0){ #caso exista estação com r > 0.84 #na.omit existe pois pode haver situações que não existe dados pareados gerando NA na correlão entre estações

      for (j in 1:length(stats::na.omit(ordem))){ #para todas estações com r > 0.84

        if (nrow(stats::na.omit(df[,c(period, estPrencher, ordem[j])]))>=minimunObsPairs){ #se o número de observações pareadas forem maior ou igual a "minimunObsPairs"

          regrdf = df[,c(period, estPrencher, ordem[j])] #df com estação a ser preenchida e estação que vai preencher

          names(regrdf) = c(period, "y", "x") #renomear para regressão

          df2 <- regrdf %>% dplyr::filter(!is.na(y)) #retirar NAs da estação que será preenchida

          fit <- stats::lm(y~x, data = df2) #regressão

          if (!exists("df3")){       #df3 é o df com dados preenchidos, mas pode ser que em uma rodada não preencha todos.

            df3 <- regrdf %>%
              dplyr::mutate(pred = stats::predict(fit, .)) %>%
              # Replace NA with pred in var1
              dplyr::mutate(preenchido = ifelse(is.na(y), pred, y)) %>%
              dplyr::select(dplyr::all_of(period), preenchido)

          } else {

            df3 = regrdf %>%
              dplyr::mutate(pred = stats::predict(fit, .)) %>%
              dplyr::mutate(preenchido = ifelse(is.na(df3$preenchido), pred, df3$preenchido)) %>%
              dplyr::select(dplyr::all_of(period), preenchido)

          }


          if(sum(is.na(df3)[,2])==0) break

        }
      }

      if (exists("df3")){

        preenchidos[[i]] = df3

        remove(df3) #remover df3 para próxima rodada (estação)

      } else {preenchidos[[i]] = df[,c(period, estPrencher)]}


    } else {preenchidos[[i]] = df[,c(period, estPrencher)]}

    names(preenchidos[[i]]) = c(period, names(StatisticsResult$df_series)[3])

    preenchidos[[i]] = dplyr::arrange(preenchidos[[i]], dplyr::across(dplyr::starts_with(period))) %>%
      dplyr::mutate(station_code = estPrencher) %>%
      dplyr::select(c(3,1,2))
  }

  names(preenchidos) = names(df[,-1])

  #reorder preenchidos based on date if its a montlhy series
  if(names(preenchidos[[1]])[2] == "monthWaterYear"){

    preenchidos = preenchidos %>%
      lapply(function(x) x %>%
      dplyr::arrange(monthWaterYear))

  }

  #list of df with filling results of each station
  resultados[[1]] = preenchidos

  #convert list to df
  resultados[[2]] = preenchidos %>%
    dplyr::bind_rows()

  #convert list to df wider format
  resultados[[3]] = resultados[[2]] %>%
    tidyr::pivot_wider(names_from = station_code, values_from = 3)

  #failureMatrix
  resultados[[4]] = resultados[[3]] %>%
    dplyr::mutate_at(c(2:ncol(.)), ~ dplyr::if_else(is.na(.), TRUE, FALSE))


  #plot
  g = reshape2::melt(resultados[[4]], id.vars = period) %>%
    dplyr::rename(!!period := dplyr::contains("Year"),
                  station_code = "variable") %>%
    dplyr::as_tibble() %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[period]],
                                 y = .data$station_code,
                                 fill = .data$value)) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::scale_fill_manual(name =  paste(names(StatisticsResult$df_series)[3] %>%
                                               stringr::str_extract(pattern = "[^_]+"), "Data", sep = " "),
                               values=c("FALSE"="#00b0f6", "TRUE"="#f8766d"),
                               labels = c("complete", "missing"))+
    ggplot2::theme_bw()

  resultados[[5]] = g

  names(resultados) = c("series", "df_series", "df_serie_wider", "failure_matrix", "plot")

  print(g)

  class(resultados) <- c(class(resultados), 'fillGaps')

  return(resultados)
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("V1", "y", "pred", "preenchido"))

