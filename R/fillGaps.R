#' Fill gaps at monthly or annual time series
#'
#' @encoding UTF-8
#'
#' @description Takes as input a list containing annual or monthly time series statistic
#'    for each station (output from [hydrobr::flowStatistics()] or [hydrobr::rainStatistics()])
#'    and try to fill gaps based on linear regression among them.
#'
#' @param StatisticsResult list, tibble data frame; A list containing statistic data frame [tibble::tibble()] object
#'    for each station (output from [hydrobr::flowStatistics()] or [hydrobr::rainStatistics()]).
#' @param minimumCor value; minimum correlation between stations. default = 0.84
#' @param minimunObsPairs value; minimum of observation pairwise between stations to be filled with.
#' If [StatisticsResult] is annual time series, minimunObsPairs is equal to number of commom years.
#' If [StatisticsResult] is monthly time series, minimunObsPairs is equal to number of common months.
#'
#' @return A list containing 4 objects:
#'   * a list containing the data frames [tibble::tibble()] for each station after
#'   gap filled.
#'   * a failureMatrix indicating if the gap was filled (TRUE) or not (FALSE)
#'   * the saved plot.
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
#' # Annual mean stream flow serie for each station
#' Qmean_years = flowStatistics(final_data, statistics = "Qmean")
#'
#' #fill Gaps of Annual time series
#'
#' Qmean_years_filled = fillGaps(StatisticsResult = Qmean_years, minimumCor = 0.84, minimunObsPairs = 10)
#'
#' @export
fillGaps = function(StatisticsResult, minimumCor = 0.84, minimunObsPairs = 10){


  #identify type of serie (annual or monthly)
  #if monthly: minimunObsPairs = minimunObsPairs*12

  if (names(StatisticsResult[[1]])[1]=="waterYear"){

    period = "waterYear"

  } else if(names(StatisticsResult[[1]])[1]=="monthWaterYear"){

    period = "monthWaterYear"

    minimunObsPairs = minimunObsPairs*12

  } else {stop ("Please choose \"selectStations\" output result \"Anual\" or \"Monthly\".")}


  resultados = list()

  #convert list of station serie to tibble and rename columns
  df = Reduce(function(x, y) dplyr::full_join(x, y, by = period), StatisticsResult) %>%
    dplyr::arrange(dplyr::across(dplyr::starts_with(period))) %>%
    stats::setNames(c(period, names(StatisticsResult)))

  #correlation between stations
  corN = tibble::as_tibble(cor(df[,-1], use = "pairwise.complete.obs"))

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


    if (length(na.omit(ordem))>0){ #caso exista estação com r > 0.84 #na.omit existe pois pode haver situações que não existe dados pareados gerando NA na correlão entre estações

      for (j in 1:length(na.omit(ordem))){ #para todas estações com r > 0.84

        if (nrow(na.omit(df[,c(period, estPrencher, ordem[j])]))>=minimunObsPairs){ #se o número de observações pareadas forem maior ou igual a "minimunObsPairs"

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

    names(preenchidos[[i]]) = c(period, names(StatisticsResult)[i])

    preenchidos[[i]] = dplyr::arrange(preenchidos[[i]], dplyr::across(starts_with(period)))
  }

  names(preenchidos) = names(df[,-1])

  resultados[[1]] = preenchidos

  preenchidos = Reduce(function(x, y) dplyr::full_join(x, y, by = period), preenchidos)


  preenchidos[,2:ncol(preenchidos)] = !is.na(preenchidos[,2:ncol(preenchidos)])

  preenchidos = preenchidos %>%
    dplyr::select(c(all_of(period), sort(names(preenchidos)[-1])))

  resultados[[2]] = preenchidos

  names(resultados) = c('serie', "failureMatrix")

  x1 = reshape2::melt(preenchidos, id.vars = period)

  g = ggplot2::ggplot(x1, ggplot2::aes(x1[,1], variable, fill = value)) +
    ggplot2::geom_tile(color="black") +
    ggplot2::scale_fill_manual(values = c("white", "green")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")

  resultados[[3]] = g
  names(resultados[[3]]) = "plot"

  print(g)

  return(resultados)
}
