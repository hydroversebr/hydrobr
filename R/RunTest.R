#' Wald-Wolfowitz runs trend test
#'
#' @encoding UTF-8
#'
#' @description Performs the Wald-Wolfowitz runs trend test for annual or monthly rainfall (or streamflow) times series
#'
#'
#' @param dfSeriesFromFillorSerieStatisticsFunc tibble containing annual or monthly series of all stations;
#' @param byMonth logical. if byMounth = TRUE, MannKendall test is performed for each month.
#'
#' @return p-value for each continuous stations data
#'
#' @references
#' randtest package (https://cran.r-project.org/web/packages/randtests/index.html)
#'
#' @examplesIf interactive()
#'
#' # Fech a inventory of fluviometric stations for the state of Minas Gerais
#'
#' inv <- inventory(
#'   states = "MINAS GERAIS",
#'   stationType = "flu",
#'   as_sf = TRUE,
#'   aoi = NULL)
#'
#' # Download the first 10 stations from the inventory
#'
#' s_data <- stationsData(
#'   inventoryResult = inv[1:10,],
#'   deleteNAstations = TRUE)
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
#' Qmean_years = seriesStatistics(final_data, statistics = "Qmean")
#'
#' #MannKendall test
#' RunTest(dfSeriesFromFillorSerieStatisticsFunc = Qmean_years$df_series, byMonth = FALSE)
#'
#'
#' @export
#'
RunTest <- function(dfSeriesFromFillorSerieStatisticsFunc, byMonth = FALSE) { # se byMonth for igual a TRUE, faz o RunTest por mÊs da série mensal. Caso contrário na série mensal ou anual

  # ## Verification if arguments are in the desired format
  # # is StatisticsResult an outcome from rainStatistics or flowStatistics function?
  #
  # if (!attributes(dfSeriesFromFillorSerieStatisticsFunc)$class[2] %in% c('flowStatistics','rainStatistics', 'fillGaps')) {
  #   stop(
  #     call. = FALSE,
  #     '`StatisticsResult` does not inherit attribute "flowStatistics" or "rainStatistics".
  #      The outcome from the flowStatistics() or rainStatistics() function should be passed as argument'
  #   )
  # }
  ## verify byMonth parameter

  dfSeriesFromFillorSerieStatisticsFunc = dfSeriesFromFillorSerieStatisticsFunc %>%
    split(dfSeriesFromFillorSerieStatisticsFunc$station_code)

  #identify type of serie (annual or monthly)

  ## verify byMonth parameter

  #identify type of serie (annual or monthly)

  if (names(dfSeriesFromFillorSerieStatisticsFunc[[1]])[2] == "waterYear"){
    period = "waterYear"
  } else {period = "monthWaterYear"}

  if (byMonth == TRUE & period == "waterYear" | !is.logical(byMonth) | !length(byMonth) == 1) {
    stop(
      call. = FALSE,
      '`byMonth` should be a logical vector of length == 1 (TRUE or FALSE).
       if `dfSeriesFromFillorSerieStatisticsFunc` is an annual series list, byMonth is necessarily `FALSE`.
       See arguments details for more information'
    )
  }

  dfSeriesFromFillorSerieStatisticsFunc2 <- lapply(dfSeriesFromFillorSerieStatisticsFunc, function(x) { # nome da coluna modificado para que a função funcione para vazao e precipitacao
    names(x) <- c("station_code", "period", "value")
    x
  })

  if (byMonth == FALSE) { # for annual and monthly series compute trendness


    pvalueRun <- as.numeric()
    station <- as.numeric()

    for (i in 1:length(dfSeriesFromFillorSerieStatisticsFunc)) {
      testRun <- dfSeriesFromFillorSerieStatisticsFunc[[i]] %>%
        dplyr::pull(3) %>%
        randtests::runs.test(plot = FALSE)
      pvalueRun[i] <- round(testRun$p.value, 3)
      station[i] <- names(dfSeriesFromFillorSerieStatisticsFunc)[i]
    }

    testRun <- tibble::as_tibble(apply(cbind(station, pvalueRun), 2, as.numeric))
    testRun # valores menores que 0.05 rejeita hipótese nula (não há tendência)
    names(testRun) = c("station_code", "pvalueRun")

  } else { # fazer teste de Run por mês

    pvalueRun <- as.numeric()
    estacao <- as.numeric()
    testRun <- list()

    for (i in 1:length(dfSeriesFromFillorSerieStatisticsFunc)) {
      testRun[[i]] <- dfSeriesFromFillorSerieStatisticsFunc[[i]] %>%
        stats::setNames(c("station_code", "monthWaterYear", "value")) %>%
        dplyr::mutate(month = lubridate::month(monthWaterYear)) %>% # criar mês
        dplyr::group_by(month) %>% # agrupar por mÊs
        dplyr::group_map(~ randtests::runs.test(.x$value, plot = FALSE)) %>% # realziar teste de Run no mÊs
        lapply(FUN = function(x) x$p.value) %>% # pegar p-value
        unlist() %>% # converter para vetor
        base::round(3) %>% # arredondar
        dplyr::bind_cols(dfSeriesFromFillorSerieStatisticsFunc[[i]] %>% # concatenar vetor de pvalue com mês associado
                           dplyr::mutate(month = lubridate::month(monthWaterYear)) %>%
                           dplyr::pull(month) %>%
                           base::unique()) %>%
        stats::setNames(c(paste("pval_Rtest_", names(dfSeriesFromFillorSerieStatisticsFunc)[i], sep = ""), "month")) %>% # renomear colunas
        dplyr::arrange(month) %>% # ordenar por mês
        dplyr::mutate(month = as.character(lubridate::month(month, label = TRUE))) %>% # converter número do mês em nome do mÊs
        dplyr::select(month, dplyr::everything()) %>%  # ordenar colunas %>%
        suppressMessages()
    }

    testRun <- Reduce(function(x, y) dplyr::full_join(x, y, by = "month"), testRun) # criar data.frame com todas as estações
    testRun

  }

  names(testRun) = c("station_code", "pvalueRun")
  testRun$station_code = as.character(testRun$station_code)
  return(testRun)
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("month"))
