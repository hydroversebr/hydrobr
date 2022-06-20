#' Performs the Wald-Wolfowitz runs test of randomness for list of continuous stations data.
#'
#' @encoding UTF-8
#'
#' @description Performs the Wald-Wolfowitz runs test of randomness for list of continuous stations data.
#'
#'
#' @param resultFillorStatistics list with elements containing continuous stations data;
#' @param by_month logical. if by_mounth = TRUE, run test is performed for each month.
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
#' Qmean_years = flowStatistics(final_data, statistics = "Qmean")
#'
#' #MannKendall test
#' RunTest(resultFillorStatistics = Qmean_years, by_month = FALSE)
#'
#'
#' @export
#'
RunTest <- function(resultFillorStatistics, by_month = TRUE) { # se by_month for igual a TRUE, faz o RunTest por mÊs da série mensal. Caso contrário na série mensal ou anual

  ## Verification if arguments are in the desired format
  # is StatisticsResult an outcome from rainStatistics or flowStatistics function?

  if (!attributes(resultFillorStatistics)$class[2] %in% c('flowStatistics','rainStatistics', 'fillGaps')) {
    stop(
      call. = FALSE,
      '`StatisticsResult` does not inherit attribute "flowStatistics" or "rainStatistics".
       The outcome from the flowStatistics() or rainStatistics() function should be passed as argument'
    )
  }

  ## verify by_month parameter

  resultFillorStatistics = resultFillorStatistics$series

  #identify type of serie (annual or monthly)

  if (names(resultFillorStatistics[[1]])[2] == "waterYear"){
    period = "waterYear"
  } else {period = "monthWaterYear"}

  if (by_month == TRUE & period == "waterYear" | !is.logical(by_month) | !length(by_month) == 1) {
    stop(
      call. = FALSE,
      '`by_month` should be a logical vector of length == 1 (TRUE or FALSE).
       if `resultFillorStatistics` is an annual series list, by_month is necessarily `FALSE`.
       See arguments details for more information'
    )
  }

  if (by_month == FALSE) { # for annual and monthly series compute trendness


    pvalueRun <- as.numeric()
    station <- as.numeric()

    for (i in 1:length(resultFillorStatistics)) {
      testRun <- resultFillorStatistics[[i]] %>%
        dplyr::pull(3) %>%
        randtests::runs.test(plot = FALSE)
      pvalueRun[i] <- round(testRun$p.value, 3)
      station[i] <- names(resultFillorStatistics)[i]
    }

    testRun <- tibble::as_tibble(apply(cbind(station, pvalueRun), 2, as.numeric))
    testRun # valores menores que 0.05 rejeita hipótese nula (não há tendência)

  } else { # fazer teste de Run por mês

    pvalueRun <- as.numeric()
    estacao <- as.numeric()
    testRun <- list()

    for (i in 1:length(resultFillorStatistics)) {
      testRun[[i]] <- resultFillorStatistics[[i]] %>%
        stats::setNames(c("station_code", "monthWaterYear", "value")) %>%
        dplyr::mutate(month = lubridate::month(monthWaterYear)) %>% # criar mês
        dplyr::group_by(month) %>% # agrupar por mÊs
        dplyr::group_map(~ randtests::runs.test(.x$value, plot = FALSE)) %>% # realziar teste de Run no mÊs
        lapply(FUN = function(x) x$p.value) %>% # pegar p-value
        unlist() %>% # converter para vetor
        base::round(3) %>% # arredondar
        dplyr::bind_cols(resultFillorStatistics[[i]] %>% # concatenar vetor de pvalue com mês associado
                    dplyr::mutate(month = lubridate::month(monthWaterYear)) %>%
                    dplyr::pull(month) %>%
                    base::unique()) %>%
        stats::setNames(c(paste("pval_Rtest_", names(resultFillorStatistics)[i], sep = ""), "month")) %>% # renomear colunas
        dplyr::arrange(month) %>% # ordenar por mês
        dplyr::mutate(month = as.character(lubridate::month(month, label = TRUE))) %>% # converter número do mês em nome do mÊs
        dplyr::select(month, dplyr::everything()) %>%  # ordenar colunas %>%
        suppressMessages()
    }

    testRun <- Reduce(function(x, y) dplyr::full_join(x, y, by = "month"), testRun) # criar data.frame com todas as estações
    testRun

  }
  return(testRun)
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("month"))
