#' Performs the MannKendall trend test for list of continuous stations data.
#'
#' @encoding UTF-8
#'
#' @description Performs the MannKendall trend test for list of continuous stations data.
#'
#'
#' @param dfStationlist list with elements containing continuous stations data;
#' @param by_month logical. if by_mounth = TRUE, MannKendall test is performed for each month.
#'
#' @return p-value for each continuous stations data
#'
#' @references
#' randtest package (https://cran.r-project.org/web/packages/randtests/index.html)
#'
#' @examplesIf interactive()
#'
#'#' inv <- inventory(
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
#' #MannKendall test
#' MKTest(dfStationlist = Qmean_years, by_month = FALSE)
#'
#'
#' @export
MKTest <- function(listStationData, by_month = FALSE) { # se by_month for igual a TRUE, faz o MKTest por mÊs da série mensal. Caso contrário na série mensal ou anual


  if (by_month == FALSE) { # se os a estatística for anual


    pvalueMK <- as.numeric()
    station <- as.numeric()

    for (i in 1:length(listStationData)) {
      testMK <- listStationData[[i]] %>%
        dplyr::pull(2) %>%
        Kendall::MannKendall()

      pvalueMK[i] <- round(testMK$sl[1],3)
      station[i] <- names(listStationData)[i]
    }

    testMK <- tibble::as_tibble(apply(cbind(station, pvalueMK), 2, as.numeric))
    testMK # valores menores que 0.05 rejeita hipótese nula (não há tendência)

  } else { # fazer teste de Run por mês

    testMK <- list()

    for (i in 1:length(listStationData)) {
      testMK[[i]] <- listStationData[[i]] %>%
        stats::setNames(c("monthWaterYear", "value")) %>%
        dplyr::mutate(month = lubridate::month(monthWaterYear)) %>% # criar mês
        dplyr::group_by(month) %>% # agrupar por mÊs
        dplyr::group_map(~ MannKendall(.x$value)) %>% # realziar teste de Run no mÊs
        lapply(FUN = function(x) x$sl[1]) %>% # pegar p-valuetestMK$sl[1]
        unlist() %>% # converter para vetor
        round(3) %>% # arredondar
        dplyr::bind_cols(listStationData[[i]] %>% # concatenar vetor de pvalue com mês associado
                           dplyr::mutate(month = lubridate::month(monthWaterYear)) %>%
                           dplyr::group_by(month) %>%
                           dplyr::pull(month) %>%
                           .[c(1:12)]) %>%
        stats::setNames(c(paste("pval_MKtest_", names(listStationData)[i], sep = ""), "month")) %>% # renomear colunas
        dplyr::arrange(month) %>% # ordenar por mês
        dplyr::mutate(month = as.character(lubridate::month(month, label = TRUE))) %>% # converter número do mês em nome do mÊs
        dplyr::select(month, dplyr::everything()) %>%  # ordenar colunas %>%
        suppressMessages()
    }

    testMK <- Reduce(function(x, y) dplyr::full_join(x, y, by = "month"), testMK) # criar data.frame com todas as estações
    testMK

  }
  return(testMK)
}
