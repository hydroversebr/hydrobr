#' Performs the Wald-Wolfowitz runs test of randomness for list of continuous stations data.
#'
#' @encoding UTF-8
#'
#' @description Performs the Wald-Wolfowitz runs test of randomness for list of continuous stations data.
#'
#'
#' @param dfStationlist list with elements containing continuous stations data;
#' @param by_month logical. if by_mounth = TRUE, run test is performed for each month.
#'
#' @return p-value for each continuous stations data
#'
#' @references
#' randtest package (https://cran.r-project.org/web/packages/randtests/index.html)
#'
#' @examplesIf interactive()
#'
#' RunTest(dfStationlist = data, by_month = FALSE)
#'
#'
#' @export

RunTest <- function(dfStationlist, by_month = FALSE) { # se by_month for igual a TRUE, faz o RunTest por mÊs da série mensal. Caso contrário na série mensal ou anual


  if (by_month == FALSE) { # se os a estatística for anual


    pvalueRun <- as.numeric()
    station <- as.numeric()

    for (i in 1:length(dfStationlist)) {
      testRun <- dfStationlist[[i]] %>%
        dplyr::pull(2) %>%
        randtests::runs.test(plot = FALSE)
      pvalueRun[i] <- round(testRun$p.value, 3)
      station[i] <- names(dfStationlist)[i]
    }

    testRun <- tibble::as_tibble(apply(cbind(station, pvalueRun), 2, as.numeric))
    testRun # valores menores que 0.05 rejeita hipótese nula (não há tendência)

  } else { # fazer teste de Run por mês

    pvalueRun <- as.numeric()
    estacao <- as.numeric()
    testRun <- list()

    for (i in 1:length(dfStationlist)) {
      testRun[[i]] <- dfStationlist[[i]] %>%
        stats::setNames(c("monthWaterYear", "value")) %>%
        dplyr::mutate(month = lubridate::month(monthWaterYear)) %>% # criar mês
        dplyr::group_by(month) %>% # agrupar por mÊs
        dplyr::group_map(~ runs.test(.x$value, plot = FALSE)) %>% # realziar teste de Run no mÊs
        lapply(FUN = function(x) x$p.value) %>% # pegar p-value
        unlist() %>% # converter para vetor
        base::round(3) %>% # arredondar
        dplyr::bind_cols(dfStationlist[[i]] %>% # concatenar vetor de pvalue com mês associado
                    dplyr::mutate(month = lubridate::month(monthWaterYear)) %>%
                    dplyr::group_by(month) %>%
                    dplyr::pull(month) %>%
                    .[c(1:12)]) %>%
        stats::setNames(c(paste("pval_Rtest_", names(dfStationlist)[i], sep = ""), "month")) %>% # renomear colunas
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
