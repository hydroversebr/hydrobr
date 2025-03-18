#' Bootstrap Pettitt trend test
#'
#' @encoding UTF-8
#'
#' @description Performs the Bootstrap Pettitt trend test for annual or monthly rainfall (or streamflow) times series
#'
#'
#' @param dfSeriesFromFillorSerieStatisticsFunc tibble containing annual or monthly series of all stations;
#' @param byMonth logical. if byMounth = TRUE, Pettitt test is performed for each month;
#' @param plotGraph logical. defalt = FALSE;
#' @param dirSub string. directory path to save plots. default = "./pettittTestGraph";
#' @param ylab character. ylab description, i.e, 'Qmean (m³/s)';
#' @param legendlabel character. Legend label, i.e, 'Annual mean streamflow'.
#'
#' @return p-value for each continuous stations data
#'
#' @references
#'
#' Based on "https://github.com/fabiobayer/bootpettitt"
#'
#' Bootstrap Pettitt test for detecting change points in hydroclimatological data: case study of Itaipu Hydroelectric Plant, Brazil
#' (https://www.tandfonline.com/doi/full/10.1080/02626667.2019.1632461)
#'
#'
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
#' #Bootstrap pettitt test
#' PettittBootTest(dfSeriesFromFillorSerieStatisticsFunc = Qmean_years$df_series,
#' byMonth = FALSE,
#' plotGraph = FALSE,
#' dirSub = "./petitTestGraph",
#'ylab = "Qmean (m³/s)",
#'legendlabel = "Annual mean streamflow")
#'
#'
#' @export
#' @importFrom rlang :=
#'

PettittBootTest <- function(dfSeriesFromFillorSerieStatisticsFunc,
                            byMonth = FALSE,
                            plotGraph = TRUE,
                            dirSub = "./pettittTestGraph",
                            ylab,
                            legendlabel) {
  boot.pettitttest <- function(y, B = 1000) {
    n <- length(y)
    teste1 <- trend::pettitt.test(y)
    pvalor1 <- teste1$p.value
    estat1 <- teste1$statistic
    loca <- teste1$estimate
    estat_boot <- rep(0, B)
    for (j in 1:B) {
      indice <- sample(1:n, replace = T)
      y_boot <- y[indice]
      teste_boot <- trend::pettitt.test(y_boot)
      estat_boot[j] <- teste_boot$statistic
    }
    pvalor2 <- (1 + sum(abs(estat_boot) >= abs(estat1))) / (B +
                                                              1)
    mresults <- matrix(rep(NA, 2), nrow = 2)
    colnames(mresults) <- c("p-value")
    rownames(mresults) <- c("Pettitt test", "Bootstrap Pettitt test")
    mresults[, 1] <- c(pvalor1, pvalor2)
    mresults1 <- data.frame(
      test = c("Pettitt test", "Bootstrap Pettitt test"),
      pvalue = c(pvalor1, pvalor2), probableChangePoint = c(
        loca,
        loca
      )
    )
    return(mresults1)
  }

  dataGrafico = dfSeriesFromFillorSerieStatisticsFunc %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(.[[2]]) %>%
    dplyr::ungroup() %>%
    dplyr::slice(1, dplyr::n()) %>%
    dplyr::pull(2)


  dfSeriesFromFillorSerieStatisticsFunc <- dfSeriesFromFillorSerieStatisticsFunc %>%
    split(dfSeriesFromFillorSerieStatisticsFunc$station_code)

  if (names(dfSeriesFromFillorSerieStatisticsFunc[[1]])[2] ==
      "waterYear") {
    period <- "waterYear"
  } else {
    period <- "monthWaterYear"
  }

  if (byMonth == TRUE & period == "waterYear" | !is.logical(byMonth) |
      !length(byMonth) == 1) {
    stop(call. = FALSE, "`byMonth` should be a logical vector of length == 1 (TRUE or FALSE).\n       if `dfSeriesFromFillorSerieStatisticsFunc` is an annual series list, byMonth is necessarily `FALSE`.\n       See arguments details for more information")
  }


  dfSeriesFromFillorSerieStatisticsFunc2 <- lapply(
    dfSeriesFromFillorSerieStatisticsFunc,
    function(x) {
      names(x) <- c("station_code", "period", "value")
      x
    }
  )

  if (byMonth == FALSE) {

    if (dir.exists(dirSub) == FALSE) {
      dir.create(dirSub, recursive = TRUE)
    }

    pvaluePTBoot <- as.numeric()
    station <- as.numeric()
    dateChange <- as.numeric()
    i <- 1
    for (i in 1:length(dfSeriesFromFillorSerieStatisticsFunc2)) {
      testPT <- dfSeriesFromFillorSerieStatisticsFunc2[[i]] %>%
        dplyr::pull(3) %>%
        stats::na.omit() %>%
        boot.pettitttest()
      pvaluePTBoot[i] <- round(testPT$pvalue[2], 3)
      dateChange[i] <- dfSeriesFromFillorSerieStatisticsFunc2[[i]]$period[testPT$probableChangePoint[1]] %>%
        as.character()
      station[i] <- names(dfSeriesFromFillorSerieStatisticsFunc2)[i]

      dfSeriesFromFillorSerieStatisticsFunc2[[i]] = dfSeriesFromFillorSerieStatisticsFunc2[[i]] %>%
        dplyr::mutate(date = as.Date(paste0(.[[2]], "-01", "-01"))) %>%
        padr::pad(start_val = as.Date(paste0(dataGrafico[1], "-01", "-01")),
                  end_val = as.Date(paste0(dataGrafico[2], "-01", "-01")),
                  by = "date") %>%
        dplyr::mutate("period" = lubridate::year(date)) %>%
        dplyr::select(c(1,2,3))

      if (plotGraph == TRUE) {
        valuesBefore <- dfSeriesFromFillorSerieStatisticsFunc2[[i]]$value[1:testPT$probableChangePoint[1]]
        valuesAfter <- dfSeriesFromFillorSerieStatisticsFunc2[[i]]$value[(testPT$probableChangePoint[1] +
                                                                            1):nrow(dfSeriesFromFillorSerieStatisticsFunc2[[i]])]

        meanValues <- c(
          rep(mean(valuesBefore, na.rm = TRUE), length(valuesBefore)),
          rep(mean(valuesAfter, na.rm = TRUE), length(valuesAfter))
        )

        variavel <- names(dfSeriesFromFillorSerieStatisticsFunc[[i]])[3]

        # if (substr(variavel, 1, 1) == "Q") {
        #   ylab <- paste(substring(variavel, 1, nchar(variavel) -
        #     5), "(m3_s)")
        # } else {
        #   ylab <- paste(substring(variavel, 1, nchar(variavel) -
        #     3), "(mm)")
        # }

        ylab = ylab

        dfSeriesFromFillorSerieStatisticsFunc2[[i]] %>%
          ggplot2::ggplot() +
          ggplot2::geom_point(ggplot2::aes(.data$period,
                                           .data$value,
                                           colour = "value"
          )) +
          ggplot2::geom_line(ggplot2::aes(.data$period,
                                          .data$value,
                                          colour = "value"
          )) +
          ggplot2::geom_line(ggplot2::aes(
            x = .data$period,
            y = meanValues, color = "tend"
          )) +
          ggplot2::scale_color_manual(
            name = "Legend",
            values = c(value = "darkblue", tend = "red"),
            labels = c("Mean value", legendlabel)
          ) +
          ggplot2::labs(
            x = period, y = ylab, title = paste("Pettitt test (p.value = ",
                                                round(testPT$pvalue, 3), ")",
                                                sep = ""
            ),
            subtitle = names(dfSeriesFromFillorSerieStatisticsFunc2)[i]
          ) +
          ggplot2::theme_bw(base_size = 16) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(
            angle = 90,
            vjust = 0.5, hjust = 1
          )) +
          ggplot2::theme(
            legend.position = c(
              1,
              1
            ), legend.direction = "horizontal", legend.justification = c(
              1,
              0
            ), legend.key.height = ggplot2::unit(1, "cm"),
            legend.key.width = ggplot2::unit(1, "cm"),
            legend.text = ggplot2::element_text(size = 14)
          )

        ggplot2::ggsave(
          paste(dirSub, "/", names(dfSeriesFromFillorSerieStatisticsFunc[[1]][2]),
                "_", names(dfSeriesFromFillorSerieStatisticsFunc2)[i],
                ".png",
                sep = ""
          ),
          dpi = 200, width = 14,
          height = 7
        )
      }
    }

    testPT <- dplyr::as_tibble(data.frame(station, pvaluePTBoot,dateChange)) %>% dplyr::mutate(station = as.character(station))

    testPT

    names(testPT) <- c("station_code", "pvaluePTBoot", "dateChange")
  } else {
    pvaluePTBoot <- as.numeric()
    station <- as.numeric()
    dateChange <- as.numeric()
    testPT <- base::month.abb[1:12] %>%
      dplyr::as_tibble() %>%
      stats::setNames("month")
    j <- 1
    for (j in 1:length(dfSeriesFromFillorSerieStatisticsFunc2)) {
      i <- 1
      for (i in 1:12) {
        station_name <- unique(dfSeriesFromFillorSerieStatisticsFunc2[[j]]$station_code)
        monthdf <- dfSeriesFromFillorSerieStatisticsFunc2[[j]] %>%
          dplyr::mutate(month = lubridate::month(period)) %>%
          dplyr::filter(month == i)
        testPT_m <- monthdf %>%
          dplyr::pull(3) %>%
          stats::na.omit() %>%
          boot.pettitttest()
        pvaluePTBoot[i] <- round(
          testPT_m$pvalue[2],
          3
        )
        station[i] <- station_name
        dateChange[i] <- dfSeriesFromFillorSerieStatisticsFunc2[[i]]$period[testPT$probableChangePoint[1]] %>%
          as.character()
        if (plotGraph == TRUE) {
          dir_path <- paste(dirSub, "/monthly/", station_name,
                            sep = ""
          )
          if (dir.exists(dir_path) == FALSE) {
            dir.create(dir_path, recursive = TRUE)
          }
          valuesBefore <- monthdf$value[1:testPT_m$probableChangePoint[1]]
          valuesAfter <- monthdf$value[(testPT_m$probableChangePoint[1] +
                                          1):nrow(monthdf)]
          meanValues <- c(
            rep(mean(valuesBefore), length(valuesBefore)),
            rep(mean(valuesAfter), length(valuesAfter))
          )
          variavel <- names(dfSeriesFromFillorSerieStatisticsFunc[[1]][[1]])[3]

          # if (substr(variavel, 1, 1) == "Q") {
          #   ylab <- paste(substring(variavel, 1, nchar(variavel) -
          #     5), "(m3_s)")
          # } else {
          #   ylab <- paste(substring(variavel, 1, nchar(variavel) -
          #     3), "(mm)")
          # }

          ylab = ylab

          dfSeriesFromFillorSerieStatisticsFunc2[[j]] %>%
            dplyr::mutate(month = lubridate::month(period)) %>%
            dplyr::filter(month == i) %>%
            ggplot2::ggplot() +
            ggplot2::geom_point(ggplot2::aes(.data$period,
                                             .data$value,
                                             colour = "value"
            )) +
            ggplot2::geom_line(ggplot2::aes(.data$period,
                                            .data$value,
                                            colour = "value"
            )) +
            ggplot2::geom_line(ggplot2::aes(
              x = .data$period,
              y = meanValues, color = "tend"
            )) +
            ggplot2::scale_color_manual(
              name = "Legend",
              values = c(value = "darkblue", tend = "red"),
              labels = c("Mean value", legendlabel)
            ) +
            ggplot2::labs(
              x = period, y = ylab, title = paste("Pettitt test (p.value = ",
                                                  round(testPT_m$p.value, 3), ")",
                                                  sep = ""
              ),
              subtitle = names(dfSeriesFromFillorSerieStatisticsFunc2)[j]
            ) +
            ggplot2::theme_bw(base_size = 16) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(
              angle = 90,
              vjust = 0.5, hjust = 1
            )) +
            ggplot2::theme(legend.position = c(
              1,
              1
            ), legend.direction = "horizontal", legend.justification = c(
              1,
              0
            ), legend.key.height = ggplot2::unit(
              1,
              "cm"
            ), legend.key.width = ggplot2::unit(
              1,
              "cm"
            ), legend.text = ggplot2::element_text(size = 14))
          ggplot2::ggsave(
            paste(dir_path, "/", substring(
              variavel,
              1, nchar(variavel) - 5
            ), "_", station_name,
            "_", i, ".png",
            sep = ""
            ),
            dpi = 200, width = 14,
            height = 7
          )
        }
      }
      testPT <- testPT %>% dplyr::mutate(`:=`(!!paste("pval_PeTtest_",
                                                      station_name,
                                                      sep = ""
      ), pvaluePTBoot))
    }
  }
  return(testPT)
}



if(getRversion() >= "2.15.1")  utils::globalVariables(c("month"))
