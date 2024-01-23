#' Pettitt trend test
#'
#' @encoding UTF-8
#'
#' @description Performs the Pettitt trend test for annual or monthly rainfall (or streamflow) times series
#'
#'
#' @param dfSeriesFromFillorSerieStatisticsFunc tibble containing annual or monthly series of all stations;
#' @param byMonth logical. if byMounth = TRUE, Pettitt test is performed for each month.
#' @param plotGraph logical. defalt = FALSE
#' @param dirSub string. directory path to save plots. default = "./petitTestGraph".
#'
#' @return p-value for each continuous stations data
#'
#' @references
#' trend package (https://cran.r-project.org/web/packages/trend/trend.pdf)
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
#' MKTest(dfSeriesFromFillorSerieStatisticsFunc = Qmean_years$df_series, byMonth = FALSE)
#'
#'
#' @export
#' @importFrom rlang :=
#'
PetitTest <- function(dfSeriesFromFillorSerieStatisticsFunc, byMonth = FALSE, plotGraph = FALSE, dirSub = "./petitTestGraph") { # se byMonth for igual a TRUE, faz o RunTest por mÊs da série mensal. Caso contrário na série mensal ou anual

  ## Verification if arguments are in the desired format
  # is StatisticsResult an outcome from rainStatistics or flowStatistics function?

  # if (!attributes(dfSeriesFromFillorSerieStatisticsFunc)$class[2] %in% c('flowStatistics','rainStatistics', 'fillGaps')) {
  #   stop(
  #     call. = FALSE,
  #     '`StatisticsResult` does not inherit attribute "flowStatistics" or "rainStatistics".
  #      The outcome from the flowStatistics() or rainStatistics() function should be passed as argument'
  #   )
  # }


  dfSeriesFromFillorSerieStatisticsFunc = dfSeriesFromFillorSerieStatisticsFunc %>%
    split(dfSeriesFromFillorSerieStatisticsFunc$station_code)


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

  # if(names(dfSeriesFromFillorSerieStatisticsFunc[[1]])[2] == "monthWaterYear"){
  #
  #   dfSeriesFromFillorSerieStatisticsFunc2 = dfSeriesFromFillorSerieStatisticsFunc2 %>%
  #     lapply(function(x) x %>% dplyr::mutate(period = as.Date(paste("01", period, sep = "-"),
  #                                                             tryFormats = "%d-%m-%Y")))
  # }

  if (byMonth == FALSE) { # for time series

    if (dir.exists(dirSub) == FALSE) {
      dir.create(dirSub, recursive = TRUE)
    }

    pvaluePT <- as.numeric()
    station <- as.numeric()
    dateChange = as.numeric()

    i = 1
    for (i in 1:length(dfSeriesFromFillorSerieStatisticsFunc2)) {

      # pvalue

      testPT <- dfSeriesFromFillorSerieStatisticsFunc2[[i]] %>%
        dplyr::pull(3) %>%
        stats::na.omit() %>%
        trend::pettitt.test()

      pvaluePT[i] <- round(testPT$p.value, 3)
      dateChange[i] = dfSeriesFromFillorSerieStatisticsFunc2[[i]]$period[testPT$estimate[1]] %>%
        as.character()
      station[i] <- names(dfSeriesFromFillorSerieStatisticsFunc2)[i]




      # graph

      if (plotGraph == TRUE) {

        ##trendness line parameters for plot

        valuesBefore <- dfSeriesFromFillorSerieStatisticsFunc2[[i]]$value[1:testPT$estimate[1]] # values before break point

        valuesAfter <- dfSeriesFromFillorSerieStatisticsFunc2[[i]]$value[(testPT$estimate[1] + 1):nrow(dfSeriesFromFillorSerieStatisticsFunc2[[i]])] #value after break point

        meanValues <- c( # vector with mean values before and after breakpoint
          rep(mean(valuesBefore), length(valuesBefore)),
          rep(mean(valuesAfter), length(valuesAfter))
        )

        # graph parameters

        #identify type of data

        variavel = names(dfSeriesFromFillorSerieStatisticsFunc[[i]])[3]

        if (substr(variavel, 1, 1) == "Q") {

          ylab <- paste(substring(variavel, 1, nchar(variavel)-5), "(m3_s)")

        } else {

          ylab <- paste(substring(variavel, 1, nchar(variavel)-3), "(mm)")
        }



        #plot

        dfSeriesFromFillorSerieStatisticsFunc2[[i]] %>%
          ggplot2::ggplot() +
          ggplot2::geom_point(ggplot2::aes(.data$period, .data$value, colour = "value")) +
          ggplot2::geom_line(ggplot2::aes(.data$period, .data$value, colour = "value")) +
          ggplot2::geom_line(ggplot2::aes(x = .data$period, y = meanValues, color = "tend")) +
          ggplot2::scale_color_manual(name = "Legend",
                                      values = c("value" = "darkblue", "tend" = "red"),
                                      labels = c("Streamflow", "Tendency line"))+
          ggplot2::labs(
            x = period,
            y = ylab,
            title = paste("Petit test (p.value = ", round(testPT$p.value, 3), ")", sep = ""),
            subtitle = names(dfSeriesFromFillorSerieStatisticsFunc2)[i]
          ) +
          ggplot2::theme_bw(base_size = 16) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
          ggplot2::theme(
            legend.position = c(1, 1),
            legend.direction = "horizontal",
            legend.justification = c(1, 0),
            legend.key.height = ggplot2::unit(1, 'cm'),
            legend.key.width = ggplot2::unit(1, 'cm'),
            legend.text = ggplot2::element_text(size=14)
          )


        ggplot2::ggsave(paste(dirSub,
                              "/",
                              names(dfSeriesFromFillorSerieStatisticsFunc[[1]][2]),
                              "_",
                              names(dfSeriesFromFillorSerieStatisticsFunc2)[i],
                              ".png",
                              sep = ""
        ), dpi = 200, width = 14, height = 7)
      }
    }

    testPT <- dplyr::as_tibble(apply(cbind(station, pvaluePT, dateChange), 2, as.numeric))
    testPT


  } else { # runtest for each month


    pvaluePT <- as.numeric()
    station <- as.numeric()
    dateChange = as.numeric()

    testPT <- base::month.abb[1:12] %>%
      dplyr::as_tibble() %>%
      stats::setNames("month")

    j = 1
    for (j in 1:length(dfSeriesFromFillorSerieStatisticsFunc2)) {
      i = 1
      for (i in 1:12) {

        station_name = unique(dfSeriesFromFillorSerieStatisticsFunc2[[j]]$station_code)

        #df with monthly i data of station j

        monthdf <- dfSeriesFromFillorSerieStatisticsFunc2[[j]] %>%
          dplyr::mutate(month = lubridate::month(period)) %>%
          dplyr::filter(month == i)

        #petit test for monthly i data of station j

        testPT_m <- monthdf %>%
          dplyr::pull(3) %>%
          stats::na.omit() %>%
          trend::pettitt.test()


        pvaluePT[i] <- round(testPT_m$p.value, 3)

        station[i] <- station_name

        dateChange[i] = dfSeriesFromFillorSerieStatisticsFunc2[[i]]$period[testPT_m$estimate[1]] %>%
          as.character()

        # gráfico

        if (plotGraph == TRUE) {

          dir_path = paste(dirSub, "/monthly/", station_name, sep = "")

          if (dir.exists(dir_path) == FALSE) {
            dir.create(dir_path, recursive = TRUE)
          }

          valuesBefore <- monthdf$value[1:testPT_m$estimate[1]] # values before break point

          valuesAfter <- monthdf$value[(testPT_m$estimate[1] + 1):nrow(monthdf)] #value after break point

          meanValues <- c( # vector with mean values before and after breakpoint
            rep(mean(valuesBefore), length(valuesBefore)),
            rep(mean(valuesAfter), length(valuesAfter))
          )

          # graph parameters

          variavel = names(dfSeriesFromFillorSerieStatisticsFunc[[1]][[1]])[3]

          if (substr(variavel, 1, 1) == "Q") {

            ylab <- paste(substring(variavel, 1, nchar(variavel)-5), "(m3_s)")

          } else {

            ylab <- paste(substring(variavel, 1, nchar(variavel)-3), "(mm)")
          }

          #plot

          dfSeriesFromFillorSerieStatisticsFunc2[[j]] %>%
            dplyr::mutate(month = lubridate::month(period)) %>%
            dplyr::filter(month == i) %>%
            ggplot2::ggplot() +
            ggplot2::geom_point(ggplot2::aes(.data$period, .data$value, colour = "value")) +
            ggplot2::geom_line(ggplot2::aes(.data$period, .data$value, colour = "value")) +
            ggplot2::geom_line(ggplot2::aes(x = .data$period, y = meanValues, color = "tend")) +
            ggplot2::scale_color_manual(name = "Legend",
                                        values = c("value" = "darkblue", "tend" = "red"),
                                        labels = c("Streamflow", "Tendency line"))+
            ggplot2::labs(
              x = period,
              y = ylab,
              title = paste("Petit test (p.value = ", round(testPT_m$p.value, 3), ")", sep = ""),
              subtitle = names(dfSeriesFromFillorSerieStatisticsFunc2)[j]
            ) +
            ggplot2::theme_bw(base_size = 16) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
            ggplot2::theme(
              legend.position = c(1, 1),
              legend.direction = "horizontal",
              legend.justification = c(1, 0),
              legend.key.height = ggplot2::unit(1, 'cm'),
              legend.key.width = ggplot2::unit(1, 'cm'),
              legend.text = ggplot2::element_text(size=14)
            )


          ggplot2::ggsave(paste(dir_path,
                                "/",
                                substring(variavel, 1, nchar(variavel)-5),
                                "_",
                                station_name,
                                "_",
                                i,
                                ".png",
                                sep = ""
          ), dpi = 200, width = 14, height = 7)

        }
      }

      testPT <- testPT %>%
        dplyr::mutate(!!paste("pval_PeTtest_", station_name, sep = "") := pvaluePT)
    }
  }

  return(testPT)
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("month"))
