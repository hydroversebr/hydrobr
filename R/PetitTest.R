#' Performs the MannKendall trend test for list of continuous stations data.
#'
#' @encoding UTF-8
#'
#' @description Performs the MannKendall trend test for list of continuous stations data.
#'
#'
#' @param resultFillorStatistics list with elements containing continuous stations data.
#' @param by_month logical. if by_mounth = TRUE, MannKendall test is performed for each month. default = FALSE.
#' @param plotGraph logical. defalt = FALSE
#' @param dirSub string. directory path to save plots. default = "./petitTestGraph".
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
#' MKTest(resultFillorStatistics = Qmean_years, by_month = FALSE)
#'
#'
#' @export
#' @importFrom rlang :=
PetitTest <- function(resultFillorStatistics, by_month = FALSE, plotGraph = FALSE, dirSub = "./petitTestGraph") { # se by_month for igual a TRUE, faz o RunTest por mÊs da série mensal. Caso contrário na série mensal ou anual

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

  resultFillorStatistics2 <- lapply(resultFillorStatistics, function(x) { # nome da coluna modificado para que a função funcione para vazao e precipitacao
    names(x) <- c(names(resultFillorStatistics[[1]])[1], "period", "value")
    x
  })

  # if(names(resultFillorStatistics[[1]])[2] == "monthWaterYear"){
  #
  #   resultFillorStatistics2 = resultFillorStatistics2 %>%
  #     lapply(function(x) x %>% dplyr::mutate(period = as.Date(paste("01", period, sep = "-"),
  #                                                             tryFormats = "%d-%m-%Y")))
  # }

  if (by_month == FALSE) { # for time series

    if (dir.exists(dirSub) == FALSE) {
      dir.create(dirSub, recursive = TRUE)
    }

    pvaluePT <- as.numeric()
    station <- as.numeric()

    i = 1
    for (i in 1:length(resultFillorStatistics2)) {

      # pvalue

      testPT <- resultFillorStatistics2[[i]] %>%
        dplyr::pull(3) %>%
        trend::pettitt.test()

      pvaluePT[i] <- round(testPT$p.value, 3)
      station[i] <- names(resultFillorStatistics2)[i]

      # graph

      if (plotGraph == TRUE) {

        ##trendness line parameters for plot

        valuesBefore <- resultFillorStatistics2[[i]]$value[1:testPT$estimate[1]] # values before break point

        valuesAfter <- resultFillorStatistics2[[i]]$value[(testPT$estimate[1] + 1):nrow(resultFillorStatistics2[[i]])] #value after break point

        meanValues <- c( # vector with mean values before and after breakpoint
          rep(mean(valuesBefore), length(valuesBefore)),
          rep(mean(valuesAfter), length(valuesAfter))
        )

        # graph parameters

        #identify type of data

        variavel = names(resultFillorStatistics[[i]])[3]

        if (substr(variavel, 1, 1) == "Q") {

          ylab <- paste(substring(variavel, 1, nchar(variavel)-5), "(m3_s)")

        } else {

          ylab <- paste(substring(variavel, 1, nchar(variavel)-3), "(mm)")
        }



        #plot

        resultFillorStatistics2[[i]] %>%
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
            subtitle = names(resultFillorStatistics2)[i]
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
                     names(resultFillorStatistics[[1]][2]),
                     "_",
                     names(resultFillorStatistics2)[i],
                     ".png",
                     sep = ""
        ), dpi = 200, width = 14, height = 7)
      }
    }

    testPT <- dplyr::as_tibble(apply(cbind(station, pvaluePT), 2, as.numeric))
    testPT


  } else { # runtest for each month


    pvaluePT <- as.numeric()
    station <- as.numeric()
    testPT <- base::month.abb[1:12] %>%
      dplyr::as_tibble() %>%
      stats::setNames("month")

    j = 1
    i = 1
    for (j in 1:length(resultFillorStatistics$series)) {

      for (i in 1:12) {

        station_name = unique(resultFillorStatistics2[[j]]$station_code)

        #df with monthly i data of station j

        monthdf <- resultFillorStatistics2[[j]] %>%
          dplyr::mutate(month = lubridate::month(period)) %>%
          dplyr::filter(month == i)

        #petit test for monthly i data of station j

        testPT_m <- monthdf %>%
          dplyr::pull(3) %>%
          trend::pettitt.test()


        pvaluePT[i] <- round(testPT_m$p.value, 3)

        station[i] <- station_name

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

         variavel = names(resultFillorStatistics[[1]][[1]])[3]

          if (substr(variavel, 1, 1) == "Q") {

            ylab <- paste(substring(variavel, 1, nchar(variavel)-5), "(m3_s)")

          } else {

            ylab <- paste(substring(variavel, 1, nchar(variavel)-3), "(mm)")
          }

          #plot

          resultFillorStatistics2[[j]] %>%
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
              subtitle = names(resultFillorStatistics2)[j]
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
