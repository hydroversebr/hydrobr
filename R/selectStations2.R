selectStation2 = function (organizeResult, mode = "yearly", maxMissing = 10, 
          minYears = 15, month = 1, iniYear = NULL, finYear = NULL, 
          consistedOnly = FALSE, plot = TRUE) {
  if (!attributes(organizeResult)$hydrobr_class %in% "organize") {
    stop(call. = FALSE, "`organizeResult` does not inherit attribute \"organize\".\n       The outcome from the organize() function should be passed as argument")
  }
  if (!is.character(mode) | length(mode) != 1) {
    stop(call. = FALSE, "`mode` should be a character vector of length == 1 (either \"yearly\" or \"monthly\").\n       See arguments details for more information.")
  }
  if (!mode %in% c("yearly", "monthly")) {
    stop(call. = FALSE, "`mode` should be a character vector of length == 1 (either \"yearly\" or \"monthly\").\n       See arguments details for more information.")
  }
  if (!is.numeric(maxMissing) | length(maxMissing) != 1) {
    stop(call. = FALSE, "`maxMissing` should be a numeric vector of length == 1 (ex: 10).\n       See arguments details for more information.")
  }
  if (!is.numeric(minYears) | length(minYears) != 1) {
    stop(call. = FALSE, "`minYears` should be a numeric vector of length == 1 (ex: 15).\n       See arguments details for more information.")
  }
  if (!is.numeric(month) | length(month) != 1) {
    stop(call. = FALSE, "`month` should be a numeric vector of length == 1.\n       `month` should represent a month of the year, e.g., 1 (January).\n       See arguments details for more information.")
  }
  if (!is.null(iniYear)) {
    if (!is.numeric(iniYear) | length(iniYear) != 1) {
      stop(call. = FALSE, "`iniYear` should be a numeric vector of length == 1 (ex: 1990).\n         See arguments details for more information.")
    }
  }
  if (!is.null(finYear)) {
    if (!is.numeric(finYear) | length(finYear) != 1) {
      stop(call. = FALSE, "`finYear` should be a numeric vector of length == 1 (ex: 2020).\n         See arguments details for more information.")
    }
  }
  if (!is.logical(consistedOnly) | length(consistedOnly) != 
      1) {
    stop(call. = FALSE, "`consistedOnly` should be logical and have length == 1 (either TRUE or FALSE).")
  }
  
  if (names(organizeResult[[1]])[4] == "streamflow_m3_s") {
    stationType <- "flu"
  }  else {
    stationType <- "plu"
  }
  varName <- switch(stationType, plu = "rainfall_mm", flu = "stream_flow_m3_s")
  organizeResultDF <- organizeResult %>% do.call(what = dplyr::bind_rows) %>% 
    dplyr::rename(value = 4)
  if (consistedOnly == TRUE) {
    organizeResultDF <- organizeResultDF %>% dplyr::mutate(value = dplyr::if_else(.data$consistency_level == 
                                                                                    2, .data$value, NA_real_))
  }
  if (!is.null(iniYear)) {
    organizeResultDF <- organizeResultDF %>% dplyr::filter(lubridate::year(.data$date) >= 
                                                             iniYear)
  }
  if (!is.null(finYear)) {
    organizeResultDF <- organizeResultDF %>% dplyr::filter(lubridate::year(.data$date) <= 
                                                             finYear)
  }
  
  selectDataResult <- split(organizeResultDF, organizeResultDF$station_code) %>%
  purrr::map(.f = function(df) {
    dplyr::tibble(date = seq(
      from = paste0(
        min(lubridate::year(df$date)),
        stringr::str_pad(month,
          side = "left", pad = 0,
          width = 2
        ), "01"
      ) %>% as.Date(.data, format = "%Y%m%d"),
      to = paste0(max(lubridate::year(df$date))+1, stringr::str_pad(month,
        side = "left", pad = 0, width = 2
      ), "01") %>%
        as.Date(.data, format = "%Y%m%d"), by = 1
    )) %>%
      dplyr::mutate(
        civilYear = lubridate::year(.data$date),
        monthCivilYear = as.Date(paste("01", stringr::str_pad(lubridate::month(.data$date),
          side = "left", pad = 0, width = 2
        ), lubridate::year(.data$date),
        sep = "-"
        ), tryFormats = "%d-%m-%Y"), waterYear = lfstat::water_year(.data$date,
          origin = month, assign = "start"
        ) %>% as.character() %>%
          as.numeric(), monthWaterYear = as.Date(paste("01",
          stringr::str_pad(lubridate::month(.data$date),
            side = "left", pad = 0, width = 2
          ), .data$waterYear,
          sep = "-"
        ), tryFormats = "%d-%m-%Y")
      ) %>%
      dplyr::left_join(df, by = "date") %>%
      dplyr::mutate(station_code = unique(df$station_code))
  }) %>%
  do.call(what = dplyr::bind_rows) %>%
    dplyr::filter(date<as.Date(paste(finYear+1, "-01", "-01", sep = ""), tryFormats = "%Y-%m-%d"))
  
  
options(warn = -1)
  
  if (mode == "yearly") {
    
    failureMatrix <- selectDataResult %>%
  dplyr::group_by_at(c(
    "station_code",
    "waterYear"
  )) %>%
  dplyr::mutate(missing = 100 * sum(is.na(.data$value)) / dplyr::n()) %>%
  dplyr::select(
    dplyr::matches("^waterYear$"),
    dplyr::matches("station_code"),
    dplyr::matches("missing")
  ) %>%
  dplyr::mutate(missing = .data$missing <= maxMissing) %>%
  dplyr::distinct() %>%
  dplyr::group_by_at("station_code") %>%
  dplyr::filter(sum(.data$missing == T) >= minYears) %>%
  tidyr::pivot_wider(
    names_from = .data$station_code,
    values_from = .data$missing
  )

missingMatrix <- selectDataResult %>%
  dplyr::group_by_at(c("station_code", "waterYear")) %>%
  dplyr::mutate(missing = 100 * sum(is.na(.data$value)) / dplyr::n()) %>%
  dplyr::select(
    dplyr::matches("^waterYear$"),
    dplyr::matches("station_code"),
    dplyr::matches("missing")
  ) %>%
  dplyr::distinct() %>%
  dplyr::filter(.data$station_code %in%
    names(failureMatrix)[-1]) %>%
  tidyr::pivot_wider(
    names_from = .data$station_code,
    values_from = .data$missing
  )

g <- selectDataResult %>%
  dplyr::group_by_at(c("station_code", "waterYear")) %>%
  dplyr::mutate(consistency_level = dplyr::if_else(is.na(.data$consistency_level), 0, as.numeric(.data$consistency_level))) %>%
  dplyr::summarise(
    consisted = round(100 * sum(.data$consistency_level == 2) / dplyr::n(), 0) == 100,
    missing = (100 * sum(is.na(.data$value)) / dplyr::n()) < maxMissing,
    .groups = "drop"
  ) %>%
  dplyr::filter(.data$station_code %in% names(failureMatrix)[-1])

# label = if(length(unique(g$missing))>1){
#
#   c(paste0(">  ", maxMissing, "% missing"),
#     paste0("<= ", maxMissing, "% missing"))
#
# } else {
#     paste0("<= ", maxMissing, "% missing")
#   }

plotg <- g %>%
  ggplot2::ggplot(ggplot2::aes(
    x = .data$waterYear,
    y = .data$station_code,
    fill = .data$consisted,
    alpha = .data$missing
  )) +
  ggplot2::geom_tile(color = "black") +
  ggplot2::scale_fill_manual(
    name = "consistency level",
    values = c("FALSE" = "#f8766d", "TRUE" = "#00b0f6"),
    labels = c("Raw", "Consisted")
  ) +
  ggplot2::scale_alpha_discrete(
    name = "(transparency)",
    range = if (length(unique(g$missing)) > 1) {
      c(0.5, 1)
    } else {
      c(1, 0.5)
    },
    labels = if (length(unique(g$missing)) > 1) {
      c(
        paste0(">  ", maxMissing, "% missing"),
        paste0("<= ", maxMissing, "% missing")
      )
    } else {
      c(
        paste0("<= ", maxMissing, "% missing"),
        paste0(">  ", maxMissing, "% missing")
      )
    }
  ) +
  ggplot2::theme_bw()
      # ggplot2::scale_fill_distiller(name = "consistency level", 
      #                               palette = "Spectral",
      #                               direction = 1,
      #                               breaks = c(0, 50, 100),
      #                               labels = c("(100%) Raw","(50%/50%)","(100%) Consisted"))+
  
      
  } else {
    
    keepStations <- selectDataResult %>%
  dplyr::group_by_at(c(
    "station_code",
    "monthWaterYear"
  )) %>%
  dplyr::mutate(missing = 100 *
    sum(is.na(.data$value)) / dplyr::n()) %>%
  dplyr::select(
    dplyr::matches("monthWaterYear"),
    dplyr::matches("station_code"), dplyr::matches("missing")
  ) %>%
  dplyr::distinct() %>%
  dplyr::filter(.data$missing <=
    maxMissing) %>%
  dplyr::mutate(month = lubridate::month(monthWaterYear)) %>%
  dplyr::group_by_at(c("station_code", "month")) %>%
  dplyr::select(dplyr::matches("station_code"), dplyr::matches("^month$")) %>%
  dplyr::filter(dplyr::n() >= minYears) %>%
  dplyr::distinct() %>%
  dplyr::group_by_at("station_code") %>%
  dplyr::select(dplyr::matches("station_code")) %>%
  dplyr::filter(dplyr::n() == 12) %>%
  dplyr::distinct()
    
failureMatrix <- selectDataResult %>%
  dplyr::filter(.data$station_code %in%
    keepStations$station_code) %>%
  dplyr::group_by_at(c(
    "station_code",
    "monthWaterYear"
  )) %>%
  dplyr::mutate(missing = 100 *
    sum(is.na(.data$value)) / dplyr::n()) %>%
  dplyr::select(
    dplyr::matches("monthWaterYear"),
    dplyr::matches("station_code"), dplyr::matches("missing")
  ) %>%
  dplyr::mutate(missing = .data$missing <= maxMissing) %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(
    names_from = .data$station_code,
    values_from = .data$missing
  )

missingMatrix <- selectDataResult %>%
  dplyr::filter(.data$station_code %in%
    keepStations$station_code) %>%
  dplyr::group_by_at(c(
    "station_code",
    "monthWaterYear"
  )) %>%
  dplyr::mutate(missing = 100 *
    sum(is.na(.data$value)) / dplyr::n()) %>%
  dplyr::select(
    dplyr::matches("monthWaterYear"),
    dplyr::matches("station_code"), dplyr::matches("missing")
  ) %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(
    names_from = .data$station_code,
    values_from = .data$missing
  )


g <- selectDataResult %>%
  dplyr::filter(.data$station_code %in%
    keepStations$station_code) %>%
  dplyr::group_by_at(c(
    "station_code",
    "monthWaterYear"
  )) %>%
  dplyr::mutate(consistency_level = dplyr::if_else(is.na(.data$consistency_level),
    0, as.numeric(.data$consistency_level)
  )) %>%
  dplyr::summarise(
    consisted = round(100 * sum(.data$consistency_level == 2) / dplyr::n(), 0) == 100,
    missing = (100 * sum(is.na(.data$value)) / dplyr::n()) < maxMissing,
    .groups = "drop")

plotg = g %>%
  ggplot2::ggplot(ggplot2::aes(
    x = .data$monthWaterYear,
    y = .data$station_code,
    fill = .data$consisted,
    alpha = .data$missing
  )) +
  ggplot2::geom_tile(color = "black") +
  ggplot2::scale_fill_manual(
    name = "consistency level",
    values = c("FALSE" = "#f8766d", "TRUE" = "#00b0f6"),
    labels = c("Raw", "Consisted")
  ) +
  ggplot2::scale_alpha_discrete(
    name = "(transparency)",
    range = if (length(unique(g$missing)) > 1) {
      c(0.5, 1)
    } else {
      c(1, 0.5)
    },
    labels = if (length(unique(g$missing)) > 1) {
      c(
        paste0(">  ", maxMissing, "% missing"),
        paste0("<= ", maxMissing, "% missing")
      )
    } else {
      c(
        paste0("<= ", maxMissing, "% missing"),
        paste0(">  ", maxMissing, "% missing")
      )
    }
  ) +
  ggplot2::theme_bw()


  }
  options(warn = 1)
  selectDataResult <- selectDataResult %>% dplyr::left_join(failureMatrix %>% 
                                                              tidyr::pivot_longer(cols = 2:ncol(failureMatrix), names_to = "station_code", 
                                                                                  values_to = "keep")) %>% dplyr::filter(.data$keep == 
                                                                                                                           T) %>% dplyr::select(dplyr::matches("station_code"), 
                                                                                                                                                dplyr::matches("consistency_level"), dplyr::matches("^date$"), 
                                                                                                                                                dplyr::matches("value"), dplyr::matches("^civilYear"), 
                                                                                                                                                dplyr::matches("^monthCivilYear"), dplyr::matches("^waterYear"), 
                                                                                                                                                dplyr::matches("^monthWaterYear"))
  if (maxMissing == 0) {
    selectDataResult <- selectDataResult %>% dplyr::mutate(maxMissing = "0")
  } else {
    selectDataResult <- selectDataResult %>% dplyr::mutate(maxMissing = paste0("<= ", 
                                                                               maxMissing, "%"))
  }
  names(selectDataResult)[4] <- varName
  
  if (plot == T) {
    print(plotg)
  }
  out <- list(split(selectDataResult, selectDataResult$station_code), 
              failureMatrix, missingMatrix, plotg)
  names(out) <- c("series", "failureMatrix", "missingMatrix", 
                  "plot")
  class(out) <- c(class(out), "selectData")
  return(out)
}
