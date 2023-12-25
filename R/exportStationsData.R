#' Export Stations Data
#'
#'@description Export station data from [hydrobr::stationsData], [hydrobr::organize], [hydrobr::selectStations]

#'
#' @param stationsTimeSerieList list of tibble; lista de tibbles contendo série de dados temporal de cada estação.
#' Aceita resultado das funções [hydrobr::stationsData], [hydrobr::organize], [hydrobr::selectStations]. Para função [hydrobr::selectStations],
#' utlizar elemento da lista contendo série de dados temporais.
#'
#'@param directory character; diretório para o qual tabelas .xlsx de cada estação serão exportadas.
#'
#' @return dataframe contendo o início do mês úmido e seco.
#'
#' @details Exporta uma tabela .xlsx para cada estação na série
#'
#' @export
#'
#' @import openxlsx





exportStationsData = function(stationsTimeSerieList, directory){


  namesStationDataFlu <- c(
    "estacaocodigo", "nivelconsistencia", "data", "mediadiaria", "metodoobtencaovazoes", "maxima",
    "minima", "media", "diamaxima", "diaminima", "maximastatus", "minimastatus", "mediastatus", "mediaanual",
    "mediaanualstatus", "vazao01", "vazao02", "vazao03", "vazao04", "vazao05", "vazao06", "vazao07", "vazao08",
    "vazao09", "vazao10", "vazao11", "vazao12", "vazao13", "vazao14", "vazao15", "vazao16", "vazao17", "vazao18",
    "vazao19", "vazao20", "vazao21", "vazao22", "vazao23", "vazao24", "vazao25", "vazao26", "vazao27", "vazao28", "vazao29",
    "vazao30", "vazao31", "vazao01status", "vazao02status", "vazao03status", "vazao04status", "vazao05status", "vazao06status",
    "vazao07status", "vazao08status", "vazao09status", "vazao10status", "vazao11status", "vazao12status", "vazao13status",
    "vazao14status", "vazao15status", "vazao16status", "vazao17status", "vazao18status", "vazao19status", "vazao20status",
    "vazao21status", "vazao22status", "vazao23status", "vazao24status", "vazao25status", "vazao26status", "vazao27status",
    "vazao28status", "vazao29status", "vazao30status", "vazao31status", "datains"
  )

  namesOrganizeFlu = c("station_code","consistency_level","date","streamflow_m3_s")
  namesOrganizePlu = c("station_code","consistency_level","date","rainfall_mm")

  namesSelectStationPlu= c("station_code", "consistency_level", "date", "rainfall_mm",
                           "civilYear", "monthCivilYear", "waterYear", "monthWaterYear","maxMissing"
  )

  namesSelectStationFlu=c(
    "station_code", "consistency_level", "date", "stream_flow_m3_s",
    "civilYear", "monthCivilYear", "waterYear", "monthWaterYear",
    "maxMissing"
  )


  stopifnot(
    "`stationsTimeSerieList` parameter must be a list of tibble resulted from `stationsData`, `selectStation`, `organize` function" = is.list(stationsTimeSerieList),
    "`selectionsResultSerie` parameter must be a list of tibble resulted from `selectStation` function" =
      identical(names(stationsTimeSerieList[[1]]), namesStationDataFlu) |
      identical(names(stationsTimeSerieList[[1]]), namesOrganizePlu) |
      identical(names(stationsTimeSerieList[[1]]), namesOrganizeFlu) |
      identical(names(stationsTimeSerieList[[1]]), namesSelectStationPlu) |
      identical(names(stationsTimeSerieList[[1]]), namesSelectStationFlu),
    "directroy must be a filepath in which stationData will be exported to (i.e., `./stationDataXLSX`)" = is.character(directory)
  )


  if (dir.exists(directory) == FALSE){
    dir.create(directory, recursive = TRUE)
  }

  a = lapply(1:length(stationsTimeSerieList), FUN = function(i)
    openxlsx::write.xlsx(stationsTimeSerieList[[i]],
                         paste0(directory,
                                "/",
                                names(stationsTimeSerieList[i]),
                                ".xlsx"))
  ) %>%
    suppressMessages() %>%
    suppressWarnings()

}

