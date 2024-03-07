#' Lag 1 AutoCorrelation considering all hydrological years months begin
#'
#' @description
#' Same processing done at [hydrobr::lag1AutoCorrelation] but considering all 12 months possible to start hydrological year
#' Function idealized to assistant hydrological year definition.
#'
#' @param selectStationsAllmonthsRDSfolder character. folder path with 12 Rdata files generated with [hydrobr::selectStationsAllMonths].
#'
#' @param statistics character; indicates statistics.
#'  * The supported statistics for streamflow are:
#' (1) mean stream flow (Qmean); (2)  minimum of seven-day moving average of daily stream flow (Q7);
#' (3) stream flow associated with a percentage of time (Qperm); (4) maximum stream flow (Qmax);
#' and (5) minimum stream flow (Qmin).
#'  * The supported statistics are: (1) total rainfall (Rtotal); (2)  maximum rainfall (Rmax);
#' (3) rainy days (Rdays).
#'  * The default value is "Qmean".
#'
#' @param permanence numeric; percentage of time if "Qperm" is choose as statistic parameter.
#'   The default is 95 percent.
#'
#' @return
#'
#' list with 12 tibble objects. Each tibble contatin autocorrelation for all stations for respective hydrological month begin.
#'
#' @export
#'
#' @examples
#'
#'
#' ' \dontrun{
#'
#' # Fech a inventory of fluviometric stations for the state of Minas Gerais.
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
#' selectStationsAllMonths(
#'   organizeResult = org_data,
#'   mode = "yearly",
#'   maxMissing = 10,
#'   minYears = 15,
#'   iniYear = NULL,
#'   finYear = NULL,
#'   consistedOnly = FALSE,
#'   folderPathWithNameDescription = "./loop/selecStation_15years_5porc",
#'   plot = TRUE
#' )
#'
#' #autocorrelation for Qmean series for all hydrological year month begin
#'
#' autoCorQmld = autocorrelationAll(selectStationsAllmonthsRDSfolder = "./loop", statistics = "Qmean")
#'
#'
autocorrelationAll = function(selectStationsAllmonthsRDSfolder,
                              statistics = "Qmean",
                              permanence = 95){


  lista1 = list.files(selectStationsAllmonthsRDSfolder, full.names = T) %>%
    gtools::mixedsort()

  listaCor = list()

  for (i in 1:length(lista1)){

    dados = base::readRDS(lista1[i])

    dados1 = dados$series %>%
      hydrobr::seriesStatistics(statistics = statistics, permanence = permanence)

    listaCor[[i]] = lag1Correlation(dados1$series) %>%
      dplyr::mutate(month = i) %>%
      dplyr::as_tibble()

    print(paste0("starting hydrological year at month ", i))

  }

  return(listaCor)

}


if(getRversion() >= "2.15.1")  utils::globalVariables("lag1Correlation")
