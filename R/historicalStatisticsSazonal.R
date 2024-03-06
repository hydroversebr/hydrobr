#' Historical Statistics for sazonal period (underdevelopment. available for streamdflow and annual series)
#'
#' @description
#' Historical series is splitted in two (six months each) and [hydrobr::historicalStatistics] is computed for both period.
#'
#'
#' @param selectStationsResultSeries list, tibble data frame; provides a list containing
#'   the data frames of filtered records for each station
#'   (series output from [hydrobr::selectStations()] function).
#'
#' @param statistics character; indicates statistics.
#'
#'  * The supported statistics for streamflow are:
#' (1) mean stream flow (Qmean);
#' (2) minimum of seven-day moving average of daily stream flow associated with return period (Q7T);
#' (3) stream flow associated with a percentage of time (Qperm);
#' (4) maximum stream flow (Qmax);
#' (5) minimum stream flow (Qmin).
#'
#'  * The supported statistics are:
#'  (1) total rainfall (Rtotal);
#'  (2)  maximum rainfall (Rmax);
#'  (3) rainy days (Rdays).
#'
#'  * The default value is "Qmean".
#'
#' @param pReturn numeric; return period if "Q7T" is choose as statistic parameter.
#'   The default is 10 year.
#'
#' @param permanence numeric; percentage of time if "Qperm" is choose as statistic parameter.
#'   The default is 95 percent.
#'
#' @return
#'
#' tibble object containing desired statistics for boths period and the ratio (porcentage) between
#' historicalStatistics and sazonal historic statistics
#'
#' #' @export
#'
#' @examples
#'
#'
#'  \dontrun{
#'
#'  #' # Fech a inventory of fluviometric stations for the state of Minas Gerais.
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
#' # annual mean stream flow serie for each station
#' QmeanS = historicalStatisticsSazonal(final_data$series, statistics = "Qmean")
#'
#'
#'
#'}
#'
historicalStatisticsSazonal = function(selectStationsResultSeries,
                                       statistics = "Qmean",
                                       permanence = 95,
                                       pReturn = 10
){

  startMonth = selectStationsResultSeries %>%
    dplyr::bind_rows() %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date) %>%
    dplyr::slice(1) %>%
    dplyr::pull(date) %>%
    lubridate::month()

  d = rep(1:12, 2)

  months = d[startMonth:(startMonth+11)]

  months1 = months[1:6]

  ####################### Calcular vazão de interesse para série Anual

  a = hydrobr::historicalStatistics(selectStationsResultSeries, statistics = statistics, permanence = permanence, pReturn = pReturn)


  #selecionar 6 primeiros meses e computar vazao de permanencia

  b = selectStationsResultSeries %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(month = lubridate::month(date)) %>%
    dplyr::filter(month %in% months1) %>%
    list() %>%
    hydrobr::historicalStatistics(statistics = statistics, permanence = permanence, pReturn = pReturn)



  c = selectStationsResultSeries %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(month = lubridate::month(date)) %>%
    dplyr::filter(!month %in% months1) %>%
    list() %>%
    hydrobr::historicalStatistics(statistics = statistics, permanence = permanence, pReturn = pReturn)


  if(statistics == "Qperm"){

    lista = a %>%
      dplyr::mutate("Q{permanence}_m3_s_first6_months" := b[2] %>% dplyr::pull(),
                    "Q{permanence}_m3_s_last6_months" := c[2] %>% dplyr::pull(),
                    "Q{permanence}_first6m/Q{permanence}_hist_porcent" := round(b[2]/a[2]*100,1) %>% dplyr::pull(),
                    "Q{permanence}_last6m/Q{permanence}_hist_porcent" := round(c[2]/a[2]*100,1) %>% dplyr::pull(),
                    InicialMonth = startMonth)
  } else {

    lista = a %>%
      dplyr::mutate("{statistics}_m3_s_first6_months" := b[2] %>% dplyr::pull(),
                    "{statistics}_m3_s_last6_months" := c[2] %>% dplyr::pull(),
                    "{statistics}_m3_s_first6m/{statistics}_hist_porcent" := round(b[2]/a[2]*100,1) %>% dplyr::pull(),
                    "{statistics}_m3_s_first6m/{statistics}_hist_porcent" := round(c[2]/a[2]*100,1) %>% dplyr::pull(),
                    InicialMonth = startMonth)


  }



  return(lista)

}
