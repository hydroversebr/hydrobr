#' Historical Statistics for sazonal period considering all hydrological years months begin (underdevelopment. available for streamdflow and annual series)
#'
#' @description
#' Same processing done at [hydrobr::historicalStatisticsSazonal] but considering all 12 months possible to start hydrological year
#'
#'
#' @param selectStationsAllmonthsRDSfolder character. folder path with 12 Rdata files generated with [hydrobr::selectStationsAllMonths].
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
#' list with 12 tibble objects. Each one containing desired statistics for boths period and the ratio (porcentage) between
#' historicalStatistics and sazonal historic statistics
#'
#' @export
#'
#' @examples
#'#'
#' \dontrun{
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
#' # annual mean stream flow serie for each station
#'
#' hsQmean = historicalStatisticsSazonalAll("./loop", statistics = "Qmean")
#'
#'
#'}
#'
#'



historicalStatisticsSazonalAll = function(selectStationsAllmonthsRDSfolder,
                                          statistics = "Qmean",
                                          permanence = 95,
                                          pReturn = 10
){


  lista = list()
  lista1 = list.files(selectStationsAllmonthsRDSfolder, full.names = T) %>%
    gtools::mixedsort()

  for (i in 1:length(lista1)){

    estSelec1 = readRDS(lista1[i])

    startMonth = i

    d = rep(1:12, 2)

    months = d[startMonth:(startMonth+11)]

    months1 = months[1:6]
    print("first 6 months:")
    print(months1)

    ####################### Calcular vazão de interesse para série Anual

    a = hydrobr::historicalStatistics(estSelec1$series, statistics = statistics, permanence = permanence, pReturn = pReturn)


    #selecionar 6 primeiros meses e computar vazao de permanencia

    b = estSelec1$series %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(month = lubridate::month(date)) %>%
      dplyr::filter(month %in% months1) %>%
      list() %>%
      hydrobr::historicalStatistics(statistics = statistics, permanence = permanence, pReturn = pReturn)



    c = estSelec1$series %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(month = lubridate::month(date)) %>%
      dplyr::filter(!month %in% months1) %>%
      list() %>%
      hydrobr::historicalStatistics(statistics = statistics, permanence = permanence, pReturn = pReturn)



    if(statistics == "Qperm"){

      lista[[i]] = a %>%
        dplyr::mutate("Q{permanence}_m3_s_first6_months" := b[2] %>% dplyr::pull(),
                      "Q{permanence}_m3_s_last6_months" := c[2] %>% dplyr::pull(),
                      "Q{permanence}_first6m_ratio_Q{permanence}_hist_porcent" := round(b[2]/a[2]*100,1) %>% dplyr::pull(),
                      "Q{permanence}_last6m_ratio_Q{permanence}_hist_porcent" := round(c[2]/a[2]*100,1) %>% dplyr::pull(),
                      InicialMonth = startMonth)
    } else {

      lista[[i]] = a %>%
        dplyr::mutate("{statistics}_m3_s_first6_months" := b[2] %>% dplyr::pull(),
                      "{statistics}_m3_s_last6_months" := c[2] %>% dplyr::pull(),
                      "{statistics}_first6m_ratio_{statistics}_hist_porcent" := round(b[2]/a[2]*100,1) %>% dplyr::pull(),
                      "{statistics}_last6m_ratio_{statistics}_hist_porcent" := round(c[2]/a[2]*100,1) %>% dplyr::pull(),
                      InicialMonth = startMonth)

    }





  }

  names(lista) = paste0("month", c(1:12))

  return(lista)

}
