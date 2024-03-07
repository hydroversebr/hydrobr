#' Lag 1 AutoCorrelation
#'
#' @description
#' Evaluate serial correlation (or autocorrelation) with 1 lag.
#'
#'
#'
#' @param seriesStatisticsResultSeries list of tibbles containing annual or monthly series of all stations;
#'   (series output from [hydrobr::seriesStatistics()] function).
#'
#' @return
#' tibble with autocorrelation based on lag1 (CorrelationLag1 column), absolute confidente interval and logical column indicating if station series is autocorrelated
#'
#'
#' @references
#' Based on acf function
#' (https://www.rdocumentation.org/packages/forecast/versions/8.21.1/topics/Acf)
#'
#' Fundamentals of Statistical Hydrology
#' (https://link.springer.com/book/10.1007/978-3-319-43561-9)
#'
#' The Mann-Kendall Test Modified by Effective Sample Size to Detect Trend in Serially Correlated Hydrological Series
#' (https://link.springer.com/article/10.1023/B:WARM.0000043140.61082.60)
#'
#'
#' @export
#'
#'
#'
#' @examples
#'
#' \dontrun{
#'
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
#'
#' #lag1Autocorrelation
#'
#' autoCorr = lag1AutoCorrelation(Qmean_years$series)
#'
#' }
#'
#'
#'
#'
lag1AutoCorrelation = function(seriesStatisticsResultSeries){

  funcao = function(y){

    dados2 = y %>%
      dplyr::pull(3) %>%
      stats::acf(plot = F)

    intervalConf = stats::qnorm((1 + 0.95)/2)/sqrt(dados2$n.used)

    p = data.frame(CorrelationLag1 = dados2$acf %>%
                     as.data.frame() %>%
                     dplyr::slice(2),
                   confidenceInterval =  intervalConf) %>%
      stats::setNames(c("CorrelationLag1", "confidenceInterval")) %>%
      dplyr::mutate(autocorrelated =dplyr:: if_else(abs(CorrelationLag1)<abs(confidenceInterval), F, T))


    return(p)

  }

  t = lapply(seriesStatisticsResultSeries, FUN = funcao)

  t = t %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(station_code = seriesStatisticsResultSeries %>%
             dplyr::bind_rows() %>%
             {base::unique(.$station_code)}) %>%
    dplyr::select(4,dplyr::everything()) %>%
    dplyr::as_tibble()

  return(t)

}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("consistency_level",
                                                        "CorrelationLag1",
                                                        "confidenceInterval"))


