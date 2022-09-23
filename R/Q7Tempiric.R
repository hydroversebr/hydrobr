#' Compute Q7 associated to desired return period based on empirical distribution
#' (function design for package internal use. See [hydrobr::historicalStatistics] for
#' general use)
#'
#' @encoding UTF-8
#'
#' @description Takes as input a vector of Q7 series and Compute Q7 associated to
#'  desired return period.
#'
#' @param vectorSerieQ7 numeric vector.
#'
#'
#' @param pReturn numeric, desired period of return.
#'
#' @return numeric vector.
#'
#' @details frequency evaluated with Kimbal method.
#'
#' @examplesIf interactive()
#' # Fech a inventory of fluviometric stations for the state of Minas Gerais
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
#' Q7_years = seriesStatistics(final_data, statistics = "Q7")
#'
#' # Q7 associated with return period of 10 years for first station
#' Q7.10 = vectorSerieQ7(final_data$series[[1]] %>% pull(3), statistics = "Q7")
#'
#' @export
#' @importFrom rlang .data
#' @importFrom rlang :=


Q7Tempiric = function(vectorSerieQ7, pReturn = 10){

  #frequency computation based on Kendal formula

  fobs <- (1:length(vectorSerieQ7))/(length(vectorSerieQ7)+1)

  #dataframe with Q7 serie and probability associated
  df =data.frame(vazao = sort(vectorSerieQ7),
                 fobs = fobs)

  #if any probability in data.frame match with desired probability (1/T)

  if(sum(unique(df[,2])==1/pReturn) == 1){

    #get matched Q7
    Q = df[df$fobs == .1,1]

  } else {

    #evaluate Q7 based on interpolation of frequency

    high = df %>% dplyr::filter(fobs>=1/pReturn) %>% dplyr::slice(1)

    low = df %>% dplyr::filter(fobs<1/T) %>% dplyr::slice(nrow(.))

    Q = (high$vazao-low$vazao)/(high$fobs-low$fobs)*(1/T - low$fobs) + low$vazao

  }

  return(Q)

}
