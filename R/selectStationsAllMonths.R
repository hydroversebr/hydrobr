#' Select stations loop for all months
#'
#' @description
#' Execute selectStation function for all months (1:12)
#'
#'
#'@param organizeResult list, tibble data frame; provides a list containing
#'   the data frames of raw records for each station downloaded from ANA web API
#'   (output from [hydrobr::stationsData()] function).
#' @param mode character; indicates in which scale to check missing data, 'monthly'
#'   or 'yearly'. The default is 'yearly'.
#' @param maxMissing numeric; indicates the maximum threshold of missing data allowed.
#'   The default is 10 percent.
#' @param minYears numeric; indicates the minimum years of complete data allowed. The
#'   default is 15 years.
#' @param iniYear numeric; filters the time series to begin on this year (inclusive).
#'   If you choose to use water year instead of civil year, e.g., month = 6,
#'   the first observation used is from the date "01-06-`iniYear`".
#'   The default is NULL (use entire period).
#' @param finYear numeric; filters the time series to end on this year (inclusive).
#'   If you choose to use water year instead of civil year, e.g., month = 6,
#'   the last observation used is from the date "31-05-`finYear`".
#'   The default is NULL (use entire period).
#' @param consistedOnly logical; should only consisted data be considered?
#'   The default is FALSE.
#' @param folderPathWithNameDescription character. folder path and description of selectStations parameters.
#' @param plot logical; plot the figure? The default is TRUE. The figure is saved
#'   regardless.
#'
#' @return
#' 12 Rdata files. Each one contain selectStation result considering inicial hydrological month.
#'
#' @export
#'
#' @importFrom gtools mixedsort
#'
#' @examples
#'
#' \dontrun{
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
#' org_data <- organize(stationsDataResult = s_data)
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
#'#'
#' files = list.files("./loop", full.names = TRUE) %>% gtools::mixedsort()
#'
#' selectStationsMonth1 = readRDS(files[1])
#'
#'}



selectStationsAllMonths = function(organizeResult,
                                   mode = "yearly",
                                   maxMissing = 10,
                                   minYears = 15,
                                   iniYear = NULL,
                                   finYear = NULL,
                                   consistedOnly = FALSE,
                                   folderPathWithNameDescription,
                                   plot = TRUE){

  dir.create(folderPathWithNameDescription, recursive = T, showWarnings = F)

  for (i in 1:12){

    estSelec1 = hydrobr::selectStations(organizeResult = organizeResult,
                                        maxMissing = maxMissing,
                                        month = i,
                                        mode = "yearly",
                                        minYears = minYears,
                                        iniYear = iniYear,
                                        finYear = finYear,
                                        consistedOnly = consistedOnly,
                                        plot = plot
    )

    saveRDS(estSelec1, file = paste0(folderPathWithNameDescription, "_month", i,".Rdata"))

    print(paste0("month ", i, " Done"))

  }



}
