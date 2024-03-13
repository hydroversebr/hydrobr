#' Aggregate Chirps Time Series by year, month
#'
#' @description
#' Aggregate Chirps Rainfall Times Series by year, month or yearmonth
#'
#'
#' @param chirpsStack spatRaster. spatRaster stack with chirps daily chirps data (output from [hydrobr::downloadChirpsRainfall()])
#' @param month numeric; indicates the month when the water year begins. The default is
#'   1 (use civil year).
#' @param inicialYear numeric; filters the time series to begin on this year (inclusive).
#'   If you choose to use water year instead of civil year, e.g., month = 6,
#'   the first observation used is from the date "01-06-`inicialYear`".
#' @param finalYear numeric; filters the time series to end on this year (inclusive).
#'   If you choose to use water year instead of civil year, e.g., month = 6,
#'   the last observation used is from the date "31-05-`finalYear`".
#' @param fun function to be applied. The following functions have been are implemented "sum", "mean", "median", "modal", "which", "which.min", "which.max", "min", "max", "prod", "any", "all", "sd", "std", "first".
#' @param group_by character. One of the following values: "years", "months", "yearmonths" which daily data will be grouped.
#' @param cores positive integer. cores to be used for a 'parallel' processing.
#'
#' @return
#' spatRaster object with aggregated rainfall data.
#'
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#'
#'require(terra)
#'
#'area_of_interest = vect("./paracatu.shp")
#'
#'
#'downloadChirpsRainfall(dir_out = "./temp/chirpsRainfall",
#'                     years = c(1990:2019),
#'                    aoi = area_of_interest)
#'
#'
#'chirpsStackAoi= list.files("./temp/chirpsRainfall", full.names = T) %>%
#'terra::rast()
#'
#'
#'#Annual mean rainfall
#'chirpsYear = aggregateChirpsTS(chirpsStack = chirpsStackAoi,
#'month = 11,
#'inicialYear = 1990,
#'finalYear = 2019,
#' fun = "sum",
#' group_by = "years",
#' cores = 23)
#'
#'
#'}
#'
#'
aggregateChirpsTS = function(chirpsStack, month, inicialYear, finalYear, fun, group_by, cores = NULL){

  stopifnot(
    "`chirpsStack` parameter must be SpatRaster object (output of downloadChirpsRainfall)" = class(chirpsStack)[1]=="SpatRaster",
    "`fun` must be `sum`, `mean`, `median`, `modal`, `which`, `which.min`, `which.max`, `min`, `max`, `prod`, `any`, `all`, `sd`, `std`, `first`" = fun %in% c("sum", "mean", "median", "modal", "which", "which.min", "which.max", "min", "max", "prod", "any", "all", "sd", "std", "first"),
    "`group_by` must be character (`years`, `months`, `yearmonths`" = group_by %in% c("years", "months", "yearmonths"),
    "`cores` must be numeric" = is.numeric(cores) | is.null(cores))

  print("Processing time dependes of area size and time series length. Consider to take a small sample size to get experience and/or use parallel processing (cores input). To detect number of cores use available `parallel::detectCores()")

  #renomear tempo nas camadas para datas
  terra::time(chirpsStack) = gsub(pattern = "chirps-v2.0.", "", names(chirpsStack)) %>%
    as.Date("%Y.%m.%d")

  #filtrar chirpsStack pelos anos final e inicial
  chirpsStack = terra::subset(chirpsStack, terra::time(chirpsStack) >= paste0(inicialYear, "-01-01"))
  chirpsStack = terra::subset(chirpsStack, terra::time(chirpsStack) <= paste0(finalYear, "-12-31"))


  #anohidrológico
  hidrologicalYear <- gsub(pattern = "chirps-v2.0.", "",names(chirpsStack)) %>%
    base::as.Date("%Y.%m.%d") %>%
    dplyr::as_tibble() %>%
    dplyr::rename(date = 1) %>%
    dplyr::mutate(
      # Retrieve year from date
      civilYear      = lubridate::year(.data$date),
      # Retrieve year-month from date
      monthCivilYear = .data$date - (lubridate::day(.data$date) - 1),
      # Calculate water year
      waterYear      = lubridate::year(.data$date %>%
                                         lubridate::add_with_rollback(months(-(month - 1)))),
      # same as monthCivilYear but replace civilYear by waterYear
      monthWaterYear = as.Date(paste0(.data$waterYear, substr(.data$monthCivilYear, 5, 10)))
    )


  #verificar anos que possuem 12 monthwateryear. Ou seja, wateryears completos
  anoCompletos = hidrologicalYear$monthWaterYear %>%
    base::unique() %>%
    lubridate::year() %>%
    base::table() %>%
    dplyr::as_tibble() %>%
    dplyr::filter(n == 12) %>%
    dplyr::pull(1) %>%
    base::sort()


  #filtrar dados com awateryear completos
  finalData = hidrologicalYear %>%
    dplyr::filter(waterYear >=anoCompletos[1]) %>%
    dplyr::filter(waterYear <= dplyr::last(anoCompletos))


  #filtrar chirpsStack pelos anos final e inicial
  chirpsStack = subset(chirpsStack, terra::time(chirpsStack) >= dplyr::first(finalData)$date)
  chirpsStack = subset(chirpsStack, terra::time(chirpsStack) <= dplyr::last(finalData)$date)


  terra::time(chirpsStack) = finalData$monthWaterYear

  names(chirpsStack) = finalData$monthWaterYear %>%
    as.Date("%Y.%m.%d")

  terra::time(chirpsStack) = names(chirpsStack) %>%
    as.Date()

  newTS = terra::tapp(chirpsStack, index = group_by, fun = fun, cores = cores)

  return(newTS)


}
