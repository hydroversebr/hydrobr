#' Extract and organize terraClimate data
#'
#' @description
#' Extract TerraClimate spatial data based on a polygon and organize it into a time series which can be used at [hydrobr::selectStations()]
#'
#' @param terraClimateRast rast object contain result from downloadterra
#' @param aoi object of class \code{sf} or \code{terra} (polygon); Provides the boundaries
#'   where extraction should be done.
#' @param fun character; function which should be use on extraction. Available functions are:
#' min, max, sum, mean, median, mode, majority, minority.
#' @param colname chracter; colname of aoi which should be used for display results.
#'
#' @return A list containing an organized data frame [tibble::tibble()] object
#'    for each station. The data frames will contain the following columns:
#'    station_code: station unique identifier (chr).
#'    consistency_level: data consistency level (1 = raw, 2 = consisted) (chr).
#'    date: date format YYYY-MM-DD (Date).
#'    rainfall_mm/streamflow_m3_s: rain/streamflow gauge measurement (dbl).
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'es = geobr::read_municipality(code_muni = "ES")
#'
#'terraClimate = downloadTerraClimate("./temp3",
#'                                    variable = "ppt",
#'                                    years = c(2019:2021),
#'                                    aoi = es)
#'
#'ppt_es = terraClimateExtOrg(terraClimateRast = terraClimate,
#'                            aoi = es,
#'                            fun = "mean",
#'                            colname = "name_muni")
#'
#'
#'}


terraClimateExtOrg = function(terraClimateRast, aoi, fun, colname){

  # Verificações iniciais
  stopifnot(
    "`terraClimateRast` must be a raster of class `rast` (terra package)" = "SpatRaster" %in% class(terraClimateRast),
    "`aoi` must be a polygon of class `sf` (sf package)" = sum(class(aoi) %in% c("sf", "SpatVector"))==1,
    "`fun` must be a character indicating function to be use in zonal statistics" = is.character(fun),
    "`fun` must be a character indicating function to be use in zonal statistics" = fun %in% c("min", "max", "sum", "mean", "median", "mode", "majority", "minority"),
    "`colname` must be a character indicating column of `aoi` to summarise zonal statistics" = colname %in% names(aoi)

  )

  #if aoi  is SpatVector convert to sf

  if(unique(class(aoi)%in%"SpatVector")==TRUE){

    aoi = aoi %>%
      sf::st_as_sf(aoi)

  }

  #convert aoi to wgs84
  aoi = aoi %>%
    sf::st_transform(crs = 4326)


  #variaveis e unidades
  variaveis = data.frame(variavel_real =  c("aet", "def", "pet", "ppt", "q", "soil", "srad", "swe", "tmax", "tmin", "vap", "ws", "vpd", "PDSI"),
                         unidade = c("_mm", "_mm", "_mm", "_mm", "_mm", "_mm", "_w_m2", "_mm", "_c", "_c", "_kpa", "_m_s", "kpa", "adimensional"))

  #variavel do dado de entrada
  variable = substr(names(terraClimateRast)[1],1,nchar(names(terraClimateRast)[1])-11)

  #variavel e unidade do dado de entrada
  variavel_unidade = variaveis %>%
    dplyr::filter(variavel_real == variable) %>%
    dplyr::mutate(nome = paste0(variavel_real, unidade)) %>%
    dplyr::pull(nome)

  #extracao
  zonal = exactextractr::exact_extract(terraClimateRast,
                                       aoi,
                                       fun = fun,
                                       append_cols = colname)

  #organizacao dos dados em série temporal
  lista = zonal %>%
    tidyr::pivot_longer(cols = 2:ncol(.)) %>%
    dplyr::mutate(name = substr(.$name, nchar(.$name)-9,nchar(.$name)),
                  data = as.Date(name),
                  month = lubridate::month(data),
                  consistency_level = 2,
                  !!variavel_unidade := value) %>%
    dplyr::select(station_code = dplyr::any_of(colname),
                  consistency_level,
                  date = data,
                  dplyr::contains(variavel_unidade)) %>%
    split(.$station_code)


  return(lista)


}


if(getRversion() >= "2.15.1")  utils::globalVariables(c("variable",
                                                        "variavel_real",
                                                        "unidade",
                                                        "nome",
                                                        "name",
                                                        "data",
                                                        "value"))
