#' Download Chirps Rainfall daily data by year
#'
#' @description
#' Download Chirps Rainfall daily data by year based on area of interest
#'
#'
#' @param dir_out character. Directory where you want to save the raster images that you are going to download.
#' @param years numeric. The period in years that the function should download images.
#' @param aoi spatVector object. Provides the boundaries where chirps data should be limited to (datum = WGS84).
#'
#' @returns raster files of each year containing daily layers for each (1 for each day of given year).
#'
#' @details
#' Simple application of get_chirps function of chirps package
#'
#'
#' @references
#'
#' https://cran.r-project.org/web/packages/chirps/chirps.pdf
#'
#'
#' @examples
#'
#'
#'\dontrun{
#'
#'require(terra)
#'
#'area_of_interest = vect("./paracatu.shp")
#'
#'
#'downloadChirpsRainfall(dir_out = "./temp/chirpsRainfall",
#'                     years = c(1990:1991),
#'                    aoi = area_of_interest)
#'
#'}
#'
#'
#'@export
#'
downloadChirpsRainfall = function(dir_out, years, aoi){


    stopifnot(
      "`dir_out` parameter must be character indicating output folder path (i.e `c:/temp`)" = is.character(dir_out),
      "`years` must be numeric vector containing years to be downloaded" = is.numeric(years),
      "`aoi` must be a polygon of class `SpatVector` (terra package)" = class(aoi) == "SpatVector")



    dir.create(dir_out, showWarnings = FALSE)


    for (i in 1:length(years)){

      print(paste0(years[i], " Downloading"))

      inicial = paste0(years[i], "-01","-01")

      final = paste0(years[i], "-12","-31")

      if(years[i] == 1981){

        inicial = paste0(years[i], "-01", "-02")

      }


      dates = c(inicial, final)

      chirpsData <- chirps::get_chirps(aoi, dates, server = "CHC", as.raster = TRUE)

      print(paste0(years[i], " Saving"))

      terra::writeRaster(chirpsData, filename = paste0(dir_out,
                                                       "/chirps_rainfall", years[i], ".tif"),
                         filetype = "GTiff", overwrite = TRUE)
    }
}



  # if(variable %in% c("tmax", "tmin")){
  #
  #   i = 1
  #   for (i in 1:length(years)){
  #
  #     print(paste0(years[i], " Downloading"))
  #
  #     inicial = paste0(years[i], "-01","-01")
  #
  #     final = paste0(years[i], "-12","-31")
  #
  #     dates = c(inicial, final)
  #
  #     chirpsData <- chirps::get_chirts(aoi, var = stringr::str_to_title(variable), dates, server = "CHC", as.raster = TRUE)
  #
  #     print(paste0(years[i], " Saving"))
  #
  #     terra::writeRaster(chirpsData, filename = paste0(dir_out,
  #                                                      "/chirps_", "variable", years[i], ".tif"),
  #                        filetype = "GTiff", overwrite = TRUE)
  #   }
  # }





