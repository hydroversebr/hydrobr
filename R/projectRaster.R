#' Project raster
#'
#' @encoding UTF-8
#'
#' @description Project raster to desired EPSG
#'
#'
#' @param raster character vector. Input raster file path.
#' @param projectionEPSG character vector. Input "EPSG Geodetic Parameter Dataset"
#' @param resolution numeric vector. Projected raster resolution in meter.
#' @param rasterP_path character vector. Projected output raster file path.
#'
#'
#' @return Projected raster
#'
#'
#' @references
#' terra package (https://cran.r-project.org/web/packages/terra/index.html)
#'
#'
#' @examplesIf interactive()
#'
#' #
#'
#' projectRaster(raster = "./dem.tif", projectionEPSG = "epsg:32723", resolution = 30, rasterP_path = "./demP.tif")
#'
#'
#' @export

projectRaster = function(raster, projectionEPSG, resolution, rasterP_path){


  #project raster with random resolution

  r = terra::project(rast(raster),
                     y = projectionEPSG,
                     datatype = "INT2U")

  #set raster resolution

  terra::res(r) = resolution

  #reproject raster base on new resolution configuration

  terra::project(rast(raster),
                 y = r, mask = TRUE,
                 filename = rasterP_path,
                 overwrite = TRUE,
                 datatype = "INT2U")
}
