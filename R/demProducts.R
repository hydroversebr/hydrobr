#' (UNDER DEVELOPMENT) Digital elevation model sub-products
#'
#' @encoding UTF-8
#'
#' @description Obtain digital elevation model sub-products
#'
#'
#' @param demP character vector. Input DEM raster file path.
#' @param streamP character vector. Input stream shapefile path.
#' @param outputDir character vector. Output rasters file path.
#'
#'
#' @return Create eigth DEM subproducts:
#'  (1) DEM aspect;
#'  (2) DEM burned by streams;
#'  (3) DEM Sinks;
#'  (4) DEM slope in degrees;
#'  (5) DEM slope in porcent;
#'  (6) DEM burned by streans filled;
#'  (7) Flow Accumulation based on (6);
#'  (8) Flow Direction based on (6).
#'
#'
#' @references
#' whitetoolbox package (https://cran.r-project.org/web/packages/whitebox/index.html)
#'
#'
#' @examplesIf interactive()
#'
#' #
#'
#' demProducts("./dem.tif", "./streams.shp", "./demProducts")
#'
#'
#' @export
demProducts = function(demP, streamP, outputDir){

  if (dir.exists(outputDir) == FALSE){
    dir.create(outputDir, recursive = TRUE)
  }

  print(whitebox::wbt_slope(demP,
                            paste(outputDir, "/01DEM_slope_porcent.tif", sep = ""),
                            units = "percent"))

  print(whitebox::wbt_slope(demP,
                            paste(outputDir, "/01DEM_slope_dregrees.tif", sep = ""),
                            units = "degrees"))

  print(whitebox::wbt_aspect(demP,
                             paste(outputDir, "/01DEM_aspect.tif", sep = "")))

  print(whitebox::wbt_sink(demP,
                           paste(outputDir, "/01DEM_Sinks.tif", sep = ""),
                           zero_background = TRUE))


  print(whitebox::wbt_fill_burn(demP,
                                streamP,
                                paste(outputDir, "/01DEM_Burned.tif", sep = "")))


  print(whitebox::wbt_fill_depressions(paste(outputDir, "/01DEM_Burned.tif", sep = ""),
                                       paste(outputDir, "/02DEM_Burned_Fill.tif", sep = "")))


  print(whitebox::wbt_d8_flow_accumulation(paste(outputDir, "/02DEM_Burned_Fill.tif", sep = ""),
                                           paste(outputDir, "/03flowAccumulation.tif", sep = ""),
                                           out_type = "cells"))


  print(whitebox::wbt_d8_pointer(paste(outputDir, "/02DEM_Burned_Fill.tif", sep = ""),
                                 paste(outputDir, "/03flowDirection.tif", sep = "")))

}
