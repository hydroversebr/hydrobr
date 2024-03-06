#' Watershed delimitation based on area of ANA stations
#'
#' @encoding UTF-8
#'
#' @description This function delimitate stations watershed based on area value displayed at ANA fluviométric stations data.
#'
#'
#' @param stationsPath character vector. Shapefile path of ANA fluviometric stations
#' @param flowAcumPath character vector. Flow Accumulation raster path
#' @param flowDir8Path character vector. Flow direction raster path
#' @param bufferSearch value. Search radius in meters to snap pour points
#' @param outputDirPath character vector. Output directory path
#'
#'
#' @details shapefiles and rasters must be at same projection coordinate system (metric)
#'
#' @return - nested and unested watersheds in raster and shape format
#' - snaped pour points
#'
#'
#'#' @references
#' whitetoolbox package (https://cran.r-project.org/web/packages/whitebox/index.html)
#'
#' @examples
#'
#' \dontrun{
#' #stations in area of interest
#'
#' stations = inventory(
#'   stationType = "flu",
#'   as_sf = T,
#'   aoi = sf::st_read("./example/data/aoi_example.shp")
#')
#'
#' # omit station with area equal NA, reproject to epsg of dem and export it
#'
#' sf::st_write(
#'   na.omit(stations) %>% sf::st_transform(crs = "epsg:32723"),
#'   dsn = "./example/results/stations_aoi.shp",
#'   delete_dsn = TRUE, delete_layer = TRUE
#')
#'
#' #run watersheDelimit
#' watersheDelimit(
#'   stationsPath = "./example/results/stations_aoi.shp",
#'   flowAcumPath = "./example/results/demproducts/03flowAccumulation.tif",
#'   flowDir8Path = "./example/results/demproducts/03flowDirection.tif",
#'   bufferSearch = 1000,
#'   outputDirPath = "./example/results/watershedsDelimit"
#')
#'
#'}
#' @export


wDelimitationAreaBased = function(stationsPath,
                            flowAcumPath,
                            flowDir8Path,
                            bufferSearch = 1000,
                            outputDirPath)
{
  stationsPath <- sf::st_read(stationsPath, quiet = TRUE) %>%
    dplyr::arrange(sttn_cd)

  #set variable names
  # names(stationsPath) <- c("state", "station_code", "name", "lat",
  #                          "long", "station_type", "area_km2", "geometry")

  flowAcumRaster <- terra::rast(flowAcumPath)

  flowDir8Raster <- terra::rast(flowDir8Path)

  areaSP <- as.numeric()


  #create dir for raster basins results
  dir.create(paste(outputDirPath, "/WaterShedRaster", sep = ""),
             showWarnings = FALSE, recursive = TRUE)

  #create dir for shapefiles basins results
  dir.create(paste(outputDirPath, "/WaterShedShape", sep = ""),
             showWarnings = FALSE, recursive = TRUE)

  #create dir for pourpoints results
  dir.create(paste(outputDirPath, "/pourPoints", sep = ""),
             showWarnings = FALSE, recursive = TRUE)


  #loop to delineate individual basins
  i = 2
  for (i in 1:nrow(stationsPath)) {

    #number of pixel associated with area_km2 at ANA database for station i

    nPixel <- round(as.numeric(stationsPath$are_km2[i]) *
                      100 * 10000/terra::res(flowAcumRaster)[1]^2, 0)

    #buffer around station point
    buf <- sf::st_buffer(stationsPath[i, ], bufferSearch)

    #crop acummulation raster by buffer
    Acum <- terra::crop(flowAcumRaster, buf)

    #search value at acumulation raster  which is nearest of nPixel
    nearestValue <- terra::na.omit(terra::unique(Acum))[which.min(abs(terra::na.omit(terra::unique(Acum)) - nPixel) %>% dplyr::pull()), ]
    Acum[Acum != nearestValue] <- NA

    #convert pour point to shapefile and delineate watershed boundaries
    suppressWarnings(terra::as.points(Acum) %>% sf::st_as_sf() %>% dplyr::mutate(sttn_cd = stationsPath$sttn_cd[i]) %>%
                       dplyr::select(sttn_cd, geometry) %>%
                       sf::st_write(paste(outputDirPath, "/pourPoints/pour_", stationsPath$sttn_cd[i],
                                          ".shp", sep = ""), delete_layer = TRUE, append = FALSE,
                                    quiet = TRUE))

    whitebox::wbt_watershed(flowDir8Path, paste(outputDirPath, "/pourPoints/pour_",
                                                stationsPath$sttn_cd[i], ".shp",
                                                sep = ""), paste(outputDirPath, "/waterShedRaster/",
                                                                 "waterShed", stationsPath$sttn_cd[i], ".tif",
                                                                 sep = ""))

    #read watershed, trim extent and export agains
    y <- terra::trim(terra::rast(paste(outputDirPath, "/waterShedRaster/",
                                       "waterShed", stationsPath$sttn_cd[i], ".tif",
                                       sep = "")))

    terra::writeRaster(y, paste(outputDirPath, "/waterShedRaster/",
                                "waterShed", stationsPath$sttn_cd[i], ".tif",
                                sep = ""), overwrite = TRUE, datatype = "INT2S")

    #calculate watershed area
    areaSP[i] <- nearestValue *
      terra::res(flowAcumRaster)[1]^2/10000/100

    #convert watershed rastes to polygon, set station_code, area_km2, area_km2_predicted and export
    q <- sf::st_as_sf(terra::as.polygons(y)) %>% dplyr::mutate(station_code = stationsPath$sttn_cd[i],
                                                               area_km2 = stationsPath$are_km2[i], area_km2p = areaSP[i]) %>%
      dplyr::select(station_code, area_km2, area_km2p,
                    geometry)

    suppressWarnings(sf::st_write(q, paste(outputDirPath,
                                           "/waterShedShape/", "waterShed", stationsPath$sttn_cd[i],
                                           ".shp", sep = ""), delete_layer = TRUE, append = FALSE,
                                  quiet = TRUE))
    print(paste("Station ", stationsPath$sttn_cd[i],
                " Done ", i, "/", nrow(stationsPath), sep = ""))
  }

  print("Generating final files")

  #read basins pour_points, combine in one shapefile and export
  suppressWarnings(list.files(outputDirPath, "/pourPoints/pour_", pattern = ".shp",
                              full.names = TRUE) %>% lapply(sf::st_read, quiet = TRUE) %>%
                     dplyr::bind_rows() %>% dplyr::mutate(station_code = stationsPath$sttn_cd,
                                                          area_km2 = stationsPath$are_km2, area_km2p = areaSP) %>%
                     dplyr::select(station_code, area_km2, area_km2p, geometry) %>%
                     sf::st_write(paste(outputDirPath, "/StationsPourPointsSnaped.shp",
                                        sep = ""), delete_layer = TRUE, append = TRUE, quiet = TRUE))

  #combine basins in onshape file and export
  suppressWarnings(list.files(paste(outputDirPath, "/waterShedShape",
                                    sep = ""
  ), pattern = ".shp", full.names = TRUE) %>%
    lapply(sf::st_read, quiet = TRUE) %>% dplyr::bind_rows() %>%
    sf::st_write(
      paste(outputDirPath, "/Watersheds_unested.shp",
            sep = ""
      ),
      delete_layer = TRUE, append = FALSE,
      quiet = TRUE
    ))

  #delimitar nested watersheds
  whitebox::wbt_watershed(flowDir8Path, paste(outputDirPath,
                                              "/StationsPourPointsSnaped.shp", sep = ""), paste(outputDirPath,
                                                                                                "/Watersheds_nested.tif", sep = ""))

  #import nested and trim extent
  r <- terra::trim(terra::rast(paste(outputDirPath, "/Watersheds_nested.tif",
                                     sep = "")))

  #renames nested basins
  unicos <- sort(base::unique(stats::na.omit(terra::values(r))))
  reclassMatrix <- base::data.matrix(cbind(c(unicos), c(unicos[-1],
                                                        unicos[length(unicos)] + 1), as.numeric(stationsPath$sttn_cd)))
  r1 <- terra::classify(r, reclassMatrix, right = FALSE)

  #export
  terra::writeRaster(r1, paste(outputDirPath, "/Watersheds_nested.tif",
                               sep = ""), overwrite = TRUE, datatype = "INT4U")

  #convert nestedbasin to rater and export
  suppressWarnings(sf::st_as_sf(terra::as.polygons(r1)) %>%
                     dplyr::mutate(station_code = Watersheds_nested, areakm2 = stationsPath$are_km2,
                                   areakm2_p = round(areaSP, 0)) %>% dplyr::select(station_code,
                                                                                   areakm2, areakm2_p, geometry) %>% sf::st_write(paste(outputDirPath,
                                                                                                                                        "/Watersheds_nested.shp", sep = ""), delete_layer = TRUE,
                                                                                                                                  append = FALSE, quiet = TRUE))
  #dt with area_km2 at ANA database and predicted by watershed
  resumo <- base::data.frame(estCod = as.numeric(stationsPath$sttn_cd),
                             areaHidroWeb_km2 = as.numeric(stationsPath$are_km2),
                             areaPredicted_km2 = round(areaSP, 0))
  resumo$error_porcent <- round((resumo[, 3] - resumo[, 2])/resumo[,
                                                                   2] * 100, 2)
  # unlink(tempDirPath, recursive = TRUE)
  print("Job Done! Congratz!")
  print(resumo)

}



if(getRversion() >= "2.15.1")  utils::globalVariables(c("areakm2",
                                                        'areakm2_p',
                                                        "area_km2",
                                                        "area_km2p",
                                                        'Watersheds_nested',
                                                        'geometry',
                                                        'sttn_cd'))
