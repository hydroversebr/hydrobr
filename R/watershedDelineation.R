#' Watershed delineation based on pour points
#'
#' @encoding UTF-8
#'
#' @description This function delineates watershed boundaries. See details.
#'
#'
#' @param stationsPath character vector. Shapefile path of ANA fluviometric stations
#' @param flowAcumPath character vector. Flow Accumulation raster path
#' @param flowDir8Path character vector. Flow direction raster path
#' @param bufferSearch value. Search radius in meters to snap pour points
#' @param outputDirPath character vector. Output directory path
#'
#'
#' @details
#' - Stations shapefile must include a 'sttn_cd' column containing numerical identifiers for stations.
#' - station shapefile must include an 'area_km2' column containing area values for stations. If the area is not known, leave it empty. However, the column must exist.
#' - The delineation could be based on two algorithms: (1)
#' Pour points are snapped based on river proximity.
#' The algorithm chooses the river point with the biggest flow accumulation value.
#' This function is particularly useful when the basin area value is not known in advance.
#' This function delineates watershed boundaries based on the area value displayed in ANA fluviometric station data.
#'
#'
#' shapefiles and rasters must be at same projection coordinate system (metric)
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
#' watershedDelineation(
#'   stationsPath = "./example/results/stations_aoi.shp",
#'   flowAcumPath = "./example/results/demproducts/03flowAccumulation.tif",
#'   flowDir8Path = "./example/results/demproducts/03flowDirection.tif",
#'   bufferSearch = 1000,
#'   outputDirPath = "./example/results/watershedsDelimit"
#')
#'
#'}
#'
#' @export



watershedDelineation = function(stationsPath,
                              flowAcumPath,
                              flowDir8Path,
                              bufferSearch = 1000,
                              outputDirPath)
{


  stopifnot(
    "Stations shapefile must include a 'sttn_cd' column containing numerical identifiers for stations." = "sttn_cd" %in% names(sf::st_read(stationsPath, quiet = TRUE)),
    "The station shapefile must include an 'area_km2' column containing area values for stations. If the area is not known, leave it empty. However, the column must exist." = "are_km2" %in% names(sf::st_read(stationsPath, quiet = TRUE)))

  stationsPath <- sf::st_read(stationsPath, quiet = TRUE) %>%
    dplyr::arrange(sttn_cd)

  flowAcumRaster <- terra::rast(flowAcumPath)

  flowDir8Raster <- terra::rast(flowDir8Path)

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

  for (i in 1:nrow(stationsPath)) {

    if("are_km2" %in% names(stationsPath) & !is.na(stationsPath$are_km2[i])){

      print("area_based")
      #number of pixel associated with area_km2 at ANA database for station i

      nPixel <- round(as.numeric(stationsPath$are_km2[i]) *
                        100 * 10000/terra::res(flowAcumRaster)[1]^2, 0)

      #buffer around station point
      buf <- sf::st_buffer(stationsPath[i, ], bufferSearch)

      #crop acummulation raster by buffer
      Acum <- terra::crop(flowAcumRaster, buf) %>%
        terra::mask(buf)

      #search value at acumulation raster  which is nearest of nPixel
      nearestValue <- terra::na.omit(terra::unique(Acum))[which.min(abs(terra::na.omit(terra::unique(Acum)) - nPixel) %>% dplyr::pull()), ]
      Acum[Acum != nearestValue] <- NA

      #convert pour point to shapefile and delineate watershed boundaries
      suppressWarnings(terra::as.points(Acum) %>% sf::st_as_sf() %>% dplyr::slice(1) %>% dplyr::mutate(sttn_cd = stationsPath$sttn_cd[i]) %>%
                         dplyr::select(sttn_cd, geometry) %>%
                         sf::st_write(paste(outputDirPath, "/pourPoints/pour_", stationsPath$sttn_cd[i],
                                            ".shp", sep = ""), delete_layer = TRUE, append = FALSE,
                                      quiet = TRUE))

      whitebox::wbt_watershed(flowDir8Path, paste(outputDirPath, "/pourPoints/pour_",
                                                  stationsPath$sttn_cd[i], ".shp",
                                                  sep = ""), paste(outputDirPath, "/waterShedRaster/",
                                                                   "waterShed", stationsPath$sttn_cd[i], ".tif",
                                                                   sep = ""), verbose_mode = NULL)

      #read watershed, trim extent and export agains
      y <- terra::trim(terra::rast(paste(outputDirPath, "/waterShedRaster/",
                                         "waterShed", stationsPath$sttn_cd[i], ".tif",
                                         sep = "")))

      terra::writeRaster(y, paste(outputDirPath, "/waterShedRaster/",
                                  "waterShed", stationsPath$sttn_cd[i], ".tif",
                                  sep = ""), overwrite = TRUE, datatype = "INT2S")

      #convert watershed rastes to polygon, set station_code, area_km2, area_km2_predicted and export
      q <- sf::st_as_sf(terra::as.polygons(y)) %>% dplyr::mutate(station_code = stationsPath$sttn_cd[i],
                                                                 area_km2 = stationsPath$are_km2[i], area_km2p = sf::st_area(., drop = TRUE)/1000000)%>%
        dplyr::select(station_code, area_km2, area_km2p,
                      geometry)

      suppressWarnings(sf::st_write(q, paste(outputDirPath,
                                             "/waterShedShape/", "waterShed", stationsPath$sttn_cd[i],
                                             ".shp", sep = ""), delete_layer = TRUE, append = FALSE,
                                    quiet = TRUE))

      print(paste("--------> Station ", stationsPath$sttn_cd[i],
                  " Done ", i, "/", nrow(stationsPath),  " <--------", sep = ""))



    } else {

      print("no_area_based")
      #select station "i" and export to folder

      stationsPath %>%
        dplyr::slice(i) %>%
        sf::st_write(paste(outputDirPath, "/pourPoints/pour_", stationsPath$sttn_cd[i],
                           ".shp", sep = ""), delete_layer = TRUE, append = FALSE,
                     quiet = TRUE)

      #snap based on whitebox and overwrite shape generate at last step

      whitebox::wbt_snap_pour_points(pour_pts = paste(outputDirPath, "/pourPoints/pour_", stationsPath$sttn_cd[i],
                                                      ".shp", sep = ""),
                                     flow_accum = flowAcumPath,
                                     output = paste(outputDirPath, "/pourPoints/pour_", stationsPath$sttn_cd[i],".shp", sep = ""),
                                     snap_dist = bufferSearch)


      #delinate basin

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


      #convert watershed rastes to polygon, set station_code, area_km2, area_km2_predicted and export
      q <- sf::st_as_sf(terra::as.polygons(y)) %>% dplyr::mutate(station_code = stationsPath$sttn_cd[i],
                                                                 area_km2 = stationsPath$are_km2[i], area_km2p = sf::st_area(., drop = TRUE)/1000000)%>%
        dplyr::select(station_code, area_km2, area_km2p,
                      geometry)

      suppressWarnings(sf::st_write(q, paste(outputDirPath,
                                             "/waterShedShape/", "waterShed", stationsPath$sttn_cd[i],
                                             ".shp", sep = ""), delete_layer = TRUE, append = FALSE,
                                    quiet = TRUE))
      print(paste("--------> Station ", stationsPath$sttn_cd[i],
                  " Done ", i, "/", nrow(stationsPath),  " <--------", sep = ""))




    }


  }

  print("Generating final files")


  #read basins pour_points, combine in one shapefile and export
  suppressWarnings(list.files(paste0(outputDirPath, "/pourPoints"), pattern = ".shp",
                              full.names = TRUE) %>% lapply(sf::st_read, quiet = TRUE) %>%
                     dplyr::bind_rows() %>% dplyr::mutate(station_code = stationsPath$sttn_cd,
                                                          area_km2 = stationsPath$are_km2) %>%
                     dplyr::select(station_code, area_km2, geometry) %>%
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
                                   a_km2p_nest = sf::st_area(., drop = TRUE)/1000000) %>% dplyr::select(station_code,
                                                                                                        areakm2, a_km2p_nest, geometry) %>% sf::st_write(paste(outputDirPath,
                                                                                                                                                               "/Watersheds_nested.shp", sep = ""), delete_layer = TRUE,
                                                                                                                                                         append = FALSE, quiet = TRUE))

  #dt with area_km2 at ANA database and predicted by watershed

  resumo = list.files(paste(outputDirPath,
                            "/waterShedShape", sep = ""), full.names = T, pattern = ".shp") %>%
    lapply(sf::st_read, quiet = TRUE) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(Relative_Error_Porcent = round((ar_km2p - are_km2)/are_km2*100, 2)) %>%
    dplyr::as_tibble() %>%
    dplyr::select(sttn_cd, area_km2 = are_km2, area_km2p = ar_km2p, dplyr::contains("Relative"))


  gc()
  return(resumo)
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
