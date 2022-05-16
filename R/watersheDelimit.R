#' Delimitation of watersheds based on ANA fluviométric stations
#'
#' @encoding UTF-8
#'
#' @description Delimitation of watersheds based on ANA fluviométric stations
#'
#' @param stationsPath character vector. Shapefile path of ANA fluviometric stations
#' @param flowAcumPath character vector. Flow Accumulation raster path
#' @param flowDir8Path character vector. Flow direction raster path
#' @param bufferSearch value. Search radius in meters to snap pour points
#' @param outputDirPath character vector. Output directory path
#' @param tempDirPath character vector. Temporary file directory path
#'
#'
#' @return Watersheds delineated
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
#' #stations in area of interest
#'
#' stations = inventory(stationType = "flu",
#'                      as_sf = T,
#'                      aoi = sf::st_read("./example/data/aoi_example.shp")
#'                      )
#'
#' #omit station with area equal NA, reproject to epsg of dem and export it
#'
#' sf::st_write(na.omit(stations) %>% sf::st_transform(crs = "epsg:32723")),
#' dsn = "./example/results/stations_aoi.shp",
#' delete_dsn = TRUE, delete_layer = TRUE)
#'
#' #run watersheDelimit
#' watersheDelimit(stationsPath = "./example/results/stations_aoi.shp",
#'                 flowAcumPath = "./example/results/demproducts/03flowAccumulation.tif",
#'                 flowDir8Path = "./example/results/demproducts/03flowDirection.tif",
#'                 bufferSearch = 1000,
#'                 outputDirPath = "./example/results/watershedsDelimit",
#'                 tempDirPath = "./temp"
#'                 )
#'
#'
#' @export
watersheDelimit <- function(stationsPath,
                            flowAcumPath,
                            flowDir8Path,
                            bufferSearch = 1000,
                            outputDirPath,
                            tempDirPath = "./temp") {

  # read station shapefile
  stationsPath <- sf::st_read(stationsPath, quiet = TRUE)
  names(stationsPath) <- c("state", "station_code", "lat", "long", "station_type", "area_km2", "geometry")

  # load flowaccumulation and flow direction
  flowAcumRaster <- terra::rast(flowAcumPath)
  flowDir8Raster <- terra::rast(flowDir8Path)


  areaSP <- as.numeric()

  # chect temp dir or create it
  if (dir.exists(tempDirPath) == FALSE) {
    dir.create(tempDirPath, recursive = TRUE)
  }

  # create folder to output raster and shapefiles
  dir.create(paste(outputDirPath, "/WaterShedRaster", sep = ""), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste(outputDirPath, "/WaterShedShape", sep = ""), showWarnings = FALSE, recursive = TRUE)

  # generate individual basins

  for (i in 1:nrow(stationsPath)) {

    # pixel value associated with Area of fluviometric ANA station "i"

    nPixel <- round(as.numeric(stationsPath$area_km2[i]) * 100 * 10000 / terra::res(flowAcumRaster)[1]^2, 0)

    # buffer around fluviometric ANA station "i"
    buf <- sf::st_buffer(stationsPath[i, ], bufferSearch) # fazer buffer do ponto

    # mask flow accumulation raster by station buffer
    Acum <- terra::crop(flowAcumRaster, buf)

    # identify value in flow Accumulation raster nearest "npixel"
    nearestValue <- terra::na.omit(terra::unique(Acum))[which.min(abs(terra::na.omit(terra::unique(Acum)) - nPixel) %>% dplyr::pull()), ]

    # set NA to all values different then "nearestValue"
    Acum[Acum != nearestValue] <- NA

    ## convert pour point to point and export to tempDir
    suppressWarnings(terra::as.points(Acum) %>%
      sf::st_as_sf() %>%
      sf::st_write(paste(tempDirPath,
        "/pour_",
        stationsPath$station_code[i],
        ".shp",
        sep = ""
      ),
      delete_layer = TRUE,
      append = FALSE,
      quiet = TRUE
      ))

    # obtain watershed based on pour_point
    whitebox::wbt_watershed(
      flowDir8Path,
      paste(tempDirPath,
        "/pour_",
        stationsPath$station_code[i],
        ".shp",
        sep = ""
      ),
      paste(outputDirPath,
        "/waterShedRaster/",
        "waterShed",
        stationsPath$station_code[i],
        ".tif",
        sep = ""
      )
    )

    # import watershed and convert to lighter format
    y <- terra::trim(terra::rast(paste(outputDirPath,
      "/waterShedRaster/",
      "waterShed",
      stationsPath$station_code[i],
      ".tif",
      sep = ""
    )))

    terra::writeRaster(y,
      paste(outputDirPath,
        "/waterShedRaster/",
        "waterShed",
        stationsPath$station_code[i],
        ".tif",
        sep = ""
      ),
      overwrite = TRUE,
      datatype = "INT2S"
    )

    # delimited watershed area in km²
    areaSP[i] <- sum(na.omit(terra::values(y))) * terra::res(flowAcumRaster)[1]^2 / 10000 / 100


    # convert watershed raster to shapefile and export it. *Added estimated area and ANA declared area
    q <- sf::st_as_sf(terra::as.polygons(y)) %>%
      dplyr::mutate(
        station_code = stationsPath$station_code[i],
        area_km2 = stationsPath$area_km2[i],
        area_km2p = areaSP[i]
      ) %>%
      dplyr::select(station_code, area_km2, area_km2p, geometry)

    suppressWarnings(sf::st_write(q, paste(outputDirPath, "/waterShedShape/", "waterShed", stationsPath$station_code[i], ".shp", sep = ""),
      delete_layer = TRUE,
      append = FALSE,
      quiet = TRUE
    ))

    print(paste("Station ", stationsPath$station_code[i], " Done ", i, "/", nrow(stationsPath), sep = ""))
  }

  print("Generating final files")

  # Concatente pour_points, add areas and export

  suppressWarnings(list.files(tempDirPath, pattern = ".shp", full.names = TRUE) %>%
    lapply(sf::st_read, quiet = TRUE) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      station_code = stationsPath$station_code,
      area_km2 = stationsPath$area_km2,
      area_km2p = areaSP
    ) %>%
    dplyr::select(station_code, area_km2, area_km2p, geometry) %>%
    sf::st_write(paste(outputDirPath, "/StationsSnaped.shp", sep = ""), delete_layer = TRUE, append = TRUE, quiet = TRUE))
  # Concatenate watersheds to shapefile (unested)

  suppressWarnings(list.files(paste(outputDirPath, "/waterShedShape", sep = ""), pattern = ".shp", full.names = TRUE) %>%
    lapply(sf::st_read, quiet = TRUE) %>%
    dplyr::bind_rows() %>%
    sf::st_write(paste(outputDirPath,
      "/Watersheds_unested.shp",
      sep = ""
    ),
    delete_layer = TRUE,
    append = FALSE,
    quiet = TRUE
    ))

  # concatenated waatesheds shapefile to raster

  whitebox::wbt_watershed(
    flowDir8Path,
    paste(outputDirPath, "/StationsSnaped.shp", sep = ""),
    paste(outputDirPath, "/Watersheds_nested.tif", sep = "")
  )

  # reclassify nested watershed raster values to stations code and convert to shapefile

  r <- terra::trim(terra::rast(paste(outputDirPath, "/Watersheds_nested.tif", sep = "")))

  unicos <- sort(base::unique(na.omit(terra::values(r))))

  reclassMatrix <- base::data.matrix(cbind(
    c(unicos),
    c(unicos[-1], unicos[length(unicos)] + 1),
    as.numeric(stationsPath$station_code)
  ))

  r1 <- terra::classify(r, reclassMatrix, right = FALSE)

  terra::writeRaster(r1, paste(outputDirPath, "/Watersheds_nested.tif", sep = ""), overwrite = TRUE, datatype = "INT4U")

  suppressWarnings(sf::st_as_sf(terra::as.polygons(r1)) %>%
    dplyr::mutate(
      station_code = Watersheds_nested,
      areakm2 = stationsPath$area_km2,
      areakm2_p = round(areaSP, 0)
    ) %>%
    dplyr::select(station_code, areakm2, areakm2_p, geometry) %>%
    sf::st_write(paste(outputDirPath, "/Watersheds_nested.shp", sep = ""),
      delete_layer = TRUE,
      append = FALSE,
      quiet = TRUE
    ))

  # data frame with stations and respectivily areas (hidroweb and hydrobr)

  resumo <- base::data.frame(
    estCod = as.numeric(stationsPath$station_code),
    areaHidroWeb_km2 = as.numeric(stationsPath$area_km2),
    areaPredicted_km2 = round(areaSP, 0)
  )

  resumo$error_porcent <- round((resumo[, 3] - resumo[, 2]) / resumo[, 2] * 100, 2)

  unlink("./temp", recursive = TRUE)

  print("Job Done! Congratz!")

  return(resumo)
}
