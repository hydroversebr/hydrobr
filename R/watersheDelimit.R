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
#' watersheDelimit(stationsPath = "ana_station.shp",
#'                  flowAcumPath = "flowAccum.tif",
#'                  flowDir8Path = "./flowDir8.tif",
#'                  bufferSearch = 1000,
#'                  outputDirPath = "./watersheds",
#'                  tempDirPath = "./temp")
#'
#'
#' @export
watersheDelimit = function(stationsPath,
                          flowAcumPath,
                          flowDir8Path,
                          bufferSearch = 1000,
                          outputDirPath,
                          tempDirPath = "./temp"){

  stationsPath = sf::st_read(stationsPath, quiet = TRUE)
  names(stationsPath) = c("state", "station_code", "area_km2", "station_type", "geometry")
  flowAcumRaster = terra::rast(flowAcumPath)
  flowDir8Raster = terra::rast(flowDir8Path)


  areaSP = as.numeric()

  if (dir.exists(tempDirPath) == FALSE){
    dir.create(tempDirPath, recursive = TRUE)
  }

  dir.create(paste(outputDirPath, "/WaterShedRaster", sep = ""), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste(outputDirPath, "/WaterShedShape", sep = ""),  showWarnings = FALSE, recursive = TRUE)

  #Gerar bacias individualizadas
  i = 1
  for (i in 1:nrow(stationsPath)){

    nPixel = round(as.numeric(stationsPath$area_km2[i])*100*10000/terra::res(flowAcumRaster)[1]^2,0) #numero de pixels acumulados associado à area de drenagem da estação

    buf = sf::st_buffer(stationsPath[i,], bufferSearch) #fazer buffer do ponto

    Acum = terra::crop(flowAcumRaster, buf) #cortar fluxo acumulado pelo buffer

    nearestValue = base::na.omit(terra::unique(Acum))[which.min(abs(base::na.omit(terra::unique(Acum))-nPixel) %>% dplyr::pull()),] #pegar valor mais proximo "npixel" no flowaccumulations

    Acum[Acum!=nearestValue] = NA #todos outros valores transformados em NA

    ##converter raster da foz para shape formato ponto e exportar para diretório temporário
    terra::as.points(Acum) %>%
      sf::st_as_sf() %>%
      sf::st_write(paste(tempDirPath,
                     "/pour_",
                     stationsPath$station_code[i],
                     ".shp",
                     sep = ""),
               delete_layer = TRUE,
               append = FALSE,
               quiet = TRUE)

    #delimitar bacia com base no ponto da foz
    whitebox::wbt_watershed(flowDir8Path,
                  paste(tempDirPath,
                        "/pour_",
                        stationsPath$station_code[i],
                        ".shp",
                        sep = ""),
                  paste(outputDirPath,
                        "/waterShedRaster/",
                        "waterShed",
                        stationsPath$station_code[i],
                        ".tif",
                        sep = ""))

    #importar bacia delimitada
    y = terra::trim(terra::rast(paste(outputDirPath,
                               "/waterShedRaster/",
                               "waterShed",
                               stationsPath$station_code[i],
                               ".tif",
                               sep = "")))

    #Exportar para formato mais leve
    terra::writeRaster(y,
                       paste(outputDirPath,
                             "/waterShedRaster/",
                             "waterShed",
                             stationsPath$station_code[i],
                             ".tif",
                             sep = ""),
                       overwrite = TRUE,
                       datatype = "INT2S")

    #Área em km² da bacia delimitada com MDE
    areaSP[i] = sum(na.omit(terra::values(y)))*terra::res(flowAcumRaster)[1]^2/10000/100


    #converter raster de bacia para shapefile e adicionar area hidroweb e estimada
    q = sf::st_as_sf(terra::as.polygons(y)) %>%
      dplyr::mutate(station_code = stationsPath$station_code[i],
                    area_km2 = stationsPath$area_km2[i],
                    area_km2p = areaSP[i]) %>%
      dplyr::select(station_code, area_km2, area_km2p, geometry)

    #exportar bacia em shapefile
    sf::st_write(q, paste(outputDirPath, "/waterShedShape/","waterShed", stationsPath$station_code[i],".shp", sep = ""),
                 delete_layer = TRUE,
                 append = FALSE,
                 quiet = TRUE)

    print(paste("Station ", stationsPath$station_code[i], " Done ", i, "/", nrow(stationsPath), sep = ""))
  }

  #Concatenar fozes das bacias, adicionar áreas hidroweb e estimada. Exportar depois

  list.files(tempDirPath, pattern = ".shp", full.names = TRUE) %>%
    lapply(sf::st_read, quiet = TRUE) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(station_code = stationsPath$station_code,
           area_km2 = stationsPath$area_km2,
           area_km2p = areaSP) %>%
    dplyr::select(station_code, area_km2, area_km2p, geometry) %>%
    sf::st_write(paste(outputDirPath, "/StationsSnaped.shp", sep = ""), delete_layer = TRUE, append = TRUE, quiet = TRUE)

  #Concatenar bacias das estações (unested)

  list.files(paste(outputDirPath,"/waterShedShape", sep =""), pattern = ".shp", full.names = TRUE) %>%
    lapply(sf::st_read, quiet = TRUE) %>%
    dplyr::bind_rows() %>%
    sf::st_write(paste(outputDirPath, "/Watersheds_unested.shp", sep = ""), delete_layer = TRUE, append = FALSE, quiet = TRUE)

  #Concatenar bacias das estações (unested)

  print("Generating final files")

  #Gerar bacia concatenadas no formato raster

  whitebox::wbt_watershed(flowDir8Path,
                paste(outputDirPath, "/StationsSnaped.shp", sep = ""),
                paste(outputDirPath,"/Watersheds.tif", sep = ""))



  #parei aqui #estaçoes invertidas

  r = terra::trim(terra::rast(paste(outputDirPath,"/Watersheds.tif", sep = "")))

  unicos = sort(base::unique(na.omit(terra::values(r))))

  reclassMatrix = base::data.matrix(cbind(c(unicos),
                                          c(unicos[-1],unicos[length(unicos)]+1),
                                          as.numeric(stationsPath$station_code)))

  r1 = terra::classify(r, reclassMatrix, right = FALSE)

  terra::writeRaster(r1, paste(outputDirPath,"/Watersheds_nested.tif", sep = ""), overwrite = TRUE, datatype = "INT4U")

  sf::st_as_sf(terra::as.polygons(r1)) %>%
    dplyr::mutate(station_code = Watersheds,
           areakm2 = stationsPath$area_km2,
           areakm2_p = round(areaSP,0)) %>%
    dplyr::select(station_code, areakm2, areakm2_p, geometry) %>%
    sf::st_write(paste(outputDirPath, "/Watersheds_nested.shp", sep = ""),
             delete_layer = TRUE,
             append = FALSE,
             quiet = TRUE)


  resumo = base::data.frame(estCod = as.numeric(stationsPath$station_code),
                            areaHidroWeb_km2 = as.numeric(stationsPath$area_km2),
                            areaPredicted_km2 = round(areaSP,0))

  resumo$error_porcent = round((resumo[,3]-resumo[,2])/resumo[,2]*100,2)

  unlink("./temp", recursive = TRUE)

  print("Job Done! Congratz!")

  return(resumo)

}


