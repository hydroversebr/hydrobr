#' Download terraClimate monthly data
#'
#' @description
#' Download TerraClimate monthly data based on area of interest
#'
#' @param aoi spatVector object. Provides the boundaries where chirps data should be limited to (datum = WGS84).
#' @param dir_out character. Directory where you want to save the raster images that you are going to download.
#' @param variable character. Variable to download. See details for more information.
#' @param years numeric. The period in years that the function should download images.
#' @param ncores numeric. numeric. The number of processor cores to use for parallelizing the download operation. Default is 1 (no parallelization).
#' @param retry numeric. numeric. The number of retry attempts for failed downloads. Default is 100.
#' @param timeout numeric.numeric. The timeout in seconds for each download attempt. Default is 600.
#'
#' @details
#'
#' - Variable descriptions:
#'
#' aet (Actual Evapotranspiration, monthly total), units = mm
#'
#' def (Climate Water Deficit, monthly total), units = mm
#'
#' pet (Potential evapotranspiration, monthly total), units = mm
#'
#' ppt (Precipitation, monthly total), units = mm
#'
#' q (Runoff, monthly total), units = mm
#'
#' soil (Soil Moisture, total column - at end of month), units = mm
#'
#' srad (Downward surface shortwave radiation), units = W/m2
#'
#' swe (Snow water equivalent - at end of month), units = mm
#'
#' tmax (Max Temperature, average for month), units = C
#'
#' tmin (Min Temperature, average for month), units = C
#'
#' vap (Vapor pressure, average for month), units  = kPa
#'
#' ws (Wind speed, average for month), units = m/s
#'
#' vpd (Vapor Pressure Deficit, average for month), units = kpa
#'
#' PDSI (Palmer Drought Severity Index, at end of month), units = unitless
#'
#' @returns raster files of each year containing 12 layers each (1 for each month of given year).
#'
#' @references adapted from download_terraclimate function of cropDemand package.
#'
#' https://search.r-project.org/CRAN/refmans/cropDemand/html/00Index.html
#'
#' Abatzoglou, J.T., S.Z. Dobrowski, S.A. Parks, K.C. Hegewisch, 2018, Terraclimate, a high-resolution global dataset of monthly climate and climatic water balance from 1958-2015, Scientific Data,
#'
#' @export
#' @examples
#'
#'\dontrun{
#'
#'require(terra)
#'
#'area_of_interest = vect("./paracatu.shp")
#'
#'
#'pptAoi = downloadTerraClimateParallel(dir_out = "./temp/terraClimate",
#'                      variable = "ppt",
#'                     years = c(1990:2000),
#'                     ncores = 5,
#'                    aoi = area_of_interest)
#'
#'
#'
#'#set sequential plan
#'future::plan(future::sequential)
#'
#'}
#'

downloadTerraClimateParallel = function (aoi,
                                          dir_out,
                                          variable,
                                          years,
                                          ncores = 1,
                                          retry = 1000,
                                          timeout = 120)

{

  stopifnot(
    "`dir_out` parameter must be character indicating output folder path (i.e `c:/temp`)" = is.character(dir_out),
    "`variable` must be `ppt` or `eto`" = variable %in% c("ppt", "aet", "def", "pet", "q", "soil", 'srad', "swe", "tmax", "tmin", "vap", "ws", "vpd", "PDSI"),
    "`years` must be numeric vector containing years to be downloaded" = is.numeric(years),
    "`aoi` must be a polygon of class `SpatVector` (terra package)" = class(aoi) == "SpatVector",
    "`ncores`  must be numeric" = is.numeric(ncores),
    "`retry`  must be numeric" = is.numeric(retry),
    "`timeout`  must be numeric" = is.numeric(timeout))


  #criar diretório de saída
  dir.create(dir_out, recursive = F, showWarnings = FALSE)

  #converster sistema de coordenadas da área de interesse para wgs84
  aoi = terra::project(aoi, y = "epsg:4326")

  #links para download----

  links = sapply(years, function(year) {

    baseurl <- paste0("http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_",
                      variable, "_", year, ".nc")
  })

  #nomes dos arquivos de saída----

  names = sapply(years, function(year) {

    name_img <- paste0("TerraClimate_", variable, "_",
                       year, ".nc")

    outfile <- paste0(dir_out, "/", name_img)
  })


  #combinando url e diretorios de saida

  info = tibble::tibble(url = links,
                        outdir = names)


  #funcao para baixar 1 imagem

  download_tile <- function(url, destfile, timeout) {

    options(timeout = timeout)

    flag <- tryCatch(
      {
        utils::download.file(
          url = url, method = "libcurl",
          destfile = destfile, mode = "wb", quiet = FALSE
        )

        "sucesso"
      },
      error = function(e) {
        "download_failed"
      },
      warning = function(w) {
        "download_failed"
      }
    )

    return(flag)

  }

  # download_tile(url = links[1], destfile = names[1], timeout = 5)

  #funcao para baixar tiles em paralelo com retry

  # url = links
  # destfile = names
  # timeout = 60

  download_tile_with_retry <- function(url, destfile, retry, timeout) {

    failed_downloads <- which(rep(TRUE, length(url)))

    results = rep("download_failed", length(url))

    for (attempt in 1:retry) {

      # print(attempt)

      if (length(failed_downloads) > 0) {

        p <- progressr::progressor(along = 1:length(failed_downloads))

        results[failed_downloads] <- future.apply::future_lapply(failed_downloads,

                                                                 FUN = function(i) {

                                                                   p()

                                                                   download_tile(
                                                                     url = url[i],
                                                                     destfile = destfile[i],
                                                                     timeout = timeout
                                                                   )



                                                                 }
        )

        failed_downloads <- which(sapply(results, function(w) {
          unique(as.character(w) == "download_failed")

        }))
      }

      if (length(failed_downloads) == 0) {

        print(paste0("Download Complete"))

        break}

    }

    return(failed_downloads)

  }



  progressr::handlers("txtprogressbar")

  future::plan(future::multisession, workers = ncores)

  progressr::handlers(global = TRUE)

  teste = suppressWarnings(download_tile_with_retry(url = links,
                                                    destfile = names,
                                                    retry = retry,
                                                    timeout = timeout))



  future::plan(future::sequential)

  #funcao para ler rasters baixados, cortar e exportar como tif

  lerRasters = function(imagens){

    img <- terra::rast(imagens)

    terra::crs(img) <- "EPSG:4326"

    img <- terra::crop(img, aoi)

    img <- terra::mask(img, aoi)

    unlink(imagens)

    ano = substr(imagens, nchar(imagens) - 7 + 1, nchar(imagens)-3)

    names(img) = paste0(variable, "_", seq(as.Date(paste0(ano, "-01-01")), as.Date(paste0(ano, "-12-01")), by = "month"))

    terra::writeRaster(img, filename = gsub(imagens, replacement = ".tif", pattern = ".nc"),
                       filetype = "GTiff", overwrite = TRUE)

  }

  #

  finalRaster = terra::rast(lapply(names, FUN = lerRasters))

  unlink(list.files(dir_out, pattern = ".json", full.names = T))

  return(finalRaster)

}






if(getRversion() >= "2.15.1")  utils::globalVariables(c("variable"))
