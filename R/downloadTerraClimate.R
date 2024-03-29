#' Download terraClimate monthly data
#'
#' @description
#' Download TerraClimate monthly data based on area of interest
#'
#'
#' @param dir_out character. Directory where you want to save the raster images that you are going to download.
#' @param variable character. Variable to download. See details for more information
#' @param years numeric. The period in years that the function should download images.
#' @param aoi spatVector object. Provides the boundaries where chirps data should be limited to (datum = WGS84).
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
#'downloadTerraClimate(dir_out = "./temp/terraClimate",
#'                      variable = "ppt",
#'                     years = c(1990:1991),
#'                    aoi = area_of_interest)
#'
#'}
#'
#'
#'


downloadTerraClimate = function (dir_out, variable, years, aoi) {

  stopifnot(
    "`dir_out` parameter must be character indicating output folder path (i.e `c:/temp`)" = is.character(dir_out),
    "`variable` must be `ppt` or `eto`" = variable %in% c("ppt", "aet", "def", "pet", "q", "soil", 'srad', "swe", "tmax", "tmin", "vap", "ws", "vpd", "PDSI"),
    "`years` must be numeric vector containing years to be downloaded" = is.numeric(years),
    "`aoi` must be a polygon of class `SpatVector` (terra package)" = class(aoi) == "SpatVector")





  #create output dir if do not exist

  dir.create(dir_out, recursive = F, showWarnings = FALSE)

  #set crs do spatVector
  terra::crs(aoi) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


  if (variable == "ppt" | variable == "pet") {
    for (i in 1:length(years)) {

      baseurl <- paste0("http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_",
                        variable, "_", years[i], ".nc")

      name_img <- paste0("TerraClimate_", variable, "_",
                         years[i], ".nc")

      outfile <- paste0(dir_out, "/", name_img)

      utils::download.file(url = baseurl, method = "libcurl", destfile = outfile,
                           mode = "wb", quiet = FALSE)

      img <- terra::rast(list.files(dir_out, pattern = name_img,
                                    full.names = T))

      crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

      terra::crs(img) <- crs

      img <- terra::crop(img, aoi)

      img <- terra::mask(img, aoi)

      unlink(outfile)

      terra::writeRaster(img, filename = paste0(dir_out,
                                                "/", paste0(substr(name_img, 1, 21), ".tif")),
                         filetype = "GTiff", overwrite = TRUE)

      print(paste0(years[i], " Done"))

    }


    unlink(list.files(dir_out, pattern = ".json",
                      full.names = T))

    list_img <- lapply(list.files(dir_out, pattern = ".tif$",
                                  full.names = T), terra::rast)

    s <- terra::rast(list_img)

    return(s)
  }

  else {
    message("Please, pay attention. You should download the variable ppt or eto!")
  }
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("variable"))
