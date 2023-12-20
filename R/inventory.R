#' Retrieves stations inventory from ANA web API.
#'
#' @encoding UTF-8
#'
#' @description Downloads pluviometric and fluviometric stations inventory from
#'   the Brazilian National Water Agency (ANA) and returns a tidy
#'   data frame [tibble::tibble()] object. The inventory is optionally
#'   returned as simple features [sf::st_as_sf()] object (CRS: WGS84).
#'   The user can alternatively provide an area of interest to download all
#'   stations within its boundaries.
#'
#' @param states character vector; state(s) name(s) that you wish to download
#'   data for. Example: \dQuote{MINAS GERAIS}, \dQuote{DISTRITO FEDERAL},
#'   \dQuote{GOIÁS}, etc. Ignored if argument \code{aoi} is passed.
#' @param stationType character; indicates what type of stations
#'   to download. Supported values are \dQuote{flu} (fluviometric)
#'   and \dQuote{plu} (pluviometric). The default is \dQuote{plu}.
#' @param as_sf logical; should inventory be returned as \code{sf} object?
#'   The default is FALSE
#' @param aoi object of class \code{sf} (polygon). Provides the boundaries
#'   where stations should be limited to (optional). Overrides `states` argument.
#'
#' @return A data frame (either a \code{tibble} or a \code{sf}) containing the
#'  following columns:
#'  state: state name (chr).
#'  station_code: station unique identifier (chr).
#'  lat: latitude (dbl).
#'  long: longitude (dbl).
#'  stationType: station type (chr).
#'
#' @references
#' Dados Abertos da Agência Nacional de Águas e Saneamento Básico.
#'
#' <https://dadosabertos.ana.gov.br/>
#'
#' HIDRO - Inventário pluviométrico/fluviométrico atualizado.
#'
#' <https://dadosabertos.ana.gov.br/documents/ae318ebacb4b41cda37fbdd82125078b/about>
#'
#' @examplesIf interactive()
#' # Fetch pluviometric "plu" stations for the states of "GOIÁS" and "MINAS GERAIS"
#'
#' inventory(
#'   states = c("GOIÁS", "MINAS GERAIS"),
#'   stationType = "plu",
#'   as_sf = TRUE,
#'   aoi = NULL
#' )
#'
#' @export
inventory <- function(states, stationType = "plu", as_sf = F, aoi = NULL) {


  # Retrieve Brazilian states list
  br_states <- xml2::read_html("http://telemetriaws1.ana.gov.br//ServiceANA.asmx/HidroEstado?codUf=") %>%
    xml2::xml_find_all(".//nome") %>%
    xml2::xml_contents() %>%
    xml2::xml_text()
  br_states <- br_states[1:27]

  ## Verification if arguments are in the desired format
  suppressMessages(sf::sf_use_s2(FALSE))
  # Was a sf passed as aoi?
  if (!is.null(aoi)) {
    if (!any(class(aoi) == "sf")) {
      stop(
        call. = FALSE,
        "Provided `aoi` is not a polygon/multipolygon sf object"
      )
    } else if (!sf::st_is(aoi, c("MULTIPOLYGON", "POLYGON"))) {
      stop(
        call. = FALSE,
        "Provided `aoi` is not a polygon/multipolygon sf object"
      )
    } else {
      cat("Subsetting states in AOI... \n")

      # So it can use WGS84 for intersection
      aoi <- aoi %>% sf::st_transform(crs = "WGS84")

      # Update states character vector
      states <- suppressMessages(
        geobr::read_state(
          code_state = "all",
          year = 2017,
          simplified = TRUE,
          showProgress = FALSE
        ) %>%
          sf::st_transform(crs = "WGS84") %>%
          # Get intersection with area of interest
          sf::st_intersection(aoi) %>%
          dplyr::as_tibble() %>%
          # Select states name and convert to character vector
          dplyr::select(dplyr::all_of('name_state')) %>%
          apply(2, toupper) %>%
          as.vector()
      )
    }
  }

  # Is states a character vector?
  if (!is.character(states)) {
    stop(
      call. = FALSE,
      "`states` should be a character vector. See arguments details."
    )
  }

  # States in states list?
  if (!all(states %in% br_states)) {
    stop(
      call. = FALSE,
      paste0(
        "From `states`: ",
        paste(states[which(!states %in% br_states)], collapse = ", "),
        " not in Brazilian states list. See arguments details."
      )
    )
  }

  # stationType lenght
  if (length(stationType) != 1) {
    stop(
      call. = FALSE,
      '`stationType` should have length == 1 (either "plu" or "flu").'
    )
  }

  # stationType argument
  if (!any(stationType %in% c("plu", "flu"))) {
    stop(
      call. = FALSE,
      '`stationType` should be either "plu" or "flu".'
    )
  }

  # is as_sf logical?
  if (!is.logical(as_sf) | length(as_sf) != 1) {
    stop(
      call. = FALSE,
      "`as_sf` should be logical and have length == 1 (either TRUE or FALSE)."
    )
  }

  ## Query
  # Create empty list to receive stations info
  serief <- list()

  # Define type of station to download
  if (stationType == "flu") {
    stationType <- 1
  } else {
    stationType <- 2
  }

  # Loop to download inventory using ANA's API by state
  cat("Downloading... \n")
  for (i in 1:length(states)) {
    # Adjusting string for API query
    estadoG <- gsub(" ", "%20", states[i])

    # Raw HTML to retrieve stations inventory
    html_raw1 <- xml2::read_html(paste("http://telemetriaws1.ana.gov.br/ServiceANA.asmx/HidroInventario?codEstDE=&codEstATE=&tpEst=", stationType, "&nmEst=&nmRio=&codSubBacia=&codBacia=&nmMunicipio=&nmEstado=", estadoG, "&sgResp=&sgOper=&telemetrica=", sep = ""))

    # Scrapping info for each station
    estac <- as.data.frame(cbind(
      xml2::xml_text(xml2::xml_contents(xml2::xml_find_all(html_raw1, ".//nmestado"))),
      xml2::xml_double(xml2::xml_contents(xml2::xml_find_all(html_raw1, ".//codigo"))),
      xml2::xml_text(xml2::xml_contents(xml2::xml_find_all(html_raw1, ".//nome"))),
      xml2::xml_double(xml2::xml_contents(xml2::xml_find_all(html_raw1, ".//latitude"))),
      xml2::xml_double(xml2::xml_contents(xml2::xml_find_all(html_raw1, ".//longitude"))),
      xml2::xml_double(xml2::xml_find_all(html_raw1, ".//areadrenagem"))
    ))

    # Filtering
    estac <- estac %>%
      # Convert to tibble format
      dplyr::as_tibble() %>%
      # Reassure stations are from the desired state
      dplyr::filter(estac$V1 == states[i]) %>%
      # Eliminate duplicate rows by station_code
      dplyr::distinct_at(2, .keep_all = TRUE) %>%
      # Rename columns
      rlang::set_names(c("state", "station_code", "name", "lat", "long", "area_km2")) %>%
      # Change area_km2 class to numeric
      dplyr::mutate(dplyr::across('area_km2', .fns = as.numeric))

    # Save stations by state in list format
    serief[[i]] <- estac
    cat(states[i], " finished. \n")
  }

  # Bind all states inventory together in single data frame
  serief <- do.call(rbind, serief)

  # Create additional column to inform type of station
  if (stationType == 2) {
    serief <- serief %>%
      dplyr::mutate(stationType = "pluviometric") %>%
      # If stationType == 'pluviometric', remove drainage area column
      dplyr::select(-'area_km2')
  } else {
    serief <- serief %>%
      dplyr::mutate(stationType = "fluviometric")
  }

  # Return final object
  columns_to_select <- c('state', 'station_code', 'name', 'lat', 'long', 'stationType',  'area_km2', 'geometry')
  serief <- serief %>% sf::st_as_sf(coords = c("long", "lat"), crs = 'WGS84')
  serief <- serief %>%
    dplyr::mutate(
      lat  = sf::st_coordinates(serief$geometry)[, 2],
      long = sf::st_coordinates(serief$geometry)[, 1]
    ) %>%
    dplyr::select(dplyr::any_of(columns_to_select))

  # If aoi is provided, subset the stations
  if (!is.null(aoi)) {
    serief <- suppressMessages(serief[aoi, ])
  }

  # Return object either as tibble or sf
  if (as_sf == F) {
    serief <- serief %>% dplyr::as_tibble() %>% dplyr::select(-'geometry')
  }

  # Create attribute to facilitate input/output check
  # attr(serief, 'hydrobr_class') <- 'inventory'
  return(serief)
}


