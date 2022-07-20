#' Retrieves raw data for the stations inventory from ANA web API
#'
#' @encoding UTF-8
#'
#' @description Takes as input an inventory of stations (output from
#'   [hydrobr::inventory()]) and downloads raw stations data from the Brazilian
#'   National Water Agency (ANA). The user can choose wether to maintain
#'   stations with missing data.
#'
#' @param inventoryResult tibble data frame; provides the station inventory (output
#'   from [hydrobr::inventory()] function) for which to download data for.
#' @param deleteNAstations logical; should stations with no data be removed?
#'   The default is TRUE.
#'
#' @return A list containing a data frame [tibble::tibble()] object for each station.
#'   The data frame format is identical to the format provided by ANA.
#'
#' @details Improvement of the code developed by Artur Lourenço
#' (https://github.com/ArturLourenco/HidroWebFix)
#'
#' @references
#' Dados Abertos da Agência Nacional de Águas e Saneamento Básico.
#'
#' <https://dadosabertos.ana.gov.br/>
#'
#' HIDRO - Inventário pluviométrico/fluviométrico atualizado.
#'
#' <https://dadosabertos.ana.gov.br/documents/fb3426be2d4a4f9abfa90fb87b30bd4f/about>
#'
#' @examplesIf interactive()
#' # Fech a inventory of fluviometric stations for the state of Minas Gerais
#'
#' inv <- inventory(
#'   states = "MINAS GERAIS",
#'   stationType = "flu",
#'   as_sf = TRUE,
#'   aoi = NULL
#' )
#'
#' # Download the first 10 stations from the inventory
#'
#' s_data <- stationsData(
#'   inventoryResult = inv[1:10,],
#'   deleteNAstations = TRUE
#' )
#'
#' @export
stationsData <- function(inventoryResult, deleteNAstations = TRUE) {

  ## Verification if arguments are in the desired format
  # is inventoryResult an outcome from inventory function?
  if (!attributes(inventoryResult)$hydrobr_class %in% 'inventory') {
    stop(
      call. = FALSE,
      '`inventoryResults` does not inherit attribute "inventory".
         The outcome from the inventory() function should be passed as argument'
    )
  }

  # is deleteNAstations logical?
  if (!is.logical(deleteNAstations) | length(deleteNAstations) != 1) {
    stop(
      call. = FALSE,
      "`deleteNAstations` should be logical and have length == 1 (either TRUE or FALSE)."
    )
  }

  if (!any(names(inventoryResult) == "station_code")) {
    stop(
      call. = FALSE,
      '`inventoryResults` does not have a "station_code" column.
         Use inventory() function to retrieve stations inventory'
    )
  }

  ## Query
  # Create list to receive results
  serie <- list()

  # Assert stationType for query
  if (inventoryResult$stationType[1] == "fluviometric") {
    stationType <- 3
  } else if (inventoryResult$stationType[1] == "pluviometric") {
    stationType <- 2
  } else {
    stop("Inventory missing stationType column/parameter")
  }

  # Begin loop to retrieve data
  for (i in 1:nrow(inventoryResult)) {
    # Subset station_code
    station_number <- inventoryResult$station_code[i]

    # Query for station
    html_raw <- xml2::read_html(
      paste("http://telemetriaws1.ana.gov.br/ServiceANA.asmx/HidroSerieHistorica?codEstacao=",
            station_number,
            "&dataInicio=&dataFim=&tipoDados=",
            stationType,
            "&nivelConsistencia=",
            sep = ""
            )
      )

    station_df <- html_raw %>%
      # Retrieve station data
      xml2::xml_find_all(".//documentelement") %>%
      xml2::xml_children() %>%
      xml2::as_list()

    # Convert list to rows
    station_df<- lapply(station_df, function(row) {
        # Fill empty arguments with NA so columns are preserved
        row[sapply(row, function(x) { length(x) == 0})] <- NA
        row %>%
          unlist() %>%
          t() %>%
          dplyr::as_tibble()
      }) %>%
      # Binding rows
      do.call(what = dplyr::bind_rows)


    # Is there data for this station?
    if (ncol(station_df) == 1) {
      serie[[i]] <- "No Data"
    } else {
      station_df <- station_df %>%
        # Convert flow/precipitation columns to numeric and datahora to date format
        dplyr::mutate(
          dplyr::across(dplyr::matches("vazao..$"), as.numeric),
          dplyr::across(dplyr::matches("chuva..$"), as.numeric),
          dplyr::across(dplyr::matches("data"), as.Date)
        ) %>%
        dplyr::rename(data = dplyr::any_of("datahora")) %>%
        dplyr::arrange(dplyr::across('data'))

      serie[[i]] <- station_df
    }

    print(paste(i, "/", length(inventoryResult$station_code), " (station ", station_number, " done)", sep = ""))
  }

  # Name list objects according to station codes
  names(serie) <- inventoryResult$station_code

  # Remove stations with missing data?
  if (deleteNAstations == TRUE) {
    serie <- serie[serie != "No Data"]
  }

  # Create attribute to facilitate input/output check
  attr(serie, 'hydrobr_class') <- 'stationsData'
  return(serie)
}
