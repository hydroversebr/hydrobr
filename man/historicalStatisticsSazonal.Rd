% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/historicalStatisticsSazonal.R
\name{historicalStatisticsSazonal}
\alias{historicalStatisticsSazonal}
\title{Historical Statistics for sazonal period (underdevelopment. available for streamdflow and annual series)}
\usage{
historicalStatisticsSazonal(
  selectStationsResultSeries,
  statistics = "Qmean",
  permanence = 95,
  pReturn = 10
)
}
\arguments{
\item{selectStationsResultSeries}{list, tibble data frame; provides a list containing
the data frames of filtered records for each station
(series output from \code{\link[=selectStations]{selectStations()}} function).}

\item{statistics}{character; indicates statistics.
\itemize{
\item The supported statistics for streamflow are:
(1) mean stream flow (Qmean);
(2) minimum of seven-day moving average of daily stream flow associated with return period (Q7T);
(3) stream flow associated with a percentage of time (Qperm);
(4) maximum stream flow (Qmax);
(5) minimum stream flow (Qmin).
\item The supported statistics are:
(1) total rainfall (Rtotal);
(2)  maximum rainfall (Rmax);
(3) rainy days (Rdays).
\item The default value is "Qmean".
}}

\item{permanence}{numeric; percentage of time if "Qperm" is choose as statistic parameter.
The default is 95 percent.}

\item{pReturn}{numeric; return period if "Q7T" is choose as statistic parameter.
The default is 10 year.}
}
\value{
tibble object containing desired statistics for boths period and the ratio (porcentage) between
historicalStatistics and sazonal historic statistics
}
\description{
Historical series is splitted in two (six months each) and \link{historicalStatistics} is computed for both period.
}
\examples{


\dontrun{

 #' # Fech a inventory of fluviometric stations for the state of Minas Gerais.

inv <- inventory(
  states = "MINAS GERAIS",
  stationType = "flu",
  as_sf = TRUE,
  aoi = NULL
)

# Download the first 10 stations from the inventory

s_data <- stationsData(
  inventoryResult = inv[1:10,],
  deleteNAstations = TRUE
)

# Organize the data for the stations

org_data <- organize(
  stationsDataResult = s_data
)

# Filter the data for desired period and quality contorl

final_data <- selectStations(
  stationsDataResult = org_data,
  mode = "yearly",
  maxMissing = 10,
  minYears = 15,
  month = 1,
  iniYear = NULL,
  finYear = NULL,
  consistedOnly = FALSE,
  plot = TRUE
)

# annual mean stream flow serie for each station
QmeanS = historicalStatisticsSazonal(final_data$series, statistics = "Qmean")



}
}
