% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seriesStatistics.R
\encoding{UTF-8}
\name{seriesStatistics}
\alias{seriesStatistics}
\title{Compute streamflow or rainfall statistics by month or year}
\usage{
seriesStatistics(
  selectStationsResultSeries,
  statistics = "Qmean",
  permanence = 95,
  rainyDays = 1,
  byMonth = FALSE
)
}
\arguments{
\item{selectStationsResultSeries}{list, tibble data frame; provides a list containing
the data frames of filtered records for each station
(series output from \code{\link[=selectStations]{selectStations()}} function).}

\item{statistics}{character; indicates statistics.
\itemize{
\item The supported statistics for streamflow are:
(1) mean stream flow (Qmean); (2)  minimum of seven-day moving average of daily stream flow (Q7);
(3) stream flow associated with a percentage of time (Qperm); (4) maximum stream flow (Qmax);
and (5) minimum stream flow (Qmin).
\item The supported statistics are: (1) total rainfall (Rtotal); (2)  maximum rainfall (Rmax);
(3) rainy days (Rdays).
\item The default value is "Qmean".
}}

\item{permanence}{numeric; percentage of time if "Qperm" is choose as statistic parameter
The default is 95 percent.}

\item{rainyDays}{numeric; number of day to be consider if "Rmax" is choose as statistic parameters.
For example, if rainyDays = 2, seriesStatistics will compute max value considering 2 day of raining for each wateryear (or monthWaterYear).
Default is 1 day.}

\item{byMonth}{logical;  if byMounth = TRUE, seriesStatistics is performed for by month. default = FALSE.}
}
\value{
A list containing 3 objects:
\itemize{
\item a list containing statistic a data frame \code{\link[tibble:tibble]{tibble::tibble()}} object for each station.
\item a data frame \code{\link[tibble:tibble]{tibble::tibble()}} with statistic of all stations in wide format
\item a data frame \code{\link[tibble:tibble]{tibble::tibble()}} with statistic of all stations in longer format
}
}
\description{
Takes as input a list containing data frames of organized and filtered records
for each station (output from \code{\link[=selectStations]{selectStations()}}) and compute streamflow or rainfall
statistics by month or year.
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Fech a inventory of fluviometric stations for the state of Minas Gerais

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
Qmean_years = seriesStatistics(final_data$series, statistics = "Qmean")
\dontshow{\}) # examplesIf}
}
