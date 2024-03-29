% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selectStations.R
\encoding{UTF-8}
\name{selectStations}
\alias{selectStations}
\title{Summarizes and filter the stations (organized) data by month or year}
\usage{
selectStations(
  organizeResult,
  mode = "yearly",
  maxMissing = 10,
  minYears = 15,
  month = 1,
  iniYear = NULL,
  finYear = NULL,
  consistedOnly = FALSE,
  plot = TRUE
)
}
\arguments{
\item{organizeResult}{list, tibble data frame; provides a list containing
the data frames of raw records for each station downloaded from ANA web API
(output from \code{\link[=stationsData]{stationsData()}} function).}

\item{mode}{character; indicates in which scale to check missing data, 'monthly'
or 'yearly'. The default is 'yearly'.}

\item{maxMissing}{numeric; indicates the maximum threshold of missing data allowed.
The default is 10 percent.}

\item{minYears}{numeric; indicates the minimum years of complete data allowed. The
default is 15 years.}

\item{month}{numeric; indicates the month when the water year begins. The default is
1 (use civil year).}

\item{iniYear}{numeric; filters the time series to begin on this year (inclusive).
If you choose to use water year instead of civil year, e.g., month = 6,
the first observation used is from the date "01-06-\code{iniYear}".
The default is NULL (use entire period).}

\item{finYear}{numeric; filters the time series to end on this year (inclusive).
If you choose to use water year instead of civil year, e.g., month = 6,
the last observation used is from the date "31-05-\code{finYear}".
The default is NULL (use entire period).}

\item{consistedOnly}{logical; should only consisted data be considered?
The default is FALSE.}

\item{plot}{logical; plot the figure? The default is TRUE. The figure is saved
regardless.}
}
\value{
A list containing 4 objects:
\itemize{
\item a list containing the data frames \code{\link[tibble:tibble]{tibble::tibble()}} for each station after
removing periods exceeding \code{maxMissing} and filtering out stations which
observational period is shorter than \code{minYears}.
\item a failureMatrix indicating if the period exceeds the threshold of \code{maxMissing}
data and columns only  for stations with at least \code{minYears} of complete
observational data
\item a missingMatrix indicating the percentage of missing data and columns only
for stations with at least \code{minYears} of complete observational data
\item the saved plot.
}
}
\description{
Takes as input a list containing data frames of organized records
for each station (output from \code{\link[=organize]{organize()}}) and (i) filters the time
series within a range of years, (ii) filters out months or years exceeding
the maximum threshold of missing data, and (iii) filters out stations
with less than a minimum years of complete observations.
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

org_data <- organize(stationsDataResult = s_data)

# Filter the data for desired period and quality contorl

final_data <- selectStations(
  organizeResult = org_data,
  mode = "yearly",
  maxMissing = 10,
  minYears = 15,
  month = 1,
  iniYear = NULL,
  finYear = NULL,
  consistedOnly = FALSE,
  plot = TRUE
)
\dontshow{\}) # examplesIf}
}
