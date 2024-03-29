% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportStationsData.R
\name{exportStationsData}
\alias{exportStationsData}
\title{Export Stations Data}
\usage{
exportStationsData(stationsTimeSerieList, directory)
}
\arguments{
\item{stationsTimeSerieList}{list of tibble; lista de tibbles contendo série de dados temporal de cada estação.
Aceita resultado das funções \link{stationsData}, \link{organize}, \link{selectStations}. Para função \link{selectStations},
utlizar elemento da lista contendo série de dados temporais.}

\item{directory}{character; diretório para o qual tabelas .xlsx de cada estação serão exportadas.}
}
\value{
dataframe contendo o início do mês úmido e seco.
}
\description{
Export station data from \link{stationsData}, \link{organize}, \link{selectStations}
}
\details{
Exporta uma tabela .xlsx para cada estação na série
}
