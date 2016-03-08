\name{california_stations}

\alias{california_stations}

\docType{data}

\title{Weather stations in California}

\description{This is a list of weather stations in California that are contained in the UC IPM database. This can also be generated with make_california_UCIPM_station_list(), but this takes quite a while. So this dataset is supposed to be a shortcut to this.}

\usage{data(california_stations)}

\format{
  a data.frame containing stations from the California UC IPM database (), with the following columns: "Name", "Code", "Interval", "Lat", "Long", "Elev".
  \describe{
    \item{\code{Name}}{mame of the weather station}
    \item{\code{Code}}{ code of the weather station, indicating the name and the database it comes from}
    \item{\code{Interval}}{ period of available data (as character string)}
    \item{\code{Lat}}{ latitude of the station}
    \item{\code{Long}}{ longitude of the station}
    \item{\code{Elev}}{ elevation of the station}
  }
}

\source{UC IPM website: http://www.ipm.ucdavis.edu/WEATHER/index.html}

\examples{
data(california_stations)
}
\keyword{datasets}
