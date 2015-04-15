\name{KA_weather}

\alias{KA_weather}

\docType{data}

\title{Weather data for Klein-Altendorf, Germany}

\description{Daily temperature data from Klein-Altendorf, Germany, for use in combination with the example phenology dataset KA_bloom.}

\usage{data(KA_weather)}

\format{
  A data frame with observations on the following 5 variables.
  \describe{
    \item{\code{Year}}{a numeric vector - the observation year}
    \item{\code{Month}}{a numeric vector - the observation month}
    \item{\code{Day}}{a numeric vector - the observation day}
    \item{\code{Tmax}}{a numeric vector - daily maximum temperature}
    \item{\code{Tmin}}{a numeric vector - daily minimum temperature}
  }
}

\source{data were collected by Achim Kunz and Michael Blanke, University of Bonn}

\references{
Luedeling E, Kunz A and Blanke M, 2013. Identification of chilling and heat requirements of cherry trees - a statistical approach. International Journal of Biometeorology 57,679-689.
}

\examples{
data(KA_weather)
}
\keyword{datasets}
