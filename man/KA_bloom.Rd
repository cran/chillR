\name{KA_bloom}

\alias{KA_bloom}

\docType{data}

\title{Cherry bloom data for Klein-Altendorf, Germany}

\description{Bloom data of sweet cherry var. 'Schneiders spaete Knorpelkirsche' recorded at Klein-Altendorf, Germany, the experimental station of the
University of Bonn}

\usage{data(KA_bloom)}

\format{
  A data frame with the following 2 variables.
  \describe{
    \item{\code{Year}}{a numeric vector, indicating the observation year}
    \item{\code{pheno}}{ a vector that, when coerced by as.numeric, contains bloom data in Julian dates (day of the year)}
  }
}

\source{data were collected by Achim Kunz and Michael Blanke, University of Bonn}

\references{
Luedeling E, Kunz A and Blanke M, 2013. Identification of chilling and heat requirements of cherry trees - a statistical approach. International Journal of Biometeorology 57,679-689.
}

\examples{
data(KA_bloom)
}
\keyword{datasets}
