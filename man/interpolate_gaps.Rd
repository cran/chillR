\name{interpolate_gaps}

\alias{interpolate_gaps}

\title{Linear gap interpolation}

\description{This function linearly interpolates gaps in data series, such as daily temperature records.}

\usage{interpolate_gaps(x)}

\arguments{
  \item{x}{   a numeric vector, or a vector that can be coerced with as.numeric. Missing values are either NA or non-numeric values.}
}

\details{The function returns a list with two elements: interp is a new vector, in which all gaps in x have been linearly interpolated. missing is
a second vector, which contains information on which values were filled in by interpolation.}

\value{
 \item{interp}{   numeric vector, in which all gaps in x have been linearly interpolated}
 \item{missing}{   boolean vector of the same length as interp and x, which marks all gaps in x as TRUE}
}

\references{Luedeling E, Kunz A and Blanke M, in press. Identification of chilling and heat requirements of cherry trees - a statistical approach.
International Journal of Biometeorology.}

\author{Eike Luedeling}

\examples{
weather<-KA_weather
Tmin_int<-interpolate_gaps(KA_weather[,"Tmin"])
weather[,"Tmin"]<-Tmin_int$interp
weather[,"Tmin_interpolated"]<-Tmin_int$missing

Tmax_int<-interpolate_gaps(KA_weather[,"Tmax"])
weather[,"Tmax"]<-Tmax_int$interp
weather[,"Tmax_interpolated"]<-Tmax_int$missing
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utility }

