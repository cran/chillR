% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{PhenoFlex}
\alias{PhenoFlex}
\title{PhenoFlex}
\usage{
PhenoFlex(
  temp,
  times,
  A0 = 6319.5,
  A1 = 5.939917e+13,
  E0 = 3372.8,
  E1 = 9900.3,
  slope = 1.6,
  Tf = 4,
  s1 = 0.5,
  Tu = 25,
  Tb = 4,
  Tc = 36,
  yc = 40,
  Delta = 4,
  Imodel = 0L,
  zc = 190,
  stopatzc = TRUE,
  deg_celsius = TRUE,
  basic_output = TRUE
)
}
\arguments{
\item{temp}{Vector of temperatures.}

\item{times}{numeric vector. Optional times at which the temperatures where measured,
if not given, hourly temperatures will be assumed}

\item{A0}{numeric. Parameter \eqn{A_0}{A0} of the dynamic model}

\item{A1}{numeric. Parameter \eqn{A_1}{A1} of the dynamic model}

\item{E0}{numeric. Parameter \eqn{E_0}{E0} of the dynamic model}

\item{E1}{numeric. Parameter \eqn{E_1}{E1} of the dynamic model}

\item{slope}{numeric. Slope parameter for sigmoidal function}

\item{Tf}{numeric. Transition temperature (in degree Kelvin) for the
sigmoidal function}

\item{s1}{numeric. Slope of transition from chill to heat accumulation}

\item{Tu}{numeric. GDH optimal temperature}

\item{Tb}{numeric. GDH base temperature (lower threshold)}

\item{Tc}{numeric. GDH upper temperature (upper threshold)}

\item{yc}{numeric. Critical value defining end of chill accumulation}

\item{Delta}{numeric. Width of Gaussian heat accumulation model}

\item{Imodel}{integer. Heat accumulation model: 0 for GDH and 1 for Gaussian}

\item{zc}{numeric. Critical value of z determining the end of heat accumulation}

\item{stopatzc}{boolean. If `TRUE`, the PhenoFlex is applied until the end of the temperature series. Default is to stop once the value zc has been reached.}

\item{deg_celsius}{boolean. whether or not the temperature vector
and the model temperature parameters are
in degree Celsius (Kelvin otherwise)}

\item{basic_output}{boolean. If `TRUE`, only the bloomindex is returned as a named element of the return list.}
}
\value{
A list is returned with named element `bloomindex`, which is the index at which blooming occurs. When `basic_output=FALSE` also `x`, `y`, `z` and `xs` are
returned as named element of this list, which are numeric vectors of the same length as the input vector `temp` containing the hourly temperatures.
}
\description{
Combined model of the dynamic model for chill accumulation and the GDH model
}
\examples{
data(KA_weather)
hourtemps <- stack_hourly_temps(KA_weather, latitude=50.4)
iSeason <- genSeason(hourtemps, years=c(2009))
zc <- 190
yc <- 40
x <- PhenoFlex(temp=hourtemps$hourtemps$Temp[iSeason[[1]]],
               times=c(1: length(hourtemps$hourtemps$Temp[iSeason[[1]]])),
               zc=zc, stopatzc=TRUE, yc=yc, basic_output=FALSE)
DBreakDay <- x$bloomindex
ii <- c(1:DBreakDay)
plot(x=ii, y=x$z[ii], xlab="Hour Index", ylab="z", col="red", type="l")
abline(h=zc, lty=2)
plot(x=ii, y=x$y[ii], xlab="Hour Index", ylab="y", col="red", type="l")
abline(h=yc, lty=2)
}
\author{
Carsten Urbach <urbach@hiskp.uni-bonn.de>
}
