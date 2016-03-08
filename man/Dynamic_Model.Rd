\name{Dynamic_Model}

\alias{Dynamic_Model}

\title{Calculation of cumulative chill according to the Dynamic Model}

\description{This function calculates winter chill for temperate trees according to the Dynamic Model.}

\usage{
Dynamic_Model(HourTemp,summ=TRUE)
}

\arguments{
  \item{HourTemp}{   Vector of hourly temperatures.}
  \item{summ}{   Boolean parameter indicating whether calculated metrics should be provided as cumulative values over the entire record (TRUE) or as the actual accumulation for each hour (FALSE).}
}
\details{  Chill Portions are calculated as suggested by Erez et al. (1990).}

\value{   Vector of length length(HourTemp) containing the cumulative Chill Portions over the entire duration of HourTemp.}

\references{
Dynamic Model references:

Erez A, Fishman S, Linsley-Noakes GC, Allan P (1990) The dynamic model for rest completion in peach buds. Acta Hortic 276, 165-174

Fishman S, Erez A, Couvillon GA (1987a) The temperature dependence of dormancy breaking in plants - computer simulation of processes studied under controlled temperatures. J Theor Biol 126(3), 309-321

Fishman S, Erez A, Couvillon GA (1987b) The temperature dependence of dormancy breaking in plants - mathematical analysis of a two-step model involving a cooperative transition. J Theor Biol 124(4), 473-483
}

\author{Eike Luedeling}

\examples{
weather<-fix_weather(KA_weather[which(KA_weather$Year>2006),])

hourtemps<-stack_hourly_temps(weather,latitude=50.4)

Dynamic_Model(hourtemps$hourtemps$Temp)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ chill and heat calculation }
