\name{Chilling_Hours}

\alias{Chilling_Hours}

\title{Calculation of cumulative chill according to the Chilling Hours Model}

\description{This function calculates winter chill for temperate trees according to the Chilling Hours Model.}

\usage{
Chilling_Hours(HourTemp,summ=TRUE)
}

\arguments{
  \item{HourTemp}{   Vector of hourly temperatures.}
  \item{summ}{   Boolean parameter indicating whether calculated metrics should be provided as cumulative values over the entire record (TRUE) or as the actual accumulation for each hour (FALSE).}
}
\details{  Chilling Hours are calculated as suggested by Bennett (1949) (all hours with temperatures between 0 and 7.2 degrees C are considered as one Chilling Hour.}

\value{   Vector of length length(HourTemp) containing the cumulative Chilling Hours over the entire duration of HourTemp.}

\references{
Chilling Hours references:

Weinberger JH (1950) Chilling requirements of peach varieties. Proc Am Soc Hortic Sci 56, 122-128

Bennett JP (1949) Temperature and bud rest period. Calif Agric 3 (11), 9+12
}

\author{Eike Luedeling}

\note{After doing extensive model comparisons, and reviewing a lot of relevant literature, I do not recommend using the Chilling Hours,
especially in warm climates! The Dynamic Model (Chill Portions), though far from perfect, seems much more reliable.}

\examples{

weather<-fix_weather(KA_weather[which(KA_weather$Year>2006),])

hourtemps<-stack_hourly_temps(weather,latitude=50.4)

Chilling_Hours(hourtemps$hourtemps$Temp)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ chill and heat calculation }
