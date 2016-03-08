\name{GDH}

\alias{GDH}

\title{Calculation of cumulative heat according to the Growing Degree Hours Model}

\description{This function calculates heat for temperate trees according to the Growing Degree Hours Model.}

\usage{
GDH(HourTemp,summ=TRUE)
}

\arguments{
  \item{HourTemp}{   Vector of hourly temperatures.}
  \item{summ}{   Boolean parameter indicating whether calculated metrics should be provided as cumulative values over the entire record (TRUE) or as the actual accumulation for each hour (FALSE).}
}
\details{  Growing Degree Hours are calculated as suggested by Anderson et al. (1986).}

\value{   Vector of length length(HourTemp) containing the cumulative Growing Degree Hours over the entire duration of HourTemp.}

\references{
Growing Degree Hours reference:

Anderson JL, Richardson EA, Kesner CD (1986) Validation of chill unit and flower bud phenology models for 'Montmorency' sour cherry. Acta Hortic 184, 71-78

}

\author{Eike Luedeling}

\examples{

weather<-fix_weather(KA_weather[which(KA_weather$Year>2006),])

hourtemps<-stack_hourly_temps(weather,latitude=50.4)

GDH(hourtemps$hourtemps$Temp)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ chill and heat calculation }
