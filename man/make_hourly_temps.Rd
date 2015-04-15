\name{make_hourly_temps}

\alias{make_hourly_temps}

\title{Make hourly temperature record from daily data}

\description{This function generates hourly temperature records for a particular location from daily minimum and maximum temperatures and latitude.}

\usage{make_hourly_temps(latitude, year_file)}

\arguments{
  \item{latitude}{the geographic latitude (in decimal degrees) of the location of interest}
  \item{year_file}{a data frame containing data on daily minimum temperature (called Tmin), daily maximum temperature (called Tmax), and date information.
Dates can either be specified by two columns called Year and JDay, which contain the Year and Julian date (day of the year), or as three columns called Year, Month and Day.
year_file cannot have any missing values, so it may be a good idea to process the relevant columns with make_all_day_table and interpolate_gaps before.}
}
\details{Temperature estimates are based on an idealized daily temperature curve that uses a sine curve for daytime warming and a logarithmic decay function
for nighttime cooling. The input data frame can have more columns, which are preserved, but ignored in the processing. References to papers outlining the
procedures are given below.}

\value{data frame containing all the columns of year_file, plus 24 columns for hourly temperatures (called Hour_1 ... Hour_24).}
\references{Luedeling E, Kunz A and Blanke M, 2013. Identification of chilling and heat requirements of cherry trees - a statistical approach. International Journal of Biometeorology 57,679-689.

Luedeling E, Girvetz EH, Semenov MA and Brown PH, 2011. Climate change affects winter chill for temperate fruit and nut trees. PLoS ONE 6(5), e20155.

The temperature interpolation is described in

Linvill DE, 1990. Calculating chilling hours and chill units from daily maximum and minimum temperature observations. HortScience 25(1), 14-16.

Calculation of sunrise, sunset and daylength was done according to

Spencer JW, 1971. Fourier series representation of the position of the Sun. Search 2(5), 172.

Almorox J, Hontoria C and Benito M, 2005. Statistical validation of daylength definitions for estimation of global solar radiation in Toledo, Spain. Energy Conversion and Management 46(9-10), 1465-1471)
}
\author{Eike Luedeling}

\examples{
weather<-make_all_day_table(KA_weather)
Tmin_int<-interpolate_gaps(KA_weather[,"Tmin"])
weather[,"Tmin"]<-Tmin_int$interp
weather[,"Tmin_interpolated"]<-Tmin_int$missing

Tmax_int<-interpolate_gaps(KA_weather[,"Tmax"])
weather[,"Tmax"]<-Tmax_int$interp
weather[,"Tmax_interpolated"]<-Tmax_int$missing

THourly<-make_hourly_temps(50.4,weather)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utility }

