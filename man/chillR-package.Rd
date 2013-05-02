\name{chillR-package}
\alias{chillR-package}
\alias{chillR}
\docType{package}
\title{Statistical methods for phenology analysis in temperate fruit trees}
\description{
This package can be used to do a range of analyses on long-term weather records and - more importantly - on long-term observational records of the 
phenology of temperate vegetation, i.e. vegetation with a chilling and a heat requirement that must be fulfilled in order to initiate spring phenology.
}
\details{
\tabular{ll}{
Package: \tab chillR\cr
Type: \tab Package\cr
Version: \tab 0.54\cr
Date: \tab 2013-05-15\cr
License: \tab The "GNU General Public License" version 3\cr
}

}
\author{Eike Luedeling
Maintainer: Eike Luedeling <eike@eikeluedeling.com>
}
\references{
Applications of some of the methods in the package:

Luedeling E, Zhang M, Luedeling V and Girvetz EH, 2009. Sensitivity of winter chill models for fruit and nut trees to climatic changes expected in California's Central Valley. Agriculture, Ecosystems and Environment 133, 23-31.

Luedeling E, Zhang M, McGranahan G and Leslie C, 2009. Validation of winter chill models using historic records of walnut phenology. Agricultural and Forest Meteorology 149, 1854-1864.

Luedeling E and Brown PH, 2011. A global analysis of the comparability of winter chill models for fruit and nut trees. International Journal of Biometeorology 55, 411-421.

Luedeling E, Kunz A and Blanke M, 2011. Mehr Chilling fuer Obstbaeume in waermeren Wintern? (More winter chill for fruit trees in warmer winters?). Erwerbs-Obstbau 53, 145-155.

Review on chilling models in a climate change context:

Luedeling E, 2012. Climate change impacts on winter chill for temperate fruit and nut production: a review. Scientia Horticulturae 144, 218-229.

The chillR package:

Luedeling E, Kunz A and Blanke M, in press. Identification of chilling and heat requirements of cherry trees - a statistical approach. International Journal of Biometeorology.
}

\keyword{ package }

\examples{

weather<-KA_weather[which(KA_weather$Year>2004&!(KA_weather$Year==2005&KA_weather$Month<6)),]
weather[,"Tmin"]<-interpolate_gaps(weather[,"Tmin"])$interp
weather[,"Tmax"]<-interpolate_gaps(weather[,"Tmax"])$interp

PLS_results_path<-paste(getwd(),"/PLS_output",sep="")

PLS_results<-PLS_pheno(
  weather_data_frame=weather,
  split_month=6,   #last month in same year
  bio_data_frame=KA_bloom,
  PLS_results_path=PLS_results_path)

weather<-PLS_results$weather_file

THourly<-make_hourly_temps(50.4,weather)

stack<-stack_hourly_temps(hour_file=THourly)
cc<-chilling(stack,305,60)
}
