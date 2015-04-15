\name{chillR-package}
\alias{chillR-package}
\alias{chillR}
\docType{package}
\title{Statistical methods for phenology analysis in temperate fruit trees}
\description{
The phenology of plants (i.e. the timing of their annual life phases) depends on climatic cues. For temperate trees and many other plants, spring phases, such as leaf emergence and flowering, have been found to result from the effects of both cool (chilling) conditions and heat. Fruit tree scientists (pomologists) have developed some metrics to quantify chilling and heat. 'chillR' contains functions for processing temperature records into chilling (Chilling Hours, Utah Chill Units and Chill Portions) and heat units (Growing Degree Hours). Regarding chilling metrics, Chill Portions are often considered the most promising, but they are difficult to calculate. This package makes it easy. 'chillR' also contains procedures for conducting a PLS analysis relating phenological dates (e.g. bloom dates) to either mean temperatures or mean chill and heat accumulation rates, based on long-term weather and phenology records.
}
\details{
\tabular{ll}{
Package: \tab chillR\cr
Type: \tab Package\cr
Version: \tab 0.55\cr
Date: \tab 2015-04-14\cr
License: \tab The "GNU General Public License" version 3\cr
}

}
\author{Eike Luedeling
Maintainer: Eike Luedeling <eike@eikeluedeling.com>
}
\references{
Applications of some of the methods in the package:

Luedeling E, Zhang M, Luedeling V and Girvetz EH, 2009. Sensitivity of winter chill models for fruit and nut trees to climatic changes expected in California's Central Valley. Agriculture, Ecosystems and Environment 133, 23-31

Luedeling E, Zhang M, McGranahan G and Leslie C, 2009. Validation of winter chill models using historic records of walnut phenology. Agricultural and Forest Meteorology 149, 1854-1864

Luedeling E and Brown PH, 2011. A global analysis of the comparability of winter chill models for fruit and nut trees. International Journal of Biometeorology 55, 411-421

Luedeling E, Kunz A and Blanke M, 2011. Mehr Chilling fuer Obstbaeume in waermeren Wintern? (More winter chill for fruit trees in warmer winters?). Erwerbs-Obstbau 53, 145-155

Luedeling E, Guo L, Dai J, Leslie C, Blanke M, 2013. Differential responses of trees to temperature variation during the chilling and forcing phases. Agricultural and Forest Meteorology 181, 33-42.

Review on chilling models in a climate change context:

Luedeling E, 2012. Climate change impacts on winter chill for temperate fruit and nut production: a review. Scientia Horticulturae 144, 218-229

The chillR package:

Luedeling E, Kunz A and Blanke M, 2013. Identification of chilling and heat requirements of cherry trees - a statistical approach. International Journal of Biometeorology 57,679-689.
}
\keyword{ package }

\examples{

weather<-KA_weather[which(KA_weather$Year>2004&!(KA_weather$Year==2005&KA_weather$Month<6)),]
weather<-make_all_day_table(weather)
weather[,"Tmin"]<-interpolate_gaps(weather[,"Tmin"])$interp
weather[,"Tmax"]<-interpolate_gaps(weather[,"Tmax"])$interp

PLS_results<-PLS_pheno(
  weather_data=KA_weather,
  split_month=6,   #last month in same year
  bio_data=KA_bloom)
  
PLS_results_path<-paste(getwd(),"/PLS_output",sep="")
  
plot_PLS(PLS_results,PLS_results_path)

THourly<-make_hourly_temps(50.4,weather)

stack<-stack_hourly_temps(hour_file=THourly)
cc<-chilling(stack,305,60)
}
