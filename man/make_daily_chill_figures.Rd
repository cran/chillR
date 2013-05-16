\name{make_daily_chill_figures}

\alias{make_daily_chill_figures}

\title{Produce image of daily chill and heat accumulation}

\description{Function to make figures showing the mean rate of chill and heat accumulation for each day of the year, as well as as the standard deviation.}

\usage{make_daily_chill_figures(daily_chill, file_path)}

\arguments{
  \item{daily_chill}{a daily chill object. This should be generated with the daily_chill function.}
  \item{file_path}{the path where data should be saved. Can either end with '/' or include a prefix for all images that are produced.}
}

\details{Chill metrics are calculated as given in the references below. Chilling Hours are all hours with temperatures between 0 and 7.2 degrees C.
Units of the Utah Model are calculated as suggested by Richardson et al. (1974) (different weights for different temperature ranges, and negation of
chilling by warm temperatures). Chill Portions are calculated according to Fishman et al. (1987a,b). More honestly, they are calculated according to an
Excel sheet produced by Amnon Erez and colleagues, which converts the complex equations in the Fishman papers into relatively simple Excel functions.
These were translated into R. References to papers that include the full functions are given below.
Growing Degree Hours are calculated according to Anderson et al. (1986), using the default values they suggest.
This function uses the Kendall package.}

\value{data frame containing all information used to make the figures that are saved. For each Julian Date, means and standard deviations of all chill and heat
metrics are saved. In addition, Mann-Kendall tests are performed for daily accumulations of all metrics. p and tau values from this test indicate the level of
statistical significance. This non-parametric test is reliable for time series data.}

\references{
Model references:

Chilling Hours:

Weinberger JH (1950) Chilling requirements of peach varieties. Proc Am Soc Hortic Sci 56, 122-128

Bennett JP (1949) Temperature and bud rest period. Calif Agric 3 (11), 9+12

Utah Model: 

Richardson EA, Seeley SD, Walker DR (1974) A model for estimating the completion of rest for Redhaven and Elberta peach trees. HortScience 9(4), 331-332

Dynamic Model:

Erez A, Fishman S, Linsley-Noakes GC, Allan P (1990) The dynamic model for rest completion in peach buds. Acta Hortic 276, 165-174

Fishman S, Erez A, Couvillon GA (1987a) The temperature dependence of dormancy breaking in plants - computer simulation of processes studied under controlled temperatures. J Theor Biol 126(3), 309-321

Fishman S, Erez A, Couvillon GA (1987b) The temperature dependence of dormancy breaking in plants - mathematical analysis of a two-step model involving a cooperative transition. J Theor Biol 124(4), 473-483

Growing Degree Hours:

Anderson JL, Richardson EA, Kesner CD (1986) Validation of chill unit and flower bud phenology models for 'Montmorency' sour cherry. Acta Hortic 184, 71-78

Model comparisons and model equations:

Luedeling E, Zhang M, Luedeling V and Girvetz EH, 2009. Sensitivity of winter chill models for fruit and nut trees to climatic changes expected in California's Central Valley. Agriculture, Ecosystems and Environment 133, 23-31

Luedeling E, Zhang M, McGranahan G and Leslie C, 2009. Validation of winter chill models using historic records of walnut phenology. Agricultural and Forest Meteorology 149, 1854-1864

Luedeling E and Brown PH, 2011. A global analysis of the comparability of winter chill models for fruit and nut trees. International Journal of Biometeorology 55, 411-421

Luedeling E, Kunz A and Blanke M, 2011. Mehr Chilling fuer Obstbaeume in waermeren Wintern? (More winter chill for fruit trees in warmer winters?). Erwerbs-Obstbau 53, 145-155


Review on chilling models in a climate change context:

Luedeling E, 2012. Climate change impacts on winter chill for temperate fruit and nut production: a review. Scientia Horticulturae 144, 218-229

The chillR package

Luedeling E, Kunz A and Blanke M, in press. Identification of chilling and heat requirements of cherry trees - a statistical approach. International Journal of Biometeorology.
}
\author{Eike Luedeling}

\note{After doing extensive model comparisons, and reviewing a lot of relevant literature, I do not recommend using the Chilling Hours or Utah Models,
especially in warm climates! The Dynamic Model (Chill Portions), though far from perfect, seems much more reliable.}

\examples{

weather<-KA_weather[which(KA_weather$Year>2005),]
weather[,"Tmin"]<-interpolate_gaps(weather[,"Tmin"])$interp
weather[,"Tmax"]<-interpolate_gaps(weather[,"Tmax"])$interp

dc<-daily_chill(stack_hourly_temps(hour_file=make_hourly_temps(50.4,weather)), 11)

md<-make_daily_chill_figures(dc, paste(getwd(),"/daily_chill_",sep=""))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ chill and heat calculation }

