\name{bloom_prediction}

\alias{bloom_prediction}

\title{Bloom prediction from chilling and forcing requirements, assumed to be fulfilled strictly in sequence}

\description{This is a pretty rudimentary function to predict phenological dates from chilling and forcing requirements and hourly chilling and forcing data.
Note that there are enormous uncertainties in these predictions, which are hardly ever acknowledged. So please use this function with caution.}

\usage{bloom_prediction(HourChillTable, Chill_model, Chill_req, Heat_req)}

\arguments{
  \item{HourChillTable}{   a fairly particular data frame resulting from the chilling_hourtable function. However, the table should be processed in the way outlined
in the example. It is also important that the weather data frame was processed by the pheno_PLS function before. This will hopefully get simplified soon.}
  \item{Chill_model}{character string describing the chill model to use. Options are "Chilling_Hours", "Chill_Portions" and "Chill_Units".}
  \item{Chill_req}{Numeric parameter indicating the chilling requirement of the particular growth stage}
  \item{Heat_req}{Numeric parameter indicating the heat requirement of the particular growth stage}
}
\details{This function is a bit preliminary at the moment. It will hopefully be refined later.

Chill metrics are calculated as given in the references below. Chilling Hours are all hours with temperatures between 0 and 7.2 degrees C.
Units of the Utah Model are calculated as suggested by Richardson et al. (1974) (different weights for different temperature ranges, and negation of
chilling by warm temperatures). Chill Portions are calculated according to Fishman et al. (1987a,b). More honestly, they are calculated according to an
Excel sheet produced by Amnon Erez and colleagues, which converts the complex equations in the Fishman papers into relatively simple Excel functions.
These were translated into R. References to papers that include the full functions are given below.
Growing Degree Hours are calculated according to Anderson et al. (1986), using the default values they suggest.}

\value{data frame containing the predicted dates of chilling requirement fulfillment and timing of the phenological stage. Columns are Creqfull, Creq_year,
Crey_month, Creq_day and Creq_JDay (the row number, date and Julian date of chilling requirement fulfillement), Hreqfull, Hreq_year, Hreq_month, Hreq_day and
Hreq_JDay (the row number, date and Julian date of heat requirement fulfillment - this corresponds to the timing of the phenological event.}

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

require(chillR)
weather<-KA_weather[which(KA_weather$Year>2004),]
Tmin_int<-interpolate_gaps(weather[,"Tmin"])
weather[,"Tmin"]<-Tmin_int$interp
weather[,"Tmin_interpolated"]<-Tmin_int$missing

Tmax_int<-interpolate_gaps(weather[,"Tmax"])
weather[,"Tmax"]<-Tmax_int$interp
weather[,"Tmax_interpolated"]<-Tmax_int$missing

PLS_results_path<-paste(getwd(),"/PLS_output",sep="")

PLS_results<-PLS_pheno(
  weather_data_frame=weather,
  split_month=6,   #last month in same year
  bio_data_frame=KA_bloom,
  PLS_results_path=PLS_results_path)

weather<-PLS_results$weather_file[which(PLS_results$weather_file$Year>2007),]

THourly<-make_hourly_temps(50.4,weather)

stack<-stack_hourly_temps(hour_file=THourly)

Start_JDay=305
CT<-chilling_hourtable(stack,Start_JDay)

cchh<-CT$Chilling_Hours
ccpp<-CT$Chill_Portions
ccuu<-CT$Chill_Units
sea<-CT$Season
stdd<-CT$JDay
for (s in unique(CT$Season))
{cchh[which(sea==s)]<-cchh[which(sea==s)]-cchh[which(sea==s&stdd==round(Start_JDay))][1]
 ccpp[which(sea==s)]<-ccpp[which(sea==s)]-ccpp[which(sea==s&stdd==round(Start_JDay))][1]
 ccuu[which(sea==s)]<-ccuu[which(sea==s)]-ccuu[which(sea==s&stdd==round(Start_JDay))][1]
 
}
CT$Chilling_Hours<-cchh
CT$Chill_Portions<-ccpp
CT$Chill_Units<-ccuu

bloom_prediction(CT,Chill_model="Chill_Portions",Chill_req=60,Heat_req=5000)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ bloom prediction }

