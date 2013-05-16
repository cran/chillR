\name{VIP}

\alias{VIP}

\title{Calculate VIP scores for PLS regression}

\description{This function calculates the Variable Importance in the Projection statistic for the Partial Least Squares regression. It is used in the PLS function.
Executing it in isolation will probably not be useful to most users.}

\usage{VIP(object)}

\arguments{
  \item{object}{an mvr object, as produced by the pls procedure or a range of other functions}
}

\details{This is required to produce the VIP scores for the PLS procedure.}

\value{ data frame with as many columns as independent variables are input into the PLS analysis. The number of columns corresponds to the number of latent components
selected for the analysis. Values in the data frame are the VIP values corresponding to each variable for the respective component.}

\references{
the function is mostly identical to the one provided on http://mevik.net/work/software/pls.html.

Here is the reference given there:

Chong, Il-Gyo & Jun, Chi-Hyuck, 2005, Performance of some variable selection methods when multicollinearity is present, Chemometrics and Intelligent Laboratory Systems 78, 103-112

This reference refers to the chillR package:

Luedeling E, Kunz A and Blanke M, in press. Identification of chilling and heat requirements of cherry trees - a statistical approach. International Journal of Biometeorology.
}

\author{Eike Luedeling, but the function was mainly copied from http://mevik.net/work/software/pls.html; the reference given there is listed below}

\examples{
require(chillR)
weather<-KA_weather
Tmin_int<-interpolate_gaps(KA_weather[,"Tmin"])
weather[,"Tmin"]<-Tmin_int$interp
weather[,"Tmin_interpolated"]<-Tmin_int$missing

Tmax_int<-interpolate_gaps(KA_weather[,"Tmax"])
weather[,"Tmax"]<-Tmax_int$interp
weather[,"Tmax_interpolated"]<-Tmax_int$missing

PLS_results_path<-paste(getwd(),"/PLS_output",sep="")

PLS_results<-PLS_pheno(
  weather_data_frame=KA_weather,
  split_month=6,   #last month in same year
  bio_data_frame=KA_bloom,
  PLS_results_path=PLS_results_path)

weather_file<-PLS_results[["weather_file"]]

bio_data<-KA_bloom
runn_mean<-11
split_month<-6

#running mean

ww<-weather_file[,"Tmean"]
rr<-weather_file[,"Tmean"]        
for (dd in 1:length(ww))
{if (dd<ceiling(runn_mean/2))
{rr[dd]<-mean(ww[1:(dd+floor(runn_mean/2))])}
if ((dd>=ceiling(runn_mean/2))&(dd<=length(ww)-ceiling(runn_mean/2)))
{rr[dd]<-mean(ww[(dd-floor(runn_mean/2)):(dd+floor(runn_mean/2))])}
if (dd>(length(ww)-ceiling(runn_mean/2)))
{rr[dd]<-mean(ww[(dd-floor(runn_mean/2)):length(ww)])}
}
weather_file[,"runn"]<-rr


seasons<-unique(weather_file$Season)

for (yy in seasons)
{yearweather<-weather_file[which(weather_file$Season==yy),]
weathervector<-c(yearweather$runn[1:365])
if(yy==seasons[1]) year_res<-weathervector else
year_res<-rbind(year_res,weathervector)
if(nrow(yearweather)==365) {labdates<-yearweather$Date;labJdates<-yearweather$JDay}
}

colnames(year_res)<-c(paste("runn_",1:365,sep=""))
year_res<-cbind(Season=seasons,year_res)
data_set<-year_res


full_seasons<-which(!is.na(rowMeans(data_set)))
data_set<-data_set[full_seasons,]
newseasons<-data_set[,"Season"]
suppressWarnings(bio_data<-
   bio_data[which(!is.na(match(bio_data[,"Year"],newseasons))),])
suppressWarnings(bio_data<-
   bio_data[which(!is.na(as.numeric(as.character(bio_data$pheno)))),])
suppressWarnings(bio<-as.numeric(as.character(bio_data$pheno)))

indep<-as.matrix(data_set[which(!is.na(match(data_set[,"Season"],bio_data$Year))),])
#indep<-as.matrix(data_set[which(data_set[,"Season"] %in% bio_data$Year),])
indep<-indep[,2:ncol(indep)]

sdindep<-apply(indep,2,sd)
sdindep[which(sdindep==0)]<-1

PLS_output<-plsr(bio ~ indep,2,validation="none",method="oscorespls",scale=sdindep)

VIP(PLS_output)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utility }

