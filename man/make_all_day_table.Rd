\name{make_all_day_table}

\alias{make_all_day_table}

\title{Fill in missing days in incomplete time series}

\description{Time series often have gaps, and these are often not marked by 'no data' values but simply missing from the dataset.
This function completes the time series by adding lines for all these missing records. For these lines, all values are set to 'NA'.}

\usage{
make_all_day_table(tab)
}

\arguments{
  \item{tab}{   a data.frame containing a time series dataset. It should have columns c("Year", "Month", "Day") or c("YEAR", "MONTH","DAY")
  or "YEARMODA".}
}

\value{   data frame containing all the columns of the input data frame, but one row for each day between the start and end of the dataset.
Data values for the missing rows are filled in as 'NA'. Dates are expressed as c("YEARMODA","DATE","Year","Month","Day"). In this, 'DATE' is the
date in ISOdate format.}

\references{
Luedeling E, Kunz A and Blanke M, 2013. Identification of chilling and heat requirements of cherry trees - a statistical approach. International Journal of Biometeorology 57,679-689.
}
\author{Eike Luedeling}


\examples{
#use a subset of the KA_weather dataset and add an additional day after a gap
KA_weather_gap<-rbind(KA_weather[1:100,],c(Year=1998,Month=6,Day=3,Tmax=26,Tmin=14)) 
#fill in the gaps
make_all_day_table(KA_weather_gap)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utility }
