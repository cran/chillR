\name{PLS_pheno}

\alias{PLS_pheno}

\title{Partial Least Squares analysis of phenology vs. daily mean temperatures}

\description{This function conducts a Partial Least Squares (PLS) regression analysis relating an annual biological phenomenon,
e.g. fruit tree flowering or leaf emergence, to mean daily temperatures of the preceding 12 months. It produces figures that illustrate
statistical correlations between temperature variation during certain phases and the timing of phenological event.}

\usage{
PLS_pheno(weather_data_frame, bio_data_frame, split_month, PLS_results_path, runn_mean,
crossvalidate = TRUE, use_Tmean = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{weather_data_frame}{   a dataframe containing daily minimum and maximum temperature data (in columns called Tmin and Tmax, respectively),
and/or mean daily temperature (in a column called Tmean). There also has to be a column for Year and one for JDay (the Julian date, or day of the year).
Alternatively, the date can also be given in three columns (Years, Month and Day).}
  \item{bio_data_frame}{   a data frame that contains information on the timing of phenological events by year. It should consist of two columns called Year and pheno.
Data in the pheno column should be in Julian date (day of the year).}
  \item{split_month}{   the procedure analyzes data by phenological year, which can start and end in any month during the calendar year (currently only at the beginning of a month).
This variable indicates the last month (e.g. 5 for May) that should be included in the record for a given phenological year. All subsequent months are
assigned to the following phenological year.}
  \item{PLS_results_path}{   The path where analysis outputs should be saved. Should end with '/'}
  \item{runn_mean}{   application of a running mean function to daily mean temperatures before running the PLS procedure substantially enhances the clarity
of outputs. runn_mean requires an odd integer value specifying how many days should be included in this running mean. runn_mean=11 has usually produced
good results.}
  \item{crossvalidate}{   boolean variable indicating whether or not to use cross validation in the PLS procedure. Defaults to TRUE, which should be acceptable in most cases.}
  \item{use_Tmean}{   boolean variable indicating whether or not the column Tmean from the weather_data_frame should be used as input for the PLS
analysis. If this is set to FALSE, Tmean is calculated as the arithmetic mean of Tmin and Tmax.}
}
\details{PLS regression is useful for exploring the relationship between daily temperature data and biological phenomena that only occur once per year.
The statistical challenge is that a normally quite small number of observations must be related to variation in a much larger number (365) of daily temperatures,
which are also highly autocorrelated. Most regression approaches are not suitable for this, but PLS regression offers a potential solution. The method is frequently used
in chemometrics and hyperspectral remote sensing, where similar statistical challenges are encountered. The basic mechanism is that PLS first constructs latent factors (similar to
principal components) from the independent data (temperatures) and then uses these components for the regression. The contribution of each individual variable to the PLS
model is then evaluated with two main metrics: the Variable Importance in the Projection statistic (VIP) indicates how much variation in a given independent variable is correlated
with variation in the dependent variable. A threshold of 0.8 is often used for determining importance. The standardized model coefficients of the PLS model then give an indication
of the direction and strength of the effect, e.g. if coefficients are positive and high, high values for the respective independent variable are correlated with high values of the
dependent variable (e.g. late occurrence of a phenological stage).
This procedure was inspired by the challenge of explaining variation in bloom and leaf emergence dates of temperate fruit trees in Mediterranean climates. These are generally
understood to result from (more of less) sequential fulfillment of a chilling and a forcing requirement. During the chilling phase, cool temperatures are needed; during the forcing
phase, trees need heat. There is no easily visible change in tree buds that would indicate the transition between these two phases, making it difficult to develop a good model
of these processes. Where long-term phenology data are available and can be couple with daily temperature records, PLS regression allows detection of the chilling/forcing transition.
This procedure has not often been applied to biological phenomena at the time of writing this, and there may be constraints to how generally applicable it is.
Yet is has passed the test of scientific peer review a few times, and it has produced plausible results in a number of settings.
This package draws heavily from the pls package.}

\value{
 \item{weather_file}{   the weather dataset used for the PLS analysis. This is similar to the weather_data_frame input, except that possible gaps have been
interpolated. There are also columns that show whether or not this happened for each daily temperature value.}   
 \item{PLS_output}{   a data frame containing all important outputs of the PLS regression. Columns are Date (in MMDD format), JDay (Julian date, or day of the year),
Coefficient (the PLS model coefficient for each daily temperature variable), and VIP (the Variable Importance in the Projection score).}
}

\references{
The method is described here:

Luedeling E and Gassner A, 2012. Partial Least Squares Regression for analyzing walnut phenology in California. Agricultural and Forest Meteorology 158, 43-52.

Wold S (1995) PLS for multivariate linear modeling. In: van der Waterbeemd H (ed) Chemometric methods in molecular design: methods and principles in medicinal chemistry, vol 2. Chemie, Weinheim, pp 195-218.

Wold S, Sjostrom M, Eriksson L (2001) PLS-regression: a basic tool of chemometrics. Chemometr Intell Lab 58(2), 109-130.

Mevik B-H, Wehrens R, Liland KH (2011) PLS: Partial Least Squares and Principal Component Regression. R package version 2.3-0. http://CRAN.R-project.org/package0pls.

Some applications:

Luedeling E, Kunz A and Blanke M, in press. Identification of chilling and heat requirements of cherry trees - a statistical approach. International Journal of Biometeorology.

Yu H, Luedeling E and Xu J, 2010. Stronger winter than spring warming delays spring phenology on the Tibetan Plateau. Proceedings of the National Academy of Sciences (PNAS) 107 (51), 22151-22156.

Yu H, Xu J, Okuto E and Luedeling E, 2012. Seasonal Response of Grasslands to Climate Change on the Tibetan Plateau. PLoS ONE 7(11), e49230.
}
\author{Eike Luedeling}

\examples{
weather<-KA_weather
weather[,"Tmin"]<-interpolate_gaps(weather[,"Tmin"])$interp
weather[,"Tmax"]<-interpolate_gaps(weather[,"Tmax"])$interp

PLS_results_path<-paste(getwd(),"/PLS_output",sep="")

PLS_results<-PLS_pheno(
  weather_data_frame=KA_weather,
  split_month=6,   #last month in same year
  bio_data_frame=KA_bloom,
  PLS_results_path=PLS_results_path)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ phenology analysis }
