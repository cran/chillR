\name{plot_PLS}

\alias{plot_PLS}

\title{Output of Partial Least Squares analysis results of phenology vs. daily mean temperatures}

\description{This function produces figures that illustrate statistical correlations between temperature
variation during certain phases and the timing of phenological event, based on a PLS analysis conducted with the
PLS_pheno or the PLS_chill_force function.}

\usage{
plot_PLS(PLS_output, PLS_results_path, VIP_threshold=0.8,colorscheme="color")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PLS_output}{   a PLS_output object - the output of the PLS_pheno function. This object is a list with a list element called PLS_summary (and an
  optional object called PLS_output). This element is a data.frame with the following columns: Date, JDay, Coef, VIP, Tmean, Tstdev. Date is the day of the year in MDD format. JDay is the Julian day (day of the year) of the year in which the biological event is observed; since the analysis will often start in the year before the event, this column often starts with negative numbers. Coef is the coefficient of the PLS regression output. VIP is the Variable Importance in the Projection, another output of the PLS regression. Tmean is the mean observed temperature of the respective day of the year, for the duration of the phenology record. Tstdev is the standard deviation of temperature on a given day of the year over the length of the phenology record.}
  \item{PLS_results_path}{   the path where analysis outputs should be saved. Should include the file name, but without suffix.}
  \item{VIP_threshold}{   the VIP threshold, above which a variable is considered important. Defaults to 0.8.}
  \item{colorscheme}{   color scheme used for plotting. For grayscale image, this should be set to "bw". Otherwise a color plot is produced.}
}
\details{Ths figure illustrates results from the PLS_pheno function, which uses Partial Least Squares (or Projection to Latent Structures) regression to examine the relationship between mean daily temperatures and the timing of an annual biological event. It produces a plot (as a bmp image) with three panels: the top panel shows the value of the VIP score for each day of the year; the middle panel shows the model coefficients and the bottom panel shows the mean temperature and its standard deviation. In the top plot, all days with VIP scores above VIP_threshold are shown in blue. In the other two panels, values for the same days are shown in red, which high VIP scores coincide with negative model coefficients, and in green for positive coefficients.
This function does not produce an output, but as side effects it produces a bmp image and a table that summarizes all data used for making the figure in the specified folder.}

\references{
The method is described here:

Luedeling E and Gassner A, 2012. Partial Least Squares Regression for analyzing walnut phenology in California. Agricultural and Forest Meteorology 158, 43-52.

Wold S, 1995. PLS for multivariate linear modeling. In: van der Waterbeemd H (ed) Chemometric methods in molecular design: methods and principles in medicinal chemistry, vol 2. Chemie, Weinheim, pp 195-218.

Wold S, Sjostrom M, Eriksson L, 2001. PLS-regression: a basic tool of chemometrics. Chemometr Intell Lab 58(2), 109-130.

Mevik B-H, Wehrens R, Liland KH, 2011. PLS: Partial Least Squares and Principal Component Regression. R package version 2.3-0. http://CRAN.R-project.org/package0pls.

Some applications:

Guo L, Dai J, Wang M, Xu J, Luedeling E, 2015. Responses of spring phenology in temperate zone trees to climate warming: a case study of apricot flowering in China. Agricultural and Forest Meteorology 201, 1-7.

Luedeling E, Kunz A and Blanke M, 2013. Identification of chilling and heat requirements of cherry trees - a statistical approach. International Journal of Biometeorology 57,679-689.

Yu H, Luedeling E and Xu J, 2010. Stronger winter than spring warming delays spring phenology on the Tibetan Plateau. Proceedings of the National Academy of Sciences (PNAS) 107 (51), 22151-22156.

Yu H, Xu J, Okuto E and Luedeling E, 2012. Seasonal Response of Grasslands to Climate Change on the Tibetan Plateau. PLoS ONE 7(11), e49230.
}
\author{Eike Luedeling}

\examples{
weather<-KA_weather
weather[,"Tmin"]<-interpolate_gaps(weather[,"Tmin"])$interp
weather[,"Tmax"]<-interpolate_gaps(weather[,"Tmax"])$interp

PLS_results<-PLS_pheno(
  weather_data=KA_weather,
  split_month=6,   #last month in same year
  bio_data=KA_bloom)
  
PLS_results_path<-paste(getwd(),"/PLS_output",sep="")
  
plot_PLS(PLS_results,PLS_results_path)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ phenology analysis }
