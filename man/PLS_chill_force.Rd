\name{PLS_chill_force}

\alias{PLS_chill_force}

\title{Partial Least Squares analysis of phenology vs. accumulated daily chill and heat}

\description{This function conducts a Partial Least Squares (PLS) regression analysis relating an annual biological phenomenon,
e.g. fruit tree flowering or leaf emergence, to mean daily rates of chill (with three models) and heat accumulation of the preceding 12 months.
It produces figures that illustrate statistical correlations between temperature variation during certain phases and the timing of phenological events.}

\usage{
PLS_chill_force(daily_chill, bio_data_frame, split_month, PLS_results_path,
crossvalidate = TRUE, image_type = "png", colorscheme = "normal")
}

\arguments{
  \item{daily_chill}{   a daily chill object. This should be generated with the daily_chill function.}
  \item{bio_data_frame}{   a data frame that contains information on the timing of phenological events by year. It should consist of two columns called Year and pheno.
Data in the pheno column should be in Julian date (day of the year).}
  \item{split_month}{   the procedure analyzes data by phenological year, which can start and end in any month during the calendar year (currently only at the beginning of a month).
This variable indicates the last month (e.g. 5 for May) that should be included in the record for a given phenological year. All subsequent months are
assigned to the following phenological year.}
  \item{PLS_results_path}{   The path where analysis outputs should be saved. Should end with '/'}
  \item{crossvalidate}{   boolean variable indicating whether or not to use cross validation in the PLS procedure. Defaults to TRUE, which should be acceptable in most cases.}
  \item{image_type}{   the type of image to produce. This currently has only two options: "tiff" or anything else (the default). If this is not "tiff", a png image is produced. The "tiff" option was
added to produce publishable figures that adhere to the requirements of most scientific journals.}
  \item{colorscheme}{   the color scheme for the figure. This currently has only two options: "bw" or anythings else (the default). "bw" produces a grayscale
image, otherwise the image is in color}
}

\details{PLS regression is useful for exploring the relationship between daily chill and heat accumulation rates and biological phenomena that only occur once per year.
The statistical challenge is that a normally quite small number of observations must be related to variation in a much larger number (730) of daily chill and heat values,
which are also highly autocorrelated. Most regression approaches are not suitable for this, but PLS regression offers a potential solution. The method is frequently used
in chemometrics and hyperspectral remote sensing, where similar statistical challenges are encountered. The basic mechanism is that PLS first constructs latent factors (similar to
principal components) from the independent data (daily chill and heat accumulation) and then uses these components for the regression. The contribution of each individual variable to the PLS
model is then evaluated with two main metrics: the Variable Importance in the Projection statistic (VIP) indicates how much variation in a given independent variable is correlated
with variation in the dependent variable. A threshold of 0.8 is often used for determining importance. The standardized model coefficients of the PLS model then give an indication
of the direction and strength of the effect, e.g. if coefficients are positive and high, high values for the respective independent variable are correlated with high values of the
dependent variable (e.g. late occurrence of a phenological stage).
This procedure was inspired by the challenge of explaining variation in bloom and leaf emergence dates of temperate fruit trees in Mediterranean climates. These are generally
understood to result from (more of less) sequential fulfillment of a chilling and a forcing requirement. During the chilling phase, cool temperatures are needed; during the forcing
phase, trees need heat. There is no easily visible change in tree buds that would indicate the transition between these two phases, making it difficult to develop a good model
of these processes. Where long-term phenology data are available and can be coupled with daily chill and heat records (derived from daily temperature data),
PLS regression allows detection of the chilling/forcing transition.
This procedure has not often been applied to biological phenomena at the time of writing this, and there may be constraints to how generally applicable it is.
Yet is has passed the test of scientific peer review a few times, and it has produced plausible results in a number of settings.
This package draws heavily from the pls package.

Chill metrics are calculated as given in the references below. Chilling Hours are all hours with temperatures between 0 and 7.2 degrees C.
Units of the Utah Model are calculated as suggested by Richardson et al. (1974) (different weights for different temperature ranges, and negation of
chilling by warm temperatures). Chill Portions are calculated according to Fishman et al. (1987a,b). More honestly, they are calculated according to an
Excel sheet produced by Amnon Erez and colleagues, which converts the complex equations in the Fishman papers into relatively simple Excel functions.
These were translated into R. References to papers that include the full functions are given below.
Growing Degree Hours are calculated according to Anderson et al. (1986), using the default values they suggest.
}

\value{
 \item{weather_file}{   the daily_chill object used for the PLS analysis. This is similar to the daily_chill input.}   
 \item{PLS_output}{   a data frame containing all important outputs of the PLS regression. Columns are Date (in MMDD format), JDay (Julian date, or day of the year),
Coefficient (the PLS model coefficient for each daily temperature variable), and VIP (the Variable Importance in the Projection score).}
}

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


The PLS method is described here:

Luedeling E and Gassner A, 2012. Partial Least Squares Regression for analyzing walnut phenology in California. Agricultural and Forest Meteorology 158, 43-52.

Wold S (1995) PLS for multivariate linear modeling. In: van der Waterbeemd H (ed) Chemometric methods in molecular design: methods and principles in medicinal chemistry, vol 2. Chemie, Weinheim, pp 195-218.

Wold S, Sjostrom M, Eriksson L (2001) PLS-regression: a basic tool of chemometrics. Chemometr Intell Lab 58(2), 109-130.

Mevik B-H, Wehrens R, Liland KH (2011) PLS: Partial Least Squares and Principal Component Regression. R package version 2.3-0. http://CRAN.R-project.org/package0pls.

Some applications of the PLS procedure:

Luedeling E, Kunz A and Blanke M, in press. Identification of chilling and heat requirements of cherry trees - a statistical approach. International Journal of Biometeorology.

Yu H, Luedeling E and Xu J, 2010. Stronger winter than spring warming delays spring phenology on the Tibetan Plateau. Proceedings of the National Academy of Sciences (PNAS) 107 (51), 22151-22156.

Yu H, Xu J, Okuto E and Luedeling E, 2012. Seasonal Response of Grasslands to Climate Change on the Tibetan Plateau. PLoS ONE 7(11), e49230.

The exact procedure was used here:

Luedeling E, Guo L, Dai J, Leslie C, Blanke M. Differential responses of trees to temperature variation during the chilling and forcing phases. under review.

The chillR package:

Luedeling E, Kunz A and Blanke M, in press. Identification of chilling and heat requirements of cherry trees - a statistical approach. International Journal of Biometeorology.
}
\author{Eike Luedeling}

\note{After doing extensive model comparisons, and reviewing a lot of relevant literature, I do not recommend using the Chilling Hours or Utah Models,
especially in warm climates! The Dynamic Model (Chill Portions), though far from perfect, seems much more reliable.}

\examples{

weather<-KA_weather[which(KA_weather$Year>2004&!(KA_weather$Year==2005&KA_weather$Month<6)),]
weather[,"Tmin"]<-interpolate_gaps(weather[,"Tmin"])$interp
weather[,"Tmax"]<-interpolate_gaps(weather[,"Tmax"])$interp

dc<-daily_chill(stack_hourly_temps(hour_file=make_hourly_temps(50.4,weather)), 11)

plscf<-PLS_chill_force(daily_chill=dc, bio_data_frame=KA_bloom, split_month=6,
     PLS_results_path=paste(getwd(),"/PLS_output",sep=""),
     crossvalidate = TRUE, image_type = "png",
     colorscheme = "normal")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ phenology analysis }
\keyword{ chill and heat calculation }