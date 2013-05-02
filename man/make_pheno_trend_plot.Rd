\name{make_pheno_trend_plot}

\alias{make_pheno_trend_plot}

\title{Make image showing phenology response to temperatures during two phases}

\description{The timing of many developmental stages of temperate plants is understood to depend on temperatures during two phases. This function seeks to
illustrate this dependency by plotting phenological dates as colored surface, as a function of mean temperatures during both phases, which are indicated on the
x and y axes.}

\usage{
make_pheno_trend_plot(weather_data_frame=KA_weather,
   split_month=6,   #last month in same year
   pheno=KA_bloom,use_Tmean=FALSE,Start_JDay_chill,End_JDay_chill,Start_JDay_heat,
   End_JDay_heat,outpath,file_name,plot_title,ylabel=NA,xlabel=NA,legend_label=NA,
   image_type="png",colorscheme="normal")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{weather_data_frame}{   a dataframe containing daily minimum and maximum temperature data (in columns called Tmin and Tmax, respectively),
and/or mean daily temperature (in a column called Tmean). There also has to be a column for Year and one for JDay (the Julian date, or day of the year).
Alternatively, the date can also be given in three columns (Year, Month and Day).}
  \item{split_month}{   the procedure analyzes data by phenological year, which can start and end in any month during the calendar year (currently only at the beginning of a month).
This variable indicates the last month (e.g. 5 for May) that should be included in the record for a given phenological year. All subsequent months are
assigned to the following phenological year.}
  \item{pheno}{   a data frame that contains information on the timing of phenological events by year. It should consist of two columns called Year and pheno.
Data in the pheno column should be in Julian date (day of the year).}
  \item{use_Tmean}{   boolean variable indicating whether or not the column Tmean from the weather_data_frame should be used as input for the PLS
analysis. If this is set to FALSE, Tmean is calculated as the arithmetic mean of Tmin and Tmax.}
  \item{Start_JDay_chill}{   the Julian date, on which the first relevant period (e.g. the chilling phase) starts}
  \item{End_JDay_chill}{   the Julian date, on which the first relevant period (e.g. the chilling phase) ends}
  \item{Start_JDay_heat}{   the Julian date, on which the second relevant period (e.g. the forcing phase) starts}
  \item{End_JDay_heat}{   the Julian date, on which the second relevant period (e.g. the forcing phase) ends}
  \item{outpath}{   the output path}
  \item{file_name}{   the output file name}
  \item{plot_title}{   the title of the plot}
  \item{ylabel}{   the label for the y-axis. There is a default, but in many cases, it may be desirable to customize this}
  \item{xlabel}{   the label for the x-axis. There is a default, but in many cases, it may be desirable to customize this}
  \item{legend_label}{   the label for the legend (color scheme). There is a default, but in many cases, it may be desirable to customize this}
  \item{image_type}{   the type of image to produce. This currently has only two options: "tiff" or anything else (the default). If this is not "tiff", a png image is produced. The "tiff" option was
added to produce publishable figures that adhere to the requirements of most scientific journals.}
  \item{colorscheme}{   the color scheme for the figure. This currently has only two options: "bw" or anythings else (the default). "bw" produces a grayscale
image, otherwise the figure will be in color}
}

\details{The generation of the color surface is based on the Kriging technique, which is typically used for interpolation of spatial data. The use for this particular
purpose is a bit experimental. The function uses the Krig function from the fields package. It also requires the spam package.}

\value{
 \item{pheno}{data frame containing all data needed for reproducing the plot: Year (during which the phenological event occurred - the year in which the
phenological season indicated by split_month ended), pheno (the date on which the phenological event occurred), Chill_Tmean (mean temperature during
the first relevant phase), Heat_Tmean (mean temperature during the second relevant phase) and Year_Tmean (mean annual temperature - not actually
used in the plot)}
 \item{ylabel}{character string used for labeling the y axis}
 \item{xlabel}{character string used for labeling the x axis}
}
\references{
Guo L, Dai J, Ranjitkar S, Xu J, Luedeling E. Response of chestnut phenology in China to climate variation and change. under review

Luedeling E, Guo L, Dai J, Leslie C, Blanke M. Differential responses of trees to temperature variation during the chilling and forcing phases. under review

the interpolation was done according to:

Furrer, R., Nychka, D. and Sain, S., 2012. Fields: Tools for spatial data. R package version 6.7.

Reference to the chillR package:

Luedeling E, Kunz A and Blanke M, in press. Identification of chilling and heat requirements of cherry trees - a statistical approach. International Journal of Biometeorology.
}

\author{Eike Luedeling}

\examples{

weather<-KA_weather
weather[,"Tmin"]<-interpolate_gaps(weather[,"Tmin"])$interp
weather[,"Tmax"]<-interpolate_gaps(weather[,"Tmax"])$interp
PLS_results_path<-paste(getwd(),"/PLS_output",sep="")

#the PLS function is also included in this example, because it is useful for identifying which
#phases are likely relevant for plant phase timing.

PLS_results<-PLS_pheno(
  weather_data_frame=KA_weather,
  split_month=6,   #last month in same year
  bio_data_frame=KA_bloom,
  PLS_results_path=PLS_results_path)

file_path<-paste(getwd(),"/",sep="")

mpt<-make_pheno_trend_plot(weather_data_frame = weather, split_month = 6,
     pheno = KA_bloom, use_Tmean = FALSE, Start_JDay_chill = 260, 
     End_JDay_chill = 64, Start_JDay_heat = 44, End_JDay_heat = 103,
     outpath = file_path, file_name = "pheno_trend_plot",
     plot_title = "Impacts of chilling and forcing temperatures on cherry phenology",
     ylabel = NA, xlabel = NA, legend_label = NA, image_type = "png", 
     colorscheme = "normal")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ phenology analysis }
