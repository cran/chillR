% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_chill_plot.R
\name{make_chill_plot}
\alias{make_chill_plot}
\title{Plot climate metrics over time}
\usage{
make_chill_plot(
  chill,
  model,
  start_year = NA,
  end_year = NA,
  metriclabel = NULL,
  yearlabel = "End_year",
  misstolerance = 10,
  image_type = NA,
  outpath = NA,
  filename = NA,
  fonttype = "serif",
  plotylim = NA,
  plottitle = NULL
)
}
\arguments{
\item{chill}{a chill object generated either with the chilling function or
with tempResponse. For this function to work properly, the chill object
should have been subjected to quality control (i.e. metrics should have been
calculated from weather records with a QC element. If you prepare weather
data with fix_weather, this should work.)}

\item{model}{the name of the column of the chill object that contains the
metric to be displayed}

\item{start_year}{the first year shown in the diagram. Default
to NA, which means the first date on record is used as start_year.}

\item{end_year}{the last year shown in the diagram.  Default
to NA, which means the last date on record is used as end_year.}

\item{metriclabel}{character string that can be used for labeling the y-axis
in the plot. If this is not specified, the function will use the model
argument.}

\item{yearlabel}{character string indicating the name of the column in the chill
object that is to be used for the time axis.}

\item{misstolerance}{Percentage of missing values that leads to exclusion of
an annual value from plotting.}

\item{image_type}{Character string indicating the file format that should be
output. Image files are only produced for the moment, if this is "png". All
other values, as well as the default NA lead to output as an R plot only.}

\item{outpath}{Path to the folder where the images should be saved. Should
include a trailing "/".}

\item{filename}{Suffix of the filenames for output graph files. These will
be amended by the name of the metric and by the file extension.}

\item{fonttype}{The type of font to be used for the figures. Can be 'serif'
(default) for a Times New Roman like font, 'sans' for an Arial type font or
'mono' for a typewriter type font.}

\item{plotylim}{numeric vector of length 2 indicating the extent of the y axis.
Defaults to NA, which means that y limits are determined automatically.}

\item{plottitle}{character string indicating the plot title. Defaults to NULL for
no title.}
}
\value{
only a side effect - plot of climate metric over time; bars are
color coded according to the number of missing values. Bars with numbers of
missing values above the misstolerance are not show and instead marked '*'
(to distinguish them from 0 counts)
}
\description{
This function generates a plot of a climate metric over multiple years,
including an indication of data quality, i.e. the share of missing values.
Output can be either an R plot or a .png image
}
\details{
Plots climatic metrics computed with chilling or tempResponse, indicating
the completeness of the temperature record by shades of gray.
}
\examples{

make_chill_plot(tempResponse(stack_hourly_temps(fix_weather(KA_weather[KA_weather$Year>2005,]))),
"Chill_Portions",start_year=1990,end_year=2010,metriclabel="Chill Portions")

 
}
\author{
Eike Luedeling
}
\keyword{utility}
