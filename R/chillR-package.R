

#' Weather stations in California
#' 
#' This is a list of weather stations in California that are contained in the
#' UC IPM database. This can also be generated with
#' make_california_UCIPM_station_list(), but this takes quite a while. So this
#' dataset is supposed to be a shortcut to this.
#' 
#' 
#' @name california_stations
#' @docType data
#' @format a data.frame containing stations from the California UC IPM database
#' (), with the columns: "Name", "Code", "Interval", "Lat", "Long",
#' "Elev".  \describe{ \item{list("Name")}{name of the weather station}
#' \item{list("Code")}{ code of the weather station, indicating the name and
#' the database it comes from} \item{list("Interval")}{ period of available
#' data (as character string)} \item{list("Lat")}{ latitude of the station}
#' \item{list("Long")}{ longitude of the station} \item{list("Elev")}{
#' elevation of the station} }
#' @source UC IPM website: http://www.ipm.ucdavis.edu/WEATHER/index.html
#' @keywords datasets
#' @importFrom pls plsr explvar crossval
#' @importFrom Kendall Kendall
#' @importFrom fields Krig surface tim.colors image.plot predictSurface
#' @importFrom readxl read_excel
#' @importFrom XML htmlParse getNodeSet xmlToDataFrame getChildrenStrings
#' @importFrom httr POST content
#' @importFrom grDevices bmp colorRampPalette dev.off png tiff gray.colors rainbow rgb
#' @importFrom graphics arrows axis box grconvertX lines mtext par plot points barplot contour image layout rect text
#' @importFrom stats aggregate coef sd median
#' @importFrom utils stack write.csv download.file read.csv read.fwf read.table unzip
#' @importFrom Rcpp evalCpp
#' @examples
#' 
#' data(california_stations)
#' 
NULL





#' \if{html}{\figure{chillR.png}{options: width='25\%' alt='chillR logo'}}
#' chillR: statistical methods for phenology analysis in temperate fruit trees
#' 
#' @description 
#' 
#' \if{latex}{\figure{chillR.png}{options: width=1in}}
#' 
#' \code{chillR} contains functions for processing temperature records into chilling (Chilling Hours, Utah Chill Units
#' and Chill Portions) and heat units (Growing Degree Hours). Regarding chilling metrics, Chill Portions are often
#' considered the most promising, but they are difficult to calculate. This package makes it easy.
#' \code{chillR} also contains procedures for conducting a Partial Least Square regression analysis relating
#' phenological dates (e.g. bloom dates) to either mean temperatures or mean chill and heat accumulation rates,
#' based on long-term weather and phenology records (Luedeling and Gassner 2012).
#' As of version 0.65, it also includes functions for generating weather scenarios with a weather generator,
#' for conducting climate change analyses for temperature-based climatic metrics and for plotting results from such
#' analyses. Since version 0.70, \code{chillR} contains a function for interpolating hourly temperature records.
#' As of version 0.72.6, the package contains functions to design elegant plots using the
#' \href{https://CRAN.R-project.org/package=ggplot2}{ggplot2} package.
#' 
#' 
#' @keywords internal
"_PACKAGE"
#' 
#' @references
#' Luedeling E and Gassner A, 2012. Partial Least Squares Regression for
#' analyzing walnut phenology in California. Agricultural and Forest
#' Meteorology 158, 43-52. \doi{10.1016/j.agrformet.2011.10.020}
#' 
#' @author \tabular{ll}{
#' \strong{Prof. Dr. Eike Luedeling} (creator)\cr
#' \email{eike@@eikeluedeling.com}\cr   
#' \href{https://orcid.org/0000-0002-7316-3631}{ORCID}\cr
#' \href{https://github.com/eikeluedeling}{eikeluedeling on GitHub}\cr   
#'  \cr
#' \strong{Lars Caspersen} (contributor)\cr
#' \email{lcaspers@@uni-bonn.de}\cr
#' \href{https://orcid.org/0009-0000-3057-7327}{ORCID}\cr
#' \href{https://github.com/larscaspersen}{larscaspersen on GitHub}\cr
#'  \cr
#' \strong{Dr. Eduardo Fernández} (contributor)\cr
#' \email{eduardo.fernandez.c@@pucv.cl}\cr
#' \href{https://orcid.org/0000-0002-6949-9685}{ORCID}\cr
#' \href{https://github.com/EduardoFernandezC}{EduardoFernandezC on GitHub}
#' }
NULL




#' Cherry bloom data for Klein-Altendorf, Germany
#' 
#' Bloom data of sweet cherry var. 'Schneiders spaete Knorpelkirsche' recorded
#' at Klein-Altendorf, Germany, the experimental station of the University of
#' Bonn
#' 
#' 
#' @name KA_bloom
#' @docType data
#' @format A data frame with the following 2 variables.  \describe{
#' \item{Year}{a numeric vector, indicating the observation year}
#' \item{pheno}{ a vector that, when coerced by as.numeric, contains
#' bloom data in Julian dates (day of the year)} }
#' @references Luedeling E, Kunz A and Blanke M, 2013. Identification of
#' chilling and heat requirements of cherry trees - a statistical approach.
#' International Journal of Biometeorology 57,679-689.
#' @source data were collected by Achim Kunz and Michael Blanke, University of
#' Bonn
#' @keywords datasets
#' @examples
#' 
#' data(KA_bloom)
#' 
NULL





#' Weather data for Klein-Altendorf, Germany
#' 
#' Daily temperature data from Klein-Altendorf, Germany, for use in combination
#' with the example phenology dataset KA_bloom.
#' 
#' 
#' @name KA_weather
#' @docType data
#' @format A data frame with observations on the following 5 variables.
#' \describe{ \item{Year}{a numeric vector - the observation year}
#' \item{Month}{a numeric vector - the observation month}
#' \item{Day}{a numeric vector - the observation day}
#' \item{Tmax}{a numeric vector - daily maximum temperature}
#' \item{Tmin}{a numeric vector - daily minimum temperature} }
#' @references Luedeling E, Kunz A and Blanke M, 2013. Identification of
#' chilling and heat requirements of cherry trees - a statistical approach.
#' International Journal of Biometeorology 57,679-689.
#' @source data were collected by Achim Kunz and Michael Blanke, University of
#' Bonn
#' @keywords datasets
#' @examples
#' 
#' data(KA_weather)
#' 
NULL



#' Hourly temperature data sample
#' 
#' Hourly temperature data recorded in a walnut orchard near the city of Winters,
#' California, USA for 3rd March to 11th November 2008. The dataset contains the
#' full record of recorded temperatures, as well as an additional dataset, in
#' which 500 data gaps of different length were introduced.
#' 
#' 
#' @name Winters_hours_gaps
#' @docType data
#' @format A data frame with observations on the following 5 variables.
#' \describe{ \item{Year}{a numeric vector - the observation year}
#' \item{Month}{a numeric vector - the observation month}
#' \item{Day}{a numeric vector - the observation day}
#' \item{Hour}{a numeric vector - the observation day}
#' \item{Temp_gaps}{a numeric vector - daily maximum temperature}
#' \item{Temp}{a numeric vector - daily minimum temperature} }
#' @source data were collected by Eike Luedeling, at that time at the 
#' University of California Davis (now University of Bonn) in a walnut orchard
#' near Winters, California
#' @keywords datasets
#' @examples
#' 
#' data(Winters_hours_gaps)
#' 
NULL



