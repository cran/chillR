% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_differences_between_characters.R
\name{extract_differences_between_characters}
\alias{extract_differences_between_characters}
\title{Identify shared leading or trailing character strings}
\usage{
extract_differences_between_characters(strings)
}
\arguments{
\item{strings}{vector of character strings for elements to be extracted from.}
}
\value{
vector of strings similar to the input vector but without shared leading and trailing characters.
}
\description{
For a vector of character strings, identify elements between shared leading and/or trailing substrings,
e.g. for a vector such as c("XXX01YYY",XXX02YYY") extract the numbers.
}
\examples{

  extract_differences_between_characters(c("Temp_01","Temp_02","Temp_03"))
  extract_differences_between_characters(c("Temp_01_Tmin","Temp_02_Tmin","Temp_03_Tmin"))
  extract_differences_between_characters(c("a","b"))                                           
 
}
\author{
Eike Luedeling
}
\keyword{utility}
