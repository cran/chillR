% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_if_equal.R
\name{test_if_equal}
\alias{test_if_equal}
\title{Test if all character vectors in a string are equal}
\usage{
test_if_equal(test_vector)
}
\arguments{
\item{test_vector}{vector of strings or numbers to be tested.}
}
\value{
TRUE if all elements of the vector are the same; FALSE otherwise.
}
\description{
Compares all elements of a vector of numbers or character strings and returns TRUE
if they are all the same, FALSE otherwise.
}
\examples{

  test_if_equal(c(1,3,1))
  test_if_equal(c("a","a","a"))
  test_if_equal(c("a","b","a"))                                            
 
}
\author{
Eike Luedeling
}
\keyword{utility}
