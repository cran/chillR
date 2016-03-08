\name{runn_mean}
\alias{runn_mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Running mean of a vector
}
\description{
Function to calculate the running mean of a numeric vector}
\usage{
runn_mean(vec,runn_mean)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{numeric vector}
  \item{runn_mean}{number of vector elements to use for calculating the running mean}
}
\value{
numeric vector containing the running mean}
\author{Eike Luedeling}
\examples{

plot(runn_mean(rnorm(1000),150))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
