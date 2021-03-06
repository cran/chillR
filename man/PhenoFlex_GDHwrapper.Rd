% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phenologyFitter.R
\name{PhenoFlex_GDHwrapper}
\alias{PhenoFlex_GDHwrapper}
\title{PhenoFlex_GDHwrapper}
\usage{
PhenoFlex_GDHwrapper(x, par)
}
\arguments{
\item{x}{data.frame with at least columns `Temp` and `JDay`}

\item{par}{numeric vector of length 11 with the `PhenoFlex` fit
parameters in the following order: 1. yc, 2. zc, 3. s1, 4. Tu, 5. E0,
6. E1, 7, A0, 8. A1, 9. Tf, 10. Tc, 11. Tb and 12. slope. For details see
\link{PhenoFlex}}
}
\value{
A single numeric value with the JDay prediction for the
temperaturs in `x$Temp` and the \link{PhenoFlex} parameters
in `par`.
}
\description{
PhenoFlex wrapper function for the `phenologyFitter` function using the GDH heat accumulation model
}
