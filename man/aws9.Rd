% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docu.R
\docType{data}
\name{aws9}
\alias{aws9}
\title{AWS9 2m air temperature}
\format{
A list with two components:
\describe{
  \item{monthly:}{A data frame with 197 observations of 3 variables:
  \describe{
    \item{month:}{Character vector with the month in the format YYYY-mm.}
    \item{dates.posix:}{A vector of POSIXct dates corresponding to the mean
    monthly temperatures.}
    \item{t2m:}{Numeric vector of mean monthly 2 m air temperatures in
    degrees Celsius.}}}
  \item{annual:}{A data frame with 18 observations of 3 variables:
  \describe{
    \item{year:}{Character vector with the year in the format YYYY.}
    \item{dates.posix:}{A vector of POSIXct dates corresponding to the mean
    annual temperatures.}
    \item{t2m:}{Numeric vector of annual mean 2 m air temperatures in degrees
    Celsius.}}}
}
}
\source{
The full AWS9 dataset is available upon request from Carleen Tijm-Reijmer
(c.h.tijm-reijmer@uu.nl).
}
\usage{
aws9
}
\description{
A dataset of the measurements of 2 m air temperature from the automatic
weather station AWS9 at Kohnen Station. The data are provided at monthly as
well as annual resolution for the period December 1998 (start of operation of
AWS9) until April 2014.
}
\references{
Reijmer, C. H. and van den Broeke, M. R.: Temporal and spatial variability of
    the surface mass balance in Dronning Maud Land, Antarctica, as derived
    from automatic weather stations, J. Glac., 49(167), 512-520,
    doi:10.3189/172756503781830494, 2003.
}
\keyword{datasets}
