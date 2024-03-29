% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Compression.R
\name{Compression}
\alias{Compression}
\title{Compression by densification}
\usage{
Compression(z1, z2, rate)
}
\arguments{
\item{z1}{initial depth of the depth increment.}

\item{z2}{final depth of the depth increment in the same units as
\code{z1}; must be larger than \code{z1}.}

\item{rate}{constant rate of firn densification relative to the surface
density; in inverse units of \code{z1}.}
}
\value{
the relative compression of a depth increment when transported from
depth \code{z1} to depth \code{z2} in dimensionless units.
}
\description{
Amount of compression of a depth increment for linear densification of firn
with a constant rate.
}
\details{
The compression is calculated based on the assumption of conservation of
mass and assuming that densification leads only to a compression of a firn
parcel in vertical direction and not to any stretching in horizontal
direction.
}
\author{
Thomas Münch
}
