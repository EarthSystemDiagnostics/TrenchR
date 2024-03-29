% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ModifyRecord.R
\name{ModifyRecord}
\alias{ModifyRecord}
\title{Model temporal change of firn profile}
\usage{
ModifyRecord(
  rec.in,
  res,
  depth.hires,
  depth.lores = NULL,
  SIGMA = NULL,
  STRETCH = NULL,
  ADV = NULL
)
}
\arguments{
\item{rec.in}{numeric vector with the firn proxy (isotope) record to be
modified.}

\item{res}{the (high) depth resolution of the firn record on which the
the modification processes are calculated.}

\item{depth.hires}{numeric vector with the (high-resolution) depth scale on
which \code{rec.in} is recorded; must be of the same length as
\code{rec.in} and in units of \code{res}.}

\item{depth.lores}{numeric vector with a lower resolution depth scale of
\code{rec.in}; in the units of \code{res}. This is optional: A sufficiently
high depth resolution is needed for proper simulation of the modification
processes; if \code{rec.in} and \code{depth.hires} provide an interpolated
version of the original record to achieve such a higher resolution,
\code{depth.lores} can be used to provide the original lower resolution
depth scale, and the modified version of the record is then returned on both
the interpolated (high) and the original (low) depth resolution.}

\item{SIGMA}{diffusion length value to smooth the record; must be in the
same units as \code{res}. If \code{NULL}, no smoothing by diffusion is
modelled.}

\item{STRETCH}{value of the amount of compression due to densification of
the original depth scale of \code{record} over time; must be in the same
units as \code{res}. If \code{NULL}, no compression by densification is
modelled.}

\item{ADV}{value of downward advection of the \code{record} over time; must
be in the same units as \code{res}. If \code{NULL}, no downward advection is
modelled.}
}
\value{
A list with one or two elements:
\describe{
  \item{HiRes:}{the modified record on the high depth resolution
    \code{res};}
  \item{LoRes:}{if \code{depth.lores} is not \code{NULL}, the modified
    record on this lower depth resolution.}
}
}
\description{
Modify a proxy firn profile (e.g., an isotope record) to simulate changes
over time given values of downward advection, compression by densification,
and diffusional smoothing.
}
\seealso{
\code{\link{DiffuseRecord}}; \code{\link{CompressRecord}}
}
\author{
Thomas Münch
}
