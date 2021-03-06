% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OpenDevice.R
\name{OpenDevice}
\alias{OpenDevice}
\title{Open quartz graphics device.}
\usage{
OpenDevice(file = NULL, path = getwd(), height = 6, width = 8, ...)
}
\arguments{
\item{file}{filename for saving the plot. Must include the desired file
extension for setting the \code{type} parameter to \code{quartz}. Setting to
\code{NULL} (the default) opens a \code{quartz} device for on-screen
plotting.}

\item{path}{path to the directory in which to save the plot. If the
specified directory does not exist, a warning is issued but nevertheless the
function attempts to create the directory. Default path is the current
working directory.}

\item{height}{height of the plotting area in inches. Default '6'.}

\item{width}{width of the plotting area in inches. Default '8'.}

\item{...}{further arguments to \code{quartz}, e.g., dpi resolution in case
of png output.}
}
\description{
This is a wrapper function to open a \code{quartz} graphics device for
on-screen plotting or saving to a file.
}
\seealso{
\code{quartz}
}
\author{
Thomas Münch
}
