#' Trench T15-1 Stable Water Isotopologue Data and Meta Information.
#'
#' A dataset providing the stable water isotopologue data measured on Kohnen
#' Snow Trench T15-1 (oxygen isotopes, hydrogen isotopes, and deuterium excess)
#' together with meta information.
#'
#' @format A list with three components:
#' \describe{
#'   \item{data:}{A 3 x 125 x 11 array with the isotopologue data: the first
#'     index refers to the isotopic species (1 = d18O, 2 = d2H, 3 =
#'     d-excess), the second index to the sample depth, the third index to the
#'     profile position.}
#'   \item{depth:}{Numeric vector of length 125 providing the sample depths in
#'     [cm] on the absolute depth scale, thus 0 cm referring to the maximum
#'     surface height of the trench.}
#'   \item{meta:}{A data frame with 12 observations of 3 variables:
#'   \describe{
#'     \item{profileName:}{Character vector with the profile names.}
#'     \item{profilePos:}{Numeric vector of the horizontal profile positions in
#'       [m] along the trench.}
#'     \item{profileSurfaceHeight:}{Numeric vector of the surface heights in
#'     [cm] at the profile positions (positive downwards) relative to the
#'     maximum surface height of the trench.}}}
#' }
#' @details
#' Note that the meta data frame contains one additional profile at position
#' 26.5 m which defines the maximum surface height, but for which, however, is
#' no isotope profile available.\cr
#' To create this dataset, the data available from PANGAEA has to be converted
#' to the format described here.
#' @source \url{https://doi.pangaea.de/10.1594/PANGAEA.876639}
"t15.trench1"

#' Trench T15-2 Stable Water Isotopologue Data and Meta Information.
#'
#' A dataset providing the stable water isotopologue data measured on Kohnen
#' Snow Trench T15-2 (oxygen isotopes, hydrogen isotopes, and deuterium excess)
#' together with meta information.
#'
#' @format A list with three components:
#' \describe{
#'   \item{data:}{A 3 x 125 x 11 array with the isotopologue data: the first
#'     index refers to the isotopic species (1 = d18O, 2 = d2H, 3 =
#'     d-excess), the second index to the sample depth, the third index to the
#'     profile position.}
#'   \item{depth:}{Numeric vector of length 125 providing the sample depths in
#'     [cm] on the absolute depth scale, thus 0 cm referring to the maximum
#'     surface height of the trench.}
#'   \item{meta:}{A data frame with 11 observations of 3 variables:
#'   \describe{
#'     \item{profileName:}{Character vector with the profile names.}
#'     \item{profilePos:}{Numeric vector of the horizontal profile positions in
#'       [m] along the trench.}
#'     \item{profileSurfaceHeight:}{Numeric vector of the surface heights in
#'     [cm] at the profile positions (positive downwards) relative to the
#'     maximum surface height of the trench.}}}
#' }
#' @details
#' To create this dataset, the data available from PANGAEA has to be converted
#' to the format described here.
#' @source \url{https://doi.pangaea.de/10.1594/PANGAEA.876639}
"t15.trench2"

#' Trench T13-1 Stable Water Isotopologue Data and Meta Information.
#'
#' A dataset providing the stable water isotopologue data measured on Kohnen
#' Snow Trench T13-1 (oxygen isotopes, hydrogen isotopes, and deuterium excess)
#' together with meta information.
#'
#' @format A list with three components:
#' \describe{
#'   \item{data:}{A 3 x 38 x 38 array with the isotopologue data: the first
#'     index refers to the isotopic species (1 = d18O, 2 = d2H, 3 =
#'     d-excess), the second index to the sample depth, the third index to the
#'     profile position.}
#'   \item{depth:}{Numeric vector of length 38 providing the sample depths in
#'     [cm] on the absolute depth scale, thus 0 cm referring to the maximum
#'     surface height of the trench.}
#'   \item{meta:}{A data frame with 38 observations of 3 variables:
#'   \describe{
#'     \item{profileName:}{Character vector with the profile names.}
#'     \item{profilePos:}{Numeric vector of the horizontal profile positions in
#'       [cm] along the trench.}
#'     \item{profileSurfaceHeight:}{Numeric vector of the surface heights in
#'     [cm] at the profile positions (positive downwards) relative to the
#'     maximum surface height of the trench.}}}
#' }
#' @details
#' To create this dataset, the data available from PANGAEA has to be converted
#' to the format described here.
#' @source \url{https://doi.pangaea.de/10.1594/PANGAEA.861675}
"t13.trench1"

#' Trench T13-2 Stable Water Isotopologue Data and Meta Information.
#'
#' A dataset providing the stable water isotopologue data measured on Kohnen
#' Snow Trench T13-2 (oxygen isotopes, hydrogen isotopes, and deuterium excess)
#' together with meta information.
#'
#' @format A list with three components:
#' \describe{
#'   \item{data:}{A 3 x 38 x 4 array with the isotopologue data: the first
#'     index refers to the isotopic species (1 = d18O, 2 = d2H, 3 =
#'     d-excess), the second index to the sample depth, the third index to the
#'     profile position.}
#'   \item{depth:}{Numeric vector of length 38 providing the sample depths in
#'     [cm] on the absolute depth scale, thus 0 cm referring to the maximum
#'     surface height of the trench.}
#'   \item{meta:}{A data frame with 4 observations of 3 variables:
#'   \describe{
#'     \item{profileName:}{Character vector with the profile names.}
#'     \item{profilePos:}{Numeric vector of the horizontal profile positions in
#'       [cm] along the trench.}
#'     \item{profileSurfaceHeight:}{Numeric vector of the surface heights in
#'     [cm] at the profile positions (positive downwards) relative to the
#'     maximum surface height of the trench.}}}
#' }
#' @details
#' To create this dataset, the data available from PANGAEA has to be converted
#' to the format described here.
#' @source \url{https://doi.pangaea.de/10.1594/PANGAEA.861675}
"t13.trench2"

#' AWS9 2m air temperature.
#'
#' A dataset of the measurements of 2 m air temperature from the automatic
#' weather station AWS9 at Kohnen Station. The data are provided at monthly as
#' well as annual resolution for the period December 1998 (start of operation of
#' AWS9) until April 2014.
#'
#' @format A list with two components:
#' \describe{
#'   \item{monthly:}{A data frame with 197 observations of 3 variables:
#'   \describe{
#'     \item{month:}{Character vector with the month in the format YYYY-mm.}
#'     \item{dates.posix:}{A vector of POSIXct dates corresponding to the mean
#'     monthly temperatures.}
#'     \item{t2m:}{Numeric vector of mean monthly 2 m air temperatures in
#'     degrees Celsius.}}}
#'   \item{annual:}{A data frame with 18 observations of 3 variables:
#'   \describe{
#'     \item{year:}{Character vector with the year in the format YYYY.}
#'     \item{dates.posix:}{A vector of POSIXct dates corresponding to the mean
#'     annual temperatures.}
#'     \item{t2m:}{Numeric vector of annual mean 2 m air temperatures in degrees
#'     Celsius.}}}
#' }
#' @references
#' Reijmer, C. H. and van den Broeke, M. R.: Temporal and spatial variability of
#'     the surface mass balance in Dronning Maud Land, Antarctica, as derived
#'     from automatic weather stations, J. Glac., 49(167), 512-520,
#'     doi:10.3189/172756503781830494, 2003.
#' @source
#' The full AWS9 dataset is available upon request from Carleen Tijm-Reijmer
#' (c.h.tijm-reijmer@uu.nl).
"aws9"

#' Data for TC2017 Figure 5.
#'
#' This file contains the data displayed in Figure 5 of the TC17 paper and is
#' the result of a call to \code{\link{LoopParamSpace}} for the parameter ranges
#' of downward advection, differential diffusion and compression from
#' densification as described in the paper.
#'
#' @format A list with nine components:
#' \describe{
#'   \item{adv:}{Numeric vector with the range of advection values in cm for
#'     which the RMSD is computed.}
#'   \item{sigma:}{Numeric vector with the range of differential diffusion
#'     lengths in cm for which the RMSD is computed.}
#'   \item{densf:}{Numeric vector with the range of compression values in cm for
#'     which the RMSD is computed.}
#'   \item{adv.opt:}{The optimal advection value in cm.}
#'   \item{sigma.opt:}{The optimal differential diffusion length in cm.}
#'   \item{densf.opt:}{The optimal compression value in cm.}
#'   \item{adv.opt.arr:}{An array of dimension \code{length(sigma)} x
#'     \code{length(densf)} which contains the optimal advection values at
#'     which, for fixed compression and differential diffusion, the RMSD between
#'     the mean T15 and the modified mean T13 profile is minimal.}
#'   \item{RMSD:}{An array of dimension \code{length(adv)} x
#'     \code{length(sigma)} x \code{length(densf)} which contains the RMSD value
#'     between T15 and the modified T13 mean profile for every combination of
#'     downward advection, differential diffusion and compression.}
#'   \item{RMSD.opt:}{The array \code{RMSD} projected onto the optimal
#'     downward-advection value.}
#' }
#' @inherit Muench2017 references
#' @source
#' A call to \code{LoopParamSpace}.
#' @seealso \code{\link{LoopParamSpace}}
"ParamSpace"

#' Citation for Münch et al. (2016)
#' @references
#' Münch, T., et al., Regional climate signal vs. local noise: a two-dimensional
#' view of water isotopes in Antarctic firn at Kohnen Station, Dronning Maud
#' Land, Clim. Past, 12(7), 1565-1581, doi:10.5194/cp-12-1565-2016, 2016.
#' @name Muench2016
NULL

#' Citation for Münch et al. (2017)
#' @references
#' Münch, T., et al., Constraints on post-depositional isotope modifications
#' in East Antarctic firn from analysing temporal changes of isotope profiles,
#' The Cryosphere, 11(5), 2175-2188, doi:10.5194/tc-11-2175-2017, 2017.
#' @name Muench2017
NULL
