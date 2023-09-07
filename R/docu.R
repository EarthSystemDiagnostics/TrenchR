#' Trench T15-1 stable water isotopologue data and meta information
#'
#' A dataset providing the stable water isotopologue data measured on Kohnen
#' Snow Trench T15-1 (oxygen isotopes, hydrogen isotopes, and deuterium excess)
#' together with meta information.
#'
#' @format A tibble with 1500 rows and 8 variables:
#' \describe{
#'   \item{profileName:}{character string of the name of the trench profile.}
#'   \item{profilePosition:}{the horizontal position of the trench profile in m
#'     relative to the first profile.}
#'   \item{surfaceHeight:}{the surface "height" in cm at each profile position
#'     (positive downwards) relative to the maximum observed surface height.}
#'   \item{sampleNumber:}{number enumerating the depth samples taken at each
#'     profile position; i.e. the profile name together with the sample number
#'     specify the exact position of the isotope sample within the entire
#'     trench.}
#'   \item{depth:}{depth in cm of the isotope sample on an absolute depth scale,
#'     i.e. relative to the maximum observed surface height along the profile
#'     positions.}
#'   \item{d18O:}{the sample's oxygen isotope composition in permil VSMOW/SLAP.}
#'   \item{dD:}{the sample's hydrogen isotope composition in permil VSMOW/SLAP.}
#'   \item{dxs:}{d-excess value in permil VSMOW/SLAP calculated from d18O and
#'     dD.}
#' }
#' @details
#' Note that isotope values close to the surface are NA when the surface height
#' at a profile position does not reach the maximum surface height. Also one
#' depth sample (T15-1-G-108) was lost in the measurement process and is listed
#' as an NA value here.
#'
#' For data source files and for how to create this dataset from source, see the
#' \code{data-raw} folder in the R package source. The data are also available
#' in a similar format from PANGAEA, see the source information below, but note
#' that sample positions with NA values, due to the variable surface height or
#' because of missing samples, are not included in the PANGAEA dataset.
#' @source \url{https://doi.pangaea.de/10.1594/PANGAEA.876637}
"t15.trench1"

#' Trench T15-2 stable water isotopologue data and meta information
#'
#' A dataset providing the stable water isotopologue data measured on Kohnen
#' Snow Trench T15-2 (oxygen isotopes, hydrogen isotopes, and deuterium excess)
#' together with meta information.
#'
#' @format A tibble with 1375 rows and 8 variables:
#' \describe{
#'   \item{profileName:}{character string of the name of the trench profile.}
#'   \item{profilePosition:}{the horizontal position of the trench profile in m
#'     relative to the first profile.}
#'   \item{surfaceHeight:}{the surface "height" in cm at each profile position
#'     (positive downwards) relative to the maximum observed surface height.}
#'   \item{sampleNumber:}{number enumerating the depth samples taken at each
#'     profile position; i.e. the profile name together with the sample number
#'     specify the exact position of the isotope sample within the entire
#'     trench.}
#'   \item{depth:}{depth in cm of the isotope sample on an absolute depth scale,
#'     i.e. relative to the maximum observed surface height along the profile
#'     positions.}
#'   \item{d18O:}{the sample's oxygen isotope composition in permil VSMOW/SLAP.}
#'   \item{dD:}{the sample's hydrogen isotope composition in permil VSMOW/SLAP.}
#'   \item{dxs:}{d-excess value in permil VSMOW/SLAP calculated from d18O and
#'     dD.}
#' }
#' @details
#' Note that isotope values close to the surface are NA when the surface height
#' at a profile position does not reach the maximum surface height. Also a few
#' depth samples (T15-2-W-39 and 52; T15-2-X-48, 50-52, 74 and 77; T15-2-Y-36
#' and 42) were lost in the measurement process and are listed as NA values
#' here.
#'
#' For data source files and for how to create this dataset from source, see the
#' \code{data-raw} folder in the R package source. The data are also available
#' in a similar format from PANGAEA, see the source information below, but note
#' that sample positions with NA values, due to the variable surface height or
#' because of missing samples, are not included in the PANGAEA dataset.
#' @source \url{https://doi.pangaea.de/10.1594/PANGAEA.876638}
"t15.trench2"

#' Trench T13-1 stable water isotopologue data and meta information
#'
#' A dataset providing the stable water isotopologue data measured on Kohnen
#' Snow Trench T13-1 (oxygen isotopes, hydrogen isotopes, and deuterium excess)
#' together with meta information.
#'
#' @format A tibble with 1444 rows and 8 variables:
#' \describe{
#'   \item{profileName:}{character string of the name of the trench profile.}
#'   \item{profilePosition:}{the horizontal position of the trench profile in m
#'     relative to the predefined trench starting point.}
#'   \item{surfaceHeight:}{the surface "height" in cm at each profile position
#'     (positive downwards) relative to the maximum observed surface height.}
#'   \item{sampleNumber:}{number enumerating the depth samples taken at each
#'     profile position; i.e. the profile name together with the sample number
#'     specify the exact position of the isotope sample within the entire
#'     trench.}
#'   \item{depth:}{depth in cm of the isotope sample on an absolute depth scale,
#'     i.e. relative to the maximum observed surface height along the profile
#'     positions.}
#'   \item{d18O:}{the sample's oxygen isotope composition in permil VSMOW/SLAP.}
#'   \item{dD:}{the sample's hydrogen isotope composition in permil VSMOW/SLAP.}
#'   \item{dxs:}{d-excess value in permil VSMOW/SLAP calculated from d18O and
#'     dD.}
#' }
#' @details
#' Note that isotope values close to the surface are NA when the surface height
#' at a profile position does not reach the maximum surface height. Also one
#' depth sample of profile 3 (T13-1-03-12) was lost in the measurement process,
#' and large parts of profile T13-1-01 could not be sampled during the field
#' work due to instabilities in the trench wall; these are also listed as NA
#' values here.
#'
#' For data source files and for how to create this dataset from source, see the
#' \code{data-raw} folder in the R package source. The data are also available
#' in a similar format from PANGAEA, see the source information below, but note
#' that sample positions with NA values, due to the variable surface height or
#' because of missing samples, are not included in the PANGAEA dataset.
#' @source \url{https://doi.pangaea.de/10.1594/PANGAEA.861673}
"t13.trench1"

#' Trench T13-2 stable water isotopologue data and meta information
#'
#' A dataset providing the stable water isotopologue data measured on Kohnen
#' Snow Trench T13-2 (oxygen isotopes, hydrogen isotopes, and deuterium excess)
#' together with meta information.
#'
#' @format A tibble with 152 rows and 8 variables:
#' \describe{
#'   \item{profileName:}{character string of the name of the trench profile.}
#'   \item{profilePosition:}{the horizontal position of the trench profile in m
#'     relative to the predefined trench starting point.}
#'   \item{surfaceHeight:}{the surface "height" in cm at each profile position
#'     (positive downwards) relative to the maximum observed surface height.}
#'   \item{sampleNumber:}{number enumerating the depth samples taken at each
#'     profile position; i.e. the profile name together with the sample number
#'     specify the exact position of the isotope sample within the entire
#'     trench.}
#'   \item{depth:}{depth in cm of the isotope sample on an absolute depth scale,
#'     i.e. relative to the maximum observed surface height along the profile
#'     positions.}
#'   \item{d18O:}{the sample's oxygen isotope composition in permil VSMOW/SLAP.}
#'   \item{dD:}{the sample's hydrogen isotope composition in permil VSMOW/SLAP.}
#'   \item{dxs:}{d-excess value in permil VSMOW/SLAP calculated from d18O and
#'     dD.}
#' }
#' @details
#' Note that isotope values close to the surface are NA when the surface height
#' at a profile position does not reach the maximum surface height.
#'
#' For data source files and for how to create this dataset from source, see the
#' \code{data-raw} folder in the R package source. The data are also available
#' in a similar format from PANGAEA, see the source information below, but note
#' that sample positions with NA values, due to the variable surface height, are
#' not included in the PANGAEA dataset.
#' @source \url{https://doi.pangaea.de/10.1594/PANGAEA.861674}
"t13.trench2"

#' AWS9 2m air temperature
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

#' Data for TC2017 Figure 5
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
