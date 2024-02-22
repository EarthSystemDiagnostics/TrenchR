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
#' at a profile position does not reach the maximum surface height. One
#' depth sample (T15-1-G-108) was lost in the measurement process and has been
#' linearly interpolated from the neighbouring values.
#'
#' For data source files and for how to create this dataset from source, see the
#' \code{data-raw} folder in the R package source. The data are also available
#' in a similar format from PANGAEA, see the source information below, but note
#' that sample positions with NA values due to the variable surface height are
#' not included in the PANGAEA dataset and the missing sample has not been
#' interpolated.
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
#' at a profile position does not reach the maximum surface height. A few
#' depth samples (T15-2-W-39 and 52; T15-2-X-48, 50-52, 74 and 77; T15-2-Y-36
#' and 42) were lost in the measurement process and have been linearly
#' interpolated from the neighbouring values.
#'
#' For data source files and for how to create this dataset from source, see the
#' \code{data-raw} folder in the R package source. The data are also available
#' in a similar format from PANGAEA, see the source information below, but note
#' that sample positions with NA values due to the variable surface height are
#' not included in the PANGAEA dataset and missing samples have not been
#' interpolated.
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
#' at a profile position does not reach the maximum surface height. One depth
#' sample (T13-1-03-12) was lost in the measurement process and has been
#' linearly interpolated from the neighbouring values. Large parts of profile
#' T13-1-01 could not be sampled during the field work due to instabilities in
#' the trench wall; these parts have not been interpolated and are instead
#' listed as NA values here.
#'
#' For data source files and for how to create this dataset from source, see the
#' \code{data-raw} folder in the R package source. The data are also available
#' in a similar format from PANGAEA, see the source information below, but note
#' that sample positions with NA values due to the variable surface height and
#' for the corrupted first trench profile (T13-1-01) are not included in the
#' PANGAEA dataset, and the other missing sample has not been interpolated.
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
#' that sample positions with NA values due to the variable surface height are
#' not included in the PANGAEA dataset.
#' @source \url{https://doi.pangaea.de/10.1594/PANGAEA.861674}
"t13.trench2"

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
