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

