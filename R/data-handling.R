#' Horizontal trench profile positions
#'
#' Easily retrieve the horizontal profile positions of a trench data set.
#'
#' @param data a tibble with a trench data set with horizontal profile positions
#'   listed in the column \code{profilePosition}.
#' @return numeric vector of the profile positions.
#' @author Thomas Münch
#' @examples
#' getX(t13.trench1)
#' @export
#'
getX <- function(data) {

  if (!"profilePosition" %in% colnames(data)) {
    stop("No column 'profilePosition' found in trench data.", call. = FALSE)
  }

  unique(data[["profilePosition"]])

}

#' Common vertical trench scale
#'
#' Easily retrieve the absolute vertical scale that is common to all profiles of
#' the supplied trench data set. Most commonly this is a depth scale.
#'
#' @param data a trench data set following the default structure used in the
#'   package.
#' @param var the name of the vertical scale as a character string; defaults to
#'   \code{"depth"}.
#' @return numeric vector of the vertical scale.
#' @author Thomas Münch
#' @examples
#' getZ(t13.trench1)
#' @export
#'
getZ <- function(data, var = "depth") {

  if (!var %in% colnames(data)) {
    stop("Unknown column name for vertical scale.")
  }

  unique(data[[var]])

}

#' Trench surface profile
#'
#' Easily retrieve the profile positions and the corresponding surface heights
#' for a supplied trench data set.
#'
#' @inheritParams getZ
#' @return a tibble of two variables: the horizontal profile position in the
#'   first column and the surface heights at each position in the second column.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @author Thomas Münch
#' @examples
#' getSurfaceProfile(t13.trench2)
#' @export
#'
getSurfaceProfile <- function(data) {

  if (!all(c("profilePosition", "surfaceHeight") %in% colnames(data))) {
    stop("'profilePosition' and 'surfaceHeight' columns both needed ",
         "to extract surface profile.", call. = FALSE)
  }

  data %>%
    dplyr::select(position = "profilePosition", height = "surfaceHeight") %>%
    dplyr::group_by(.data$position) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup()

}

#' 2D trench record
#'
#' Obtain a two-dimensional (vertical x horizontal) representation of a certain
#' variable for a given trench data set.
#'
#' @param var character string with the name of the requested variable; must
#'   match one of the data column variables in \code{data}, or be one of the
#'   following:
#'   \describe{
#'   \item{\code{"dxs"}:}{this will return d-excess data as calculated from
#'     \code{"dD - 8 * d18O"} given in the trench data, but only if a column
#'   \code{"dxs"} is not already present.}
#'   \item{\code{"nssSulfate"}:}{this will return non-sea-salt sulfate data as
#'     calculated from \code{"Sulfate - 0.252 * Sodium"} given in the trench
#'     data, but only if a column \code{"nssSulfate"} is not already present.}
#'   }
#' @param simplify logical; for \code{FALSE} (the default) the result is
#'   returned as a tibble, else it is simplified to a matrix.
#' @inheritParams getZ
#' @return a tibble or matrix of the requested trench data, with rows indexing
#'   the vertical scale (most commonly depth) and columns the horizontal profile
#'   position.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @author Thomas Münch
#' @examples
#'
#' # Per default, oxygen isotope data is returned:
#' make2D(t13.trench2)
#'
#' # Request another variable:
#' make2D(t13.trench2, var = "dD")
#' # Obtain the same result as a simple matrix:
#' make2D(t13.trench2, var = "dD", simplify = TRUE)
#'
#' @export
#'
make2D <- function(data, var = "d18O", simplify = FALSE) {

  is.trench(data, full = FALSE)

  if (var == "dxs" & (!"dxs" %in% colnames(data))) {
    if ((!"d18O" %in% colnames(data)) | (!"dD" %in% colnames(data))) {
      stop("Column variable(s) missing to calculate 'dexcess'.", call. = FALSE)
    }
    data <- data %>%
      dplyr::mutate(dxs = .data$dD - 8 * .data$d18O)
  }
  if (var == "nssSulfate" & (!"nssSulfate" %in% colnames(data))) {
    if ((!"Sulfate" %in% colnames(data)) | (!"Sodium" %in% colnames(data))) {
      stop("Column variable(s) missing to calculate 'nssSulfate'.",
           call. = FALSE)
    }
    data <- data %>%
      dplyr::mutate(nssSulfate = .data$Sulfate - 0.252 * .data$Sodium)
  }

  if (!var %in% colnames(data)) {
    stop("Unknown column variable selected.", call. = FALSE)
  }

  result <- data %>%
    dplyr::select("profileName", "sampleNumber", dplyr::all_of(var)) %>%
    tidyr::pivot_wider(names_from = "profileName",
                       values_from = dplyr::all_of(var)) %>%
    dplyr::select(-"sampleNumber")

  if (simplify) {
    as.matrix(result)
  } else {
    result
  }

}

#' Trench mean profile
#'
#' Calculate the mean profile for a certain trench variable by averaging across
#' the horizontal dimension of the trench data.
#'
#' @inheritParams make2D
#' @param vscale a character string giving the name of the vertical scale
#'   variable used in the trench data; defaults to \code{"depth"}; see also
#'   \code{\link{getZ}}.
#' @param na.rm a logical evaluating to \code{TRUE} or \code{FALSE} indicating
#'   whether \code{NA} values should be stripped before the computation of the
#'   mean profile proceeds.
#' @param df a logical; for \code{TRUE} (the default) the mean profile is
#'   returned as a tibble where the first column includes the vertical scale
#'   of the trench data given in the variable \code{vscale}, else the mean
#'   profile is returned as a simple numeric vector.
#' @importFrom dplyr %>%
#' @return a tibble or numeric vector.
#' @author Thomas Münch
#' @examples
#'
#' # incomplete surface region is stripped
#' makeMean(t13.trench2)
#' # mean values are also calculated for incomplete surface region
#' makeMean(t13.trench2, na.rm = TRUE)
#'
#' # get dD data as a simple vector
#' makeMean(t13.trench2, var = "dD", df = FALSE)
#'
#' @export
#'
makeMean <- function(data, var = "d18O", vscale = "depth",
                     na.rm = FALSE, df = TRUE) {

  if (df) {
    tibble::tibble(
      getZ(data, var = vscale), rowMeans(make2D(data, var), na.rm = na.rm)) %>%
      stats::setNames(c(vscale, var))
  } else {
    rowMeans(make2D(data, var), na.rm = na.rm)
  }

}

#' Check if data is a valid trench data set
#'
#' @param data a data frame.
#' @param full logical; if \code{TRUE} check for the complete set of necessary
#'   data frame columns, else check only that \code{profileName} and
#'   \code{surfaceNumber} columns exist.
#' @return an error message is created if \code{data} is no valid trench data.
#' @author Thomas Münch
#' @noRd
#'
is.trench <- function(data, full = TRUE) {

  trenchCols <- c("profileName", "sampleNumber")
  if (full) trenchCols <- c(trenchCols, "profilePosition", "surfaceHeight")

  iMissing <- which(is.na(match(trenchCols, colnames(data))))

  if (length(iMissing)) {

    stop("Incomplete trench data set: columns ",
         paste(trenchCols[iMissing], collapse = ", "),
         " missing.", call. = FALSE)
  }

}

#' Bottom of varying trench surface region
#'
#' Extract the value of the bin in vertical dimension ("depth") for which a
#' complete horizontal data set across all trench profile positions is
#' available.
#'
#' This function assumes that NA values at the start of a trench profile are due
#' to tabulating the trench data on an absolute scale in vertical dimension, so
#' that positions where the surface height is below the maximum surface height
#' have no data. It extracts the value of the bin for which **all** profiles
#' have a data value for the first time.
#'
#' @param data a trench data set following the default structure used in the
#'   package.
#' @param var character string with the name of a trench data variable.
#' @param vscale  character string giving the name of the vertical scale
#'   variable used in the trench data; defaults to \code{"depth"}.
#' @return value of \code{vscale} for the first vertical bin where all profiles
#'   exhibit data in \code{var}.
#' @author Thomas Münch
#' @examples
#' getFirstCompleteDepthBin(t13.trench1)
#' # per definition, all data variables should give the same result
#' getFirstCompleteDepthBin(t13.trench1, var = "dD")
#' @export
#'
getFirstCompleteDepthBin <- function(data, var = "d18O", vscale = "depth") {

  if (!vscale %in% colnames(data)) {
    stop("Unknown column name for vertical scale.")
  }
  if (!var %in% colnames(data)) {
    stop("Unknown data variable.")
  }
  if (!"profileName" %in% colnames(data)) {
    stop("Need column 'profileName'.")
  }

  data %>%
    dplyr::group_by(.data$profileName) %>%
    tidyr::drop_na(dplyr::all_of(var)) %>%
    dplyr::select("profileName", dplyr::all_of(vscale)) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup() %>%
    dplyr::pull(vscale) %>%
    max(.)

}
