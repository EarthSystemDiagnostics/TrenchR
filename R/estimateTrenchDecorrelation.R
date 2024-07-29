#' Autocorrelation estimation on unevenly spaced data
#'
#' This function estimates the autocorrelation of a numeric data vector, based
#' on the Pearson correlation coefficient and using the Gaussian kernel estimation
#' technique of Rehfeld et al. (2011) to account for an irregular sampling
#' spacing of the data. NA values in the data are removed before estimation with
#' the sampling spacing adjusted accordingly, but when the number of NAs exceeds
#' 1/3 of the number of data points, autocorrelation is returned as NA.
#'
#' This function is adapted from the R port by Kira Rehfeld of the function of
#' the same name from the MATLAB NESToolbox
#' (<https://tocsy.pik-potsdam.de/nest.php>), which is licensed under
#' GPL-3
#' Copyright (C) 2013 Kira Rehfeld
#'
#' Changes by Thomas Münch to the original R port code are as follows:
#' - function solely calculates autocorrelation, so takes only a single data
#'   vector as input
#' - code does not expect input data as a zoo object but as a simple numeric
#'   vector, so the sampling positioning vector is required as additional input
#'   to the function (but the code internally ensures that the data are sorted
#'   according to the sampling positions)
#' - handling of NA values in the data is introduced as described above
#' - extended input error checking
#' - using modernised R code
#' - normalisation of sampling interval units is based on the original MATLAB
#'   (and Python port) code (see <https://tocsy.pik-potsdam.de/nest.php>), and
#'   not on the - probably erroneous - reverse normalisation as implemented in
#'   the original R port code
#'
#' @param x a numeric vector to estimate its autocorrelation.
#' @param pos numeric vector the same length of \code{x} supplying the
#'   non-equidistant positions (e.g. spatial positions, observation time points)
#'   at which \code{x} is tabulated.
#' @param lag integer vector of lags for which autocorrelations are
#'   estimated. Note that only if \code{lag} contains `0`, the function returns
#'   true autocorrelations since only then all estimated kernel correlations can
#'   be normalized to the lag-0 kernel correlation.
#' @param h width of the Gaussian kernel to estimate correlations in normalised
#'   units, defaults to 0.25 (Rehfeld et al., 2011); ideally, there should be no
#'   need for adjustment.
#'
#' @return numeric vector the same length as \code{lag} with the estimated
#'   (auto)correlation values.
#'
#' @author Kira Rehfeld, Thomas Münch
#' @references
#' Rehfeld, K., Marwan, N., Heitzig, J., and Kurths, J.: Comparison of
#'   correlation analysis techniques for irregularly sampled time series,
#'   Nonlinear Proc. Geoph., 18(3), 389–404, doi:
#'   https://doi.org/10.5194/npg18-389-2011, 2011
#' @noRd
#'
nexcf <- function(x, pos, lag = 0, h = 0.25) {

  # error checking

  nx <- length(x)
  nl <- length(lag)

  nna <- length(ina <- which(is.na(x)))

  if (nna > (1 / 3) * nx) {

    warning("nexcf: Too many NAs to estimate autocorrelation.", call. = FALSE)
    return(rep(NA, nl))

  }

  if (missing(pos))
    stop("nexcf: Supply positions on which `x` is tabulated.", call. = FALSE)

  if (length(x) != length(pos))
    stop("nexcf: Length of data does not match length of `pos`.", call. = FALSE)

  if (any(is.na(pos)))
    stop("nexcf: Missing values present in `pos`.", call. = FALSE)
  if (any(is.na(lag)))
    stop("nexcf: Missing values present in `lag`.", call. = FALSE)

  if (any(lag < 0)) stop("nexcf: All lags must be >= 0.", call. = FALSE)

  if (is.na(h)) stop("nexcf: `h` is NA.", call. = FALSE)
  if (h <= 0) stop("nexcf: `h` must be > 0.", call. = FALSE)

  # wrapper fun for Gaussian kernel
  .gauss <- function(dt, h) {1 / (sqrt(2 * pi) * h) * exp(-dt^2 / (2 * h^2))}

  # kernel correlation estimation
  .kernel_estimate <- function(l, p, t, h) {

    # build distance matrix for considered lag
    dist <- t + l

    # Gaussian kernel matrix
    g_kernel <- .gauss(dist, h)
    weight <- sum(g_kernel)

    kernel_cont <- (dist <= 5 * h) & (dist >= (-5 * h))

    if (sum(g_kernel[kernel_cont], na.rm = TRUE) < 0.5) {

      warning("nexcf: Insufficient kernel content; try using a larger kernel.",
              call. = FALSE)
      return(NA)
    }

    # correlation estimate
    sum(p * g_kernel) / weight

  }

  # remove any NA values

  if (nna) {

    x <- x[-ina]
    pos <- pos[-ina]
  }

  # order `x` with respect to `pos`

  i <- sort.list(pos)
  x <- x[i]
  pos <- pos[i]

  # normalization factor for the positioning

  dtlag <- ifelse(nl == 1, 1, mean(diff(pos)))

  tx <- pos / dtlag
  normlag <- lag / dtlag

  # normalize data

  x <- scale(x)

  # pairwise position distance and data product matrices

  tdist_xy <- -1 * outer(tx, tx, "-")
  pdist_xy <- x %*% t(x)

  # run kernel estimation

  corr <- normlag %>%
    sapply(.kernel_estimate, p = pdist_xy, t = tdist_xy, h = h)

  if (length(i <- which(is.na(corr))) == nl)
    stop("nexcf: Could not estimate any autocorrelation values.", call. = FALSE)

  if (nl > 1 & any(lag == 0)) corr <- corr / corr[which(lag == 0)]

  return(corr)

}

#' Calculate lag-1 autocorrelation for equidistant sampling positions
#'
#' Internal function called from \code{\link{estimateTrenchDecorrelation}}; not
#' for stand-alone usage.
#'
#' @param x a trench dataset in matrix form as output by \code{\link{make2D}}.
#' @param x.no.surface \code{x} without the potentially incomplete surface
#'   region (e.g., as obtained by applying \code{\link{removeSurfaceRegion}} on
#'   the underlying generic trench data before calling \code{make2D}).
#' @param direction integer signalling in which direction to calculate the
#'   average lag-1 autocorrelation: `1` for horizontal, `2` for vertical.
#'
#' @return the average lag-1 autocorrelation in the trench direction specified
#'   by \code{direction}. Here, lag-1 corresponds to shifting the data vectors
#'   by one index (base R `acf` estimation); the corresponding physical lag is
#'   given by the distance between the sampling positions in the requested
#'   trench \code{direction}.
#'
#' @author Thomas Münch
#' @noRd
#'
calcEquidistantAC1 <- function(x, x.no.surface, direction) {

  if (!is.matrix(x)) stop("`x` must be a matrix.")
  if (!is.matrix(x.no.surface)) stop("`x.no.surface` must be a matrix.")

  if (ncol(x) != ncol(x.no.surface) | nrow(x.no.surface) > nrow(x)) {
    stop("Dimensions of `x` and `x.no.surface` do not match.")
  }

  if (!direction %in% c(1, 2)) stop("`direction` must be set to `1` or `2`.")

  # wrapper to calculate lag-1 autocorrelation
  .acf <- function(x) {stats::acf(x, plot = FALSE, na.action = na.pass)$acf[2]}

  # check if any internal NAs are present
  has.na <- any(is.na(x.no.surface))

  # which matrix to analyse depends on analysis direction and NA positions
  if (direction == 1) x.analysis <- x.no.surface else x.analysis <- x

  if (has.na) {

    # remove rows/cols that contain internal NAs

    na.indices <- which(is.na(x.no.surface), arr.ind = TRUE)[, direction] %>%
      unique()
    nna <- length(na.indices)

    dirchar <- ifelse(direction == 1, "row(s)", "column(s)")
    sprintf("%i %s removed containing NA values.", nna, dirchar) %>%
        warning(call. = FALSE)

    if (direction == 2) x.analysis <- t(x.analysis)

    x.analysis <- x.analysis[-na.indices, , drop = FALSE]

    if (direction == 2) x.analysis <- t(x.analysis)

  }

  apply(x.analysis, direction, .acf) %>%
    mean()

}

#' Calculate lag-1 autocorrelation for non-equidistant sampling positions
#'
#' Internal function called from \code{\link{estimateTrenchDecorrelation}}; not
#' for stand-alone usage.
#'
#' @param x a trench dataset in matrix form as output by \code{\link{make2D}}.
#' @param pos numeric vector supplying the non-equidistant sampling positions
#'   (e.g. spatial positions, observation time points) in the requested
#'   `direction` of the trench dataset.
#' @param direction integer signalling in which direction to calculate the
#'   average lag-1 autocorrelation: `1` for horizontal, `2` for vertical.
#' @param lag integer vector of lags for which autocorrelations are
#'   estimated, measured in the same physical units as `pos`. Needs to be of the
#'   form `lag = c(0, <lag-1>)`, where <lag-1> is the physical distance
#'   corresponding to the desired lag-1 autocorrelation; defaults to `c(0, 1)`.
#'
#' @return the average lag-1 autocorrelation (i.e., corresponding to the second
#'   entry of `lag`) in the trench direction specified by \code{direction}.
#'
#' @author Thomas Münch
#' @noRd
#'
calcNonEquidistantAC1 <- function(x, pos, direction, lag = c(0, 1)) {

  if (!is.matrix(x)) stop("`x` must be a matrix.")
  if (!direction %in% c(1, 2)) stop("`direction` must be set to `1` or `2`.")

  n <- if (direction == 1) ncol(x) else nrow(x)
  if (length(pos) != n)
    stop("Length of `pos` does not match requested dimension of data.")

  if (length(lag) != 2) stop("`lag` needs to be of length 2.")
  if (lag[1] != 0) stop("First element of `lag` must be 0.")

  apply(x, direction, nexcf, pos = pos, lag = lag) %>%
    rowMeans(na.rm = TRUE) %>%
    .[2]

}
