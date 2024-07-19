#' Autocorrelation estimation on unevenly spaced data
#'
#' This function estimates the autocorrelation of a numeric data vector, based
#' on the Pearson correlation coefficient and using the Gaussian kernel estimation
#' technique of Rehfeld et al. (2011) to account for an irregular sampling
#' spacing of the data.
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

  n <- length(lag)

  if (any(is.na(x)))
    stop("nexcf: No missing values allowed in `x`.", call. = FALSE)

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

  # order `x` with respect to `pos`

  i <- sort.list(pos)
  x <- x[i]
  pos <- pos[i]

  # normalization factor for the positioning

  dtlag <- ifelse(n == 1, 1, mean(diff(pos)))

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

  if (length(i <- which(is.na(corr))) == n)
    stop("nexcf: Could not estimate any autocorrelation values.", call. = FALSE)

  if (n > 1 & any(lag == 0)) corr <- corr / corr[which(lag == 0)]

  return(corr)

}
