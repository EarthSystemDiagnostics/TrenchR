#' Correlation between mean trench profiles
#'
#' Calculate the mean profiles for two trench datasets and obtain the
#' correlation between them, optionally allowing for relative shifts between the
#' profiles and/or for interpolation onto a higher resolution.
#'
#' @param t1 a generic trench dataset.
#' @param t2 a second generic trench dataset.
#' @param .var character string with the name of the trench variable for which
#'   to compute the mean profile correlation; see also \code{\link{make2D}}.
#' @param vscale the name of the vertical scale as a character string; must be
#'   the same for both datasets and both scales must be equal; defaults to
#'   \code{"depth"}.
#' @param na.rm a logical evaluating to \code{TRUE} or \code{FALSE} indicating
#'   whether \code{NA} values should be stripped before the computation of the
#'   mean profiles proceeds.
#' @param res resolution of the vertical trench scales (\code{vscale}); here, a
#'   different (higher) than the inherent resolution can be specified onto which
#'   the mean profiles are interpolated before computing their correlation.
#'   Default \code{NULL} means to use the inherent resolution of the trench
#'   datasets.
#' @param lag a vector of lags, i.e. positive and negative indices by which to
#'   shift the mean profiles relative to each other in order to find their
#'   maximum correlation. Default \code{NULL} means to apply no shifts.
#' @return a tibble with one row, including per default the correlation and the
#'   root-mean-square deviation (`rmsd`) between the mean profiles; if a vector
#'   of \code{lags} has been specified, it includes the correlation and `rmsd`
#'   for the optimal shift (maximum correlation) and additionally the bin index
#'   and respective value of the vertical scale for which this optimal
#'   correlation is observed.
#'
#' @author Thomas Münch
#' @examples
#'
#' getMeanProfileCorrelation(t15.trench1, t15.trench2)
#' getMeanProfileCorrelation(t15.trench1, t15.trench2, .var = "dD")
#'
#' getMeanProfileCorrelation(t15.trench1, t15.trench2,
#'                           lag = -4 : 4)
#' getMeanProfileCorrelation(t15.trench1, t15.trench2,
#'                           res = 0.5, lag = -24 : 24)
#'
#' @export
#'
getMeanProfileCorrelation <- function(t1, t2, .var = "d18O", vscale = "depth",
                                      na.rm = FALSE, res = NULL, lag = NULL) {

  depth <- getZ(t1, vscale = vscale)
  if (!is.identical(depth, getZ(t2, vscale = vscale)))
    stop("Trench datasets must have a common `vscale`.", call. = FALSE)

  x1 <- makeMean(t1, .var = .var, vscale = vscale, na.rm = na.rm, df = FALSE)
  x2 <- makeMean(t2, .var = .var, vscale = vscale, na.rm = na.rm, df = FALSE)

  if ((nr <- length(res))) {

    if (nr > 1) stop("`res` must be of length 1.", call. = FALSE)

    r <- range(depth)
    newdepth <- seq(r[1], r[2], by = res)

    x1 <- stats::approx(depth, x1, newdepth)$y
    x2 <- stats::approx(depth, x2, newdepth)$y

  } else {

    res <- depth[2] - depth[1]

  }

  if (!length(lag)) {

    dplyr::tibble(
             correlation = stats::cor(x1, x2, use = "pairwise.complete.obs"),
             rmsd = stattools::rmsd(x1, x2, na.rm = TRUE))

  } else {

    stattools::ShiftCorrelation(x1, x2, shifts = lag, unit = res) %>%
      setNames(c("bin_shift", "vscale_shift", "correlation")) %>%
      dplyr::mutate(
               rmsd = stattools::rmsd(x1, prxytools::Lag(x2, .data$bin_shift),
                                      na.rm = TRUE)) %>%
        dplyr::as_tibble()

  }

}
