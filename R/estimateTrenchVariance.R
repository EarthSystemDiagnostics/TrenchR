#' Horizontal and vertical variance of trench dataset
#'
#' Calculate the average variance in horizontal and vertical direction of a
#' trench dataset. For this, the variances across each vertical and horizontal
#' position, respectively, of the trench dataset are estimated and then
#' averaged. Missing observations are silently removed from the data before
#' calculation. To allow for a more flexible input, the trench data can be
#' passed in the generic package form, or as a simple numeric matrix or data
#' frame where columns represent the horizontal trench profile positions and
#' rows represent the vertical scale of the data.
#'
#' @param data either a generic trench data set, or a trench data set in form
#'   of a numeric matrix or data frame, where columns represent the horizontal
#'   trench profile positions and rows represent the vertical scale of the
#'   data.
#' @param .var character string with the name of the trench variable for which to
#'   compute the variances; see also \code{\link{make2D}}. Only needed when
#'   \code{data} is passed as a generic trench data set.
#' @param dof.h number of effective degrees of freedom in horizontal direction.
#' @param dof.v number of effective degrees of freedom in vertical direction.
#' @param alpha probability for the confidence interval calculation;
#'   i.e. confidence intervals are calculated for confidence levels from
#'   \code{alpha} to \code{1 - alpha}.
#' @importFrom dplyr %>%
#' @return a tibble of four variables: the \code{direction} of the variance
#'   calculation as a character string ("horizontal" and "vertical"), the
#'   estimated variance \code{var}, and the \code{lower} and \code{upper} end of
#'   the confidence interval.
#' @author Thomas Münch
#' @examples
#'
#' estimateTrenchVariance(t15.trench1, .var = "d18O")
#'
#' # different input format and some example DOFs to see the effect:
#' make2D(t15.trench1, .var = "d18O") %>%
#'   estimateTrenchVariance(dof.h = 5, dof.v = 30) # (no realistic DOFs!)
#'
#' @export
#'
estimateTrenchVariance <- function(data, .var = NULL,
                                   dof.h = NULL, dof.v = NULL, alpha = 1 / 6) {

  is.generic.trench <- TRUE
  tryCatch(is.trench(data, check = "minimum"),
           error = function(e) {is.generic.trench <<- FALSE})

  if (is.generic.trench) {

    if (!length(.var)) {
      stop("Need name of trench data variable for generic trench 'data'.",
           call. = FALSE)
    }
    trench <- make2D(data, .var = .var)

  } else {

    trench <- data
    if (!is.matrix(trench) & !is.data.frame(trench)) {
      stop("Input 'data' neither generic trench, nor a matrix or data frame.",
           call. = FALSE)
    }
    if (!is.numeric(as.matrix(trench))) {
      stop("Non-numeric columns in data.", call. = FALSE)
    }

  }

  if (!length(dof.h)) dof.h <- ncol(trench)
  if (!length(dof.v)) dof.v <- nrow(trench)

  CI <- function(x, dof.h, dof.v, a) {
    n <- dof.h * dof.v - 1
    x * n / qchisq(a, n)
  }
  
  h.var <- mean(apply(trench, 1, var, na.rm = TRUE), na.rm = TRUE)
  v.var <- mean(apply(trench, 2, var, na.rm = TRUE), na.rm = TRUE)

  tibble::tibble(
    direction = c("horizontal", "vertical"), var = c(h.var, v.var)) %>%
    dplyr::mutate(
      lower = CI(.data$var, dof.h, dof.v, 1 - alpha),
      upper = CI(.data$var, dof.h, dof.v, alpha))

}
