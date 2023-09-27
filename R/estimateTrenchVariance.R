#' Horizontal and vertical variance of trench dataset
#'
#' Calculate the average variance in horizontal and vertical direction of a
#' trench dataset. For this, the variances for each row and column,
#' respectively, of the trench dataset are estimated and then averaged. Missing
#' observations are silently removed from the data before calculation.
#'
#' @param trench a trench data set in form of a matrix or a data frame, where
#'   columns represent the horizontal trench profile positions and rows
#'   represent the vertical scale of the data; see also \code{\link{make2D}}.
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
#' make2D(t15.trench1) %>%
#'   estimateTrenchVariance()
#' # some example DOFs to see the effect (no realistic values!):
#' make2D(t15.trench1) %>%
#'   estimateTrenchVariance(dof.h = 5, dof.v = 30)
#'
#' @export
#'
estimateTrenchVariance <- function(trench, dof.h = NULL, dof.v = NULL,
                                   alpha = 1 / 6) {

  if (!is.matrix(trench) & !is.data.frame(trench)) {
    stop("Input 'trench' must be a matrix or a data frame.")
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
