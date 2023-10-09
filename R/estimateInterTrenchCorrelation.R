#' Pairwise correlation between trenches
#'
#' Calculate the pairwise correlations of all records in a trench with all
#' records in a second trench; optionally allowing for relative shifts of the
#' records with respect to each other in order to maximize their correlations.
#'
#' @param t1 a generic trench data set.
#' @param t2 a second generic trench data set.
#' @param .var character string with the name of the trench variable for which
#'   to compute the correlations; see also \code{\link{make2D}}.
#' @param optimize logical; set to \code{TRUE} to allow for relative shifts
#'   of the records, specified by \code{lag}, to maximize their correlations.
#' @param lag a vector of lags, i.e. positive and negative index shifts, used
#'   when \code{optimize = TRUE} to shift the records of one trench relative to
#'   the records of the other trench in order to find their maximum
#'   correlations.
#' @importFrom dplyr %>%
#' @return For \code{optimize = FALSE}, a matrix of all pairwise correlations
#'   between the records in \code{t1} and in \code{t2}, where rows tabulate the
#'   profile positions of \code{t1} and columns those of \code{t2}. For
#'   \code{optimize = TRUE}, a list is returned of two matrices in the same
#'   format, where component \code{cor} contains the optimized correlations
#'   and component \code{i} contains the index shifts corresponding to the
#'   maximum correlations.
#' @author Thomas Münch
#' @examples
#'
#' raw <- estimateInterTrenchCorrelation(t13.trench1, t13.trench2)
#' opt <- estimateInterTrenchCorrelation(t13.trench1, t13.trench2,
#'                                       optimize = TRUE, lag = -4 : 4)
#'
#' mean(raw)
#' mean(opt$cor)
#'
#' @export
#'
estimateInterTrenchCorrelation <- function(t1, t2, .var = "d18O",
                                           optimize = FALSE, lag = NULL) {

  if (!optimize) {
    
    x <- make2D(t1, .var = .var)
    y <- make2D(t2, .var = .var)
    cor(x, y, use = "pairwise.complete.obs")

  } else {

    if (!length(lag)) {
      stop("Need vector of lags to optimize correlations.", call. = FALSE)
    }

    x <- make2D(t1, .var = .var, simplify = TRUE)
    y <- make2D(t2, .var = .var, simplify = TRUE)
    n <- ncol(x)
    m <- ncol(y)

    nms <- list(unique(t1$profileName), unique(t2$profileName))

    optc <- opti <- matrix(nrow = n, ncol = m)
    dimnames(optc) <- dimnames(opti) <- nms

    for (i in 1 : n) {
      for (j in 1 : m) {

        opt <- stattools::ShiftCorrelation(x[, i], y[, j], shifts = lag)
        optc[i, j] <- opt$cor
        opti[i, j] <- opt$i

      }
    }

    list(cor = optc, i = opti)
  }

}
