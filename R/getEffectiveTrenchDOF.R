#' Effective degrees of freedom of autocorrelated trench profiles
#'
#' Calculate the effective degrees of freedom for a set of trench profiles,
#' either given by the number of profiles and their (constant) inter-profile
#' distance, or given by the specific horizontal profile positions, when the
#' horizontal data variations along the trench follow a first-order
#' autoregressive process.
#'
#' @param a1 horizontal autocorrelation of the trench data at lag 1, where lag 1
#'   is measured relative to unit profile distance.
#' @param positions a vector of horizontal profile positions.
#' @param N integer number of profiles; if \code{positions} is not specified,
#'   one can specify the profile set via the number of profiles and a constant
#'   inter-profile distance \code{delta}.
#' @param delta inter-profile distance; if \code{positions} is not specified,
#'   one can specify the profile set via the number of profiles \code{N} and a
#'   constant inter-profile distance.
#' @return number of effective degrees of freedom
#' @examples
#'
#' getEffectiveTrenchDOF(a1 = 0.5, N = 10, delta = 2)
#' # is equivalent:
#' getEffectiveTrenchDOF(a1 = 0.5, positions = seq(0, 18, 2))
#'
#' # for zero autocorrelation Neff = N:
#' getEffectiveTrenchDOF(a1 = 0, N = 10, delta = 2)
#'
#' @author Thomas Münch
#' @inherit Muench2016 references
#' @export
#'
getEffectiveTrenchDOF <- function(a1, positions = NULL,
                                  N = length(positions), delta = 1) {

  if (!length(positions)) {

    if (N == 0) stop("Specify number of profiles.")
    if (N < 0) stop("N must be non-negative.")

    n_set <- diff(seq(from = 0, by = delta, length.out = N))

  } else {

    n_set <- diff(positions)
    if (!all(n_set >= 0)) stop("Profile positions must be in increasing order.")

  }

  if (N == 1) {

    res <- 1

    } else {

      exps <- cumsum(n_set)
      tmp <- sum(a1^exps)

      for (i in 1 : (length(n_set) - 1)) {
            
        exps <- cumsum(n_set[-(1 : i)])
        tmp <- tmp + sum(a1^exps)

      }

      res <- N^2 / (N + 2 * tmp)

    }

  return(res)

}
