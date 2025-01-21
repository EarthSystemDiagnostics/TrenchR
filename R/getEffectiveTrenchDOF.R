#' Effective degrees of freedom of autocorrelated trench profiles
#'
#' Calculate the effective degrees of freedom for a set of trench profiles,
#' either given by the number of profiles and their (constant) inter-profile
#' distance, or given by their specific horizontal profile positions. This
#' calculation assumes the horizontal data variations along the trench to follow
#' a first-order autoregressive (AR1) process.
#'
#' The effective degrees of freedom, `N_eff`, returned by this function are
#' the inverse of the quantity `sigma^{*2}_{i}` in Münch et al. (2016),
#' Eq. (A12): `sigma^{*2}_{i} = 1 / N_eff`; see also Eq. (1) in Münch et
#' al. (2017).
#'
#' @param lambda horizontal decorrelation length of the assumed AR1 process
#'   measured in the same units as \code{positions}; if specified used to
#'   calculate the corresponding lag-1 autocorrelation via \code{a1 = exp(-1 /
#'   lambda)}.
#' @param a1 horizontal autocorrelation of the trench data at lag 1, where lag 1
#'   is measured relative to unit profile distance; only used if \code{lambda}
#'   is not specified, and specifying both \code{lambda} and \code{a1} gives an
#'   error.
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
#' # alternatively, one can specify a decorrelation length directly;
#' # above a1 (measured at unit distace) corresponds to lambda = -1 / log(a1):
#' getEffectiveTrenchDOF(lambda = -1 / log(0.5), N = 10, delta = 2)
#'
#' # for zero autocorrelation Neff = N:
#' getEffectiveTrenchDOF(a1 = 0, N = 10, delta = 2) # or
#' getEffectiveTrenchDOF(lambda = 0, N = 10, delta = 2)
#'
#' @author Thomas Münch
#' @inherit MuenchTrenchPaper references
#' @export
#'
getEffectiveTrenchDOF <- function(lambda = NULL, a1 = NULL, positions = NULL,
                                  N = length(positions), delta = 1) {

  n1 <- length(lambda)
  n2 <- length(a1)

  if (n1 > 1) stop("`lambda` must be of length 1 or `NULL`.")
  if (n2 > 1) stop("`a1` must be of length 1 or `NULL`.")

  if ((n1 + n2) == 0 | (n1 + n2) == 2) stop("Specify either `lambda` or `a1`.")

  if (n2 == 0) a1 <- exp(-1 / lambda)

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
