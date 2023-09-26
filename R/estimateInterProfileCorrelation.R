#' Correlation between profiles depending on distance
#'
#' Estimate the pairwise correlation between trench profiles as a function of
#' the distance between the profiles.
#'
#' For a given distance, the function searches the trench data set for all pairs
#' which are separated horizontally by this distance, allowing for a given
#' tolerance in separation distance. It is ensured that profile pairs are
#' counted only once. For every found pair, the correlation between the two
#' profiles is calculated using pairwise complete observations, and the
#' correlations are averaged across all pairs. Uncertainty of the mean
#' correlation is given by the standard deviation across the correlations from
#' all pairs, and by the standard error. The latter is obtained from dividing
#' the standard deviation by the square root of an effective number of
#' observations, which is calculated from the number of found pairs and the
#' average horizontal distance between the trench profiles for the given
#' horizontal autocorrelation (see \code{\link{getEffectiveTrenchDOF}}). This is
#' still an optimistic error estimate, since e.g. the correlation values from
#' two profile pairs located at positions [1, 2] and [2, 3] are certainly not
#' independent, even when accounting for autocorrelation.
#'
#' @param trench a trench data set in form of a matrix or a data frame, where
#'   columns represent the horizontal trench profile positions and rows
#'   represent the vertical scale of the data; see also \code{\link{make2D}}.
#' @param profilePosition vector of the horizontal trench profile positions; see
#'   also \code{\link{getSurfaceProfile}}.
#' @param distances vector of profile spacings for which profile pairs shall be
#'   seeked and analysed for their correlation; must have the same units as
#'   \code{profilePosition}.
#' @param rangeTol relative tolerance allowed for profile spacings; i.e. for a
#'   given spacing d, all profile pairs separated by d * (1 +/- rangeTol) are
#'   analysed.
#' @param a1 horizontal autocorrelation of the trench data at lag 1 to obtain an
#'   effective number of profiles used for the error estimates (see details).
#' @return a tibble of five variables: the analysed profile spacings, the found
#'   number of pairs, the mean correlation between the pairs, and the standard
#'   deviation and standard error of the mean correlation (see details).
#' @author Thomas Münch
#' @examples
#'
#' IPC <- make2D(t13.trench2) %>%
#'   estimateInterProfileCorrelation(
#'     profilePosition = getSurfaceProfile(t13.trench2)$position,
#'     distances = c(10, 30))
#'
#' @export
#'
estimateInterProfileCorrelation <- function(trench, profilePosition, distances,
                                            rangeTol = 0.05, a1 = 0) {

  if (!is.matrix(trench) & !is.data.frame(trench)) {
    stop("Input 'trench' must be a matrix or a data frame.")
  }

  distAvg <- mean(diff(profilePosition))

  results <- list()
  meanCor <- stdDev <- stdErr <- Nfound <- vector()

  count <- 1

  for (iRange in distances) {

    for (iPrf in (1 : ncol(trench))) {

      # search for target profiles at distance iRange +- tolerance
      iTargets <- which(
        (profilePosition <= profilePosition[iPrf] + (1 + rangeTol) * iRange) &
        (profilePosition >= profilePosition[iPrf] + (1 - rangeTol) * iRange))

      # remove 1st profile if included in list of target profiles
      startPrf <- which(iTargets == iPrf)
      if (length(startPrf) != 0) iTargets <- iTargets[-startPrf]
      
      if (length(iTargets) == 0) {

        # no pairs found
        results[[iPrf]] <- NA

      } else {
        
        results[[iPrf]] <- cor(trench[, c(iPrf, iTargets)],
                               use = "pairwise.complete.obs")[1, -1]
      }
    }

    tmp <- unlist(results)[!is.na(unlist(results))]

    Nfound[count]  <- length(tmp)
    meanCor[count] <- mean(tmp)
    stdDev[count]  <- sd(tmp)
    stdErr[count]  <- ifelse(
      Nfound[count] == 0, as.numeric(NA),
      sd(tmp) /
      sqrt(getEffectiveTrenchDOF(a1, N = Nfound[count], delta = distAvg)))

    count <- count + 1

  }

  tibble::tibble(distances = distances, N = Nfound,
                 cor = meanCor, sd = stdDev, se = stdErr)

}
