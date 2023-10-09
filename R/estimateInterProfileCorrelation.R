#' Correlation between profiles depending on distance
#'
#' Estimate the pairwise correlation between trench profiles as a function of
#' the distance between the profiles. To allow for more flexibility, the trench
#' data can be passed in the generic package form, or as a numeric simple matrix
#' or data frame where columns represent the horizontal trench profile positions
#' and rows represent the vertical scale of the data.
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
#' @param data either a generic trench data set, or a trench data set in form
#'   of a numeric matrix or data frame, where columns represent the horizontal
#'   trench profile positions and rows represent the vertical scale of the
#'   data.
#' @param distances a vector of inter-profile distances for which profile pairs
#'   shall be seeked and analysed for their correlation; must be measured in the
#'   same units as the profile positions of the trench data set.
#' @param profilePosition optional vector of the horizontal trench profile
#'   positions; only needed when \code{data} is passed as a simple matrix or
#'   data frame.
#' @param .var character string with the name of the trench variable for which to
#'   compute the correlations; see also \code{\link{make2D}}. Only needed when
#'   the \code{data} is passed as a generic trench data set.
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
#' IPC <- t13.trench2 %>%
#'   estimateInterProfileCorrelation(distances = c(10, 30))
#' # you can pass data also in 2D form,
#' # but then the profile positions need to be provided seperately
#' IPC2 <- make2D(t13.trench2) %>%
#'   estimateInterProfileCorrelation(
#'     profilePosition = getX(t13.trench2),
#'     distances = c(10, 30))
#' all.equal(IPC, IPC2) # by definition
#'
#' @export
#'
estimateInterProfileCorrelation <- function(data, distances,
                                            profilePosition = NULL,
                                            .var = "d18O",
                                            rangeTol = 0.05, a1 = 0) {

  is.generic.trench <- TRUE
  tryCatch(is.trench(data, check = "incl.pos"),
           error = function(e) {is.generic.trench <<- FALSE})

  if (is.generic.trench) {

    profilePosition <- getX(data)
    trench <- make2D(data, .var = .var)

  } else {

    trench <- data
    if (!is.matrix(trench) & !is.data.frame(trench)) {
      stop("Input 'data' neither generic trench, nor a matrix or data frame.",
           call. = FALSE)
    }
    if (!length(profilePosition)) {
      stop("Need a vector of profile positions.", call. = FALSE)
    }
    if (length(profilePosition) != ncol(trench)) {
      stop("Number of profile positions does not match columns in data.",
           call. = FALSE)
    }
    if (!is.numeric(as.matrix(trench))) {
      stop("Non-numeric columns in data.", call. = FALSE)
    }

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
