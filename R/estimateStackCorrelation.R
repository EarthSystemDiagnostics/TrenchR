#' Profile stack correlation statistics
#'
#' This function determines with a Monte Carlo search algorithm all possible
#' unique stacks in a trench which consist of a specified number of individual
#' trench profiles at a required spacing. It then calculates the mean profile
#' for every stack and the correlation of this mean profile with a given
#' reference signal, and returns the average correlation for each combination
#' of distance and number of records.
#'
#' This function can be used to empirically evaluate Eqs. (A14) and (A15) in
#' Münch et al. (2016).
#'
#' @param reference numeric vector with a reference signal to which the profile
#'   sets are correlated; its length must match the length of the profiles in
#'   \code{data}.
#' @param distances a vector of inter-profile distances for which profile sets
#'   shall be seeked and analysed for their correlation with the
#'   \code{reference} profile; must be measured in the same units as the profile
#'   positions of the trench data set.
#' @param nprofiles vector of desired number of profiles in each set.
#' @param rangeTol relative tolerance allowed for profile spacings; i.e. for a
#'   given spacing d, all profile pairs separated by d * (1 +/- rangeTol) are
#'   analysed. You can specify one global tolerance that is applied to every
#'   distance in \code{distances}, or one tolerance for each distance.
#' @param na.rm a logical evaluating to \code{TRUE} or \code{FALSE} indicating
#'   whether \code{NA} values should be stripped before the computation of the
#'   mean stack profiles proceeds.
#' @param verbose logical; if \code{TRUE} print progress messages.
#' @inheritParams estimateInterProfileCorrelation
#' @return a data frame with six columns: the requested inter-profile distance,
#'   the number (`N`) of individual profiles in a stack, the average correlation
#'   across all found stacks (consisting of `N` profiles at the given
#'   inter-profile distance) to the reference, the number of found stacks, and
#'   the actual minimum and maximum inter-profile distances across the found
#'   stacks (a sanity check, since this range should lie within the specified
#'   tolerance according to `distance * (1 +/- rangeTol)`).
#'
#' @author Thomas Münch
#' @inherit Muench2016 references
#' @examples
#' # How well do trench subsets represent the overall trench mean profile?
#' reference <- makeMean(t13.trench1, df = FALSE)
#' representativity <- estimateStackCorrelation(t13.trench1, reference,
#'                                              distances = c(2, 10),
#'                                              nprofiles = 5, rangeTol = 0.2,
#'                                              verbose = FALSE)
#' representativity
#' @export
#'
estimateStackCorrelation <- function(data, reference, distances, nprofiles,
                                     profilePosition = NULL, .var = "d18O",
                                     rangeTol = 0.05, na.rm = FALSE,
                                     verbose = TRUE) {

  is.generic.trench <- TRUE
  tryCatch(is.trench(data, check = "incl.pos"),
           error = function(e) {is.generic.trench <<- FALSE})

  if (is.generic.trench) {

    profilePosition <- getX(data)
    trench <- make2D(data, .var = .var, simplify = TRUE)

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

  if (length(reference) != nrow(trench)) {
    stop("Length of `reference` must match profile length of `data`.",
         call. = FALSE)
  }

  ntol <- length(rangeTol)
  ndist <- length(distances)
  if (ntol != 1) {
    if (ntol != ndist) {
      stop("`rangeTol` must either have length 1 or ",
           "the same length as `distances`.", call. = FALSE)
    }
  } else {
    rangeTol <- rep(rangeTol, ndist)
  }

  save <- data.frame()

  ## process loops

  for (dist in distances) { ## loop spacings

    if (verbose) cat(dist, "")

    tolerance <- dist * rangeTol[which(dist == distances)]
    
    for (i in nprofiles) { ## loop number of desired profiles

      if (verbose) {
        progress <- round((which(i == nprofiles) - 1) / length(nprofiles), 2)
        cat(progress, "%... ")
      }

      foundSets <- list()

      ## try to find `i` profiles but limit attempts
      iP <- 0; iR <- 0
      while ((iP < 1000) & (iR < 10000)) {

        iR <- iR + 1 # increment search attempt

        # start from arbitrary profile
        start <- sample(1 : ncol(trench), 1)
        # define positions with required distances relative to start profile
        spacings <- profilePosition[start] + dist * (0 : (i - 1))

        # determine whether there are actually profiles at these positions
        set <- sapply(1 : i, function(x) {

          found <- which((profilePosition >= spacings[x] - tolerance) &
                         (profilePosition <= spacings[x] + tolerance))

          # no actual profile found
          if (length(found) == 0) found <- NA
          # more than one profile found; select one randomly
          if (length(found) > 1)
            found <- sample(found, 1)

          return(found)
        })

        # extract found profiles
        set <- set[!is.na(set)]
        # prevent multiple occurrences of the same profile
        if (length(set) != 0)
          set <- sort(set)[c(TRUE, diff(sort(set)) != 0)]

        ## check if selected profiles are indeed in desired range tolerance
        inRange <- all(!diff(profilePosition[set]) < dist - tolerance &
                       !diff(profilePosition[set]) > dist + tolerance)

        if (length(set) == i & inRange) { # desired number of profiles found

          iP <- iP + 1 # increment successful run
          foundSets[[iP]] <- set
        }
      }

      if (length(foundSets) > 0) {

        # remove duplicate sets possibly found in random search
        foundSets <- unique(foundSets)

        # average profiles in each set and calculate correlations with reference
        .get.cor <- function(x, na.rm) {
          profile <- apply(trench[, x, drop = FALSE], 1, mean, na.rm = na.rm)
          cor.test(profile, reference)$estimate
        }

        # obtain mean correlation
        meanCorrelation <- foundSets %>%
          sapply(.get.cor, na.rm = na.rm) %>%
          mean()

        # obtain range of profile distances across all sets
        foundDistances  <-
          if (i ==  1) {
            NA
          } else {
            foundSets %>%
              sapply(function(x) {diff(profilePosition[x])}) %>%
              range()
          }

        # collect results
        results <- data.frame(distance = dist, N = i,
                              cor = meanCorrelation,
                              n_sets = length(foundSets),
                              d_min = min(foundDistances),
                              d_max = max(foundDistances))

      } else {

        # no sets found
        results <- data.frame(distance = dist, N = i, cor = NA, n_sets = 0,
                              d_min = NA, d_max = NA)

      }

      save <- rbind(save, results)

    }

    if (verbose) cat("\n")
  }

  return(save)

}
