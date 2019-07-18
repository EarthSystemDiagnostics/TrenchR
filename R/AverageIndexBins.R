##' Means from index binning.
##'
##' Calculate bin means defined by vector index positions.
##'
##' Averaging bins are defined consecutively from the first index position
##' to the position one before the second index, i.e., such that the right end
##' of the first bin does not overlap with the left end of the second bin, and
##' so on. The final bin is treated separately: if the last index coincides
##' with the last data point of \code{x}, this point is included in the last bin
##' average, else the bin is treated as the other bins.
##' @param x numeric vector for which to compute bin means according to the
##' indices given in \code{ind}.
##' @param ind numeric vector of indices specifying the binning windows to
##' average over, see details.
##' @param na.rm a logical value indicating whether ‘NA’ values should be
##' stripped before the computation proceeds. Defaults to \code{TRUE}.
##' @return a numeric vector of length \code{length(ind) - 1} with the mean
##' values of each bin with end points defined by \code{ind}.
##' @author Thomas Münch
##' @examples
##' x <- rnorm(20)
##'
##' # final bin includes final data point:
##' AverageIndexBins(x, ind = c(1, 7, 13, 17, 20))
##' # final index is not final data point:
##' AverageIndexBins(x, ind = c(1, 7, 13, 17))
##' @export
AverageIndexBins <- function(x, ind, na.rm = TRUE) {

    n <- length(ind)

    # if final index is final data point, include it in the last bin average
    if (ind[n] == length(x)) ind[n] <- ind[n] + 1
    
    means <- vector()
    for (i in (1 : (n - 1))) {

        # bins defined from first to one before second index
        range <- ind[i] : (ind[i + 1] - 1)
        means[i] <- mean(x[range], na.rm = na.rm)
    }
    
    return(means)
}
