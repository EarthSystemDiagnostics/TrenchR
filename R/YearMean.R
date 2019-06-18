##' Means from index binning.
##'
##' Calculate bin means defined by vector index positions.
##' @param x numeric vector for which to compute bin means according to the
##' indices given in \code{ind}.
##' @param ind numeric vector of indices specifying the binning windows to
##' average over; i.e. first binning window is from index number 1 to 2, second
##' binning window from index number 2 to 3, etc.
##' @param na.rm a logical value indicating whether ‘NA’ values should be
##' stripped before the computation proceeds. Defaults to \code{TRUE}.
##' @return a numeric vector of length \code{length(ind) - 1} with the mean
##' values of each bin with end points defined by \code{ind}.
##' @author Thomas Münch
##' @examples
##' x <- rnorm(20)
##' x.bin <- YearMean(x, ind = c(1, 7, 13, 17, 20))
##' @export
YearMean <- function(x, ind, na.rm = TRUE) {
    
    means <- vector()
    for (i in (1 : (length(ind) - 1))) {
        range <- ind[i] : ind[i + 1]
        means[i] <- mean(x[range], na.rm = na.rm)
    }
    
    return(means)
}
