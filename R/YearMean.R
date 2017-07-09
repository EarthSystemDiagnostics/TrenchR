##' Annual means from binning.
##'
##' Description.
##' @param x The vector for which to compute the annual means according to the
##'     binning specified by \code{ind}.
##' @param ind A vector of indices specifying the binning windows for
##'     calculating annual means.
##' @param na.rm a logical value indicating whether ‘NA’ values should be
##'     stripped before the computation proceeds. Defaults to \code{TRUE}.
##' @return A vector of annual means.
##' @author Thomas Münch
##' @export
YearMean <- function(x, ind, na.rm = TRUE) {
    
    means <- vector()
    for (i in (1 : (length(ind) - 1))) {
        range <- ind[i] : ind[i + 1]
        means[i] <- mean(x[range], na.rm = na.rm)
    }
    
    return(means)
}
