##' Annual means from binning.
##'
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @param x 
##' @param ind
##' @param na.rm
##' @return 
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
