##' Find Locations of Maxima or Minima.
##'
##' Details
##' @param x 
##' @param partial 
##' @param decreasing 
##' @return The indices of the maxima or minima of x.
##' @author Thomas Münch
which.peaks <- function(x, partial = TRUE, decreasing = FALSE) {

    x.diff <- diff(x)
    
     if (decreasing) {
         if (partial) {
             which(diff(c(FALSE, x.diff > 0, TRUE)) > 0)
         } else {
             which(diff(x.diff > 0) > 0) + 1
         }
     } else {
         if (partial) {
             which(diff(c(TRUE, x.diff >= 0, FALSE)) < 0)
         } else {
             which(diff(x.diff >= 0) < 0) + 1
         }
     }
}
