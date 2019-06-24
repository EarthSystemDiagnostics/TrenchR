##' Find Maxima or Minima.
##'
##' Find the positions of the maxima or minima in a data vector.
##' @param x Numeric vector for which maxima (minima) shall be analysed.
##' @param minima if \code{FALSE} (the default) return the positions of the
##' maxima of \code{x}, else return the positions of the minima.
##' @param partial if \code{TRUE}, the first and last data points of \code{x}
##' are considered to be maxima or minima as well; defaults to \code{FALSE}.
##' @return Numeric vector with the index positions of the maxima (minima) of
##' \code{x}.
##' @author Thomas Münch
##' @export
which.peaks <- function(x, minima = FALSE, partial = FALSE) {

    x.diff <- diff(x)
    
     if (minima) {
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
