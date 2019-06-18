##' Compress a record.
##'
##' Linearly interpolate given data onto a compressed depth scale.
##'
##' The compressed depth scale is calculated by reducing the maximum depth of
##' \code{depth} by the amount of \code{stretch} and dividing the new depth
##' scale into \code{n} equal bins, where \code{n} is the length of
##' \code{depth}. The proxy values on the compressed depth scale are found by
##' linear interpolation of the original values using \code{\link{approx}}.
##' @param depth numeric vector woth the original depth scale of \code{rec}.
##' @param rec numeric vector of proxy values; must be of same length as
##' \code{depth}.
##' @param stretch numeric value of the amount of compression of the original
##' depth scale in the same units as \code{depth}.
##' @return a list with two components:
##' \itemize{
##'   \item depth: numeric vector of length \code{depth} with the compressed
##'     depth scale.
##'   \item rec: numeric vector of length \code{depth} with the proxy values on
##'     the compressed depth scale.
##' }
##' @author Thomas Münch
##' @seealso \code{\link{approx}}
##' @export
CompressRecord <- function(depth, rec, stretch) {

    n <- length(depth)
    if (n != length(rec)) {
        stop("Depth and record vector must be of same length.")
    }

    res <- list()
    
    # depth scale after densification
    res$depth <- seq(depth[1], depth[n] - stretch, length.out = n)

    # approximate record on original depth scale
    res$rec <- approx(res$depth, rec, depth)$y

    return(res)

}

