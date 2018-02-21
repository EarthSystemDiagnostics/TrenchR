##' Compress a firn record.
##'
##' .. content for \details{} ..
##' @param depth 
##' @param rec 
##' @param stretch 
##' @return 
##' @author Thomas Münch
##' @export
CompressRecord <- function(depth, rec, stretch) {

    if (length(depth) != length(rec)) {
        stop("Depth and record vector must be of same length.")
    }

    n <- length(depth)
    res <- list()
    
    # depth scale after densification
    res$depth <- seq(depth[1], depth[n] - stretch, length.out = n)

    # approximate record on original depth scale
    res$rec <- approx(res$depth, rec, depth)$y

    return(res)

}

