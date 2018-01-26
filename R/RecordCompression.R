##' Calculate compression of firn record.
##'
##' .. content for \details{} ..
##' @param length.in 
##' @param adv
##' @param rate
##' @param res numerical resolution for calculation
##' @return 
##' @author Thomas Münch
##' @references
##' Muench, T., et al., Constraints on post-depositional isotope modifications
##' in East Antarctic firn from analysing temporal changes of isotope profiles,
##' The Cryosphere, doi:10.5194/tc-11-2175-2017, 2017.
##' @export
RecordCompression <- function(length.in, adv, rate, res = 1) {

    # input densification rate is in %/m; convert to 1/cm
    rate <- rate / 10000
    
    # positions of first record entry during advection
    pos1 <- seq(0, adv - res, res)
    # positions of last record entry during advection
    pos2 <- seq(length.in, length.in + adv - res, res)

    # size of top/bottom bin of record during densification
    res1 <- res2 <- vector(length = length(pos1))
    for (ix in 1 : length(res1)) {
        res1[ix] <- Compression(pos1[ix], pos1[ix] + res, rate)
        res2[ix] <- Compression(pos2[ix], pos2[ix] + res, rate)
    }

    # average compression
    bin.size <- (prod(res1) + prod(res2)) / 2

    # average length of record after densification
    length.out <- bin.size * length.in

    # return length change
    return(length.in - length.out)

}

