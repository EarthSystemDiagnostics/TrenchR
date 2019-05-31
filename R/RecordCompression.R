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
RecordCompression <- function(z.start = 0, adv, length.in, rate, res = 1) {

    # input densification rate is in %/m; convert to 1/cm
    rate <- rate / 10000

    # define positions of depth increments
    pos <- seq(z.start + res / 2, z.start + length.in, res)

    # calculate compression of defined bins
    compr <- vector(length = length(pos))
    for (i in 1 : length(pos)) {

        compr[i] <- res *
            Compression(z1 = pos[i], z2 = pos[i] + adv, rate = rate)
    }

    # compressed length of record
    length.out <- sum(compr)

    # return length change
    return(length.in - length.out)

}

