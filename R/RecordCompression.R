##' Calculate compression of firn-core record.
##'
##' Calculate the compression due to densification with a constant rate during
##' downward-advection of a firn-core record.
##' @param z.start the depth of the upper end of the firn-core record in [cm].
##' @param adv the amount of downward advection in [cm].
##' @param length.in the initial length of the firn-core record in [cm].
##' @param rate constant rate of firn densification relative to the surface
##' density; in [\% per metre].
##' @param res resolution (in [cm]) of the compression calculation, i.e. the
##' size of the individual depth bins of the record ("snow parcels") that are
##' advected downwards.
##' @return the length change of the firn-core record in [cm].
##' @author Thomas Münch
##' @seealso \code{\link{Compression}}
##' @references
##' Münch, T., et al., Constraints on post-depositional isotope modifications
##' in East Antarctic firn from analysing temporal changes of isotope profiles,
##' The Cryosphere, doi:10.5194/tc-11-2175-2017, 2017.
##' @examples
##' # Length change of trench record for mean estimated densification rate
##' # (see Münch et al., 2017):
##' RecordCompression(adv = 50, length.in = 100, rate = 4.5)
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

