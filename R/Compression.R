##' Compression by densification.
##'
##' Amount of compression of a depth increment for linear densification of firn
##' with a constant rate.
##'
##' The compression is calculated based on the assumption of conservation of
##' mass and assuming that densification leads only to a compression of a firn
##' parcel in vertical direction and not to any stretching in horizontal
##' direction.
##' @param z1 initial depth of the depth increment.
##' @param z2 final depth of the depth increment in the same units as
##' \code{z1}; must be larger than \code{z1}.
##' @param rate constant rate of firn densification relative to the surface
##' density; in inverse units of \code{z1}.
##' @return the relative compression of a depth increment when transported from
##' depth \code{z1} to depth \code{z2} in dimensionless units.
##' @author Thomas Münch
##' @export
Compression <- function(z1, z2, rate) {

    if (z2 < z1) stop("'z2' must be larger than 'z1'.")
    
    return((1 + rate * z1)/(1 + rate * z2))
}
