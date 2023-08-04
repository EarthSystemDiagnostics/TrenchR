##' Model temporal change of firn profile.
##'
##' Modify a proxy firn profile (e.g., an isotope record) to simulate changes
##' over time given values of downward advection, compression by densification,
##' and diffusional smoothing.
##' @param rec.in numeric vector with the firn proxy (isotope) record to be
##' modified.
##' @param res the (high) depth resolution of the firn record on which the
##' the modification processes are calculated.
##' @param depth.hires numeric vector with the (high-resolution) depth scale on
##' which \code{rec.in} is recorded; must be of the same length as
##' \code{rec.in} and in units of \code{res}.
##' @param depth.lores numeric vector with a lower resolution depth scale of
##' \code{rec.in}; in the units of \code{res}. This is optional: A sufficiently
##' high depth resolution is needed for proper simulation of the modification
##' processes; if \code{rec.in} and \code{depth.hires} provide an interpolated
##' version of the original record to achieve such a higher resolution,
##' \code{depth.lores} can be used to provide the original lower resolution
##' depth scale, and the modified version of the record is then returned on both
##' the interpolated (high) and the original (low) depth resolution.
##' @param SIGMA diffusion length value to smooth the record; must be in the
##' same units as \code{res}. If \code{NULL}, no smoothing by diffusion is
##' modelled.
##' @param STRETCH value of the amount of compression due to densification of
##' the original depth scale of \code{record} over time; must be in the same
##' units as \code{res}. If \code{NULL}, no compression by densification is
##' modelled.
##' @param ADV value of downward advection of the \code{record} over time; must
##' be in the same units as \code{res}. If \code{NULL}, no downward advection is
##' modelled.
##' @return A list with one or two elements:
##' \describe{
##'   \item{HiRes:}{the modified record on the high depth resolution
##'     \code{res};}
##'   \item{LoRes:}{if \code{depth.lores} is not \code{NULL}, the modified
##'     record on this lower depth resolution.}
##' }
##' @author Thomas Münch
##' @seealso \code{\link{DiffuseRecord}}; \code{\link{CompressRecord}}
##' @export
ModifyRecord <- function(rec.in, res, depth.hires, depth.lores = NULL,
                         SIGMA = NULL, STRETCH = NULL, ADV = NULL) {

    if (is.null(c(SIGMA, STRETCH, ADV))) {
        warning("No modification parameters - returning input.")
    }

    if (length(rec.in) != length(depth.hires)) {
        stop("Input record and depth scale must be of the same length.")
    }
    
    rec.out <- rec.in

    # non-NA range of the data
    noNA <- which(!is.na(rec.in))

    # DIFFUSION
    
    if (!is.null(SIGMA)) {

        if (length(SIGMA) > 1 | is.na(SIGMA)) {
            stop("No valid diffusion parameter.")
        }

        diff <- DiffuseRecord(rec.in[noNA], sigma = rep(SIGMA, length(noNA)),
                              res = res)

        rec.out[noNA] <- diff
        
    }

    # DENSIFICATION

    if (!is.null(STRETCH)) {

        if (length(STRETCH) > 1 | is.na(STRETCH)) {
            stop("No valid densification parameter.")
        }

        stretched <- CompressRecord(depth.hires[noNA],
                                    rec.out[noNA], STRETCH)$rec
        
        rec.out[noNA] <- stretched
        
    }

    # ADVECTION

    if (!is.null(ADV)) {

        if (length(ADV) > 1 | is.na(ADV)) {
            stop("No valid ADVECTION parameter.")
        }

        rec.out <- prxytools::Lag(rec.out, shift = ADV / res)

    }

    # return the modified (high-resolution) record
    ret.HiRes <- rec.out
    # extract the original lower resolution, if appropriate
    if (!is.null(depth.lores)) {
        ret.LoRes <- rec.out[match(depth.lores, depth.hires)]
    } else {
        ret.LoRes <- NULL
    }

    return(list(
        HiRes = ret.HiRes,
        LoRes = ret.LoRes
    ))

}

