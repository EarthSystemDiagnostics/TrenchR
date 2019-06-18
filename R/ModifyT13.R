##' Model temporal change of isotope profile.
##'
##' Modify an isotope profile to simulate changes over time given values of
##' downward advection, compression by densification, and diffusional
##' smoothing.
##' @param TR tbd
##' @param SIGMA diffusion length value to smooth the record; must be in the
##' same units as \code{res}.
##' @param STRETCH value of the amount of compression due to densification of
##' the original depth scale of \code{record} over time; must be in the same
##' units as \code{res}.
##' @param ADV value of downward advection of the \code{record} over time; must
##' be in the same units as \code{res}.
##' @return A list with two elements:
##' \describe{
##'   \item{HiRes:}{the modified record on the high depth resolution
##'     \code{res};}
##'   \item{LoRes:}{the modified record on the low depth resolution according to
##'     \code{depth.lores}.}
##' }
##' @author Thomas Münch
##' @seealso \code{\link{DiffuseRecord}}; \code{\link{CompressRecord}}
##' @export
ModifyT13 <- function(TR = prepareTrenchData(na.treat = TRUE)$oxy,
                      SIGMA = NULL, STRETCH = NULL, ADV = NULL) {

    if (is.null(c(SIGMA, STRETCH, ADV))) {
        warning("No modification parameters - returning input.")
    }
    
    ret <- input <- TR$mean13_HiRes

    # non-NA range of high-resolution trench T13 data
    noNA <- which(!is.na(input))

    # DIFFUSION
    
    if (!is.null(SIGMA)) {

        if (length(SIGMA) > 1 | is.na(SIGMA)) {
            stop("No valid diffusion parameter.")
        }

        diff <- DiffuseRecord(input[noNA], sigma = rep(SIGMA, length(noNA)),
                              res = TR$HiRes)

        ret[noNA] <- diff
        
    }

    # DENSIFICATION

    if (!is.null(STRETCH)) {

        if (length(STRETCH) > 1 | is.na(STRETCH)) {
            stop("No valid densification parameter.")
        }

        stretched <- CompressRecord(TR$depth_HiRes[noNA],
                                    ret[noNA], STRETCH)$rec
        
        ret[noNA] <- stretched
        
    }

    # ADVECTION

    if (!is.null(ADV)) {

        if (length(ADV) > 1 | is.na(ADV)) {
            stop("No valid ADVECTION parameter.")
        }

        ret <- Hmisc::Lag(ret, shift = ADV / TR$HiRes)

    }

    # EXTRACT 3CM RESOLUTION
    ret.HiRes <- ret
    ret.LoRes <- ret[match(TR$depth, TR$depth_HiRes)]

    return(list(
        HiRes = ret.HiRes,
        LoRes = ret.LoRes
    ))

}

