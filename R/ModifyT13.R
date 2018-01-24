##' Model T13 temporal changes.
##'
##' Modify the T13 mean isotope profile. (Details)
##' @param TR 
##' @param SIGMA
##' @param STRETCH
##' @param ADV
##' @return A list.
##' @author Thomas Münch
##' @export
ModifyT13 <- function(TR = prepareTrenchData(na.treat = TRUE)$oxy,
                      SIGMA = NULL, STRETCH = NULL, ADV = NULL) {

    if (is.null(c(SIGMA, STRETCH, ADV))) {
        warning("No modification parameters - returning input.")
    }

    ret <- input <- TR$mean13_HiRes

    # non-NA range of high-resolution trench T13 data
    noNA <- which(!is.na(input))
    n <- length(noNA)

    # DIFFUSION
    
    if (!is.null(SIGMA)) {

        if (length(SIGMA) > 1 | is.na(SIGMA)) {
            stop("No valid diffusion parameter.")
        }

        diff <- DiffuseRecord(input[noNA], sigma = rep(SIGMA, n),
                              res = TR$HiRes)

        ret[noNA] <- diff
        
    }

    # DENSIFICATION

    if (!is.null(STRETCH)) {

        if (length(STRETCH) > 1 | is.na(STRETCH)) {
            stop("No valid densification parameter.")
        }

        # depth scale after densification
        depth.stretch <- seq(TR$depth_HiRes[noNA][1],
                             TR$depth_HiRes[noNA][n] - STRETCH,
                             length.out = n)

        # approximate record on compressed depth scale
        stretched <- approx(depth.stretch, ret[noNA],
                            TR$depth_HiRes[noNA])$y

        ret[noNA] <- stretched
        
    }

    # ADVECTION

    if (!is.null(ADV)) {

        if (length(ADV) > 1 | is.na(ADV)) {
            stop("No valid ADVECTION parameter.")
        }

        ret <- Hmisc::Lag(ret, shift = ADV)

    }

    # EXTRACT 3CM RESOLUTION
    ret.HiRes <- ret
    ret.LoRes <- ret[match(TR$depth, TR$depth_HiRes)]

    return(list(
        HiRes = ret.HiRes,
        LoRes = ret.LoRes
    ))

}

