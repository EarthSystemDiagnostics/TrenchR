##' Firn densification rate
##'
##' Function to return the relative densification rate in \% per metre over a
##' specified depth, given a record of firn densities. The densification rate is
##' found from linear regression of the firn density profile against depth.
##' @param dat a list providing a firn density profile with two
##' elements: \code{depth}, a vector with the depth scale of the density
##' profile in metre; and \code{density}, a vector of firn densities. If
##' \code{NULL} (the default), the stacked B41/B42 density profile, provided
##' with the package, is used for the calculation (see Münch et al., 2017).
##' @param lim the depth (in m) over which the densification rate shall be
##' calculated.
##' @return the relative linear densification rate (in \% per metre) from zero
##' depth to the depth \code{lim}.
##' @author Thomas Münch
##' @inherit Muench2017 references
##' @export
DensificationRate <- function(dat = NULL, lim) {

    if (is.null(dat)) {
        warning(paste("No specific input data supplied --",
                      "using data of paper for caluclations."), call. = FALSE)
        dat <- list()
        dat$depth <- density.kohnen$b41$depth
        dat$density <- rowMeans(
            cbind(density.kohnen$b41$density,
                  approx(density.kohnen$b42$depth, density.kohnen$b42$density,
                         dat$depth)$y))
    }

    # linear regression
    ran <- which(dat$depth <= lim)
    reg <- lm(dat$density[ran] ~ dat$depth[ran])
    tmp <- coefficients(reg)
    # relative densification rate [% per m]
    densf.rate <- as.numeric((tmp[2] / tmp[1]) * 100)
    
return(densf.rate)

}


