##' Differential firn diffusion
##'
##' Calculate the differential diffusion, as defined in Münch et al. (2007),
##' over a specified depth interval given a diffusion length profile.
##'
##' Mean diffusion lengths are calculated for the depth intervals \code{[z00,
##' z01]} and \code{[z10, z11]}, and the differential diffusion length is
##' obtained between these two intervals according to Eq. (2) in Münch et
##' al. (2017).
##' @param dat a list providing a firn density profile, in order to calculate
##' diffusion lengths, with two elements: \code{depth}, a vector with the depth
##' scale of the density profile in [m]; and \code{density}, a vector of firn
##' densities in [kg/m^3]. If \code{NULL} (the default), the fitted B41/B42
##' density profile, provided with the package, is used for the calculation (see
##' Münch et al., 2017).
##' @param z00 upper boundary of the first depth interval.
##' @param z01 lower boundary of the first depth interval.
##' @param z10 upper boundary of the second depth interval.
##' @param z11 lower boundary of the second depth interval.
##' @param P atmospheric surface pressure in [mbar] to calculate diffusion
##' lengths; default is 650 mbar for Kohnen Station.
##' @param T annual mean surface air temperature in [K] to calculate diffusion
##' lengths; default is 228.5 K for Kohnen Station.
##' @param bdot annual mean accumulation rate in [kg/m^2/yr] to calculate
##' diffusion lengths; default is 64 kg/m^2/yr for Kohnen Station.
##' @return a named vector with two elements: \code{oxy}, the differential
##' diffusion length value for oxygen isotopes; \code{dtr}, the differential
##' diffusion length value for deuterium.
##' @author Thomas Münch
##' @source Eq. (2) in Münch et al. (2017)
##' @inherit Muench2017 references
##' @export
DifferentialDiffusion <- function(dat = NULL, z00, z01, z10, z11,
                                  P = 650, T = 228.5, bdot = 64) {

    if (!requireNamespace("FirnR", quietly = TRUE)) {
        stop(paste("FirnR needed for this function to work. The package is",
                   "available on reasonable request from the author."),
             call. = FALSE)
    }
    
    if (is.null(dat)) {
        warning(paste("No specific input data supplied",
                      "-- using data of Münch et al. (2017) for calculations."),
                call. = FALSE)
        dat <- list()
        dat$depth   <- density.kohnen$b4x.fit$depth
        dat$density <- density.kohnen$b4x.fit$density
    }


    ix1 <- which(dat$depth >= z00 & dat$depth <= z01)
    ix2 <- which(dat$depth > z10 & dat$depth <= z11)

    ## d18O
    sigma <- FirnR::DiffusionLength(dat$depth, dat$density,
                                    P = P, T = T, bdot = bdot)
    s1 <- mean(sigma[ix1])
    s2 <- mean(sigma[ix2])

    s.tilde.oxy <- sqrt(s2^2 - s1^2)

    ## d2H
    sigma <- FirnR::DiffusionLength(dat$depth, dat$density,
                                    P = P, T = T, bdot = bdot, dD = TRUE)
    s1 <- mean(sigma[ix1])
    s2 <- mean(sigma[ix2])

    s.tilde.dtr <- sqrt(s2^2 - s1^2)

    return(c(oxy = s.tilde.oxy, dtr = s.tilde.dtr))

}
    
