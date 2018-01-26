##' Differential firn diffusion.
##'
##' .. content for \details{} ..
##' @param dat 
##' @param z00 
##' @param z01 
##' @param z10 
##' @param z11 
##' @param P 
##' @param T 
##' @param bdot 
##' @return 
##' @author Thomas Münch
##' @references
##' Muench, T., et al., Constraints on post-depositional isotope modifications
##' in East Antarctic firn from analysing temporal changes of isotope profiles,
##' The Cryosphere, doi:10.5194/tc-11-2175-2017, 2017.
##' @export
DifferentialDiffusion <- function(dat = NULL, z00, z01, z10, z11,
                                  P = 650, T = 228.5, bdot = 64) {

    if (!requireNamespace("FirnR", quietly = TRUE)) {
        stop(paste("FirnR needed for this function to work. The package is",
                   "available on reasonable request from the author."),
             call. = FALSE)
    }
    
    if (is.null(dat)) {
        message(paste("DifferentialDiffusion: No specific input data supplied",
                      " -- using data of paper for caluclations."))
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

    s.tilde.oxy <- sqrt(s2^2 - s1^2)  ##~1.925 cm

    ## d2H
    sigma <- FirnR::DiffusionLength(dat$depth, dat$density,
                                    P = P, T = T, bdot = bdot, dD = TRUE)
    s1 <- mean(sigma[ix1])
    s2 <- mean(sigma[ix2])

    s.tilde.dtr <- sqrt(s2^2 - s1^2)  ##~1.750 cm

    return(list(oxy = s.tilde.oxy, dtr = s.tilde.dtr))

}
    
