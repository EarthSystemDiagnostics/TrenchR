##' Calculate densification rate.
##'
##' .. content for \details{} ..
##' @param dat 
##' @param lim 
##' @return 
##' @author Thomas Münch
##' @references
##' Muench, T., et al., Constraints on post-depositional isotope modifications
##' in East Antarctic firn from analysing temporal changes of isotope profiles,
##' The Cryosphere, doi:10.5194/tc-11-2175-2017, 2017.
##' @export
DensificationRate <- function(dat = NULL, lim) {

    if (is.null(dat)) {
        message(paste("DensificationRate: No specific input data supplied --",
                      "using data of paper for caluclations."))
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


