##' Loop over parameter space to find minimum RMSD.
##'
##' Loop over a given parameter space of advection, diffusion and densification
##' values to modify a given record accordingly, and return the overall minimum
##' root mean square deviation (RMSD) to a reference record.
##'
##' Note that for computational efficiency, the implemented order of
##' modifications is (1) downward advection, (2) diffusion, and (3) compression
##' from densification. This is slightly unphysical since the diffusional
##' smoothing acts over the uncompressed depth scale. However, it affects the
##' results only slightly in the domain of high diffusion lengths and high
##' compression values, and does not affect the main results discussed in Münch
##' et al. (2017).
##' @param TR tbd
##' @param advSpace tbd
##' @param sigmaSpace tbd
##' @param densfSpace tbd
##' @return A list with nine components:
##' \describe{
##'   \item{adv:}{a copy of \code{advSpace};}
##'   \item{sigma:}{a copy of \code{sigmaSpace};}
##'   \item{densf:}{a copy of \code{densfSpace};}
##'   \item{adv.opt:}{the optimal advection value;}
##'   \item{sigma.opt:}{the optimal diffusion length;}
##'   \item{densf.opt:}{the optimal compression value;}
##'   \item{adv.opt.arr:}{an array of dimension \code{length(sigma)} x
##'     \code{length(densf)} which contains the optimal advection values at
##'     which, for fixed compression and diffusion, the RMSD between the
##'     reference and the modified input record is minimal;}
##'   \item{RMSD:}{an array of dimension \code{length(adv)} x
##'     \code{length(sigma)} x \code{length(densf)} which contains the RMSD value
##'     between the reference and the modified input record for every
##'     combination of downward advection, diffusion and compression;}
##'   \item{RMSD.opt:}{the array \code{RMSD} projected onto the optimal
##'     downward-advection value.}
##' }
##' @author Thomas Münch
##' @seealso \code{\link{DiffuseRecord}}; \code{\link{CompressRecord}};
##' \code{?ParamSpace}
##' @references
##' Münch, T., et al., Constraints on post-depositional isotope modifications
##' in East Antarctic firn from analysing temporal changes of isotope profiles,
##' The Cryosphere, doi:10.5194/tc-11-2175-2017, 2017.
##' @export
LoopParamSpace <- function(TR = prepareTrenchData()$oxy,
                           advSpace = seq(40, 60, 0.5)/TR$HiRes,
                           sigmaSpace = seq(0, 8, 0.1),
                           densfSpace = seq(0, 10, 0.1)) {

    res <- TR$HiRes

    noNA <- which(!is.na(TR$mean13_HiRes))

    depth <- TR$depth
    depth_HiRes <- TR$depth_HiRes[noNA]

    IN <- TR$mean13_HiRes[noNA]
    REF <- TR$mean15_HiRes

    RMSD <- array(dim = c(length(advSpace), length(sigmaSpace),
                          length(densfSpace)))

    for (k in advSpace){

        print(k * res)

        for (s in sigmaSpace){

            # diffuse the input record
            diff <- DiffuseRecord(IN, sigma = rep(s, length(IN)), res = res)

            for (d in densfSpace){

                # compress the diffused record
                diff.stretch <- TR$mean13_HiRes
                diff.stretch[noNA] <- CompressRecord(depth_HiRes,
                                                     diff, stretch = d)$rec

                # advect the diffused and stretched record
                diff.stretch.adv <- Hmisc::Lag(diff.stretch, k)

                iX <- which(advSpace == k)
                iR <- which(sigmaSpace == s)
                iC <- which(densfSpace == d)

                RMSD[iX, iR, iC] <- rmsd(diff.stretch.adv, REF, na.rm = TRUE)

            }
        }
    }


    MIN <- which(RMSD == min(RMSD), arr.ind = TRUE)

    # project onto optimal shift

    KOPT <- array(dim = c(length(sigmaSpace), length(densfSpace)))
    RMSD.opt <- array(dim = c(length(sigmaSpace), length(densfSpace)))
    for (iS in 1 : length(sigmaSpace)) {
        for (iD in 1 : length(densfSpace)) {

            iOPT <- which.min(RMSD[, iS, iD])
            KOPT[iS, iD] <- res * advSpace[iOPT]
            RMSD.opt[iS, iD] <- RMSD[iOPT, iS, iD]
        }
    }

    # save data
    ParamSpace = list(
        adv = advSpace,
        sigma = sigmaSpace,
        densf = densfSpace,
        adv.opt = advSpace[MIN[1]] * res,
        sigma.opt = sigmaSpace[MIN[2]],
        densf.opt = densfSpace[MIN[3]],
        adv.opt.arr = KOPT,
        RMSD = RMSD,
        RMSD.opt = RMSD.opt)

    return(ParamSpace)
}
