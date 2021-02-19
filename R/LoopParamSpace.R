##' Loop over parameter space to find minimum RMSD.
##'
##' Loop over a given parameter space of advection, diffusion and densification
##' values to modify a given record accordingly, and return the overall minimum
##' root mean square deviation (RMSD) to a reference record.
##'
##' Note that all NA's are stripped from the input record before the computation
##' process. This is problematic for the diffusion calculation in case of a
##' large number of internal NA's.
##'
##' Note further that for computational efficiency, the implemented order of
##' modifications is (1) downward advection, (2) diffusion, and (3) compression
##' from densification. This is slightly unphysical since the diffusional
##' smoothing so acts over the uncompressed depth scale. However, it affects the
##' results only slightly in the domain of high diffusion lengths and high
##' compression values, and does not affect the main results discussed in Münch
##' et al. (2017).
##' @param rec.in numeric vector with the input record which is modified
##' according to the given parameter spaces of advection, diffusion and
##' densification.
##' @param reference numeric vector with a reference record against which the
##' root mean square deviation of the modified record is calculated; must be of
##' the same length as \code{rec.in}.
##' @param res the depth resolution of \code{rec.in} and \code{reference}.
##' @param depth the depth scale of the records; must be in the same units as
##' \code{res}.
##' @param advSpace numeric vector of downward advection values to loop over; in
##' the same units as \code{res}. Advection values can be zero or negative, but
##' the absolute non-zero values must be larger than \code{res}.
##' @param sigmaSpace numeric vector of differential diffusion length values to
##' loop over; in the same units as \code{res}.
##' @param densfSpace numeric vector of compression values from densification to
##' loop over; in the same units as \code{res}.
##' @return A list with nine components:
##' \describe{
##'   \item{adv:}{a copy of \code{advSpace};}
##'   \item{sigma:}{a copy of \code{sigmaSpace};}
##'   \item{densf:}{a copy of \code{densfSpace};}
##'   \item{adv.opt:}{the optimal advection value;}
##'   \item{sigma.opt:}{the optimal diffusion length;}
##'   \item{densf.opt:}{the optimal compression value;}
##'   \item{adv.opt.arr:}{an array of dimension \code{length(sigmaSpace)} x
##'     \code{length(densfSpace)} which contains the optimal advection values at
##'     which, for fixed compression and diffusion, the RMSD between the
##'     reference and the modified input record is minimal;}
##'   \item{RMSD:}{an array of dimension \code{length(advSpace)} x
##'     \code{length(sigmaSpace)} x \code{length(densfSpace)} which contains the
##'     RMSD value between the reference and the modified input record for every
##'     combination of downward advection, diffusion and compression;}
##'   \item{RMSD.opt:}{the array \code{RMSD} projected onto the optimal
##'     downward-advection value.}
##' }
##' @author Thomas Münch
##' @seealso \code{\link{DiffuseRecord}}; \code{\link{CompressRecord}};
##' \code{?ParamSpace}
##' @inherit Muench2017 references
##' @examples
##' # Run the analysis for Fig. (5) in Münch et al. (2017)
##' # (note that this takes some amount of computation time):
##' \donttest{
##' TR <- prepareTrenchData()$oxy
##' ParamSpace <- LoopParamSpace(rec.in = TR$mean13_HiRes,
##'                              reference = TR$mean15_HiRes,
##'                              res = TR$HiRes, depth = TR$depth_HiRes,
##'                              advSpace = seq(40, 60, 0.5),
##'                              sigmaSpace = seq(0, 8, 0.1),
##'                              densfSpace = seq(0, 10, 0.1))
##' }
##' @export
LoopParamSpace <- function(rec.in, reference, res, depth,
                           advSpace, sigmaSpace, densfSpace) {

    if (length(rec.in) != length(reference)) {
        stop("'rec.in' and 'reference' must be of the same length.")
    }

    if (length(i <- which(abs(advSpace) != 0))) {

      if (min(abs(advSpace[i])) < res) {
        stop("Advection values smaller than 'res' present.")
      }
    }
    
    advSpace <- advSpace / res

    noNA <- which(!is.na(rec.in))

    depth <- depth[noNA]
    IN <- rec.in[noNA]
    REF <- reference

    RMSD <- array(dim = c(length(advSpace), length(sigmaSpace),
                          length(densfSpace)))

    for (k in advSpace){

        print(k * res)

        for (s in sigmaSpace){

            # diffuse the input record
            diff <- DiffuseRecord(IN, sigma = rep(s, length(IN)), res = res)

            for (d in densfSpace){

                # compress the diffused record
                diff.stretch <- rec.in
                diff.stretch[noNA] <- CompressRecord(depth, diff,
                                                     stretch = d)$rec

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
