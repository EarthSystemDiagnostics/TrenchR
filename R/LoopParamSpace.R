##' Loop over parameter space to find minimum RMSD.
##'
##' Description.
##'
##' Details.
##' @param TR 
##' @param advSpace 
##' @param sigmaSpace 
##' @param densfSpace 
##' @return value.
##' @author Thomas Münch
##' @export
LoopParamSpace <- function(TR = prepareTrenchData()$oxy,
                           advSpace = seq(40, 60, 0.5)/TR$HiRes,
                           sigmaSpace = seq(0, 8, 0.1),
                           densfSpace = seq(0, 10, 0.1)) {

    res <- TR$HiRes

    noNA <- which(!is.na(TR$mean13_HiRes))
    N <- length(noNA)

    depth <- TR$depth
    depth_HiRes <- TR$depth_HiRes[noNA]

    IN <- TR$mean13_HiRes[noNA]
    REF <- TR$mean15_HiRes

    RMSD <- array(dim = c(length(advSpace), length(sigmaSpace),
                          length(densfSpace)))

    for (k in advSpace){

        print(k * res)

        for (s in sigmaSpace){

            diff <- DiffuseRecord(IN, sigma = rep(s, length(IN)), res = res)

            for (d in densfSpace){

                stretch <- d

                depth.stretch <- seq(depth_HiRes[1], depth_HiRes[N] - stretch,
                                     length.out = N)
    
                diff.stretch <- TR$mean13_HiRes
                diff.stretch[noNA] <- approx(depth.stretch, diff, depth_HiRes)$y
                diff.stretch.adv <- Hmisc::Lag(diff.stretch, k)

                iX <- which(advSpace == k)
                iR <- which(sigmaSpace == s)
                iC <- which(densfSpace == d)

                RMSD[iX, iR, iC] <- rmsd(diff.stretch.adv, REF, na.rm = TRUE)

            }
        }
    }


    MIN <- which(RMSD == min(RMSD), arr.ind = TRUE)

    ## project onto optimal shift

    KOPT <- array(dim = c(length(sigmaSpace), length(densfSpace)))
    RMSD.opt <- array(dim = c(length(sigmaSpace), length(densfSpace)))
    for (iS in 1 : length(sigmaSpace)) {
        for (iD in 1 : length(densfSpace)) {

            iOPT <- which.min(RMSD[, iS, iD])
            KOPT[iS, iD] <- res * advSpace[iOPT]
            RMSD.opt[iS, iD] <- RMSD[iOPT, iS, iD]
        }
    }

    ## save data
    ParamSpace=list(
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
