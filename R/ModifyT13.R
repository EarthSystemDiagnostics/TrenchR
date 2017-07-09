##' Modify the T13 mean isotope profile.
##'
##' Details
##' @param TR 
##' @param mod.param 
##' @return A list.
##' @author Thomas Münch
##' @export
ModifyT13 <- function(TR = prepareTrenchData(na.treat = TRUE)$oxy, mod.param) {

    
    # non-NA range of high-resolution trench T13 data;
    # needed for diffusion function
    noNA <- which(!is.na(TR$mean13_HiRes))
    n <- length(noNA)

    # T13 DENSIFICATION

    # depth scale according to compression (independent estimate)
    depth.stretch <- seq(TR$depth_HiRes[noNA][1],
                         TR$depth_HiRes[noNA][n] - mod.param$stretch,
                         length.out = n)
    # depth scale according to compression (optimal estimate)
    depth.stretchOPT <- seq(TR$depth_HiRes[noNA][1],
                            TR$depth_HiRes[noNA][n] - mod.param$stretchOPT,
                            length.out = n)

    # approximate record on compressed depth scales
    mean13_HiRes.stretch <- approx(depth.stretch, TR$mean13_HiRes[noNA],
                                   TR$depth_HiRes[noNA])$y
    mean13_HiRes.stretchOPT <- approx(depth.stretchOPT, TR$mean13_HiRes[noNA],
                                      TR$depth_HiRes[noNA])$y

    # T13 ADVECTED, DIFFUSED AND COMPRESSED

    tmp <- DiffuseRecord(TR$mean13_HiRes[noNA],
                         sigma = rep(mod.param$SIGMA, n),
                         res = TR$HiRes)

    mean13_HiRes.diff.stretch <- TR$mean13_HiRes
    mean13_HiRes.diff.stretch[noNA] <- approx(depth.stretch, tmp,
                                              TR$depth_HiRes[noNA])$y
    mean13_HiRes.diff.stretch.adv <- Hmisc::Lag(mean13_HiRes.diff.stretch,
                                                shift = mod.param$ADV)

    tmp <- DiffuseRecord(TR$mean13_HiRes[noNA],
                         sigma = rep(mod.param$SIGMAopt, n),
                         res = TR$HiRes)

    mean13_HiRes.diff.stretch_OPT <- TR$mean13_HiRes
    mean13_HiRes.diff.stretch_OPT[noNA] <- approx(depth.stretchOPT, tmp,
                                                  TR$depth_HiRes[noNA])$y
    mean13_HiRes.diff.stretch.adv_OPT <-
        Hmisc::Lag(mean13_HiRes.diff.stretch_OPT, shift = mod.param$ADVopt)


    # T13 ONLY OPTIMALLY ADVECTED
    adv.only.opt <- 97
    mean13_HiRes.adv_OPT <- Hmisc::Lag(TR$mean13_HiRes, adv.only.opt)

    # EXTRACT 3CM RESOLUTION
    mean13.diff.stretch.adv <-
        mean13_HiRes.diff.stretch.adv[match(TR$depth, TR$depth_HiRes)]
    mean13.diff.stretch.adv_OPT <- 
        mean13_HiRes.diff.stretch.adv_OPT[match(TR$depth, TR$depth_HiRes)]
    mean13.adv_OPT <- mean13_HiRes.adv_OPT[match(TR$depth, TR$depth_HiRes)]

    return(list(
        mean13_HiRes.diff.stretch.adv = mean13_HiRes.diff.stretch.adv,
        mean13_HiRes.diff.stretch.adv_OPT = mean13_HiRes.diff.stretch.adv_OPT,
        mean13_HiRes.adv_OPT = mean13_HiRes.adv_OPT,
        mean13.diff.stretch.adv = mean13.diff.stretch.adv,
        mean13.diff.stretch.adv_OPT = mean13.diff.stretch.adv_OPT,
        mean13.adv_OPT = mean13.adv_OPT
    ))
        
}
