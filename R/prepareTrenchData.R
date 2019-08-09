##' Process T13 and T15 Trench Data.
##'
##' This function processes the T13 and T15 trench isotopologue data in a way
##' that is suited for analysis and plotting used in Münch et al. (2017).
##' @param index.range Vector of depth indices to subset the T15 trench dataset;
##' defaults to the range used in Münch et al. (2017). Set to \code{NULL} to
##' extract the full depth of the data.
##' @param LoRes Depth resolution of trench dataset in cm; defaults to the
##' original T13 and T15 sampling resolution (3 cm).
##' @param HiRes Higher depth resolution to interpolate trench data onto;
##' defaults to 0.5 cm as used in Münch et al. (2017).
##' @param k13 Vertical shift [cm] of the mean T13-2 relative to the mean T13-1
##' profile; defaults to the optimal shift that maximizes the profile
##' correlation.
##' @param k15 Vertical shift [cm] of the mean T15-2 relative to the mean T15-1
##' profile; defaults to the optimal shift that maximizes the profile
##' correlation.
##' @param na.treat if \code{TRUE}, include the incomplete surface region of the
##' trenches when calculating the mean profiles; defaults to \code{FALSE}.
##' @return A list of three sub-lists corresponding to the three measured
##'     isotopologue data (index i = 1: d18O, i = 2: d2H, i = 3: d-excess). Each
##'     sub-list has the following components:
##'     \itemize{
##'     \item trench13.1: Data of isotope species \code{i} for trench T13-1.
##'     \item trench13.2: Data of isotope species \code{i} for trench T13-2.
##'     \item trench15.1: Data of isotope species \code{i} for trench T15-1
##'           for the  depth range specified by \code{index.range}.
##'     \item trench15.2: Data of isotope species \code{i} for trench T15-2
##'           for the depth range specified by \code{index.range}.
##'     \item mean13.1: Mean isotope profile \code{i} of trench T13-1.
##'     \item mean13.2: Mean isotope profile \code{i} of trench T13-2.
##'     \item mean15.1: Mean isotope profile \code{i} of trench T15-1.
##'     \item mean15.2: Mean isotope profile \code{i} of trench T15-2.
##'     \item trench13.1_HiRes: \code{trench13.1} linearly interpolated onto the
##'           higher resolution specified by \code{HiRes}.
##'     \item trench13.2_HiRes: Same for \code{trench13.2}.
##'     \item trench15.1_HiRes: Same for \code{trench15.1}.
##'     \item trench15.2_HiRes: Same for \code{trench15.2}.
##'     \item mean13.1_HiRes: Same for \code{mean13.1}.
##'     \item mean13.2_HiRes: Same for \code{mean13.2}.
##'     \item mean15.1_HiRes: Same for \code{mean15.1}.
##'     \item mean15.2_HiRes: Same for \code{mean15.2}.
##'     \item mean13: Total mean isotope profile \code{i} of trench T13,
##'           accounting for the vertical shift specified by \code{k13}.
##'     \item mean13_HiRes: \code{mean13} linearly interpolated onto the
##'           higher resolution specified by \code{HiRes}.
##'     \item mean15: Total mean isotope profile \code{i} of trench T15.
##'     \item mean15_HiRes: \code{mean15} linearly interpolated onto the
##'           higher resolution specified by \code{HiRes} and accounting for the
##'           vertical shift specified by \code{k15}.
##'     \item k13: Copy of input \code{k13}.
##'     \item k15: Copy of input \code{k15}.
##'     \item LoRes: Copy of input \code{LoRes}.
##'     \item HiRes: Copy of input \code{HiRes}.
##'     \item depth: Numeric vector of sample depths on their original
##'           resolution (specified by \code{LoRes}) for the T15 index range
##'           \code{index.range}. 
##'     \item depth_HiRes: Numeric vector of sample depths corresponding to the
##'           linearly interpolated data on the higher resolution \code{HiRes}.
##'     \item XPOS: Horizontal profile positions in [m] of the T15-1 and T15-2
##'           profiles relative to the trench starting point (= profile #1).
##'     \item XPOS.t13.1: Horizontal profile positions in [m] of the T13-1
##'           profiles relative to the trench starting point.
##'     \item XPOS.t13.2: Horizontal profile positions in [m] of the T13-2
##'           profiles relative to the trench starting point.
##'     \item SPRF.t1: List: Horizontal position (component \code{x}) and
##'           corresponding surface height in [cm] (component \code{y})
##'           relative to the highest observed surface height for trench
##'           T15-1 (positive downwards). This set includes the additional
##'           profile at position 26.5 m for which no isotope profile is
##'           present.
##'     \item SPRF.t2: List: Horizontal position (component \code{x}) and
##'           corresponding surface height in [cm] (component \code{y})
##'           relative to the highest observed surface height for trench
##'           T15-2 (positive downwards).
##'     \item SRF.b: A list with six elements of the following quantities (in
##'           that order): starting depths of horizontally complete datasets for
##'           T13-1; T13-2; T15-1; T15-2; total T13 (accounting for vertical
##'           shift of T13-2); and total T15.
##' }
##' @author Thomas Münch
##' @inherit Muench2017 references
prepareTrenchData <- function(index.range = 1 : 59, LoRes = 3, HiRes = 0.5,
                              k13 = 3, k15 = -0.5, na.treat = FALSE) {

    # extract full T15 record if desired
    if (is.null(index.range)) {
        index.range <- seq(1, length(t15.trench1$depth))
    }

    # horizontal profile positions and surface height profile
    XPOS <- t15.trench1$meta$profilePos[-7]
    SPRF.t1 <- list(x = t15.trench1$meta$profilePos,
                    y = t15.trench1$meta$profileSurfaceHeight)
    SPRF.t2 <- list(x = t15.trench2$meta$profilePos,
                    y = t15.trench2$meta$profileSurfaceHeight)
    XPOS.t13.1 <- t13.trench1$meta$profilePos/100.
    XPOS.t13.2 <- t13.trench2$meta$profilePos/100.
    
    # sample depths
    depth <- t15.trench1$depth[index.range]

    # KS 14/15 trenches
    t15.1 <- t15.trench1$data[, index.range, ]
    t15.2 <- t15.trench2$data[, index.range, ]
    
    # KS 12/13 trenches
    t13.1 <- t13.trench1$data[, , -1]
    t13.2 <- t13.trench2$data[, , ]
                            
    # bottom depths of trench surface regions
    SRF.b <- list()
    k1 <- which(apply(t13.1[1, , ], 1, function(row) {all(!is.na(row))}))[1]
    SRF.b$t13.1 <- depth[k1]

    k2 <- which(apply(t13.2[1, , ], 1, function(row) {all(!is.na(row))}))[1]
    SRF.b$t13.2 <- depth[k2]

    k3 <- which(apply(t15.1[1, , ], 1, function(row) {all(!is.na(row))}))[1]
    SRF.b$t15.1 <- depth[k3]

    k4 <- which(apply(t15.2[1, , ], 1, function(row) {all(!is.na(row))}))[1]
    SRF.b$t15.2 <- depth[k4]

    k <- max(k1, k2) + k13/LoRes
    SRF.b$t13 <- depth[k]

    k <- max(k3, k4)
    SRF.b$t15 <- depth[k]


    # loop over isotope types
    res <- list()
    res$dxs <- res$dtr <- res$oxy <- list()
    for (i.proxy in 1 : 3) {

        trench13.1 <- t13.1[i.proxy, , ]
        trench13.2 <- t13.2[i.proxy, , ]
        
        trench15.1 <- t15.1[i.proxy, , ]
        trench15.2 <- t15.2[i.proxy, , ]
        
        # mean profiles
        mean15.1 <- rowMeans(trench15.1, na.rm = na.treat)
        mean15.2 <- rowMeans(trench15.2, na.rm = na.treat)
        mean13.1 <- rowMeans(trench13.1, na.rm = na.treat)
        mean13.2 <- rowMeans(trench13.2, na.rm = na.treat)
        
        # high-resolution data
        
        depth_HiRes <- seq(min(depth), max(depth), HiRes)

        trench15.1_HiRes <- apply(trench15.1, 2, function(prf) {
            approx(depth, prf, depth_HiRes)$y})
        trench15.2_HiRes <- apply(trench15.2, 2, function(prf) {
            approx(depth, prf, depth_HiRes)$y})
        
        trench13.1_HiRes <- apply(trench13.1, 2, function(prf) {
            approx(depth[1:38], prf, depth_HiRes)$y})
        trench13.2_HiRes <- apply(trench13.2, 2, function(prf) {
            approx(depth[1:38], prf, depth_HiRes)$y})

        mean15.1_HiRes <- approx(depth, mean15.1, depth_HiRes)$y
        mean15.2_HiRes <- approx(depth, mean15.2, depth_HiRes)$y
        mean13.1_HiRes <- approx(depth[1:38], mean13.1, depth_HiRes)$y
        mean13.2_HiRes <- approx(depth[1:38], mean13.2, depth_HiRes)$y


        
        # mean profiles for each season
        
        mean15_HiRes <- rowMeans(cbind(mean15.1_HiRes,
                                       Hmisc::Lag(mean15.2_HiRes,
                                                  shift = k15 / HiRes)))
        mean15 <- approx(depth_HiRes, mean15_HiRes, depth)$y

        mean13_HiRes <- rowMeans(cbind(mean13.1_HiRes,
                                       Hmisc::Lag(mean13.2_HiRes,
                                                  shift = k13 / HiRes)))
        mean13 <- rowMeans(cbind(mean13.1,
                                 Hmisc::Lag(mean13.2, shift = k13 / LoRes)))


        res[[i.proxy]]$trench13.1 <- trench13.1
        res[[i.proxy]]$trench13.2 <- trench13.2
        res[[i.proxy]]$trench15.1 <- trench15.1
        res[[i.proxy]]$trench15.2 <- trench15.2
        
        res[[i.proxy]]$mean13.1 <- mean13.1
        res[[i.proxy]]$mean13.2 <- mean13.2
        res[[i.proxy]]$mean15.1 <- mean15.1
        res[[i.proxy]]$mean15.2 <- mean15.2

        res[[i.proxy]]$trench13.1_HiRes <- trench13.1_HiRes
        res[[i.proxy]]$trench13.2_HiRes <- trench13.2_HiRes
        res[[i.proxy]]$trench15.1_HiRes <- trench15.1_HiRes
        res[[i.proxy]]$trench15.2_HiRes <- trench15.2_HiRes
        
        res[[i.proxy]]$mean13.1_HiRes <- mean13.1_HiRes
        res[[i.proxy]]$mean13.2_HiRes <- mean13.2_HiRes
        res[[i.proxy]]$mean15.1_HiRes <- mean15.1_HiRes
        res[[i.proxy]]$mean15.2_HiRes <- mean15.2_HiRes

        res[[i.proxy]]$mean13 <- mean13
        res[[i.proxy]]$mean13_HiRes <- mean13_HiRes
        res[[i.proxy]]$mean15 <- mean15
        res[[i.proxy]]$mean15_HiRes <- mean15_HiRes

        res[[i.proxy]]$k13 <- k13
        res[[i.proxy]]$k15 <- k15
        res[[i.proxy]]$LoRes <- LoRes
        res[[i.proxy]]$HiRes <- HiRes

        res[[i.proxy]]$depth <- depth
        res[[i.proxy]]$depth_HiRes <- depth_HiRes
        res[[i.proxy]]$XPOS <- XPOS
        res[[i.proxy]]$XPOS.t13.1 <- XPOS.t13.1
        res[[i.proxy]]$XPOS.t13.2 <- XPOS.t13.2

        res[[i.proxy]]$SPRF.t1 <- SPRF.t1
        res[[i.proxy]]$SPRF.t2 <- SPRF.t2

        res[[i.proxy]]$SRF.b <- SRF.b

    }

    return(res)
    
}    


