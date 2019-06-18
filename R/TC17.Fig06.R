##' Produce TC17 Figure 06.
##'
##' This function makes all necessary calculations and plots the results for
##' Figure 06 shown in Münch et al. (2017).
##' @param mod.param list of the parameters according to which the original
##' T13 mean profile is modified to model the temporal changes. It must containt
##' the following elements: \code{ADV.ind}, \code{ADV.opt}, \code{ADV.only},
##' \code{SIGMA.ind}, \code{SIGMA.opt}, \code{STRETCH.ind}, and
##' \code{STRETCH.opt} (see also \code{\link{SetModificationPar}}). If
##' \code{NULL} (the default), the original data from Münch et al. (2017) is
##' used for plotting.
##' @author Thomas Münch
##' @references
##' Münch, T., et al., Constraints on post-depositional isotope modifications
##' in East Antarctic firn from analysing temporal changes of isotope profiles,
##' The Cryosphere, doi:10.5194/tc-11-2175-2017, 2017.
##' @seealso \code{\link{SetModificationPar}}
##' @export
TC17.Fig06 <- function(mod.param = NULL) {

    if (is.null(mod.param)) {
        mod.param <- SetModificationPar()
    }

    TR = prepareTrenchData(na.treat = TRUE)$oxy

    pars <- SetPlotPar(oma = c(5, 0, 0.5, 0), mar = c(0, 6, 0, 6),
                       mfrow = c(1, 2))
    op <- par(pars)

    # color scale
    my.col <- c("dodgerblue", "#1b9e77", "#d95f02", "#7570b3")

    # T13* and T13**
    T13.star     <- ModifyT13(TR,
                              SIGMA = mod.param$SIGMA.opt,
                              STRETCH = mod.param$STRETCH.opt,
                              ADV = mod.param$ADV.opt)
    T13.starstar <- ModifyT13(TR,
                              SIGMA = mod.param$SIGMA.ind,
                              STRETCH = mod.param$STRETCH.ind,
                              ADV = mod.param$ADV.ind)


    #---------------------------------------------------------------------------
    # Fig06-a

    # auxiliary variables
    v11 <- v1 <- TR$mean13
    v22 <- v2 <- ModifyT13(STRETCH = mod.param$STRETCH.opt)$HiRes
    v33 <- v3 <- ModifyT13(SIGMA = mod.param$SIGMA.opt)$LoRes
    v44 <- v4 <- T13.star$LoRes

    p1 <- (p1 <- which(!is.na(v2)))[c(1, length(p1))]
    p2 <- (p2 <- which(!is.na(v3)))[c(1, length(p2))]
    p3 <- (p3 <- which(!is.na(v4)))[c(1, length(p3))]

    # define surface region
    ind1 <- which(TR$depth <= TR$SRF.b$t13)
    ind2 <- which(TR$depth_HiRes <= TR$SRF.b$t13)
    ind4 <- which(TR$depth <= TR$SRF.b$t13 +
                  ceiling(mod.param$ADV.opt))

    # profiles w/o surface region
    v11[ind1[-length(ind1)]]  <- NA
    v22[ind2[-length(ind2)]]  <- NA
    v33[ind1[-length(ind1)]] <- NA
    v44[ind4[-length(ind4)]]  <- NA

    plot(TR$depth[1:38], v1, type = "n", axes = FALSE,
         xlab = "", ylab = "", xlim = c(0, 125), ylim = c(-54, -40))

    lines(TR$depth[1:38], v11, lwd = 2.5, col = my.col[1])
    lines(TR$depth[ind1], v1[ind1],
          lwd = 1.5, lty = 5, col = my.col[1])

    lines(TR$depth_HiRes, v22, lwd = 1.5, col = "firebrick")
    lines(TR$depth_HiRes[ind2], v2[ind2],
          lwd = 1.5, lty = 5, col = "firebrick")
    points(TR$depth_HiRes[p1], v2[p1],
           col = "firebrick", pch = 25, lwd = 1.5, cex = 0.75)

    lines(TR$depth, v33, lwd = 1.5, col = "black")
    lines(TR$depth[ind2], v3[ind2], lwd = 1.5, lty = 5, col = "black")
    points(TR$depth[p2], v3[p2], col = "black",
           pch = 24, lwd = 1.5, cex = 0.75)

    par(yaxp = c(-48, -40, 4))
    axis(2)

    text(-27.5, -44,
         labels = expression(delta^bold("18") * bold("O") * bold(" (\u2030)")),
         srt = 90, xpd = NA, cex = pars$cex.lab, font = pars$font.lab,
         col = "black")

    legend("topleft", "T13 record", lwd = 2, col = my.col[1],
           lty = 1, cex = 1.1, text.font = 1, bty = "n")
    MyLegend("topright", c("T13 after 2-yr incremental diffusion",
                            "T13 after linear compression"),
              lwd = c(1.5, 1.5), col = c("black", "firebrick"),
              lty = 1, cex = 1.1, text.font = 1, bty = "n",
              pch = c(24, 25), end.pch = TRUE, pch.xoff = 0.2, inset = c(0.02, 0),
              pt.cex = 0.75, pt.lwd = 1.5)

    par(new = TRUE)

    plot(TR$depth[1:38], v1, type = "n", axes = FALSE,
         xlab = "", ylab = "", xlim = c(0, 125), ylim = c(-48, -34))

    lines(TR$depth[1:38], v11, lwd = 2.5, col = my.col[1])
    lines(TR$depth[ind1], v1[ind1],
          lwd = 1.5, lty = 5, col = my.col[1])

    lines(TR$depth - mod.param$ADV.opt, v44, col = my.col[2])
    lines(TR$depth[ind4] - mod.param$ADV.opt,
          v4[ind4], lwd = 1.5, lty = 5, col = my.col[2])
    points(TR$depth[p3] - mod.param$ADV.opt,
           v4[p3], col = my.col[2], pch = 23, lwd = 1.5, cex = 0.75)

    par(yaxp = c(-48, -40, 4))
    axis(4)

    text(152.5,-44,
         labels = expression(delta^bold("18") * bold("O") * bold(" (\u2030)")),
         srt = -90, xpd = NA, cex = pars$cex.lab, font = pars$font.lab,
         col = "black")

    par(xaxp=c(0, 125, 5))
    axis(1)

    mtext("Depth (cm)", side = 1, line = 3.5,
          cex = pars$cex.lab, font = pars$font.lab)

    MyLegend("bottomright", "T13* record",
             lwd = 1.5, col = my.col[2], lty = 1, cex = 1.1, text.font = 1,
             bty = "n", pch = 23, end.pch = TRUE, pch.xoff = 0.2,
             inset = c(0.02, 0), pt.cex = 0.75, pt.lwd = 1.5)


    #---------------------------------------------------------------------------
    # Fig06-b

    # auxiliary variables
    v11 <- v1 <- TR$mean15
    v22 <- v2 <- T13.star$LoRes
    v33 <- v3 <- T13.starstar$LoRes
    # only optimal advection
    v4 <- ModifyT13(ADV = mod.param$ADV.only)$LoRes

    p1 <- (p1 <- which(!is.na(v2)))[c(1, length(p1))]
    p2 <- (p2 <- which(!is.na(v3)))[c(1, length(p2))]

    # define surface region
    ind1 <- which(TR$depth <= TR$SRF.b$t15)
    ind2 <- which(TR$depth <= TR$SRF.b$t13 +
                  ceiling(mod.param$ADV.opt))
    ind3 <- ind2 # force consistency

    # profiles w/o surface region
    v11[ind1[-length(ind1)]] <- NA
    v22[ind2[-length(ind2)]] <- NA
    v33[ind3[-length(ind3)]] <- NA

    sum.max <- which.peaks(v11, partial = TRUE)[3 : 6]

    plot(TR$depth, v1, type = "n",
         xlim = c(0, 175), ylim = c(-52, -36),
         axes = FALSE, xlab = "", ylab = "")

    for (i in 1 : length(sum.max))
        segments(x0 = TR$depth[sum.max[i]], y0 = -52 * 1.04,
                 y1 = ifelse(v3[sum.max[i]] > v1[sum.max[i]],
                             v3[sum.max[i]], v1[sum.max[i]]),
                 lty = 5, lwd = 1, col = "gray50")
    segments(x0 = 50, y0 = -48, x1 = 182, lty = 1, lwd = 1, col = "gray50")

    lines(TR$depth, v11)
    lines(TR$depth[ind1], v1[ind1],
          lwd = 1.5, lty = 5, col = "black")

    lines(TR$depth, v33, col = my.col[3])
    lines(TR$depth[ind3], v3[ind3], lwd = 1.5, lty = 5, col = my.col[3])
    points(TR$depth[p2], v3[p2], lwd = 1.5, cex = 1, pch = 23,
           col = my.col[3])

    lines(TR$depth, v22, col = my.col[2])
    lines(TR$depth[ind2], v2[ind2],
          lwd = 1.5, lty = 5, col = my.col[2])
    points(TR$depth[p1], v2[p1], lwd = 1.5, cex = 0.75, pch = 1,
           col = my.col[2])

    par(yaxp = c(-52, -36, 4))
    axis(2)
    MinorTick(nx = 1, ny = 2, side = 2)
    par(xaxp = c(0, 175, 7))
    axis(1)

    mtext("Depth (cm)", side = 1, line = 3.5,
          cex = pars$cex.lab, font = pars$font.lab)
    mtext(expression(delta^bold("18") * bold("O") * bold(" (\u2030)")),
          side = 2, line = 3.5, las = 0,
          cex = pars$cex.lab, font = pars$font.lab)

    MyLegend("topleft", c("T15", "T13* (opt. param.)", "T13** (ind. param.)"),
              lwd = 1.5, lty = 1, col = c("black", my.col[2], my.col[3]),
              pch = c(NA, 1, 23),
              cex = 1.1, text.font = 1, bty = "n", end.pch = TRUE, pch.xoff = 0.2,
              inset = c(0.02, 0), pt.cex = 0.75, pt.lwd = 1.5)

    par(new = TRUE)

    plot(TR$depth, v1, type = "n",
         xlim = c(0, 175), ylim = c(-4, 12),
         axes = FALSE, xlab = "", ylab = "")

    lines(TR$depth, v11 - v33, col = my.col[4])
    lines(TR$depth[ind3], v1[ind3] - v3[ind3],
          lwd = 1.5, lty = 5, col = my.col[4])
    lines(TR$depth, v1 - v4,
          lwd = 1.5, lty = 3, col = "dimgrey")

    par(yaxp=c(-4, 4, 4))
    axis(4, col = my.col[4], col.axis = my.col[4])

    text(205, 0, "T15 - T13** (\u2030)",
         cex = pars$cex.lab, font = pars$font.lab,
         col = my.col[4], srt = -90, xpd = NA)

    par(op)

}
