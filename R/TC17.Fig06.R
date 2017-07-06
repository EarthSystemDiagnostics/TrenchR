##' Title
##'
##' Description
##' @param TR 
##' @param path 
##' @param file.name 
##' @param device 
##' @param dev.size 
##' @param save.plot
##' @param mod.param 
##' @author Thomas Münch
TC17.Fig06 <- function(TR, path = file.path(getwd(), "plots"),
                       file.name = "tc17_fig_06", device = "quartz",
                       dev.size = list(h = 6, w = 8), save.plot = FALSE,
                       mod.param) {

    plot.par <- SetPlotPar()

    if (device == "quartz") {
        quartzFonts(optima = c("Optima Regular", "Optima Bold",
                               "Optima Italic", "Optima Bold Italic"))
    }

    OpenDevice(device = device, path = path, file.name = file.name,
               height = dev.size$h, width = 2 * dev.size$w,
               save.plot = save.plot)
    par(plot.par)
    par(mfrow = c(1, 2))
    par(oma = c(5, 0, 0.5, 0), mar = c(0, 6, 0, 6))    
    if (device == "quartz") par(family = "optima")

    T13.mod.oxy <- ModifyT13(TR$oxy, mod.param)
    
    # color scale
    my.col <- c("dodgerblue", "#1b9e77", "#d95f02", "#7570b3")

    #---------------------------------------------------------------------------
    # Fig06-a

    # --------------------------------------------------------------------------
    # this part will be considerably simplified by making the function
    # 'ModifyT13' more general in a future version
    noNA <- which(!is.na(TR$oxy$mean13_HiRes))
    n <- length(noNA)

    depth.stretch <- seq(TR$oxy$depth_HiRes[noNA][1],
                         TR$oxy$depth_HiRes[noNA][n] -
                         mod.param$stretchOPT, length.out = n)
    mean13_HiRes.best.stretch <- approx(depth.stretch,
                                        TR$oxy$mean13_HiRes[noNA],
                                        TR$oxy$depth_HiRes[noNA])$y

    m.index <- match(TR$oxy$depth, TR$oxy$depth_HiRes[noNA])
    mean13_best.stretch <- mean13_HiRes.best.stretch[m.index]
    # --------------------------------------------------------------------------

    ind1 <- which(TR$oxy$depth <= TR$oxy$SRF.b$t13)
    ind2 <- which(TR$oxy$depth_HiRes[noNA] <= TR$oxy$SRF.b$t13)
    ind4 <- which(TR$oxy$depth <= TR$oxy$SRF.b$t13 +
                  ceiling(mod.param$ADVopt * TR$oxy$HiRes))
    v1 <- TR$oxy$mean13
    v2 <- mean13_HiRes.best.stretch
    v33 <- v3 <- DiffuseRecord(TR$oxy$mean13_HiRes[noNA],
                               sigma = rep(mod.param$SIGMAopt, n),
                               res = TR$oxy$HiRes)[m.index]
    v4 <- T13.mod.oxy$mean13.diff.stretch.adv_OPT

    p1 <- (p1 <- which(!is.na(v2)))[c(1, length(p1))]
    p2 <- (p2 <- which(!is.na(v3)))[c(1, length(p2))]
    p3 <- (p3 <- which(!is.na(v4)))[c(1, length(p3))]

    v1[ind1[-length(ind1)]]  <- NA
    v2[ind2[-length(ind2)]]  <- NA
    v33[ind1[-length(ind1)]] <- NA
    v4[ind4[-length(ind4)]]  <- NA

    plot(TR$oxy$depth[1:38], TR$oxy$mean13, type = "n", axes = FALSE,
         xlab = "", ylab = "", xlim = c(0, 125), ylim = c(-54, -40))

    lines(TR$oxy$depth[1:38], v1, lwd = 2.5, col = my.col[1])
    lines(TR$oxy$depth[ind1], TR$oxy$mean13[ind1],
          lwd = 1.5, lty = 5, col = my.col[1])

    lines(TR$oxy$depth_HiRes[noNA], v2, lwd = 1.5, col = "firebrick")
    lines(TR$oxy$depth_HiRes[noNA][ind2], mean13_HiRes.best.stretch[ind2],
          lwd = 1.5, lty = 5, col = "firebrick")
    points(TR$oxy$depth_HiRes[noNA][p1], mean13_HiRes.best.stretch[p1],
           col = "firebrick", pch = 24, lwd = 1.5, cex = 0.75)

    lines(TR$oxy$depth, v33, lwd = 1.5, col = "black")
    lines(TR$oxy$depth[ind2], v3[ind2], lwd = 1.5, lty = 5, col = "black")
    points(TR$oxy$depth[p2], v3[p2], col = "black",
           pch = 25, lwd = 1.5, cex = 0.75)

    par(yaxp = c(-48, -40, 4))
    axis(2)

    text(-27.5, -44,
         labels = expression(delta^bold("18") * bold("O") * bold(" (\u2030)")),
         srt = 90, xpd = NA, cex = plot.par$cex.lab, font = plot.par$font.lab,
         col = "black")

    legend("topleft", "T13 record", lwd = 2, col = "dodgerblue",
           lty = 1, cex = 1.1, text.font = 1, bty = "n")
    MyLegend("topright", c("T13 after 2-yr incremental diffusion",
                            "T13 after linear compression"),
              lwd = c(1.5, 1.5), col = c("black", "firebrick"),
              lty = 1, cex = 1.1, text.font = 1, bty = "n",
              pch = c(24, 25), end.pch = TRUE, pch.xoff = 0.2, inset = c(0.02, 0),
              pt.cex = 0.75, pt.lwd = 1.5)

    par(new = TRUE)

    plot(TR$oxy$depth[1:38], TR$oxy$mean13, type = "n", axes = FALSE,
         xlab = "", ylab = "", xlim = c(0, 125), ylim = c(-48, -34))

    lines(TR$oxy$depth[1:38], v1, lwd = 2.5, col = "dodgerblue")
    lines(TR$oxy$depth[ind1], TR$oxy$mean13[ind1],
          lwd = 1.5, lty = 5, col = "dodgerblue")

    lines(TR$oxy$depth - mod.param$ADVopt * TR$oxy$HiRes, v4, col = my.col[2])
    lines(TR$oxy$depth[ind4] - mod.param$ADVopt * TR$oxy$HiRes,
          T13.mod.oxy$mean13.diff.stretch.adv_OPT[ind4],
          lwd = 1.5, lty = 5, col = my.col[2])
    points(TR$oxy$depth[p3] - mod.param$ADVopt * TR$oxy$HiRes,
           T13.mod.oxy$mean13.diff.stretch.adv_OPT[p3],
           col = my.col[2], pch = 23, lwd = 1.5, cex = 0.75)

    par(yaxp = c(-48, -40, 4))
    axis(4)

    text(152.5,-44,
         labels = expression(delta^bold("18") * bold("O") * bold(" (\u2030)")),
         srt = -90, xpd = NA, cex = plot.par$cex.lab, font = plot.par$font.lab,
         col = "black")

    par(xaxp=c(0, 125, 5))
    axis(1)

    mtext("Depth (cm)", side = 1, line = 3.5,
          cex = plot.par$cex.lab, font = plot.par$font.lab)

    MyLegend("bottomright", "T13* record",
             lwd = 1.5, col = my.col[2], lty = 1, cex = 1.1, text.font = 1,
             bty = "n", pch = 23, end.pch = TRUE, pch.xoff = 0.2,
             inset = c(0.02, 0), pt.cex = 0.75, pt.lwd = 1.5)


    #---------------------------------------------------------------------------
    # Fig06-b

    ind1 <- which(TR$oxy$depth <= TR$oxy$SRF.b$t15)
    ind2 <- which(TR$oxy$depth <= TR$oxy$SRF.b$t13 +
                  ceiling(mod.param$ADVopt * TR$oxy$HiRes))
    ind3 <- ind2 # force consistency
    v1 <- TR$oxy$mean15
    v2 <- T13.mod.oxy$mean13.diff.stretch.adv_OPT
    v3 <- T13.mod.oxy$mean13.diff.stretch.adv
    p1 <- (p1 <- which(!is.na(v2)))[c(1, length(p1))]
    p2 <- (p2 <- which(!is.na(v3)))[c(1, length(p2))]
    v1[ind1[-length(ind1)]] <- NA
    v2[ind2[-length(ind2)]] <- NA
    v3[ind3[-length(ind3)]] <- NA

    sum.max <- which.peaks(TR$oxy$mean15)[3 : 6]

    plot(TR$oxy$depth_HiRes, TR$oxy$mean15_HiRes, type = "n",
         xlim = c(0, 175), ylim = c(-52, -36),
         axes = FALSE, xlab = "",ylab = "")

    for (i in 1 : length(sum.max))
        segments(x0 = TR$oxy$depth[sum.max[i]], y0 = -52 * 1.04,
                 y1 = ifelse(T13.mod.oxy$mean13.diff.stretch.adv[sum.max[i]] >
                             TR$oxy$mean15[sum.max[i]],
                             T13.mod.oxy$mean13.diff.stretch.adv[sum.max[i]],
                             TR$oxy$mean15[sum.max[i]]),
                 lty = 5, lwd = 1, col = "gray50")
    segments(x0 = 50, y0 = -48, x1 = 182, lty = 1, lwd = 1, col = "gray50")

    lines(TR$oxy$depth, v1)
    lines(TR$oxy$depth[ind1], TR$oxy$mean15[ind1],
          lwd = 1.5, lty = 5, col = "black")

    lines(TR$oxy$depth, v3, col = my.col[3])
    lines(TR$oxy$depth[ind3], T13.mod.oxy$mean13.diff.stretch.adv[ind3],
          lwd = 1.5, lty = 5, col = my.col[3])
    points(TR$oxy$depth[p2], T13.mod.oxy$mean13.diff.stretch.adv[p2],
           col = my.col[3], pch = 1, lwd = 1.5, cex = 1)

    lines(TR$oxy$depth, v2, col = my.col[2])
    lines(TR$oxy$depth[ind2], T13.mod.oxy$mean13.diff.stretch.adv_OPT[ind2],
          lwd = 1.5, lty = 5, col = my.col[2])
    points(TR$oxy$depth[p1], T13.mod.oxy$mean13.diff.stretch.adv_OPT[p1],
           col = my.col[2], pch = 23, lwd = 1.5, cex = 0.75)

    par(yaxp = c(-52, -36, 4))
    axis(2)
    MinorTick(nx = 1, ny = 2, side = 2)
    par(xaxp = c(0, 175, 7))
    axis(1)

    mtext("Depth (cm)", side = 1, line = 3.5,
          cex = plot.par$cex.lab, font = plot.par$font.lab)
    mtext(expression(delta^bold("18") * bold("O") * bold(" (\u2030)")),
          side = 2, line = 3.5, las = 0,
          cex = plot.par$cex.lab, font = plot.par$font.lab)

    MyLegend("topleft", c("T15", "T13* (opt. param.)", "T13** (ind. param.)"),
              lwd = 1.5, lty = 1, col = c("black", my.col[2], my.col[3]),
              pch = c(NA, 1, 23),
              cex = 1.1, text.font = 1, bty = "n", end.pch = TRUE, pch.xoff = 0.2,
              inset = c(0.02, 0), pt.cex = 0.75, pt.lwd = 1.5)

    par(new = TRUE)

    plot(TR$oxy$depth_HiRes, TR$oxy$mean15_HiRes, type = "n",
         xlim = c(0, 175), ylim = c(-4, 12), axes = FALSE, xlab = "", ylab = "")

    lines(TR$oxy$depth, v1 - v3, col = my.col[4])
    lines(TR$oxy$depth[ind3],
          TR$oxy$mean15[ind3] - T13.mod.oxy$mean13.diff.stretch.adv[ind3],
          lwd = 1.5, lty = 5, col = my.col[4])
    lines(TR$oxy$depth, TR$oxy$mean15 - T13.mod.oxy$mean13.adv_OPT,
          lwd = 1.5, lty = 3, col = "dimgrey")

    par(yaxp=c(-4, 4, 4))
    axis(4, col = my.col[4], col.axis = my.col[4])

    text(205, 0, "T15 - T13** (\u2030)",
         cex = plot.par$cex.lab, font = plot.par$font.lab,
         col = my.col[4], srt = -90, xpd = NA)

    if (save.plot) dev.off()

}
