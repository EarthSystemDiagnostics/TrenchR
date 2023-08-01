##' Produce TC17 Figure 01.
##'
##' This function makes all necessary calculations and plots the results for
##' Figure 01 shown in Münch et al. (2017).
##' @param cheat Originally, the annual mean isotope data have been erroneously
##' calculated such that the last value contributing to annual bin \code{i} was
##' also included in bin \code{i + 1}, due to an erroneous implementation within
##' the bin averaging function. This bug has been fixed. For \code{cheat =
##' TRUE}, the original paper figure with the erroneous bin definition can be
##' reproduced, while for \code{cheat = FALSE}, the correct implementation is
##' used. The difference in the annual mean data between the two figure versions
##' is, however, minor, and it thus does not influence any results or
##' conclusions of Münch et al. (2017).
##' @author Thomas Münch
##' @inherit Muench2017 references
TC17.Fig01 <- function(cheat = TRUE) {

    TR <- prepareTrenchData()$oxy
    T13.annual <- T13AnnualMeans(t1 = TR$mean13.1,
                                 t2 = Hmisc::Lag(TR$mean13.2,
                                                 TR$k13 / TR$LoRes),
                                 depth = TR$depth,
                                 cheat = cheat)

    op <- grfxtools::Par(mar = c(5, 5, 4, 2), mfrow = c(1, 2),
                         lwd = 2, font.lab = 2, font.axis = 2)


    #---------------------------------------------------------------------------
    # Fig01-a

    plot(TR$depth[1 : 38], TR$mean12.1, type = 'n',
         xlim = c(0, 125), ylim = c(-49, -39), axes = FALSE,
         xlab = "", ylab = "")
    axis(1)
    axis(2, las = 1)
    box()
    axis(3, labels = T13.annual$summer.max$years,
         at = T13.annual$summer.max$depth,
         cex.axis = 0.75 * par()$cex.lab)
    mtext("Depth (cm)", side = 1, line = 3.5, cex = par()$cex.lab,
          font = par()$font.lab)
    mtext(expression(bold("Trench ") *
                     delta^bold("18") * bold("O") * bold(" (\u2030)")),
          side = 2, line = 3.25, las = 0,
          cex = par()$cex.lab, font = par()$font.lab)
    mtext("Year", side = 3, line = 2.75,
          cex = par()$cex.lab, font = par()$font.lab)

    abline(v = T13.annual$summer.max$depth,
           lty = 5, lwd = 1.5, col = "darkgrey")

    lines(TR$depth[1 : 38], TR$mean13.1, lty = 1, col = "black")
    lines(TR$depth[1 : 38],
          Hmisc::Lag(TR$mean13.2, TR$k13 / TR$LoRes),
          col = "firebrick3")

    lines(T13.annual$means$depth, T13.annual$means$T1,
          type = 'b', lty = 1, pch = 19, col = "black")
    lines(T13.annual$means$depth, T13.annual$means$T2,
          type = 'b', lty = 1, pch = 19, col = "firebrick3")

    grfxtools::Polyplot(T13.annual$means$depth,
                        T13.annual$means$max.T1, T13.annual$means$min.T1,
                        col = "black", alpha = 0.15)
    grfxtools::Polyplot(T13.annual$means$depth,
                        T13.annual$means$max.T2, T13.annual$means$min.T2,
                        col = "firebrick3", alpha = 0.15)

    legend("topright",
           legend = c("T13-1", "T13-2"),
           col = c(1, "firebrick3"),
           cex = 1.25, text.font = 2, text.col = c(1, "firebrick3"), bty = "n")
    legend("bottomright", c("3 cm resolution", "Annual mean"),
           col = "darkgrey", lty = c(1, NA), pch = c(NA, 19), cex = 1.25,
           text.font = 2, bty = "n")


    #---------------------------------------------------------------------------
    # Fig01-b

    xlim <- as.POSIXct(c("2013-03-01 00:00:00","2008-01-01 00:00:00"),
                       tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    y.lines <- as.POSIXct(paste(c("2013-01-01", "2012-01-01", "2011-01-01",
                                  "2010-01-01", "2009-01-01","2008-01-01"),
                                "00:00:00"),
                          tz = "UTC", format = "%Y-%m-%d %H:%M:%S")

    plot(aws9$monthly$dates.posix, aws9$monthly$t2m, type = "l", axes = FALSE,
         xlim = xlim, ylim = c(-60, -20), xlab = "", ylab = "")
    lines(aws9$annual$dates.posix, aws9$annual$t2m,
          type="b",lty=1,pch=19,col="dodgerblue")

    abline(v = y.lines, lty = 5, lwd = 1.5, col = "darkgrey")
    axis(1, at = y.lines,
         labels = c("2013", "2012", "2011", "2010", "2009", "2008"),
         cex.axis = 0.75 * par()$cex.lab)
    axis(3, at = y.lines,
         labels = c("2013", "2012", "2011", "2010", "2009", "2008"),
         cex.axis = 0.75 * par()$cex.lab)
    axis(2)
    box()

    mtext("Year", side = c(1, 3), line = c(3.5, 2.75),
          cex = par()$cex.lab, font = par()$font.lab)
    mtext(bquote(bold("AWS 2m air temperature"~paste('(',degree,'C)'))),
          side = 2, line = 3.25, las = 0,
          cex = par()$cex.lab, font = par()$font.lab)

    legend("bottomright", c("Monthly mean", "Annual mean"),
           col = c("black", "dodgerblue"), lty = c(1, NA), pch = c(NA, 19),
           cex = 1.25, text.font = 2, bty = "n")

    par(op)

}

##' Produce TC17 Figure 02.
##'
##' This function makes all necessary calculations and plots the results for
##' Figure 02 shown in Münch et al. (2017).
##' @author Thomas Münch
##' @inherit Muench2017 references
TC17.Fig02 <- function() {

    op <- grfxtools::Par(mar = c(6, 6, 6, 6), lwd = 2,
                         font.lab = 2, font.axis = 2)

    # lat/lon for relevant sites
    edml <- c(-75.0025, 0.0684)
    aws <- c(-75.00063, 0.07793)

    b41 <- c(-75.000706, 0.067905)
    b42 <- c(-75.000626, 0.067777)

    t13.1 <- list()
    t13.1$start <- c(-75.00641, 0.07498)
    t13.1$end <-   c(-75.00673, 0.076024)
    t13.2 <- list()
    t13.2$start <- c(-75.00849, 0.08693)
    t13.2$end <-   c(-75.00877, 0.08819)

    t15.1 <- list()
    t15.1$start <- c(-75.00719, 0.0796)
    t15.1$end <-   c(-75.00760, 0.0805)
    t15.2 <- list()
    t15.2$start <- c(-75.00944, 0.0967)
    t15.2$end <-   c(-75.00986, 0.0972)

    # distance scale along -75 S
    ref <- -75
    lon <- seq(0.065, 0.1, 0.001)
    dist.lon <- sapply(lon, function(l) {
        geosphere::distGeo(c(0, ref), c(l, ref))})

    dist.equi <- seq(1740, 2880, 20)
    lon.equidist <- approx(dist.lon, lon, dist.equi)$y

    xdist <- c(2000, 2200, 2400, 2600, 2800)
    xlon <- lon.equidist[match(xdist, dist.equi)]

    # distance scale along 0.08 E
    ref <- 0.08
    lat <- seq(-74.985, -75.025, -0.001)
    dist.lat <- sapply(lat, function(l){
        geosphere::distGeo(c(ref, lat[1]), c(ref, l))})

    dist.lat <- dist.lat - dist.lat[which(lat == -75)]

    dist.equi <- seq(-1660, 2780, 20)
    lat.equidist <- approx(dist.lat, lat, dist.equi)$y

    ydist <- c(-500, 0, 500, 1000, 1500)
    ylat <- lat.equidist[match(ydist, dist.equi)]

    # arrows for North direction and mean wind direction
    x0 <- 0.0975
    y0 <- -74.9975

    angle.meanwind <- 57. * pi / 180.
    len <- 0.005
    x1 <- 0.085
    y1 <- -75.005
    x2 <- x1 + len * sin(angle.meanwind)
    y2 <- y1 + len * cos(angle.meanwind)

    plot(aws[2], aws[1], type = "n", axes = FALSE,
         xlim = c(0.065, 0.1), ylim = c(-75.015, -74.995),
         xlab = "", ylab = "")
    grid(lty = 1, lwd = 0.75)

    axis(side = 1, at = seq(0.065, 0.1, 0.005),
         labels = c(0.065, NA, 0.075, NA, 0.085, NA, 0.095, NA))
    axis(side = 2)
    axis(side = 3, at = xlon, labels = xdist)
    axis(side = 4, at = ylat, labels = ydist)
    box(lwd = 1.5)

    mtext(bquote(paste(bold("Longitude ("), degree, bold("E)"))),
          side = 1, line = 4.5,
          cex = par()$cex.lab, font = par()$font.lab)
    mtext(bquote(paste(bold("Latitude ("), degree, bold("N)"))), las = 0,
          side = 2, line = 4.5,
          cex = par()$cex.lab, font = par()$font.lab)
    mtext("Distance (m)", side = 3, line = 4,
          cex = par()$cex.lab, font = par()$font.lab)
    text(0.108, -75.005, labels = "Distance (m)", srt = -90, xpd = NA,
         cex = par()$cex.lab, font = par()$font.lab)

    points(edml[2], edml[1], pch = 24, col = "black", bg = "black")
    points(edml[2], edml[1], pch = 25, col = "black", bg = "black")

    points(aws[2], aws[1], pch = 23,
           col = "black", bg = "dodgerblue3", cex = 1.5, lwd = 1.5)

    points(b41[2], b41[1], pch = 21,
           col = "black", bg = "darkgreen", cex = 1.5, lwd = 1.5)
    points(b42[2], b42[1], pch = 21,
           col = "black", bg = "darkgreen", cex = 1.5, lwd = 1.5)

    points(t13.1$start[2], t13.1$start[1],
           col = "black", lwd = 0.75, cex = 0.5)
    points(t13.1$end[2], t13.1$end[1],
           col = "black", lwd = 0.75, cex = 0.5)
    lines(c(t13.1$start[2], t13.1$end[2]), c(t13.1$start[1], t13.1$end[1]),
          col = "black", lwd = 2.5, lty = 1)

    points(t13.2$start[2], t13.2$start[1],
           col = "black", lwd = 0.75, cex = 0.5)
    points(t13.2$end[2], t13.2$end[1],
           col = "black", lwd = 0.75, cex = 0.5)
    lines(c(t13.2$start[2], t13.2$end[2]), c(t13.2$start[1], t13.2$end[1]),
          col = "firebrick3", lwd = 2.5, lty = 1)

    points(t15.1$start[2], t15.1$start[1],
           col = "black", lwd = 0.75, cex = 0.5)
    points(t15.1$end[2], t15.1$end[1],
           col = "black", lwd = 0.75, cex = 0.5)
    lines(c(t15.1$start[2], t15.1$end[2]), c(t15.1$start[1], t15.1$end[1]),
          col = "black", lwd = 2.5, lty = 1)

    points(t15.2$start[2], t15.2$start[1],
           col = "black", lwd = 0.75, cex = 0.5)
    points(t15.2$end[2], t15.2$end[1],
           col = "black", lwd = 0.75, cex = 0.5)
    lines(c(t15.2$start[2], t15.2$end[2]), c(t15.2$start[1], t15.2$end[1]),
          col = "firebrick3", lwd = 2.5, lty = 1)

    prettymapr::addnortharrow(scale = 0.5, padin = c(0.25, 0.25))
    arrows(x1, y1, x2, y2, code = 1, length = 0.15, angle = 25, lwd = 1)

    par(op)

}

##' Produce TC17 Figure 03a.
##'
##' @param TR the data set; defaults to the processed T15 oxygen isotope data.
##' @author Thomas Münch
##' @inherit Muench2017 references
TC17.Fig03a <- function(TR = prepareTrenchData(na.treat = TRUE)$oxy) {

    op <- grfxtools::Par(oma = c(0, 0, 0, 0.5), mar = c(5, 5, 0.5, 2),
                         lwd = 2, font.lab = 2, font.axis = 2)

    # limit the colorscale
    MAX <- -35
    MIN <- -55
    T1 <- TR$trench15.1
    T1[T1 > MAX] <- MAX
    T1[T1 < MIN] <- MIN

    # fill part of surface layer with first non-NA value/profile
    # to improve plot appearance
    first.not.na <- apply(T1, 2, function(x) {which(!is.na(x))[1]})
    for (i in 1 : 11) {
        T1[first.not.na[i] - 1, i] <- T1[first.not.na[i], i]
    }

    top <- -15
    bottom <- TR$depth[length(TR$depth)]
    palette <- colorRampPalette(rev(RColorBrewer::brewer.pal(10, "RdYlBu")))

    filled.contour(TR$XPOS, TR$depth / 100, t(T1),
                   color.palette = palette,
                   plot.title = {
                       grid(col = "black", nx = NA, ny = NULL);
                       title(xlab = "Trench position (m)");
                       lines(TR$SPRF.t1$x, TR$SPRF.t1$y / 100)},
                   zlim = c(MIN, MAX), ylim = c(bottom, top) / 100)
    mtext("Depth (m)", side = 2, line = 3.5,
          cex = par()$cex.lab, font = par()$font.lab, las = 0)
    text(51, mean(c(bottom, top)) / 100,
         labels = expression(delta^bold("18") * bold("O") * bold(" (\u2030)")),
         srt = -90, xpd = NA, cex = par()$cex.lab, font = par()$font.lab)

    par(op)

}

##' Produce TC17 Figure 03b.
##'
##' @param TR the data set; defaults to the processed T15 oxygen isotope data.
##' @author Thomas Münch
##' @inherit Muench2017 references
TC17.Fig03b <- function(TR = prepareTrenchData(na.treat = TRUE)$oxy) {

    op <- grfxtools::Par(oma = c(0, 0, 0, 0.5), mar = c(5, 5, 0.5, 2),
                         lwd = 2, font.lab = 2, font.axis = 2)

    # limit the colorscale
    MAX <- -35
    MIN <- -55
    T2 <- TR$trench15.2
    T2[T2 > MAX] <- MAX
    T2[T2 < MIN] <- MIN

    # fill part of surface layer with first non-NA value/profile
    # to improve plot appearance
    first.not.na <- apply(T2 ,2, function(x) {which(!is.na(x))[1]})
    for (i in 1 : 11) {
        T2[first.not.na[i] - 1, i] <- T2[first.not.na[i], i]}

    top <- -15
    bottom <- TR$depth[length(TR$depth)]
    palette <- colorRampPalette(rev(RColorBrewer::brewer.pal(10, "RdYlBu")))

    filled.contour(TR$XPOS, TR$depth / 100, t(T2),
                   color.palette = palette,
                   plot.title = {
                     grid(col = "black", nx = NA, ny = NULL);
                     title(xlab = "Trench position (m)");
                     lines(TR$SPRF.t2$x, TR$SPRF.t2$y / 100)},
                   zlim = c(MIN, MAX), ylim = c(bottom, top) / 100)
    text(51, mean(c(bottom, top)) / 100,
         labels = expression(delta^bold("18") * bold("O") * bold(" (\u2030)")),
         srt = -90, xpd = NA, cex = par()$cex.lab, font = par()$font.lab)

    par(op)

}

##' Produce TC17 Figure 03c.
##'
##' @param TR the data set; defaults to the processed T15 oxygen isotope data.
##' @author Thomas Münch
##' @inherit Muench2017 references
TC17.Fig03c <- function(TR = prepareTrenchData(na.treat = TRUE)$oxy) {

    op <- grfxtools::Par(lwd = 2, font.lab = 2, font.axis = 2)

    ind1 <- which(TR$depth <= TR$SRF.b$t15.1)
    ind2 <- which(TR$depth <= TR$SRF.b$t15.2)
    v1 <- TR$mean15.1
    v2 <- TR$mean15.2
    p1 <- (p1 <- which(!is.na(v1)))[c(1, length(p1))]
    p2 <- (p2 <- which(!is.na(v2)))[c(1, length(p2))]
    v1[ind1[-length(ind1)]] <- NA
    v2[ind2[-length(ind2)]] <- NA

    plot(TR$depth / 100, TR$mean15.1, type = "n", las = 1,
         xlim = c(-5, 175) / 100, ylim = c(-52, -34),
         axes = FALSE, xlab = "", ylab = "")
    par(xaxp = c(0, 1.5, 3))
    axis(1)
    grfxtools::MinorTick(tick.ratio = 0.75, extend = 1)
    par(yaxp = c(-52, -36, 4))
    axis(2)
    grfxtools::MinorTick(side = 2, extend = 1)
    box()
    abline(v = c(0, 50, 100, 150) / 100, col = "black", lty = "dotted")

    mtext("Depth (m)", side = 1, line = 3.5,
          cex = par()$cex.lab, font = par()$font.lab)
    mtext(expression(delta^bold("18") * bold("O") * bold("  (\u2030)")),
          side = 2, line = 3.25, las = 0,
          cex = par()$cex.lab, font = par()$font.lab)

    lines(TR$depth / 100, v1, col = "black")
    lines((TR$depth + TR$k15) / 100, v2, col = "firebrick3")

    lines(TR$depth[ind1] / 100, TR$mean15.1[ind1],
          col = "black", lwd = 1.5, lty = 5)
    lines((TR$depth[ind2] + TR$k15) / 100, TR$mean15.2[ind2],
          col = "firebrick3", lwd = 1.5, lty = 5)

    points(TR$depth[p1] / 100, TR$mean15.1[p1],
           col = "black", pch = 1, lwd = 1.5, cex = 0.75)
    points((TR$depth[p2] + TR$k15) / 100, TR$mean15.2[p2],
           col = "firebrick3", pch = 23, lwd = 1.5, cex = 0.75)

    my.legend("bottomright", legend = c("T15-1", "T15-2"),
              pch = c(1, 23), lwd = 1.5, lty = c(1,1), col = c(1, "firebrick3"),
              cex = 1.25, text.font = 2, pt.cex = 0.75, pt.lwd = 1.5, bty = "n",
              end.pch = TRUE, pch.xoff = 0.2)

    par(op)
}

##' Produce TC17 Figure 04.
##'
##' This function makes all necessary calculations and plots the results for
##' Figure 04 shown in Münch et al. (2017).
##' @author Thomas Münch
##' @inherit Muench2017 references
TC17.Fig04 <- function() {

    TR = prepareTrenchData(na.treat = TRUE)$oxy
    
    op <- grfxtools::Par(oma = c(5, 0, 0.5, 0), mar = c(0, 6, 0, 6),
                         lwd = 2, font.lab = 2, font.axis = 2)

    ind1 <- which(TR$depth <= TR$SRF.b$t15)
    ind2 <- which(TR$depth <= TR$SRF.b$t13)
    v1 <- TR$mean15
    v2 <- TR$mean13
    v1[ind1[-length(ind1)]] <- NA
    v2[ind2[-length(ind2)]] <- NA

    plot(TR$depth, v1, type = "l", axes = FALSE,
         xlab = "", ylab = "", xlim = c(0, 175), ylim = c(-58, -36))
    lines(TR$depth[ind1], TR$mean15[ind1],
          lwd = 1.5, lty = 5, col = "black")

    par(yaxp = c(-52, -36, 4))
    axis(2)
    grfxtools::MinorTick(n = 2, side = 2)
    text(-35, -44,
         labels = expression(delta^bold("18") * bold("O")*bold(" (\u2030)")),
         srt = 90, xpd = NA, cex = par()$cex.lab, font = par()$font.lab,
         col = "black")
    text(17.5, -37, "T15 (2015)",
         cex = par()$cex.lab, font = par()$font.lab)

    par(new = TRUE)

    plot(TR$depth[1:38], v2, type = "l", axes = FALSE,
         xlab = "", ylab = "", xlim = c(0, 175), ylim = c(-48, -26),
         col = "dodgerblue")
    lines(TR$depth[ind2], TR$mean13[ind2], lwd = 1.5, lty = 5,
          col = "dodgerblue")

    par(yaxp = c(-48, -40, 2))
    axis(4, col = "dodgerblue", col.axis = "dodgerblue")
    grfxtools::MinorTick(n = 2, side = 4, col = "dodgerblue")
    par(xaxp = c(0, 175, 7))
    axis(1)

    text(210, -44,
         labels = expression(delta^bold("18") * bold("O") * bold(" (\u2030)")),
         srt = -90, xpd = NA, cex = par()$cex.lab, font = par()$font.lab,
         col = "dodgerblue")
    mtext("Depth (cm)", side = 1, line = 3.5,
          cex = par()$cex.lab, font = par()$font.lab)
    text(175 - 17.5, -40.75, "T13 (2013)",
         cex = par()$cex.lab, font = par()$font.lab, col = "dodgerblue")

    par(op)

}

##' Produce TC17 Figure 05.
##'
##' This function makes all necessary calculations and plots the results for
##' Figure 05 shown in Münch et al. (2017).
##' @param dat input data structure resulting from a call of
##' \code{\link{LoopParamSpace}}; if \code{NULL} (the default), the original
##' data presented in Münch et al. (2017) is used for plotting which is
##' supplied with this package in the variable \code{\link{ParamSpace}}.
##' @author Thomas Münch
##' @inherit Muench2017 references
TC17.Fig05 <- function(dat = NULL) {

    if (is.null(dat)) {
        message(paste("Fig05: No specific input data supplied --",
                      "using data of paper for plotting."))
        dat <- ParamSpace
    }

    op <- grfxtools::Par(lwd = 2, font.lab = 2, font.axis = 2)

    palette <- colorRampPalette(rev(RColorBrewer::brewer.pal(10, "RdYlBu")))

    filled.contour(dat$sigma, dat$densf, dat$adv.opt.arr,
                   color.palette = palette, zlim = c(40, 60),
                   plot.title =
                       title(xlab = "Differential diffusion length (cm)",
                             ylab = "Compression (cm)"),
                   plot.axes = {
                       contour(dat$sigma,
                               dat$densf,
                               dat$RMSD.opt,
                               add = TRUE, labcex = 1);
                       points(2.3, 3.5, pch = 21, col = "black",
                              bg = "black", cex = 1.25);
                       axis(1); axis(2)})
    
    text(8.1, 5, labels = "Optimal downward advection (cm)",
         srt = -90, xpd = NA, cex = par()$cex.lab, font = par()$font.lab)

    par(op)

}

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
##' @inherit Muench2017 references
##' @seealso \code{\link{SetModificationPar}}
TC17.Fig06 <- function(mod.param = NULL) {

    if (is.null(mod.param)) {
        mod.param <- SetModificationPar()
    }

    op <- grfxtools::Par(oma = c(5, 0, 0.5, 0), mar = c(0, 6, 0, 6),
                         mfrow = c(1, 2), lwd = 2, font.lab = 2, font.axis = 2)

    # color scale
    my.col <- c("dodgerblue", "#1b9e77", "#d95f02", "#7570b3")

    # T13* and T13**
    TR <- prepareTrenchData(na.treat = TRUE)$oxy
    T13.star     <- ModifyRecord(rec.in = TR$mean13_HiRes,
                                 res = TR$HiRes,
                                 depth.hires = TR$depth_HiRes,
                                 depth.lores = TR$depth,
                                 SIGMA = mod.param$SIGMA.opt,
                                 STRETCH = mod.param$STRETCH.opt,
                                 ADV = mod.param$ADV.opt)
    T13.starstar <- ModifyRecord(rec.in = TR$mean13_HiRes,
                                 res = TR$HiRes,
                                 depth.hires = TR$depth_HiRes,
                                 depth.lores = TR$depth,
                                 SIGMA = mod.param$SIGMA.ind,
                                 STRETCH = mod.param$STRETCH.ind,
                                 ADV = mod.param$ADV.ind)


    #---------------------------------------------------------------------------
    # Fig06-a

    # auxiliary variables
    v11 <- v1 <- TR$mean13
    v22 <- v2 <- ModifyRecord(rec.in = TR$mean13_HiRes,
                              res = TR$HiRes,
                              depth.hires = TR$depth_HiRes,
                              depth.lores = TR$depth,
                              STRETCH = mod.param$STRETCH.opt)$HiRes
    v33 <- v3 <- ModifyRecord(rec.in = TR$mean13_HiRes,
                              res = TR$HiRes,
                              depth.hires = TR$depth_HiRes,
                              depth.lores = TR$depth,
                              SIGMA = mod.param$SIGMA.opt)$LoRes
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
    v11[ind1[-length(ind1)]] <- NA
    v22[ind2[-length(ind2)]] <- NA
    v33[ind1[-length(ind1)]] <- NA
    v44[ind4[-length(ind4)]] <- NA

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
         srt = 90, xpd = NA, cex = par()$cex.lab, font = par()$font.lab,
         col = "black")

    legend("topleft", "T13 record", lwd = 2, col = my.col[1],
           lty = 1, cex = 1.1, text.font = 1, bty = "n")
    my.legend("topright", c("T13 after 2-yr incremental diffusion",
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
         srt = -90, xpd = NA, cex = par()$cex.lab, font = par()$font.lab,
         col = "black")

    par(xaxp = c(0, 125, 5))
    axis(1)

    mtext("Depth (cm)", side = 1, line = 3.5,
          cex = par()$cex.lab, font = par()$font.lab)

    my.legend("bottomright", "T13* record",
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
    v4 <- ModifyRecord(rec.in = TR$mean13_HiRes,
                       res = TR$HiRes,
                       depth.hires = TR$depth_HiRes,
                       depth.lores = TR$depth,
                       ADV = mod.param$ADV.only)$LoRes

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

    sum.max <- prxytools::LocatePeaks(v11, partial = TRUE)[3 : 6]

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
    grfxtools::MinorTick(n = 2, side = 2)
    par(xaxp = c(0, 175, 7))
    axis(1)

    mtext("Depth (cm)", side = 1, line = 3.5,
          cex = par()$cex.lab, font = par()$font.lab)
    mtext(expression(delta^bold("18") * bold("O") * bold(" (\u2030)")),
          side = 2, line = 3.5, las = 0,
          cex = par()$cex.lab, font = par()$font.lab)

    my.legend("topleft", c("T15", "T13* (opt. param.)", "T13** (ind. param.)"),
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

    par(yaxp = c(-4, 4, 4))
    axis(4, col = my.col[4], col.axis = my.col[4])

    text(205, 0, "T15 - T13** (\u2030)",
         cex = par()$cex.lab, font = par()$font.lab,
         col = my.col[4], srt = -90, xpd = NA)

    par(op)

}

##' Produce TC17 Figure 07.
##'
##' This function makes all necessary calculations and plots the results for
##' Figure 07 shown in Münch et al. (2017).
##' @param mod.param  list of the parameters according to which the original
##' T13 mean profile is modified to model the temporal changes. It must containt
##' the following elements: \code{ADV.ind}, \code{SIGMA.ind} and
##' \code{STRETCH.ind} (see also \code{\link{SetModificationPar}}). If
##' \code{NULL} (the default), the original data from Münch et al. (2017) is
##' used for plotting.
##' @author Thomas Münch
##' @inherit Muench2017 references
##' @seealso \code{\link{SetModificationPar}}
TC17.Fig07 <- function(mod.param = NULL) {
    
    if (is.null(mod.param)) {
        mod.param <- SetModificationPar()
    }

    op <- grfxtools::Par(mfrow = c(1, 2), xaxs = "r", yaxs = "i", lwd = 1.5,
                       mar = c(5, 5.5, 0.5, 0.5), font.lab = 2, font.axis = 2)

    # T13**
    TR <- prepareTrenchData()$oxy
    T13.starstar <- ModifyRecord(rec.in = TR$mean13_HiRes,
                                 res = TR$HiRes,
                                 depth.hires = TR$depth_HiRes,
                                 depth.lores = TR$depth,
                                 SIGMA = mod.param$SIGMA.ind,
                                 STRETCH = mod.param$STRETCH.ind,
                                 ADV = mod.param$ADV.ind)

    # profile differences
    diff.13 <- TR$mean13.1 -
        Hmisc::Lag(TR$mean13.2, shift = TR$k13 / TR$LoRes)

    diff.15 <- TR$mean15.1_HiRes -
        Hmisc::Lag(TR$mean15.2_HiRes, shift = TR$k15 / TR$HiRes)
    diff.15 <- diff.15[match(TR$depth, TR$depth_HiRes)]

    diff.2yr <- TR$mean15 - T13.starstar$LoRes

    
    #---------------------------------------------------------------------------
    # Fig07-a

    hist(diff.13, breaks = 10, freq = FALSE,
         xlim = c(-4, 4), ylim = c(0, 0.525),
         density = 10, main = "", xlab = "", ylab = "", col = "dodgerblue")
    hist(diff.15, add = TRUE, breaks = 10, freq = FALSE,
         density = 10, angle = -45)

    mtext("Spatial differences between trenches (\u2030)",
          side = 1, line = 3.5,
          cex = par()$cex.lab, font = par()$font.lab)
    mtext(expression(bold("Normalised frequency density (\u2030")
                     ^{"-1"} * bold(")")),
          side = 2, line = 3.75, las = 0,
          cex = par()$cex.lab, font = par()$font.lab)

    legend("topleft", c(expression(Delta * bold("T13")),
                        expression(Delta * bold("T15"))),
           lwd = 1.5, lty = 1, col = c("dodgerblue", "black"),
           bty = "n", inset = c(0, 0.02))

    
    #---------------------------------------------------------------------------
    # Fig07-b

    hist(c(diff.13, diff.15), breaks = 10, freq = FALSE,
         xlim = c(-4, 4), ylim = c(0, 0.525),
         density = 10, main = "", xlab = "", ylab = "", col = "dimgrey")
    hist(diff.2yr, add = TRUE, breaks = 10, freq = FALSE,
         density = 10, angle = -45, col = "firebrick")

    mtext("Spatial and temporal differences (\u2030)",
          side = 1, line = 3.5,
          cex = par()$cex.lab, font = par()$font.lab)
    mtext(expression(bold("Normalised frequency density (\u2030")
                     ^{"-1"} * bold(")")),
          side = 2, line = 3.75, las = 0,
          cex = par()$cex.lab, font = par()$font.lab)

    legend("topleft",
           c(expression(bold("Combined spatial: ") * Delta *
                        bold("T13 and ") * Delta * bold("T15")),
             expression(bold("Temporal: T15-T13**"))),
           lwd = 1.5, lty = 1, col = c("dimgrey", "firebrick"),
           bty = "n", inset = c(0, 0.02))

    par(op)

}
