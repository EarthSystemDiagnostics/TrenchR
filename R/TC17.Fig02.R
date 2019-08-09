##' Produce TC17 Figure 02.
##'
##' This function makes all necessary calculations and plots the results for
##' Figure 02 shown in Münch et al. (2017).
##' @author Thomas Münch
##' @inherit Muench2017 references
TC17.Fig02 <- function() {

    pars <- SetPlotPar(mar = c(6, 6, 6, 6))
    op <- par(pars)

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
          cex = pars$cex.lab, font = pars$font.lab)
    mtext(bquote(paste(bold("Latitude ("), degree, bold("N)"))), las = 0,
          side = 2, line = 4.5,
          cex = pars$cex.lab, font = pars$font.lab)
    mtext("Distance (m)", side = 3, line = 4,
          cex = pars$cex.lab, font = pars$font.lab)
    text(0.108, -75.005, labels = "Distance (m)", srt = -90, xpd = NA,
         cex = pars$cex.lab, font = pars$font.lab)

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

    GISTools::north.arrow(x0, y0, len = 0.0004,
                          lab = "N", lwd = 1, cex.lab = 1.25)
    arrows(x1, y1, x2, y2, code = 1, length = 0.15, angle = 25, lwd = 1)

    par(op)

}
