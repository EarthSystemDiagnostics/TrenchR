##' Produce TC17 Figure 03.
##'
##' This function makes all necessary calculations and plots the results for
##' Figure 03 shown in Muench et al. (2017).
##' @param TR The results from a call to \code{\link{prepareTrenchData(na.treat
##' = TRUE)}}.
##' @param path The path to the directory in which to save the plot (for
##' \code{save.plot = TRUE}). Defaults to the folder \code{plots} in the current
##' working directory. If this folder does not exist, it is attempted to create
##' with a warning (see also \code{\link{OpenDevice}}). 
##' @param file.name The name of the file (excluding extension) to save the
##' plot in.
##' @param device The graphics device to be used to display and save the
##' plot. Defaults to the \code{quartz} device which is the only currently
##' implemented device option.
##' @param save.plot if \code{TRUE}, the plot is saved as a png file in the
##' folder specified by \code{path}. Defaults to \code{FALSE} which results in
##' on-screen display of the plot.
##' @author Thomas Münch
##' @references
##' Muench, T., et al., Constraints on post-depositional isotope modifications
##' in East Antarctic firn from analysing temporal changes of isotope profiles,
##' The Cryosphere, doi:10.5194/tc-11-2175-2017, 2017.
##' @export
TC17.Fig03 <- function(TR = prepareTrenchData(na.treat = TRUE)$oxy,
                       path = file.path(getwd(), "plots"),
                       file.name = "tc17_fig_03", device = "quartz",
                       save.plot = FALSE) {

    param <- SetPlotPar()
    plot.par <- param$par
    dev.size <- param$dev.size


    #---------------------------------------------------------------------------
    # Fig03-a
    adj <- 1.75
    OpenDevice(device = device, path = path,
               file.name = paste(file.name, "a", sep = ""),
               type = "png", height = dev.size$h, width = dev.size$w + adj,
               save.plot = save.plot)
    par(plot.par)
    par(oma = c(0, 0, 0, 0.5), mar = c(5, 5, 0.5, 2))

    # limit the colorscale
    MAX <- -35
    MIN <- -55
    T1 <- TR$trench15.1
    T1[T1 > MAX] <- MAX
    T1[T1 < MIN] <- MIN
    T2 <- TR$trench15.2
    T2[T2 > MAX] <- MAX
    T2[T2 < MIN] <- MIN

    # fill part of surface layer with first non-NA value/profile
    # to improve plot appearance
    first.not.na <- apply(T1, 2, function(x) {which(!is.na(x))[1]})
    for (i in 1 : 11) {
        T1[first.not.na[i] - 1, i] <- T1[first.not.na[i], i]
    }
    first.not.na <- apply(T2 ,2, function(x) {which(!is.na(x))[1]})
    for (i in 1 : 11) {
        T2[first.not.na[i] - 1, i] <- T2[first.not.na[i], i]}

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
          cex = plot.par$cex.lab, font = plot.par$font.lab, las = 0)
    text(51, mean(c(bottom, top)) / 100,
         labels = expression(delta^bold("18") * bold("O") * bold(" (\u2030)")),
         srt = -90, xpd = NA, cex = plot.par$cex.lab, font = plot.par$font.lab)

    if (save.plot) dev.off()

    #---------------------------------------------------------------------------
    # Fig03-b
    OpenDevice(device = device, path = path,
               file.name = paste(file.name, "b", sep = ""),
               type = "png", height = dev.size$h, width = dev.size$w + adj,
               save.plot = save.plot)
    par(plot.par)
    par(oma = c(0, 0, 0, 0.5), mar = c(5, 5, 0.5, 2))

    filled.contour(TR$XPOS, TR$depth / 100, t(T2),
                   color.palette = palette,
                   plot.title = {
                       grid(col = "black", nx = NA, ny = NULL);
                       title(xlab = "Trench position (m)");
                       lines(TR$SPRF.t2$x, TR$SPRF.t2$y / 100)},
                   zlim = c(MIN, MAX), ylim = c(bottom, top) / 100)
    text(51, mean(c(bottom, top)) / 100,
         labels = expression(delta^bold("18") * bold("O") * bold(" (\u2030)")),
         srt = -90, xpd = NA, cex = plot.par$cex.lab, font = plot.par$font.lab)

    if (save.plot) dev.off()

    #---------------------------------------------------------------------------
    # Fig03-c
    OpenDevice(device = device, path = path,
               file.name = paste(file.name, "c", sep = ""),
               type = "png", height = dev.size$h, width = dev.size$w,
               save.plot = save.plot)
    par(plot.par)

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
    par(xaxp = c(0, 1.75, 7))
    axis(1, at = c(0.0, 0.5, 1.0, 1.5))
    axis(1, at = c(0.25, 0.75, 1.25, 1.75), tcl = 0.75 * par("tcl"),
         labels = FALSE)
    axis(2, at = seq(-52, -34, 4))
    axis(2, at = seq(-50, -34, 4), tcl = 0.5 * par("tcl"), labels = FALSE)
    box()
    abline(v = c(0, 50, 100, 150) / 100, col = "black", lty = "dotted")

    mtext("Depth (m)", side = 1, line = 3.5,
          cex = plot.par$cex.lab, font = plot.par$font.lab)
    mtext(expression(delta^bold("18") * bold("O") * bold("  (\u2030)")),
          side = 2, line = 3.25, las = 0,
          cex = plot.par$cex.lab, font = plot.par$font.lab)

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

    MyLegend("bottomright", legend = c("T15-1", "T15-2"),
             pch = c(1, 23), lwd = 1.5, lty = c(1,1), col = c(1, "firebrick3"),
             cex = 1.25, text.font = 2, pt.cex = 0.75, pt.lwd = 1.5, bty = "n",
             end.pch = TRUE, pch.xoff = 0.2)

    if (save.plot) dev.off()

}
    
