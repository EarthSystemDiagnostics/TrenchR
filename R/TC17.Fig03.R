##' TC17 Figure 03.
##'
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @param TR 
##' @param path 
##' @param "plots") 
##' @param file.name 
##' @param device 
##' @param dev.size 
##' @param save.plot 
##' @return 
##' @author Thomas Münch
##' @export
TC17.Fig03 <- function(TR, path = file.path(getwd(), "plots"),
                       file.name = "tc17_fig_03", device = "quartz",
                       dev.size = list(h = 6, w = 8), save.plot = FALSE) {

    plot.par <- SetPlotPar()
    plot.file <- file.path(path, file.name)

    if (device == "quartz") {
        quartzFonts(optima = c("Optima Regular", "Optima Bold",
                               "Optima Italic", "Optima Bold Italic"))
    }


    #---------------------------------------------------------------------------
    # Fig03-a
    adj <- 1.75
    OpenDevice(device = device, plot.file = paste(plot.file, "a", sep = ""),
               type = "png", height = dev.size$h, width = dev.size$w + adj,
               save.plot = save.plot)
    par(plot.par)
    par(oma = c(0, 0, 0, 0.5), mar = c(5, 5, 0.5, 2))
    if (device == "quartz") par(family = "optima")

    # limit the colorscale
    MAX <- -35
    MIN <- -55
    T1 <- TR$oxy$trench15.1
    T1[T1 > MAX] <- MAX
    T1[T1 < MIN] <- MIN
    T2 <- TR$oxy$trench15.2
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
    bottom <- TR$oxy$depth[length(TR$oxy$depth)]
    palette <- colorRampPalette(rev(RColorBrewer::brewer.pal(10, "RdYlBu")))


    filled.contour(TR$oxy$XPOS, TR$oxy$depth / 100, t(T1),
                   color.palette = palette,
                   plot.title = {
                       grid(col = "black", nx = NA, ny = NULL);
                       title(xlab = "Trench position (m)");
                       lines(TR$oxy$SPRF.t1$x, TR$oxy$SPRF.t1$y / 100)},
                   zlim = c(MIN, MAX), ylim = c(bottom, top) / 100)
    mtext("Depth (m)", side = 2, line = 3.5,
          cex = plot.par$cex.lab, font = plot.par$font.lab, las = 0)
    text(51, mean(c(bottom, top)) / 100,
         labels = expression(delta^bold("18") * bold("O") * bold(" (\u2030)")),
         srt = -90, xpd = NA, cex = plot.par$cex.lab, font = plot.par$font.lab)

    if (save.plot) dev.off()

    #---------------------------------------------------------------------------
    # Fig03-b
    OpenDevice(device = device, plot.file = paste(plot.file, "b", sep = ""),
               type = "png", height = dev.size$h, width = dev.size$w + adj,
               save.plot = save.plot)
    par(plot.par)
    par(oma = c(0, 0, 0, 0.5), mar = c(5, 5, 0.5, 2))
    if (device == "quartz") par(family = "optima")


    filled.contour(TR$oxy$XPOS, TR$oxy$depth / 100, t(T2),
                   color.palette = palette,
                   plot.title = {
                       grid(col = "black", nx = NA, ny = NULL);
                       title(xlab = "Trench position (m)");
                       lines(TR$oxy$SPRF.t2$x, TR$oxy$SPRF.t2$y / 100)},
                   zlim = c(MIN, MAX), ylim = c(bottom, top) / 100)
    text(51, mean(c(bottom, top)) / 100,
         labels = expression(delta^bold("18") * bold("O") * bold(" (\u2030)")),
         srt = -90, xpd = NA, cex = plot.par$cex.lab, font = plot.par$font.lab)

    if (save.plot) dev.off()

    #---------------------------------------------------------------------------
    # Fig03-c
    OpenDevice(device = device, plot.file = paste(plot.file, "c", sep = ""),
               type = "png", height = dev.size$h, width = dev.size$w,
               save.plot = save.plot)
    par(plot.par)
    if (device == "quartz") par(family = "optima")

    ind1 <- which(TR$oxy$depth <= TR$oxy$SRF.b$t15.1)
    ind2 <- which(TR$oxy$depth <= TR$oxy$SRF.b$t15.2)
    v1 <- TR$oxy$mean15.1
    v2 <- TR$oxy$mean15.2
    p1 <- (p1 <- which(!is.na(v1)))[c(1, length(p1))]
    p2 <- (p2 <- which(!is.na(v2)))[c(1, length(p2))]
    v1[ind1[-length(ind1)]] <- NA
    v2[ind2[-length(ind2)]] <- NA

    plot(TR$oxy$depth, TR$oxy$mean15.1, type = "n", las = 1,
         xlim = c(-5, 175), ylim = c(-52, -34),
         axes = FALSE, xlab = "", ylab = "")
    par(xaxp = c(0, 175, 7))
    axis(1)
    axis(2, at = seq(-52, -34, 4))
    axis(2, at = seq(-50, -34, 4), tcl = 0.5 * par("tcl"), labels = FALSE)
    box()
    abline(v = c(0, 50, 100, 150), col = "black", lty = "dotted")

    mtext("Depth (cm)", side = 1, line = 3.5,
          cex = plot.par$cex.lab, font = plot.par$font.lab)
    mtext(expression(delta^bold("18") * bold("O") * bold("  (\u2030)")),
          side = 2, line = 3.25, las = 0,
          cex = plot.par$cex.lab, font = plot.par$font.lab)

    lines(TR$oxy$depth, v1, col = "black")
    lines(TR$oxy$depth + TR$oxy$k15, v2, col = "firebrick3")

    lines(TR$oxy$depth[ind1], TR$oxy$mean15.1[ind1],
          col = "black", lwd = 1.5, lty = 5)
    lines(TR$oxy$depth[ind2] + TR$oxy$k15, TR$oxy$mean15.2[ind2],
          col = "firebrick3", lwd = 1.5, lty = 5)

    points(TR$oxy$depth[p1], TR$oxy$mean15.1[p1],
           col = "black", pch = 1, lwd = 1.5, cex = 0.75)
    points(TR$oxy$depth[p2] + TR$oxy$k15, TR$oxy$mean15.2[p2],
           col = "firebrick3", pch = 23, lwd = 1.5, cex = 0.75)

    MyLegend("bottomright", legend = c("T15-1", "T15-2"),
             pch = c(1, 23), lwd = 1.5, lty = c(1,1), col = c(1, "firebrick3"),
             cex = 1.25, text.font = 2, pt.cex = 0.75, pt.lwd = 1.5, bty = "n",
             end.pch = TRUE, pch.xoff = 0.2)

    if (save.plot) dev.off()

}
    
