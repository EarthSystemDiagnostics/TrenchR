##' Produce TC17 Figure 03.
##'
##' This function makes all necessary calculations and plots the results for
##' Figure 03 shown in Münch et al. (2017).
##'
##' Note that this function will not use the R Studio Graphics device for
##' on-screen plotting even if it is your default device in order to preserve
##' the setting of the default size of the plotting region (see also
##' \code{\link{dev.new}}).
##' @param graphics.dev graphics device (function name) to be used for saving
##' the plots, see details. Defaults to \code{NULL} for on-screen plotting.
##' @param path path to the directory in which to save the plots. Defaults to
##' \code{NULL} for on-screen plotting but needs to be specified for saving the
##' plot using the supplied device in \code{graphics.dev}.
##' @param file name of the file for saving including the respective file
##' extension. Defaults to \code{NULL} for on-screen plotting but needs to be
##' specified for saving the plot using the supplied device in
##' \code{graphics.dev}.
##' @param height height of the plotting area in inches. Default '6'.
##' @param width width of the plotting area in inches. Default '8'.
##' @param adj.width extension of the width of the plotting area in inches for
##' subplots (a) and (b). Default '1.75'.
##' @author Thomas Münch
##' @inherit Muench2017 references
TC17.Fig03 <- function(graphics.dev = NULL, path = NULL, file = NULL,
                       height = 6, width = 8, adj.width = 1.75, ...) {

    if (!is.null(graphics.dev)) {
        if (is.null(path)) {
            stop("path argument missing for saving the plots.")
        }
        if (is.null(file)) {
            stop("file argument missing for saving the plots.")
        }
    }

    TR = prepareTrenchData(na.treat = TRUE)$oxy

    #---------------------------------------------------------------------------
    # Fig03-a
    if (is.null(graphics.dev)) {
        dev.new(height = height, width = width + adj.width, ...,
                noRStudioGD = TRUE)
    } else {
        file.a <- paste(paste(tools::file_path_sans_ext(file), "a", sep = ""),
                      tools::file_ext(file), sep = ".")
        graphics.dev(file = file.a, path, height = height,
                     width = width + adj.width, ...)
    }

    TC17.Fig03a(TR = TR)

    #---------------------------------------------------------------------------
    # Fig03-b
    if (is.null(graphics.dev)) {
        dev.new(height = height, width = width + adj.width, ...,
                noRStudioGD = TRUE)
    } else {
        dev.off()
        file.b <- paste(paste(tools::file_path_sans_ext(file), "b", sep = ""),
                        tools::file_ext(file), sep = ".")
        graphics.dev(file = file.b, path, height = height,
                     width = width + adj.width, ...)
    }

    TC17.Fig03b(TR = TR)

    #---------------------------------------------------------------------------
    # Fig03-c
    if (is.null(graphics.dev)) {
        dev.new(height = height, width = width, ...,
                noRStudioGD = TRUE)
    } else {
        dev.off()
        file.c <- paste(paste(tools::file_path_sans_ext(file), "c", sep = ""),
                        tools::file_ext(file), sep = ".")
        graphics.dev(file = file.c, path, height = height,
                     width = width, ...)
    }

    TC17.Fig03c(TR = TR)

    if (!is.null(graphics.dev)) {
        dev.off()
    }

}

##' Produce TC17 Figure 03a.
##'
##' @param TR the data set; defaults to the processed T15 oxygen isotope data.
##' @author Thomas Münch
##' @inherit Muench2017 references
TC17.Fig03a <- function(TR = prepareTrenchData(na.treat = TRUE)$oxy) {

    op <- par(pars <- SetPlotPar(oma = c(0, 0, 0, 0.5), mar = c(5, 5, 0.5, 2)))

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
          cex = pars$cex.lab, font = pars$font.lab, las = 0)
    text(51, mean(c(bottom, top)) / 100,
         labels = expression(delta^bold("18") * bold("O") * bold(" (\u2030)")),
         srt = -90, xpd = NA, cex = pars$cex.lab, font = pars$font.lab)

    par(op)

}

##' Produce TC17 Figure 03b.
##'
##' @param TR the data set; defaults to the processed T15 oxygen isotope data.
##' @author Thomas Münch
##' @inherit Muench2017 references
TC17.Fig03b <- function(TR = prepareTrenchData(na.treat = TRUE)$oxy) {

    op <- par(pars <- SetPlotPar(oma = c(0, 0, 0, 0.5), mar = c(5, 5, 0.5, 2)))

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
         srt = -90, xpd = NA, cex = pars$cex.lab, font = pars$font.lab)

    par(op)

}

##' Produce TC17 Figure 03c.
##'
##' @param TR the data set; defaults to the processed T15 oxygen isotope data.
##' @author Thomas Münch
##' @inherit Muench2017 references
TC17.Fig03c <- function(TR = prepareTrenchData(na.treat = TRUE)$oxy) {

    op <- par(pars <- SetPlotPar())

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
          cex = pars$cex.lab, font = pars$font.lab)
    mtext(expression(delta^bold("18") * bold("O") * bold("  (\u2030)")),
          side = 2, line = 3.25, las = 0,
          cex = pars$cex.lab, font = pars$font.lab)

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
