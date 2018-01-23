##' Produce TC17 Figure 04.
##'
##' This function makes all necessary calculations and plots the results for
##' Figure 04 shown in Muench et al. (2017).
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
##' @param save.plot if \code{TRUE}, the plot is saved as a pdf file in the
##' folder specified by \code{path}. Defaults to \code{FALSE} which results in
##' on-screen display of the plot.
##' @author Thomas Münch
##' @references
##' Muench, T., et al., Constraints on post-depositional isotope modifications
##' in East Antarctic firn from analysing temporal changes of isotope profiles,
##' The Cryosphere, doi:10.5194/tc-11-2175-2017, 2017.
##' @export
TC17.Fig04 <- function(TR, path = file.path(getwd(), "plots"),
                       file.name = "tc17_fig_04", device = "quartz",
                       save.plot = FALSE) {
    
    param <- SetPlotPar()
    plot.par <- param$par
    dev.size <- param$dev.size
    
    OpenDevice(device = device, path = path, file.name = file.name,
               height = dev.size$h, width = dev.size$w,
               save.plot = save.plot)
    par(plot.par)
    par(oma = c(5, 0, 0.5, 0), mar = c(0, 6, 0, 6))

    ind1 <- which(TR$oxy$depth <= TR$oxy$SRF.b$t15)
    ind2 <- which(TR$oxy$depth <= TR$oxy$SRF.b$t13)
    v1 <- TR$oxy$mean15
    v2 <- TR$oxy$mean13
    v1[ind1[-length(ind1)]] <- NA
    v2[ind2[-length(ind2)]] <- NA

    plot(TR$oxy$depth, v1, type = "l", axes = FALSE,
         xlab = "", ylab = "", xlim = c(0, 175), ylim = c(-58, -36))
    lines(TR$oxy$depth[ind1], TR$oxy$mean15[ind1],
          lwd = 1.5, lty = 5, col = "black")

    par(yaxp = c(-52, -36, 4))
    axis(2)
    MinorTick(nx = 1, ny = 2, side = 2)
    text(-35, -44,
         labels = expression(delta^bold("18") * bold("O")*bold(" (\u2030)")),
         srt = 90, xpd = NA, cex = plot.par$cex.lab, font = plot.par$font.lab,
         col = "black")
    text(17.5, -37, "T15 (2015)",
         cex = plot.par$cex.lab, font = plot.par$font.lab)

    par(new = TRUE)

    plot(TR$oxy$depth[1:38], v2, type = "l", axes = FALSE,
         xlab = "", ylab = "", xlim = c(0, 175), ylim = c(-48, -26),
         col = "dodgerblue")
    lines(TR$oxy$depth[ind2], TR$oxy$mean13[ind2], lwd = 1.5, lty = 5,
          col = "dodgerblue")

    par(yaxp = c(-48, -40, 2))
    axis(4, col = "dodgerblue", col.axis = "dodgerblue")
    MinorTick(nx = 1, ny = 2, side = 4, col = "dodgerblue")
    par(xaxp = c(0, 175, 7))
    axis(1)

    text(210, -44,
         labels = expression(delta^bold("18") * bold("O") * bold(" (\u2030)")),
         srt = -90, xpd = NA, cex = plot.par$cex.lab, font = plot.par$font.lab,
         col = "dodgerblue")
    mtext("Depth (cm)", side = 1, line = 3.5,
          cex = plot.par$cex.lab, font = plot.par$font.lab)
    text(175 - 17.5, -40.75, "T13 (2013)",
         cex = plot.par$cex.lab, font = plot.par$font.lab, col = "dodgerblue")

    if (save.plot) dev.off()

}
