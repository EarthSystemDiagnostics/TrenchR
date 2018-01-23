##' Produce TC17 Figure 05.
##'
##' This function makes all necessary calculations and plots the results for
##' Figure 05 shown in Muench et al. (2017).
##' @param ParamSpace See \code{\link{ParamSpace}}.
##' @param path The path to the directory in which to save the plot (for
##' \code{save.plot = TRUE}). Defaults to the folder \code{plots} in the current
##' working directory. If this folder does not exist, it is attempted to create
##' with a warning (see also \code{\link{OpenDevice}}).
##' @param file.name The name of the file (excluding extension) to save the
##' plot in.
##' @param dev.size The graphics device to be used to display and save the
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
TC17.Fig05 <- function(ParamSpace, path = file.path(getwd(), "plots"),
                       file.name = "tc17_fig_05", device = "quartz",
                       save.plot = FALSE) {

    param <- SetPlotPar()
    plot.par <- param$par
    dev.size <- param$dev.size

    OpenDevice(device = device, path = path, file.name = file.name,
               type = "png", height = dev.size$h, width = dev.size$w,
               save.plot = save.plot)
    par(plot.par)

    palette <- colorRampPalette(rev(RColorBrewer::brewer.pal(10, "RdYlBu")))

    filled.contour(ParamSpace$sigma, ParamSpace$densf, ParamSpace$adv.opt.arr,
                   color.palette = palette, zlim = c(40, 60),
                   plot.title =
                       title(xlab = "Differential diffusion length (cm)",
                             ylab = "Compression (cm)"),
                   plot.axes = {
                       contour(ParamSpace$sigma,
                               ParamSpace$densf,
                               ParamSpace$RMSD.opt,
                               add = TRUE, labcex = 1);
                       points(2.3, 3.5, pch = 21, col = "black",
                              bg = "black", cex = 1.25);
                       axis(1); axis(2)})
    
    text(8.1, 5, labels = "Optimal downward advection (cm)",
         srt = -90, xpd = NA, cex = plot.par$cex.lab, font = plot.par$font.lab)

    if (save.plot) dev.off()

}

