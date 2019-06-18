##' Produce TC17 Figure 05.
##'
##' This function makes all necessary calculations and plots the results for
##' Figure 05 shown in Münch et al. (2017).
##' @param dat input data structure resulting from a call of
##' \code{\link{LoopParamSpace}}; if \code{NULL} (the default), the original
##' data presented in Münch et al. (2017) is used for plotting which is
##' supplied with this package in the variable \code{\link{ParamSpace}}.
##' @author Thomas Münch
##' @references
##' Münch, T., et al., Constraints on post-depositional isotope modifications
##' in East Antarctic firn from analysing temporal changes of isotope profiles,
##' The Cryosphere, doi:10.5194/tc-11-2175-2017, 2017.
##' @export
TC17.Fig05 <- function(dat = NULL) {

    if (is.null(dat)) {
        message(paste("Fig05: No specific input data supplied --",
                      "using data of paper for plotting."))
        dat <- ParamSpace
    }

    pars <- SetPlotPar()
    op <- par(pars)

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
         srt = -90, xpd = NA, cex = pars$cex.lab, font = pars$font.lab)

    par(op)

}

