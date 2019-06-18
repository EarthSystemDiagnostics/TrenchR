##' Produce TC2017 Figures.
##'
##' This function is a wrapper to produce all figures in Münch et al. (2017)
##' using the \code{quartz} graphics device for Mac OS X systems.
##' @param save if \code{TRUE}, the plots are saved as files in the
##' folder specified by \code{path}. Defaults to \code{FALSE} which results in
##' on-screen display of the plots.
##' @param path path to the directory in which to save the plots (for
##' \code{save = TRUE}). Defaults to \code{plots} in the current working
##' directory. If this folder does not exist, it is attempted to create it
##' with a warning (see also \code{\link{OpenDevice}}).
##' @param height height of the plotting area in inches. Default '6'.
##' @param width width of the plotting area in inches. Default '8'.
##' @author Thomas Münch
##' @references
##' Münch, T., et al., Constraints on post-depositional isotope modifications
##' in East Antarctic firn from analysing temporal changes of isotope profiles,
##' The Cryosphere, doi:10.5194/tc-11-2175-2017, 2017.
MakeTC17Figures <- function(save = FALSE, path = file.path(getwd(), "plots"),
                            height = 6, width = 8) {

    file <- NULL

    if (save) file <- "tc17_fig_01.pdf"
    OpenDevice(file = file, path = path,
               height = 1.125 * height, width = 2 * width)
    TC17.Fig01()
    if (save) dev.off()

    if (save) file <- "tc17_fig_02.pdf"
    OpenDevice(file = file, path = path,
               height = width, width = width)
    TC17.Fig02()
    if (save) dev.off()

    if (save) {
        graphics.dev <- OpenDevice
        dpi <- 300
    } else {
        graphics.dev <- NULL
        dpi <- NULL
    }
    TC17.Fig03(graphics.dev = graphics.dev, path = path,
               file = "tc17_fig_03.png", dpi = dpi)

    if (save) file <- "tc17_fig_04.pdf"
    OpenDevice(file = file, path = path,
               height = height, width = width)
    TC17.Fig04()
    if (save) dev.off()

    if (save) {
        file <- "tc17_fig_05.png"
        dpi <- 300
    } else {
        dpi <- NULL
    }
    OpenDevice(file = file, path = path,
               height = height, width = width, dpi = dpi)
    TC17.Fig05()
    if (save) dev.off()

    if (save) file <- "tc17_fig_06.pdf"
    OpenDevice(file = file, path = path,
               height = height, width = 2 * width)
    TC17.Fig06()
    if (save) dev.off()

    if (save) file <- "tc17_fig_07.pdf"
    OpenDevice(file = file, path = path,
               height = width, width = 2 * width)
    TC17.Fig07()
    if (save) dev.off()

}
    
