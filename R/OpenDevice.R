##' Open a graphics device.
##'
##' This is a wrapper function to open a graphics device for plotting, or a file
##' stream to save the plot into a pdf file.
##' @param device Character string specifying the plotting device; defaults to
##' "quartz" which is suitable for macOS systems. Currently, no other devices
##' are implemented.
##' @param plot.file The file path specifying where to save the plot.
##' @param height The height of the graphics device; defaults to 6 inches.
##' @param width The width of the graphics device; defaults to 8 inches.
##' @param save.plot if \code{TRUE} a file stream is opened according to
##' \code{file.path} to save the plot as a pdf file. Defaults to \code{FALSE}
##' which simply opens a device window. Setting \code{save.plot} to \code{TRUE}
##' without specying a \code{file.path} results in an error.
##' @author Thomas Münch
##' @export
OpenDevice <- function(device = "quartz", plot.file,
                       height = 6, width = 8, save.plot = FALSE) {

    if (save.plot == TRUE & is.null(plot.file)) {
        stop("No file path specified for saving.")
    }
    
    if (device == "quartz") {

        if (save.plot) {
            quartz(file = plot.file, type = "pdf",
                   height = height, width = width)
        } else {
            quartz(height = height, width = width)
        }
    } else {
        stop("Unknown device.")
    }

}

