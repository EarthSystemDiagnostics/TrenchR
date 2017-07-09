##' Open a graphics device.
##'
##' This is a wrapper function to open a graphics device for plotting, or a file
##' stream to save the plot into file of type specified by \code{type}.
##' @param device Character string specifying the plotting device; defaults to
##' "quartz" which is suitable for macOS systems. Currently, no other devices
##' are implemented.
##' @param path The path to the directory in which to save the plot (for
##' \code{save.plot = TRUE}). If the specified directory does not exist, a
##' warning is issed but nevertheless the function attempts to create the
##' directory.
##' @param file.name The name of the file under which to save the plot.
##' @param type The type of output to use. Defaults to \code{pdf}.
##' @param height The height of the graphics device; defaults to 6 inches.
##' @param width The width of the graphics device; defaults to 8 inches.
##' @param save.plot if \code{TRUE} a file stream is opened according to
##' \code{file.path} to save the plot as a pdf file. Defaults to \code{FALSE}
##' which simply opens a device window. Setting \code{save.plot} to \code{TRUE}
##' without specying a \code{file.path} results in an error.
##' @author Thomas Münch
##' @export
OpenDevice <- function(device = "quartz", path, file.name, type = "pdf",
                       height = 6, width = 8, save.plot = FALSE) {

    if (save.plot == TRUE & (is.null(file.name) | is.null(path))) {
        stop("No file path specified for saving.")
    }
    
    if (device == "quartz") {

        if (save.plot) {
            if (!file.exists(path)) {
                warning(paste("Directory specified by path does not exist.",
                              "Creating it..."))
                dir.create(path)
            }
            plot.file <- file.path(path, file.name)
            plot.file <- paste(plot.file, type, sep = ".")
            if (type == "png") {
                quartz(file = plot.file, type = type,
                       height = height, width = width, dpi = 300)
            } else {
                quartz(file = plot.file, type = type,
                       height = height, width = width)
            }
        } else {
            quartz(height = height, width = width)
        }
    } else {
        stop("Unknown device.")
    }

}

