##' Open quartz graphics device.
##'
##' This is a wrapper function to open a \code{quartz} graphics device for
##' on-screen plotting or saving to a file.
##' @param file filename for saving the plot. Must include the desired file
##' extension for setting the \code{type} parameter to \code{quartz}. Setting to
##' \code{NULL} (the default) opens a \code{quartz} device for on-screen
##' plotting.
##' @param path path to the directory in which to save the plot. If the
##' specified directory does not exist, a warning is issued but nevertheless the
##' function attempts to create the directory. Default path is the current
##' working directory.
##' @param height height of the plotting area in inches. Default '6'.
##' @param width width of the plotting area in inches. Default '8'.
##' @param ... further arguments to \code{quartz}, e.g., dpi resolution in case
##' of png output.
##' @seealso \code{quartz}
##' @author Thomas Münch
OpenDevice <- function(file = NULL, path = getwd(),
                       height = 6, width = 8, ...) {

    if (!is.null(file)) {

        type <- tools::file_ext(file)
        if (nchar(type) == 0)
            stop("No file extension found for setting 'type'.")

        if (!file.exists(path)) {
            warning(paste("Directory specified by path does not exist.",
                          "Creating it..."))
            dir.create(path)
        }
        
        plot.file <- file.path(path, file)
        
    } else {

        plot.file <- NULL
        type <- "native"
    }

    quartz(height = height, width = width, file = plot.file, type = type, ...)

}

