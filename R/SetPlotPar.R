##' Default plotting parameters.
##'
##' This function provides a list with default plotting parameters that are used
##' to create the plots for the TC paper.
##' @return A list of two components:
##' \itemize{
##' \item The list 'par' with the following default plotting parameters:
##'     \itemize{
##'     \item mar = c(5, 5, 0.5, 0.5)
##'     \item lwd = 2
##'     \item las = 1
##'     \item font.lab = 2
##'     \item font.axis = 2
##'     \item cex.main = 1.5
##'     \item cex.lab = 1.5
##'     \item cex.axis = 1.25}
##' \item The list 'dev.size' with the following default device sizes:
##'     \itemize{
##'     \item h = 6: the device height
##'     \item w = 8: the device width}
##' }
##' @author Thomas Münch
##' @export
SetPlotPar <- function() {

    par <- list(
        mar = c(5, 5, 0.5, 0.5),
        lwd = 2,
        las = 1,
        font.lab  = 2,
        font.axis = 2,
        cex.main  = 1.5,
        cex.lab   = 1.5,
        cex.axis  = 1.25
    )

    dev.size <- list(h = 6, w = 8)

    res <- list(par = par, dev.size = dev.size)

    return(res)

}

