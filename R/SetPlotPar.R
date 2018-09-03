##' Load plotting parameters.
##'
##' This function returns a list of the plotting parameters specified as its
##' function arguments, which can then be set via a call to \code{par}. The
##' default function parameters are used for the plots of the TC paper;
##' additional parameters can be specified via \code{...}. This wrapper function
##' provides a convenient way to set new graphics parameters and save their old
##' values for later restoring at the same time; see the example.
##' @return A list of plotting parameters to be used with \code{par()}.
##' \itemize{
##'   \item mar = c(5, 5, 0.5, 0.5)
##'   \item lwd = 2
##'   \item las = 1
##'   \item font.lab = 2
##'   \item font.axis = 2
##'   \item cex.main = 1.5
##'   \item cex.lab = 1.5
##'   \item cex.axis = 1.25
##' }
##' @author Thomas Münch
##' @seealso \code{\link{par}}
##' @examples
##' op <- par(SetPlotPar())
##' plot(1 : 10, xlab = "X title", ylab = "Y title", type = "l")
##' par(op)
SetPlotPar <- function(mar = c(5, 5, 0.5, 0.5), lwd = 2, las = 1,
                       font.lab = 2, font.axis = 2, cex.main = 1.5,
                       cex.lab = 1.5, cex.axis = 1.25, ...) {

    par <- c(as.list(environment()), list(...))
    return(par)

}

