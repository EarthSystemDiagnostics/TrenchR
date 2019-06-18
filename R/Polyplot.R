##' Draw error shading.
##'
##' A wrapper function for the \code{polygon} function to draw error shadings
##' (or confidence intervals) on a line plot.
##' @param x numeric vector of x values of the error band.
##' @param y1 numeric vector for the upper bound of the error band; must be of
##' the same length as \code{x}.
##' @param y2 numeric vector for the lower bound of the error band; must be of
##' the same length as \code{x}.
##' @param col colour of the error band.
##' @param alpha opacity factor for \code{col} within [0,1].
##' @param ... additional parameters which are passed to \code{polygon}.
##' @seealso \code{\link{polygon}}
##' @author Thomas Münch
##' @examples
##' x <- 1 : 10
##' plot(x, type = "n", xlab = "x", ylab = "y")
##' Polyplot(x, y1 = x + 2, y2 = x - 2)
##' lines(x, lwd = 2)
Polyplot <- function(x, y2, col = "grey", ...) {
    
    polygon(c(x, rev(x)), c(y2[2, ], rev(y2[1, ])),
            col = col, border = NA, ...)

}
