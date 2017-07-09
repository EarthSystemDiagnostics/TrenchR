##' Wrapper for polygon plot.
##'
##' Description.
##' @param x x
##' @param y2 y2
##' @param col colour.
##' @param ... Further graphical parameters passed to the \code{polygon}
##'     function.
##' @author Thomas Münch
Polyplot <- function(x, y2, col = "grey", ...) {
    
    polygon(c(x, rev(x)), c(y2[2, ], rev(y2[1, ])),
            col = col, border = NA, ...)

}
