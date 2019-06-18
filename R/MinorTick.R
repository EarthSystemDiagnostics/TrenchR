##' Make aesthetic minor ticks.
##'
##' This is a modified version of the function \code{minor.tick} from the
##' package \code{Hmisc}. Main difference is preventing overplotting of existing
##' major ticks by the new minor ticks and allowing to use colored minor ticks.
##' @param nx number of intervals in which to divide the area between major tick
##' marks on the X-axis. Set to 1 to suppress minor tick marks.
##' @param ny same as ‘nx’ but for the Y-axis.
##' @param tick.ratio ratio of lengths of minor tick marks to major tick
##' marks. The length of major tick marks is retrieved from ‘par("tck")’.
##' @param side an integer specifying on which axis the minor ticks are to be
##' drawn on. The axis is denoted as follows: 1=below, 2=left, 3=above and
##' 4=right.
##' @param col color of the minor ticks.
##' @author Frank Harrell, Earl Bellinger, Viktor Horvath, Thomas Münch
##' @export
MinorTick <- function(nx = 2,ny = 2, tick.ratio = 0.5,
                      side = 1, col = "black") {

    ax <- function(w, n, tick.ratio, side = side) {
        tick.pos <- if (w == "x") 
            par("xaxp")
        else par("yaxp")
        distance.between.major <- (tick.pos[2] - tick.pos[1]) / tick.pos[3]
        low.major <- low.minor <- tick.pos[1]
        hi.major <- hi.minor <- tick.pos[2]
        major.ticks <- seq(low.minor, hi.minor, by = distance.between.major)
        minor.ticks <- seq(low.minor, hi.minor, by = distance.between.major / n)
        minor.ticks <-
            minor.ticks[-which(!is.na(match(minor.ticks, major.ticks)))]

        axis(side, minor.ticks, 
            labels = FALSE, tcl = par("tcl") * tick.ratio, col = col)
    }
    if (nx > 1) 
        ax("x", nx, tick.ratio = tick.ratio, side = side)
    if (ny > 1) 
        ax("y", ny, tick.ratio = tick.ratio, side = side)
    invisible()
}
