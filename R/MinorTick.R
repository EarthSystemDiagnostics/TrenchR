##' Make aesthetic minor ticks.
##'
##' Description.
##' @param nx 
##' @param ny 
##' @param tick.ratio 
##' @param side 
##' @param col 
##' @author Thomas Münch
##' @export
MinorTick <- function(nx = 2,ny = 2, tick.ratio = 0.5,
                      side = 1, col = "black") {
    ## +++ adapted after function from package 'Hmisc' +++ ##

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
