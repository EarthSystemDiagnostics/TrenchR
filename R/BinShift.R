##' Shift a vector.
##'
##' .. content for \details{} ..
##' @param k 
##' @param vec 
##' @return 
##' @author Thomas Münch
##' @export
BinShift <- function(k, vec) {
    
    if (k == 0)
        vec.new <- vec
    if (k < 0)
        vec.new <- c(vec[-(1 : abs(k))], rep(NA, abs(k)))
    if (k > 0)
        vec.new <- c(rep(NA, k), vec[-(length(vec) : (length(vec) - (k - 1)))])

    return(vec.new)

}

