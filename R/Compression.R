##' Compression by linear densification.
##'
##' .. content for \details{} ..
##' @param z1 
##' @param z2 
##' @param rate 
##' @return 
##' @author Thomas Münch
Compression <- function(z1, z2, rate) {
    
    return((1 + rate * z1)/(1 + rate * z2))
}
