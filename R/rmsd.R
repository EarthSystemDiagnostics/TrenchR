##' Root-mean-square deviation.
##'
##' This function calculates the root-mean square deviation of two numeric
##' vectors.
##' @param v1 Numeric vector for which to compute the rmsd with \code{v2}.
##' @param v2 Numeric vector for which to compute the rmsd with \code{v1}. Must
##'     have the same length as \code{v1}.
##' @param na.rm a logical value indicating whether ‘NA’ values should be
##'     stripped before the computation proceeds. Defaults to \code{FALSE}.
##' @return The root-mean square deviation of \code{v1} and \code{v2}, or
##'     \code{NA} if any of the elements of the vectors is \code{NA} and if
##'     \code{na.rm = FALSE}.
##' @author Thomas Münch
##' @export
rmsd <- function(v1, v2, na.rm = FALSE) {

    if (length(v1) != length(v2)) {
        stop("Arguments must have the same length.")
    }
    res <- sqrt(mean((v1 - v2)^2, na.rm = na.rm))
    
    return(res)

}
