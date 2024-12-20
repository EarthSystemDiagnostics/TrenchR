#' Bin summary values
#'
#' Calculate summary values of a trench mean profile partitioned into
#' differently sized bins. Possible summary operations are the mean value,
#' e.g. to calculate annual mean values based on a given depth-age relationship,
#' or minimum and maximum values.
#'
#' @param x a data frame with two columns supplying a trench mean profile
#'   (e.g. obtained from \code{\link{makeMean}}): the first column provides the
#'   vertical scale and the second column the proxy values.
#' @param bins a data frame with two columns supplying the break point positions
#'   defining the bins: the first column provides the break points in units of
#'   the vertical scale (e.g. \code{"depth"}) and the second column provides
#'   corresponding label values for the bins (e.g. the years for a depth-age
#'   relationship).
#' @param summary character string for the summary operation to perform on each
#'   bin: default \code{"mean"} is to calculate the mean value across each bin;
#'   other options are \code{"min"} and \code{"max"} for the minimum and maximum
#'   values, respectively.
#' @param right logical; indicate whether the bins should be closed on the right
#'   and open on the left (\code{TRUE}, the default), or vice versa
#'   (\code{FALSE}). For \code{n} break point positions (i.e. = the number of
#'   rows of \code{bins}), there are \code{n-1} bin intervals with corresponding
#'   summary values. If the bins are closed on the right, the label values 2 : n
#'   in \code{bins} are used to label the output; if the bins are closed on the
#'   left, the first \code{n-1} label values are used instead.
#' @param label.name optional name for the label value column in the output data
#'   frame; if none is specified (the default) "label" is used.
#' @param na.rm logical; indicate whether \code{NA} values should be stripped
#'   before the computation of the bin summary proceeds.
#'
#' @return a tibble with two columns and \code{n-1} rows, where the first column
#'   provides the bin label values and the second column the proxy summary
#'   values for those bins.
#'
#' @author Thomas Münch
#' @examples
#'
#' # bin average for T13-1, but with completely random bins
#'
#' x <- makeMean(t13.trench1)
#' bins <- data.frame(depth = c(1.5, 22.5, 46.5, 88.5), bin_no = 1 : 4)
#' 
#' summarizeBin(x, bins)
#' summarizeBin(x, bins, na.rm = TRUE)
#'
#' summarizeBin(x, bins, na.rm = TRUE,
#'              right = FALSE, label.name = "year")
#'
#' # winter d-excess minima from actual dating
#'
#' bins <- data.frame(depth = c(1.5, 28.5, 49.5, 67.5, 94.5),
#'                    year = 2013 : 2009)
#'
#' library(magrittr)
#' makeMean(t13.trench1, .var = "dxs", na.rm = TRUE) %>%
#'   summarizeBin(bins, summary = "min")
#' 
#' @export
#'
summarizeBin <- function(x, bins, summary = "mean", right = TRUE,
                         label.name = "", na.rm = FALSE) {

  if (!is.data.frame(x)) stop("`x` must be a data frame.", call. = FALSE)
  if (ncol(x) != 2) stop("`x` may only have two columns.", call. = FALSE)

  if (!is.data.frame(bins)) stop("`bins` must be a data frame.", call. = FALSE)
  if (ncol(bins) != 2) stop("`bins` may only have two columns.", call. = FALSE)

  # select summary function
  .fun <- match.arg(summary, c("mean", "min", "max")) %>%
    match.fun()

  if (!nchar(label.name)) label.name <- "label"

  # proxy variable name
  .var <- names(x)[2]
  
  # name columns and sort break points
  names(x) <- c("x", "y")
  names(bins) <- c("breaks", "label")
  bins <- bins[order(bins$breaks), ]

  # left/right-open determines bin label values
  bin.labels <- bins$label[-1] # for left-open
  if (!right)
    bin.labels <- bins$label[1 : (length(bins$label) - 1)] # for right-open

  x %>%
    dplyr::mutate(bin = cut(.data$x, breaks = bins$breaks,
                            right = right)) %>%
    dplyr::group_by(.data$bin) %>%
    dplyr::summarize(summary = .fun(.data$y, na.rm = na.rm)) %>%
    dplyr::filter(!is.na(.data$bin)) %>%
    dplyr::pull("summary") %>%
    tibble::tibble(bin.labels, .) %>%
    stats::setNames(c(label.name, .var))

}
