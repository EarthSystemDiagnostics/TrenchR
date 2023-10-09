#' Trench signal-to-noise ratio
#'
#' Estimate the signal-to-noise ratio (SNR) for a trench variable from the
#' average pairwise correlations evaluated at specified profile distances. To
#' allow for more flexibility, the trench data can be passed in the generic
#' package form, or as a simple numeric matrix or data frame where columns
#' represent the horizontal trench profile positions and rows represent the
#' vertical scale of the data.
#'
#' @param distances a vector of inter-profile distances at which average
#'   pairwise profile correlations are to be estimated in order to calculate the
#'   SNR.
#' @param .var character string with the name of the trench variable for which
#'   to estimate the SNR; see also \code{\link{make2D}}. Only needed when the
#'   \code{data} is passed as a generic trench data set.
#' @param a1 horizontal autocorrelation of the trench data at lag 1, where lag 1
#'   is measured relative to unit profile distance, used for the error estimate
#'   (see \code{\link{estimateInterProfileCorrelation}}).
#' @param rangeTol relative tolerance allowed for inter-profile distances (see
#'   \code{\link{estimateInterProfileCorrelation}}).
#' @inheritParams estimateInterProfileCorrelation
#' @importFrom rlang .data
#' @return a tibble of two variables: estimated signal-to-noise ratio
#'   (\code{snr}) and its standard error (\code{se}).
#' @author Thomas Münch
#' @examples
#'
#' distances <- c(10, 20, 30)
#' estimateSNR(t15.trench1, distances, a1 = exp(-1 / 1.53))
#' estimateSNR(t15.trench1, distances, a1 = exp(-1 / 1.53), .var = "dxs")
#'
#' @export
#'
estimateSNR <- function(data, distances, profilePosition = NULL, .var = "d18O",
                        a1 = 0, rangeTol = 0.05, ...) {

  data %>%
    estimateInterProfileCorrelation(distances, profilePosition,
                                    .var, rangeTol, a1) %>%
    dplyr::summarise(cor = mean(.data$cor),
                     lim = sqrt(sum(.data$se^2)) / dplyr::n()) %>%
    dplyr::transmute(snr = .data$cor / (1 - .data$cor),
                     se  = .data$lim / (1 - .data$cor)^2)

}
