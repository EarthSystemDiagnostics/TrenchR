#' Retrieve summary information
#'
#' Summarize a trench dataset by retrieving information on the number and
#' distances of sampled profiles and on statistical data of a requested trench
#' variable: number of samples and their range, mean, and standard deviation.
#'
#' @param data a generic trench dataset. 
#' @param .var optional character string with the name of a trench variable for
#'   which summary information shall be retrieved; default setting only returns
#'   information on the sampled trench profiles, see the examples.
#' @return a tibble with one row.
#'
#' @author Thomas Münch
#' @examples
#'
#' summarizeTrench(t13.trench1)
#' summarizeTrench(t13.trench1, .var = "d18O")
#' summarizeTrench(t13.trench1, .var = "dD")
#' 
#' @export
#'
summarizeTrench <- function(data, .var = NULL) {

  is.trench(data, check = "incl.pos")

  profile.positions <- getX(data)

  nprof <- length(profile.positions)
  ran <- range(diff(profile.positions))

  res <- dplyr::tibble(Nprofiles = nprof, d_min = ran[1], d_max = ran[2])

  if (!is.null(.var)) {

    if (!.var %in% colnames(data)) {
      stop("Unknown column variable selected.", call. = FALSE)
    }

    x <- data[[.var]]

    res <- res %>%
      dplyr::mutate(
               .var = .var,
               Nsamples = length(na.omit(x)),
               min = min(x, na.rm = TRUE),
               max = max(x, na.rm = TRUE),
               mean = mean(x, na.rm = TRUE),
               sd = sd(x, na.rm = TRUE))

  }

  return(res)

}
