##' T13 annual-mean isotope time series.
##'
##' Calculate annual-mean time series of the T13 trench isotope records defined
##' by binning the records according to the isotopic maxima and minima. From
##' this, four different time series are created: averages from bins defined by
##' (1) the summer maxima, (2) the winter minima, (3) the midpoints of the
##' ascending slopes flanking the maxima and (4) the midpoints of the descending
##' slopes.
##' @param t1 numeric vector of the T13--1 isotope record. 
##' @param t2 numeric vector of the T13--2 isotope record.
##' @param depth depth numeric vector of the common T13 depth scale (i.e. after
##' the optimal shift of T13--2 to maximise the inter-trench correlation; see
##' Münch et al. (2016)).
##' @param i.max index positions of the summer maxima of the trench mean isotope
##' record; default indices are the ones used in Münch et al. (2016).
##' @param i.min index positions of the winter minima of the trench mean isotope
##' record; default indices are the ones used in Münch et al. (2016).
##' @param start.year assumed year of the first summer maximum where the maximum
##' is defined to occur in January of the year; defaults to \code{2013}.
##' @return A list of three data frames:
##' \describe{
##'   \item{means:}{the annual-mean time series of the T13--1, T13--2 and mean
##'   T13 isotope records together with the range of annual means from the
##'   different binning methods;}
##'   \item{summer.max:}{the annual time series of summer maximum values for the
##'   T13--1, T13--2 and mean T13 isotope records;}
##'   \item{winter.min:}{the annual time series of winter minimum values for the
##'   T13--1, T13--2 and mean T13 isotope records.}
##' }
##' @author Thomas Münch
##' @references
##' Münch, T., et al., Regional climate signal vs. local noise: a
##' two-dimensional view of water isotopes in Antarctic firn at Kohnen Station,
##' Dronning Maud Land, Clim. Past, doi:10.5194/cp-12-1565-2016, 2016.
T13AnnualMeans <- function(t1, t2, depth,
                           i.max = c(5, 11, 19, 24, 33, 38),
                           i.min = c(7, 14, 21, 26, 37),
                           start.year = 2013) {
    
    ind <- list()
    ind$summer <- i.max
    ind$winter <- i.min
    for (i in 1 : (length(ind$summer) - 1)) {
        ind$flk.up[i]   <- round(mean(c(ind$summer[i], ind$winter[i])))
        ind$flk.dwn[i]  <- round(mean(c(ind$summer[i + 1], ind$winter[i])))
    }
    
    summer.max <- matrix(nrow = length(ind$summer), ncol = 5)
    summer.max[, 1] <- start.year : (start.year - length(ind$summer) + 1)
    summer.max[, 2] <- depth[ind$summer]
    summer.max[, 3] <- t1[ind$summer]
    summer.max[, 4] <- t2[ind$summer]
    summer.max[, 5] <- rowMeans(cbind(summer.max[, 3], summer.max[, 4]))

    winter.min <- matrix(nrow = length(ind$winter), ncol = 5)
    winter.min[, 1] <- (start.year - 1) :
                               ((start.year - 1) - length(ind$winter) + 1)
    winter.min[, 2] <- depth[ind$winter]
    winter.min[, 3] <- t1[ind$winter]
    winter.min[, 4] <- t2[ind$winter]
    winter.min[, 5] <- rowMeans(cbind(winter.min[, 3], winter.min[, 4]))

    colnames(summer.max) <- c("years", "depth", "T1", "T2", "mean")
    colnames(winter.min) <- c("years", "depth", "T1", "T2", "mean")

    means <- matrix(nrow = length(ind$summer) - 1, ncol = 11)

    tmp1 <- cbind(
        c(YearMean(t1, ind$summer)),
        c(YearMean(t1, ind$winter), NA),
        c(YearMean(t1, ind$flk.up), NA),
        c(NA, YearMean(t1, ind$flk.dwn)))

    tmp2 <- cbind(
        c(YearMean(t2, ind$summer)),
        c(YearMean(t2, ind$winter), NA),
        c(YearMean(t2, ind$flk.up), NA),
        c(NA, YearMean(t2, ind$flk.dwn)))

    tmp <- array(dim = c(dim(tmp1), 2))
    tmp[, , 1] <- tmp1
    tmp[, , 2] <- tmp2

    trench.mean <- apply(tmp, c(1, 2), mean, na.rm = TRUE)
    
    means[, 1] <- summer.max[, 1][-1]
    means[, 2] <- depth[ind$summer[-length(ind$summer)]] +
        0.5 * diff(depth[ind$summer])

    means[, 3] <- rowMeans(tmp1, na.rm = TRUE)
    means[, 4] <- rowMeans(tmp2, na.rm = TRUE)
    means[, 5] <- rowMeans(trench.mean, na.rm = TRUE)

    means[, c(6, 7)]   <- matrix(apply(tmp1, 1, range, na.rm = TRUE),
                                       nrow = length(ind$summer) - 1, ncol = 2,
                                       byrow = TRUE)
    means[, c(8, 9)]   <- matrix(apply(tmp2, 1, range, na.rm = TRUE),
                                       nrow = length(ind$summer) - 1, ncol = 2,
                                       byrow = TRUE)
    means[, c(10, 11)] <- matrix(apply(trench.mean, 1, range, na.rm = TRUE),
                                       nrow = length(ind$summer) - 1, ncol = 2,
                                       byrow = TRUE)

    colnames(means) <- c("years", "depth", "T1", "T2", "total",
                               "min.T1", "max.T1", "min.T2", "max.T2",
                               "min.total", "max.total")

    res <- list(means = as.data.frame(means),
                summer.max = as.data.frame(summer.max),
                winter.min = as.data.frame(winter.min))
    return(res)

}
