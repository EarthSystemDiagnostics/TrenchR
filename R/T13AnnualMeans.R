##' T13 annual mean isotope time series.
##'
##' Description.
##' @param t1 t1
##' @param t2 t2
##' @param depth depth 
##' @param i.max i.max
##' @param i.min i.min
##' @param start.year start.year
##' @return A list.
##' @author Thomas Muench
##' @export
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
