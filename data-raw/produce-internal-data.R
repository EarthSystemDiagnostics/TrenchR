##
## Produce internal TrenchR data
##
## Thomas Muench, AWI, 02/2024
##

# ==============================================================================
# I. Read Kohnen AWS9 weather station observational data and process it into the
#    desired structure
# ==============================================================================

# required packages

library(rlang)

rlang::check_installed(c("plyr", "dplyr", "usethis"),
                       version = c("1.8.9", "1.1.3", NA))
library(magrittr) # installed by dplyr

# ------------------------------------------------------------------------------
# read and process data

# set working directory to your package source folder
# setwd("<path>")

# read
aws.raw <- readRDS("data-raw/aws/aws_9_20632_HOUR_19971229-20140408.rds")

# handle missing values
aws.raw[which(aws.raw <= -999., arr.ind = TRUE)] <- NA

# process
aws.raw <- aws.raw %>%
  dplyr::mutate(
           dates.posix =
             as.POSIXct(paste(.data$Date, .data$Hour), tz = "UTC",
                        format = "%Y/%m/%d %H:%M:%S"),
           dates.julian =
             julian(.data$dates.posix,
                    origin = as.Date("1998/01/01", tz = "UTC")),
           t2m = .data$T,
           pressure = .data$P,
           rel.humid = .data$RH,
           spec.humid = .data$q,
           S.in = .data$Sin,
           S.out = .data$Sout,
           L.in = .data$Lin,
           L.out = .data$Lout,
           wind.speed = .data$WS,
           wind.dir = .data$WD,
           .keep = "none")

# calculate monthly and annual means
aws9 <- list(

  # monthly means
  monthly = aws.raw %>%
    dplyr::mutate(month = format(.data$dates.posix, "%Y-%m", tz = "UTC")) %>%
    plyr::ddply("month", plyr::colwise(mean), na.rm = TRUE) %>%
    dplyr::select("month", "dates.posix", "t2m"),

  # annual means
  annual = aws.raw %>%
    dplyr::mutate(year = format(.data$dates.posix, "%Y", tz = "UTC")) %>%
    plyr::ddply("year", plyr::colwise(mean), na.rm = TRUE) %>%
    dplyr::select("year", "dates.posix", "t2m")

)


# ==============================================================================
# II. Run the analysis for Fig. (5) in Münch et al. (2017)
# ==============================================================================

# required packages

library(TrenchR)

if (!require(FirnR))
  stop("Package 'FirnR' needed. ",
       "It is available on request from the 'TrenchR' package author(s).")
rlang::check_installed("usethis", version = NA)

# load data

TR <- TrenchR:::makeHiResKohnenTrenches(.var = "d18O")

# calculate

ParamSpace <- FirnR::LoopRecordModifications(
  record = TR$mean13_HiRes, reference = TR$mean15_HiRes,
  advection = seq(40, 60, 0.5),
  sigma = seq(0, 8, 0.1),
  compression = seq(0, 10, 0.1)
  )


# ==============================================================================
# III. Store analysis parameters used for Münch et al. (2017) paper
# ==============================================================================

# loRes:      original T13 and T15 depth sampling resolution [cm];
# hiRes:      higher depth resolution to interpolate trench data [cm];
# k13:        optimal vertical shift [cm] of the mean T13-2 relative to the
#             mean T13-1 profile;
# k15:        optimal vertical shift [cm] of the mean T15-2 relative to the
#             mean T15-1 profile;
# ix:         index vector for the T15 depth range analysed in the paper;
# surfaceBot: the depth of the trench surface layers from which onwards a full
#             horizontal data set is available; also given for the mean T13 and
#             T15 data.

tc17.paper.param <- list(
  loRes = 3,
  hiRes = 0.5,
  k13 = 3,
  k15 = -0.5,
  ix = 1 : 59,
  surfaceBot = c(
    t13.1 = getFirstCompleteDepthBin(t13.trench1),
    t13.2 = getFirstCompleteDepthBin(t13.trench2),
    t15.1 = getFirstCompleteDepthBin(t15.trench1),
    t15.2 = getFirstCompleteDepthBin(t15.trench2))
)

tc17.paper.param$surfaceBot <- c(
  tc17.paper.param$surfaceBot,
  t13 = max(tc17.paper.param$surfaceBot["t13.1"],
            tc17.paper.param$surfaceBot["t13.2"]) + tc17.paper.param$k13,
  t15 = max(tc17.paper.param$surfaceBot["t15.1"],
            tc17.paper.param$surfaceBot["t15.2"])
)


# ==============================================================================
# IV. Store temporal change parameters used in Münch et al. (2017) paper
# ==============================================================================

# ADV.*:     2-yr downward advection [cm];
# SIGMA.*:   2-yr differential diffusion length for oxygen isotopes [cm];
# STRETCH.*: 2-yr compression from densification [cm].

tc17.modif.param <- list(

  # optimal parameters from parameter space analysis (II. above)
  ADV.opt = unname(ParamSpace$optimum["advection"]),
  SIGMA.opt = unname(ParamSpace$optimum["sigma"]),
  STRETCH.opt = unname(ParamSpace$optimum["compression"]),

  # independent parameter estimations
  ADV.ind = 50, # from snow stake measurements
  SIGMA.ind = data.frame(
    depth = FirnR::b41.b42.density$stack$depth,
    sigma = FirnR::DiffusionLength(
                     FirnR::b41.b42.density$stack$depth,
                     FirnR::b41.b42.density$stack$fitDensity,
                     P = 650, T = 228.5, bdot = 64)
  ) %>%
    FirnR::GetDifferentialDiffusion(0, 1, 0.5, 1.5) %>%
    round(1),
  STRETCH.ind = FirnR::EstimateCompression(top.depth = 0, length = 1,
                                           advection = 0.5, rate = 0.046) %>%
    {{.} * 1e2} %>%
    round(1),

  # best match for only advection
  ADV.only = ParamSpace$advection[which.min(ParamSpace$RMSD[, 1, 1])]

)


# ==============================================================================
# V. Calculate T13 annual-mean isotope time series
#    (CP2016 Fig.05 and TC2017 Fig.01)
# ==============================================================================

rlang::check_installed("prxytools", version = "0.4.0")

#' Calculate annual-mean time series of the T13 trench isotope records defined
#' by binning the records according to the isotopic maxima and minima. From
#' this, four different time series are created: averages from bins defined by
#' (1) the summer maxima, (2) the winter minima, (3) the midpoints of the
#' ascending slopes flanking the maxima and (4) the midpoints of the descending
#' slopes.
#'
#' Originally, the annual mean isotope data have been erroneously calculated
#' such that the last value contributing to annual bin \code{i} was also
#' included in bin \code{i + 1}, due to an erroneous implementation within the
#' bin averaging function. This bug has been fixed. For \code{cheat = TRUE}, the
#' original annual mean data using this erroneous bin definition can be
#' reproduced, while for \code{cheat = FALSE}, the correct implementation is
#' used. The difference in the annual mean data between the two versions is,
#' however, minor, and it thus does not influence any results or conclusions of
#' Münch et al. (2017).
#'
#' @param t1 numeric vector of the T13--1 isotope record.
#' @param t2 numeric vector of the T13--2 isotope record.
#' @param depth numeric vector of the common T13 depth scale (i.e. after
#' the optimal shift of T13--2 to maximise the inter-trench correlation).
#' @param i.max index positions of the summer maxima.
#' @param i.min index positions of the winter minima.
#' @param start.year assumed year of the first summer maximum where the maximum
#' is defined to occur in January of the year.
#' @param cheat logical; for \code{TRUE} the paper data is reproduced.
#'
#' @return A list of three data frames:
#'   means: the annual-mean time series of the T13--1, T13--2 and mean T13
#'     isotope records together with the range of annual means from the
#'     different binning methods.
#'   summer.max: the annual time series of summer maximum values for the T13--1,
#'     T13--2 and mean T13 isotope records.
#'   winter.min: the annual time series of winter minimum values for the T13--1,
#'     T13--2 and mean T13 isotope records.
#'
T13AnnualMeans <- function(t1, t2, depth, i.max, i.min, start.year, cheat) {

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

  if (!cheat) {

    # calculate means with proper, non-overlapping bin definition

    tmp1 <- cbind(
      c(prxytools::AverageByIndex(t1, ind$summer)),
      c(prxytools::AverageByIndex(t1, ind$winter), NA),
      c(prxytools::AverageByIndex(t1, ind$flk.up), NA),
      c(NA, prxytools::AverageByIndex(t1, ind$flk.dwn)))

    tmp2 <- cbind(
      c(prxytools::AverageByIndex(t2, ind$summer)),
      c(prxytools::AverageByIndex(t2, ind$winter), NA),
      c(prxytools::AverageByIndex(t2, ind$flk.up), NA),
      c(NA, prxytools::AverageByIndex(t2, ind$flk.dwn)))

  } else {

    # calculate means with overlapping bins to reproduce the paper figure

    # pad records with NA to facilitate using bug-fixed averaging function
    t1.cheat <- c(t1, rep(NA, 10))
    t2.cheat <- c(t2, rep(NA, 10))

    j <- ind$summer
    n <- length(j) - 1
    x1.1 <- x1.2 <- numeric(n)
    for (i in 1 : n) {
      x1.1[i] <- prxytools::AverageByIndex(t1.cheat, c(j[i], j[i + 1] + 1))
      x1.2[i] <- prxytools::AverageByIndex(t2.cheat, c(j[i], j[i + 1] + 1))
    }

    j <- ind$winter
    n <- length(j) - 1
    x2.1 <- x2.2 <- numeric(n)
    for (i in 1 : n) {
      x2.1[i] <- prxytools::AverageByIndex(t1.cheat, c(j[i], j[i + 1] + 1))
      x2.2[i] <- prxytools::AverageByIndex(t2.cheat, c(j[i], j[i + 1] + 1))
    }

    j <- ind$flk.up
    n <- length(j) - 1
    x3.1 <- x3.2 <- numeric(n)
    for (i in 1 : n) {
      x3.1[i] <- prxytools::AverageByIndex(t1.cheat, c(j[i], j[i + 1] + 1))
      x3.2[i] <- prxytools::AverageByIndex(t2.cheat, c(j[i], j[i + 1] + 1))
    }

    j <- ind$flk.dwn
    n <- length(j) - 1
    x4.1 <- x4.2 <- numeric(n)
    for (i in 1 : n) {
      x4.1[i] <- prxytools::AverageByIndex(t1.cheat, c(j[i], j[i + 1] + 1))
      x4.2[i] <- prxytools::AverageByIndex(t2.cheat, c(j[i], j[i + 1] + 1))
    }

    tmp1 <- cbind(x1.1, c(x2.1, NA), c(x3.1, NA), c(NA, x4.1))
    tmp2 <- cbind(x1.2, c(x2.2, NA), c(x3.2, NA), c(NA, x4.2))

  }

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

trPar <- tc17.paper.param

mean13.1 <- t13.trench1 %>%
  dplyr::filter(profileName != "T13-1-01") %>%
  makeMean(df = TRUE)
mean13.2 <- t13.trench2 %>%
  makeMean(df = TRUE) %>%
  dplyr::mutate(d18O = prxytools::Lag(.data$d18O, trPar$k13 / trPar$loRes))

depth <- mean13.1$depth

t1 <- mean13.1$d18O
t2 <- mean13.2$d18O

i.max <- c(5, 11, 19, 24, 33, 38)
i.min <- c(7, 14, 21, 26, 37)

start.year <- 2013

t13.annual.means <- list(
  correct = T13AnnualMeans(
    t1, t2, depth, i.max, i.min, start.year, cheat = FALSE),
  cheated = T13AnnualMeans(
    t1, t2, depth, i.max, i.min, start.year, cheat = TRUE)
)


# ==============================================================================
# Save package data
# ==============================================================================

usethis::use_data(aws9, ParamSpace, tc17.paper.param,
                  tc17.modif.param, t13.annual.means,
                  internal = TRUE, overwrite = TRUE)
