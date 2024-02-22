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
# Save package data
# ==============================================================================

usethis::use_data(aws9, ParamSpace, tc17.paper.param, tc17.modif.param,
                  internal = TRUE, overwrite = TRUE)
