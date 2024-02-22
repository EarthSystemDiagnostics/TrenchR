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
# Save package data
# ==============================================================================

usethis::use_data(aws9, ParamSpace, internal = TRUE, overwrite = TRUE)
