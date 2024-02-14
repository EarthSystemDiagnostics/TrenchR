##
## Read Kohnen AWS9 weather station observational data and
## process it into the desired structure
##
## Thomas Muench, AWI, 02/2024
##

# required packages

library(rlang)

rlang::check_installed(c("plyr", "dplyr", "usethis"),
                       version = c("1.8.9", "1.1.3", NA))
library(magrittr) # installed by dplyr

# ------------------------------------------------------------------------------
# Read data and store in package

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

usethis::use_data(aws9, internal = TRUE, overwrite = TRUE)
