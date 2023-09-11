##
## Read Kohnen T13 and T15 profile data and
## process it into the package data structure
##
## Thomas Muench, AWI, 09/2023
##

# required packages
library(magrittr)

library(dplyr)   # v>=1.1.3
library(fs)      # v>=1.6.3
library(purrr)   # v>=1.0.2
library(stringr) # v>=1.5.0
library(tidyr)   # v>=1.3.0
library(usethis) # v>=2.2.1

# ------------------------------------------------------------------------------
# Library function to do the actual work

#' Read a single Kohnen trench profile given its meta information, including
#' linear interpolation of missing samples.
#'
#' @return a tibble
#' @author Thomas Münch
readTrenchProfile <- function(file, meta) {

  prfNm <- strsplit(file, "_")[[1]][3] %>%
    stringr::str_remove(".txt")

  profiles2interpolate <- c("T13-1-03", "T15-1-G", "T15-2-W",
                            "T15-2-X", "T15-2-Y")
  interpolate <- FALSE
  if (prfNm %in% profiles2interpolate) {
    interpolate <- TRUE
    intpl <- function(x, y, n = 1) {round(approx(x, y, x)$y, digits = n)}
  }

  profilePosition <- dplyr::filter(meta, profileName == prfNm) %>%
    dplyr::pull(`profilePos_.m.`)
  surfaceHeight <- dplyr::filter(meta, profileName == prfNm) %>%
    dplyr::pull(`profileSurfaceHeight_.cm.`)

  file %>%
    read.table(header = TRUE, sep = "\t", na.strings = "") %>%
    dplyr::as_tibble() %>%
    dplyr::rename(depth = depth_.cm., d18O = d18O_.permil.,
                  dD = d2H_.permil., dxs = dxs_.permil.) %>%
    dplyr::mutate(profileName = prfNm,
                  profilePosition = profilePosition,
                  surfaceHeight = surfaceHeight) %>%
    tidyr::separate_wider_delim(cols = sample, delim = "-",
                                names = c("t", "p", "sampleNumber")) %>%
    dplyr::mutate(sampleNumber = as.integer(sampleNumber)) %>%
    dplyr::select(profileName, profilePosition, surfaceHeight,
                  sampleNumber, depth, d18O, dD, dxs) %>%
    {
      if (interpolate) {
        dplyr::mutate(., d18O = intpl(depth, d18O, n = 2),
                      dD = intpl(depth, dD), dxs = intpl(depth, dxs))
      } else { . }
    }

}

# ------------------------------------------------------------------------------
# Read data and store in package

# set working directory to your package source folder
# setwd("<path>")

# read in meta information
meta_t13_1 <- read.table(file = "data-raw/t13/meta_trench1.txt", header = TRUE,
                         sep = "\t", na.strings = "")
meta_t13_2 <- read.table(file = "data-raw/t13/meta_trench2.txt", header = TRUE,
                         sep = "\t", na.strings = "")
meta_t15_1 <- read.table(file = "data-raw/t15/meta_trench1.txt", header = TRUE,
                         sep = "\t", na.strings = "")
meta_t15_2 <- read.table(file = "data-raw/t15/meta_trench2.txt", header = TRUE,
                         sep = "\t", na.strings = "")

# read the four Kohnen trenches
t13.trench1 <- fs::dir_ls(path = "data-raw/t13/", glob = "*_isotopes_T13-1-*.txt") %>%
  as.character() %>%
  purrr::map(readTrenchProfile, meta = meta_t13_1) %>%
  dplyr::bind_rows()
t13.trench2 <- fs::dir_ls(path = "data-raw/t13/", glob = "*_isotopes_T13-2-*.txt") %>%
  as.character() %>%
  purrr::map(readTrenchProfile, meta = meta_t13_2) %>%
  dplyr::bind_rows()

t15.trench1 <- fs::dir_ls(path = "data-raw/t15/", glob = "*_isotopes_T15-1-*.txt") %>%
  as.character() %>%
  purrr::map(readTrenchProfile, meta = meta_t15_1) %>%
  dplyr::bind_rows()
t15.trench2 <- fs::dir_ls(path = "data-raw/t15/", glob = "*_isotopes_T15-2-*.txt") %>%
  as.character() %>%
  purrr::map(readTrenchProfile, meta = meta_t15_2) %>%
  dplyr::bind_rows()

usethis::use_data(t13.trench1, overwrite = TRUE)
usethis::use_data(t13.trench2, overwrite = TRUE)
usethis::use_data(t15.trench1, overwrite = TRUE)
usethis::use_data(t15.trench2, overwrite = TRUE)

