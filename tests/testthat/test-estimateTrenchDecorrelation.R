context("decorrelation-length-estimation")

test_that("gaussian kernel autocorrelation estimation works", {

  pos <- 1 : 11
  x <- rnorm(length(pos))

  # test error checks

  m <- "nexcf: Supply positions on which `x` is tabulated."
  expect_error(nexcf(x), m, fixed = TRUE)

  m <- "nexcf: Length of data does not match length of `pos`."
  expect_error(nexcf(x, 1 : 10), m, fixed = TRUE)

  m <- "nexcf: Missing values present in `pos`."
  expect_error(nexcf(x, c(1 : 6, NA, 8 : 11)), m, fixed = TRUE)

  m <- "nexcf: Missing values present in `lag`."
  expect_error(nexcf(x, 1 : 11, lag = c(0, NA, 2)), m, fixed = TRUE)

  m <- "nexcf: All lags must be >= 0."
  expect_error(nexcf(x, pos, lag = -1), m, fixed = TRUE)
  expect_error(nexcf(x, pos, lag = c(-2, -1, 0, 1, 2)), m, fixed = TRUE)

  m <- "nexcf: `h` is NA."
  expect_error(nexcf(x, pos, h = NA), m, fixed = TRUE)

  m <- "nexcf: `h` must be > 0."
  expect_error(nexcf(x, pos, h = -0.2), m, fixed = TRUE)

  m <- "nexcf: Could not estimate any autocorrelation values."
  expect_error(nexcf(rep(1, length(pos)), pos, lag = 0 : 5), m, fixed = TRUE)

  # test return type

  expect_type(nexcf(x, pos), "double")
  expect_type(nexcf(x, pos, lag = 0 : 5), "double")

  expect_length(nexcf(x, pos), 1)
  expect_length(nexcf(x, pos, lag = 0 : 4), 5)
  expect_length(nexcf(x, pos, lag = 1 : 3), 3)

  # test normalization of autocorrelation values

  expect_lt(a0 <- nexcf(x, pos, lag = 0), 1)
  expect_equal(nexcf(x, pos, lag = 0 : 1)[1], 1)

  ax <- nexcf(x, pos, lag = 1 : 5)
  expect_equal(nexcf(x, pos, lag = 0 : 5), c(1, ax / a0))

  # result should be independent of lag order

  expect_equal(nexcf(x, pos, lag = 0 : 5), rev(nexcf(x, pos, lag = 5 : 0)))

  # test removal of NA values

  m <- "nexcf: Too many NAs to estimate autocorrelation."
  expect_warning(a1 <- nexcf(NA), m, fixed = TRUE)
  expect_equal(a1, NA)

  x <- rnorm(7)
  pos <- c(1, 2.3, 2.78, 3.3, 4.5, 5.03, 6.7)

  expected <- nexcf(x, pos, lag = c(0, 1))[2]

  x.withNA <- c(x[1 : 3], NA, x[4 : 7], NA)
  pos.withNA <- c(pos[1 : 3], 3, pos[4 : 7], 7)

  actual <- nexcf(x.withNA, pos.withNA, lag = c(0, 1))[2]

  expect_equal(actual, expected)

  # test special case of T13-1 trench data to ensure consistency

  trench <- t13.trench1 %>%
    dplyr::filter(depth >= getFirstCompleteDepthBin(t13.trench1)) %>%
    dplyr::filter(profileName != "T13-1-01") %>%
    make2D(simplify = TRUE)

  pos <- getX(t13.trench1)[-1]

  actual <- trench %>%
    apply(1, nexcf, pos = pos, lag = 0 : 8) %>%
    rowMeans() %>%
    round(6)

  expected <- c(1.000000, 0.538069, 0.330319, 0.157260, 0.054609,
                0.126455, -0.020411, 0.022563, -0.079460)

  expect_equal(actual, expected)

  # test ordering

  i <- sample(seq(ncol(trench)), ncol(trench))
  actual <- trench[, i] %>%
    apply(1, nexcf, pos = pos[i], lag = 0 : 8) %>%
    rowMeans() %>%
    round(6)

  expect_equal(actual, expected)  
  
})

test_that("lag-1 autocorrelation estimation works", {

  # test error checks

  expect_error(calcEquidistantAC1(1, 1, 1),
               "`x` must be a matrix.", fixed = TRUE)
  expect_error(calcEquidistantAC1(matrix(NA, 5, 10), 1, 1),
               "`x.no.surface` must be a matrix.", fixed = TRUE)

  m <- "Dimensions of `x` and `x.no.surface` do not match."
  expect_error(calcEquidistantAC1(matrix(NA, 5, 10), matrix(NA, 5, 7), 1),
               m, fixed = TRUE)
  expect_error(calcEquidistantAC1(matrix(NA, 5, 10), matrix(NA, 5, 13), 1),
               m, fixed = TRUE)
  expect_error(calcEquidistantAC1(matrix(NA, 5, 10), matrix(NA, 8, 10), 1),
               m, fixed = TRUE)

  m <- "`direction` must be set to `1` or `2`."
  expect_error(calcEquidistantAC1(matrix(NA, 5, 10), matrix(NA, 2, 10), -1),
               m, fixed = TRUE)
  expect_error(calcEquidistantAC1(matrix(NA, 5, 10), matrix(NA, 2, 10), 6.2),
               m, fixed = TRUE)

  # ----------------------------------------------------------------------------
  # case I: evenly spaced data

  # I-a no NAs

  nr <- 10
  nc <- 3

  x <- sapply(seq(nc), rnorm, n = nr)

  av <- apply(x, 2, function(xx) {
    stats::acf(xx, lag.max = 1, plot = FALSE)$acf[2]}) %>%
    mean()

  ah <- apply(x, 1, function(xx) {
    stats::acf(xx, lag.max = 1, plot = FALSE)$acf[2]}) %>%
    mean()

  actual <- calcEquidistantAC1(x, x, direction = 2)
  expect_equal(actual, av)

  actual <- calcEquidistantAC1(x, x, direction = 1)
  expect_equal(actual, ah)

  # I-b internal NAs

  x[6, 2] <- NA
  x[9, 1] <- NA

  av <- stats::acf(x[, -c(1, 2)], lag.max = 1, plot = FALSE)$acf[2]

  ah <- apply(x[-c(6, 9), ], 1, function(xx) {
    stats::acf(xx, lag.max = 1, plot = FALSE)$acf[2]}) %>%
    mean()

  expect_warning(
    actual <- calcEquidistantAC1(x, x, direction = 2),
    "2 column(s) removed containing NA values.", fixed = TRUE)
  expect_equal(actual, av)

  expect_warning(
    actual <- calcEquidistantAC1(x, x, direction = 1),
    "2 row(s) removed containing NA values.", fixed = TRUE)
  expect_equal(actual, ah)

  # I-c NAs from incomplete surface region (-> should only affect horizontal AC)

  ns1 <- 0
  ns2 <- 1
  ns3 <- 3
  x1 <- rnorm(nr - ns1)
  x2 <- rnorm(nr - ns2)
  x3 <- rnorm(nr - ns3)

  x <- cbind(c(rep(NA, ns1), x1), c(rep(NA, ns2), x2), c(rep(NA, ns3), x3))

  av <- c(stats::acf(x1, lag.max = 1, plot = FALSE)$acf[2],
          stats::acf(x2, lag.max = 1, plot = FALSE)$acf[2],
          stats::acf(x3, lag.max = 1, plot = FALSE)$acf[2]
          ) %>%
    mean()

  ns <- max(c(ns1, ns2, ns3))
  ah <- x[-(1 : ns), ] %>%
    apply(1, function(xx) {
      stats::acf(xx, lag.max = 1, plot = FALSE)$acf[2]}) %>%
    mean()

  actual <- calcEquidistantAC1(x, x[-(1 : ns), ], direction = 2)
  expect_equal(actual, av)

  actual <- calcEquidistantAC1(x, x[-(1 : ns), ], direction = 1)
  expect_equal(actual, ah)

  # I-d NAs from incomplete surface region + internal NAs

  i <- 8
  x2[i] <- NA
  x <- cbind(c(rep(NA, ns1), x1), c(rep(NA, ns2), x2), c(rep(NA, ns3), x3))

  av <- c(stats::acf(x1, lag.max = 1, plot = FALSE)$acf[2],
          stats::acf(x3, lag.max = 1, plot = FALSE)$acf[2]
          ) %>%
    mean()

  ah <- x[-c(1 : ns, i + ns2), ] %>%
    apply(1, function(xx) {
      stats::acf(xx, lag.max = 1, plot = FALSE)$acf[2]}) %>%
    mean()

  suppressWarnings(
    actual <- calcEquidistantAC1(x, x[-(1 : ns), ], direction = 2))
  expect_equal(actual, av)

  suppressWarnings(
    actual <- calcEquidistantAC1(x, x[-(1 : ns), ], direction = 1))
  expect_equal(actual, ah)

})
