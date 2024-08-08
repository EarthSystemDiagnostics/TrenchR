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

  # ----------------------------------------------------------------------------
  # case I: evenly spaced data

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


  # ----------------------------------------------------------------------------
  # case II: non-equidistantly spaced data

  # test error checks

  expect_error(calcNonEquidistantAC1(1, 1, 1),
               "`x` must be a matrix.", fixed = TRUE)

  m <- "`direction` must be set to `1` or `2`."
  expect_error(calcNonEquidistantAC1(matrix(NA, 5, 10), 1, -1),
               m, fixed = TRUE)
  expect_error(calcNonEquidistantAC1(matrix(NA, 5, 10), 1, 6.2),
               m, fixed = TRUE)

  m <- "Length of `pos` does not match requested dimension of data."
  expect_error(
    calcNonEquidistantAC1(matrix(NA, 5, 9), pos = 1 : 7, direction = 1),
    m, fixed = TRUE)
  expect_error(
    calcNonEquidistantAC1(matrix(NA, 5, 7), pos = 1 : 7, direction = 2),
    m, fixed = TRUE)

  m <- "`lag` needs to be of length 2."
  expect_error(
    calcNonEquidistantAC1(matrix(NA, 5, 7), pos = 1 : 5,
                          direction = 2, lag = 0),
    m, fixed = TRUE)
  expect_error(
    calcNonEquidistantAC1(matrix(NA, 5, 7), pos = 1 : 5,
                          direction = 2, lag = 0 : 5),
    m, fixed = TRUE)

  m <- "First element of `lag` must be 0."
  expect_error(
    calcNonEquidistantAC1(matrix(NA, 5, 7), pos = 1 : 5,
                          direction = 2, lag = 1 : 2),
    m, fixed = TRUE)

  # test on T13-2 data

  trench <- t13.trench2 %>% make2D(simplify = TRUE)

  # vertical autocorrelation
  # (could also be estimated with standard method due to equidistant sampling)
  av <- sapply(seq(ncol(trench)), function(i) {

    x <- as.numeric(na.omit(trench[, i]))
    pos <- seq(0, by = 3, length.out = length(x))

    nexcf(x, pos, lag = c(0, 3))[2]

  })

  expected <- mean(av)
  actual   <- calcNonEquidistantAC1(trench, pos = getZ(t13.trench2),
                                    direction = 2, lag = c(0, 3))

  expect_equal(actual, expected)

  # horizontal autocorrelation
  # (doesn't actually make sense since too few profile positions)
  pos <- getX(t13.trench2)
  ah <- sapply(4 : nrow(trench), function(i) {

    x <- trench[i, ]

    nexcf(x, pos, lag = c(0, 1))[2]

  })
  ah <- c(
    ah,
    nexcf(trench[2, -2], pos[-2], lag = c(0, 1))[2], # depth sample 2
    nexcf(trench[3, -2], pos[-2], lag = c(0, 1))[2]  # depth sample 3
  )

  expected <- mean(ah)
  suppressWarnings( # warning from removal of first row (too many NAs)
    actual <- calcNonEquidistantAC1(trench, pos = getX(t13.trench2),
                                    direction = 1)
  )

  expect_equal(actual, expected)

  # test case of T13-1 trench data (paper analysis) to ensure consistency
  # (see also nexcf test above)

  trench <- t13.trench1 %>%
    removeSurfaceRegion() %>%
    dplyr::filter(profileName != "T13-1-01") %>%
    make2D(simplify = TRUE)

  pos <- getX(t13.trench1)[-1]

  actual <- calcNonEquidistantAC1(trench, pos, direction = 1) %>%
    round(6)

  expect_equal(actual, 0.538069)

})

test_that("decorrelation length estimation works", {

  # case I: equidistant in vertical, non-equidistant in horizontal diretion

  x <- t13.trench1 %>% make2D(simplify = TRUE)
  x.no.surface <- t13.trench1 %>% removeSurfaceRegion() %>%
    make2D(simplify = TRUE)

  suppressWarnings({
    a1.h <- calcNonEquidistantAC1(x, pos = getX(t13.trench1), direction = 1)
    a1.v <- calcEquidistantAC1(x, x.no.surface, direction = 2)
    })

  expected <- tibble::tibble(
    direction = c("horizontal", "vertical"),
    lambda = c(-1 / log(a1.h), -3 / log(a1.v))
  )

  actual <- suppressWarnings(estimateTrenchDecorrelation(t13.trench1))

  expect_equal(actual, expected)

  # case I: non-equidistant in vertical, equidistant in horizontal diretion

  # remove rows to get non-equidistant vertical sampling
  n <- sort(sample(1 : length(getZ(t15.trench2)), size = 70))
  trench <- t15.trench2 %>% dplyr::slice(n, .by = "profilePosition")

  x <- trench %>% make2D(simplify = TRUE)
  x.no.surface <- trench %>% removeSurfaceRegion() %>%
    make2D(simplify = TRUE)

  a1.h <- calcEquidistantAC1(x, x.no.surface, direction = 1)
  a1.v <- calcNonEquidistantAC1(x, pos = getZ(trench), direction = 2)

  expected <- tibble::tibble(
    direction = c("horizontal", "vertical"),
    lambda = c(-5 / log(a1.h), -1 / log(a1.v))
  )

  actual <- estimateTrenchDecorrelation(trench)

  expect_equal(actual, expected)

})

test_that("in case of estimated a1 < 0 the user gets NA decorrelation", {

  # by chance, negative a1 estimation happens for the following data in
  # horizontal direction:
  x <- dplyr::filter(t15.trench2, depth <= 175.5)

  m <- "NA decorrelation length due to negative lag-1 autocorrelation estimate."
  expect_warning(actual <- estimateTrenchDecorrelation(x), m)

  # check that lambda is really NA and not NaN
  expect_true(is.na(as.character(actual$lambda[1])))
  expect_false(is.na(as.character(actual$lambda[2])))

  # also check with synthetic data
  n <- 1000
  x1 <- tibble::tibble(sampleNumber = 1 : n, depth = 1 : n, profilePosition = 0,
                       data = c(arima.sim(list(ar = -0.8), n)))
  x2 <- tibble::tibble(sampleNumber = 1 : n, depth = 1 : n, profilePosition = 1,
                       data = c(arima.sim(list(ar = -0.8), n)))
  x3 <- tibble::tibble(sampleNumber = 1 : n, depth = 1 : n, profilePosition = 2,
                       data = c(arima.sim(list(ar = -0.8), n)))

  trench <- dplyr::bind_rows(profile1 = x1, profile2 = x2, profile3 = x3,
                             .id = "profileName")

  expect_warning(estimateTrenchDecorrelation(trench, .var = "data"), m)
  expect_true(is.na(as.character(actual$lambda[1])))

})
