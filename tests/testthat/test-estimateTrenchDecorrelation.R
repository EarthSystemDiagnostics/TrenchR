context("decorrelation-length-estimation")

test_that("gaussian kernel autocorrelation estimation works", {

  pos <- 1 : 11
  x <- rnorm(length(pos))

  # test error checks

  m <- "nexcf: No missing values allowed in `x`."
  expect_error(nexcf(NA), m, fixed = TRUE)
  expect_error(nexcf(c(NA, x[2 : 9], NA, NA), pos), m, fixed = TRUE)

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
