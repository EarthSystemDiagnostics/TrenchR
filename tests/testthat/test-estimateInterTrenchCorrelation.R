context("inter-trench-correlation")

test_that("inter-trench correlation works", {

  set.seed(20231005)

  a <- rnorm(100)
  b <- rnorm(100)
  c <- rnorm(100)
  d <- rnorm(100)
  e <- rnorm(100)
  f <- rnorm(100)

  t1 <- tibble::tibble(
    profileName = rep(letters[1 : 2], each = 100),
    sampleNumber = rep(1 : 100, times = 2),
    depth = rep(1 : 100, times = 2),
    foo = c(a, b))
  t2 <- tibble::tibble(
    profileName = rep(letters[3 : 6], each = 100),
    sampleNumber = rep(1 : 100, times = 4),
    depth = rep(1 : 100, times = 4),
    foo = c(c, d, e, f))

  expected1 <- matrix(
    data = c(cor(a, c), cor(a, d), cor(a, e), cor(a, f),
             cor(b, c), cor(b, d), cor(b, e), cor(b, f)),
    nrow = 2, ncol = 4, byrow = TRUE,
    dimnames = list(letters[1 : 2], letters[3 : 6]))

  actual1 <- estimateInterTrenchCorrelation(t1, t2, .var = "foo")
  expect_equal(expected1, actual1)

  expected2 <- list(cor = expected1,
                    i = matrix(0, nrow = 2, ncol = 4, byrow = TRUE,
                               dimnames = list(letters[1 : 2], letters[3 : 6])))
  actual2 <- estimateInterTrenchCorrelation(t1, t2, .var = "foo",
                                            optimize = TRUE, lag = 0)

  expect_error(
    estimateInterTrenchCorrelation(t1, t2, .var = "foo", optimize = TRUE),
    "Need vector of lags to optimize correlations.")
  expect_equal(expected2, actual2)

})
