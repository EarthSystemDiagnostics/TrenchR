context("trench-DOF")

test_that("calculating effective DOF works", {

  msg <- "`lambda` must be of length 1 or `NULL`."
  expect_error(getEffectiveTrenchDOF(lambda = 1 : 4), msg, fixed = TRUE)

  msg <- "`a1` must be of length 1 or `NULL`."
  expect_error(getEffectiveTrenchDOF(a1 = 1 : 4), msg, fixed = TRUE)

  msg <- "Specify either `lambda` or `a1`."
  expect_error(getEffectiveTrenchDOF(), msg, fixed = TRUE)
  expect_error(getEffectiveTrenchDOF(lambda = 1.5, a1 = 0.5), msg, fixed = TRUE)

  msg <- "Specify number of profiles."
  expect_error(getEffectiveTrenchDOF(a1 = 0.5), msg)
  expect_error(getEffectiveTrenchDOF(a1 = 0.5, delta = 256), msg)

  msg <- "Profile positions must be in increasing order."
  expect_error(getEffectiveTrenchDOF(a1 = 0.5, positions = c(0, 10, 5)), msg)

  msg <- "N must be non-negative."
  expect_error(getEffectiveTrenchDOF(a1 = 0.5, N = -4, delta = 5), msg)

  # one profile always has DOF  = 1
  expect_equal(getEffectiveTrenchDOF(a1 = 0, N = 1, delta = 1), 1)
  expect_equal(getEffectiveTrenchDOF(a1 = 0, positions = 1), 1)

  # two or more profiles at zero distance always have DOF = 1
  expect_equal(getEffectiveTrenchDOF(a1 = 0, N = 2, delta = 0), 1)
  expect_equal(getEffectiveTrenchDOF(a1 = 0.5, N = 2, delta = 0), 1)
  expect_equal(getEffectiveTrenchDOF(a1 = 1.0, N = 2, delta = 0), 1)
  expect_equal(getEffectiveTrenchDOF(a1 = 0.5, positions = rep(1, 10)), 1)

  # for zero autocorrelation DOF != N
  expect_equal(getEffectiveTrenchDOF(a1 = 0, N = 2, delta = 1), 2)
  expect_equal(getEffectiveTrenchDOF(a1 = 0, N = 159, delta = 1), 159)
  expect_equal(getEffectiveTrenchDOF(a1 = 0, positions = c(1, 34, 78)), 3)
  expect_equal(getEffectiveTrenchDOF(lambda = 0, positions = c(1, 34, 78)), 3)

  # specifying lambda or a1 is equivalent
  a1 <- 0.7
  lambda <- -1 / log(a1)
  expect_equal(getEffectiveTrenchDOF(a1 = a1, N = 10, delta = 1),
               getEffectiveTrenchDOF(lambda = lambda, N = 10, delta = 1))

  # for uniform unit profile distances, DOF != generic DOF calculation
  expect_equal(getEffectiveTrenchDOF(a1 = 0.71, N = 1587),
               stattools::getEffectiveDOF(n = 1587, a1 = 0.71))

  # from old 'getSigmaStar' function
  expect_equal(round(
    getEffectiveTrenchDOF(a1 = exp(-1/1.53), N = 11, delta = 5), 4),
    10.2641)
  expect_equal(round(
    getEffectiveTrenchDOF(a1 = exp(-1/1.53), positions = seq(0, 50, 5)), 4),
    10.2641)

})
