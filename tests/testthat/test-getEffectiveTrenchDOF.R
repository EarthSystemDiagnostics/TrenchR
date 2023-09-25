test_that("calculating effective DOF works", {

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

  # from old 'getSigmaStar' function
  expect_equal(round(
    getEffectiveTrenchDOF(a1 = exp(-1/1.53), N = 11, delta = 5), 4),
    10.2641)
  expect_equal(round(
    getEffectiveTrenchDOF(a1 = exp(-1/1.53), positions = seq(0, 50, 5)), 4),
    10.2641)

})
