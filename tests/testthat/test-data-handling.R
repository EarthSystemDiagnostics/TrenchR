test_that("extraction of depth profile works", {

  msg <- "No column 'profilePosition' found in trench data."

  foo <- tibble::tibble(x = 1, y = 2)
  expect_error(getX(foo), msg)

  foo <- tibble::tibble(x = 1, y = 2, profilePosition = 0)
  expect_equal(getX(foo), 0)

  expect_error(getX(dplyr::select(t15.trench1, -"profilePosition")), msg)
  expect_equal(getX(t15.trench2), seq(0, 50, 5))

})

test_that("extraction of depth profile works", {

  expect_error(getZ(t13.trench1, var = "foo"),
               "Unknown column name for vertical scale.")
  expect_equal(getZ(t13.trench1), seq(from = 1.5, by = 3, length.out = 38))

})

test_that("production of surface profile works", {

  msg <- paste("'profilePosition' and 'surfaceHeight' columns both needed",
               "to extract surface profile.")

  foo <- tibble::tibble(x = 1, y = 2)
  expect_error(getSurfaceProfile(foo), msg, fixed = TRUE)
  foo <- tibble::tibble(x = 1, y = 2, profilePosition = 0)
  expect_error(getSurfaceProfile(foo), msg, fixed = TRUE)
  foo <- tibble::tibble(x = 1,y = 2, surfaceHeight = 10)
  expect_error(getSurfaceProfile(foo), msg, fixed = TRUE)
  foo <- tibble::tibble(x = 1,y = 2, profilePosition = 0, surfaceHeight = 10)
  expect_no_error(getSurfaceProfile(foo))

  target <- tibble::tibble(
    position = seq(0, 50, 5),
    height = c(12, 10, 14, 14.5, 10, 8, 12, 9.5, 5, 0, 2)
  )

  expect_equal(getSurfaceProfile(t15.trench2), target)

})

test_that("production of 2D profile works", {

  # test error checking

  foo <- tibble::tibble(x = 1)
  expect_error(make2D(foo))
  foo <- tibble::tibble(x = 1, profileName = "A")
  expect_error(make2D(foo))
  foo <- tibble::tibble(x = 1, sampleNumber = 1)
  expect_error(make2D(foo))
  foo <- tibble::tibble(x = 1, profileName = "A", sampleNumber = 1)
  expect_no_error(make2D(foo, var = "x"))

  foo <- dplyr::select(t15.trench1, -d18O)
  expect_error(make2D(foo), "Unknown column variable selected.")
  expect_no_error(make2D(foo, var = "dxs"))

  foo <- dplyr::select(t15.trench1, -d18O, -dxs)
  expect_error(make2D(foo, var = "dxs"),
               "Column variable(s) missing to calculate 'dexcess'.",
               fixed = TRUE)

  foo <- dplyr::select(t15.trench1, -dD, -dxs)
  expect_error(make2D(foo, var = "dxs"),
               "Column variable(s) missing to calculate 'dexcess'.",
               fixed = TRUE)

  foo <- dplyr::select(t15.trench1, -d18O, -dD, -dxs)
  expect_error(make2D(foo, var = "dxs"),
               "Column variable(s) missing to calculate 'dexcess'.",
               fixed = TRUE)
  expect_error(make2D(foo, var = "nssSulfate"),
               "Column variable(s) missing to calculate 'nssSulfate'.",
               fixed = TRUE)

  # test d-excess and nssSulfate calculation

  foo <- tibble::tibble(
    profileName = rep(c("A", "B"), each = 3),
    sampleNumber = rep(1 : 3, times = 2),
    d18O = c(-30.61, -42.36, -38.01, -42.66, -51.98, -41.63),
    dD = c(-240.5, -338.7, -304.7, -335.1, -408.5, -327.7),
    dxs = c(4.38, 0.18, -0.62, 6.18, 7.34, 5.34)
  )
  target <- tibble::tibble(A = c(4.38, 0.18, -0.62), B = c(6.18, 7.34, 5.34))
  expect_equal(make2D(dplyr::select(foo, -dxs), var = "dxs"), target)

  foo <- tibble::tibble(
    profileName = rep(c("A", "B"), each = 2),
    sampleNumber = rep(1 : 2, times = 2),
    Sulfate = c(20.6, 34.9, 99.5, 106.1),
    Sodium = c(234.7, 217.3, 99.6, 178.1),
    nssSulfate = c(-38.54, -19.86, 74.4, 61.22)
  )
  target <- tibble::tibble(A = c(-38.54, -19.86), B = c(74.4, 61.22))
  expect_equal(
    round(make2D(dplyr::select(foo, -nssSulfate), var = "nssSulfate"),
          digits = 2), target)

  # test purely synthetic data

  foo <- tibble::tibble(
    profileName = rep(c("A", "B", "C"), each = 2),
    sampleNumber = rep(1 : 2, times = 3),
    foovar1 = c(4, 7, 1, 25, 2, 56),
    foovar2 = c(35, 875, 213, 76, 3, 24)
  )

  target <- tibble::tibble(A = c(4, 7), B = c(1, 25), C = c(2, 56))
  expect_equal(make2D(foo, var = "foovar1"), target)
  target <- as.matrix(
    tibble::tibble(A = c(35, 875), B = c(213, 76), C = c(3, 24)))
  expect_equal(make2D(foo, var = "foovar2", simplify = TRUE), target)

  # tests with actual trench data

  target <- matrix(c(-43.56, NA, -43.16, -47.11,
                     -43.71, -37.83, -45.9, -47.98,
                     -43.33, -42.28, -47.73, -47.03,
                     -47.04, -47.02, -47.96, -46.34,
                     -49.73, -49.33, -47.31, -42.23),
                   nrow = 5, ncol = 4, byrow = TRUE)
  colnames(target) <- c("T13-2-01", "T13-2-02", "T13-2-03", "T13-2-04")

  expect_equal(make2D(t13.trench2, simplify = TRUE)[3 : 7, ], target)

  target <- matrix(c(-408.2, -381,
                     -385.3, -356.4,
                     -343.1, -343.8,
                     -328.8, -369.8),
                   nrow = 4, ncol = 2, byrow = TRUE)
  colnames(target) <- c("T15-1-L", "T15-1-M")

  expect_equal(
    make2D(t15.trench1, var = "dD", simplify = TRUE)[10 : 13, 11 : 12],
    target)

})

test_that("production of mean profile works", {

  foo <- dplyr::filter(t13.trench2, sampleNumber <= 3, profilePosition < 30)
  foo <- dplyr::mutate(foo, foovar = c(1 : 3, c(NA, 3, 4), 3 : 5))

  # return type should be a simple vector
  target <- c(NA, 3, 4)
  expect_equal(makeMean(foo, var = "foovar", df = FALSE), target)
  target <- c(2, 3, 4)
  expect_equal(makeMean(foo, var = "foovar", na.rm = TRUE, df = FALSE), target)

  # return type should be a tibble
  target <- tibble::tibble(depth = c(1.5, 4.5, 7.5), foovar = target)
  expect_equal(makeMean(foo, var = "foovar", na.rm = TRUE), target)

})

test_that("checking for proper trench data works", {

  x1 <- tibble::tibble(x = 1)
  msg1a <- paste("Incomplete trench data set: columns profileName,",
                 "sampleNumber missing.")
  msg1b <- paste("Incomplete trench data set: columns profileName,",
                 "sampleNumber, profilePosition, surfaceHeight missing.")
  x2 <- tibble::tibble(x = 1, profileName = "A")
  msg2 <- paste("Incomplete trench data set: columns sampleNumber",
                "missing.")
  x3 <- tibble::tibble(x = 1, profileName = "A", sampleNumber = 1)
  msg3 <- paste("Incomplete trench data set: columns profilePosition,",
                "surfaceHeight missing.")

  expect_error(is.trench(x1, full = FALSE), msg1a)
  expect_error(is.trench(x1), msg1b)
  expect_error(is.trench(x2, full = FALSE), msg2)
  expect_no_error(is.trench(x3, full = FALSE))
  expect_error(is.trench(x3), msg3)

  x4 <- tibble::tibble(x = 1, profileName = "A", sampleNumber = 1,
                       profilePosition = 10)
  msg4 <- paste("Incomplete trench data set: columns surfaceHeight missing.")
  x5 <- tibble::tibble(x = 1, profileName = "A", sampleNumber = 1,
                       surfaceHeight = 2.68)
  msg5 <- paste("Incomplete trench data set: columns profilePosition missing.")
  x6 <- tibble::tibble(x = 1, profileName = "A", sampleNumber = 1,
                       profilePosition = 10, surfaceHeight = 2.68)

  expect_error(is.trench(x4), msg4)
  expect_error(is.trench(x5), msg5)
  expect_no_error(is.trench(x6))

})

test_that("extracting the bottom of the surface layer works", {

  expect_error(getFirstCompleteDepthBin(t13.trench1, var = "foo"))
  expect_error(getFirstCompleteDepthBin(t13.trench1, vscale = "foo"))

  foo <- tibble::tibble(
    profileName = rep(c("A", "B", "C"), each = 3),
    depth = rep(1 : 3, times = 3),
    d18O = c(1, 2, 3, NA, 2, 3, NA, NA, 3)
  )

  expect_error(getFirstCompleteDepthBin(dplyr::select(foo, -"profileName")),
               "Need column 'profileName'.", fixed = TRUE)

  expect_equal(getFirstCompleteDepthBin(foo), 3)

})
