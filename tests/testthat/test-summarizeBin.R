test_that("bin averaging based on given 'age-depth' relationship works", {

  x <- data.frame(depth = 1 : 10, d18O = 1 : 10)
  bins <- data.frame(depth = c(1, 4, 8, 10), year = 1 : 4)

  # error checks
  m <- "`x` must be a data frame."
  expect_error(summarizeBin(1, bins), m, fixed = TRUE)
  expect_error(summarizeBin(list(a = 1, b = 2), bins), m, fixed = TRUE)
  m <- "`x` may only have two columns."
  expect_error(summarizeBin(data.frame(depth = 1), bins), m, fixed = TRUE)
  expect_error(summarizeBin(
    data.frame(depth = 1, d18O = 2, foo = 3), bins), m, fixed = TRUE)
  m <- "`bins` must be a data frame."
  expect_error(summarizeBin(x, bins = c(1, 4, 5)), m, fixed = TRUE)
  m <- "`bins` may only have two columns."
  expect_error(
    summarizeBin(
      x, bins = data.frame(year = 1 : 2, depth = c(4, 7), foo = rnorm(2))),
    m, fixed = TRUE)
  expect_error(summarizeBin(x, bins, "foo"))

  # default
  expected <- tibble::tibble(label = 2 : 4, d18O = c(3, 6.5, 9.5))
  actual <- summarizeBin(x, bins)
  expect_equal(actual, expected)

  # data outside bins should be discarded
  xx <- data.frame(depth = 0 : 15, d18O = 0 : 15)
  actual <- summarizeBin(xx, bins)
  expect_equal(actual, expected)

  # non-increasing bin order
  bins.shuffled <- data.frame(depth = c(8, 1, 10, 4), year = c(3, 1, 4, 2))
  actual <- summarizeBin(x, bins.shuffled)
  expect_equal(actual, expected)

  # label name is given
  expected <- setNames(expected, c("year", "d18O"))
  actual <- summarizeBin(x, bins, label.name = "year")
  expect_equal(actual, expected)

  # closed on left, open on right
  expected <- tibble::tibble(label = 1 : 3, d18O = c(2, 5.5, 8.5))
  actual <- summarizeBin(x, bins, right = FALSE)
  expect_equal(actual, expected)

  # NA values
  xx <- data.frame(depth = 1 : 10, d18O = c(1 : 4, NA, 6, NA, NA, 9 : 10))

  expected <- tibble::tibble(label = 2 : 4, d18O = c(3, NA, 9.5))
  actual <- summarizeBin(xx, bins)
  expect_equal(actual, expected)

  expected <- tibble::tibble(label = 2 : 4, d18O = c(3, 6, 9.5))
  actual <- summarizeBin(xx, bins, na.rm = TRUE)
  expect_equal(actual, expected)

  expected <- tibble::tibble(label = 1 : 3, d18O = c(2, 5, 9))
  actual <- summarizeBin(xx, bins, right = FALSE, na.rm = TRUE)
  expect_equal(actual, expected)

  # min value is requested
  expected <- tibble::tibble(label = 2 : 4, d18O = c(2, 5, 9))
  actual <- summarizeBin(x, bins, "min")
  expect_equal(actual, expected)

  # max value is requested
  expected <- tibble::tibble(label = 2 : 4, d18O = c(4, 8, 10))
  actual <- summarizeBin(x, bins, "max")
  expect_equal(actual, expected)

})
