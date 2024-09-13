test_that("function is.equidistant works", {

  m <- "'x' needs to be numeric."

  x <- "a"
  expect_error(is.equidistant(x), m)
  x <- factor(1 : 5, levels = 1 : 5)
  expect_error(is.equidistant(x), m)
  x <- as.POSIXct("2024-01-12 11:10")
  expect_error(is.equidistant(x), m)

  expect_true(is.equidistant(42))
  expect_true(is.equidistant(pi))

  expect_true(is.equidistant(1 : 10))
  expect_true(is.equidistant(10 : 1))
  expect_true(is.equidistant((1 : 10) / 147))
  expect_true(is.equidistant(c(2.5, 5, 7.5, 10, 12.5)))
  expect_true(is.equidistant(seq(0, 38, 0.1)))
  expect_true(is.equidistant(seq(1, 56, 0.3748)))

  expect_false(is.equidistant(c(1 : 10, 18)))
  expect_false(is.equidistant(c(2, 5, 7.5, 10, 12.5)))
  expect_false(is.equidistant(c(0, seq(1, 56, 0.3748), 58, 123)))
  expect_false(is.equidistant(runif(42)))

})

test_that("function is.identical works", {

  expect_false(is.identical("a", "b"))
  expect_false(is.identical(pi, pi + 0.1))
  expect_false(is.identical(1 : 10, 1 : 17))
  expect_false(is.identical(seq(1, 10, 1), seq(1, 20, 2)))

  expect_true(is.identical("abc", "abc"))
  expect_true(is.identical(pi, pi + 1e-8))
  expect_true(is.identical(getZ(t15.trench1), getZ(t15.trench2)))

})
