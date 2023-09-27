context("trench-variance-estimation")

test_that("trench variance calculation works", {

  msg <- "Input 'trench' must be a matrix or a data frame."
  expect_error(estimateTrenchVariance(numeric(1)), msg, fixed = TRUE)
  expect_error(estimateTrenchVariance(character(1)), msg, fixed = TRUE)
  expect_error(estimateTrenchVariance(list(a = 1, b = 2)), msg, fixed = TRUE)

  trench <- data.frame(
    a = 1 : 3, b = sqrt(2) * (1 : 3), c = sqrt(3) * (1 : 3))

  n <- 3 * 3 - 1
  a <- 1 / 6
  v <- var(c(1, sqrt(2), sqrt(3)))
  expected <- tibble::tibble(
    direction = c("horizontal", "vertical"),
    var = c((14/3) * v, 2),
    lower = c((14/3) * v * n / qchisq(1 - a, n),
              2 * n / qchisq(1 - a, n)),
    upper = c((14/3) * v * n / qchisq(a, n), 2 * n / qchisq(a, n))) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, digits = 7)))

  actual <- estimateTrenchVariance(trench) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, digits = 7)))

  expect_equal(actual, expected)

  a <- 1 / 10

  expected <- tibble::tibble(
    direction = c("horizontal", "vertical"),
    var = c((14/3) * v, 2),
    lower = c((14/3) * v * n / qchisq(1 - a, n),
              2 * n / qchisq(1 - a, n)),
    upper = c((14/3) * v * n / qchisq(a, n), 2 * n / qchisq(a, n))) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, digits = 7)))

  actual <- estimateTrenchVariance(as.matrix(trench), alpha = a) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, digits = 7)))

  expect_equal(actual, expected)

  n <- 1.5 * 2 - 1
  a <- 1 / 6
  expected <- tibble::tibble(
    direction = c("horizontal", "vertical"),
    var = c((14/3) * v, 2),
    lower = c((14/3) * v * n / qchisq(1 - a, n),
              2 * n / qchisq(1 - a, n)),
    upper = c((14/3) * v * n / qchisq(a, n), 2 * n / qchisq(a, n))) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, digits = 7)))

  actual <- estimateTrenchVariance(trench, dof.h = 1.5, dof.v = 2) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, digits = 7)))

  expect_equal(actual, expected)

})
