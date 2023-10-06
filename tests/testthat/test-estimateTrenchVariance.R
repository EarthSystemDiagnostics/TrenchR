context("trench-variance-estimation")

test_that("error checking works", {

  msg <- "Input 'data' neither generic trench, nor a matrix or data frame."
  expect_error(estimateTrenchVariance(numeric(1)), msg, fixed = TRUE)
  expect_error(estimateTrenchVariance(character(1)), msg, fixed = TRUE)
  expect_error(estimateTrenchVariance(list(a = 1, b = 2)), msg, fixed = TRUE)

  msg <- "Need name of trench data variable for generic trench 'data'."
  data <- tibble::tibble(profileName = "A", sampleNumber = 1, d18O = 1)
  expect_error(estimateTrenchVariance(data), msg, fixed = TRUE)

  msg <- "Non-numeric columns in data."
  expect_error(estimateTrenchVariance(tibble::tibble(name = "a", foo = 1)),
               msg, fixed = TRUE)

})

test_that("variance calculation works", {

  trench1 <- data.frame(
    a = 1 : 3, b = sqrt(2) * (1 : 3), c = sqrt(3) * (1 : 3))
  trench2 <- tibble::tibble(
    profileName = rep(letters[1 : 3], each = 3),
    sampleNumber = rep(1 : 3, times = 3),
    foo = c((1 : 3), sqrt(2) * (1 : 3), sqrt(3) * (1 : 3)))

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

  actual1 <- estimateTrenchVariance(trench1) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, digits = 7)))
  actual2 <- estimateTrenchVariance(trench2, .var = "foo") %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, digits = 7)))

  expect_equal(actual1, expected)
  expect_equal(actual2, expected)

  a <- 1 / 10

  expected <- tibble::tibble(
    direction = c("horizontal", "vertical"),
    var = c((14/3) * v, 2),
    lower = c((14/3) * v * n / qchisq(1 - a, n),
              2 * n / qchisq(1 - a, n)),
    upper = c((14/3) * v * n / qchisq(a, n), 2 * n / qchisq(a, n))) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, digits = 7)))

  actual <- estimateTrenchVariance(as.matrix(trench1), alpha = a) %>%
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

  actual <- estimateTrenchVariance(trench1, dof.h = 1.5, dof.v = 2) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, digits = 7)))

  expect_equal(actual, expected)

})
