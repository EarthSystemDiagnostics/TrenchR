context("inter-profile-correlation")

test_that("error checking works", {

  msg <- "Input 'data' neither generic trench, nor a matrix or data frame."
  expect_error(
    estimateInterProfileCorrelation(
      numeric(1), distances = 1, profilePosition = 1),
    msg, fixed = TRUE)
  expect_error(
    estimateInterProfileCorrelation(
      character(1), distances = 1, profilePosition = 1),
    msg, fixed = TRUE)
  expect_error(
    estimateInterProfileCorrelation(
      list(foo = 1, bar = 2), distances = 1, profilePosition = 1),
    msg, fixed = TRUE)

  msg <- "Need a vector of profile positions."
  expect_error(
    estimateInterProfileCorrelation(matrix(nrow = 2, ncol = 2), distances = 1),
    msg, fixed = TRUE)
  msg <- "Number of profile positions does not match columns in data."
  expect_error(
    estimateInterProfileCorrelation(matrix(nrow = 2, ncol = 2),
                                    distances = 1, profilePosition = 1),
    msg, fixed = TRUE)
  msg <- "Non-numeric columns in data."
  expect_error(
    estimateInterProfileCorrelation(data.frame(name = "a", foo = 1),
                                    distances = 1, profilePosition = c(1, 2)),
    msg, fixed = TRUE)

})

test_that("inter profile correlation calculation works", {

  profilePosition <- c(0, 1, 2, 4)
  trench <- data.frame(
    a = 1 : 4, b = 1 : 4, c = 1 : 4, d = 4 : 1)
  generic.trench <- tibble::tibble(
    profileName = rep(letters[1 : 4], each = 4),
    sampleNumber = rep(1 : 4, times = 4),
    profilePosition = rep(profilePosition, each = 4),
    d18O = c(rep(1 : 4, times = 3), 4 : 1))

  actual1 <- estimateInterProfileCorrelation(
    trench, distances = 1 : 3, profilePosition)
  actual2 <- estimateInterProfileCorrelation(
    as.matrix(trench), distances = 1 : 3, profilePosition)
  actual3 <- estimateInterProfileCorrelation(generic.trench, distances = 1 : 3)
  expected1 <- tibble::tibble(
    distances = 1 : 3, N = c(2, 2, 1), cor = c(1, 0, -1),
    sd = c(0, sqrt(2), NA), se = c(0, 1, NA))

  actual4 <- estimateInterProfileCorrelation(
    as.matrix(trench), distances = 1 : 3, profilePosition, a1 = 0.9) %>%
    round(digits = 5)
  actual5 <- estimateInterProfileCorrelation(
    generic.trench, distances = 1 : 3, a1 = 0.9) %>%
    round(digits = 5)
  dof <- getEffectiveTrenchDOF(a1 = 0.9, N = 2, delta = mean(diff(profilePosition)))
  expected2 <- tibble::tibble(
    distances = 1 : 3, N = c(2, 2, 1), cor = c(1, 0, -1),
    sd = c(0, sqrt(2), NA), se = c(0, sqrt(2) / sqrt(dof), NA)) %>%
    round(digits = 5)

    expect_equal(actual1, expected1)
    expect_equal(actual2, expected1)
    expect_equal(actual3, expected1)
    expect_equal(actual4, expected2)
    expect_equal(actual5, expected2)

    profilePosition <- c(0, 0.95, 2.1, 3.71)

    actual1 <- estimateInterProfileCorrelation(
      trench, distances = 1 : 3, profilePosition, rangeTol = 0)
    actual2 <- estimateInterProfileCorrelation(
      as.matrix(trench), distances = 1 : 3, profilePosition, rangeTol = 0.2)

    expected3 <- tibble::tibble(
      distances = as.integer(1 : 3), N = as.integer(c(0, 0, 0)),
      cor = c(NaN, NaN, NaN),
      sd = as.numeric(rep(NA, 3)), se = as.numeric(rep(NA, 3)))

    expect_equal(actual1, expected3)
    expect_equal(actual2, expected1)
    
})
