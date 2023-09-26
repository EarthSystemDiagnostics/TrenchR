context("snr-calculation")

test_that("SNR calculation works", {

  data <- tibble::tibble(
    profileName = rep(c("a", "b", "c", "d"), each = 4),
    profilePosition = rep(c(0, 1, 2, 4), each = 4),
    sampleNumber = rep(1 : 4, times = 4),
    surfaceHeight = rep(0, 16),
    depth = rep(1 : 4, times = 4),
    d18O = c(1 : 4, 1 : 4, 1 : 4, 4 : 1))
  
  expected = tibble::tibble(snr = 0, se = as.numeric(NA))
  actual <- estimateSNR(data, distances = 1 : 3)
  expect_equal(actual, expected)

  data <- data[1 : 12, ]
  data$d18O[5 : 8] <- (1 : 4)^2

  actual <- estimateSNR(data, distances = 1)
  expect_equal(round(actual$snr, 5), 62.99606)
  expect_equal(actual$se, 0)

})
