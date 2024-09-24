test_that("trench summary function works", {

  expect_error(summarizeTrench(data.frame(a = 1 : 5, b = rnorm(5))))
  expect_error(summarizeTrench(t15.trench2, .var = "foo"),
               "Unknown column variable selected.", fixed = TRUE)

  expected <- dplyr::tibble(Nprofiles = 11, d_min = 5, d_max = 5)

  expect_equal(summarizeTrench(t15.trench2), expected)

  expected <-
    dplyr::bind_cols(expected,
                     dplyr::tibble(.var = "d18O",
                                   Nsamples = length(na.omit(t15.trench2$d18O)),
                                   min = min(t15.trench2$d18O, na.rm = TRUE),
                                   max = max(t15.trench2$d18O, na.rm = TRUE),
                                   mean = mean(t15.trench2$d18O, na.rm = TRUE),
                                   sd = sd(t15.trench2$d18O, na.rm = TRUE)))

  expect_equal(summarizeTrench(t15.trench2, "d18O"), expected)

})
