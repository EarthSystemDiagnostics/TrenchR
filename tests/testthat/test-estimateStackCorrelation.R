context("stack-correlation-stats")

test_that("error checking works", {

  msg <- "Input 'data' neither generic trench, nor a matrix or data frame."
  expect_error(
    estimateStackCorrelation(
      numeric(1), reference = 1, distances = 1, nprofiles = 1),
    msg, fixed = TRUE)
  expect_error(
    estimateStackCorrelation(
      character(1), reference = 1, distances = 1, nprofiles = 1),
    msg, fixed = TRUE)
  expect_error(
    estimateStackCorrelation(
      list(foo = 1, bar = 2), reference = 1, distances = 1, nprofiles = 1),
    msg, fixed = TRUE)

  msg <- "Need a vector of profile positions."
  expect_error(
    estimateStackCorrelation(matrix(NA_real_, nrow = 2, ncol = 2),
                             reference = 1, distances = 1, nprofiles = 1),
    msg, fixed = TRUE)
  msg <- "Number of profile positions does not match columns in data."
  expect_error(
    estimateStackCorrelation(matrix(NA_real_, nrow = 2, ncol = 2),
                             reference = 1, distances = 1, nprofiles = 1,
                             profilePosition = 1),
    msg, fixed = TRUE)
  msg <- "Non-numeric columns in data."
  expect_error(
    estimateStackCorrelation(data.frame(name = "a", foo = 1), reference = 1,
                             distances = 1, nprofiles = 1,
                             profilePosition = c(1, 2)),
    msg, fixed = TRUE)
  msg <- "Length of `reference` must match profile length of `data`."
  expect_error(
    estimateStackCorrelation(matrix(NA_real_, nrow = 2, ncol = 2),
                             reference = 1, distances = 1, nprofiles = 1,
                             profilePosition = c(1, 2)),
    msg, fixed = TRUE)

  msg <- paste("`rangeTol` must either have length 1",
               "or the same length as `distances`.")
  expect_error(
    estimateStackCorrelation(matrix(NA_real_, nrow = 2, ncol = 2),
                             reference = 1 : 2, distances = 1, nprofiles = 1,
                             profilePosition = c(1, 2), rangeTol = c(0.1, 0.2)),
    msg, fixed = TRUE)

})

test_that("output structure is valid", {

  # test function output structure with real trench data

  reference <- makeMean(t13.trench1, df = FALSE)

  data <- t13.trench1 # data as generic trench dataset
  stats1 <- estimateStackCorrelation(data, reference,
                                     distances = c(2, 10),
                                     nprofiles = 5, rangeTol = 0,
                                     verbose = FALSE)

  data <- make2D(t13.trench1, simplify = TRUE) # data as matrix
  stats2 <- estimateStackCorrelation(data, reference,
                                     profilePosition = getX(t13.trench1),
                                     distances = c(2, 10),
                                     nprofiles = 5, rangeTol = 0,
                                     verbose = FALSE)

  expect_type(stats1, "list")
  expect_true(is.data.frame(stats1))
  expect_equal(dim(stats1), c(2, 6))
  expect_named(stats1, c("distance", "N", "cor", "n_sets", "d_min", "d_max"))
  expect_equal(stats1$distance, c(2, 10))
  expect_equal(stats1$N, rep(5, 2))

  expect_equal(stats1, stats2)

  # actually find something

  expect_no_error(
    estimateStackCorrelation(data, reference,
                             profilePosition = getX(t13.trench1),
                             distances = 10, nprofiles = 2, rangeTol = 0.2,
                             verbose = FALSE)
  )

  # for single-profile "stacks" we can quantitatively test results

  stats3 <- estimateStackCorrelation(data, reference,
                                     profilePosition = getX(t13.trench1),
                                     distances = c(2, 10),
                                     nprofiles = 1,
                                     verbose = FALSE)

  expected_cor <- data %>%
    cor(reference, use = "pairwise.complete.obs") %>%
    mean()
  expected <- data.frame(distance = c(2, 10),
                         N = rep(1, 2),
                         cor = rep(expected_cor, 2),
                         n_sets = rep(nrow(data), 2),
                         d_min = rep(NA_integer_, 2),
                         d_max = rep(NA_integer_, 2))

  expect_equal(stats3, expected)

})
