test_that("mean profile correlation opimisation works", {

  m <- "Trench datasets must have a common `vscale`."

  t1 <- tibble::tibble(sampleNumber = 1 : 2, depth = c(2, 4),
                       my_data = c(1.1, 2.1))
  t2 <- tibble::tibble(sampleNumber = 1 : 2, depth = c(2, 5),
                       my_data = c(2.2, 1.2))
  
  expect_error(getMeanProfileCorrelation(t13.trench1, t15.trench1),
               m, fixed = TRUE)
  expect_error(getMeanProfileCorrelation(t1, t2), m, fixed = TRUE)

  m <- "`res` must be of length 1."
  expect_error(
    getMeanProfileCorrelation(t13.trench1, t13.trench2, res = c(3, 0.5)),
    m ,fixed = TRUE)

  r1 <- getMeanProfileCorrelation(t13.trench1, t13.trench2)
  r2 <- getMeanProfileCorrelation(t13.trench1, t13.trench2, res = 0.5)
  r3 <- getMeanProfileCorrelation(t13.trench1, t13.trench2, lag = -1 : 1)
  r4 <- getMeanProfileCorrelation(t13.trench1, t13.trench2,
                                  res = 0.5, lag = -1 : 1)

  nms1 <- c("correlation", "rmsd")
  nms2 <- c("bin_shift", "vscale_shift", nms1)

  expect_type(r1, "list")
  expect_type(r2, "list")
  expect_type(r3, "list")
  expect_type(r4, "list")

  expect_s3_class(r1, "tbl_df")
  expect_s3_class(r2, "tbl_df")
  expect_s3_class(r3, "tbl_df")
  expect_s3_class(r4, "tbl_df")

  expect_named(r1, nms1)
  expect_named(r2, nms1)
  expect_named(r3, nms2)
  expect_named(r4, nms2)

  expect_equal(dim(r1), c(1, 2))
  expect_equal(dim(r2), c(1, 2))
  expect_equal(dim(r3), c(1, 4))
  expect_equal(dim(r4), c(1, 4))

})
