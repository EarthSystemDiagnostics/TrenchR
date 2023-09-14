test_that("2D plotting function works", {

  data <- tibble::tibble(
    profileName = rep(c("A", "B", "C"), each = 3),
    profilePosition = rep(c(0, 5, 10), each = 3),
    surfaceHeight = rep(0.5, 9),
    sampleNumber = rep(1 : 3, times = 3),
    depth = rep(1 : 3, times = 3),
    d18O = runif(9, 1, 10)
  )

  expect_error(plot2D(dplyr::select(data, -"profileName")))
  expect_error(plot2D(dplyr::select(data, -"profilePosition")))
  expect_error(plot2D(dplyr::select(data, -"surfaceHeight")))
  expect_error(plot2D(dplyr::select(data, -"sampleNumber")))
  expect_error(plot2D(dplyr::select(data, -"profileName", -"surfaceHeight")))

  msg <- paste("Argument 'palette' must be a vector of colours for image plots.")
  expect_error(plot2D(data, palette = hcl.colors), msg, fixed = TRUE)
  msg <- paste("Argument 'palette' must be a function that can be used",
               "to assign colours in a filled contour plot.")
  expect_error(plot2D(data, palette = 1 : 5, filledContour = TRUE),
               msg, fixed = TRUE)

  expect_no_error(plot2D(data, xlim = c(0, 10), ylim = c(3.5, 0)))
  expect_no_error(plot2D(data, xlim = c(0, 10), ylim = c(3.5, 0),
                  horizontal = TRUE))
  expect_no_error(plot2D(data, xlim = c(0, 10), ylim = c(3.5, 0),
                         filledContour = TRUE, fill = TRUE, log = TRUE))

})
