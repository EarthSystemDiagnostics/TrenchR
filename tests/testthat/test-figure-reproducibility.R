context("TC17 figure data reproducibility")

dataFile <- if (endsWith(getwd(), "testthat")) {
              "test_data/figure_data_tc17.rda"
            } else {
              "tests/testthat/test_data/figure_data_tc17.rda"
            }

load(dataFile)

test_that("fig01 is reproducible", {

  trPar <- tc17.paper.param

  mean13.1 <- t13.trench1 %>%
    dplyr::filter(profileName != "T13-1-01") %>%
    makeMean(df = TRUE)
  mean13.2 <- t13.trench2 %>%
    makeMean(df = TRUE)

  T13.annual <- t13.annual.means$cheated

  expect_equal(mean13.1$depth, fig01$TR.depth)
  expect_equal(mean13.2$depth, fig01$TR.depth)
  expect_equal(mean13.1$d18O, fig01$TR.mean13.1)
  expect_equal(mean13.2$d18O, fig01$TR.mean13.2)
  expect_equal(trPar$k13, fig01$TR.k13)
  expect_equal(trPar$loRes, fig01$TR.LoRes)

  expect_equal(T13.annual, fig01$T13.annual)

  expect_equal(aws9$monthly, fig01$aws9.monthly)
  expect_equal(aws9$annual, fig01$aws9.annual)

})

test_that("fig03 is reproducible", {

  trPar <- tc17.paper.param

  t15.1.2d <- t15.trench1 %>%
    dplyr::filter(profileName != "T15-1-DUNE1") %>%
    make2D(simplify = TRUE)
  t15.2.2d <- t15.trench2 %>%
    make2D(simplify = TRUE)
  attr(t15.1.2d, "dimnames") <- NULL
  attr(t15.2.2d, "dimnames") <- NULL

  mean15.1 <- t15.trench1 %>%
    dplyr::filter(profileName != "T15-1-DUNE1") %>%
    makeMean(na.rm = TRUE, df = TRUE) %>%
    dplyr::slice(trPar$ix)
  mean15.2 <- t15.trench2 %>%
    makeMean(na.rm = TRUE, df = TRUE) %>%
    dplyr::slice(trPar$ix)
  sprf.t1 <- getSurfaceProfile(t15.trench1)
  sprf.t2 <- getSurfaceProfile(t15.trench2)

  expect_equal(t15.1.2d[1 : 59, ], fig03$TR.trench15.1)
  expect_equal(t15.2.2d[1 : 59, ], fig03$TR.trench15.2)

  expect_equal(mean15.1$depth, fig03$TR.depth)
  expect_equal(mean15.2$depth, fig03$TR.depth)

  expect_equal(sprf.t1$position[-7], fig03$TR.XPOS)
  expect_equal(sprf.t2$position, fig03$TR.XPOS)

  expect_equal(sprf.t1$position, fig03$TR.SPRF.t1$x)
  expect_equal(sprf.t2$position, fig03$TR.SPRF.t2$x)
  expect_equal(sprf.t1$height, fig03$TR.SPRF.t1$y)
  expect_equal(sprf.t2$height, fig03$TR.SPRF.t2$y)

  expect_equal(trPar$surfaceBot, simplify2array(fig03$TR.SRF.b))

  expect_equal(mean15.1$d18O, fig03$TR.mean15.1)
  expect_equal(mean15.2$d18O, fig03$TR.mean15.2)

  expect_equal(trPar$k15, fig03$TR.k15)

})

test_that("fig04 is reproducible", {

  TR <- makeHiResKohnenTrenches(na.rm = TRUE)

  expect_equal(TR$mean13$y, fig04$TR.mean13)
  expect_equal(TR$mean15$y, fig04$TR.mean15)

})

test_that("fig05 is reproducible", {

  expect_equal(ParamSpace$advection, fig05$ParamSpace$adv / 2)
  expect_equal(ParamSpace$sigma, fig05$ParamSpace$sigma)
  expect_equal(ParamSpace$compression, fig05$ParamSpace$densf)

  expect_equal(unname(ParamSpace$optimum["advection"]),
               fig05$ParamSpace$adv.opt)
  expect_equal(unname(ParamSpace$optimum["sigma"]),
               fig05$ParamSpace$sigma.opt)
  expect_equal(unname(ParamSpace$optimum["compression"]),
               fig05$ParamSpace$densf.opt)

  # take into account the rounding of the source data for the trench datasets
  # (commit 5a9999c)
  is.equal <- function(x, y, tolerance = sqrt(.Machine$double.eps)) {

    r <- all.equal(x, y, tolerance = tolerance)
    if (is.logical(r)) return(TRUE) else return(FALSE)

  }

  expect_true(
    is.equal(ParamSpace$RMSD, fig05$ParamSpace$RMSD, tolerance = 1e-4)
  )

})

test_that("fig06 is reproducible", {

  # ------------------------------------------------------------------------------
  # expectation values

    trPar <- tc17.paper.param
    mod.param <- tc17.modif.param

    TR <- makeHiResKohnenTrenches(na.rm = TRUE)
    T13.star     <- ModifyRecord(rec.in = TR$mean13_HiRes$y,
                                 res = trPar$hiRes,
                                 depth.hires = TR$mean13_HiRes$depth,
                                 depth.lores = TR$mean15$depth,
                                 SIGMA = mod.param$SIGMA.opt,
                                 STRETCH = mod.param$STRETCH.opt,
                                 ADV = mod.param$ADV.opt)
    T13.starstar <- ModifyRecord(rec.in = TR$mean13_HiRes$y,
                                 res = trPar$hiRes,
                                 depth.hires = TR$mean13_HiRes$depth,
                                 depth.lores = TR$mean15$depth,
                                 SIGMA = mod.param$SIGMA.ind,
                                 STRETCH = mod.param$STRETCH.ind,
                                 ADV = mod.param$ADV.ind)

    v2 <- ModifyRecord(rec.in = TR$mean13_HiRes$y,
                       res = trPar$hiRes,
                       depth.hires = TR$mean13_HiRes$depth,
                       depth.lores = TR$mean13$depth,
                       STRETCH = mod.param$STRETCH.opt)$HiRes
    v3 <- ModifyRecord(rec.in = TR$mean13_HiRes$y,
                       res = trPar$hiRes,
                       depth.hires = TR$mean13_HiRes$depth,
                       depth.lores = TR$mean13$depth,
                       SIGMA = mod.param$SIGMA.opt)$LoRes
    v4 <- ModifyRecord(rec.in = TR$mean13_HiRes$y,
                       res = trPar$hiRes,
                       depth.hires = TR$mean13_HiRes$depth,
                       depth.lores = TR$mean13$depth,
                       ADV = mod.param$ADV.only)$LoRes

    v11 <- TR$mean15$y
    ind1 <- which(TR$mean15$depth <= trPar$surfaceBot["t15"])
    v11[ind1[-length(ind1)]] <- NA

    sum.max <- prxytools::LocatePeaks(v11, partial = TRUE)[3 : 6]

  # ------------------------------------------------------------------------------
  # tests

  names(fig06$mod.param$SIGMA.ind) <- NULL
  expect_equal(mod.param, fig06$mod.param)
  
  expect_equal(trPar$hiRes, fig06$TR.HiRes)
  expect_equal(TR$mean15_HiRes$depth, fig06$TR.depth_HiRes)
  expect_equal(TR$mean13_HiRes$y, fig06$TR.mean13_HiRes)

  expect_equal(T13.star, fig06$T13.star)
  expect_equal(T13.starstar, fig06$T13.starstar)

  expect_equal(v2, fig06$v2)
  expect_equal(v3, fig06$v3[1 : 38])
  expect_equal(v4, fig06$v4[1 : 38])

  expect_equal(sum.max, fig06$sum.max)

})

test_that("fig07 is reproducible", {

  # ------------------------------------------------------------------------------
  # expectation values

  trPar <- tc17.paper.param
  mod.param <- tc17.modif.param

  TR <- makeHiResKohnenTrenches()
  T13.starstar <- ModifyRecord(rec.in = TR$mean13_HiRes$y,
                               res = trPar$hiRes,
                               depth.hires = TR$mean13_HiRes$depth,
                               depth.lores = TR$mean15$depth,
                               SIGMA = mod.param$SIGMA.ind,
                               STRETCH = mod.param$STRETCH.ind,
                               ADV = mod.param$ADV.ind)

  diff.13 <- TR$mean13.1$y -
    prxytools::Lag(TR$mean13.2$y, shift = trPar$k13 / trPar$loRes)

  diff.15 <- TR$mean15.1_HiRes$y -
    prxytools::Lag(TR$mean15.2_HiRes$y, shift = trPar$k15 / trPar$hiRes)
  diff.15 <- diff.15[match(TR$mean15$depth, TR$mean15_HiRes$depth)]

  diff.2yr <- TR$mean15$y - T13.starstar$LoRes

  # ------------------------------------------------------------------------------
  # tests

  expect_equal(T13.starstar, fig07$T13.starstar)

  expect_equal(diff.13, fig07$diff.13)
  expect_equal(diff.15, fig07$diff.15)
  expect_equal(diff.2yr, fig07$diff.2yr)
  
})

test_that("running figure reproduction function works", {

  expect_error(produceTC17Figures("foo"))

  # suppressWarnings needed when running devtools::test() to avoid
  # warnings of permil sign graphical conversion failures
  suppressWarnings(expect_no_error(produceTC17Figures("f1")))
  suppressWarnings(expect_no_error(produceTC17Figures("f2")))
  suppressWarnings(expect_no_error(produceTC17Figures("f3a")))
  suppressWarnings(expect_no_error(produceTC17Figures("f3b")))
  suppressWarnings(expect_no_error(produceTC17Figures("f3c")))
  suppressWarnings(expect_no_error(produceTC17Figures("f4")))
  suppressWarnings(expect_no_error(produceTC17Figures("f5")))
  suppressWarnings(expect_no_error(produceTC17Figures("f6")))
  suppressWarnings(expect_no_error(produceTC17Figures("f7")))

})
