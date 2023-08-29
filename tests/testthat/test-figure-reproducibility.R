context("Ensure TC17 figure data reproducibility")

dataFile <- if (endsWith(getwd(), "testthat")) {
              "test_data/figure_data_tc17.rda"
            } else {
              "tests/testthat/test_data/figure_data_tc17.rda"
            }

load(dataFile)

test_that("fig01 is reproducible", {

  TR <- prepareTrenchData()$oxy
  T13.annual <- T13AnnualMeans(t1 = TR$mean13.1,
                               t2 = prxytools::Lag(TR$mean13.2,
                                               TR$k13 / TR$LoRes),
                               depth = TR$depth,
                               cheat = TRUE)

  expect_equal(TR$depth[1 : 38], fig01$TR.depth)
  expect_equal(TR$mean13.1, fig01$TR.mean13.1)
  expect_equal(TR$mean13.2, fig01$TR.mean13.2)
  expect_equal(TR$k13, fig01$TR.k13)
  expect_equal(TR$LoRes, fig01$TR.LoRes)

  expect_equal(T13.annual, fig01$T13.annual)

  expect_equal(aws9$monthly, fig01$aws9.monthly)
  expect_equal(aws9$annual, fig01$aws9.annual)

})

test_that("fig03 is reproducible", {

  TR <- prepareTrenchData(na.treat = TRUE)$oxy

  expect_equal(TR$trench15.1, fig03$TR.trench15.1)
  expect_equal(TR$trench15.2, fig03$TR.trench15.2)

  expect_equal(TR$depth, fig03$TR.depth)
  expect_equal(TR$XPOS, fig03$TR.XPOS)
  expect_equal(TR$SPRF.t1, fig03$TR.SPRF.t1)
  expect_equal(TR$SPRF.t2, fig03$TR.SPRF.t2)
  expect_equal(TR$SRF.b, fig03$TR.SRF.b)

  expect_equal(TR$mean15.1, fig03$TR.mean15.1)
  expect_equal(TR$mean15.2, fig03$TR.mean15.2)

  expect_equal(TR$k15, fig03$TR.k15)

})

test_that("fig04 is reproducible", {

  TR <- prepareTrenchData(na.treat = TRUE)$oxy

  expect_equal(TR$mean13, fig04$TR.mean13)
  expect_equal(TR$mean15, fig04$TR.mean15)

})

test_that("fig05 is reproducible", {

  expect_equal(ParamSpace, fig05$ParamSpace)

})

test_that("fig06 is reproducible", {

  # ------------------------------------------------------------------------------
  # expectation values

  TR <- prepareTrenchData(na.treat = TRUE)$oxy

  mod.param <- SetModificationPar()

  T13.star <- ModifyRecord(rec.in = TR$mean13_HiRes,
                           res = TR$HiRes,
                           depth.hires = TR$depth_HiRes,
                           depth.lores = TR$depth,
                           SIGMA = mod.param$SIGMA.opt,
                           STRETCH = mod.param$STRETCH.opt,
                           ADV = mod.param$ADV.opt)
  T13.starstar <- ModifyRecord(rec.in = TR$mean13_HiRes,
                               res = TR$HiRes,
                               depth.hires = TR$depth_HiRes,
                               depth.lores = TR$depth,
                               SIGMA = mod.param$SIGMA.ind,
                               STRETCH = mod.param$STRETCH.ind,
                               ADV = mod.param$ADV.ind)

  v2 <- ModifyRecord(rec.in = TR$mean13_HiRes,
                     res = TR$HiRes,
                     depth.hires = TR$depth_HiRes,
                     depth.lores = TR$depth,
                     STRETCH = mod.param$STRETCH.opt)$HiRes

  v3 <- ModifyRecord(rec.in = TR$mean13_HiRes,
                     res = TR$HiRes,
                     depth.hires = TR$depth_HiRes,
                     depth.lores = TR$depth,
                     SIGMA = mod.param$SIGMA.opt)$LoRes

  v4 <- ModifyRecord(rec.in = TR$mean13_HiRes,
                     res = TR$HiRes,
                     depth.hires = TR$depth_HiRes,
                     depth.lores = TR$depth,
                     ADV = mod.param$ADV.only)$LoRes

  v11 <- TR$mean15
  ind1 <- which(TR$depth <= TR$SRF.b$t15)                 
  v11[ind1[-length(ind1)]] <- NA

  sum.max <- prxytools::LocatePeaks(v11, partial = TRUE)[3 : 6]

  # ------------------------------------------------------------------------------
  # tests
  
  expect_equal(mod.param, fig06$mod.param)
  
  expect_equal(TR$HiRes, fig06$TR.HiRes)
  expect_equal(TR$depth_HiRes, fig06$TR.depth_HiRes)
  expect_equal(TR$mean13_HiRes, fig06$TR.mean13_HiRes)

  expect_equal(T13.star, fig06$T13.star)
  expect_equal(T13.starstar, fig06$T13.starstar)

  expect_equal(v2, fig06$v2)
  expect_equal(v3, fig06$v3)
  expect_equal(v4, fig06$v4)

  expect_equal(sum.max, fig06$sum.max)

})

test_that("fig07 is reproducible", {

  # ------------------------------------------------------------------------------
  # expectation values
  
  TR <- prepareTrenchData()$oxy
  mod.param <- SetModificationPar()

  T13.starstar <- ModifyRecord(rec.in = TR$mean13_HiRes,
                               res = TR$HiRes,
                               depth.hires = TR$depth_HiRes,
                               depth.lores = TR$depth,
                               SIGMA = mod.param$SIGMA.ind,
                               STRETCH = mod.param$STRETCH.ind,
                               ADV = mod.param$ADV.ind)

  diff.13 <- TR$mean13.1 -
    prxytools::Lag(TR$mean13.2, shift = TR$k13 / TR$LoRes)

  diff.15 <- TR$mean15.1_HiRes -
    prxytools::Lag(TR$mean15.2_HiRes, shift = TR$k15 / TR$HiRes)
  diff.15 <- diff.15[match(TR$depth, TR$depth_HiRes)]

  diff.2yr <- TR$mean15 - T13.starstar$LoRes

  # ------------------------------------------------------------------------------
  # tests

  expect_equal(T13.starstar, fig07$T13.starstar)

  expect_equal(diff.13, fig07$diff.13)
  expect_equal(diff.15, fig07$diff.15)
  expect_equal(diff.2yr, fig07$diff.2yr)
  
})
