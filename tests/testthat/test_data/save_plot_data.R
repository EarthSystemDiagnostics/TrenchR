##
## script to save as an R data file the relevant data needed to reproduce the TC
## paper figures; the saved data can then be used as a testthat target to ensure
## redroducibility of the paper figures.
##
## !OBS! If a plotting reproducibility test fails, solve the issue by updating
## the test data here **only if** you are absolutely sure about **why** the test
## fails!!!
##

library(TrenchR)

# ------------------------------------------------------------------------------
# fig01

TR <- TrenchR:::prepareTrenchData()$oxy
T13.annual <- TrenchR:::T13AnnualMeans(t1 = TR$mean13.1,
                                       t2 = prxytools::Lag(TR$mean13.2,
                                                       TR$k13 / TR$LoRes),
                                       depth = TR$depth,
                                       cheat = TRUE)

fig01 <- list(

  TR.depth = TR$depth[1 : 38],
  TR.mean13.1 = TR$mean13.1,
  TR.mean13.2 = TR$mean13.2,

  TR.k13 = TR$k13,
  TR.LoRes = TR$LoRes,

  T13.annual = T13.annual,

  aws9.monthly = aws9$monthly,
  aws9.annual = aws9$annual

)

# ------------------------------------------------------------------------------
# fig03

TR <- TrenchR:::prepareTrenchData(na.treat = TRUE)$oxy

fig03 <- list(

  TR.trench15.1 = TR$trench15.1,
  TR.trench15.2 = TR$trench15.2,

  TR.depth = TR$depth,
  TR.XPOS = TR$XPOS,
  TR.SPRF.t1 = TR$SPRF.t1,
  TR.SPRF.t2 = TR$SPRF.t2,
  TR.SRF.b = TR$SRF.b,

  TR.mean15.1 = TR$mean15.1,
  TR.mean15.2 = TR$mean15.2,

  TR.k15 = TR$k15

)

# ------------------------------------------------------------------------------
# fig04

TR <- TrenchR:::prepareTrenchData(na.treat = TRUE)$oxy

fig04 <- list(

  TR.mean13 = TR$mean13,
  TR.mean15 = TR$mean15

)

# ------------------------------------------------------------------------------
# fig05

fig05 <- list(ParamSpace = ParamSpace)

# ------------------------------------------------------------------------------
# fig06

TR <- TrenchR:::prepareTrenchData(na.treat = TRUE)$oxy
mod.param <- TrenchR:::SetModificationPar()

fig06 <- list(

  mod.param = mod.param,

  TR.HiRes = TR$HiRes,
  TR.depth_HiRes = TR$depth_HiRes,
  TR.mean13_HiRes = TR$mean13_HiRes,

  T13.star = ModifyRecord(rec.in = TR$mean13_HiRes,
                          res = TR$HiRes,
                          depth.hires = TR$depth_HiRes,
                          depth.lores = TR$depth,
                          SIGMA = mod.param$SIGMA.opt,
                          STRETCH = mod.param$STRETCH.opt,
                          ADV = mod.param$ADV.opt),
  T13.starstar = ModifyRecord(rec.in = TR$mean13_HiRes,
                              res = TR$HiRes,
                              depth.hires = TR$depth_HiRes,
                              depth.lores = TR$depth,
                              SIGMA = mod.param$SIGMA.ind,
                              STRETCH = mod.param$STRETCH.ind,
                              ADV = mod.param$ADV.ind),

  v2 = ModifyRecord(rec.in = TR$mean13_HiRes,
                    res = TR$HiRes,
                    depth.hires = TR$depth_HiRes,
                    depth.lores = TR$depth,
                    STRETCH = mod.param$STRETCH.opt)$HiRes,
  v3 = ModifyRecord(rec.in = TR$mean13_HiRes,
                    res = TR$HiRes,
                    depth.hires = TR$depth_HiRes,
                    depth.lores = TR$depth,
                    SIGMA = mod.param$SIGMA.opt)$LoRes,
  v4 = ModifyRecord(rec.in = TR$mean13_HiRes,
                    res = TR$HiRes,
                    depth.hires = TR$depth_HiRes,
                    depth.lores = TR$depth,
                    ADV = mod.param$ADV.only)$LoRes,

  sum.max = c(27, 35, 40, 48)

)

# ------------------------------------------------------------------------------
# fig07

TR <- TrenchR:::prepareTrenchData()$oxy
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

fig07 <- list(

  T13.starstar = T13.starstar,

  diff.13 = diff.13,
  diff.15 = diff.15,
  diff.2yr = diff.2yr

)

# ------------------------------------------------------------------------------

save(fig01, fig03, fig04, fig05, fig06, fig07,
     file = "./tests/testthat/test_data/figure_data_tc17.rda")

