##' Produce TC17 Figures.
##'
##' Details
##' @param path 
##' @param device 
##' @param save.plot 
##' @author Thomas Münch
MakeTC17Figures <- function(path = file.path(getwd(), "plots"),
                            device = "quartz", save.plot = FALSE) {

    TR1 <- prepareTrenchData()
    TR2 <- prepareTrenchData(na.treat = TRUE)
    mod.param <- list()
    mod.param$ADV        <- 100
    mod.param$ADVopt     <- 101
    mod.param$SIGMA      <- 1.9
    mod.param$SIGMAopt   <- 2.3
    mod.param$stretch    <- 2.2
    mod.param$stretchOPT <- 3.5

    TC17.Fig01(TR1, path = path, device = device, save.plot = save.plot)

    TC17.Fig03(TR2, path = path, device = device, save.plot = save.plot)

    TC17.Fig04(TR2, path = path, device = device, save.plot = save.plot)

    TC17.Fig05(ParamSpace, path = path, device = device, save.plot = save.plot)

    TC17.Fig06(TR2, path = path, device = device, save.plot = save.plot,
               mod.param = mod.param)

    TC17.Fig07(TR1, path = path, device = device, save.plot = save.plot,
               mod.param = mod.param)

}
    
