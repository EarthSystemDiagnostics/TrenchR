##' Produce all TC17 Figures.
##'
##' This function is a wrapper to produce all figures shown in Muench et
##' al. (2017).
##' @param adv.opt
##' @param sigma.opt
##' @param stretch.opt
##' @param adv.ind
##' @param sigma.ind
##' @param stretch.ind
##' @param path The path to the directory in which to save the plots (for
##' \code{save.plot = TRUE}). Defaults to the folder \code{plots} in the current
##' working directory. If this folder does not exist, it is attempted to create
##' with a warning (see also \code{\link{OpenDevice}}). 
##' @param device The graphics device to be used to display and save the
##' plots. Defaults to the \code{quartz} device which is the only currently
##' implemented device option.
##' @param save.plot if \code{TRUE}, the plots are saved as files in the
##' folder specified by \code{path}. Defaults to \code{FALSE} which results in
##' on-screen display of the plots.
##' @author Thomas Münch
##' @references
##' Muench, T., et al., Constraints on post-depositional isotope modifications
##' in East Antarctic firn from analysing temporal changes of isotope profiles,
##' The Cryosphere, doi:10.5194/tc-11-2175-2017, 2017.
##' @export
MakeTC17Figures <- function(adv.opt = ParamSpace$adv.opt,
                            sigma.opt = ParamSpace$sigma.opt,
                            stretch.opt = ParamSpace$densf.opt,
                            adv.ind = 50, sigma.ind = 1.9, stretch.ind = 2.2,
                            adv.only = 48.5,
                            path = file.path(getwd(), "plots"),
                            device = "quartz", save.plot = FALSE) {

    TR1 <- prepareTrenchData()
    TR2 <- prepareTrenchData(na.treat = TRUE)

    # modification parameters (optimal and independent) for temporal changes of T13
    mod.param <- list()
    mod.param$ADV.ind     <- adv.ind  / TR1$oxy$HiRes
    mod.param$ADV.opt     <- adv.opt  / TR1$oxy$HiRes
    mod.param$ADV.only    <- adv.only / TR1$oxy$HiRes
    mod.param$SIGMA.ind   <- sigma.ind
    mod.param$SIGMA.opt   <- sigma.opt
    mod.param$STRETCH.ind <- stretch.ind
    mod.param$STRETCH.opt <- stretch.opt

    TC17.Fig01(TR1$oxy, path = path, device = device, save.plot = save.plot)

    TC17.Fig02(path = path, device = device, save.plot = save.plot)

    TC17.Fig03(TR2$oxy, path = path, device = device, save.plot = save.plot)

    TC17.Fig04(TR2$oxy, path = path, device = device, save.plot = save.plot)

    TC17.Fig05(path = path, device = device, save.plot = save.plot)

    TC17.Fig06(TR2$oxy, path = path, device = device, save.plot = save.plot,
               mod.param = mod.param)

    TC17.Fig07(TR1$oxy, path = path, device = device, save.plot = save.plot,
               mod.param = mod.param)

}
    
