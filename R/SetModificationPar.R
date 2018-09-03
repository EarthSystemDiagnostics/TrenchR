##' T13 temporal change parameters.
##'
##' This function provides a list of the parameters used in Muench et al. (2017)
##' according to which the temporal change of the original T13 mean trench
##' profile was modelled.
##'
##' The modification parameters are given as arguments to this function. Default
##' values are the ones presented and used in the paper; different values could
##' be specified by setting the respective parameter values in the function
##' call. The default optimal parameters stem from the analysis in
##' \code{\link{LoopParamSpace}} stored in \code{{ParamSpace}. The parameters
##' can be used to model the temporal change of the T13 mean profile using the
##' function \code{\link{ModifyT13}}.
##' @return A list of the following modification parameters:
##' \itemize{
##'   \item ADV.opt optimal 2-yr downward advection [cm];
##'   \item SIGMA.opt optimal 2-yr differential diffusion length [cm];
##'   \item STRETCH.opt optimal 2-yr compression from densification [cm];
##'   \item ADV.ind independently inferred 2-yr downward advection [cm];
##'   \item SIGMA.ind independently inferred 2-yr differential diffusion length
##'     [cm]; 
##'   \item STRETCH.ind independently inferred 2-yr compression [cm];
##'   \item ADV.only optimal 2-yr downward advection [cm] allowing no diffusion
##'     and densification ("advection only").
##' }
##' @seealso \code{\link{LoopParamSpace}}; \code{\link{ModifyT13}}
##' @author Thomas Münch
##' @export
SetModificationPar <- function(ADV.opt = ParamSpace$adv.opt,
                               SIGMA.opt = ParamSpace$sigma.opt,
                               STRETCH.opt = ParamSpace$densf.opt,
                               ADV.ind = 50, SIGMA.ind = 1.9,
                               STRETCH.ind = 2.2, ADV.only = 48.5) {
    
    mod.param <- as.list(environment())
    return(mod.param)

}

