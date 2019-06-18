##' Produce TC17 Figure 07.
##'
##' This function makes all necessary calculations and plots the results for
##' Figure 07 shown in Münch et al. (2017).
##' @param mod.param  list of the parameters according to which the original
##' T13 mean profile is modified to model the temporal changes. It must containt
##' the following elements: \code{ADV.ind}, \code{SIGMA.ind} and
##' \code{STRETCH.ind} (see also \code{\link{SetModificationPar}}). If
##' \code{NULL} (the default), the original data from Münch et al. (2017) is
##' used for plotting.
##' @author Thomas Münch
##' @references
##' Münch, T., et al., Constraints on post-depositional isotope modifications
##' in East Antarctic firn from analysing temporal changes of isotope profiles,
##' The Cryosphere, doi:10.5194/tc-11-2175-2017, 2017.
##' @seealso \code{\link{SetModificationPar}}
##' @export
TC17.Fig07 <- function(mod.param = NULL) {
    
    if (is.null(mod.param)) {
        mod.param <- SetModificationPar()
    }

    TR = prepareTrenchData()$oxy

    pars <- SetPlotPar(mfrow = c(1, 2), xaxs = "r", yaxs = "i", lwd = 1.5,
                       mar = c(5, 5.5, 0.5, 0.5))
    op <- par(pars)

    # T13**
    T13.starstar <- ModifyT13(TR,
                              SIGMA = mod.param$SIGMA.ind,
                              STRETCH = mod.param$STRETCH.ind,
                              ADV = mod.param$ADV.ind)

    # profile differences
    diff.13 <- TR$mean13.1 -
        Hmisc::Lag(TR$mean13.2, shift = TR$k13 / TR$LoRes)

    diff.15 <- TR$mean15.1_HiRes -
        Hmisc::Lag(TR$mean15.2_HiRes, shift = TR$k15 / TR$HiRes)
    diff.15 <- diff.15[match(TR$depth, TR$depth_HiRes)]

    diff.2yr <- TR$mean15 - T13.starstar$LoRes

    
    #---------------------------------------------------------------------------
    # Fig07-a

    hist(diff.13, breaks = 10, freq = FALSE,
         xlim = c(-4, 4), ylim = c(0, 0.525),
         density = 10, main = "", xlab = "", ylab = "", col = "dodgerblue")
    hist(diff.15, add = TRUE, breaks = 10, freq = FALSE,
         density = 10, angle = -45)

    mtext("Spatial differences between trenches (\u2030)",
          side = 1, line = 3.5,
          cex = pars$cex.lab, font = pars$font.lab)
    mtext(expression(bold("Normalised frequency density (\u2030")
                     ^{"-1"} * bold(")")),
          side = 2, line = 3.75, las = 0,
          cex = pars$cex.lab, font = pars$font.lab)

    legend("topleft", c(expression(Delta * bold("T13")),
                        expression(Delta * bold("T15"))),
           lwd = 1.5, lty = 1, col = c("dodgerblue", "black"),
           bty = "n", inset = c(0, 0.02))

    
    #---------------------------------------------------------------------------
    # Fig07-b

    hist(c(diff.13, diff.15), breaks = 10, freq = FALSE,
         xlim = c(-4, 4), ylim = c(0, 0.525),
         density = 10, main = "", xlab = "", ylab = "", col = "dimgrey")
    hist(diff.2yr, add = TRUE, breaks = 10, freq = FALSE,
         density = 10, angle = -45, col = "firebrick")

    mtext("Spatial and temporal differences (\u2030)",
          side = 1, line = 3.5,
          cex = pars$cex.lab, font = pars$font.lab)
    mtext(expression(bold("Normalised frequency density (\u2030")
                     ^{"-1"} * bold(")")),
          side = 2, line = 3.75, las = 0,
          cex = pars$cex.lab, font = pars$font.lab)

    legend("topleft",
           c(expression(bold("Combined spatial: ") * Delta *
                        bold("T13 and ") * Delta * bold("T15")),
             expression(bold("Temporal: T15-T13**"))),
           lwd = 1.5, lty = 1, col = c("dimgrey", "firebrick"),
           bty = "n", inset = c(0, 0.02))

    par(op)

}

