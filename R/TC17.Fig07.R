##' Produce TC17 Figure 07.
##'
##' This function makes all necessary calculations and plots the results for
##' Figure 07 shown in Muench et al. (2017).
##' @param TR The results from a call to \code{\link{prepareTrenchData()}}.
##' @param path The path to the directory in which to save the plot (for
##' \code{save.plot = TRUE}). Defaults to the folder \code{plots} in the current
##' working directory. If this folder does not exist, it is attempted to create
##' with a warning (see also \code{\link{OpenDevice}}).
##' @param file.name The name of the file (excluding extension) to save the
##' plot in.
##' @param device The graphics device to be used to display and save the
##' plot. Defaults to the \code{quartz} device which is the only currently
##' implemented device option.
##' @param save.plot if \code{TRUE}, the plot is saved as a pdf file in the
##' folder specified by \code{path}. Defaults to \code{FALSE} which results in
##' on-screen display of the plot.
##' @param mod.param  A list with the parameters according to which the T13 mean
##' profile shall be modified. It must containt the following elements:
##' \code{ADV}, \code{ADVopt}, \code{SIGMA}, \code{SIGMAopt}, \code{stretch},
##' and \code{stretchOPT}.
##' @author Thomas Münch
##' @references
##' Muench, T., et al., Constraints on post-depositional isotope modifications
##' in East Antarctic firn from analysing temporal changes of isotope profiles,
##' The Cryosphere Discussions, doi:10.5194/tc-2017-35, 2017.
##' @export
TC17.Fig07 <- function(TR, path = file.path(getwd(), "plots"),
                       file.name = "tc17_fig_07", device = "quartz",
                       save.plot = FALSE, mod.param) {

    param <- SetPlotPar()
    plot.par <- param$par
    dev.size <- param$dev.size

    OpenDevice(device = device, path = path, file.name = file.name,
               height = dev.size$w, width = 2 * dev.size$w,
               save.plot = save.plot)
    par(plot.par)
    par(mfrow = c(1, 2), xaxs = "r", yaxs = "i", lwd = 1.5,
        mar = c(5, 5.5, 0.5, 0.5))

    T13.mod.oxy <- ModifyT13(TR$oxy, mod.param)

    diff.13 <- TR$oxy$mean13.1 -
        Hmisc::Lag(TR$oxy$mean13.2, shift = TR$oxy$k13 / TR$oxy$LoRes)

    diff.15 <- TR$oxy$mean15.1_HiRes -
        Hmisc::Lag(TR$oxy$mean15.2_HiRes, shift = TR$oxy$k15 / TR$oxy$HiRes)
    diff.15 <- diff.15[match(TR$oxy$depth, TR$oxy$depth_HiRes)]

    diff.2yr <- TR$oxy$mean15 - T13.mod.oxy$mean13.diff.stretch.adv

    
    #---------------------------------------------------------------------------
    # Fig07-a

    hist(diff.13, breaks = 10, freq = FALSE,
         xlim = c(-4, 4), ylim = c(0, 0.525),
         density = 10, main = "", xlab = "", ylab = "", col = "dodgerblue")
    hist(diff.15, add = TRUE, breaks = 10, freq = FALSE,
         density = 10, angle = -45)

    mtext("Spatial differences between trenches (\u2030)",
          side = 1, line = 3.5,
          cex = plot.par$cex.lab, font = plot.par$font.lab)
    mtext(expression(bold("Normalised frequency density (\u2030")
                     ^{"-1"} * bold(")")),
          side = 2, line = 3.75, las = 0,
          cex = plot.par$cex.lab, font = plot.par$font.lab)

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
          cex = plot.par$cex.lab, font = plot.par$font.lab)
    mtext(expression(bold("Normalised frequency density (\u2030")
                     ^{"-1"} * bold(")")),
          side = 2, line = 3.75, las = 0,
          cex = plot.par$cex.lab, font = plot.par$font.lab)

    legend("topleft",
           c(expression(bold("combined spatial: ") * Delta *
                        bold("T13 and ") * Delta * bold("T15")),
             expression(bold("temporal: T15-T13**"))),
           lwd = 1.5, lty = 1, col = c("dimgrey", "firebrick"),
           bty = "n", inset = c(0, 0.02))

    if (save.plot) dev.off()

}

