##' Title
##'
##' Description
##' @param TR 
##' @param path 
##' @param file.name 
##' @param device 
##' @param dev.size 
##' @param save.plot 
##' @author Thomas Münch
TC17.Fig07 <- function(TR, path = file.path(getwd(), "plots"),
                       file.name = "tc17_fig_07", device = "quartz",
                       dev.size = list(h = 6, w = 8), save.plot = FALSE) {

    plot.par <- SetPlotPar()
    plot.file <- file.path(path, file.name)

    if (device == "quartz") {
        quartzFonts(optima = c("Optima Regular", "Optima Bold",
                               "Optima Italic", "Optima Bold Italic"))
    }

    OpenDevice(device = device, plot.file = plot.file,
               height = dev.size$h, width = 2 * dev.size$w,
               save.plot = save.plot)
    par(plot.par)
    par(mfrow = c(1, 2), xaxs = "r", yaxs = "i", lwd = 1.5,
        mar = c(5, 5.5, 0.5, 0.5))
    if (device == "quartz") par(family = "optima")

    T12.mod.oxy <- ModifyT13(TR$oxy, param$oxy)

    diff.13 <- TR$oxy$mean13.1 -
        Hmisc::Lag(TR$oxy$mean13.2, k = TR$oxy$k12/TR$oxy$LoRes)

    diff.15 <- TR$oxy$mean15.1_HiRes -
        Hmisc::Lag(TR$oxy$mean15.2_HiRes, k = TR$oxy$k14/TR$oxy$HiRes)
    diff.15 <- diff.15[match(TR$oxy$depth, TR$oxy$depth_HiRes)]

    diff.2yr <- TR$oxy$mean15 - T12.mod.oxy$mean13.diff.stretch.adv

    
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

