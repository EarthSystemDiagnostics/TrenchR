##' Title.
##'
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @param TR 
##' @param path 
##' @param "plots") 
##' @param file.name 
##' @param device 
##' @param dev.size 
##' @param save.plot 
##' @return 
##' @author Thomas Münch
##' @export
TC17.Fig04 <- function(TR, path = file.path(getwd(), "plots"),
                       file.name = "tc17_fig_04", device = "quartz",
                       dev.size = list(h = 6, w = 8), save.plot = FALSE) {
    
    plot.par <- SetPlotPar()
    plot.file <- file.path(path, file.name)

    if (device == "quartz") {
        quartzFonts(optima = c("Optima Regular", "Optima Bold",
                               "Optima Italic", "Optima Bold Italic"))
    }

    OpenDevice(device = device, plot.file = plot.file,
               height = dev.size$h, width = dev.size$w,
               save.plot = save.plot)
    par(plot.par)
    par(oma = c(5, 0, 0.5, 0), mar = c(0, 6, 0, 6))
    if (device == "quartz") par(family = "optima")

    ind1 <- which(TR$oxy$depth <= TR$oxy$SRF.b$t15)
    ind2 <- which(TR$oxy$depth <= TR$oxy$SRF.b$t13)
    v1 <- TR$oxy$mean15
    v2 <- TR$oxy$mean13
    v1[ind1[-length(ind1)]] <- NA
    v2[ind2[-length(ind2)]] <- NA

    plot(TR$oxy$depth, v1, type = "l", axes = FALSE,
         xlab = "", ylab = "", xlim = c(0, 175), ylim = c(-58, -36))
    lines(TR$oxy$depth[ind1], TR$oxy$mean15[ind1],
          lwd = 1.5, lty = 5, col = "black")

    par(yaxp = c(-52, -36, 4))
    axis(2)
    minor.tick(nx = 1, ny = 2, side = 2)
    text(-35, -44,
         labels = expression(delta^bold("18") * bold("O")*bold(" (\u2030)")),
         srt = 90, xpd = NA, cex = my.par$cex.lab, font = my.par$font.lab,
         col = "black")
    text(17.5, -37, "T15 (2015)",
         cex = my.par$cex.lab, font = my.par$font.lab)

    par(new = TRUE)

    plot(TR$oxy$depth[1:38], v2, type = "l", axes = FALSE,
         xlab = "", ylab = "", xlim = c(0, 175), ylim = c(-48, -26),
         col = "dodgerblue")
    lines(TR$oxy$depth[ind2], TR$oxy$mean13[ind2], lwd = 1.5, lty = 5,
          col = "dodgerblue")

    par(yaxp = c(-48, -40, 2))
    axis(4, col = "dodgerblue", col.axis = "dodgerblue")
    minor.tick(nx = 1, ny = 2, side = 4, col = "dodgerblue")
    par(xaxp = c(0, 175, 7))
    axis(1)

    text(210, -44,
         labels = expression(delta^bold("18") * bold("O") * bold(" (\u2030)")),
         srt = -90, xpd = NA, cex = my.par$cex.lab, font = my.par$font.lab,
         col = "dodgerblue")
    mtext("Depth (cm)", side = 1, line = 3.5,
          cex = my.par$cex.lab, font = my.par$font.lab)
    text(175 - 17.5, -40.75, "T13 (2013)",
         cex = my.par$cex.lab, font = my.par$font.lab, col = "dodgerblue")

    if (save.plot) dev.off()

}
