##' TC17 Figure 01.
##'
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @param TR 
##' @param path 
##' @param "plots") 
##' @param device 
##' @param dev.size 
##' @param save.plot 
##' @author Thomas Münch
##' @export
TC17.Fig01 <- function(TR, path = file.path(getwd(), "plots"),
                       file.name = "tc17_fig_01.pdf", device = "quartz",
                       dev.size = list(h = 6, w = 8), save.plot = FALSE) {

    plot.par <- SetPlotPar()
    plot.file <- file.path(path, file.name)

    T13.annual <- T13AnnualMeans(t1 = TR$oxy$mean13.1,
                                 t2 = BinShift(TR$oxy$k13 / TR$oxy$LoRes,
                                               TR$oxy$mean13.2),
                                 depth = TR$oxy$depth)

    if (device == "quartz") {
        quartzFonts(optima = c("Optima Regular", "Optima Bold",
                               "Optima Italic", "Optima Bold Italic"))
    }
    OpenDevice(device = device, plot.file = plot.file,
               height = dev.size$h + 0.75, width = 2 * dev.size$w,
               save.plot = save.plot)

    par(plot.par)
    par(mfrow = c(1, 2))
    par(mar = c(5, 5, 4, 2))
    if (device == "quartz") par(family = 'optima')


    #---------------------------------------------------------------------------
    # Fig01-a

    plot(TR$oxy$depth[1 : 38], TR$oxy$mean12.1, type = 'n',
         xlim = c(0, 125), ylim = c(-49, -39), axes = FALSE,
         xlab = "", ylab = "")
    axis(1)
    axis(2, las = 1)
    box()
    axis(3, labels = T13.annual$summer.max$years,
         at = T13.annual$summer.max$depth,
         cex.axis = 0.75 * plot.par$cex.lab)
    mtext('Depth (cm)', side = 1, line = 3.5, cex = plot.par$cex.lab,
          font = plot.par$font.lab)
    mtext(expression(bold("Trench ") *
                     delta^bold("18") * bold("O") * bold(" (\u2030)")),
          side = 2, line = 3.5, las = 0,
          cex = plot.par$cex.lab, font = plot.par$font.lab)
    mtext("Year", side = 3, line = 2.85,
          cex = plot.par$cex.lab, font = plot.par$font.lab)

    abline(v = T13.annual$summer.max$depth,
           lty = 5, lwd = 1.5, col = "darkgrey")

    lines(TR$oxy$depth[1 : 38], TR$oxy$mean13.1, lty = 1, col = "black")
    lines(TR$oxy$depth[1 : 38],
          BinShift(TR$oxy$k13 / TR$oxy$LoRes, TR$oxy$mean13.2),
          col = "firebrick3")

    lines(T13.annual$means$depth, T13.annual$means$T1,
          type = 'b', lty = 1, pch = 19, col = "black")
    lines(T13.annual$means$depth, T13.annual$means$T2,
          type = 'b', lty = 1, pch = 19, col = "firebrick3")

    polyplot(T13.annual$means$depth,
             rbind(T13.annual$means$min.T1, T13.annual$means$max.T1),
             col = adjustcolor("black", 0.15))
    polyplot(T13.annual$means$depth,
             rbind(T13.annual$means$min.T2, T13.annual$means$max.T2),
             col = adjustcolor("firebrick3", 0.15))

    legend("topright",
           legend = c("T13-1", "T13-2"),
           col = c(1, "firebrick3"),
           cex = 1.25, text.font = 2, text.col = c(1, "firebrick3"), bty = "n")
    legend("bottomright", c("3 cm resolution", "annual mean"),
           col = "darkgrey", lty = c(1, NA), pch = c(NA, 19), cex = 1.25,
           text.font = 2, bty = "n")


    #---------------------------------------------------------------------------
    # Fig01-b

    xlim <- as.POSIXct(c("2013-03-01 00:00:00","2008-01-01 00:00:00"),
                       tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    y.lines <- as.POSIXct(paste(c("2013-01-01", "2012-01-01", "2011-01-01",
                                  "2010-01-01", "2009-01-01","2008-01-01"),
                                "00:00:00"),
                          tz = "UTC", format = "%Y-%m-%d %H:%M:%S")

    plot(aws9$monthly$dates.posix, aws9$monthly$t2m, type = "l", axes = FALSE,
         xlim = xlim, ylim = c(-60, -20), xlab = "", ylab = "")
    lines(aws9$annual$dates.posix, aws9$annual$t2m,
          type="b",lty=1,pch=19,col="dodgerblue")

    abline(v = y.lines, lty = 5, lwd = 1.5, col = "darkgrey")
    axis(1, at = y.lines,
         labels = c("2013", "2012", "2011", "2010", "2009", "2008"),
         cex.axis = 0.75 * plot.par$cex.lab)
    axis(3, at = y.lines,
         labels = c("2013", "2012", "2011", "2010", "2009", "2008"),
         cex.axis = 0.75 * plot.par$cex.lab)
    axis(2)
    box()

    mtext("Year", side = c(1, 3), line = c(3.5, 2.85),
          cex = plot.par$cex.lab, font = plot.par$font.lab)
    mtext(bquote(bold("AWS 2m air temperature"~paste('(',degree,'C)'))),
          side = 2, line = 3.5, las = 0,
          cex = plot.par$cex.lab, font = plot.par$font.lab)

    legend("bottomright", c("monthly mean", "annual mean"),
           col = c("black", "dodgerblue"), lty = c(1, NA), pch = c(NA, 19),
           cex = 1.25, text.font = 2, bty = "n")

    if (save.plot) dev.off()

}

