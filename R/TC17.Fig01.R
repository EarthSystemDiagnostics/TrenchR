##' Produce TC17 Figure 01.
##'
##' This function makes all necessary calculations and plots the results for
##' Figure 01 shown in Münch et al. (2017).
##' @author Thomas Münch
##' @references
##' Münch, T., et al., Constraints on post-depositional isotope modifications
##' in East Antarctic firn from analysing temporal changes of isotope profiles,
##' The Cryosphere, doi:10.5194/tc-11-2175-2017, 2017.
##' @export
TC17.Fig01 <- function() {

    TR <- prepareTrenchData()$oxy
    T13.annual <- T13AnnualMeans(t1 = TR$mean13.1,
                                 t2 = Hmisc::Lag(TR$mean13.2,
                                                 TR$k13 / TR$LoRes),
                                 depth = TR$depth)

    pars <- SetPlotPar(mar = c(5, 5, 4, 2), mfrow = c(1, 2))
    op <- par(pars)
    

    #---------------------------------------------------------------------------
    # Fig01-a

    plot(TR$depth[1 : 38], TR$mean12.1, type = 'n',
         xlim = c(0, 125), ylim = c(-49, -39), axes = FALSE,
         xlab = "", ylab = "")
    axis(1)
    axis(2, las = 1)
    box()
    axis(3, labels = T13.annual$summer.max$years,
         at = T13.annual$summer.max$depth,
         cex.axis = 0.75 * pars$cex.lab)
    mtext("Depth (cm)", side = 1, line = 3.5, cex = pars$cex.lab,
          font = pars$font.lab)
    mtext(expression(bold("Trench ") *
                     delta^bold("18") * bold("O") * bold(" (\u2030)")),
          side = 2, line = 3.25, las = 0,
          cex = pars$cex.lab, font = pars$font.lab)
    mtext("Year", side = 3, line = 2.75,
          cex = pars$cex.lab, font = pars$font.lab)

    abline(v = T13.annual$summer.max$depth,
           lty = 5, lwd = 1.5, col = "darkgrey")

    lines(TR$depth[1 : 38], TR$mean13.1, lty = 1, col = "black")
    lines(TR$depth[1 : 38],
          Hmisc::Lag(TR$mean13.2, TR$k13 / TR$LoRes),
          col = "firebrick3")

    lines(T13.annual$means$depth, T13.annual$means$T1,
          type = 'b', lty = 1, pch = 19, col = "black")
    lines(T13.annual$means$depth, T13.annual$means$T2,
          type = 'b', lty = 1, pch = 19, col = "firebrick3")

    Polyplot(T13.annual$means$depth,
             T13.annual$means$max.T1, T13.annual$means$min.T1,
             col = "black", alpha = 0.15)
    Polyplot(T13.annual$means$depth,
             T13.annual$means$max.T2, T13.annual$means$min.T2,
             col = "firebrick3", alpha = 0.15)

    legend("topright",
           legend = c("T13-1", "T13-2"),
           col = c(1, "firebrick3"),
           cex = 1.25, text.font = 2, text.col = c(1, "firebrick3"), bty = "n")
    legend("bottomright", c("3 cm resolution", "Annual mean"),
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
         cex.axis = 0.75 * pars$cex.lab)
    axis(3, at = y.lines,
         labels = c("2013", "2012", "2011", "2010", "2009", "2008"),
         cex.axis = 0.75 * pars$cex.lab)
    axis(2)
    box()

    mtext("Year", side = c(1, 3), line = c(3.5, 2.75),
          cex = pars$cex.lab, font = pars$font.lab)
    mtext(bquote(bold("AWS 2m air temperature"~paste('(',degree,'C)'))),
          side = 2, line = 3.25, las = 0,
          cex = pars$cex.lab, font = pars$font.lab)

    legend("bottomright", c("Monthly mean", "Annual mean"),
           col = c("black", "dodgerblue"), lty = c(1, NA), pch = c(NA, 19),
           cex = 1.25, text.font = 2, bty = "n")

    par(op)

}

