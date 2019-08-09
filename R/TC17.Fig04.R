##' Produce TC17 Figure 04.
##'
##' This function makes all necessary calculations and plots the results for
##' Figure 04 shown in Münch et al. (2017).
##' @author Thomas Münch
##' @inherit Muench2017 references
TC17.Fig04 <- function() {

    TR = prepareTrenchData(na.treat = TRUE)$oxy
    
    pars <- SetPlotPar(oma = c(5, 0, 0.5, 0), mar = c(0, 6, 0, 6))
    op <- par(pars)

    ind1 <- which(TR$depth <= TR$SRF.b$t15)
    ind2 <- which(TR$depth <= TR$SRF.b$t13)
    v1 <- TR$mean15
    v2 <- TR$mean13
    v1[ind1[-length(ind1)]] <- NA
    v2[ind2[-length(ind2)]] <- NA

    plot(TR$depth, v1, type = "l", axes = FALSE,
         xlab = "", ylab = "", xlim = c(0, 175), ylim = c(-58, -36))
    lines(TR$depth[ind1], TR$mean15[ind1],
          lwd = 1.5, lty = 5, col = "black")

    par(yaxp = c(-52, -36, 4))
    axis(2)
    MinorTick(nx = 1, ny = 2, side = 2)
    text(-35, -44,
         labels = expression(delta^bold("18") * bold("O")*bold(" (\u2030)")),
         srt = 90, xpd = NA, cex = pars$cex.lab, font = pars$font.lab,
         col = "black")
    text(17.5, -37, "T15 (2015)",
         cex = pars$cex.lab, font = pars$font.lab)

    par(new = TRUE)

    plot(TR$depth[1:38], v2, type = "l", axes = FALSE,
         xlab = "", ylab = "", xlim = c(0, 175), ylim = c(-48, -26),
         col = "dodgerblue")
    lines(TR$depth[ind2], TR$mean13[ind2], lwd = 1.5, lty = 5,
          col = "dodgerblue")

    par(yaxp = c(-48, -40, 2))
    axis(4, col = "dodgerblue", col.axis = "dodgerblue")
    MinorTick(nx = 1, ny = 2, side = 4, col = "dodgerblue")
    par(xaxp = c(0, 175, 7))
    axis(1)

    text(210, -44,
         labels = expression(delta^bold("18") * bold("O") * bold(" (\u2030)")),
         srt = -90, xpd = NA, cex = pars$cex.lab, font = pars$font.lab,
         col = "dodgerblue")
    mtext("Depth (cm)", side = 1, line = 3.5,
          cex = pars$cex.lab, font = pars$font.lab)
    text(175 - 17.5, -40.75, "T13 (2013)",
         cex = pars$cex.lab, font = pars$font.lab, col = "dodgerblue")

    par(op)

}
