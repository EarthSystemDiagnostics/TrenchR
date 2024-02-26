#
# ----- Data processing function -----------------------------------------------
#

#' High-depth resolution set of Kohnen T13 and T15 mean profiles for the
#' analyses in Münch et al. (2017).
#'
#' @param .var character string with the name of the trench data variable for
#'   which mean profiles are to be calculated.
#' @param na.rm a logical evaluating to \code{TRUE} or \code{FALSE} indicating
#'   whether \code{NA} values should be stripped before the computation of the
#'   mean profile proceeds.
#' @return a list of the T13 and T15 mean profiles (for each individual trench
#'   and the seasonal mean) on the original depth resolution as well as on the
#'   higher depth resolution as specified in the variable tc17.paper.param.
#' @author Thomas Münch
#' @noRd
makeHiResKohnenTrenches <- function(.var = "d18O", na.rm = FALSE) {

  intpl <- function(x, newdepth) {
    tibble::tibble(depth = newdepth, y = approx(x$depth, x$y, newdepth)$y)
  }
  seasonalMean <- function(t1, t2, lag) {
    tibble::tibble(
      t1$depth,
      {
        cbind(t1[["y"]], prxytools::Lag(t2[["y"]], shift = lag)) %>%
          rowMeans()
      }) %>%
      setNames(c("depth", "y"))
  }

  trPar <- tc17.paper.param

  x <- range(getZ(t15.trench1)[trPar$ix])
  depth_HiRes <- seq(x[1], x[2], by = trPar$hiRes)

  # T13 mean profiles
  mean13.1 <- t13.trench1 %>%
    dplyr::filter(profileName != "T13-1-01") %>%
    makeMean(.var = .var, na.rm = na.rm, df = TRUE) %>%
    setNames(c("depth", "y"))
  mean13.2 <- t13.trench2 %>%
    makeMean(.var = .var, na.rm = na.rm, df = TRUE) %>%
    setNames(c("depth", "y"))

  # T15 mean profiles for depth range analysed in paper
  mean15.1 <- t15.trench1 %>%
    dplyr::filter(profileName != "T15-1-DUNE1") %>%
    makeMean(.var = .var, na.rm = na.rm, df = TRUE) %>%
    dplyr::slice(trPar$ix) %>%
    setNames(c("depth", "y"))
  mean15.2 <- t15.trench2 %>%
    makeMean(.var = .var, na.rm = na.rm, df = TRUE) %>%
    dplyr::slice(trPar$ix) %>%
    setNames(c("depth", "y"))

  # interpolate mean profiles onto higher depth resolution

  mean13.1_HiRes <- intpl(mean13.1, depth_HiRes)
  mean13.2_HiRes <- intpl(mean13.2, depth_HiRes)
  mean15.1_HiRes <- intpl(mean15.1, depth_HiRes)
  mean15.2_HiRes <- intpl(mean15.2, depth_HiRes)

  # calculate overall mean profiles for each Kohnen season

  mean13_HiRes <- seasonalMean(mean13.1_HiRes, mean13.2_HiRes,
                               lag = trPar$k13 / trPar$hiRes)
  mean15_HiRes <- seasonalMean(mean15.1_HiRes, mean15.2_HiRes,
                               lag = trPar$k15 / trPar$hiRes)

  mean13 <- tibble::tibble(
    depth = mean13.1$depth,
    y = approx(depth_HiRes, mean13_HiRes$y, mean13.1$depth)$y)
  mean15 <- tibble::tibble(
    depth = mean15.1$depth,
    y = approx(depth_HiRes, mean15_HiRes$y, mean15.1$depth)$y)

  list(mean13.1 = mean13.1, mean13.2 = mean13.2,
       mean15.1 = mean15.1, mean15.2 = mean15.2,
       mean13.1_HiRes = mean13.1_HiRes,
       mean13.2_HiRes = mean13.2_HiRes,
       mean15.1_HiRes = mean15.1_HiRes,
       mean15.2_HiRes = mean15.2_HiRes,
       mean13 = mean13, mean15 = mean15,
       mean13_HiRes = mean13_HiRes,
       mean15_HiRes = mean15_HiRes)

}

#
# ----- Plotting function ------------------------------------------------------
#

#' Münch et al. (2017) figures
#'
#' Reproduce a desired figure of the Münch et al. (2017) publication (TC17).
#'
#' Appropriate figure aspect ratios (height x width (h x w)) in inch for the
#' graphical output device are as follows:
#'
#' \describe{
#'   \item{Fig. 1:}{h = 6.75, w = 16}
#'   \item{Fig. 2:}{h = 8, w = 8}
#'   \item{Fig. 3a, b:}{h = 6, w = 9.75}
#'   \item{Fig. 3c:}{h = 6, w = 8}
#'   \item{Fig. 4:}{h = 6, w = 8}
#'   \item{Fig. 5:}{h = 6, w = 8}
#'   \item{Fig. 6:}{h = 6, w = 16}
#'   \item{Fig. 7:}{h = 6, w = 12}
#' }
#'
#' @param which.figure character string to name the figure which shall be
#'   reproduced; must match one of "f1", "f2", "f3a", "f3b", "f3c", "f4", "f5",
#'   "f6", "f7".
#' @author Thomas Münch
#' @inherit Muench2017 references
#'
produceTC17Figures <- function(which.figure = c("f1", "f2", "f3a", "f3b", "f3c",
                                                "f4", "f5", "f6", "f7")) {

  # ----------------------------------------------------------------------------
  # individual figure function definitions

  TC17.Fig01 <- function() {

    trPar <- tc17.paper.param

    mean13.1 <- t13.trench1 %>%
      dplyr::filter(profileName != "T13-1-01") %>%
      makeMean(df = TRUE)
    mean13.2 <- t13.trench2 %>%
      makeMean(df = TRUE) %>%
      dplyr::mutate(d18O = prxytools::Lag(.data$d18O, trPar$k13 / trPar$loRes))

    T13.annual <- t13.annual.means$cheated
    # <- the 'cheated' data reproduce an earlier version of the data which were
    # used in Münch et al. (2017) based on a sightly erroneous implementation of
    # the annual binning; the correct data is stored in list element
    # 'correct'. However, the difference between the data is only of the order
    # of ~0.2 permil (root-mean-square deviation) and does not influence any
    # conclusions drawn in the paper.

    op <- grfxtools::Par(mar = c(5, 5, 4, 2), mfrow = c(1, 2),
                         lwd = 2, font.lab = 2, font.axis = 2)


    #---------------------------------------------------------------------------
    # Fig01-a

    plot(mean13.1, type = "n", xlim = c(0, 125), ylim = c(-49, -39),
         axes = FALSE, xlab = "", ylab = "")
    axis(1)
    axis(2, las = 1)
    box()
    axis(3, labels = T13.annual$summer.max$years,
         at = T13.annual$summer.max$depth,
         cex.axis = 0.75 * par()$cex.lab)
    mtext("Depth (cm)", side = 1, line = 3.5, cex = par()$cex.lab,
          font = par()$font.lab)
    mtext(grfxtools::LabelAxis(prefix = "Trench", font = par()$font.lab),
          side = 2, line = 3.25, las = 0, cex = par()$cex.lab)
    mtext("Year", side = 3, line = 2.75,
          cex = par()$cex.lab, font = par()$font.lab)

    abline(v = T13.annual$summer.max$depth,
           lty = 5, lwd = 1.5, col = "darkgrey")

    lines(mean13.1, col = "black")
    lines(mean13.2, col = "firebrick3")

    lines(T13.annual$means$depth, T13.annual$means$T1,
          type = "b", lty = 1, pch = 19, col = "black")
    lines(T13.annual$means$depth, T13.annual$means$T2,
          type = "b", lty = 1, pch = 19, col = "firebrick3")

    grfxtools::Polyplot(T13.annual$means$depth,
                        T13.annual$means$max.T1, T13.annual$means$min.T1,
                        col = "black", alpha = 0.15)
    grfxtools::Polyplot(T13.annual$means$depth,
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
         cex.axis = 0.75 * par()$cex.lab)
    axis(3, at = y.lines,
         labels = c("2013", "2012", "2011", "2010", "2009", "2008"),
         cex.axis = 0.75 * par()$cex.lab)
    axis(2)
    box()

    mtext("Year", side = c(1, 3), line = c(3.5, 2.75),
          cex = par()$cex.lab, font = par()$font.lab)
    mtext(grfxtools::LabelAxis("AWS 2m air temperature", unit = "celsius",
                               font = par()$font.lab),
          side = 2, line = 3.25, las = 0, cex = par()$cex.lab)

    legend("bottomright", c("Monthly mean", "Annual mean"),
           col = c("black", "dodgerblue"), lty = c(1, NA), pch = c(NA, 19),
           cex = 1.25, text.font = 2, bty = "n")

    par(op)

  }

  TC17.Fig02 <- function() {

    op <- grfxtools::Par(mar = c(6, 6, 6, 6), lwd = 2,
                         font.lab = 2, font.axis = 2)

    # lat/lon for relevant sites
    edml <- c(-75.0025, 0.0684)
    aws <- c(-75.00063, 0.07793)

    b41 <- c(-75.000706, 0.067905)
    b42 <- c(-75.000626, 0.067777)

    t13.1 <- list()
    t13.1$start <- c(-75.00641, 0.07498)
    t13.1$end <-   c(-75.00673, 0.076024)
    t13.2 <- list()
    t13.2$start <- c(-75.00849, 0.08693)
    t13.2$end <-   c(-75.00877, 0.08819)

    t15.1 <- list()
    t15.1$start <- c(-75.00719, 0.0796)
    t15.1$end <-   c(-75.00760, 0.0805)
    t15.2 <- list()
    t15.2$start <- c(-75.00944, 0.0967)
    t15.2$end <-   c(-75.00986, 0.0972)

    # distance scale along -75 S
    ref <- -75
    lon <- seq(0.065, 0.1, 0.001)
    dist.lon <- sapply(lon, function(l) {
      geosphere::distGeo(c(0, ref), c(l, ref))})

    dist.equi <- seq(1740, 2880, 20)
    lon.equidist <- approx(dist.lon, lon, dist.equi)$y

    xdist <- c(2000, 2200, 2400, 2600, 2800)
    xlon <- lon.equidist[match(xdist, dist.equi)]

    # distance scale along 0.08 E
    ref <- 0.08
    lat <- seq(-74.985, -75.025, -0.001)
    dist.lat <- sapply(lat, function(l){
      geosphere::distGeo(c(ref, lat[1]), c(ref, l))})

    dist.lat <- dist.lat - dist.lat[which(lat == -75)]

    dist.equi <- seq(-1660, 2780, 20)
    lat.equidist <- approx(dist.lat, lat, dist.equi)$y

    ydist <- c(-500, 0, 500, 1000, 1500)
    ylat <- lat.equidist[match(ydist, dist.equi)]

    # arrows for North direction and mean wind direction
    x0 <- 0.0975
    y0 <- -74.9975

    angle.meanwind <- 57. * pi / 180.
    len <- 0.005
    x1 <- 0.085
    y1 <- -75.005
    x2 <- x1 + len * sin(angle.meanwind)
    y2 <- y1 + len * cos(angle.meanwind)

    plot(aws[2], aws[1], type = "n", axes = FALSE,
         xlim = c(0.065, 0.1), ylim = c(-75.015, -74.995),
         xlab = "", ylab = "")
    grid(lty = 1, lwd = 0.75)

    axis(side = 1, at = seq(0.065, 0.1, 0.005),
         labels = c(0.065, NA, 0.075, NA, 0.085, NA, 0.095, NA))
    axis(side = 2)
    axis(side = 3, at = xlon, labels = xdist)
    axis(side = 4, at = ylat, labels = ydist)
    box(lwd = 1.5)

    mtext(bquote(paste(bold("Longitude ("), degree, bold("E)"))),
          side = 1, line = 4.5,
          cex = par()$cex.lab, font = par()$font.lab)
    mtext(bquote(paste(bold("Latitude ("), degree, bold("N)"))), las = 0,
          side = 2, line = 4.5,
          cex = par()$cex.lab, font = par()$font.lab)
    mtext("Distance (m)", side = 3, line = 4,
          cex = par()$cex.lab, font = par()$font.lab)
    text(0.108, -75.005, labels = "Distance (m)", srt = -90, xpd = NA,
         cex = par()$cex.lab, font = par()$font.lab)

    points(edml[2], edml[1], pch = 24, col = "black", bg = "black")
    points(edml[2], edml[1], pch = 25, col = "black", bg = "black")

    points(aws[2], aws[1], pch = 23,
           col = "black", bg = "dodgerblue3", cex = 1.5, lwd = 1.5)

    points(b41[2], b41[1], pch = 21,
           col = "black", bg = "darkgreen", cex = 1.5, lwd = 1.5)
    points(b42[2], b42[1], pch = 21,
           col = "black", bg = "darkgreen", cex = 1.5, lwd = 1.5)

    points(t13.1$start[2], t13.1$start[1],
           col = "black", lwd = 0.75, cex = 0.5)
    points(t13.1$end[2], t13.1$end[1],
           col = "black", lwd = 0.75, cex = 0.5)
    lines(c(t13.1$start[2], t13.1$end[2]), c(t13.1$start[1], t13.1$end[1]),
          col = "black", lwd = 2.5, lty = 1)

    points(t13.2$start[2], t13.2$start[1],
           col = "black", lwd = 0.75, cex = 0.5)
    points(t13.2$end[2], t13.2$end[1],
           col = "black", lwd = 0.75, cex = 0.5)
    lines(c(t13.2$start[2], t13.2$end[2]), c(t13.2$start[1], t13.2$end[1]),
          col = "firebrick3", lwd = 2.5, lty = 1)

    points(t15.1$start[2], t15.1$start[1],
           col = "black", lwd = 0.75, cex = 0.5)
    points(t15.1$end[2], t15.1$end[1],
           col = "black", lwd = 0.75, cex = 0.5)
    lines(c(t15.1$start[2], t15.1$end[2]), c(t15.1$start[1], t15.1$end[1]),
          col = "black", lwd = 2.5, lty = 1)

    points(t15.2$start[2], t15.2$start[1],
           col = "black", lwd = 0.75, cex = 0.5)
    points(t15.2$end[2], t15.2$end[1],
           col = "black", lwd = 0.75, cex = 0.5)
    lines(c(t15.2$start[2], t15.2$end[2]), c(t15.2$start[1], t15.2$end[1]),
          col = "firebrick3", lwd = 2.5, lty = 1)

    grfxtools::AddNorthArrow(scale = 0.5, padin = c(0.25, 0.25),
                             cex.text = 1.25, vadj.text = 0.9)
    arrows(x1, y1, x2, y2, code = 1, length = 0.15, angle = 25, lwd = 1)

    par(op)

  }

  TC17.Fig03a <- function() {

    op <- grfxtools::Par(oma = c(0, 0, 0, 0.5), mar = c(5, 5, 0.5, 2),
                         lwd = 2, font.lab = 2, font.axis = 2)

    # plot without DUNE1 profile
    t15.trench1 %>%
      dplyr::filter(profileName != "T15-1-DUNE1") %>%
      plot2D(rescale.v = 0.01,
             label = grfxtools::LabelAxis(font = par()$font.lab),
             xlim = c(0, 50), ylim = c(1.755, -0.15), zlim = c(-55, -35),
             filledContour = TRUE, fill = TRUE)

    par(op)

  }

  TC17.Fig03b <- function() {

    op <- grfxtools::Par(oma = c(0, 0, 0, 0.5), mar = c(5, 5, 0.5, 2),
                         lwd = 2, font.lab = 2, font.axis = 2)

    plot2D(t15.trench2, rescale.v = 0.01,
           label = grfxtools::LabelAxis(font = par()$font.lab),
           ylab = "", xlim = c(0, 50), ylim = c(1.755, -0.15), zlim = c(-55, -35),
           filledContour = TRUE, fill = TRUE)

    par(op)

  }

  TC17.Fig03c <- function() {

    trPar <- tc17.paper.param

    op <- grfxtools::Par(lwd = 2, font.lab = 2, font.axis = 2)

    # obtain mean profiles for depth range <= ~ 175 cm
    mean15.1 <- v1 <- t15.trench1 %>%
      dplyr::filter(profileName != "T15-1-DUNE1") %>% # neglect DUNE-1 profile
      makeMean(na.rm = TRUE, df = TRUE) %>%
      dplyr::slice(trPar$ix) %>%
      dplyr::mutate(depth = depth / 100) # depth in m
    mean15.2 <- v2 <- t15.trench2 %>%
      makeMean(na.rm = TRUE, df = TRUE) %>%
      dplyr::slice(trPar$ix) %>%
      dplyr::mutate(depth = depth / 100) # depth in m

    # differentiate from surface layer
    ind1 <- which(mean15.1$depth <= trPar$surfaceBot["t15.1"] / 100)
    ind2 <- which(mean15.2$depth <= trPar$surfaceBot["t15.2"] / 100)
    p1 <- (p1 <- which(!is.na(v1$d18O)))[c(1, length(p1))]
    p2 <- (p2 <- which(!is.na(v2$d18O)))[c(1, length(p2))]
    v1[ind1[-length(ind1)], "d18O"] <- NA
    v2[ind2[-length(ind2)], "d18O"] <- NA

    # account for optimal vertical shift
    mean15.2 <- dplyr::mutate(mean15.2, depth = .data$depth + (trPar$k15 / 100))
    v2 <- dplyr::mutate(v2, depth = .data$depth + (trPar$k15 / 100))

    # plot
    plot(mean15.1, type = "n", las = 1,
         xlim = c(-5, 175) / 100, ylim = c(-52, -34),
         axes = FALSE, xlab = "", ylab = "")
    par(xaxp = c(0, 1.5, 3))
    axis(1)
    grfxtools::MinorTick(tick.ratio = 0.75, extend = 1)
    par(yaxp = c(-52, -36, 4))
    axis(2)
    grfxtools::MinorTick(side = 2, extend = 1)
    box()
    abline(v = c(0, 50, 100, 150) / 100, col = "black", lty = "dotted")

    mtext("Depth (m)", side = 1, line = 3.5,
          cex = par()$cex.lab, font = par()$font.lab)
    mtext(grfxtools::LabelAxis(font = par()$font.lab),
          side = 2, line = 3.25, las = 0, cex = par()$cex.lab)

    lines(v1, col = "black")
    lines(v2, col = "firebrick3")

    lines(mean15.1[ind1, ], col = "black", lwd = 0.75 * par()$lwd, lty = 5)
    lines(mean15.2[ind2, ], col = "firebrick3", lwd = 0.75 * par()$lwd, lty = 5)

    points(mean15.1[p1, ], col = "black", pch = 1,
           lwd = 0.75 * par()$lwd, cex = 0.75)
    points(mean15.2[p2, ], col = "firebrick3", pch = 23,
           lwd = 0.75 * par()$lwd, cex = 0.75)

    grfxtools::Legend("bottomright", legend = c("T15-1", "T15-2"),
                      pch = c(1, 23), lwd = 0.75 * par()$lwd, lty = c(1, 1),
                      col = c(1, "firebrick3"), cex = 1.25,
                      text.font = par()$font.lab,
                      pt.cex = 0.75, pt.lwd = 0.75 * par()$lwd, bty = "n",
                      end.pch = TRUE, pch.xoff = 0.2)

    par(op)
  }

  TC17.Fig04 <- function() {

    trPar <- tc17.paper.param
    TR <- makeHiResKohnenTrenches(.var = "d18O", na.rm = TRUE)

    v1 <- TR$mean15
    v2 <- TR$mean13

    # differentiate from surface layer
    ind1 <- which(TR$mean15$depth <= trPar$surfaceBot["t15"])
    ind2 <- which(TR$mean15$depth <= trPar$surfaceBot["t13"])
    v1[ind1[-length(ind1)], "y"] <- NA
    v2[ind2[-length(ind2)], "y"] <- NA

    op <- grfxtools::Par(oma = c(5, 0, 0.5, 0), mar = c(0, 6, 0, 6),
                         lwd = 2, font.lab = 2, font.axis = 2)

    plot(v1, type = "l", axes = FALSE, xlab = "", ylab = "",
         xlim = c(0, 175), ylim = c(-58, -36))
    lines(TR$mean15[ind1, ], lwd = 1.5, lty = 5, col = "black")

    par(yaxp = c(-52, -36, 4))
    axis(2)
    grfxtools::MinorTick(n = 2, side = 2)
    text(-35, -44, labels = grfxtools::LabelAxis(font = par()$font.lab),
         srt = 90, xpd = NA, cex = par()$cex.lab, col = "black")
    text(17.5, -37, "T15 (2015)",
         cex = par()$cex.lab, font = par()$font.lab)

    par(new = TRUE)

    plot(v2, type = "l", axes = FALSE, xlab = "", ylab = "",
         xlim = c(0, 175), ylim = c(-48, -26), col = "dodgerblue")
    lines(TR$mean13[ind2, ], lwd = 1.5, lty = 5, col = "dodgerblue")

    par(yaxp = c(-48, -40, 2))
    axis(4, col = "dodgerblue", col.axis = "dodgerblue")
    grfxtools::MinorTick(n = 2, side = 4, col = "dodgerblue")
    par(xaxp = c(0, 175, 7))
    axis(1)

    text(210, -44, labels = grfxtools::LabelAxis(font = par()$font.lab),,
         srt = -90, xpd = NA, cex = par()$cex.lab, col = "dodgerblue")
    mtext("Depth (cm)", side = 1, line = 3.5,
          cex = par()$cex.lab, font = par()$font.lab)
    text(175 - 17.5, -40.75, "T13 (2013)",
         cex = par()$cex.lab, font = par()$font.lab, col = "dodgerblue")

    par(op)

  }

  TC17.Fig05 <- function() {

    op <- grfxtools::Par(lwd = 2, font.lab = 2, font.axis = 2)

    palette <- colorRampPalette(rev(RColorBrewer::brewer.pal(10, "RdYlBu")))

    data <- ParamSpace

    # project RMSD data onto surface of optimal advection values

    opt.adv.surface <- apply(data$RMSD, c(2, 3), which.min) %>%
      apply(c(1, 2), function(i) {data$advection[i]})

    RMSD.opt.adv.surface <- apply(data$RMSD, c(2, 3), min)

    # make plot

    filled.contour(data$sigma, data$compression, opt.adv.surface,
                   color.palette = palette, zlim = c(40, 60),
                   plot.title =
                     title(xlab = "Differential diffusion length (cm)",
                           ylab = "Compression (cm)"),
                   plot.axes = {
                     contour(data$sigma,
                             data$compression,
                             RMSD.opt.adv.surface,
                             add = TRUE, labcex = 1);
                     points(2.3, 3.5, pch = 21, col = "black",
                            bg = "black", cex = 1.25);
                     axis(1); axis(2)})
    
    text(8.1, 5, labels = "Optimal downward advection (cm)",
         srt = -90, xpd = NA, cex = par()$cex.lab, font = par()$font.lab)

    par(op)

  }

  TC17.Fig06 <- function() {

    trPar <- tc17.paper.param
    mod.param <- tc17.modif.param

    # color scale
    my.col <- c("dodgerblue", "#1b9e77", "#d95f02", "#7570b3")

    # T13* and T13**
    TR <- makeHiResKohnenTrenches(.var = "d18O", na.rm = TRUE)
    T13.star     <- ModifyRecord(rec.in = TR$mean13_HiRes$y,
                                 res = trPar$hiRes,
                                 depth.hires = TR$mean13_HiRes$depth,
                                 depth.lores = TR$mean15$depth,
                                 SIGMA = mod.param$SIGMA.opt,
                                 STRETCH = mod.param$STRETCH.opt,
                                 ADV = mod.param$ADV.opt)
    T13.starstar <- ModifyRecord(rec.in = TR$mean13_HiRes$y,
                                 res = trPar$hiRes,
                                 depth.hires = TR$mean13_HiRes$depth,
                                 depth.lores = TR$mean15$depth,
                                 SIGMA = mod.param$SIGMA.ind,
                                 STRETCH = mod.param$STRETCH.ind,
                                 ADV = mod.param$ADV.ind)

    #---------------------------------------------------------------------------

    op <- grfxtools::Par(oma = c(5, 0, 0.5, 0), mar = c(0, 6, 0, 6),
                         mfrow = c(1, 2), lwd = 2, font.lab = 2, font.axis = 2)

    #---------------------------------------------------------------------------
    # Fig06-a

    # auxiliary variables
    v11 <- v1 <- TR$mean13$y
    v22 <- v2 <- ModifyRecord(rec.in = TR$mean13_HiRes$y,
                              res = trPar$hiRes,
                              depth.hires = TR$mean13_HiRes$depth,
                              depth.lores = TR$mean13$depth,
                              STRETCH = mod.param$STRETCH.opt)$HiRes
    v33 <- v3 <- ModifyRecord(rec.in = TR$mean13_HiRes$y,
                              res = trPar$hiRes,
                              depth.hires = TR$mean13_HiRes$depth,
                              depth.lores = TR$mean13$depth,
                              SIGMA = mod.param$SIGMA.opt)$LoRes
    v44 <- v4 <- T13.star$LoRes

    p1 <- (p1 <- which(!is.na(v2)))[c(1, length(p1))]
    p2 <- (p2 <- which(!is.na(v3)))[c(1, length(p2))]
    p3 <- (p3 <- which(!is.na(v4)))[c(1, length(p3))]

    # define surface region
    ind1 <- which(TR$mean15$depth <= trPar$surfaceBot["t13"])
    ind2 <- which(TR$mean15_HiRes$depth <= trPar$surfaceBot["t13"])
    ind4 <- which(TR$mean15$depth <= trPar$surfaceBot["t13"] +
                  ceiling(mod.param$ADV.opt))

    # profiles w/o surface region
    v11[ind1[-length(ind1)]] <- NA
    v22[ind2[-length(ind2)]] <- NA
    v33[ind1[-length(ind1)]] <- NA
    v44[ind4[-length(ind4)]] <- NA

    plot(TR$mean13$depth, v1, type = "n", axes = FALSE,
         xlab = "", ylab = "", xlim = c(0, 125), ylim = c(-54, -40))

    lines(TR$mean13$depth, v11, lwd = 2.5, col = my.col[1])
    lines(TR$mean13$depth[ind1], v1[ind1],
          lwd = 1.5, lty = 5, col = my.col[1])

    lines(TR$mean13_HiRes$depth, v22, lwd = 1.5, col = "firebrick")
    lines(TR$mean13_HiRes$depth[ind2], v2[ind2],
          lwd = 1.5, lty = 5, col = "firebrick")
    points(TR$mean13_HiRes$depth[p1], v2[p1],
           col = "firebrick", pch = 25, lwd = 1.5, cex = 0.75)

    lines(TR$mean13$depth, v33, lwd = 1.5, col = "black")
    lines(TR$mean13$depth[ind2], v3[ind2], lwd = 1.5, lty = 5, col = "black")
    points(TR$mean13$depth[p2], v3[p2], col = "black",
           pch = 24, lwd = 1.5, cex = 0.75)

    par(yaxp = c(-48, -40, 4))
    axis(2)

    text(-27.5, -44, labels = grfxtools::LabelAxis(font = par()$font.lab),
         srt = 90, xpd = NA, cex = par()$cex.lab, col = "black")

    legend("topleft", "T13 record", lwd = 2, col = my.col[1],
           lty = 1, cex = 1.1, text.font = 1, bty = "n")
    grfxtools::Legend("topright", c("T13 after 2-yr incremental diffusion",
                                    "T13 after linear compression"),
                      lwd = c(1.5, 1.5), col = c("black", "firebrick"),
                      lty = 1, cex = 1.1, text.font = 1, bty = "n",
                      pch = c(24, 25), end.pch = TRUE, pch.xoff = 0.2,
                      inset = c(0.02, 0), pt.cex = 0.75, pt.lwd = 1.5)

    par(new = TRUE)

    plot(TR$mean13$depth, v1, type = "n", axes = FALSE,
         xlab = "", ylab = "", xlim = c(0, 125), ylim = c(-48, -34))

    lines(TR$mean13$depth, v11, lwd = 2.5, col = my.col[1])
    lines(TR$mean13$depth[ind1], v1[ind1],
          lwd = 1.5, lty = 5, col = my.col[1])

    lines(TR$mean15$depth - mod.param$ADV.opt, v44, col = my.col[2])
    lines(TR$mean15$depth[ind4] - mod.param$ADV.opt,
          v4[ind4], lwd = 1.5, lty = 5, col = my.col[2])
    points(TR$mean15$depth[p3] - mod.param$ADV.opt,
           v4[p3], col = my.col[2], pch = 23, lwd = 1.5, cex = 0.75)

    par(yaxp = c(-48, -40, 4))
    axis(4)

    text(152.5,-44, labels = grfxtools::LabelAxis(font = par()$font.lab),
         srt = -90, xpd = NA, cex = par()$cex.lab, col = "black")

    par(xaxp = c(0, 125, 5))
    axis(1)

    mtext("Depth (cm)", side = 1, line = 3.5,
          cex = par()$cex.lab, font = par()$font.lab)

    grfxtools::Legend("bottomright", "T13* record",
                      lwd = 1.5, col = my.col[2], lty = 1, cex = 1.1,
                      text.font = 1, bty = "n", pch = 23, end.pch = TRUE,
                      pch.xoff = 0.2, inset = c(0.02, 0), pt.cex = 0.75,
                      pt.lwd = 1.5)


    #---------------------------------------------------------------------------
    # Fig06-b

    # auxiliary variables
    v11 <- v1 <- TR$mean15$y
    v22 <- v2 <- T13.star$LoRes
    v33 <- v3 <- T13.starstar$LoRes
    # only optimal advection
    v4 <- ModifyRecord(rec.in = TR$mean13_HiRes$y,
                       res = trPar$hiRes,
                       depth.hires = TR$mean13_HiRes$depth,
                       depth.lores = TR$mean15$depth,
                       ADV = mod.param$ADV.only)$LoRes

    p1 <- (p1 <- which(!is.na(v2)))[c(1, length(p1))]
    p2 <- (p2 <- which(!is.na(v3)))[c(1, length(p2))]

    # define surface region
    ind1 <- which(TR$mean15$depth <= trPar$surfaceBot["t15"])
    ind2 <- which(TR$mean15$depth <= trPar$surfaceBot["t13"] +
                  ceiling(mod.param$ADV.opt))
    ind3 <- ind2 # force consistency

    # profiles w/o surface region
    v11[ind1[-length(ind1)]] <- NA
    v22[ind2[-length(ind2)]] <- NA
    v33[ind3[-length(ind3)]] <- NA

    sum.max <- prxytools::LocatePeaks(v11, partial = TRUE)[3 : 6]

    plot(TR$mean15$depth, v1, type = "n", xlim = c(0, 175), ylim = c(-52, -36),
         axes = FALSE, xlab = "", ylab = "")

    for (i in 1 : length(sum.max))
      segments(x0 = TR$mean15$depth[sum.max[i]], y0 = -52 * 1.04,
               y1 = ifelse(v3[sum.max[i]] > v1[sum.max[i]],
                           v3[sum.max[i]], v1[sum.max[i]]),
               lty = 5, lwd = 1, col = "gray50")
    segments(x0 = 50, y0 = -48, x1 = 182, lty = 1, lwd = 1, col = "gray50")

    lines(TR$mean15$depth, v11)
    lines(TR$mean15$depth[ind1], v1[ind1], lwd = 1.5, lty = 5, col = "black")

    lines(TR$mean15$depth, v33, col = my.col[3])
    lines(TR$mean15$depth[ind3], v3[ind3], lwd = 1.5, lty = 5, col = my.col[3])
    points(TR$mean15$depth[p2], v3[p2], lwd = 1.5, cex = 1, pch = 23,
           col = my.col[3])

    lines(TR$mean15$depth, v22, col = my.col[2])
    lines(TR$mean15$depth[ind2], v2[ind2],
          lwd = 1.5, lty = 5, col = my.col[2])
    points(TR$mean15$depth[p1], v2[p1], lwd = 1.5, cex = 0.75, pch = 1,
           col = my.col[2])

    par(yaxp = c(-52, -36, 4))
    axis(2)
    grfxtools::MinorTick(n = 2, side = 2)
    par(xaxp = c(0, 175, 7))
    axis(1)

    mtext("Depth (cm)", side = 1, line = 3.5,
          cex = par()$cex.lab, font = par()$font.lab)
    mtext(grfxtools::LabelAxis(font = par()$font.lab),
          side = 2, line = 3.5, las = 0, cex = par()$cex.lab)

    grfxtools::Legend("topleft", c("T15", "T13* (opt. param.)",
                                   "T13** (ind. param.)"),
                      lwd = 1.5, lty = 1, pch = c(NA, 1, 23),
                      col = c("black", my.col[2], my.col[3]),
                      cex = 1.1, text.font = 1, bty = "n",
                      end.pch = TRUE, pch.xoff = 0.2,
                      inset = c(0.02, 0), pt.cex = 0.75, pt.lwd = 1.5)

    par(new = TRUE)

    plot(TR$mean15$depth, v1, type = "n",
         xlim = c(0, 175), ylim = c(-4, 12),
         axes = FALSE, xlab = "", ylab = "")

    lines(TR$mean15$depth, v11 - v33, col = my.col[4])
    lines(TR$mean15$depth[ind3], v1[ind3] - v3[ind3],
          lwd = 1.5, lty = 5, col = my.col[4])
    lines(TR$mean15$depth, v1 - v4,
          lwd = 1.5, lty = 3, col = "dimgrey")

    par(yaxp = c(-4, 4, 4))
    axis(4, col = my.col[4], col.axis = my.col[4])

    text(205, 0, grfxtools::LabelAxis("T15 - T13**", font = par()$font.lab),
         cex = par()$cex.lab, col = my.col[4], srt = -90, xpd = NA)

    par(op)

  }

  TC17.Fig07 <- function() {

    trPar <- tc17.paper.param
    mod.param <- tc17.modif.param

    # T13**
    TR <- makeHiResKohnenTrenches(.var = "d18O")
    T13.starstar <- ModifyRecord(rec.in = TR$mean13_HiRes$y,
                                 res = trPar$hiRes,
                                 depth.hires = TR$mean13_HiRes$depth,
                                 depth.lores = TR$mean15$depth,
                                 SIGMA = mod.param$SIGMA.ind,
                                 STRETCH = mod.param$STRETCH.ind,
                                 ADV = mod.param$ADV.ind)

    # profile differences
    diff.13 <- TR$mean13.1$y -
      prxytools::Lag(TR$mean13.2$y, shift = trPar$k13 / trPar$loRes)

    diff.15 <- TR$mean15.1_HiRes$y -
      prxytools::Lag(TR$mean15.2_HiRes$y, shift = trPar$k15 / trPar$hiRes)
    diff.15 <- diff.15[match(TR$mean15$depth, TR$mean15_HiRes$depth)]

    diff.2yr <- TR$mean15$y - T13.starstar$LoRes

    #---------------------------------------------------------------------------

    op <- grfxtools::Par(mfrow = c(1, 2), xaxs = "r", yaxs = "i", lwd = 1.5,
                         mar = c(5, 5.5, 0.5, 0.5), font.lab = 2, font.axis = 2)
    
    #---------------------------------------------------------------------------
    # Fig07-a

    hist(diff.13, breaks = 10, freq = FALSE,
         xlim = c(-4, 4), ylim = c(0, 0.525),
         density = 10, main = "", xlab = "", ylab = "", col = "dodgerblue")
    hist(diff.15, add = TRUE, breaks = 10, freq = FALSE,
         density = 10, angle = -45)

    mtext("Spatial differences between trenches (\u2030)",
          side = 1, line = 3.5,
          cex = par()$cex.lab, font = par()$font.lab)
    mtext(expression(bold("Normalised frequency density (\u2030")
                     ^{"-1"} * bold(")")),
          side = 2, line = 3.75, las = 0,
          cex = par()$cex.lab, font = par()$font.lab)

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
          cex = par()$cex.lab, font = par()$font.lab)
    mtext(expression(bold("Normalised frequency density (\u2030")
                     ^{"-1"} * bold(")")),
          side = 2, line = 3.75, las = 0,
          cex = par()$cex.lab, font = par()$font.lab)

    legend("topleft",
           c(expression(bold("Combined spatial: ") * Delta *
                        bold("T13 and ") * Delta * bold("T15")),
             expression(bold("Temporal: T15-T13**"))),
           lwd = 1.5, lty = 1, col = c("dimgrey", "firebrick"),
           bty = "n", inset = c(0, 0.02))

    par(op)

  }

  # ----------------------------------------------------------------------------
  # produce desired figure

  which.figure <- match.arg(which.figure)

  switch(which.figure,
         f1  = TC17.Fig01(),
         f2  = TC17.Fig02(),
         f3a = TC17.Fig03a(),
         f3b = TC17.Fig03b(),
         f3c = TC17.Fig03c(),
         f4  = TC17.Fig04(),
         f5  = TC17.Fig05(),
         f6  = TC17.Fig06(),
         f7  = TC17.Fig07()
         )

  invisible()

}
