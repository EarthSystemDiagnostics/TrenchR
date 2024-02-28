#' Trench 2D image
#'
#' Plot a two-dimensional (vertical x horizontal) representation of a certain
#' variable for a given trench data set, either as a
#' \code{\link{filled.contour}} plot or as an \code{\link[fieds]{imagePlot}}.
#'
#' Some care has been taken to make this function as general as possible,
#' however, many aspects are hard to generalise, or would require even more
#' function parameters. Especially tricky is the positioning of the axes and
#' color bar labels; since these are placed using \code{mtext} or \code{text}
#' calls, the resulting positions of the labels heavily depend on the plot
#' dimensions and especially on the plot and outer margin settings. The default
#' settings will therefore likely result in incorrect positioning in many
#' cases. In these cases, fine tuning of the position can be achieved using the
#' \code{hadj} and \code{vadj} parameters for the colour bar legend and the
#' \code{line.h} and \code{line.v} parameters for the horizontal and vertical
#' axes labels, respectively. If needed, more (graphical) settings could be
#' passed on to the actual plotting functions via \code{...}.
#'
#' @param data a trench data set following the default structure used in the
#'   package.
#' @param .var character string with the name of the variable to be plotted,
#'   defaults to "d18O"; must match one of the data column variables in
#'   \code{data}. See also more options as explained in \code{\link{make2D}}.
#' @param vscale a character string giving the name of the vertical scale
#'   variable used in the trench data; defaults to \code{"depth"}; see also
#'   \code{\link{getZ}}.
#' @param rescale.h numeric factor which can be used to rescale the horizontal
#'   scale to a different physical unit, e.g. from cm to m.
#' @param rescale.v same as \code{rescale.x} for the vertical scale.
#' @param xlim the x limits of the plot (horizontal direction).
#' @param ylim the y limits of the plot (vertical direction).
#' @param zlim the limits of the plotted data; this is used as limits for the
#'   colour legend and thus the data range is constrained to these maximum and
#'   minimum values to improve visual appearance. If not specified directly
#'   (\code{zlim = NULL}), the limits are calculated from the data quantile
#'   range as determined from the probabilities specified by \code{qProbs}.
#' @param qProbs numerical vector of length 2 giving the lower and upper
#'   quantiles which are used to calculate \code{zlim} data limits if these are
#'   not specified directly.
#' @param xlab x (horizontal) axis label.
#' @param ylab y (vertical) axis label.
#' @param label label for the data, used to annotate the colour legend.
#' @param main optional character string to be placed above the plot.
#' @param palette a character vector of colour codes for image plots, or a
#'   colour palette function to be used to assign colours for filled contour
#'   plots. The default is to calculate the colour palette internally from ten
#'   colours of the diverging \code{RdYlBu} palette in the ColorBrewer 2.0
#'   collection.
#' @param filledContour logical; shall a \code{\link{filled.contour}} plot be
#'   created? Default \code{FALSE} plots an \code{\link[fields]{imagePlot}}.
#' @param log logical; if \code{TRUE} the data is displayed on a log10 scale.
#' @param horizontal logical; only for image plots. For \code{TRUE}, the colour
#'   bar is plotted horizontally below the x axis of the plot, else it is
#'   plotted vertically to the right of the plot.
#' @param fill logical; if NA values are present at the "top" side of the trench
#'   data, for each profile the last NA bin is filled with the value from the
#'   bin below if \code{fill} is set to \code{TRUE}. This is particularly
#'   useful to improve the visual appearance of filled contour plots when the
#'   trench has an undulating surface.
#' @param hadj adjustment of the colour bar legend in horizontal plot direction
#'   relative to the default positioning.
#' @param vadj same as \code{hadj} for adjustment in the vertical direction.
#' @param line.h margin line where to position the label for the horizontal
#'   plot axis.
#' @param line.v same as \code{line.h} for the vertical plot axis.
#' @param ... further parameters passed on to \code{\link{filled.contour}} or to
#'   \code{\link[fields]{imagePlot}}.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @author Thomas Münch
#' @seealso \code{\link{filled.contour}}, \code{\link[fields]{imagePlot}},
#'   \code{\link[grfxtools]{ColorPal}}
#' @export
#'
plot2D <- function(data, .var = "d18O", vscale = "depth",
                   rescale.h = 1, rescale.v = 1,
                   xlim, ylim, zlim = NULL, qProbs = c(0.01, 0.99),
                   xlab = "Trench position (m)", ylab = "Depth (m)",
                   label = .var, main = "", palette = NULL,
                   filledContour = FALSE, log = FALSE, horizontal = FALSE,
                   fill = FALSE, hadj = 0, vadj = 0, line.h = 3, line.v = 3.5,
                   ...) {

  is.trench(data)

  if (!length(palette)) {
    if (filledContour) {
      palette <- grfxtools::ColorPal("RdYlBu", 10, rev = TRUE, fun = TRUE)
    } else {
      palette <- grfxtools::ColorPal("RdYlBu", 10, rev = TRUE)
    }
  }

  surfaceProfile <- getSurfaceProfile(data) %>%
    dplyr::mutate(position = rescale.h * .data$position,
                  height = rescale.v * .data$height)

  x <- surfaceProfile$position
  y <- rescale.v * getZ(data, vscale = vscale)
  z <- make2D(data, .var, simplify = TRUE)

  if (!length(zlim)) zlim <- unname(quantile(z, probs = qProbs, na.rm = TRUE))

  z[z < zlim[1]] <- zlim[1]
  z[z > zlim[2]] <- zlim[2]

  if (fill) {
    # fill part of surface layer with first non-NA value/profile
    # to improve plot appearance
    firstNonNA <- apply(z, 2, function(x) {which(!is.na(x))[1]})
    for (i in 1 : ncol(z)) {
      z[firstNonNA[i] - 1, i] <- z[firstNonNA[i], i]
    }
  }

  if (log) {
    z <- log10(z)
    zlim <- log10(zlim)
  }

  # transpose for plotting function
  z <- t(z)

  if (filledContour) {

    if (!is.function(palette)) {
      stop("Argument 'palette' must be a function that can be used to assign ",
           "colours in a filled contour plot.")
    }

    graphics::filled.contour(
      x, y, z, color.palette = palette, xlim = xlim, ylim = ylim, zlim = zlim,
      plot.title = {
        grid(col = "black", nx = NA, ny = NULL);
        title(main = main);
        mtext(xlab, side = 1, line = line.h,
              cex = par()$cex.lab, font = par()$font.lab);
        mtext(ylab, side = 2, line = line.v,
              cex = par()$cex.lab, font = par()$font.lab, las = 0)
        lines(surfaceProfile, lwd = 2)},
      ...
    )

  } else {

    if (is.function(palette)) {
      stop("Argument 'palette' must be a vector of colours for image plots.")
    }

    fields::imagePlot(
      x, y, z, legend.shrink = 1, horizontal = horizontal, col = palette,
      xlim = xlim, ylim = ylim, zlim = zlim, xlab = "", ylab = "", main = main,
      ...)
    lines(surfaceProfile, lwd = 2)
    grid(col = "black", nx = NA, ny = NULL)
    mtext(xlab, side = 1, line = line.h,
          cex = par()$cex.lab, font = par()$font.lab)
    mtext(ylab, side = 2, line = line.v,
          cex = par()$cex.lab, font = par()$font.lab, las = 0)
    box()

  }

  # placement of colour legend label

  if (!filledContour & horizontal) {

    # horizontal available only for image plot
    xpos <- 0.50 + hadj
    ypos <- -0.4 + vadj
    srt  <- 0

  } else {

    # vertical
    xpos <- 1.03 + hadj
    ypos <- 0.50 + vadj
    srt  <- -90

  }

  op.usr <- par(usr = c(0, 1, 0, 1), xlog = FALSE, ylog = FALSE)
  text(xpos, ypos, labels = label, srt = srt, xpd = NA,
       cex = par()$cex.lab, font = par()$font.lab)
  par(op.usr)

}
