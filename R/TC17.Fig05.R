##" Title.
##"
##" .. content for \description{} (no empty lines) ..
##"
##" .. content for \details{} ..
##" @param ParamSpace
##" @param path 
##" @param file.name 
##" @param device 
##" @param dev.size 
##" @param save.plot
##" @author Thomas Münch
##" @export
TC17.Fig05 <- function(ParamSpace, path = file.path(getwd(), "plots"),
                       file.name = "tc17_fig_05", device = "quartz",
                       dev.size = list(h = 6, w = 8), save.plot = FALSE) {

    plot.par <- SetPlotPar()
    plot.file <- file.path(path, file.name)

    if (device == "quartz") {
        quartzFonts(optima = c("Optima Regular", "Optima Bold",
                               "Optima Italic", "Optima Bold Italic"))
    }

    OpenDevice(device = device, plot.file = plot.file,
               type = "png", height = dev.size$h, width = dev.size$w,
               save.plot = save.plot)
    par(plot.par)
    if (device == "quartz") par(family = "optima")

    palette <- colorRampPalette(rev(RColorBrewer::brewer.pal(10, "RdYlBu")))

    filled.contour(ParamSpace$sigma, ParamSpace$densf, ParamSpace$adv.opt.arr,
                   color.palette = palette, zlim = c(40, 60),
                   plot.title =
                       title(xlab = "Differential diffusion length (cm)",
                             ylab = "Compression (cm)"),
                   plot.axes = {
                       contour(ParamSpace$sigma,
                               ParamSpace$densf,
                               ParamSpace$RMSD.opt,
                               add = TRUE, labcex = 1);
                       points(2.3, 3.5, pch = 21, col = "black",
                              bg = "black", cex = 1.25);
                       axis(1); axis(2)})
    
    text(8.1, 5, labels = "Optimal downward-advection (cm)",
         srt = -90, xpd = NA, cex = plot.par$cex.lab, font = plot.par$font.lab)

    if (save.plot) dev.off()

}

