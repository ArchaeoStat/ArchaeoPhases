#' Make a single plot of a Nökel lattice.
#'
#' Plots a Nökel lattice to the display and optionally to a file.
#'
#' @param allen_set a dataframe with plot information, such as the one
#' produced by \code{illustrate_allen_relations()}
#' @param file_name optional path to the graphic file output
#' @param pad padding in inches to the margins to keep
#' labels from disappearing off the edge of the graphic
#' @param font_size font size for the labels in the plot
#' @param height height in inches of the graphic file output
#' @param width width in inches of the graphic file output
#' @param plot_title title for the plot, defaults to the title in \code{allen_set}
#' @param columns number of columns for a plot with more than one lattice
#' @param dpi dots per inch for bitmap files
#'
#' @return typically called for its side effects, returns \code{allen_set}
#'
#' @author Thomas S. Dye
#'
#' @importFrom graphics title
#' @importFrom ggplot2 .pt aes facet_wrap ggsave ggtitle labs vars xlim ylim
#' @importFrom ggplot2 theme
#' @importFrom ggraph ggraph th_no_axes geom_node_text
#' @importFrom grDevices bmp cairo_pdf cairo_ps jpeg png svg tiff
#'
#' @export
allen_plot <- function(allen_set, file_name = NULL, pad = 0.2, font_size = 11,
                       height = 7, width = 7, columns = 1, plot_title = allen_set$title, dpi = 600) {
    if (!(is.element("allen_set_illustrative", class(allen_set))
        || is.element("allen_set_empirical", class(allen_set))))
        stop("Unrecognized data for N\u00F6kel lattice.")
    g <- ggraph(graph = allen_set, layout = "nicely")
    g <- g + th_no_axes()
    g <- g + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                            panel.grid.minor = ggplot2::element_blank())
    g <- g + xlim(min(allen_set$x) - pad,
                  max(allen_set$x) + pad)
    g <- g + ylim(min(allen_set$y) - pad,
                  max(allen_set$y) + pad)
    if(dim(allen_set)[1] == 13)
        g <- g + ggtitle(plot_title)
    else
        g <- g + facet_wrap(vars(title), ncol = columns)
    ## g <- g + scico::scale_colour_scico(palette = 'grayC')
    g <- g + geom_node_text(mapping = aes(label = node,
                                                  alpha = result),
                                    data = allen_set,
                                    size = font_size/ggplot2::.pt
                                    )
    if (is.element("allen_set_illustrative", class(allen_set))) {
        g <- g + theme(legend.position = "none")
    } else {
        g <- g + labs(alpha = "Posterior\nprobability")
    }
    if (!is.null(file_name)) {
        ext <- tools::file_ext(file_name)
        switch(ext,
               pdf = cairo_pdf(filename = file_name,
                               height = height,
                               width = width,
                               pointsize = font_size,
                               onefile = TRUE,
                               fallback_resolution = 600),
               eps =, ps = cairo_ps(filename = file_name,
                                    height = height,
                                    width = width,
                                    pointsize = font_size,
                                    onefile = TRUE,
                                    fallback_resolution = 600),
               svg = svg(filename = file_name,
                         height = height,
                         width = width,
                         pointsize = font_size,
                         onefile = TRUE),
               png = png(filename = file_name,
                         height = height,
                         width = width,
                         units = "in",
                         pointsize = font_size,
                         res = dpi),
               bmp = bmp(filename = file_name,
                         height = height,
                         width = width,
                         units = "in",
                         pointsize = font_size,
                         res = dpi),
               jpg =, jpeg = jpeg(filename = file_name,
                                height = height,
                                width = width,
                                units = "in",
                                pointsize = font_size,
                                res = dpi),
               tif =, tiff = tiff(filename = file_name,
                                height = height,
                                width = width,
                                units = "in",
                                pointsize = font_size,
                                res = dpi,
                                compression ="lzw"),
               stop("unrecognized file type"))
        print(g)
        grDevices::dev.off()
    }
    print(g)
    allen_set
}
