#####################################################
#       Multi Marginal posterior Density            #
#       NEW version in ArchaeoPhases 1.4            #
#####################################################
#' Marginal posterior densities of several events
#'
#' Draws a plot of the estimated marginal posterior density for a parameter
#' and adds the mean and the credible interval at the desired level
#'
#' @param data Data frame containing the output of the MCMC algorithm.
#' @param position Numeric vector containing the position of the column
#' corresponding to the MCMC chains of interest.
#' @param level Probability corresponding to the level of confidence.
#' @param GridLength Number of equally spaced points at which the
#' density is to be estimated (for \code{density()} function).
#' @param x.scale One of "calendar" for calendar years,
#' "BP" for years before present,
#' or "elapsed" for time elapsed from a specified origin.
#' @param elapsed.origin Position of the column to use
#' as the origin for elapsed time calculations.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param caption Caption of the plot.
#' @param legend.title Title for the legend.
#' @param x.label Label of the x-axis.
#' @param y.label Label of the y-axis.
#' @param width Plot width in \code{units}.
#' @param height Plot height in \code{units}.
#' @param units String recognized by the \code{ggsave()} function,
#' one of "in", "cm", "mm".
#' @param x.min Minimum x-axis value.
#' @param x.max Maximum x-axis value.
#' @param y.grid Switch for horizontal grid lines.
#' @param file Name of the file that will be saved if specified,
#' default = \code{NULL}.
#' @param newWindow Whether or not the plot is drawn within a new window.
#'
#' @details
#' The density is estimated using \code{density()} function with
#' n = \code{GridLength.} The input MCMC chains should either be in
#' calendar years or converted to calendar years using \code{x.scale} vector or
#' \code{elapsed.origin}.
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr} and
#'
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @return \code{NULL}, called for its side effects
#'
#' @examples
#'   data(Events);
#'   MultiMarginalPlot(Events, position = c(2, 3, 4), level = 0.95)
#'
#' @importFrom stats density
#' @importFrom grDevices dev.new
#'
#' @export
MultiMarginalPlot <- function(data, position, level=0.95, GridLength = 1024,
                              x.scale = rep("calendar", length(position)), elapsed.origin = NULL,
                              title="Characteristics of several dates", subtitle = NULL,caption = "ArchaeoPhases",
                              x.label = "Calendar year",y.label = NULL,y.grid = TRUE,x.min = NULL, x.max = NULL,
                              legend.title = "Legend", height = 7, width = 7, units = "in",file = NULL, newWindow=TRUE){

  if ((length(position) <2)==TRUE) stop("Vector of position should be of length 2 at least")

  Newdata = data[ ,position]
  names <- names(data)[position]

  # x.scale can either be "calendar", "BP" or "elapsed" if any other origin that 0 and 1950
  for (i in 1:length(position)){
    if (x.scale[i] == "BP") {
      Newdata[i] <- 1950-Newdata[i]
    }else if (x.scale[i] == "elapsed") {
      Newdata[i] <- elapsed.origin - Newdata[i]
    }
  }

  colnames(Newdata) <- names
  Newdata <- cbind(Newdata, iter=1:dim(Newdata)[1])
  Newdata = as.data.frame(Newdata)
  Newdata_long <- reshape2::melt(Newdata, id="iter")

  ###   Defining the abscisse axis    ####
  L = length(position)
  densityX = matrix(ncol = L, nrow=GridLength)
  for (i in 1:L) { densityX[,i] = density(Newdata[,i], n=GridLength)$x }

  # x abscisses
  x = 10^c(0:10)
  if (is.null(x.min)) { # min
    x.min <- minValuex <- min(apply(densityX,2,min) )
    c =0
    for(i in 1:length(x)) { if( abs(minValuex/x[i])>1) {c=c+1}}
    if(c>3){ minValuex = floor(minValuex/x[c-1])*x[c-1]} else {minValuex = floor(minValuex/x[c])*x[c]}
  }

  if (is.null(x.max)) { # max
    x.max <- maxValuex <- max(apply(densityX,2,max))
    if(maxValuex!=0){
      d=0
      for(i in 1:length(x)) { if( abs(maxValuex/x[i])>1) {d=d+1}}
      if(d>3){ maxValuex = ceiling( maxValuex/x[d-1])*x[d-1] } else { maxValuex = ceiling(maxValuex/x[d])*x[d] }
    }
  }

  h <- ggplot2::ggplot(data = Newdata_long, ggplot2::aes(x=Newdata_long$value, colour = Newdata_long$variable))
  h <- h + ggplot2::geom_density(n = GridLength, data = Newdata_long)
  h <- h + ggplot2::labs(x = x.label, y = y.label, title = title, subtitle = subtitle, caption = caption)
  h <- h + ggplot2::guides(colour = ggplot2::guide_legend(title = legend.title))

  if (y.grid==FALSE) {
    h <- h + ggplot2::theme(panel.grid.major.y=ggplot2::element_blank())
  }
  h <- h + ggplot2::xlim(x.min, x.max)

  # export file
  if (!is.null(file)) {
    ggplot2::ggsave(filename = file, plot = h, height = height,width = width, units = units)
  }
  if(newWindow == TRUE) {
    dev.new(height = height, width = width)
  }
  print(h)

}

#' Marginal posterior densities of several events
#'
#' Draws a plot of the estimated marginal posterior density for a parameter
#' and adds the mean and the credible interval at the desired level
#'
#' @param data Data frame containing the output of the MCMC algorithm.
#' @param position Numeric vector containing the position of the column
#' corresponding to the MCMC chains of interest, or a vector of column names.
#' @param level Probability corresponding to the level of confidence.
#' @param grid_length Number of equally spaced points at which the
#' density is to be estimated (for \code{density()} function).
#' @param x_scale One of "calendar" for calendar years,
#' "BP" for years before present,
#' or "elapsed" for time elapsed from a specified origin.
#' @param elapsed_origin_position Position of the column to use
#' as the origin for elapsed time calculations.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param caption Caption of the plot.
#' @param x_label Label of the x-axis.
#' @param y_label Label of the y-axis.
#' @param width Plot width in \code{units}.
#' @param height Plot height in \code{units}.
#' @param units String recognized by the \code{ggsave()} function,
#' one of "in", "cm", "mm".
#' @param x_min Minimum x-axis value.
#' @param x_max Maximum x-axis value.
#' @param y_grid Switch for horizontal grid lines.
#' @param file Name of the file that will be saved if specified,
#' default = \code{NULL}.
#' @param new_window Whether or not the plot is drawn within a new window.
#' @param plot_result If \code{TRUE}, then draw a plot on the display,
#' else suppress drawing.
#' @param density_fill A color specification for the fill under the
#' density line.
#' @param density_color A color specification for the density line.
#' @param density_alpha A number between 0 for transparent and 1
#' for opaque.
#' @param mean_color A color specification for the mean line.
#' @param mean_linetype A line type specification for the mean line.
#' @param mean_size A size specification for the mean line.
#' @param ci_color A color specification for the credible interval lines.
#' @param ci_linetype A line type specification for the credible interval
#' lines.
#' @param ci_size A size specification of the credible interval lines.
#' @param fill_palette A vector of colors for qualitative data.
#' @param colors A vector of indices into palette keyed by position.
#' @param color_legend_name A label for the legend.
#'
#' @details
#' The density is estimated using \code{density()} function with
#' n = \code{grid_length.} The input MCMC chains should either be in
#' calendar years or converted to calendar years using \code{x_scale} vector or
#' \code{elapsed_origin_position}.
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr};
#' @author Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}; and
#' @author Thomas S. Dye, \email{tsd@tsdye.online}
#'
#' @return An \code{archaeophases_plot} object with the data and metadata
#' needed to reproduce the plot.
#'
#' @examples
#'   data(Events);
#'   multi_marginal_plot(Events, position = c(2, 3, 4), level = 0.95)
#'
#' @importFrom stats density
#' @importFrom grDevices dev.new
#' @importFrom ggplot2 ggplot guides theme xlim ggsave element_blank
#' geom_density geom_segment facet_grid scale_fill_manual
#' @importFrom reshape2 melt
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarize mutate if_else
#' @importFrom gplots col2hex
#'
#' @export
multi_marginal_plot <- function(data,
                                position = 1:ncol(data),
                                level = 0.95,
                                grid_length = 1024,
                                x_scale = "calendar",
                                elapsed_origin_position = NULL,
                                title="Characteristics of several dates",
                                subtitle = "Marginal densities",
                                caption = paste(level * 100,
                                                "% credible interval", sep=""),
                                x_label = "Calendar year",
                                y_label = NULL,
                                density_fill = "gray30",
                                density_color = "black",
                                density_alpha = 1,
                                mean_color = "white",
                                mean_linetype = "dashed",
                                mean_size = 0.5,
                                ci_color = mean_color,
                                ci_linetype = "dotted",
                                ci_size = mean_size,
                                y_grid = TRUE,
                                x_min = NULL,
                                x_max = NULL,
                                height = 7,
                                width = 7,
                                units = "in",
                                file = NULL,
                                new_window = TRUE,
                                plot_result = TRUE,
                                fill_palette = NULL,
                                colors = NULL,
                                color_legend_name = "Legend")
{

    if (!is.data.frame(data))
        stop("Data format not recognized.")

    if (is.element("archaeophases_plot", class(data))) {
        new_data <- data
    }
    else {
        if ((length(position) < 2) == TRUE)
            stop("Length position vector must be 2 or more.")

        new_data = data[, position]

        if (x_scale == "BP") {
            new_data <- 1950 - new_data
        }

        if (x_scale == "elapsed") {
            new_data <- data[, elapsed_origin_position] - new_data
        }
    }

    melted_data <- melt(new_data, id.vars = NULL)

    if (!is.null(colors)){
        data_names <- names(data[, position])
        melted_data <- melted_data %>%
            mutate(dens_fill = colors[match(variable, data_names)])
    }
    else {
        melted_data <- melted_data %>%
            mutate(dens_fill = density_fill)
    }

    summary <- melted_data %>%
        group_by(variable) %>%
        summarize(xbar = round(mean(value, na.rm = TRUE), 0),
                  dens_xbar = approx(density(value, n = grid_length),
                                     xout = xbar)$y,
                  c_i_low = unname(credible_interval(value, level = level)$ci["inf"]),
                  c_i_high = unname(credible_interval(value, level = level)$ci["sup"]),
                  dens_low = approx(density(value, n = grid_length),
                                    xout = c_i_low)$y,
                  dens_high = approx(density(value, n = grid_length),
                                     xout = c_i_high)$y)

    h <- ggplot(data = melted_data, mapping = aes(x = value))

    h <- h + geom_density(color = density_color,
                          alpha = density_alpha,
                          mapping = aes(fill = dens_fill, y = ..density..),
                          n = grid_length)

    h <- h + geom_segment(data = summary,
                          mapping = aes(x = xbar, xend = xbar,
                                        y = 0, yend = dens_xbar),
                          color = mean_color,
                          linetype = mean_linetype,
                          size = mean_size)

    h <- h + geom_segment(data = summary,
                          mapping = aes(x = c_i_low, xend = c_i_low,
                                        y = 0, yend = dens_low),
                          color = ci_color,
                          linetype = ci_linetype,
                          size = ci_size)

    h <- h + geom_segment(data = summary,
                          mapping = aes(x = c_i_high, xend = c_i_high,
                                        y = 0, yend = dens_high),
                          color = ci_color,
                          linetype = ci_linetype,
                          size = ci_size)

    h <- h + labs(x = x_label,
                  y = y_label,
                  title = title,
                  subtitle = subtitle,
                  caption = caption)

    if (!is.null(fill_palette)) {
        if (is.null(colors)) {
            h + scale_fill_manual(values = unname(fill_palette[density_fill]))
        }
        else {
        h <- h + scale_fill_manual(values = unname(fill_palette),
                                   name = color_legend_name)
        }
    }
    else {
        h <- h + scale_fill_manual(values = col2hex(density_fill))
    }

    if (is.null(colors)) {
        h <- h + guides(fill = FALSE)
    }

    h <- h + facet_grid(variable ~ ., switch = "y")

    if (y_grid == FALSE) {
        h <- h + theme(panel.grid.major.y = element_blank())
    }

    if (!is.null(file)) {
        ggsave(filename = file, plot = h, height = height,
               width = width, units = units)
    }

    if (plot_result == TRUE) {
        if (new_window == TRUE) {
            dev.new(height = height, width = width)
        }
        print(h)
    }

    new_archaeophases_plot(x = new_data,
                           mcmc = data,
                           call = match.call())
}
