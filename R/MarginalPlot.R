#####################################################
#          Marginal posterior Density               #
#       NEW version in ArchaeoPhases 1.4            #
#####################################################
#' Plot a marginal posterior density
#'
#' Draws a plot of the estimated marginal posterior density for the one-parameter and adds the mean
#' and the credible interval at the desired level
#'
#' @param a_chain Numeric vector containing the output of the MCMC algorithm
#' for the parameter.
#' @param level Probability corresponding to the level of confidence.
#' @param GridLength Length of the grid used to estimate the density.
#' @param title Title of the graph.
#' @param subtitle Subtitle of the graph.
#' @param caption Caption of the graph.
#' @param x.label Label of the x-axis.
#' @param y.label Label of the y-axis.
#' @param width Plot width in \code{units}.
#' @param height Plot height in \code{units}.
#' @param units String recognized by the \code{ggsave()} function,
#' one of "in", "cm", "mm".
#' @param x.min Minimum x axis value.
#' @param x.max Maximum x axis value.
#' @param x.scale One of "calendar" for calendar years,
#' "BP" for years before present,
#' or "elapsed" for time elapsed from a specified origin.
#' @param elapsed.origin.position Position of the column to use as the
#' origin for elapsed time calculations.
#' @param y.grid Switch for horizontal grid lines.
#' @param file Name of the file that will be saved if chosen, default = \code{NULL}.
#' @param newWindow Whether or not the plot is drawn within a new window.
#'
#' @return \code{NULL}, called for its side effects
#'
#' @details The density is estimated using \code{density()} function with \code{n = GridLength}.
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr} and
#'
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @examples
#'   data(Events);
#'   MarginalPlot(a_chain = Events$Event.1, level = 0.95)
#'
#' @importFrom stats density
#' @importFrom grDevices dev.new
#'
#' @export
#'
MarginalPlot <- function(a_chain, level=0.95, GridLength=1024,
                  title="Characteristics of a date", subtitle = NULL,caption = "ArchaeoPhases",
                  x.label = "Calendar year",y.label = NULL,y.grid = TRUE,
                  x.scale = "calendar", elapsed.origin.position = NULL,x.min = NULL, x.max = NULL,
                  height = 7, width = 7, units = "in",file = NULL, newWindow=TRUE){

  # x.scale can either be "calendar", "BP" or "elapsed" if any other origin that 0 and 1950
  if (x.scale == "BP") {
    a_chain <- 1950-a_chain
  }
  if (x.scale == "elapsed") {
    a_chain <- elapsed.origin.position-a_chain
  }
  # credible interval
  CR <- CredibleInterval(a_chain, level=level, roundingOfValue=4)
  CR <- c(CR,"y"=0,"yend"=0)
  # Mean
  Mean = mean(a_chain)
  # new dataframe
  data = data.frame("a_chain" = as.vector(a_chain))
  dataCR = data.frame("Inf" = CR[2], "Sup" = CR[3], "y"=0,"yend"=0, "Mean"=Mean)

  h <- ggplot2::ggplot(data = data, ggplot2::aes(x=a_chain))
  h <- h + ggplot2::geom_density(n = GridLength)
  h <- h + ggplot2::geom_segment(data = dataCR, ggplot2::aes(x=dataCR[1,1], xend=dataCR[1,2], y = dataCR[1,3], yend = dataCR[1,4], colour="steelblue"), size=3.5, show.legend=F)
  h <- h + ggplot2::geom_point(data = dataCR, ggplot2::aes(x=Mean, y = dataCR[1,3]), size = 2)
  #h <- h + ggplot2::scale_color_manual(values =c('#56B4E9', '#FC4EO7'))
  h <- h + ggplot2::labs(x = x.label, y = y.label, title = title, subtitle = subtitle, caption = caption)

  if (y.grid==FALSE) {
    h <- h + ggplot2::theme(panel.grid.major.y=ggplot2::element_blank())
  }
  # x abscisses
  if (is.null(x.min) ) {
    x.min <- min(density(a_chain, n=GridLength)$x)
  }
  if (is.null(x.max)) {
    x.max <- max(density(a_chain, n=GridLength)$x)
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

#' Plot a marginal posterior density
#'
#' Draws a plot of the marginal posterior density for a single parameter, with
#' an option to add the mean and the credible interval at the desired level
#'
#' @details
#' The plot is drawn with the current theme and color scales; the function
#' does not alter or override theme elements.
#'
#' @param data Data frame containing the output of the MCMC algorithm.
#' @param position Index of the column corresponding to the MCMC chain of
#' interest, or a column name.
#' @param level Probability corresponding to the level of confidence.
#' @param grid_length Length of the grid used to estimate the density.
#' @param title Title of the graph.  The default uses the \code{data}
#' column name.
#' @param subtitle Subtitle of the graph.  The default is
#' "Marginal posterior density".
#' @param caption Caption of the graph.  The default describes the
#' confidence of the credible interval.
#' @param x_label Label of the x-axis.
#' @param y_label Label of the y-axis.
#' @param width Plot width in \code{units}.
#' @param height Plot height in \code{units}.
#' @param units String recognized by the \code{ggsave()} function,
#' one of "in", "cm", "mm".  This parameter has no effect on the
#' display plot.
#' @param x_min Minimum x axis value.
#' @param x_max Maximum x axis value.
#' @param x_scale One of "calendar" for calendar years,
#' "BP" for years before present,
#' or "elapsed" for time elapsed from a specified origin.
#' @param elapsed_origin_position Position of the column to use as the
#' origin for elapsed time calculations.
#' @param y_grid Switch for horizontal grid lines.
#' @param file Name of the file that will be saved if chosen,
#' default = \code{NULL}.
#' @param new_window Whether or not the plot is drawn within a new window.
#' @param plot_result If \code{TRUE}, then draw a plot on the display,
#' else suppress drawing.
#' @param shade Switch for shading the area under the density within the
#' credible interval.
#' @param mean_linetype The \code{linetype} used to indicate the mean density.
#' @param mean_color The color of the line used to indicate mean density.
#' @param mean_size The width of the line used to indicate the mean density.
#' @param ci_linetype The \code{linetype} used to indicate the credible
#' intervals.
#' @param ci_color The color of the lines used to indicate the credible
#' intervals.
#' @param ci_size The width of the lines used to indicate the credible
#' intervals.
#' @param line_linetype The \code{linetype} used to indicate the density.
#' @param line_color The color of the line used to indicate the density.
#' @param line_size The width of the line used to indicate the density.
#'
#' @return An \code{archaeophases_plot} object with the data and metadata
#' needed to reproduce the plot.
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr};
#' @author Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}; and
#' @author Thomas S. Dye, \email{tsd@tsdye.online}
#'
#' @examples
#'   data(Events)
#'   mp <- marginal_plot(data = Events, position = 2, level = 0.95)
#'   ## View data and metadata
#'   str(mp)
#'
#' @importFrom stats density
#' @importFrom grDevices dev.new
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_segment geom_line labs theme xlim ggsave
#'
#' @export
#'
marginal_plot <- function(data,
                          position = 1,
                          level = 0.95,
                          grid_length = 1024,
                          title = if (is.numeric(position)) names(data)[position] else position,
                          subtitle = "Marginal posterior density",
                          caption = paste(level * 100, "% credible interval", sep=""),
                          x_label = "Calendar year",
                          y_label = "Density",
                          y_grid = TRUE,
                          x_scale = "calendar",
                          elapsed_origin_position = NULL,
                          x_min = NULL, x_max = NULL,
                          height = 7, width = 7,
                          units = "in",
                          file = NULL,
                          new_window = TRUE,
                          plot_result = TRUE,
                          ## shade = FALSE,
                          mean_linetype = "dashed",
                          mean_color = "white",
                          mean_size = 0.5,
                          ci_linetype = "dotted",
                          ci_color = mean_color,
                          ci_size = mean_size,
                          line_linetype = "solid",
                          line_color = "black",
                          line_size = 1,
                          density_color = "gray30",
                          fill_palette = NULL)
{

    if (!is.data.frame(data)) stop("Data format not recognized.")

    if (length(position) > 1) stop("Expected one marginal posterior.")

    if (is.element("archaeophases_plot", class(data))) {
        a_chain <- data
    }
    else {
        a_chain <- data.frame(x = as.vector(data[, position]))

        if (x_scale == "BP") {
            a_chain$x <- 1950 - a_chain$x
        }

        if (x_scale == "elapsed") {
            a_chain$x <- data[, elapsed_origin_position] - a_chain$x
        }
    }

    ## credible interval
    c_i <- credible_interval(a_chain$x, level = level, round_to = 4)$ci

    ## mean
    chain_mean = mean(a_chain$x)

    chain_density <- density(a_chain$x, n = grid_length) %$%
        data.frame(x = x, y = y) %>%
        mutate(mid = (x > c_i["inf"] & x < c_i["sup"]))

    dens_mean <- approx(chain_density, xout = chain_mean)
    dens_low <- approx(chain_density, xout = c_i["inf"])
    dens_high <- approx(chain_density, xout = c_i["sup"])

    if (!is.null(fill_palette)) {
        fill_color <- unname(fill_palette[density_color])
    }
    else {
        fill_color <- density_color
    }

    h <- ggplot(data = a_chain,
                mapping = aes(x = x))

    h <- h + geom_density(mapping = aes(y = ..density..),
                          color = line_color,
                          linetype = line_linetype,
                          size = line_size,
                          fill = fill_color,
                          n = grid_length)

    ## if (shade == TRUE) {
    ##     h <- h + geom_ribbon(data = chain_density[chain_density$mid,],
    ##                          show.legend = FALSE,
    ##                          fill = shade_color)
    ## }

    h <- h + geom_segment(mapping = aes(x = c_i$inf,
                                        xend = c_i$inf,
                                        y = 0,
                                        yend = dens_low$y),
                          linetype = ci_linetype,
                          color = ci_color,
                          size = ci_size)

    h <- h + geom_segment(mapping = aes(x = c_i$sup,
                                        xend = c_i$sup,
                                        y = 0,
                                        yend = dens_high$y),
                          linetype = ci_linetype,
                          color = ci_color,
                          size = ci_size)

    ## h <- h + geom_line(mapping = aes(y = y),
    ##                    linetype = line_linetype,
    ##                    color = line_color,
    ##                    size = line_size)

    h <- h + geom_segment(mapping = aes(x = chain_mean,
                                        xend = chain_mean,
                                        y = 0,
                                        yend = dens_mean$y),
                          linetype = mean_linetype,
                          color = mean_color,
                          size = mean_size)

    h <- h + labs(x = x_label, y = y_label,
                  title = title, subtitle = subtitle,
                  caption = caption)

    if (y_grid==FALSE) {
        h <- h + theme(panel.grid.major.y = ggplot2::element_blank())
    }

    ## x abscisses
    if (is.null(x_min) ) {
        x_min <- min(density(a_chain$x, n = grid_length)$x)
    }

    if (is.null(x_max)) {
        x_max <- max(density(a_chain$x, n = grid_length)$x)
    }

    h <- h + xlim(x_min, x_max)

    ## export file
    if (!is.null(file)) {
        ggsave(filename = file, plot = h, height = height,
               width = width, units = units)
    }

    if (plot_result == TRUE) {
        if(new_window == TRUE) {
            dev.new(height = height, width = width)
        }
        print(h)
    }

    new_archaeophases_plot(x = a_chain,
                           mcmc = data,
                           call = match.call())

}
