
####################################
###   Tempo plot   NEW Version 2017/08   ###

#' Tempo plot
#'
#' A statistical graphic designed for the archaeological study of
#' rhythms of the long term that embodies a theory of archaeological
#' evidence for the occurrence of events
#'
#' @param data Data frame containing the output of the MCMC algorithm.
#' @param position Numeric vector containing the position of the column
#' corresponding to the MCMC chains of interest.
#' @param plot.result List containing the data to plot,
#' typically the result of a previous run of \code{TempoPlot()}.
#' @param level Probability corresponding to the level of confidence.
#' @param count If \code{TRUE} the counting process is a number,
#' otherwise it is a probability.
#' @param Gauss If \code{TRUE}, the Gaussian approximation of the
#' credible interval is used.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param caption Caption of the plot.
#' @param legend.title Title of the plot legend.
#' @param legend.labels Vector of strings to label legend entries.
#' @param x.label Label of the x-axis.
#' @param y.label Label of the y-axis.
#' @param line.types Type of the lines drawn on the plot in the order
#' of \code{legend.labels}.
#' @param width Width of the plot in \code{units}.
#' @param height Height of the plot in \code{units}.
#' @param units Units used to specify \code{width} and \code{height},
#' one of "in" (default), "cm", or "mm".
#' @param x.min Minimum value for x-axis.
#' @param x.max Maximum value for x-axis.
#' @param colors If \code{TRUE}, the plot is drawn with colors,
#' otherwise it is drawn in black and white.
#' @param file Name of the file that will be saved if specified.
#' If \code{NULL} no file is saved.
#' @param x.scale One of "calendar", "bp", or "elapsed".
#' @param elapsed.origin.position If \code{x.scale} is "elapsed", the position
#' of the column corresponding to the event from which elapsed time is
#' calculated.
#' @param newWindow Whether or not the plot is drawn within a new window.
#' @param print.data.result If \code{TRUE}, a list containing the data to plot
#' will be returned.
#'
#' @return \code{NULL}, called for its side effects.  It may also return a list containing the
#' data to plot (if \code{print.data.result = TRUE}).
#'
#' @details
#' The tempo plot is one way to measure change over time: it estimates the cumulative occurrence
#' of archaeological events in a Bayesian calibration.  The tempo plot yields a graphic where
#' the slope of the plot directly reflects the pace of change: a period of rapid change
#' yields a steep slope and a period of slow change yields a gentle slope. When there is no
#' change, the plot is horizontal. When change is instantaneous, the plot is vertical.
#'
#' @references
#' Dye, T.S. (2016) Long-term rhythms in the development of Hawaiian social stratification.
#' Journal of Archaeological Science, 71, 1--9
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr},
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}, and
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @examples
#'   data(Events);
#'   TempoPlot(Events[1:1000, ], c(2:5), print.data.result = FALSE)
#'   TempoPlot(Events[1:1000, ], c(2:5), count = TRUE,  print.data.result = FALSE)
#'
#' @seealso \code{\link{tempo_plot}}
#'
#' @export
TempoPlot <- function (data, position, plot.result = NULL,level = 0.95,
                       count = TRUE, Gauss = FALSE, title = "Tempo plot",
                       subtitle = NULL, caption = "ArcheoPhases",
                       legend.title = "Legend",
                       legend.labels = c("Bayes estimate",
                                         "Credible interval, low",
                                         "Credible interval, high",
                                         "Gaussian approx., high",
                                         "Gaussian approx., low"),
                       x.label = "Calendar year",
                       y.label = "Cumulative events",
                       line.types = c("solid", "12", "11", "28", "28"),
                       width = 7, height = 7, units = "in",
                       x.min = NULL, x.max = NULL, colors = TRUE,
                       file = NULL, x.scale = "calendar",
                       elapsed.origin.position = NULL, newWindow=TRUE, print.data.result = FALSE)
{
  if (is.null(plot.result))
  {
    L = length(position)
    if (x.scale == "elapsed") {
      if (is.null(elapsed.origin.position)) {
        stop("Elapsed origin not specified")
      }
      else {
        data <- data - data[,elapsed.origin.position]
      }
    }

    groupOfDates = matrix(ncol = L, nrow = nrow(data))
    for (i in 1:L) {
      groupOfDates[, i] = data[, position[i]]
    }
    min = min(apply(groupOfDates, 2, min))
    max = max(apply(groupOfDates, 2, max))

    sequence = seq(min, max, length.out = 50 * ncol(groupOfDates))
    f = function(x) {
      g = stats::ecdf(x)
      y = g(sequence)
      if (count)
        y = y * ncol(groupOfDates)
      y
    }
    F = t(apply(groupOfDates, 1, f))
    # mean estimate
    moy = apply(F, 2, mean)
    # standard deviation
    ec = apply(F, 2, stats::sd)
    # Credible intervals
    qu = t(apply(F, 2, CredibleInterval)[-1,])
    # Gaussian credible intervals
    quG = cbind(moy + stats::qnorm(1 - (1 - level) / 2) * ec,
                moy - stats::qnorm(1 - (1 - level) / 2) * ec)

    if (x.scale == "BP") {
      result = list(t = 1950 - sequence, moy = (moy), qu = qu, quG = quG)
    }
    else {
      result = list(t = sequence, moy = moy, qu = qu, quG = quG)
    }
  }
  else
  {
    result = plot.result
  }


  ## Graphical part
  if (Gauss) {
    result.mat <- cbind(result$moy, result$qu, result$quG)
    colnames(result.mat) <- legend.labels
  }
  else {
    result.mat <- cbind(result$moy, result$qu)
    colnames(result.mat) <- legend.labels[1:3]
  }
  plot.result <- as.data.frame.table(result.mat)
  colnames(plot.result) <- c("Var1", "Legend", "Count")
  plot.result$Year <- result$t
  if (colors) {
    h <- ggplot2::ggplot(plot.result, ggplot2::aes(x = plot.result$Year, y = plot.result$Count,colour = plot.result$Legend))
    h <- h + ggplot2::guides(colour = ggplot2::guide_legend(title = legend.title))
  }
  else {
    h <- ggplot2::ggplot(plot.result, ggplot2::aes(x = plot.result$Year, y = plot.result$Count,linetype = plot.result$Legend))
    h <- h + ggplot2::scale_linetype_manual(values = line.types,guide = ggplot2::guide_legend(title = legend.title))
  }
  h <- h + ggplot2::geom_line()
  h <- h + ggplot2::scale_y_continuous(breaks = pretty(x = plot.result$Count))
  h <- h + ggplot2::labs(x = x.label,
                y = y.label,
                title = title,
                subtitle = subtitle,
                caption = caption)
  if (!is.null(x.min) & !is.null(x.max)) {
    h <- h + ggplot2::xlim(x.min, x.max)
  }
  if (!is.null(file)) {
    ggplot2::ggsave(filename = file, plot = h, height = height,
           width = width, units = units)
  }
  if(newWindow == TRUE) {
    grDevices::dev.new(height = height, width = width)
  }
  print(h)

  ## If the result is desired
  if (print.data.result == TRUE){
    result
  }

}

#' Tempo plot
#'
#' A statistical graphic designed for the archaeological study of
#' rhythms of the long term that embodies a theory of archaeological
#' evidence for the occurrence of events
#'
#' @param data Data frame or \code{archaeophases_mcmc} object containing
#' the output of the MCMC algorithm.
#' @param position A list, each member of which is either a numeric vector
#'     containing the positions of the columns corresponding to the MCMC chains
#'     of interest, or a vector of column names. For convenience, a vector can
#'     be substituted for the singleton list.
#' @param name A list, each member of which is a string that names the kind of
#'     event in the corresponding element of \code{position}. For convenience, a
#'     string can be substituted for the singleton list.
#' @param plot_result If \code{TRUE}, then draw a plot on the display,
#' else suppress drawing.
#' @param level Probability corresponding to the level of confidence.
#' @param count If \code{TRUE} the counting process is a number,
#' otherwise it is a probability.
#' @param Gauss If \code{TRUE}, the Gaussian approximation of the
#' credible interval is used.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param caption Caption of the plot.
#' @param legend_title Title of the plot legend.
#' @param legend_position One of "top", "bottom" (default),
#' "left", "right".
#' @param legend_labels Vector of three strings to label legend entries.
#' The strings must be unique.  The first string labels the central tendency
#' and the second and third strings label the high and low spreads.
#' @param x_label Label of the x-axis.
#' @param y_label Label of the y-axis.
#' @param line_types Type of the lines drawn on the plot in the order
#' of \code{legend_labels}.
#' @param line_sizes Width of the lines drawn on the plot in the
#' order of \code{legend_labels}.
#' @param line_colors Color names for the lines drawn on the plot
#' in the order of \code{legend_labels}.  If \code{color_palette} is
#' \code{NULL}, then standard color names are expected, otherwise the
#' color names are from the supplied \code{color_palette}.
#' @param width Width of the plot in \code{unit}.
#' @param height Height of the plot in \code{unit}.
#' @param unit String recognized by the \code{ggsave()} function,
#' one of "in" (default), "cm", or "mm".
#' @param x_min Minimum value for x-axis.
#' @param x_max Maximum value for x-axis.
#' @param color_palette A palette that supplies the colors used in the plot.
#' @param file Name of the file that will be saved if specified.
#' If \code{NULL} no file is saved.
#' @param x_scale One of "calendar" for calendar years, "BP" for years before present, or "elapsed" for time elapsed from a specified origin.
#' @param columns Number of columns for facet.
#' @param elapsed_origin_position If \code{x.scale} is "elapsed", the position
#' of the column corresponding to the event from which elapsed time is
#' calculated.
#' @param new_window Whether or not the plot is drawn within a new window.
#'
#' @return An \code{archaeophases_plot} object with the data and metadata
#' needed to reproduce the plot.
#'
#' @details
#' The tempo plot is one way to measure change over time: it estimates the
#' cumulative occurrence of archaeological events in a Bayesian calibration.
#' The tempo plot yields a graphic where the slope of the plot directly
#' reflects the pace of change: a period of rapid change yields a steep slope
#' and a period of slow change yields a gentle slope. When there is no change,
#' the plot is horizontal. When change is instantaneous, the plot is vertical.
#'
#' @references
#' Dye, T.S. (2016) Long-term rhythms in the development of Hawaiian social stratification.
#' Journal of Archaeological Science, 71, 1--9
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr},
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}, and
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @examples
#'   data(Events);
#'   tempo_plot(Events[1:1000, ], c(2:5))
#'   tempo_plot(Events[1:1000, ], c(2:5), count = TRUE)
#'
#' \dontrun{
#' # Read from connection
#' ox <- read_oxcal("http://tsdye.online/AP/ox.csv")
#' # Plot all the columns
#' tp <- tempo_plot(ox)
#' # Reproduce the tempo plot
#' plot(tp)
#' # View metadata
#' str(tp)
#' # Check that the MCMC data file hasn't changed
#' original_file(tp)
#'
#' # Use a custom palette
#' library(khroma)
#' light <- colours("light")
#' tp <- tempo_plot(ox, color_palette = light(2),
#' line_colors = c("light blue", "pale grey", "pale grey"))
#' }
#'
#' @seealso \code{\link{TempoPlot}}
#' @seealso \code{\link{new_archaeophases_plot}}
#'
#' @export
tempo_plot <- function(data,
                       position = 1:ncol(data),
                       name = list("All"),
                       level = 0.95,
                       count = TRUE,
                       Gauss = FALSE,
                       title = NULL,
                       subtitle = NULL,
                       caption = NULL,
                       legend_title = NULL,
                       legend_position = "bottom",
                       legend_labels = c("Bayes estimate",
                                         "Credible interval high",
                                         "Credible interval low"),
                       x_label = "Calendar year",
                       y_label = "Cumulative events",
                       line_types = c("solid",
                                      "dotted",
                                      "dotted"),
                       line_sizes = c(1.2, 0.8, 0.8),
                       line_colors = c("black",
                                       "grey50",
                                       "grey50"),
                       width = 7,
                       height = 7,
                       unit = "in",
                       x_min = NULL,
                       x_max = NULL,
                       color_palette = NULL,
                       file = NULL,
                       x_scale = "calendar",
                       elapsed_origin_position = NULL,
                       columns = 1,
                       new_window = TRUE,
                       plot_result = TRUE) {

    f = function(x, sequence, count, cols) {
        g <- stats::ecdf(x)
        y <- g(sequence)
        if (count)
            y <- y * cols
        y
    }

    prep_df <- function(data, position, name, eop, count) {
        data_subset <- data[, position]
        if (x_scale == "elapsed")
            data_subset <- data_subset - data[, eop]
        data_min <- min(data_subset)
        data_max <- max(data_subset)
        data_cols <- ncol(data_subset)
        data_seq <- seq(data_min, data_max, length.out = 50 * data_cols)
        doubt <- 1 - level

        F = t(apply(X = data_subset, MARGIN = 1,
                    FUN = f, sequence = data_seq,
                    count = count, cols = data_cols))
        ## mean estimate
        moy = apply(F, 2, mean)

        if (Gauss == TRUE) {
            ## standard deviation
            ec = apply(F, 2, stats::sd)
            ## Gaussian credible intervals
            qu = cbind(moy + stats::qnorm(1 - doubt / 2) * ec,
                       moy - stats::qnorm(1 - doubt / 2) * ec)
        }
        else { ## Credible intervals
            qu = t(apply(F, 2, CredibleInterval)[-1, ])
        }

        if (x_scale == "BP")
            data_seq <- 1950 - data_seq

        result_df <- data.frame(year = data_seq,
                                name = name,
                                count = moy,
                                ci_hi = qu[, 1],
                                ci_lo = qu[, 2])
    }

    if(!is.data.frame(data)) stop("Data format not recognized.")

    if(!is.element(x_scale, c("calendar", "BP", "elapsed")))
        stop(sprintf("%s is not a valid 'x_scale' value.", x_scale))

    if(x_scale == "elapsed" & is.null(elapsed_origin_position))
        stop("Elapsed origin not specified.")

    if(!is.list(position))
        position <- list(position)

    if(!is.list(name))
        name <- list(name)

    if(length(position) != length(name))
        stop("Position and name lists are different lengths.")

    if(is.element("archaeophases_plot", class(data)))
        result_df <- data
    else
        result_df <- mapply(prep_df,
                            position = position,
                            name = name,
                            MoreArgs = list(
                                data = data,
                                eop = elapsed_origin_position,
                                count = count),
                            SIMPLIFY = FALSE)

    result_to_plot <- reshape2::melt(result_df,
                                     id = c("name", "year"),
                                     measure = c("count", "ci_hi", "ci_lo"))

    h <- ggplot2::ggplot(data = result_to_plot,
                         mapping = aes(x = year,
                                       y = value,
                                       colour = variable,
                                       linetype = variable,
                                       size = variable))

    if (!is.null(color_palette)) {
        color_values <- unname(color_palette[line_colors])
    }
    else {
        color_values <- line_colors
    }

    h <- h + ggplot2::scale_color_manual(values = color_values,
                                         labels = legend_labels,
                                         name = legend_title)

    h <- h + ggplot2::scale_linetype_manual(values = line_types,
                                            labels = legend_labels,
                                            name = legend_title)

    h <- h + ggplot2::scale_size_manual(values = line_sizes,
                                        labels = legend_labels,
                                        name = legend_title)

    h <- h + ggplot2::geom_line()

    h <- h + ggplot2::labs(x = x_label,
                           y = y_label,
                           title = title,
                           subtitle = subtitle,
                           caption = caption)

    h <- h + ggplot2::theme(legend.position = legend_position)

    if (!is.null(x_min) & !is.null(x_max)) {
        h <- h + ggplot2::xlim(x_min, x_max)
    }

    if(length(position) > 1) {
        h <- h + ggplot2::facet_wrap(ggplot2::vars(name),
                                     scales = "free_y",
                                     ncol = columns)
        h <- h + theme(axis.ticks = element_blank(),
                       axis.text.y = element_blank())
    }

    if (!is.null(file)) {
        ggplot2::ggsave(filename = file,
                        plot = h,
                        height = height,
                        width = width,
                        units = unit)
    }

    if (plot_result == TRUE) {
        if (new_window == TRUE)
            grDevices::dev.new(height = height, width = width)
        print(h)
    }

    new_archaeophases_plot(x = result_df,
                           mcmc = data,
                           call = match.call())
}
