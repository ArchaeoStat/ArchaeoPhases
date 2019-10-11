
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
#' @importFrom stats ecdf sd qnorm
#' @importFrom grDevices dev.new
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
      g = ecdf(x)
      y = g(sequence)
      if (count)
        y = y * ncol(groupOfDates)
      y
    }
    F = t(apply(groupOfDates, 1, f))
    # mean estimate
    moy = apply(F, 2, mean)
    # standard deviation
    ec = apply(F, 2, sd)
    # Credible intervals
    qu = t(apply(F, 2, CredibleInterval)[-1,])
    # Gaussian credible intervals
    quG = cbind(moy + qnorm(1 - (1 - level)/2) * ec, moy - qnorm(1 -(1 - level)/2) * ec)

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
    dev.new(height = height, width = width)
  }
  print(h)

  ## If the result is desired
  if (print.data.result == TRUE){
    result
  }

}

tempo_plot <- function(data, position, level = 0.95,
                       count = TRUE, Gauss = FALSE, title = "Tempo plot",
                       subtitle = NULL, caption = "ArcheoPhases",
                       legend_title = "Legend",
                       legend_labels = c("Bayes estimate",
                                         "Credible interval, low",
                                         "Credible interval, high",
                                         "Gaussian approx., high",
                                         "Gaussian approx., low"),
                       x_label = "Calendar year",
                       y_label = "Cumulative events",
                       line.types = c("solid", "12", "11", "28", "28"),
                       width = 7, height = 7, units = "in",
                       x_min = NULL, x_max = NULL, colors = TRUE,
                       file = NULL, x_scale = "calendar",
                       elapsed_origin_position = NULL, new_window=TRUE)
{

    if(!is.list(data)) stop("Data format not recognized.")

    if(is.data.frame(data))
    {
        data <- data[, position]
        plot_names <- names(data)
        if (x_scale == "elapsed") {
            if (is.null(elapsed_origin_position)) {
                stop("Elapsed origin not specified")
            }
            else {
                data <- data - data[, elapsed_origin_position]
            }
        }
        data_min <- min(data)
        data_max <- max(data)
        data_cols <- ncols(data)
        data_seq <- seq(min, max, length.out = 50 * data_cols)
        doubt <- 1 - level

        f = function(x, sequence, count, cols) {
            g = ecdf(x)
            y = g(sequence)
            if (count)
                y = y * cols
            y
        }

        F = t(apply(X = data, MARGIN = 1, FUN = f, sequence = data_seq,
                    count = count, cols = data_cols))
                                        # mean estimate
        moy = apply(F, 2, mean)
                                        # standard deviation
        ec = apply(F, 2, sd)
                                        # Credible intervals
        qu = t(apply(F, 2, CredibleInterval)[-1,])
                                        # Gaussian credible intervals
        quG = cbind(moy + qnorm(1 - doubt / 2) * ec, moy - qnorm(1 - doubt / 2) * ec)

        if (x_scale == "BP") {
            plot_data = list(t = 1950 - sequence, moy = (moy), qu = qu, quG = quG)
        }
        else {
            plot_data = list(t = sequence, moy = moy, qu = qu, quG = quG)
        }
    }
    else {
        plot_data <- data$plot_data
        plot_names <- data$plot_names
    }

  ## Graphical part
  if (Gauss) {
    result_mat <- cbind(result$moy, result$qu, result$quG)
    colnames(result_mat) <- legend_labels
  }
  else {
    result_mat <- cbind(result$moy, result$qu)
    colnames(result_mat) <- legend_labels[1:3]
  }
  plot_result <- as.data.frame.table(result_mat)
  colnames(plot_result) <- c("Var1", "Legend", "Count")
  plot_result$Year <- plot_data$t
  if (colors) {
      h <- ggplot2::ggplot(plot_result, ggplot2::aes(x = plot_result$Year,
                                                     y = plot_result$Count,
                                                     colour = plot_result$Legend))
    h <- h + ggplot2::guides(colour = ggplot2::guide_legend(title = legend_title))
  }
  else {
      h <- ggplot2::ggplot(plot_result, ggplot2::aes(x = plot_result$Year,
                                                     y = plot_result$Count,
                                                     linetype = plot_result$Legend))
      h <- h + ggplot2::scale_linetype_manual(values = line.types,
                                              guide = ggplot2::guide_legend(title = legend_title))
  }
  h <- h + ggplot2::geom_line()
  h <- h + ggplot2::scale_y_continuous(breaks = pretty(x = plot_result$Count))
  h <- h + ggplot2::labs(x = x_label,
                y = y_label,
                title = title,
                subtitle = subtitle,
                caption = caption)
  if (!is.null(x_min) & !is.null(x_max)) {
    h <- h + ggplot2::xlim(x_min, x_max)
  }

  if (!is.null(file)) {
    ggplot2::ggsave(filename = file, plot = h, height = height,
           width = width, units = units)
  }

  if(new_window == TRUE) {
    dev.new(height = height, width = width)
  }

    print(h)

    list(plot_data = plot_data, plot_object = h,
         plot_names = plot_names)
}
