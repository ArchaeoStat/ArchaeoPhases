
####################################
###   Tempo plot   NEW Version 2017/08   ###

# The tempo plot introduced by T. S. Dye
# A statistical graphic designed for the archaeological study of rhythms of the long term that embodies a theory of archaeological evidence for the occurrence of events
#' @param data data frame containing the output of the MCMC algorithm
#' @param position numeric vector containing the position of the column corresponding to the MCMC chains of interest
#' @param plot.result a list containing the data to plot, typically the result of a previous run of TempoPlot()
#' @param print.data.result If TRUE, the list containing the data to plot will be given
#' @param level probability corresponding to the level of confidence
#' @param count, if TRUE the counting process is a number, otherwise it is a probability
#' @param Gauss if TRUE, the Gaussian approximation of the CI is used
#' @param title title of the graph
#' @param subtitle subtitle of the graph
#' @param caption caption of the graph
#' @param legend.title the title of the graph legend
#' @param legend.labels a vector of strings to label legend entries
#' @param x.label label of the x-axis
#' @param y.label label of the y-axis
#' @param line.types type of the lines drawn on the graph in the order of legend.labels
#' @param width width of the plot in units
#' @param height height of the plot in units
#' @param units units used to specify width and height.  One of "in", "cm", or "mm".  Default = "in".
#' @param x.min minimum value for x axis
#' @param x.max maximum value for x axis
#' @param colors if TRUE, the graph is drawn with colors, otherwise it is drawn in black and white
#' @param file the name of the graph (+ extension) that will be saved if chosen. Null by default.
#' @param x.scale one of "calendar", "bp", or "elapsed"
#' @param elapsed.origin.position if x.scale is "elapsed", the position of the column corresponding to the occurrence from which elapsed time is calculated
#' @param newWindow whether the plot is drawn within a new window or not
#' @param print.data.result If TRUE, the list containing the data to plot will be given
#' @return a list containing the data to plot
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

    t = seq(min, max, length.out = 50 * ncol(groupOfDates))
    f = function(x) {
      g = ecdf(x)
      y = g(t)
      if (count)
        y = y * ncol(groupOfDates)
      y
    }
    F = t(apply(groupOfDates, 1, f))
    moy = apply(F, 2, mean)
    ec = apply(F, 2, sd)
    qu = cbind(apply(F, 2, quantile, probs = (1 - level)/2, type = 8),
               apply(F, 2, quantile, probs = 1 - ((1 - level)/2), type = 8))
    quG = cbind(moy + qnorm(1 - (1 - level)/2) * ec, moy - qnorm(1 -
                                                                   (1 - level)/2) * ec)
 
    if (x.scale == "BP") {
      result = list(t = 1950 - t, moy = (moy), qu = qu, quG = quG)
    }
    else {
      result = list(t = t, moy = moy, qu = qu, quG = quG)
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