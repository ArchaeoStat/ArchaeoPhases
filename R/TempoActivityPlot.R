####################################
###   Tempo Activity plot   NEW Version 2017/08   ###
#' Plot the derivative of the tempo plot Bayesian estimate
#'
#' A statistical graphic designed for the archaeological study of rhythms of
#' the long term that embodies a theory of archaeological evidence for the
#' occurrence of events
#' @param data data frame containing the output of the MCMC algorithm
#' @param position numeric vector containing the position of the column
#' corresponding to the MCMC chains of interest
#' @param plot.result a list containing the data to plot, typically the
#' result of a previous run of \code{TempoActivityPlot()}
#' @param level probability corresponding to the level of confidence
#' @param title title of the graph
#' @param subtitle subtitle of the graph
#' @param caption caption of the graph
#' @param x.label label of the x-axis
#' @param y.label label of the y-axis
#' @param line.types type of the lines drawn on the graph
#' @param width width of the plot in \code{units}
#' @param height height of the plot in \code{units}
#' @param units units used to specify \code{width} and \code{height}, one of "in" (default),
#' "cm", or "mm"
#' @param x.min minimum value for x axis
#' @param x.max maximum value for x axis
#' @param file the name of the file to be saved if chosen, if \code{Null}, then no file is saved.
#' @param x.scale one of "calendar", "bp", or "elapsed"
#' @param elapsed.origin.position if \code{x.scale} is "elapsed", the position
#' of the column corresponding to the event from which elapsed time is calculated
#' @param newWindow whether the plot is drawn within a new window or not
#' @param print.data.result If \code{TRUE}, the list containing the data to plot
#' is returned
#' @return \code{NULL}, called for its side effects.  It may also return a list
#' containing the data to plot (if \code{print.data.result = TRUE}). The result
#' is given in calendar years (BC/AD).
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr} and
#'
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @references
#' Dye, T.S. (2016) Long-term rhythms in the development of Hawaiian social stratification.
#' Journal of Archaeological Science, 71, 1--9.
#'
#' @examples
#'   data(Events);
#'   TempoActivityPlot(Events[1:1000,], c(2:5), print.data.result=FALSE)
#'   TempoActivityPlot(Events[1:1000,], c(2:5), print.data.result=FALSE)
#'
#' @keywords tempo activity plot
#' mean
#'
#' @export
TempoActivityPlot <- function (data, position, plot.result = NULL, level = 0.95,
                               title = "Activity plot",
                               subtitle = NULL, caption = "ArcheoPhases",
                               x.label = "Calendar year",
                               y.label = "Activity",
                               line.types = c("solid"),
                               width = 7, height = 7, units = "in",
                               x.min = NULL, x.max = NULL,
                               file = NULL, x.scale = "calendar",
                               elapsed.origin.position = NULL,
                               newWindow=TRUE, print.data.result = FALSE)
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
    x = 10^c(0:10)
    if (min != 0) {
      c = 0
      for (i in 1:length(x)) {
        if (abs(min/x[i]) > 1) {
          c = c + 1
        }
      }
      if (c > 3) {
        min = floor(min/x[c - 1]) * x[c - 1]
      }
      else {
        min = floor(min/x[c]) * x[c]
      }
    }
    if (max != 0) {
      d = 0
      for (i in 1:length(x)) {
        if (abs(max/x[i]) > 1) {
          d = d + 1
        }
      }
      if (d > 3) {
        max = ceiling(max/x[d - 1]) * x[d - 1]
      }
      else {
        max = ceiling(max/x[d]) * x[d]
      }
    }
    t = seq(min, max, length.out = 50 * ncol(groupOfDates))
    f = function(x) {
      g = ecdf(x)
      y = g(t)
      #y = g(t)* ncol(groupOfDates)
    }
    F = t(apply(groupOfDates, 1, f))
    moy = apply(F, 2, mean)
    x<-t[-1]
    y<-diff(moy)/diff(t)

    if (x.scale == "bp") {
      result = list(t = 1950 - x, y = y)
    }
    else {
      result = list(t = x, y = y)
    }
  }
  else
  {
    result = plot.result
  }
  result.mat <- cbind(t=x, y=y)
  plot.result <- as.data.frame(result.mat)

  h <- ggplot2::ggplot(data = plot.result, ggplot2::aes(x = t, y = y))
  h <- h + ggplot2::scale_linetype_manual(values = line.types)
  h <- h + ggplot2::geom_line()
  h <- h + ggplot2::scale_y_continuous(breaks = pretty(x = plot.result$y))
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
