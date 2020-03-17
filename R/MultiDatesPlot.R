###############################################
#      MultiDatesPlot  # Revised version  2017/09       #
###############################################
#' Plot of credible intervals or HPD regions of a series of events
#'
#' @param data Data frame containing the output of the MCMC algorithm.
#' @param position Numeric vector containing the position of the column
#' corresponding to the MCMC chains of interest.
#' @param level Probability corresponding to the level of confidence.
#' @param roundingOfValue Integer indicating the number of decimal places to be used.
#' @param intervals One of "CI" for credible intervals, or "HPD" for highest
#' posterior density intervals.
#' @param order Order of the events. If "default" then the order of the csv file
#' is followed, if "increasing" events are ordered by the HPDInf of the
#' first region or the CIInf
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param caption Caption of the plot.
#' @param labelXaxis X axis label of the plot.
#' @param labelYaxis Y axis label of the plot.
#' @param height Height of the plot in \code{units}.
#' @param width Width of the plot in \code{units}.
#' @param units A string recognized by \code{ggsave()} function,
#' one of "in", "cm", "mm".
#' @param x.min Minimum x axis value.
#' @param x.max Maximum x axis value.
#' @param x.scale One of "calendar" for calendar years,
#' "BP" for years before present,
#' or "elapsed" for years after a specified origin.
#' @param elapsed.origin.position Position of the column corresponding
#' to the origin for elapsed time calculations.
#' @param dumbbell.size Size of the symbols used to plot events.
#' @param dot.guide Switch for guides from y-axis to plot symbols.
#' @param dot.guide.size Size of the dot guides.
#' @param y.grid Switch for horizontal grids.
#' @param file  Name of the file to be saved. If \code{NULL} then no plot is saved.
#' @param newWindow Whether the plot is drawn within a new window or not.
#' @param print.data.result If \code{TRUE}, the list containing the data to plot
#' will be returned.
#'
#' @return NULL, called for its side effects.  If \code{print.data.result = TRUE}
#' then a list containing the data to plot will be returned.
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr},
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}, and
#' @author Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @examples
#'   data(Events)
#'   MultiDatesPlot(Events, c(2, 4, 3), level = 0.95, intervals = "CI",
#'                  title = "Plot of CI intervals")
#'   MultiDatesPlot(Events, c(2, 4, 3), level = 0.95, intervals = "HPD",
#'                  title = "Plot of HPD intervals")
#'   MultiDatesPlot(Events, c(2, 4, 3), level = 0.95, intervals = "HPD",
#'                  order = "increasing")
#'
#' @importFrom grDevices dev.new
#' @importFrom stats reorder
#'
#' @export
MultiDatesPlot <- function (data, position, level = 0.95, roundingOfValue = 0,
                            intervals = "CI", order ="default",
                            title = "Plot of intervals",
                            subtitle = NULL,
                            caption = "ArchaeoPhases",
                            labelXaxis = "Calendar Year",
                            labelYaxis = NULL,
                            height = 7, width = 7, units = "in",
                            x.min = NULL, x.max = NULL,
                            x.scale = "calendar",
                            elapsed.origin.position = NULL,
                            dumbbell.size = 3, dot.guide = FALSE,
                            dot.guide.size = 0.25, y.grid = FALSE,
                            file = NULL, newWindow=TRUE, print.data.result = FALSE)
{
  if (x.scale == "elapsed") {
    if (is.null(elapsed.origin.position)) {
      stop("Elapsed origin not specified")
    }
    else {
      data <- data - data[,elapsed.origin.position]
    }
  }
  if (intervals == "CI") {
    Bornes = MultiCredibleInterval(data, position, level = level, roundingOfValue = roundingOfValue)
    Ordered = Bornes
    Ordered.df <- as.data.frame(Ordered)
    Ordered.df$y.labs <- factor(rownames(Ordered), levels = rownames(Ordered))
  }
  else if (intervals == "HPD") {
    Bornes = MultiHPD(data, position, level = level, roundingOfValue = roundingOfValue)

    Ordered.df <- as.data.frame(Bornes)
    Ordered.df$y.labs <- factor(rownames(Bornes), levels = rownames(Bornes))

    # In the case of (multiple) two intervals
    x = (dim(Bornes)[2] - 1) /2
    if ( x > 1 ){
      data = NULL
      for(j in 1:x){
        Bornesj = subset(Bornes,select=-c(2*j,2*j+1))
        data = rbind(Bornesj, data)
      }
      data = data[is.na(data[,2])==FALSE,]

      Ordered.df <- as.data.frame(data)
      Ordered.df$y.labs <- factor(rownames(data))
    }

  }

  if (x.scale == "BP") {
    Ordered.df[,2] <- 1950-Ordered.df[,2]
    Ordered.df[,3] <- 1950-Ordered.df[,3]
  }

  if(order == "increasing"){
    h <- ggplot2::ggplot(data = Ordered.df, ggplot2::aes(y = reorder(Ordered.df$y.labs, Ordered.df[,2]), x=Ordered.df[,2], xend=Ordered.df[,3]))
  } else if (order == "default") {
    h <- ggplot2::ggplot(data = Ordered.df, ggplot2::aes(y = Ordered.df$y.labs, x=Ordered.df[,2], xend=Ordered.df[,3]))
  }

  h <- h + ggplot2::labs(x = labelXaxis, y = labelYaxis, title = title,subtitle = subtitle, caption = caption)
  h <- h + ggalt::geom_dumbbell(size = dumbbell.size, dot_guide = dot.guide,dot_guide_size = dot.guide.size)

  if (!y.grid) {
    h <- h + ggplot2::theme(panel.grid.major.y=ggplot2::element_blank())
  }
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
    Bornes
  }
}

#' Plot of credible intervals or HPD regions of a series of events
#'
#' @param data Data frame containing the output of the MCMC algorithm.
#' @param position Numeric vector containing the positions of the columns
#' corresponding to the MCMC chains of interest, or a vector of column
#' names.
#' @param plot_result If \code{TRUE}, then draw a plot on the display,
#' else suppress drawing.
#' @param level Probability corresponding to the level of confidence.
#' @param round Integer indicating the number of decimal places to be used.
#' @param intervals One of "CI" for credible intervals, or "HPD" for highest
#' posterior density intervals.
#' @param order Order of the events. If "default" then the order of the csv file
#' is followed, if "increasing" events are ordered by the HPDInf of the
#' first region or the CIInf
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param caption Caption of the plot.
#' @param x_label X axis label of the plot.
#' @param y_label Y axis label of the plot.
#' @param height Height of the plot in \code{units}.
#' @param width Width of the plot in \code{units}.
#' @param units A string recognized by \code{ggsave()} function,
#' one of "in", "cm", "mm".
#' @param x_min Minimum x axis value.
#' @param x_max Maximum x axis value.
#' @param x_scale One of "calendar" for calendar years,
#' "BP" for years before present,
#' or "elapsed" for years after a specified origin.
#' @param elapsed_origin_position Position of the column corresponding
#' to the origin for elapsed time calculations.
#' @param dumbbell_size Size of the symbols used to plot events.
#' @param dot_guide Switch for guides from y-axis to plot symbols.
#' @param dot_guide_size Size of the dot guides.
#' @param y_grid Switch for horizontal grids.
#' @param file  Name of the file to be saved. If \code{NULL} then no plot is saved.
#' @param new_window Whether the plot is drawn within a new window or not.
#' @param plot_result If \code{TRUE}, then draw a plot on the display,
#' else suppress drawing.
#'
#' @return An \code{archaeophases_plot} object with the data and metadata
#' needed to reproduce the plot.
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr},
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}, and
#' @author Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @examples
#'   data(Events)
#'   multi_dates_plot(Events, c(2, 4, 3), level = 0.95, intervals = "CI",
#'                  title = "Plot of CI intervals")
#'   multi_dates_plot(Events, c(2, 4, 3), level = 0.95, intervals = "HPD",
#'                  title = "Plot of HPD intervals")
#'   multi_dates_plot(Events, c(2, 4, 3), level = 0.95, intervals = "HPD",
#'                  order = "increasing")
#'
#' @importFrom grDevices dev.new
#' @importFrom stats reorder
#'
#' @export
multi_dates_plot <- function (data,
                              position = 1:ncol(data),
                              level = 0.95,
                              plot_result = TRUE,
                              round = 0,
                              intervals = "CI",
                              order = "default",
                              title = "Plot of intervals",
                              subtitle = NULL,
                              caption = "ArchaeoPhases",
                              x_label = "Calendar Year",
                              y_label = NULL,
                              height = 7,
                              width = 7,
                              units = "in",
                              x_min = NULL,
                              x_max = NULL,
                              x_scale = "calendar",
                              elapsed_origin_position = NULL,
                              dumbbell_size = 1,
                              dot_guide = FALSE,
                              dot_guide_size = 0.25,
                              y_grid = FALSE,
                              file = NULL,
                              new_window = TRUE)
{
    if(!is.data.frame(data)) stop("Data format not recognized.")

    if (!is.element("archaeophases_plot", class(data))) {
        plot_data <- data
    }
    else {
        if (x_scale == "elapsed") {
            if (is.null(elapsed_origin_position)) {
                stop("Elapsed origin not specified")
            }
            else {
                data <- data - data[, elapsed_origin_position]
            }
        }
        plot_data <- data[, position]
    }

    if (intervals == "CI") {
        Bornes = MultiCredibleInterval(plot_data, 1:ncol(plot_data),
                                       level = level,
                                       roundingOfValue = round)
        Ordered.df <- as.data.frame(Bornes)
        Ordered.df$y.labs <- factor(rownames(Bornes),
                                    levels = rownames(Bornes))
    }
    else if (intervals == "HPD") {
        Bornes = MultiHPD(plot_data, 1:ncol(plot_data),
                          level = level,
                          roundingOfValue = round)

        Ordered.df <- as.data.frame(Bornes)
        Ordered.df$y.labs <- factor(rownames(Bornes),
                                    levels = rownames(Bornes))

                                        # In the case of (multiple) two intervals
        x = (dim(Bornes)[2] - 1) /2
        if ( x > 1 ){
            hpd_data = NULL
            for(j in 1:x){
                Bornesj = subset(Bornes, select=-c(2*j,2*j+1))
                hpd_data = rbind(Bornesj, hpd_data)
            }
            hpd_data = hpd_data[is.na(hpd_data[,2])==FALSE,]

            Ordered.df <- as.data.frame(hpd_data)
            Ordered.df$y.labs <- factor(rownames(hpd_data))
        }
    }

    if (x_scale == "BP") {
        Ordered.df[,2] <- 1950 - Ordered.df[,2]
        Ordered.df[,3] <- 1950 - Ordered.df[,3]
    }

    if (order == "increasing"){
        h <- ggplot2::ggplot(data = Ordered.df,
                             ggplot2::aes(y = reorder(Ordered.df$y.labs,
                                                      Ordered.df[,2]),
                                          x=Ordered.df[,2],
                                          xend=Ordered.df[,3]))
    } else if (order == "default") {
        h <- ggplot2::ggplot(data = Ordered.df,
                             ggplot2::aes(y = Ordered.df$y.labs,
                                          x=Ordered.df[,2],
                                          xend=Ordered.df[,3]))
    }

    h <- h + ggplot2::labs(x = x_label, y = y_label,
                           title = title,subtitle = subtitle,
                           caption = caption)

    h <- h + ggalt::geom_dumbbell(size = dumbbell_size,
                                  dot_guide = dot_guide,
                                  dot_guide_size = dot_guide_size)

    if (!y_grid) {
        h <- h + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    }

    if (!is.null(x_min) & !is.null(x_max)) {
        h <- h + ggplot2::xlim(x_min, x_max)
    }

    if (!is.null(file)) {
        ggplot2::ggsave(filename = file, plot = h, height = height,
                        width = width, units = units)
    }

    if (plot_result == TRUE) {
        if (new_window) {
            dev.new(height = height, width = width)
        }
        print(h)
    }

    new_archaeophases_plot(x = plot_data,
                           mcmc = data,
                           call = match.call())
}
