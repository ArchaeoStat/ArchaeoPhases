###################################
### Occurrence plot ## NEW 2017/08      ###
#' Plot occurrences
#'
#' A statistical graphic designed for the archaeological study of when
#' events of a specified kind occurred
#'
#' @param data Data frame containing the output of the MCMC algorithm.
#' @param position Numeric vector containing the position of the column
#' corresponding to the MCMC chains of interest.
#' @param plot.result If \code{TRUE}, then draw a plot on the display,
#' else suppress drawing.
#' @param level Probability corresponding to the level of confidence.
#' @param intervals One of "CI" for credible intervals or
#' "HPD" for highest posterior density intervals.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param caption Caption of the plot.
#' @param labelXaxis Label of the x-axis.
#' @param labelYaxis Label of the y-axis.
#' @param language String indicating a language recognized by the
#' \pkg{toOrdinal} package.
#' @param occurrence String to append to each y-axis tic label.
#' @param width Plot width in \code{units}.
#' @param height Plot height in \code{units}.
#' @param units String recognized by the \code{ggsave()} function,
#' one of "in", "cm", "mm".
#' @param x.min Minimum x-axis value.
#' @param x.max Maximum x-axis value.
#' @param x.scale One of "calendar" for calendar years,
#' "BP" for years before present,
#' or "elapsed" for time elapsed from a specified origin.
#' @param elapsed.origin.position Position of the column to use
#' as the origin for elapsed time calculations.
#' @param dumbbell.size Size of the plot symbol.
#' @param dot.guide Switch for a horizontal guide from the y axis.
#' @param dot.guide.size Size of the dot guide.
#' @param y.grid Switch for horizontal grid lines.
#' @param file Name of the file that will be saved if specified.
#' If \code{NULL} no plot will be saved.
#' @param newWindow Whether or not the plot is drawn within a new window.
#' @param print.data.result If \code{TRUE}, the list containing the data
#' to plot will be returned.
#'
#' @details
#' If we have k events, then we can estimate the calendar date t corresponding to the
#' smallest date such that the number of events observed before t is equal to k.
#' The \code{OccurrencePlot()} estimates these occurrences and gives the credible
#' interval or the highest posterior density (HPD) region with a desired level of confidence.
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr},
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}, and
#' @author Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @return \code{NULL}, called for its side effects. It may also return
#' a list containing the data to plot (if \code{print.data.result = TRUE}).
#'
#' @examples
#'   data(Events);
#'   OccurrencePlot(Events[1:1000, ], c(2:5),  print.data.result = FALSE)
#'
#' @import toOrdinal
#' @importFrom grDevices dev.new
#'
#' @export
OccurrencePlot <- function(data, position, plot.result = NULL, level = 0.95, intervals = "CI",
                           title = "Occurrence plot",
                           subtitle = NULL,
                           caption = "ArchaeoPhases",
                           labelXaxis = "Calendar year",
                           labelYaxis = NULL,
                           language = "English", occurrence = "occurrence",
                           height = 7, width = 7, units = "in",
                           x.min = NULL, x.max = NULL, x.scale = "calendar",
                           elapsed.origin.position = NULL,
                           dumbbell.size = 1, dot.guide = FALSE,
                           dot.guide.size = 0.25, y.grid = FALSE,
                           file = NULL, newWindow=TRUE, print.data.result = FALSE)
{

  sort.rows <- function(x) {
    if (is.numeric(as.matrix(x))) {
      res <- as.data.frame(t(apply(x,1,sort)))
      colnames(res) <- 1:ncol(res)
      res
    }
    else {
      stop("Cannot sort non-numeric data frame")
    }
  }
  group.dates <- function(data, position) {
    L = length(position)
    res = matrix(ncol = L, nrow=nrow(data))
    for (i in 1:L) {
      res[,i] = data[,position[i]]
    }
    res
  }
  if (x.scale == "elapsed") {
    if (is.null(elapsed.origin.position)) {
      stop("Elapsed origin not specified")
    }
    else {
      data <- data - data[,elapsed.origin.position]
    }
  }
  groupOfDates <- group.dates(data, position)
  groupOfEvents <- sort.rows(groupOfDates)

  if (intervals == "CI") {
    Bornes = MultiCredibleInterval(groupOfEvents, 1:ncol(groupOfEvents), level = level, roundingOfValue = 0)
    Ordered.df <- as.data.frame(Bornes)
    Ordered.df$y.labs <- paste(sapply(as.integer(rownames(Bornes)), toOrdinal, language = language), occurrence, sep= " ")

  }
  else if (intervals == "HPD") {
    Bornes = MultiHPD(groupOfEvents, 1:ncol(groupOfEvents), level = level)
    Ordered.df <- as.data.frame(Bornes)
    Ordered.df$y.labs <- paste(sapply(as.integer(rownames(Bornes)), toOrdinal, language = language), occurrence, sep= " ")

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
      Ordered.df$y.labs <- paste(sapply(as.integer(rownames(data)), toOrdinal, language = language), occurrence, sep= " ")
    }

  }

  if (x.scale == "BP") {
    Ordered.df[,2] <- 1950-Ordered.df[,2]
    Ordered.df[,3] <- 1950-Ordered.df[,3]
  }

  h <- ggplot2::ggplot(data = Ordered.df, ggplot2::aes(y=factor(Ordered.df$y.labs, levels=unique(Ordered.df$y.labs), ordered=TRUE), x=Ordered.df[,2], xend=Ordered.df[,3]))
  h <- h + ggalt::geom_dumbbell(size = dumbbell.size, dot_guide = dot.guide, dot_guide_size = dot.guide.size)
  h <- h + ggplot2::labs(x = labelXaxis, y = labelYaxis, title = title,subtitle = subtitle, caption = caption)

  if (!y.grid) {
    h <- h + ggplot2::theme(panel.grid.major.y=ggplot2::element_blank())
  }
  if (!is.null(x.min) & !is.null(x.max)) {
    h <- h + ggplot2::xlim(x.min, x.max)
  }
  if (!is.null(file)) {
    ggplot2::ggsave(filename = file, plot = h, height = height,width = width, units = units)
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

#' Plot occurrences
#'
#' A statistical graphic designed for the archaeological study of when
#' events of a specified kind occurred
#'
#' @param data Data frame containing the output of the MCMC algorithm.
#' @param position Numeric vector containing the position of the column
#' corresponding to the MCMC chains of interest, or a vector of column
#' names.
#' @param plot_result If \code{TRUE}, then draw a plot on the display,
#' else suppress drawing.
#' @param level Probability corresponding to the level of confidence.
#' @param intervals One of "CI" for credible intervals or
#' "HPD" for highest posterior density intervals.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param caption Caption of the plot.
#' @param x_label Label of the x-axis.
#' @param y_label Label of the y-axis.
#' @param language String indicating a language recognized by the
#' \pkg{toOrdinal} package.
#' @param occurrence String to append to each y-axis tic label.
#' @param width Plot width in \code{units}.
#' @param height Plot height in \code{units}.
#' @param units String recognized by the \code{ggsave()} function,
#' one of "in", "cm", "mm".
#' @param x_min Minimum x-axis value.
#' @param x_max Maximum x-axis value.
#' @param x_scale One of "calendar" for calendar years,
#' "BP" for years before present,
#' or "elapsed" for time elapsed from a specified origin.
#' @param elapsed_origin_position Position of the column to use
#' as the origin for elapsed time calculations.
#' @param dumbbell_size Size of the plot symbol.
#' @param dot_guide Switch for a horizontal guide from the y axis.
#' @param dot_guide_size Size of the dot guide.
#' @param y_grid Switch for horizontal grid lines.
#' @param file Name of the file that will be saved if specified.
#' If \code{NULL} no plot will be saved.
#' @param new_window Whether or not the plot is drawn within a new window.
#'
#' @details
#' If we have k events, then we can estimate the calendar date t corresponding to the
#' smallest date such that the number of events observed before t is equal to k.
#' The \code{OccurrencePlot()} estimates these occurrences and gives the credible
#' interval or the highest posterior density (HPD) region with a desired level of confidence.
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr},
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}, and
#' @author Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @return An \code{archaeophases_plot} object with the data and metadata
#' needed to reproduce the plot.
#'
#' @examples
#'   data(Events);
#'   OccurrencePlot(Events[1:1000, ], c(2:5),  print.data.result = FALSE)
#'
#'
#' \dontrun{
#' # Read from connection
#' ox <- read_oxcal("http://tsdye.online/AP/ox.csv")
#' # Plot all the columns
#' op <- occurrence_plot(ox, position = 1:ncol(ox))
#' # Plot again
#' plot(op)
#' # View metadata
#' str(op)
#' }
#'
#' @import toOrdinal
#' @importFrom grDevices dev.new
#'
#' @export
occurrence_plot <- function(data,
                            position = 1:ncol(data),
                            level = 0.95,
                            plot_result = TRUE,
                            intervals = "CI",
                            title = "Occurrence plot",
                            subtitle = NULL,
                            caption = "ArchaeoPhases",
                            x_label = "Calendar year",
                            y_label = NULL,
                            language = "English", occurrence = "occurrence",
                            height = 7, width = 7, units = "in",
                            x_min = NULL, x_max = NULL, x_scale = "calendar",
                            elapsed_origin_position = NULL,
                            dumbbell_size = 1, dot_guide = FALSE,
                            dot_guide_size = 0.25, y_grid = FALSE,
                            file = NULL, new_window=TRUE)
{
    if(!is.data.frame(data)) stop("Data format not recognized.")

    sort.rows <- function(x) {
        if (is.numeric(as.matrix(x))) {
            res <- as.data.frame(t(apply(x,1,sort)))
            colnames(res) <- 1:ncol(res)
            res
        }
        else {
            stop("Cannot sort non-numeric data frame")
        }
    }

    if(is.element("archaeophases_plot", class(data))) {
        group_of_events <- data
    }
    else {
        temp_data <- data[, position]
        if (x_scale == "elapsed") {
            if (is.null(elapsed_origin_position)) {
                stop("Elapsed origin not specified")
            }
            else {
                temp_data <- temp_data - data[,elapsed_origin_position]
            }
        }
        group_of_events <- sort.rows(temp_data)
    }

    if (intervals == "CI") {
        Bornes = MultiCredibleInterval(data = group_of_events,
                                       position = 1:ncol(group_of_events),
                                       level = level,
                                       roundingOfValue = 0)
        ordered_df <- as.data.frame(Bornes)
        ordered_df$y.labs <- paste(sapply(as.integer(rownames(Bornes)),
                                          toOrdinal,
                                          language = language),
                                   occurrence, sep= " ")
    }
    else if (intervals == "HPD") {
        Bornes = MultiHPD(group_of_events, 1:ncol(group_of_events), level = level)
        ordered_df <- as.data.frame(Bornes)
        ordered_df$y.labs <- paste(sapply(as.integer(rownames(Bornes)), toOrdinal,
                                          language = language), occurrence, sep= " ")

                                        # In the case of (multiple) two intervals
        x = (dim(Bornes)[2] - 1) /2
        if ( x > 1 ){
            data = NULL
            for(j in 1:x){
                Bornesj = subset(Bornes,select=-c(2*j,2*j+1))
                data = rbind(Bornes, data)
            }
            data = data[is.na(data[,2])==FALSE, ]
            ordered_df <- as.data.frame(data)
            ordered_df$y.labs <- paste(sapply(as.integer(rownames(data)), toOrdinal,
                                              language = language), occurrence, sep= " ")
        }
    }

    if (x_scale == "BP") {
        ordered_df[,2] <- 1950-ordered_df[,2]
        ordered_df[,3] <- 1950-ordered_df[,3]
    }


    h <- ggplot2::ggplot(data = ordered_df,
                         ggplot2::aes(y=factor(ordered_df$y.labs,
                                               levels=unique(ordered_df$y.labs),
                                               ordered=TRUE),
                                      x=ordered_df[,2], xend=ordered_df[,3]))

    h <- h + ggalt::geom_dumbbell(size = dumbbell_size,
                                  dot_guide = dot_guide,
                                  dot_guide_size = dot_guide_size)

    h <- h + ggplot2::labs(x = x_label,
                           y = y_label,
                           title = title,
                           subtitle = subtitle,
                           caption = caption)

    if (!y_grid) {
        h <- h + ggplot2::theme(panel.grid.major.y=ggplot2::element_blank())
    }

    if (!is.null(x_min) & !is.null(x_max)) {
        h <- h + ggplot2::xlim(x_min, x_max)
    }

    if (!is.null(file)) {
        ggplot2::ggsave(filename = file, plot = h, height = height,width = width, units = units)
    }

    if (plot_result == TRUE) {
        if (new_window == TRUE) {
            dev.new(height = height, width = width)
        }
        print(h)
    }

    new_archaeophases_plot(x = as.data.frame(group_of_events),
                           mcmc = data,
                           call = match.call())
}
