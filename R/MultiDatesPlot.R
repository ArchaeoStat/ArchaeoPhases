

###############################################
#      MultiDatesPlot  # Revised version  2017/09       #
###############################################

#' Plot of credible intervals or HPD regions of a series of dates
#'
#' @param data dataframe containing the output of the MCMC algorithm
#' @param position numeric vector containing the position of the column corresponding to the MCMC chains of interest
#' @param level probability corresponding to the level of confidence
#' @param intervals one of "CI" for credible intervals, or "HPD" for highest posterior density intervals
#' @param order the order of the dates. If "default" then the order of the csv file is followed, if "increasing" dates are ordered by the HPDInf of the first region or the CIInf. 
#' @param title title of the graph
#' @param subtitle subtitle of the graph
#' @param caption caption of the graph
#' @param labelXaxis x axis label of the graph
#' @param labelYaxis y axis label of the graph
#' @param height height of the graph in units
#' @param width width of the graph in units
#' @param units recognized by ggsave function, one of "in", "cm", "mm"
#' @param x.min minimum x axis value
#' @param x.max maximum x axis value
#' @param x.scale one of "calendar" for calendar years, "BP" for years before present, or "elapsed" for years after a specified origin
#' @param elapsed.origin.position the position of the column corresponding to the origin for elapsed time calculations
#' @param dumbbell.size size of the symbols used to plot dates
#' @param dot.guide switch for guides from y-axis to plot symbols
#' @param dot.guide.size size of the dot guides
#' @param y.grid switch for horizontal grids
#' @param file the name of the file to be saved. If NULL then no graph is saved.
#' @param newWindow whether the plot is drawn within a new window or not
#' @param print.data.result If TRUE, the list containing the data to plot will be given
#' @return a plot of the endpoints of the credible intervals of a series of dates
#' @export

MultiDatesPlot <- function (data, position, level = 0.95, intervals = "CI", order ="default", 
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
    Bornes = MultiCredibleInterval(data, position, level = level)
    Ordered = Bornes
    Ordered.df <- as.data.frame(Ordered)
    Ordered.df$y.labs <- factor(rownames(Ordered), levels = rownames(Ordered))
  }
  else if (intervals == "HPD") {
    Bornes = MultiHPD(data, position, level = level)

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

