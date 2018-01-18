#####################################################
#       Multi Marginal posterior Density            #
#       NEW version in ArchaeoPhases 1.4            #
#####################################################
#' Marginal posterior Densities of several dates
#'
#' Plots the densities of several chains + statistics (mean, CI, HPDR)
#'
#' @param data dataframe containing the output of the MCMC algorithm
#' @param position numeric vector containing the position of the column corresponding to the MCMC chains of interest
#' @param level probability corresponding to the level of confidence
#' @param GridLength length of the grid used to estimate the density
#' @param x.scale vector of one of "calendar" for calendar years, "BP" for years before present, or "elapsed" for time elapsed from a specified origin
#' @param elapsed.origin.position position of the column to use as the origin for elapsed time calculations
#' @param title title of the graph
#' @param subtitle subtitle of the graph
#' @param caption caption of the graph
#' @param x.label label of the x-axis
#' @param y.label label of the y-axis
#' @param width width size in units
#' @param height height size in units
#' @param units a string recognized by the ggsave function, one of "in", "cm", "mm"
#' @param x.min minimum x axis value
#' @param x.max maximum x axis value
#' @param y.grid switch for horizontal grid lines
#' @param file the name of the graph (+ extension) that will be saved if chosen. Null by default.
#' @param newWindow whether the plot is drawn within a new window or not
#' @return a plot
#' @export
#' 
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