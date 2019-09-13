#####################################################
#          Marginal posterior Density               #
#       NEW version in ArchaeoPhases 1.4            #
#####################################################
#' Plot a marginal posterior density
#'
#' Draws a plot of the estimated marginal posterior density for the one-parameter and adds the mean
#' and the credible interval at the desired level
#'
#' @param a_chain numeric vector containing the output of the MCMC algorithm for the parameter a
#' @param level probability corresponding to the level of confidence
#' @param GridLength length of the grid used to estimate the density
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
#' @param x.scale one of "calendar" for calendar years, "BP" for years before present, or "elapsed" for time elapsed from a specified origin
#'
#' @param elapsed.origin.position position of the column to use as the origin for elapsed time calculations
#' @param y.grid switch for horizontal grid lines
#' @param file the name of the graph (+ extension) that will be saved if chosen, default = \code{NULL}
#' @param newWindow whether the plot is drawn within a new window or not
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
