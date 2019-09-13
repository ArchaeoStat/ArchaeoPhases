#####################################################
#           Multiple Phase Plot  #
#####################################################

#' Several Phases Density Plots
#'
#' Plot of the marginal posterior densities of several groups
#'
#' Draws a plot with the marginal posterior densities of the minimum
#' and the maximum of the dates included in each group. No temporal
#' order between phases is required. The result is given in calendar
#' years (BC/AD).
#'
#' @param data data frame containing the output of the MCMC algorithm
#' @param position_minimum numeric vector containing the column number corresponding to the minimum of the events included in each group
#' @param position_maximum numeric vector containing the column number corresponding to the end of the groups set in the same order as in position_minimum
#' @param level probability corresponding to the level of confidence
#' @param title title of the graph
#' @param colors numeric vector of colors for each group of dates
#' @param exportFile the name of the file to be saved. If \code{NULL} then no graph is saved.
#' @param exportFormat the format of the export file, one of "PNG" or "SVG."
#' @return \code{NULL}, called for its side effects
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr} and
#'
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @examples
#'   # Data extracted from ChronoModel software
#'   data(Phases)
#'   # List of the name of the groups
#'   names(Phases)
#'   # Stipulating position_maximum
#'   MultiPhasePlot(Phases, c(4,2), c(5,3), title = "Succession of phase 1 and phase 2")
#'   # In this case, equivalent to
#'   MultiPhasePlot(Phases, c(4,2), title = "Succession of phase 1 and phase 2", colors = c(3,4))
#'   # Export
#'   MultiPhasePlot(Phases, c(4,2), exportFile = "MultiPhasePlot", exportFormat = "PNG")
#' @export
MultiPhasePlot <- function(data, position_minimum, position_maximum = position_minimum+1, level=0.95,
                           title = "Characterisation of several groups",
                           colors = NULL, exportFile = NULL, exportFormat = "PNG"){

  if (length(position_minimum)!= length(position_maximum)) {
    print('Error : the position vectors do not have the same length')
  } else {

    # construction of the new dataset
    L = length(position_minimum)

    phase = matrix(ncol = L*2, nrow=nrow(data))
    GridLength = 1024
    densityX = matrix(ncol = L*2, nrow=GridLength)
    densityY = matrix(ncol = L*2, nrow=GridLength)

    for (i in 1:L) {
      phase[,2*i-1] = data[,position_minimum[i]]
      phase[,2*i] = data[,position_maximum[i]]

      densityX[,2*i-1] = density(data[,position_minimum[i]], n=1024)$x
      densityX[,2*i] = density(data[,position_maximum[i]], n=1024)$x

      densityY[,2*i-1] = density(data[,position_minimum[i]], n=1024)$y
      densityY[,2*i] = density(data[,position_maximum[i]], n=1024)$y
    }

    minValuex <- min (apply(densityX,2,min))
    maxValuex <- min(max( apply(densityX,2,max) ), 2016)

    # rounding up x and y values
    x = 10^c(0:10)
    if(minValuex!=0){
      c =0
      for(i in 1:length(x)) { if( abs(minValuex/x[i])>1) {c=c+1}}
      if(c>3){ minValuex = floor(minValuex/x[c-1])*x[c-1]} else {minValuex = floor(minValuex/x[c])*x[c]}
    }
    if(maxValuex!=0){
      d=0
      for(i in 1:length(x)) { if( abs(maxValuex/x[i])>1) {d=d+1}}
      if(d>3){ maxValuex = ceiling(maxValuex/x[d-1])*x[d-1]} else {maxValuex = ceiling(maxValuex/x[d])*x[d]}
    }

    # x-axis
    middleValuex <- ( maxValuex + minValuex) / 2
    P1Valuex <- minValuex + ( maxValuex - minValuex ) / 4
    P3Valuex <- middleValuex + ( maxValuex - minValuex ) / 4
    # y-axis
    maxValuey <- max( apply(densityY,2,max))
    middleValuey <- maxValuey /2
    minValuey <- min( apply(densityY,2,min))

    haut = seq(minValuey,middleValuey,length.out=(L+1) )

    # Options for colors
    if (is.null(colors)) {
      pal = rainbow(L)
    } else {
      pal = colors
    }


    # Graph
    par(las=1, mfrow=c(1,1), cex.axis=0.8)
    plot(density(phase[,1], n=GridLength), main = title, ylab="Density", xlab = "Calendar Year", ylim=c(0,maxValuey+middleValuey), xlim=c(minValuex, maxValuex), bty='n',lty =1, lwd=2, axes=FALSE, col = pal[1])
    lines(density(phase[,2], n=GridLength), lty =1, lwd=2, col = pal[1])

    # abscissa axis
    axis(1, at=c(minValuex, P1Valuex, middleValuex, P3Valuex, maxValuex) , labels =c(floor( minValuex), floor( P1Valuex), floor( middleValuex), floor( P3Valuex), floor( maxValuex)))
    # ordinate axis
    axis(2, at=c(0, middleValuey, maxValuey), labels =c(0, round(middleValuey, 5), round(maxValuey, 5)) )

    ## Following phases
    for(i in 2:L) {
      lines(density(phase[,2*i-1], n = GridLength), col=pal[i], lwd=2, lty=1)
      lines(density(phase[,2*i], n = GridLength), col=pal[i], lwd=2, lty=1)
    }

    # ## Phase Time Range
    MPTR = MultiPhaseTimeRange(data=data, position_minimum=position_minimum, position_maximum=position_maximum, level=level)
    for (i in 1:L ) { segments(MPTR[i,2], maxValuey+haut[i+1],MPTR[i,3], maxValuey+haut[i+1],lwd=6,col=pal[i]) }
    text(minValuex, maxValuey+haut[2], "Time range", srt =90)

    # Options for export
    if(!is.null(exportFile)) {

      if(exportFormat == "PNG") {
        png( filename = paste(exportFile,"png", sep =".") )
      }
      if(exportFormat == "SVG") {
        svg( filename = paste(exportFile,"svg", sep =".") )
      }

      # Graph
      par(las=1, mfrow=c(1,1), cex.axis=0.8)
      plot(density(phase[,1], n=GridLength), main = title, ylab="Density", xlab = "Calendar Year", ylim=c(0,maxValuey+middleValuey), xlim=c(minValuex, maxValuex), bty='n',lty =1, lwd=2, axes=FALSE, col = pal[1])
      lines(density(phase[,2], n=GridLength), lty =1, lwd=2, col = pal[1])

      # abscissa axis
      axis(1, at=c(minValuex, P1Valuex, middleValuex, P3Valuex, maxValuex) , labels =c(floor( minValuex), floor( P1Valuex), floor( middleValuex), floor( P3Valuex), floor( maxValuex)))
      # ordinate axis
      axis(2, at=c(0, middleValuey, maxValuey), labels =c(0, round(middleValuey, 5), round(maxValuey, 5)) )

      ## Following phases
      for(i in 2:L) {
        lines(density(phase[,2*i-1], n = GridLength), col=pal[i], lwd=2, lty=1)
        lines(density(phase[,2*i], n = GridLength), col=pal[i], lwd=2, lty=1)
      }

      # ## Phase Time Range
      MPTR = MultiPhaseTimeRange(data=data, position_minimum=position_minimum, position_maximum=position_maximum, level=level)
      for (i in 1:L ) { segments(MPTR[i,2], maxValuey+haut[i+1],MPTR[i,3], maxValuey+haut[i+1],lwd=6,col=pal[i]) }
      text(minValuex, maxValuey+haut[2], "Time range", srt =90)

      dev.off()
    }   # end option export

  }
}
