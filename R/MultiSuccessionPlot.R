#####################################################
#        Multiple Phases Density Plots              #
#####################################################

#' Successive Phases Density Plots (for phases in temporal order constraint)
#'
#' This functions draws a plot of the densities of several successive phases
#' and adds several statistics (mean, CI, HPDR). The result is given in
#' calendar years (BC/AD).
#'
#' @param data Data frame containing the output of the MCMC algorithm.
#' @param position_minimum Numeric vector containing the column number
#' corresponding to the minimum of the events included in each group.
#' @param position_maximum Numeric vector containing the column number
#' corresponding to the end of the groups set in the same order as
#' in \code{position_minimum}.
#' @param level Probability corresponding to the level of confidence.
#' @param title Title of the plot.
#' @param colors Vector of colors corresponding to each group of dates.
#' @param exportFile Name of the file to be saved.
#' If \code{NULL} then no plot is saved.
#' @param exportFormat Format of the export file, either "PNG"
#' or "SVG" (default).
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr} and
#'
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @details
#' Curves represent the density of the minimum (oldest dates) and the maximum
#' (youngest dates) of the dates included in each group. Curves of the same
#' color refer to the same phase. When there is only one curve of one color,
#' it means that there is only one event in the corresponding group and then
#' the minimum equals the maximum. Time range intervals are symbolised by
#' segments above the curves drawn using the same color as the one of the curves
#' of the associated group. Transition and gap range intervals are represented
#' by two-coloured segments using the colors of successive phases. If the gap
#' between the successive groups does not exist, a cross is drawn instead of a segment.
#' @return \code{NULL}, called for its side effects
#' @examples
#'   # Data extracted from ChronoModel software
#'   data(Phases)
#'   # List of the name of the groups
#'   names(Phases)
#'   # Stipulating position_end
#'   MultiSuccessionPlot(Phases, c(4, 2), c(5, 3), title = "Succession of phase 1 and phase 2")
#'   # In this case, equivalent to
#'   MultiSuccessionPlot(Phases, c(4, 2), title = "Succession of phase 1 and phase 2", colors = c(3, 4))
#'   # export
#'   MultiSuccessionPlot(Phases, c(4, 2), exportFile = "MultiSuccessionPlot", exportFormat = "SVG")
#'
#' @importFrom stats density
#' @importFrom grDevices png rainbow svg dev.off
#' @importFrom graphics par plot lines axis segments text points
#'
#' @export
MultiSuccessionPlot <- function(data, position_minimum, position_maximum = position_minimum+1, level=0.95, title = "Characterisation of a succession of groups",
                                colors = NULL, exportFile = NULL, exportFormat = "PNG"){

  if (length(position_minimum)!= length(position_maximum)) {
    print('Error : the position vectors do not have the same length')
  } else {

    # construction of the new dataset
    L = length(position_minimum)

    GridLength=1024
    phase = matrix(ncol = L*2, nrow=nrow(data))
    densityX = matrix(ncol = L*2, nrow=GridLength)
    densityY = matrix(ncol = L*2, nrow=GridLength)

    for (i in 1:L) {
      phase[,2*i-1] = data[,position_minimum[i]]
      phase[,2*i] = data[,position_maximum[i]]

      densityX[,2*i-1] = density(data[,position_minimum[i]], n = GridLength)$x
      densityX[,2*i] = density(data[,position_maximum[i]], n = GridLength)$x

      densityY[,2*i-1] = density(data[,position_minimum[i]], n = GridLength)$y
      densityY[,2*i] = density(data[,position_maximum[i]], n = GridLength)$y
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
    # x-axis values
    middleValuex <- ( maxValuex + minValuex) / 2
    P1Valuex <- minValuex + ( maxValuex - minValuex ) / 4
    P3Valuex <- middleValuex + ( maxValuex - minValuex ) / 4
    # y-axis values
    maxValuey <- max( apply(densityY,2,max))
    middleValuey <- maxValuey /2
    minValuey <- min( apply(densityY,2,min))
    #
    haut = seq(minValuey,maxValuey,length.out=(2*L+2) )

    # Options for colors
    if (is.null(colors)) {
      pal = rainbow(L)
    } else {
      pal = colors
    }

    ## Graph
    par(las=1, mfrow=c(1,1), cex.axis=0.8)
    plot(density(phase[,1], n=GridLength), main = title, ylab="Density", xlab = "Calendar Year", ylim=c(0,2.5*maxValuey), xlim=c(minValuex, maxValuex), bty='n',lty =1, lwd=2, axes=FALSE, col = pal[1])
    lines(density(phase[,2], n=GridLength), lty =1, lwd=2, col = pal[1])

    # abscissa axis
    axis(1, at=c(minValuex, P1Valuex, middleValuex, P3Valuex, maxValuex) , labels =c(floor( minValuex), floor( P1Valuex), floor( middleValuex), floor( P3Valuex), floor( maxValuex)))
    # ordinate axis
    axis(2, at=c(0, middleValuey, maxValuey), labels =c(0, round(middleValuey, 5), round(maxValuey, 5)) )


    ## Following phases
    for(i in 2:L) {
      lines(density(phase[,2*i-1]), col=pal[i], lwd=2, lty=1)
      lines(density(phase[,2*i]), col=pal[i], lwd=2, lty=1)
    }

    ## Phase Time Range
    MPTR = MultiPhaseTimeRange(data=data, position_minimum=position_minimum, position_maximum=position_maximum, level=level)
    for (i in 1:(L) ) { segments(MPTR[i,2], maxValuey + haut[i+1], MPTR[i,3], maxValuey + haut[i+1], lwd=6,col=pal[i]) }
    text(minValuex, maxValuey + haut[trunc((L+1)/2)] , "Time range",srt =90)

    ## Phase Transition / Gap
    PTrans = MultiPhasesTransition(data=data, position_minimum=position_minimum, position_maximum=position_maximum, level=level)
    PGap = MultiPhasesGap(data=data, position_minimum=position_minimum, position_maximum=position_maximum, level=level)

    # Separateur
    segments(minValuex, maxValuey+haut[L+2], maxValuex, maxValuey+haut[L+2], lwd=.2)
    text(minValuex, maxValuey+haut[2*L+1], "Transition ",srt =90)

    segments(minValuex, 2*maxValuey+haut[1], maxValuex, 2*maxValuey+haut[1], lwd=.2)
    text(minValuex, 2*maxValuey+haut[trunc((L+2)/2)]," Gap",srt =90)


    for (i in 1:(L-1) ) {
      segments(PTrans[i,2], maxValuey+haut[L+i+2],PTrans[i,3], maxValuey+haut[L+i+2],lwd=6, col = pal[i])
      segments(PTrans[i,2], maxValuey+haut[L+i+2],PTrans[i,3], maxValuey+haut[L+i+2],lwd=6, col = pal[i+1], lty=4)

      if (PGap[i,2] == "NA" || PGap[i,3] == "NA") {
        points( (PTrans[i,3]+PTrans[i,2])/2, 2*maxValuey+haut[i+1], lwd=2, col = pal[i], pch=4)

      } else {
        segments(as.numeric(PGap[i,2]), 2*maxValuey+haut[i+1], as.numeric(PGap[i,3]), 2*maxValuey+haut[i+1], lwd=6, col = pal[i])
        segments(as.numeric(PGap[i,2]), 2*maxValuey+haut[i+1], as.numeric(PGap[i,3]), 2*maxValuey+haut[i+1], lwd=6, col = pal[i+1], lty=4)
      }

    }

    # Options for export
    if(!is.null(exportFile)) {

      if(exportFormat == "PNG") {
        png( filename = paste(exportFile,"png", sep =".") )
      }
      if(exportFormat == "SVG") {
        svg( filename = paste(exportFile,"svg", sep =".") )
      }

      par(las=1, mfrow=c(1,1), cex.axis=0.8)
      plot(density(phase[,1], n=GridLength), main = title, ylab="Density", xlab = "Calendar Year", ylim=c(0,2.5*maxValuey), xlim=c(minValuex, maxValuex), bty='n',lty =1, lwd=2, axes=FALSE, col = pal[1])
      lines(density(phase[,2], n=GridLength), lty =1, lwd=2, col = pal[1])

      # abscissa axis
      axis(1, at=c(minValuex, P1Valuex, middleValuex, P3Valuex, maxValuex) , labels =c(floor( minValuex), floor( P1Valuex), floor( middleValuex), floor( P3Valuex), floor( maxValuex)))
      # ordinate axis
      axis(2, at=c(0, middleValuey, maxValuey), labels =c(0, round(middleValuey, 5), round(maxValuey, 5)) )


      ## Following phases
      for(i in 2:L) {
        lines(density(phase[,2*i-1]), col=pal[i], lwd=2, lty=1)
        lines(density(phase[,2*i]), col=pal[i], lwd=2, lty=1)
      }

      ## Phase Time Range
      MPTR = MultiPhaseTimeRange(data=data, position_minimum=position_minimum, position_maximum=position_maximum, level=level)
      for (i in 1:(L) ) { segments(MPTR[i,2], maxValuey + haut[i+1], MPTR[i,3], maxValuey + haut[i+1], lwd=6,col=pal[i]) }
      text(minValuex, maxValuey + haut[trunc((L+1)/2)] , "Time range",srt =90)

      ## Phase Transition / Gap
      PTrans = MultiPhasesTransition(data=data, position_minimum=position_minimum, position_maximum=position_maximum, level=level)
      PGap = MultiPhasesGap(data=data, position_minimum=position_minimum, position_maximum=position_maximum, level=level)

      # Separateur
      segments(minValuex, maxValuey+haut[L+2], maxValuex, maxValuey+haut[L+2], lwd=.2)
      text(minValuex, maxValuey+haut[2*L+1], "Transition ",srt =90)

      segments(minValuex, 2*maxValuey+haut[1], maxValuex, 2*maxValuey+haut[1], lwd=.2)
      text(minValuex, 2*maxValuey+haut[trunc((L+2)/2)]," Gap",srt =90)


      for (i in 1:(L-1) ) {
        segments(PTrans[i,2], maxValuey+haut[L+i+2],PTrans[i,3], maxValuey+haut[L+i+2],lwd=6, col = pal[i])
        segments(PTrans[i,2], maxValuey+haut[L+i+2],PTrans[i,3], maxValuey+haut[L+i+2],lwd=6, col = pal[i+1], lty=4)

        if (PGap[i,2] == "NA" || PGap[i,3] == "NA") {
          points( (PTrans[i,3]+PTrans[i,2])/2, 2*maxValuey+haut[i+1], lwd=2, col = pal[i], pch=4)

        } else {
          segments(as.numeric(PGap[i,2]), 2*maxValuey+haut[i+1], as.numeric(PGap[i,3]), 2*maxValuey+haut[i+1], lwd=6, col = pal[i])
          segments(as.numeric(PGap[i,2]), 2*maxValuey+haut[i+1], as.numeric(PGap[i,3]), 2*maxValuey+haut[i+1], lwd=6, col = pal[i+1], lty=4)
        }

      }

      dev.off()
    }

  }
}
