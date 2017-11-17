#####################################################
#             Succession Plot                  #
#####################################################

#' Density Plots of two successive phases (for phases in temporal order constraint)
#'
#' Plot of the densities of two successive phases + statistics (mean, CI, HPDR)
#'
#' @param Phase1Min_chain numeric vector containing the output of the MCMC algorithm for the minimum of the events included in the oldest phase
#' @param Phase1Max_chain numeric vector containing the output of the MCMC algorithm for the maximum of the events included in the oldest phase
#' @param Phase2Min_chain numeric vector containing the output of the MCMC algorithm for the minimum of the events included in the youngest phase
#' @param Phase2Max_chain numeric vector containing the output of the MCMC algorithm for the maximum of the events included in the youngest phase
#' @param level probability corresponding to the level of confidence
#' @param title title of the graph
#' @param exportFile the name of the file to be saved. If NULL then no graph is saved. 
#' @param exportFormat the format of the export file : PNG or SVG.
#' @param GridLength length of the grid used to estimate the density
#' @return a plot of all densities + CI + mean + HDR
#' @export

SuccessionPlot <- function(Phase1Min_chain, Phase1Max_chain, Phase2Min_chain, Phase2Max_chain, level=0.95,  title = "Characterisation of a succession of groups", 
                           exportFile = NULL, exportFormat = "PNG", GridLength=1024){
  
  
  if(length(Phase1Max_chain) != length(Phase2Min_chain)) { stop('Error : the parameters do not have the same length')} # test for the length of both chains
  else{
    
    if( sum(ifelse(Phase1Min_chain <= Phase1Max_chain, 1, 0)) != length(Phase1Min_chain) ||  sum(ifelse(Phase2Min_chain <= Phase2Max_chain, 1, 0)) != length(Phase1Min_chain) || sum(ifelse( Phase1Max_chain <= Phase2Min_chain, 1, 0)) != length(Phase1Min_chain) ) {
      # test for PhaseMin_chain < PhaseMax_chain and Phase1 < Phase2
      stop('Error : PhaseMin_chain should be older than PhaseMax_chain')
    } else {
      
      minValuex <- min(density(Phase1Min_chain, n=GridLength)$x, density(Phase2Min_chain, n=GridLength)$x)
      maxValuex <- min( max(density(Phase1Max_chain, n=GridLength)$x, density(Phase2Max_chain, n=GridLength)$x), 2016)
      
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
      maxValuey <- max ( max(density(Phase1Min_chain, n=GridLength)$y) , max(density(Phase1Max_chain, n=GridLength)$y), max(density(Phase2Min_chain, n=GridLength)$y) , max(density(Phase2Max_chain, n=GridLength)$y))
      middleValuey <- maxValuey /2
      minValuey <- min ( min(density(Phase1Min_chain, n=GridLength)$y) , min(density(Phase1Max_chain, n=GridLength)$y), min(density(Phase2Min_chain, n=GridLength)$y) , min(density(Phase2Max_chain, n=GridLength)$y))
      
      haut = seq(minValuey,maxValuey,length.out=5)
      middleA <- maxValuey+ (haut[1] + haut[2]) / 2
      
      # Options for export
      if(!is.null(exportFile)) {
        
        if(exportFormat == "PNG") {
          png( filename = paste(exportFile,"png", sep =".") )
        } 
        if(exportFormat == "SVG") {
          svg( filename = paste(exportFile,"svg", sep =".") )
        }
        
      } 
      
      par(las=1, mfrow=c(1,1), cex.axis=0.8)
      plot(density(Phase1Max_chain, n=GridLength), main = title, ylab="Density", xlab = "Calendar Year", ylim=c(0,maxValuey+maxValuey), xlim=c(minValuex, maxValuex), bty='n',lty =1, lwd=2, axes=FALSE, col = "steelblue")
      lines(density(Phase1Min_chain, n=GridLength), lty =1, lwd=2, col = "steelblue")
      
      # abscissa axis
      axis(1, at=c(minValuex, P1Valuex, middleValuex, P3Valuex, maxValuex) , labels =c(floor( minValuex), floor( P1Valuex), floor( middleValuex), floor( P3Valuex), floor( maxValuex)))
      # ordinate axis
      axis(2, at=c(0, middleValuey, maxValuey), labels =c(0, round(middleValuey, 3), round(maxValuey, 3)) )
      
      ## Phase2
      lines(density(Phase2Min_chain, n=GridLength), lty =1, lwd=2, col ="violet")
      lines(density(Phase2Max_chain, n=GridLength), lty =1, lwd=2, col ="violet")
      
      ## Phase Time Range
      PTR1 = PhaseTimeRange(Phase1Min_chain, Phase1Max_chain, level=level)
      PTR2 = PhaseTimeRange(Phase2Min_chain, Phase2Max_chain, level=level)
      segments(PTR1[2],maxValuey+haut[2],PTR1[3],maxValuey+haut[2],lwd=6,col="steelblue")
      segments(PTR2[2],maxValuey+haut[3],PTR2[3],maxValuey+haut[3],lwd=6, col ="violet")
      text(minValuex, middleA,"Time range",srt =90)
      
      ## Phase Transition
      PTrans = PhasesTransition(Phase1Max_chain, Phase2Min_chain, level=level)
      segments(PTrans[2],maxValuey+haut[4],PTrans[3],maxValuey+haut[4],lwd=6, col = "steelblue")
      segments(PTrans[2],maxValuey+haut[4],PTrans[3],maxValuey+haut[4],lwd=6, col = "violet", lty=4)
      
      PGap = PhasesGap(Phase1Max_chain, Phase2Min_chain, level=level)
      if (PGap[2] == "NA" || PGap[3] == "NA") {
        points( (PTrans[3]+PTrans[2])/2, maxValuey+haut[5], lwd=2, col = "steelblue", pch=4)
      } else {
        segments(PGap[2],maxValuey+haut[5],PGap[3],maxValuey+haut[5],lwd=6, col = "steelblue")
        segments(PGap[2],maxValuey+haut[5],PGap[3],maxValuey+haut[5],lwd=6, col = "violet", lty=4)
      }
      
      text(minValuex, maxValuey+haut[4],"Transition",srt =90)
      text(minValuex, maxValuey+haut[5],"Gap",srt =90)
      
      # options for export
      if(!is.null(exportFile)) {
        dev.off()
      } 
      
    }
    
  }
  
}