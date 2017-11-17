#####################################################
#          Marginal posterior Density               #
#####################################################
#' Marginal posterior Density
#'
#' Plots the density of a_chain + statistics (mean, CI, HPDR)
#'
#' @param a_chain numeric vector containing the output of the MCMC algorithm for the parameter a
#' @param level probability corresponding to the level of confidence used for the credible interval and the highest density region
#' @param title label of the title
#' @param colors if TRUE  -> use of colors in the graph
#' @param GridLength length of the grid used to estimate the density
#' @param exportFile the name of the file to be saved. If NULL then no graph is saved. 
#' @param exportFormat the format of the export file : PNG or SVG.
#' @return a plot with the density of a_chain + +CI + mean + HDR
#' @export
MarginalPlot <- function(a_chain, level=0.95, title="Characteristics of a date", colors=TRUE, exportFile = NULL, exportFormat = "PNG", GridLength=1024){
  
  #maxValuex <- min(max(density(a_chain, n=GridLength)$x), 2016)
  maxValuex <- max(density(a_chain, n=GridLength)$x)
  minValuex <- min(density(a_chain, n=GridLength)$x)
  
  # Computing min and max values
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
  middleValuex <- minValuex + ( maxValuex - minValuex ) / 2
  P1Valuex <- minValuex + ( maxValuex - minValuex ) / 4
  P3Valuex <- middleValuex + ( maxValuex - minValuex ) / 4
  
  # y-axis
  step <- max(density(a_chain, n=GridLength)$y) /50   # used to draw CI and mean above the curve
  maxValuey <- max(density(a_chain, n=GridLength)$y)
  middleValuey <- maxValuey /2
  
  if(!is.null(exportFile)) {
    
    if(exportFormat == "PNG") {
      png( filename = paste(exportFile,"png", sep =".") )
    } 
    if(exportFormat == "SVG") {
      svg( filename = paste(exportFile,"svg", sep =".") )
    }
    
  } 
  if (colors==T){
    par(mfrow=c(1,1))
    plot(density(a_chain, n=GridLength), main = title, xlab = "Calendar Year", axes = FALSE, xlim=c(minValuex,maxValuex), ylim=c(0,max(density(a_chain, n=GridLength)$y) + step))
    # abscissa axis
    axis(1, at=c(minValuex, P1Valuex, middleValuex, P3Valuex, maxValuex) )
    # ordinate axis
    axis(2, at=c(0, middleValuey , maxValuey),labels =c(0, round(middleValuey, 3), round(maxValuey, 3)) )
    
    segments(CredibleInterval(a_chain, level)[2], 0, CredibleInterval(a_chain, level)[3], 0, lwd=6, col = 4)
    points(mean(a_chain), 0 , lwd=6, col = 2)
    
    # legend
    legend(P3Valuex, maxValuey, c("Density", "Credible Interval", "Mean"), lty=c(1, 1, 0), bty="n", pch=c(NA, NA,1), col = c("black","blue","red"), lwd=c(1,6,6), x.intersp=0.5, cex=0.9)
    
  } else {
    par(mfrow=c(1,1))
    plot(density(a_chain, n=GridLength), main = title, xlab = "Calendar Year", axes = FALSE, xlim=c(minValuex,maxValuex), ylim=c(0,max(density(a_chain, n=GridLength)$y) + step))
    # abscissa axis
    axis(1, at=c(minValuex, P1Valuex, middleValuex, P3Valuex, maxValuex) )
    # ordinate axis
    axis(2, at=c(0, middleValuey , maxValuey),labels =c(0, round(middleValuey, 3), round(maxValuey, 3)) )
    
    segments(CredibleInterval(a_chain, level)[2], 0, CredibleInterval(a_chain, level)[3], 0, lwd=6, lty=1)
    points(mean(a_chain), 0 , lwd=6, pch=1)
    
    # legend
    legend(P3Valuex, maxValuey, c("Density", "Credible Interval", "Mean"), lty=c(1,1,0), pch=c(NA,NA,1), bty="n", lwd=c(1,6,6), x.intersp=0.5, cex=0.9)
  }
  if(!is.null(exportFile)) {
    
    dev.off()
  } 
  
}