#####################################################
#           Phase marginal density plot             #
#####################################################

#' Plot the characteristics of a group of events
#'
#' This function draws the marginal posterior densities of the minimum
#' and the maximum of the events included in the phase and
#' summary statistics including mean, credible interval, and time range.
#' The result is given in calendar years (BC/AD).
#'
#' @param PhaseMin_chain numeric vector containing the output of the
#' MCMC algorithm for the minimum of the events included in the phase
#' @param PhaseMax_chain numeric vector containing the output of the
#' MCMC algorithm for the maximum of the events included in the phase
#' @param level probability corresponding to the level of confidence
#' used for the credible interval and the time range
#' @param title The title of the graph
#' @param colors if \code{TRUE}, then use of colors in the graph,
#' otherwise plot the graph in black and white
#' @param GridLength length of the grid used to estimate the density
#' @param exportFile the name of the file to be saved. If \code{NULL}, then no graph is saved
#' @param exportFormat the format of the export file, either "PNG" or "SVG"
#'
#' @return \code{NULL}, called for its side effects
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr} and
#'
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @examples
#'   data(Phases); attach(Phases)
#'   # PhasePlot(Phase.1.alpha, Phase.1.beta, level = 0.95, title = "Densities of Phase 1")
#'   PhasePlot(Phase.2.alpha, Phase.2.beta, level = 0.95, title = "Densities of Phase 2",
#'             colors = FALSE, exportFile = "CharacteristicsOfPhase", exportFormat = "SVG")
#'
#' @keywords Bayesian statistics
#' @keywords highest posterior density
#' @keywords credible interval
#' @keywords mean
#' @keywords marginal posterior density
#' @keywords individual phase
#'
#' @export
PhasePlot <- function(PhaseMin_chain, PhaseMax_chain, level=0.95, title = "Characterisation of a group of dates", colors = TRUE, exportFile = NULL, exportFormat = "PNG", GridLength=1024){

  if(length(PhaseMax_chain) != length(PhaseMin_chain)) { print('Error : the parameters do not have the same length')}   # test the length of both chains
  else{

    if( sum(ifelse(PhaseMin_chain <= PhaseMax_chain, 1, 0)) == length(PhaseMin_chain) ) {

      minValuex <- min( density(PhaseMin_chain, n=GridLength)$x)
      maxValuex <- min(max(density(PhaseMax_chain, n=GridLength)$x), 2016)

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
      maxValuey <- max ( max(density(PhaseMin_chain, n=GridLength)$y) , max(density(PhaseMax_chain, n=GridLength)$y))
      middleValuey <- maxValuey /2
      step <- maxValuey  /20

      # Options for export
      if(!is.null(exportFile)) {
        if(exportFormat == "PNG") {
          png( filename = paste(exportFile,"png", sep =".") )
        }
        if(exportFormat == "SVG") {
          svg( filename = paste(exportFile,"svg", sep =".") )
        }
      }
      if (colors==TRUE){
        # first graph
        par(las=1, mfrow=c(1,1), cex.axis=0.8)
        plot(density(PhaseMax_chain, n=GridLength), main = title, xlab="Calendar Year", axes = FALSE, ylim=c(0,maxValuey+step), xlim=c(minValuex, maxValuex), lty =1, lwd=2, col="steelblue4")
        lines(density(PhaseMin_chain, n=GridLength), lty =1, lwd=2, col ="steelblue1")

        # abscissa axis
        axis(1, at=c(minValuex, P1Valuex, middleValuex, P3Valuex, maxValuex) , labels =c(floor(minValuex), floor(P1Valuex), floor(middleValuex), floor(P3Valuex), floor(maxValuex)))
        # ordinate axis
        axis(2, at=c(0, middleValuey, maxValuey), labels =c(0, round(middleValuey, 3), round(maxValuey, 3)) )

        # segment representing the CredibleInterval of the max of the phase
        CIEnd = CredibleInterval(PhaseMax_chain, level)
        segments(CIEnd[2], 0, CIEnd[3], 0, lty = 1, lwd=6, col = "steelblue4")
        # point in red representing the mean
        points(mean(PhaseMax_chain), 0, lwd=6, col = "steelblue4")
        # segment representing the Time range of the phase in green
        PTR = PhaseTimeRange(PhaseMin_chain, PhaseMax_chain, level)
        segments(PTR[2], maxValuey+step, PTR[3], maxValuey+step, lwd=6, col = "violetred4")

        # segment representing the CredibleInterval in blue
        CIBeginning = CredibleInterval(PhaseMin_chain, level)
        segments(CIBeginning[2], step, CIBeginning[3], step, lty= 1, lwd=6, col = "steelblue1")
        # point in red representing the mean
        points(mean(PhaseMin_chain), step , lwd=6, col = "steelblue1")

        # legend
        legend(P3Valuex, maxValuey, c("Density of the Minimum", "Density of the Maximum", "with Credible Interval" ," and Mean (o)", " Phase Time Range"), lty=c(1,1,0,0,1), bty="n",col = c("steelblue1","steelblue4","black","black","violetred4"), lwd=c(2,2,6,6,6), x.intersp=0.5, cex=0.9)

      } else {

        # first graph
        par(las=1, mfrow=c(1,1), cex.axis=0.8)
        plot(density(PhaseMax_chain, n=GridLength), main = title, xlab="Calendar Year", axes = FALSE, ylim=c(0,maxValuey+step), xlim=c(minValuex, maxValuex), lty =2, lwd=2)
        lines(density(PhaseMin_chain, n=GridLength), lty =3, lwd=2)

        # abscissa axis
        axis(1, at=c(minValuex, P1Valuex, middleValuex, P3Valuex, maxValuex) , labels =c(floor(minValuex), floor(P1Valuex), floor(middleValuex), floor(P3Valuex), floor(maxValuex)))
        # ordinate axis
        axis(2, at=c(0, middleValuey, maxValuey), labels =c(0, round(middleValuey, 3), round(maxValuey, 3)) )

        # segment representing the CredibleInterval in blue
        CIEnd = CredibleInterval(PhaseMax_chain, level)
        segments(CIEnd[2], 0, CIEnd[3], 0, lty = 2, lwd=6, col = 1)
        # point in red representing the mean
        points(mean(PhaseMax_chain), 0, lwd=6, col = 1)
        # segment representing the Time range of the phase in green
        PTR = PhaseTimeRange(PhaseMin_chain, PhaseMax_chain, level)
        segments(PTR[2], maxValuey+step, PTR[3], maxValuey+step, lwd=6, col = 1)

        # segment representing the CredibleInterval in blue
        CIBeginning = CredibleInterval(PhaseMin_chain, level)
        segments(CIBeginning[2], step, CIBeginning[3], step, lty= 3, lwd=6, col = 1)
        # point in red representing the mean
        points(mean(PhaseMin_chain), step , lwd=6, col = 1)

        # legend
        legend(P3Valuex, maxValuey, c("Density of the Minimum", "Density of the Maximum", "with Credible Interval", "and Mean (o)", " Phase Time Range"), lty=c(3,2,0,0,1), bty="n",col = c(1,1,1,1,1), lwd=c(2,2,6,6,6), x.intersp=0.5, cex=0.9)

      }

      if(!is.null(exportFile)) {
        dev.off()
      }

    } else {
      print('Error : PhaseMin_chain should be older than PhaseMax_chain')
    }

  } # end if(length(PhaseMax_chain) != length(PhaseMin_chain))

}
