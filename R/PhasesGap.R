#####################################################
#          Hiatus between two phases             #
#####################################################
#'  Gap or hiatus between two successive phases (for phases in temporal order constraint)
#'
#' This function finds, if it exists, a gap or hiatus between two successive phases.
#' This gap or hiatus is the longest interval that satisfies
#' \eqn{P(Phase1Max_chain < IntervalInf < IntervalSup < Phase2Min_chain | M) = level}
#'
#' @param Phase1Max_chain Numeric vector containing the output of the MCMC
#' algorithm for the maximum of the events included in the oldest phase.
#' @param Phase2Min_chain Numeric vector containing the output of the MCMC
#' algorithm for the minimum of the events included in the following phase.
#' @param level Probability corresponding to the level of confidence.
#'
#' @return Returns a vector of values containing the level of confidence and
#' the endpoints of the gap between the successive phases. The result is
#' given in calendar years (BC/AD).
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr} and
#'
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @examples
#'   data(Phases); attach(Phases)
#'   PhasesGap(Phase.1.beta, Phase.2.alpha, 0.95)
#'   PhasesGap(Phase.1.beta, Phase.2.alpha, 0.50)
#'
#' @export
PhasesGap <- function(Phase1Max_chain, Phase2Min_chain, level=0.95){

  if(length(Phase1Max_chain) != length(Phase2Min_chain)) { stop('Error : the parameters do not have the same length')} # test for the length of both chains
  else{

    if( sum(ifelse(Phase1Max_chain <=Phase2Min_chain, 1, 0)) != length(Phase2Min_chain) )  {  # test for Phase1Max_chain < Phase2Min_chain
      stop('Error : Phase1Max_chain should be older than Phase2Min_chain')
    } else {

      interval <- function(epsilon, P1Max, P2Min, level)
      {
        q1 = quantile(P1Max ,probs = 1-epsilon) ;
        indz = (P1Max < q1)
        q2 = quantile(P2Min[indz],probs= (1-level-epsilon)/(1-epsilon))
        c(q1,q2)
      }
      hia = Vectorize(interval,"epsilon")

      epsilon = seq(0,1-level,.001)
      p = hia(epsilon, Phase1Max_chain, Phase2Min_chain, level)
      rownames(p)<- c("HiatusIntervalInf", "HiatusIntervalSup")

      D<- p[2,]-p[1,]
      DD = D[D>0]

      if (length(DD) > 0){
        I = which(D==max(DD))
        interval2 = round( p[,I], 0)
        if (p[2,I] != p[1,I]) {
          c(level=level, interval2[1], interval2[2])
        } else {
          c(level=level, HiatusIntervalInf='NA',HiatusIntervalSup='NA')
        }#end if (p[2,I] != p[1,I])

      } else {
        c(level=level, HiatusIntervalInf='NA',HiatusIntervalSup='NA')
      }#end if (length(DD) > 0)

    } # end if( sum(ifelse(Phase2Min_chain < Phase1Max_chain, 1, 0) == length(Phase2Min_chain) ) ) {  # test for Phase1Max_chain < Phase2Min_chain

  } # if(length(Phase1Max_chain) != length(Phase2Min_chain)) {

}
