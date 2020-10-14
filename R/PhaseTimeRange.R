#####################################################
#                 Phase Time Range                 #
#####################################################

#' Phase time range
#'
#' Computes the shortest interval  [IntervalInf ; IntervalSup] that satisfies
#' \eqn{P(PhaseMin_chain =< IntervalInf < IntervalSup =< PhaseMax_chain | M) = level}
#'
#' @param PhaseMin_chain : Numeric vector containing the output of the MCMC
#' algorithm for the minimum of the events included in the phase.
#' @param PhaseMax_chain : Numeric vector containing the output of the MCMC
#' algorithm for the maximum of the events included in the phase.
#' @param level Probability corresponding to the desired level of confidence.
#' @param round_to Integer indicating the number of decimal places.

#' @return A vector of values containing the desired level of confidence
#' and the endpoints of the shortest time range associated with this desired level.
#' The result is given in calendar years (BC/AD).
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr} and
#'
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @examples
#'   data(Phases); attach(Phases)
#'   PhaseTimeRange(Phase.1.alpha, Phase.1.beta, 0.95)
#'   PhaseTimeRange(Phase.2.alpha, Phase.2.beta, 0.90)
#'
#' @importFrom stats quantile
#'
#' @export
PhaseTimeRange <- function(PhaseMin_chain, PhaseMax_chain, level=0.95,round_to=O){

  if(length(PhaseMax_chain) != length(PhaseMin_chain)) { print('Error : the parameters do not have the same length')}   # test the length of both chains
  else{

    if( sum(ifelse(PhaseMin_chain <= PhaseMax_chain, 1, 0)) != length(PhaseMin_chain) )  {  # test for Beginning < End
      print('Error : PhaseMin_chain should be older than PhaseMax_chain')
    } else {

      periode <- function(epsilon, PMin, PMax, level){
        q1 = quantile(PMin, probs = epsilon)    # Computes the 'level'th quantile of the minimum of the events included in the phase
        indz = (PMin > q1)
        q2 = quantile(PMax[indz], probs= (level/(1-epsilon)))
        c(q1,q2)
      }   # end periode <- function(epsilon, PMin, PMax, level){
      per = Vectorize(periode,"epsilon")

      epsilon = seq(0,1-level,.001)       # sequence of values used to compute
      p = per(epsilon,PhaseMin_chain, PhaseMax_chain, level)
      rownames(p)<- c("TimeRangeInf", "TimeRangeSup")

      D<- p[2,]-p[1,]     # computes the length of all intervals
      I = which.min(D)    # finds the shortest interval
      range = round(p[,I], round_to)
      c(level=level, range[1], range[2]) # returns the endpoints of the shortest interval

    }
    # end if( sum(ifelse(PhaseMin_chain < PhaseMin_chain, 1, 0) == length(PhaseMin_chain) ) ) {  # test for Beginning < End

  }# end if(length(PhaseMin_chain) != length(PhaseMin_chain)) {

} # end PhaseTimeRange <- function(PhaseMin_chain, PhaseMax_chain, level, plot = F){
