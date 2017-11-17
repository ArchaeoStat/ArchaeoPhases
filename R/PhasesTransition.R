#####################################################
#                Phases Transition                  #
#####################################################

#'  Transition range between two successive phases (for phases in temporal order constraint)
#'
#' Finds if it exists the shortest interval that satisfies : P(TransitionRangeInf < Phase1Max_chain < Phase2Min_chain < TransitionRangeSup  | M) = level
#'
#' @param Phase1Max_chain numeric vector containing the output of the MCMC algorithm for the maximum of the events included in the oldest phase
#' @param Phase2Min_chain numeric vector containing the output of the MCMC algorithm for the minimum of the events included in the following phase
#' @param level probability corresponding to the level of confidence
#' @return the endpoints of the transition interval
#' @export
PhasesTransition <- function(Phase1Max_chain, Phase2Min_chain, level=0.95){
  
  result = as.matrix( PhaseTimeRange(Phase1Max_chain, Phase2Min_chain, level=level))
  rownames(result)<- c(level=level, "TransitionRangeInf", "TransitionRangeSup")
  result <- t(result)
  
  return(result[1,])
}