#####################################################
#         Multiple Phases Transition               #
#####################################################

#'  Transition range for a succession of groups (for groups in temporal order constraint)
#'
#' Finds if it exists the shortest interval that satisfies : P(TransitionRangeInf < Phase1Max < Phase2Min < TransitionRangeSup  | M) = level
#'
#' @param data dataframe containing the output of the MCMC algorithm 
#' @param position_minimum numeric vector containing the column number corresponding to the minimum of the events included in each group
#' @param position_maximum numeric vector containing the column number corresponding to the end of the groups set in the same order as in position_minimum
#' @param level probability corresponding to the level of confidence
#' @return the endpoints of the transition interval
#' @export


MultiPhasesTransition <- function(data, position_minimum, position_maximum = position_minimum+1, level=0.95){
  
  if (length(position_minimum)!= length(position_maximum)) {
    print('Error : the position vectors do not have the same length')
  } else {
    
    # number of phases
    L = length(position_minimum)
    
    #names
    names_min <- names(data)[position_minimum]
    names_max <- names(data)[position_maximum]
    
    # matrix of results
    result = matrix(nrow=L-1, ncol=3)
    colnames(result)<- c(level, "TransitionRangeInf", "TransitionRangeSup")
    
    phasenames <- vector(length = L-1)
    for (i in 1:L-1) { phasenames[i] = paste(names_max[i], "&", names_min[i+1]) }
    rownames(result)<- phasenames
    
    for (i in 1:(L-1)){
      result[i,] = PhaseTimeRange( data[,position_maximum[i]], data[,position_minimum[i+1]], level=level)
    }
    
    return(result)
    
  }
}