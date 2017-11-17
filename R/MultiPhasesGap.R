#####################################################
#       Hiatus between a succession of  groups      #
#####################################################

#'  Gap/Hiatus between a succession of groups (for groups in temporal order constraint)
#'
#' Finds if it exists a gap between two groups that is the longest interval that satisfies : P(Phase1Max < IntervalInf < IntervalSup < Phase2Min | M) = level
#'
#' @param data dataframe containing the output of the MCMC algorithm 
#' @param position_minimum numeric vector containing the column number corresponding to the minimum of the events included in each group
#' @param position_maximum numeric vector containing the column number corresponding to the end of the phases set in the same order as in position_minimum
#' @param level probability corresponding to the level of confidence
#' @return The endpoints of the longest gap
#' @export

MultiPhasesGap <- function(data, position_minimum, position_maximum = position_minimum+1, level=0.95){
  
  if (length(position_minimum)!= length(position_maximum)) {
    print('Error : the position vectors do not have the same length')
  } else {
    
    # number of phases
    L = length(position_minimum)
    
    #names
    names_Min <- names(data)[position_minimum]
    names_Max <- names(data)[position_maximum]
    
    # matrix of results
    result = matrix(nrow=L-1, ncol=3)
    colnames(result)<- c("Level","HiatusIntervalInf", "HiatusIntervalSup")
    
    phasenames <- vector(length = (L-1))
    for (i in 1:L-1) { phasenames[i] = paste(names_Max[i], "&", names_Min[i+1]) }
    rownames(result)<- phasenames
    
    for (i in 1:(L-1)){
      result[i,] = PhasesGap( data[,position_maximum[i]], data[,position_minimum[i+1]], level=level)
    }
    
    return(result)
    
  }
}