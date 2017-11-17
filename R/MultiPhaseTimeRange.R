#####################################################
#         Multiple Phase Time Range                 #
#####################################################

#' Phase Time Range for multiple groups
#'
#' Computes the shortest interval that satisfies : P(PhaseMin < IntervalInf < IntervalSup < PhaseMax | M) = level for each phase
#'
#' @param data dataframe containing the output of the MCMC algorithm 
#' @param position_minimum numeric vector containing the column number corresponding to the minimum of the events included in each phase
#' @param position_maximum numeric vector containing the column number corresponding to the maximum of the phases set in the same order as in position_minimum
#' @param level probability corresponding to the desired level of confidence
#' @return The endpoints of the shortest time range associated with the desired level
#' @export


MultiPhaseTimeRange <- function(data, position_minimum, position_maximum=position_minimum+1, level=0.95){
  
  if (length(position_minimum)!= length(position_maximum)) {
    print('Error : the position vectors do not have the same length')
  } else {
    
    # number of phases
    L = length(position_minimum)
    
    # names
    names_beginning <- names(data)[position_minimum]
    names_end <- names(data)[position_maximum]
    
    # matrix of results
    result = matrix(nrow=L, ncol=3)
    colnames(result)<- c("Level","TimeRangeInf", "TimeRangeSup")
    
    phasenames <- vector(length = L)
    for (i in 1:L) { phasenames[i] = paste(names_beginning[i], names_end[i] ) }
    rownames(result)<- phasenames
    
    for (i in 1:L){
      result[i,] = PhaseTimeRange( data[,position_minimum[i]], data[,position_maximum[i]], level=level)
    }
    
    return(result)
    
    
  }
}