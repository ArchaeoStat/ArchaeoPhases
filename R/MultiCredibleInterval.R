

#####################################################
#         Estimation of Credible interval           #
#####################################################

#' Bayesian credible interval for a series of MCMC chains
#'
#' Estimation of the shorest credible interval of the output of the MCMC algorithm for the parameter a
#'
#' @details A 100*level % credible interval is an interval that keeps N*(1-level) elements of the sample outside the interval
#' The 100*level % credible interval is the shortest of all those intervals.
#' @param data dataframe containing the output of the MCMC algorithm 
#' @param position numeric vector containing the position of the column corresponding to the MCMC chains of interest
#' @param level probability corresponding to the level of confidence used for the credible interval
#' @param roundingOfValue interger indicating the number of decimal places to be used
#' @return The endpoints of the shortest credible interval
#' @export
#'
MultiCredibleInterval <- function(data, position, level=0.95, roundingOfValue=0){
  
  # number of chains
  L = length(position)
  
  # matrix of results for each chain
  result = matrix(nrow=L, ncol=3)
  
  colnames(result) <- c("Level","CredibleIntervalInf", "CredibleIntervalSup")
  
  # names
  rownames(result) <- names(data)[position]
  
  for (i in 1:L) {
    
    sorted_sample <- sort(data[,position[i]])     # ordering the sample
    N = length(sorted_sample)                     # calculation of the sample size of the chain
    OutSample = N * (1-level)          # calculation of the number of data to be outside the interval
    
    I =  cbind(sorted_sample[1:(OutSample+1)] , sorted_sample[(N-OutSample):N])    #   combinasion of all credible intervals
    
    l = I[,2]-I[,1]   # length of intervals
    j <- which.min(l) # look for the shortest interval
    
    result[i,] =   c(level, round(I[j,1], digits = roundingOfValue), round(I[j,2],digits = roundingOfValue) )   # returns the level and the endpoints
    
  }
  return(result)
}



