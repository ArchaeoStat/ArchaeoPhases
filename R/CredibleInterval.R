
#####################################################
#     Estimation of Credible interval        #
#####################################################

#' Bayesian credible interval
#'
#' Estimation of the shorest credible interval of the output of the MCMC algorithm for the parameter a
#'
#' @details A 100*level % credible interval is an interval that keeps N*(1-level) elements of the sample outside the interval
#' The 100*level % credible interval is the shortest of all those intervals.
#' @param a_chain numeric vector containing the output of the MCMC algorithm for the parameter a
#' @param level probability corresponding to the level of confidence used for the credible interval and the highest density region
#' @param roundingOfValue interger indicating the number of decimal places to be used
#' @return The endpoints of the shortest credible interval
#' @export
#'
CredibleInterval <- function(a_chain, level=0.95, roundingOfValue=0){
  
  sorted_sample <- sort(a_chain)     # ordering the sample
  N = length(a_chain)                # calculation of the sample size
  OutSample = N * (1-level)          # calculation of the number of data to be outside the interval
  
  I =  cbind(sorted_sample[1:(OutSample+1)] , sorted_sample[(N-OutSample):N])    #   combinasion of all credible intervals
  
  l = I[,2]-I[,1]   # length of intervals
  i <- which.min(l) # look for the shortest interval
  
  return( c( "level" = level, "CredibleIntervalInf"= round(I[i,1], digits = roundingOfValue), "CredibleIntervalSup"=round(I[i,2], digits = roundingOfValue )) ) # returns the level and the endpoints rounded
  
}