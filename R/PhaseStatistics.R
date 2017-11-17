
#####################################################
#             Statistics  for Phases               #
#####################################################

#' Summary statistics for phases
#'
#' Estimation of all usual statistics of the beginning and the end of a phase and the duration of the phase
#'
#' @param PhaseMin_chain numeric vector containing the output of the MCMC algorithm for the minimum of the dates included in the phase
#' @param PhaseMax_chain numeric vector containing the output of the MCMC algorithm for the maximum of the dates included in the phase
#' @param level probability corresponding to the level of confidence used for the credible interval and the highest density region
#' @return A matrix of values corresponding to all the summary statistics
#' @export
PhaseStatistics <- function(PhaseMin_chain, PhaseMax_chain, level=0.95){
  
  #Statistics according to PhaseMin_chain
  MinStat = MarginalStatistics(PhaseMin_chain, level)
  
  #Statistics according to PhaseMax_chain parameter
  MaxStat = MarginalStatistics(PhaseMax_chain, level)
  
  #Statistics according to the duration 
  DurationStat = MarginalStatistics(PhaseMax_chain-PhaseMin_chain, level)
  
  # Resulted List
  
  if (length(MinStat) > length(MaxStat)) {
    
    NbDiff = length(MinStat) - length(MaxStat)
    Add = rep(NA,NbDiff) 
    MaxStat = c(MaxStat, Add)
    
  }else if (length(MinStat) < length(MaxStat)) {
    
    NbDiff = length(MaxStat) - length(MinStat)
    Add = rep(NA,NbDiff) 
    MinStat = c(MinStat, Add)
    
  }
  Mat1 = cbind(MinStat, MaxStat)
  
  if (dim(Mat1)[1] > length(DurationStat)) {
    
    NbDiff = dim(Mat1)[1] - length(DurationStat)
    Add = rep(NA,NbDiff) 
    DurationStat = c(DurationStat, Add)
    
  }else if (dim(Mat1)[1] < length(DurationStat))  {
    
    NbDiff = length(DurationStat) - dim(Mat1)[1]
    Add = cbind( rep(NA,NbDiff), rep(NA,NbDiff)) 
    Mat1 = rbind(Mat1, Add)
    
  }
  
  Mat = cbind(Mat1, DurationStat) 
  colnames(Mat) = c("Minimum", "Maximum", "Duration")
  return(Mat)
  
}