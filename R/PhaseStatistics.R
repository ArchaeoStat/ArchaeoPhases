
#####################################################
#             Statistics  for Phases               #
#####################################################

#' Summary statistics of a phase
#'
#' Estimation of summary statistics, including the beginning and end
#' of a phase, and the duration of the phase
#'
#' @details
#' The summary statistics are those given by the \code{MarginalStatistics()} function.
#' The time range is given by \code{PhaseTimeRange()} function.  The duration is computed
#' as follows: \eqn{duration = maximum - minimum} at each iteration of the MCMC output.
#'
#' @param PhaseMin_chain numeric vector containing the output of the MCMC
#' algorithm for the minimum of the dates included in the phase
#' @param PhaseMax_chain numeric vector containing the output of the MCMC
#' algorithm for the maximum of the dates included in the phase
#' @param level probability corresponding to the level of confidence used
#' for the credible interval and the highest density region
#' @return
#' A matrix of values corresponding to the summary statistics:
#' \describe{
#'   \item{1}{Statistics of the minimum of the dates included in the phase}
#'   \item{2}{Statistics of the maximum of the dates included in the phase}
#'   \item{3}{Statistics of the duration of the dates included in the phase}
#' }
#' The results are given in calendar year (in format BC/AD).
#'
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr} and
#'
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @examples
#'   data(Phases); attach(Phases)
#'   PhaseStatistics(Phase.1.alpha, Phase.1.beta, 0.95)
#'   PhaseStatistics(Phase.2.alpha, Phase.2.beta, 0.95)
#'
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
