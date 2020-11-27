
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
#' @param PhaseMin_chain Numeric vector containing the output of the MCMC
#' algorithm for the minimum of the dates included in the phase.
#' @param PhaseMax_chain Numeric vector containing the output of the MCMC
#' algorithm for the maximum of the dates included in the phase.
#' @param level Probability corresponding to the level of confidence used
#' for the credible interval and the highest density region.
#' @param roundingOfValue Integer indicating the number of decimal places.

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
PhaseStatistics <- function(PhaseMin_chain, PhaseMax_chain, level=0.95, roundingOfValue = 0){

  #Statistics according to PhaseMin_chain
  MinStat = MarginalStatistics(PhaseMin_chain, level,roundingOfValue)

  #Statistics according to PhaseMax_chain parameter
  MaxStat = MarginalStatistics(PhaseMax_chain, level,roundingOfValue)

  #Statistics according to the duration
  DurationStat = MarginalStatistics(PhaseMax_chain-PhaseMin_chain, level,roundingOfValue)

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

#' Summary statistics of a phase
#'
#' Estimation of summary statistics for the beginning, end,
#' and duration of a phase.
#'
#' @details
#' The summary statistics are those given by the \code{MarginalStatistics()} function.
#' The time range is given by \code{PhaseTimeRange()} function.  The duration is computed
#' as follows: \eqn{duration = maximum - minimum} at each iteration of the MCMC output.
#'
#' @param min_chain Numeric vector containing the output of the MCMC
#' algorithm for the start of the phase.
#' @param max_chain Numeric vector containing the output of the MCMC
#' algorithm for the end of the phase.
#' @param level Probability corresponding to the level of confidence used
#' for the credible interval and the highest density region.
#'@param round_to Integer indicating the number of decimal places.

#' @return
#' A list with the following components:
#' \describe{
#' \item{statistics}{A data frame where the rows correspond to the
#' summary statistics and the columns include: \code{start}, the
#' start of the phase in calendar years (BC/AD); \code{end} the end
#' of the phase in calendar years (BC/AD); and \code{duration} the
#' duration of the phase in years.}
#' \item{level}{Probability corresponding to the level of confidence used
#' for the credible interval and the highest density region.}
#' \item{call}{The function call.}
#' }
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr},
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}, and
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}
#'
#' @examples
#'   data(Phases); attach(Phases)
#'   phase_statistics(Phase.1.alpha, Phase.1.beta, 0.95)
#'   phase_statistics(Phase.2.alpha, Phase.2.beta, 0.95)
#'   ## round to decade
#'   phase_statistics(Phase.2.alpha, Phase.2.beta, 0.95, -1)
#'
#' @export
phase_statistics <- function(min_chain,
                             max_chain,
                             level = 0.95,
                             round_to = 0) {
    chains <- cbind(start = min_chain,
                    end = max_chain,
                    duration = max_chain - min_chain)

    stats <- apply(X = chains,
                   MARGIN = 2,
                   FUN = function(x, l = level, r = round_to)
                       unlist(marginal_statistics(x, l, r)))

    if(is.list(stats)) {
        stats <- lapply(X = stats,
                        FUN = "length<-",
                        max(lengths(stats)))
    }
    stats.df <- data.frame(stats)
    stats.df <- stats.df[-which(rownames(stats.df) == "level"), ]
    list(statistics = stats.df, level = level, call = match.call())
}
