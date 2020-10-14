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
PhaseStatistics <- function(min_chain,
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
