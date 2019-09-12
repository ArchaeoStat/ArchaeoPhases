#####################################################
#         Multiple Phase Time Range                 #
#####################################################
#' Phase time range for multiple groups
#'
#' Computes the shortest interval that satisfies
#' \eqn{P(PhaseMin < IntervalInf < IntervalSup < PhaseMax | M) = level}
#' for each phase
#'
#' @param data data frame containing the output of the MCMC algorithm
#' @param position_minimum numeric vector containing the column number
#' corresponding to the minimum of the events included in each phase
#' @param position_maximum numeric vector containing the column number
#' corresponding to the maximum of the phases set in the same order as
#' in \code{position_minimum}
#' @param level probability corresponding to the desired level of confidence
#'
#' @details
#' For each \code{i}, \code{MultiPhasesTimeRange()} computes the time range interval
#' for the phase defined by its minimum \code{position_minimum[i]} and
#' its maximum \code{position_maximum[i]}. The default value of
#' \code{position_maximum} corresponds to CSV files exported from
#' \href{https://chronomodel.com/}{ChronoModel} software.
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr} and
#'
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @examples
#'   # Data extracted from ChronoModel software
#'   data(Phases)
#'   # List of the name of the groups
#'   names(Phases)
#'   # Stipulating position_maximum
#'   MultiPhasesTimeRange(Phases, position_minimum = c(4,2), position_maximum = c(5,3))
#'   # In this case, equivalent to
#'   MultiPhasesTimeRange(Phases, position_minimum = c(4,2))
#'
#' @keywords Bayesian statistics
#' @keywords phase time range
#'
#' @return
#' A matrix of values containing the level of confidence and the endpoints
#' of the shortest time range associated with the desired \code{level}. The result
#' is given in calendar years (BC/AD).
#'
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
