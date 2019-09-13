#####################################################
#       Hiatus between a succession of  groups      #
#####################################################

#'  Gap or hiatus between a succession of groups (for groups in temporal order constraint)
#'
#' Finds, if it exists, a gap or hiatus between two successive groups.  This gap or hiatus
#' is the longest interval that satisfies
#' \eqn{P(Phase1Max < IntervalInf < IntervalSup < Phase2Min | M) = level}
#'
#' @param data Data frame containing the output of the MCMC algorithm.
#' @param position_minimum Numeric vector containing the column number
#' corresponding to the minimum of the events included in each group.
#' @param position_maximum Numeric vector containing the column number
#' corresponding to the end of the phases set in the same order
#' as in \code{position_minimum}.
#' @param level Probability corresponding to the level of confidence.
#'
#' @return
#' Returns a matrix of values containing the level of confidence
#' and the endpoints of the gap for each pair of successive groups. The
#' result is given in calendar years (BC/AD).
#' @details
#' For each \code{i}, \code{MultiPhasesGap()} computes the gap interval
#' for the phase defined by its minimum \code{position_minimum[i]} and
#' its maximum \code{position_maximum[i]}. The default value of
#' \code{position_maximum} corresponds to CSV files exported from
#' \href{https://chronomodel.com/}{ChronoModel} software.
#'
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
#'   MultiPhasesGap(Phases, position_minimum = c(4,2), position_maximum = c(5,3))
#'   # In this case, equivalent to
#'   MultiPhasesGap(Phases, position_minimum = c(4,2))
#'
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
