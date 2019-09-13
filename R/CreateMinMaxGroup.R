#####################################################
#         Constructing the Phases min max            #
#####################################################
#'
#' Construct the minimum and maximum for a group of events (phase)
#'
#' Constructs a data frame containing the output of the MCMC algorithm corresponding to
#' the minimum and maximum of a group of events
#'
#' @param data data frame containing the output of the MCMC algorithm
#' @param position numeric vector containing the position of the column corresponding to the MCMC chains of all dates included in the phase of interest
#' @param name name of the current group of dates or phase
#' @param add the name of the data frame in which the current minimum and maximum should be added, default = \code{NULL}
#' @param exportFile the name of the final file that will be saved if chosen, default = \code{NULL}
#'
#' @return A data frame containing the minimum and maximum of the group of dates included
#' in the phase of interest. These values may be appended to a data frame \code{add} if given.
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr} and
#'
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @examples
#'   data(Events)
#'   Temp = CreateMinMaxGroup(Events, c(2,4), name = "Phase2")
#'   Temp = CreateMinMaxGroup(Events, c(3,5), name = "Phase1", add=Temp,
#'                            exportFile = "MinMaxPhases.csv")
#' @export
#'
CreateMinMaxGroup <- function(data, position, name ="Phase", add=NULL, exportFile=NULL){

  # importing the CSV file
  dataTemp = data[position]
  Min = apply(dataTemp, 1, min)
  Max = apply(dataTemp, 1, max)

  name.Min = paste(name,".Min", sep="")
  name.Max = paste(name,".Max", sep="")

  MinMaxCurrentPhase = cbind(Min,Max)
  colnames(MinMaxCurrentPhase) <- c(name.Min,name.Max)

  if (is.null(add)){
    MinMaxPhase = MinMaxCurrentPhase
  } else {
    MinMaxPhase = cbind(add, MinMaxCurrentPhase)
  }

  if (!is.null(exportFile)){
    write.csv(MinMaxPhase, exportFile, row.names=FALSE)
  }

  return(as.data.frame(MinMaxPhase))

}
