
#####################################################
#         Constructing the Phases min max            #
#####################################################

#' Constructing the minimum and the maximum for each phase  
#'
#' Constructing a dataframe containing the output of the MCMC algorithm corresponding to the minimum and the maximum of each group of events
#'
#' @details 
#' @param data dataframe containing the output of the MCMC algorithm 
#' @param position numeric vector containing the position of the column corresponding to the MCMC chains of all dates included in the phase of interest
#' @param name name of the current group of dates or phase
#' @param add the name of the dataframe in which the current minimum and maximum should be added. Null by default. 
#' @param exportFile the name of the final file that will be saved if chosen. Null by default. 

#' @return A dataframe containing the minimum and the maximum of the group of dates included in the phase of interest. These values may be added to an already existing file "addFile" if given. 
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
  
  if (is.null(exportFile)){
    
  } else {
    write.csv(MinMaxPhase, exportFile, row.names=FALSE)
  }
  
  return(as.data.frame(MinMaxPhase))
  
}