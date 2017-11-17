#####################################################
#             Importing a CSV file                  #
#####################################################

#' Importing a CSV file
#'
#' Importing a CSV file containing the output of the MCMC algorithm from any software
#'
#' @details
#' @param file the name of the CSV file containing the output of the MCMC algorithm
#' @param dec the character used in the file for decimal points for the use of read.csv()
#' @param sep the field separator character for the use of read.csv()
#' @param comment.char a character vector of length one containing a single character or an empty string for the use of read.csv()
#' @param header a character vector of length one containing a single character or an empty string for the use of read.csv()
#' @param iterationColumn the column number containing the iteration number. If specified, the column will be withdrawn. Default = NULL.
#' @param referenceYear the year of reference of the date format
#' @param rowToWithdraw the number of the row to be withdrawn, or "last" for the last row of the data frame. Default = NULL.
#' @param bin.width The bin width specified in a BCal project.  Note that bin.width does not have to be set if the BCal default bin width of 1 is used.
#' @return A data frame (data.frame) containing a representation of the data in the file.
#' @export
#'
ImportCSV <- function(file, dec='.', sep=',', comment.char = '#',
                      header = TRUE, iterationColumn = NULL,
                      referenceYear = NULL, rowToWithdraw = NULL,
                      bin.width = NULL)
{
  # importing the CSV file
  data = read.csv(file, dec = dec, sep=sep, comment.char = comment.char,
                  header = header)
 
  # Withdrawing the iterations column
  if (!is.null(iterationColumn)){
    data = data[,-iterationColumn]
  }
  
  # Withdrawing a row
  if (!is.null(rowToWithdraw)){
    if (is.numeric(rowToWithdraw)) {
      data = data[-rowToWithdraw, ]
    }else{
      if (rowToWithdraw == "last") {
        data = data[-nrow(data), ]
      }
    }
  }
  
  # BCal bin width
  if (!is.null(bin.width)) {
    L = length(data)
    unbin <- function(value, width) {
      value * width
    }
    for (i in 1:L) {
      if (is.numeric(data[, i]) == TRUE) {
        data[, i] = sapply(data[, i], unbin, bin.width)
      }
      
    }
  }
    
  # Conversion of the MCMC samples in date format cal BP or other to BC/AD
  if (!is.null(referenceYear)){
    data2 = data
    L = length(data)
    conv <- function(value, T0){
      T0 - value
    }
    for (i in 1:L){
      if( is.numeric(data[,i]) == TRUE){
        data2[,i] = sapply(data[,i], conv, referenceYear)
      }
    }
    
    data = data2
  }
  
  return(data)

    
  


}