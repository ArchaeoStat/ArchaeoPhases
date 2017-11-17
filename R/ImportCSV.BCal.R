
#####################################################
#              Import a BCal csv file               #
#####################################################
#' Importing a BCal csv file
#'
#' Importing a csv file containing the output of the MCMC algorithm from the BCal software
#'
#' @details
#' @param file the name of the CSV file containing the output of the MCMC algorithm
#' @param bin.width The bin width specified in a BCal project.  Note that bin.width does not have to be set if the BCal default bin width of 1 is used.
#' @return A data frame (data.frame) containing a representation of the data in the file.
#' @export
#'

ImportCSV.BCal <- function(file, bin.width=NULL) {
  ImportCSV(file = file, iterationColumn = 1, referenceYear = 1950, rowToWithdraw = "last", bin.width = bin.width)
}