
#####################################################
#                   MultiHPD                        #
#####################################################

#' Bayesian HPD regions for a series of MCMC chains
#'
#' Estimation of the highest posterior density regions for each variable of a
#' simulated Markov chain. This function uses the \code{hdr()} function
#' included in the \pkg{hdrcde} package. An HPD region may be a union of
#' several intervals.
#'
#' @details Highest posterior density function region using the function
#' \code{hdr()} from the \pkg{hdrcd} package
#'
#' @param data data frame containing the output of the MCMC algorithm
#' @param position numeric vector containing the position of the column
#' corresponding to the MCMC chains of interest
#' @param level probability corresponding to the level of confidence
#' @param roundingOfValue integer indicating the number of decimal places
#'
#' @return Returns a matrix of values containing the level of confidence
#' and the endpoints of each interval for each variable of the MCMC chain.
#' The name of the resulting rows are the positions of the corresponding
#' columns in the CSV file. The result is given in calendar years (BC/AD).
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr} and
#'
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @references
#' Hyndman, R.J. (1996) Computing and graphing highest density regions.
#' American Statistician, 50, 120-126.
#' @examples
#'   data(Events)
#'   MultiHPD(Events, c(2,4,3), 0.95)
#'
#' @export
#'
MultiHPD <- function(data, position, level=0.95, roundingOfValue = 0){
  # An HPD region may be a union of several intervals

  # matrix of results for the first date
  hdr = hdr(data[,position[1]], prob = c(level * 100))$hdr
  HPDR = round(hdr, digits = 0)
  result = matrix( c(level, HPDR), nrow=1)
  dim = dim(result)[2]

  # the resulting matrix is then completed

  if(length(position) > 1){

    for (i in 2:length(position)) {

      hdr = hdr(data[,position[i]], prob = c(level * 100))$hdr
      HPDR = round(hdr, digits = roundingOfValue)
      res = c(level, HPDR)

      # comparison of the size of the current result, res, and the nomber of columns of the matrix, result
      if (length(res) > dim) { # If the new HPD region is composed of more intervals than the others
        NbCol = length(res) - dim
        AddColum = rep(NA, i-1)
        Ajout = NULL
        for (j in 1:NbCol){
          Ajout = cbind(Ajout, AddColum)
        }
        resultTemp = cbind(result, Ajout)
        result =  rbind(resultTemp, res)

      }else if (length(res) < dim) { # If the new HPD region is composed of less intervals than the others
        NbCol = dim - length(res)
        Add = rep(NA, NbCol)
        Ajout = c(res,Add)
        result = rbind(result, Ajout)
      }else{
        result =  rbind(result, res)
      }
      dim = dim(result)[2]
    }

  }

  # Adding names to rows and to columns
  nom=c()
  for( k in (1:((dim-1)/2)) ) {
    nom=c(nom,paste("HPDRInf",k))
    nom=c(nom,paste("HPRDSup",k))
  }
  colnames(result) <- c("Level", nom)
  rownames(result) <- names(data)[position]

  return(result) # returns a matrix with the level and the endpoints
}
