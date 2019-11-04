
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
#' @param data Data frame containing the output of the MCMC algorithm.
#' @param position Numeric vector containing the position of the column
#' corresponding to the MCMC chains of interest.
#' @param level Probability corresponding to the level of confidence.
#' @param roundingOfValue Integer indicating the number of decimal places.
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
#'
#' @examples
#'   data(Events)
#'   MultiHPD(Events, c(2, 4, 3), 0.95)
#'
#' @importFrom hdrcde hdr
#'
#' @export
#'
MultiHPD <- function(data, position, level=0.95, roundingOfValue = 0){
  # An HPD region may be a union of several intervals

  # matrix of results for the first date
  hdr = hdr(data[,position[1]], prob = c(level * 100))$hdr
  HPDR = round(hdr, digits = roundingOfValue)
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
#' @param data Data frame containing the output of the MCMC algorithm.
#' @param position Numeric vector containing the position of the column
#' corresponding to the MCMC chains of interest.
#' @param level Probability corresponding to the level of confidence.
#' @param round_to Integer indicating the number of decimal places.
#'
#' @return Returns a list with the following components:
#' \describe{
#' \item{results}{A data frame where the rows correspond to the columns
#' in the selected data set and the columns labeled \code{inf} and \code{sup}
#' correspond to the lower and upper endpoints of each highest posterior
#' density interval, respectively.}
#' \item{level}{Probability corresponding to the level of confidence.}
#' \item{call}{The function call.}
#' }
#' matrix of values containing the level of confidence
#' and  for each variable of the MCMC chain.
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
#'
#' @examples
#'   data(Events)
#'   multi_hpd(Events, c(2, 4, 3), 0.95)
#'
#' @importFrom hdrcde hdr
#'
#' @export
#'
multi_hpd <- function(data, position, level = 0.95, round_to = 0) {

    prob <- level * 100

    data_set <- data[, position]
    res <- apply(X = data_set,
                 MARGIN = 2,
                 FUN = function(x, p = prob, r = round_to) {
                     h <- hdr(x, prob = p)$hdr
                     round(h, digits = r)
                 })

    res.df <- NULL
    for (i in seq(along = res))
        res.df <- rbind(res.df, unlist(res[i]))
    rownames(res.df) <- names(res)
    ind <- 1:ncol(res.df)
    colnames(res.df) <- ifelse(ind %% 2 != 0, "inf", "sup")

    list(results = res.df, level = level, call = match.call())
}
