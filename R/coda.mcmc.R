#####################################################
#        Create a mcmc.list for CODA users         #
#####################################################

#' Create an \code{mcmc.list} object for \pkg{coda} users
#'
#' This wrapper function extracts parallel chains from a data frame to create an \code{mcmc.list}
#' object for use with \pkg{coda} diagnostic tools
#'
#' @param data Data frame containing the output of the MCMC algorithm.
#'
#' @param numberChains Number of parallel chains, default = 1.
#'
#' @param iterationColumn Column number corresponding to the iteration values, default = \code{NULL}.
#'
#' @return  An \code{mcmc.list} object
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr} and
#'
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @examples
#'   data(Events)
#'   mcmcList = coda.mcmc(data = Events, numberChains = 3, iterationColumn=1)
#'   plot(mcmcList)
#'   gelman.diag(mcmcList)
#'   # The multivariate criterion can not be evaluated when a phase
#'   # contains only one date. This induces colinearity problems.
#'   gelman.diag(mcmcList, multivariate=FALSE)
#'
#' @seealso \code{\link{coda}}
#'
#' @export

coda.mcmc <- function(data, numberChains = 1, iterationColumn = NULL){

  # Withdrawing the iteration column
  if (!is.null(iterationColumn)){
    data = data[,-iterationColumn]
  }



  if ( is.data.frame(data)==FALSE){ # if only 1 chain

    L = length(data) / numberChains

    obj <- list(NA);
    for (i in 1:numberChains){
      obj[[i]] = mcmc(data[ (L*(i-1)+1):(L*i)], start=1, end=L)
    }

  } else {
    dim =dim(data)
    L = dim[1]/numberChains

    # select only numeric columns
    vect = NULL
    for(i in 1:dim[2]){
      if(is.numeric(data[,i])==TRUE) { vect = c(vect,i)}
    }
    data2 = data[,vect]

    obj <- list(NA);
    for (i in 1:numberChains){
      obj[[i]] = mcmc(data2[ (L*(i-1)+1):(L*i),], start=1, end=L)
    }
  }



  mcmcList = mcmc.list(obj)
  return(mcmcList)
}
