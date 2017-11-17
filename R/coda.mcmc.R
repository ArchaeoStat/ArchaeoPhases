#####################################################
#        Create a mcmc.list for CODA users         #
#####################################################

#' Create a mcmc.list for CODA users
#' 
#' 
#' @export coda.mcmc

coda.mcmc <- function(data, numberChains = 1, iterationColumn = NULL){
  
  # Withdrawing the iteration column
  if (!is.null(iterationColumn)){
    data = data[,-iterationColumn]
  }
  
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
  
  mcmcList = mcmc.list(obj)
  return(mcmcList)
}