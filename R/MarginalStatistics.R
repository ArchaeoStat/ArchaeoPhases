#####################################################
#            Marginal  Statistics                   #
#####################################################
#' Summary statistics
#'
#' Estimation of all usual statistics
#'
#' @param a_chain numeric vector containing the output of the MCMC algorithm for the parameter a
#' @param level probability corresponding to the level of confidence used for the credible interval and the highest density region
#' @param title title of the summary statistics
#' @return A list of values corresponding to all the following statistics
#' @export
MarginalStatistics <- function(a_chain, level=0.95){
  
  # Position
  mean = round(mean(a_chain), 0)
  hdr = hdr(a_chain, prob = c(level * 100))
  map = round(hdr$mode, 0)
  quantiles = round(quantile(a_chain, c(0.25,0.5,0.75)), 0)
  
  # Dispersion
  sd = round(sd(a_chain), 0)            # standard deviation using the 'sd' function
  CI = c(round(CredibleInterval(a_chain, level)[2], 0), round(CredibleInterval(a_chain, level)[3], 0))           # Credible Interval using the function 'CredibleInterval' from the package 'Rchronomodel'
  HPDR = round(hdr$hdr, 0)              # Highest posterior density function region using the function 'hdr' from the package 'hdrcde'
  
  # Resulted
  res = c(mean, map, sd, quantiles[1], quantiles[2], quantiles[3], level, CI[1], CI[2], HPDR)
  Mat = matrix(nrow=length(res), ncol=1)
  Mat[,1] = res
  
  nom=c()
  for( k in (1: (length(HPDR)/2) ) ) {
    nom=c(nom,paste("HPDRInf",k))
    nom=c(nom,paste("HPRDSup",k))
  }
  names1 = c("mean", "MAP", "sd", "Q1", "median", "Q2", "level", "CredibleInterval Inf", "CredibleInterval Sup")
  rownames(Mat) = c(names1, nom)
  return(Mat)
}