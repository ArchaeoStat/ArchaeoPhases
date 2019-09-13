#####################################################
#            Marginal  Statistics                   #
#####################################################
#' Marginal summary statistics
#'
#' Calculates summary statistics of the output of the MCMC algorithm for a one-parameter. Results are given in calendar years (BC/AD).
#'
#' The \eqn{(100 * level)}\% highest posterior density region is estimated using \code{hdr()} function from \pkg{hdrcde} package.
#'
#' @param a_chain Numeric vector containing the output of the MCMC
#' algorithm for the parameter.
#' @param level Probability corresponding to the level of confidence
#' used for the credible interval and the highest posterior density region.
#' @param roundingOfValue Integer indicating the number of decimal places.
#'
#' @return A named matrix of values corresponding to all the following statistics:
#' \describe{
#' \item{title}{The title of the summary statistics}
#' \item{mean}{The mean of the MCMC chain. Use of \code{mean()} function.}
#' \item{map }{The maximum a posteriori of the MCMC chain. Use of \code{hdr()} function.}
#' \item{sd}{The standard deviation of the MCMC chain. Use of \code{sd()} function.
#' \item{Q1, median, Q3 }{The quantiles of the MCMC chain corresponding to 0.25, 0.50 and 0.75. Use of \code{quantile} function.}}
#' \item{CI}{The credible interval corresponding to the desired level. Use of \code{CredibleInterval()} function.}
#' \item{HPDR}{The highest posterior density regions corresponding to the desired level. Use of \code{hdr()} function.}
#' }
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr} and
#'
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#'@references
#' Hyndman, R. J. (1996) Computing and graphing highest density regions. American Statistician, 50, 120-126.
#'
#' @examples
#'   data(Events); attach(Events)
#'   MarginalStatistics(Event.1)
#'   MarginalStatistics(Event.2, level = 0.90)
#'
#' @export
MarginalStatistics <- function(a_chain, level=0.95, roundingOfValue = 0){

  # Position
  mean = round(mean(a_chain), roundingOfValue)
  hdr = hdr(a_chain, prob = c(level * 100))
  map = round(hdr$mode, roundingOfValue)
  quantiles = round(quantile(a_chain, c(0.25,0.5,0.75)), roundingOfValue)

  # Dispersion
  sd = round(sd(a_chain), roundingOfValue)            # standard deviation using the 'sd' function
  CI = c( CredibleInterval(a_chain, level, roundingOfValue=roundingOfValue)[2], CredibleInterval(a_chain, level, roundingOfValue=roundingOfValue)[3])           # Credible Interval using the function 'CredibleInterval' from the package 'Rchronomodel'
  HPDR = round(hdr$hdr, roundingOfValue)              # Highest posterior density function region using the function 'hdr' from the package 'hdrcde'

  # Resulted
  res = c(mean, map, sd, quantiles[1], quantiles[2], quantiles[3], level, CI[1], CI[2], HPDR)
  Mat = matrix(nrow=length(res), ncol=1)
  Mat[,1] = res

  nom=c()
  for( k in (1: (length(HPDR)/2) ) ) {
    nom=c(nom,paste("HPDR Inf",k))
    nom=c(nom,paste("HPDR Sup",k))
  }
  names1 = c("mean", "MAP", "sd", "Q1", "median", "Q2", "level", "Credible Interval Inf", "Credible Interval Sup")
  rownames(Mat) = c(names1, nom)
  return(Mat)
}
