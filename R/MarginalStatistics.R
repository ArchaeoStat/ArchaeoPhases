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
#' \item{sd}{The standard deviation of the MCMC chain. Use of \code{sd()} function.}
#' \item{Q1, median, Q3 }{The quantiles of the MCMC chain corresponding to 0.25, 0.50 and 0.75. Use of \code{quantile} function.}
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
#' @importFrom stats quantile
#' @importFrom hdrcde hdr
#'
#' @export
MarginalStatistics <- function(a_chain, level=0.95, roundingOfValue = 0){

  # Position
    mean = round(mean(a_chain), roundingOfValue)
    if(length(unique(a_chain)) == 1L) {
        single_year <- round(unique(a_chain), roundingOfValue)
        map <- single_year
        HPDR <- c(single_year, single_year)
    }
    else {
        hdr = hdr(a_chain, prob = c(level * 100))
        map = round(hdr$mode, roundingOfValue)
        HPDR = round(hdr$hdr, roundingOfValue)              # Highest posterior density function region using the function 'hdr' from the package 'hdrcde'
    }
  quantiles = round(quantile(a_chain, c(0.25,0.5,0.75)), roundingOfValue)

  # Dispersion
  sd = round(sd(a_chain), roundingOfValue)            # standard deviation using the 'sd' function
  CI = c( CredibleInterval(a_chain, level, roundingOfValue=roundingOfValue)[2], CredibleInterval(a_chain, level, roundingOfValue=roundingOfValue)[3])           # Credible Interval using the function 'CredibleInterval' from the package 'Rchronomodel'

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

#' Marginal summary statistics
#'
#' Calculates summary statistics of the output of the MCMC algorithm for a
#' single parameter. Results are given in calendar years (BC/AD).
#'
#' The \eqn{(100 * level)}\% highest posterior density region is estimated
#' using \code{hdr()} function from \pkg{hdrcde} package.
#'
#' @param a_chain Numeric vector containing the output of the MCMC
#' algorithm for the parameter.
#' @param level Probability corresponding to the level of confidence
#' used for the credible interval and the highest posterior density region.
#' @param round_to Integer indicating the number of decimal places.
#'
#' @return A list with the following components:
#' \describe{
#' \item{mean}{The mean of the MCMC chain.}
#' \item{map}{The maximum a posteriori of the MCMC chain.}
#' \item{sd}{The standard deviation of the MCMC chain.}
#' \item{quantiles}{A vector with the following elements:
#' \code{min} = minimum value of the MCMC chain;
#' \code{q1} = first quantile of the MCMC chain;
#' \code{median} = median of the MCMC chain;
#' \code{q2} = second quantile of the MCMC chain; and
#' \code{max} = maximum value of the MCMC chain.}
#' \item{level}{Confidence level for the credible interval
#' and highest posterior density.}
#' \item{ci}{A vector with the following elements:
#' \code{inf} = lower credible interval of the MCMC chain at \code{level}; and
#' \code{sup} = upper credible interval of the MCMC chain at \code{level}.
#' }
#' \item{hpdr}{A variable length vector with the lower and upper highest
#' posterior density regions of the MCMC chain at \code{level}.  List
#' components are named \code{inf_n} and \code{sup_n} for n = 1 to the
#' number of highest posterior density regions.}
#' }
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr},
#' @author Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}, and
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}
#'
#'@references
#' Hyndman, R. J. (1996) Computing and graphing highest density regions.
#' American Statistician, 50, 120-126.
#'
#' @examples
#'   data(Events); attach(Events)
#'   marginal_statistics(Event.1)
#'   marginal_statistics(Event.2, level = 0.90)
#'   ## convenient vector
#'   foo <- marginal_statistics(Event.1)
#'   unlist(foo)
#'
#' @importFrom stats quantile
#' @importFrom hdrcde hdr
#' @importFrom tibble is_tibble
#'
#' @export
marginal_statistics <- function(a_chain, level = 0.95, round_to = 0) {

    if (is_tibble(a_chain)) a_chain <- unlist(a_chain)

    ## Position
    mean <- round(mean(a_chain), round_to)
    if(length(unique(a_chain)) == 1L) {
        single_year <-unique(a_chain)
        map <- single_year
        hpdr <- c(inf = single_year, sup = single_year)
    }
    else {
        hdr <- hdr(a_chain, prob = level * 100)
        map <- round(hdr$mode, round_to)
        hpdr <- round(hdr$hdr, round_to)
        hpdr <- as.vector(hpdr)
        names(hpdr) <- names(hdr$hdr)
        i <- 0
        for( k in (1:(length(hpdr)/2))) {
            i <- i + 1
            names(hpdr)[i] <- paste("inf", k, sep = "_")
            i <- i + 1
            names(hpdr)[i] <- paste("sup", k, sep = "_")
        }
    }
    quantiles <- round(quantile(a_chain), round_to)
    names(quantiles) <- c("min", "q1", "median", "q3", "max")
    ## Dispersion
    sd <- round(sd(a_chain), round_to)
    ci <- credible_interval(a_chain, level, round_to = round_to)$ci

    ## Results
    list(mean = mean, map = map, sd = sd, quantiles = quantiles,
         level = level, ci = ci, hpdr = hpdr)
}


#' Marginal summary statistics for multiple MCMC chains
#'
#' Calculates summary statistics of the output of the MCMC algorithm for
#' multiple parameters. Results are given in calendar years (BC/AD).
#'
#' @param data Data frame containing the output of the MCMC algorithm.
#' @param position Numeric vector containing the positions of the columns
#' corresponding to the MCMC chains of interest, or a vector of column
#' names.
#' @param level Probability corresponding to the level of confidence
#' used for the credible interval and the highest posterior density region.
#' @param round_to Integer indicating the number of decimal places.
#'
#' @return A data frame where the rows correspond to the chains of interest and
#' columns to the following statistics:
#' \describe{
#' \item{mean}{The mean of the MCMC chain.}
#' \item{sd}{The standard deviation of the MCMC chain.}
#' \item{min}{Minimum value of the MCMC chain;}
#' \item{q1}{First quantile of the MCMC chain;}
#' \item{median}{Median of the MCMC chain;}
#' \item{q3}{Third quantile of the MCMC chain; and}
#' \item{max}{Maximum value of the MCMC chain.}
#' \item{ci.inf}{Lower credible interval of the MCMC chain at \code{level}.}
#' \item{ci.sup}{Upper credible interval of the MCMC chain at \code{level}.}
#' }
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr},
#' @author Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}, and
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}
#'
#' @examples
#'   data(Events)
#'   multi_marginal_statistics(Events, 2:5)
#'   multi_marginal_statistics(Events, 2:5, level = 0.90)
#'   ## round to decades
#'   multi_marginal_statistics(Events, 2:5, round_to = -1)
#'
#' @export

multi_marginal_statistics <- function(data,
                                      position = 1:ncol(data),
                                      level = 0.95,
                                      round_to = 0) {

    if(!is.data.frame(data)) stop("Data format not recognized.")

    data_set <- data[, position]
    data_names <- names(data_set)
    summaries <- c("mean", "sd", "quantiles.min", "quantiles.q1",
                   "quantiles.median", "quantiles.q3", "quantiles.max",
                   "ci.inf", "ci.sup")
    stat_names <- c("mean", "sd", "min", "q1", "median", "q3",
                    "max", "ci.inf", "ci.sup")

    stats <- apply(X = data_set,
                   MARGIN = 2,
                   FUN = function(x, l = level, r = round_to, s = summaries) {
                       unlist(marginal_statistics(x, l, r))[s]
                   })

    stats.df <- as.data.frame(stats)
    names(stats.df) <- data_names
    row.names(stats.df) <- stat_names

    list(statistics = t(stats.df), level = level, call = match.call())
}
