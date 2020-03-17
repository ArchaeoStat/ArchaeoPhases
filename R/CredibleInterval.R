#####################################################
#     Estimation of Credible interval        #
#####################################################
#'
#' Bayesian credible interval
#'
#' Computes the shortest credible interval of the output of the MCMC algorithm for a single parameter
#'
#' @details A \eqn{(100 * level)}\% credible interval is an interval that keeps \eqn{N * (1 - level)}
#' elements of the sample outside the interval.
#' The \eqn{(100 * level)}\% credible interval is the shortest of all those intervals.
#' @param a_chain Numeric vector containing the output of the MCMC algorithm
#' for the parameter.
#' @param level Probability corresponding to the level of confidence used for the
#' credible interval, default = 0.95.
#' @param roundingOfValue Integer indicating the number of decimal places to be used, default = 0.
#' @return A named vector of values containing the confidence level and the endpoints of the shortest
#' credible interval in calendar years (BC/AD).
#' @examples
#'   data(Events); attach(Events)
#'   CredibleInterval(Event.1)
#'   CredibleInterval(Event.12, 0.50)
#'
#' @export
CredibleInterval <- function(a_chain, level=0.95, roundingOfValue=0){

  sorted_sample <- sort(a_chain)     # ordering the sample
  N = length(a_chain)                # calculation of the sample size
  OutSample = N * (1-level)          # calculation of the number of data to be outside the interval

  I =  cbind(sorted_sample[1:(OutSample+1)] , sorted_sample[(N-OutSample):N])    #   combinasion of all credible intervals

  l = I[,2]-I[,1]   # length of intervals
  i <- which.min(l) # look for the shortest interval

  return( c( "level" = level, "Credible.Interval.Inf"= round(I[i,1], digits = roundingOfValue), "Credible.Interval.Sup"=round(I[i,2], digits = roundingOfValue )) ) # returns the level and the endpoints rounded

}

#' Bayesian credible interval
#'
#' Computes the shortest credible interval for a single parameter.
#'
#' @details A \eqn{(100 * level)}\% credible interval is an interval
#' that keeps \eqn{N * (1 - level)} elements of the sample outside the
#' interval. The \eqn{(100 * level)}\% credible interval is the shortest
#' of those intervals.
#' @param data Numeric vector containing the output of the MCMC algorithm
#' for the parameter.
#' @param level Probability corresponding to the level of confidence used
#' for the credible interval, default = 0.95.
#' @param round_to Integer indicating the number of decimal places to be used,
#' default = 0.
#' @return A list with the following components:
#' \describe{
#' \item{ci}{Named vector of length 2, with \code{inf} the
#' lower endpoint of the shortest credible interval as a calendar
#' year; and \code{sup} the upper endpoint of the shortest credible
#' interval as a calendar year;}
#' \item{level}{Confidence level for the credible intervals; and}
#' \item{call}{Function call.}
#'}
#'
#' @examples
#'   data(Events); attach(Events)
#'   credible_interval(Event.1)
#'   credible_interval(Event.12, 0.50)
#'
#' @export
credible_interval <- function(data, level = 0.95, round_to = 0) {

    if (!is.vector(data) || !is.numeric(data))
        stop("Data format not recognized.")

    sorted <- sort(data)
    N = length(data)
    outside = N * (1 - level)
    inf <- 1:(outside + 1)
    sup <- (N - outside):N
    ind <- which.min(sorted[sup] - sorted[inf])
    res <- round(c(sorted[inf][ind], sorted[sup][ind]), digits = round_to)
    names(res) <- c("inf", "sup")
    list(ci = res,
         level = level,
         call = match.call())
}
