#####################################################
#     Anteriority / posteriority Probability        #
#####################################################

#' Bayesian test for anteriority / posteriority between two parameters
#'
#' This function estimates the posterior probability that event 'a' is older than
#' event 'b' using the output of the MCMC algorithm. This provides a Bayesian test
#' for checking the following assumption: "Event a is older than event b".
#'
#' For a given output of MCMC algorithm, this function estimates the posterior probability
#' of the event 'a' < 'b' by the relative frequency of the event "the value of event 'a'
#' is less than the value of event 'b'" in the simulated Markov chain.
#'
#' @param a_chain : Numeric vector containing the output of the MCMC
#' algorithm for the first parameter.
#' @param b_chain : Numeric vector containing the output of the MCMC
#' algorithm for the second parameter.
#'
#' @return An unnamed vector with the posterior probability of the assumption:
#' "event a is older than event b"
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr} and
#'
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @examples
#'
#'   data(Events); attach(Events)
#'   # Probability that Event.1 is older than Event.12
#'   MarginalProba(Event.1, Event.12)
#'   # Probability that Event.1 is older than Event.2
#'   MarginalProba(Event.1, Event.2)
#'   # Probability that the beginning of the phase 1 is older than the end of the phase 1
#'   # Should always be 1 for every phase
#'   data(Phases); attach(Phases)
#'   MarginalProba(Phase.1.alpha, Phase.1.beta)
#'
#' @export
MarginalProba <- function(a_chain, b_chain) {
    ## bayesion test : a < b
    mean(ifelse(a_chain < b_chain, 1, 0))
}
