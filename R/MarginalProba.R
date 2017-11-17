#####################################################
#     Anteriority / posteriority Probability        #
#####################################################

#' Bayesian test for anteriority / posteriority
#'
#' Tests that a_chain < b_chain
#'
#' @param a_chain : numeric vector containing the output of the MCMC algorithm for the parameter a
#' @param b_chain : numeric vector containing the output of the MCMC algorithm for the parameter b
#' @return The baysesian probability that a < b knowing the data
#' @export
MarginalProba <- function(a_chain,b_chain){
  
  mean(ifelse(a_chain < b_chain, 1, 0))   ## bayesion test : a < b
  
}